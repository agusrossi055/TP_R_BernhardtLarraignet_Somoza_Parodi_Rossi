############################################################
# ANÁLISIS DE DATOS: Encuesta Permanente de Hogares (EPH)
# Script PRINCIPAL (ejecuta las funciones de funciones.R)
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
#
# Preguntas:
#   P1. ¿Existe una brecha de género en el ingreso laboral? ¿Cuánto se
#       explica por diferencias en horas trabajadas y cómo varía según
#       el nivel educativo?
#   P2. ¿Qué proporción de los asalariados es informal (sin descuento
#       jubilatorio)? ¿Cómo varía por edad y región, y qué penalidad
#       salarial implica la informalidad?
#   P3. ¿Cómo difiere la distribución del ingreso per cápita familiar
#       entre las regiones del país?
#
# Cómo correrlo: con el working directory en la raíz del
# repositorio, correr el script completo de arriba hacia abajo.
############################################################

set.seed(2026)   # Semilla fija (el análisis no tiene aleatoriedad,
                 # pero la dejamos por buena práctica)

suppressPackageStartupMessages(library(tidyverse))
source("analisis_datos/funciones.R")

dir_fig <- "analisis_datos/figuras"
dir_res <- "analisis_datos/resultados"
dir.create(dir_fig, showWarnings = FALSE)
dir.create(dir_res, showWarnings = FALSE)



############################################################
# 0) CARGA DE DATOS
############################################################
# Base usuaria individual de la EPH, 1er trimestre de 2026 (INDEC), el
# último trimestre publicado al momento de hacer el trabajo.
# Descargada de https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
# (archivo EPH_usu_1_Trim_2026_txt.zip -> usu_individual_T126.txt).
# Es un .txt separado por ";" con coma como separador decimal.
archivo_eph <- "analisis_datos/data/usu_individual_T126.txt"
eph_cruda   <- read_delim(archivo_eph, delim = ";", show_col_types = FALSE,
                          locale = locale(decimal_mark = ","))
stopifnot(nrow(problems(eph_cruda)) == 0)   # la lectura no tuvo errores
dim(eph_cruda)     # 43.739 personas x 234 variables


############################################################
# 1) EXPLORACIÓN Y LIMPIEZA PREVIA
############################################################
# Chequeo de consistencia: los casos sin respuesta de ingreso tienen
# ponderador de ingreso igual a 0 (el INDEC redistribuye su peso
# entre los que sí responden). Si esto falla, la ponderación sería errónea.
stopifnot(all((eph_cruda$P21 == -9) == (eph_cruda$PONDIIO == 0)))
stopifnot(all((eph_cruda$DECIFR == 12) == (eph_cruda$PONDIH == 0)))

eph <- preparar_eph(eph_cruda)

# Tabla de faltantes después de recodificar
eph |>
  summarise(across(c(edad, sexo, region, educacion, ingreso_ocup, horas_sem,
                     ingreso_hora, informal, ipcf), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA") |>
  guardar_tabla("faltantes", dir_res)

# --- Visualizaciones exploratorias ---------------------------------

# (a) Ingreso de la ocupación principal de los ocupados: escala lineal
#     y logarítmica (la distribución es muy asimétrica)
ocupados_ing <- eph |> filter(estado == 1, ingreso_ocup > 0)

g_ing_lineal <- ggplot(ocupados_ing, aes(ingreso_ocup)) +
  geom_histogram(bins = 80, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(title = "Ingreso de la ocupación principal (escala lineal)",
       x = "Pesos por mes", y = "Personas en la muestra") +
  theme_minimal()
g_ing_log <- ggplot(ocupados_ing, aes(ingreso_ocup)) +
  geom_histogram(bins = 80, fill = "steelblue", alpha = 0.7) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(title = "Ingreso de la ocupación principal (escala log)",
       x = "Pesos por mes (escala logarítmica)", y = "Personas en la muestra") +
  theme_minimal()
ggsave(file.path(dir_fig, "expl_ingreso.pdf"),
       patchwork::wrap_plots(g_ing_lineal, g_ing_log, ncol = 2),
       width = 10, height = 3.6)

# (b) Horas semanales trabajadas: se ven picos en 40, 45, 48 (jornadas
#     típicas) y una masa de ocupados con 0 horas (ausentes esa semana)
g_horas <- eph |>
  filter(estado == 1, !is.na(horas_sem)) |>
  ggplot(aes(horas_sem)) +
  geom_histogram(binwidth = 1, fill = "darkorange", alpha = 0.8) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Horas semanales trabajadas en la ocupación principal (ocupados)",
       subtitle = "Se excluyen los códigos 999 (no sabe / no responde)",
       x = "Horas por semana", y = "Personas en la muestra") +
  theme_minimal()
ggsave(file.path(dir_fig, "expl_horas.pdf"), g_horas, width = 8, height = 3.5)

# (c) Pirámide de población de la muestra (chequeo de edades/sexo)
g_piramide <- eph |>
  mutate(edad_5 = pmin(floor(pmax(edad, 0) / 5) * 5, 85)) |>
  count(sexo, edad_5, wt = pondera) |>
  mutate(n = if_else(sexo == "Varón", -n, n) / 1e6) |>
  ggplot(aes(x = edad_5, y = n, fill = sexo)) +
  geom_col(width = 4.5) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Pirámide de población (expandida con PONDERA)",
       x = "Edad (tramos de 5 años; 85 = 85 y más)",
       y = "Millones de personas", fill = NULL) +
  theme_minimal()
ggsave(file.path(dir_fig, "expl_piramide.pdf"), g_piramide, width = 6, height = 4.5)

# (d) Ingreso per cápita familiar por región (escala log)
g_ipcf_box <- eph |>
  filter(!is.na(ipcf), ipcf > 0) |>
  ggplot(aes(x = region, y = ipcf, weight = pondih)) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.alpha = 0.15) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(title = "Ingreso per cápita familiar por región",
       x = NULL, y = "Pesos por mes (escala log)") +
  theme_minimal()
ggsave(file.path(dir_fig, "expl_ipcf_region.pdf"), g_ipcf_box, width = 7, height = 4)

# Ocupados con ingreso 0: ¿quiénes son? (trabajadores familiares sin
# remuneración, CAT_OCUP = 4, y casos puntuales) -> se excluyen de P1
eph |> filter(estado == 1, ingreso_ocup == 0) |> count(cat_ocup) |> print()


############################################################
# PREGUNTA 1: Brecha de género en el ingreso laboral
############################################################
# Universo: ocupados con ingreso positivo en la ocupación principal.
# Ponderador: PONDIIO (el del ingreso de la ocupación principal).
p1 <- eph |> filter(estado == 1, ingreso_ocup > 0, pondiio > 0)

# 1.a) Ingreso mensual, horas e ingreso por hora según sexo
p1_sexo <- p1 |>
  group_by(sexo) |>
  summarise(
    n_muestra       = n(),
    poblacion       = sum(pondiio),
    ingreso_medio   = weighted.mean(ingreso_ocup, pondiio),
    ingreso_mediano = cuantil_ponderado(ingreso_ocup, pondiio, 0.5),
    horas_medias    = weighted.mean(horas_sem, pondiio, na.rm = TRUE),
    ing_hora_medio  = weighted.mean(ingreso_hora, pondiio, na.rm = TRUE),
    ing_hora_mediano = cuantil_ponderado(ingreso_hora[!is.na(ingreso_hora)],
                                         pondiio[!is.na(ingreso_hora)], 0.5),
    prop_superior_completo = weighted.mean(educacion == "Superior completo",
                                           pondiio, na.rm = TRUE)
  )
guardar_tabla(p1_sexo, "p1_por_sexo", dir_res)

p1_brechas <- tibble(
  medida = c("Ingreso mensual medio", "Ingreso mensual mediano",
             "Horas semanales medias", "Ingreso por hora medio",
             "Ingreso por hora mediano"),
  brecha_pct = c(
    brecha_pct(p1_sexo$ingreso_medio[1],    p1_sexo$ingreso_medio[2]),
    brecha_pct(p1_sexo$ingreso_mediano[1],  p1_sexo$ingreso_mediano[2]),
    brecha_pct(p1_sexo$horas_medias[1],     p1_sexo$horas_medias[2]),
    brecha_pct(p1_sexo$ing_hora_medio[1],   p1_sexo$ing_hora_medio[2]),
    brecha_pct(p1_sexo$ing_hora_mediano[1], p1_sexo$ing_hora_mediano[2]))
)
guardar_tabla(p1_brechas, "p1_brechas", dir_res)

# 1.b) Distribución de horas trabajadas por sexo
g_p1_horas <- p1 |>
  filter(!is.na(horas_sem), horas_sem > 0) |>
  mutate(tramo_horas = cut(horas_sem, breaks = c(0, 15, 34, 45, Inf),
                           labels = c("1-15", "16-34", "35-45", "Más de 45"))) |>
  count(sexo, tramo_horas, wt = pondiio) |>
  group_by(sexo) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(tramo_horas, prop, fill = sexo)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Horas semanales trabajadas en la ocupación principal, por sexo",
       x = "Horas por semana", y = "% de los ocupados", fill = NULL) +
  theme_minimal()
ggsave(file.path(dir_fig, "p1_horas_sexo.pdf"), g_p1_horas, width = 7, height = 3.8)

# 1.c) Brecha por nivel educativo: mensual vs. por hora (medianas)
p1_educ <- p1 |>
  filter(!is.na(educacion), !is.na(ingreso_hora)) |>
  group_by(educacion, sexo) |>
  summarise(n_muestra = n(),
            mediana_mensual = cuantil_ponderado(ingreso_ocup, pondiio, 0.5),
            mediana_hora    = cuantil_ponderado(ingreso_hora, pondiio, 0.5),
            horas_medias    = weighted.mean(horas_sem, pondiio),
            .groups = "drop") |>
  pivot_wider(names_from = sexo,
              values_from = c(n_muestra, mediana_mensual, mediana_hora, horas_medias)) |>
  mutate(brecha_mensual = brecha_pct(mediana_mensual_Varón, mediana_mensual_Mujer),
         brecha_hora    = brecha_pct(mediana_hora_Varón, mediana_hora_Mujer))
guardar_tabla(p1_educ, "p1_brecha_educacion", dir_res)

g_p1_educ <- p1_educ |>
  select(educacion, `Ingreso mensual` = brecha_mensual,
         `Ingreso por hora` = brecha_hora) |>
  pivot_longer(-educacion, names_to = "medida", values_to = "brecha") |>
  ggplot(aes(x = educacion, y = brecha, fill = medida)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = sprintf("%.0f%%", brecha),
                vjust = if_else(brecha >= 0, -0.3, 1.2)),
            position = position_dodge(width = 0.9), size = 3) +
  scale_fill_manual(values = c("grey40", "darkorange")) +
  labs(title = "Brecha de género en la mediana del ingreso laboral, por nivel educativo",
       subtitle = "Brecha = (varón - mujer) / varón. Positivo: las mujeres ganan menos",
       x = NULL, y = "Brecha (%)", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "bottom")
ggsave(file.path(dir_fig, "p1_brecha_educacion.pdf"), g_p1_educ, width = 8, height = 4.5)


############################################################
# PREGUNTA 2: Informalidad de los asalariados
############################################################
# Universo: asalariados ocupados (ESTADO = 1, CAT_OCUP = 3) que
# respondieron si les hacen descuento jubilatorio.
# Tasas: ponderador PONDERA. Salarios: ponderador PONDIIO.
p2 <- eph |> filter(estado == 1, cat_ocup == 3, !is.na(informal))

tasa_total <- p2 |>
  summarise(n_muestra = n(), tasa_informalidad = weighted.mean(informal, pondera))
guardar_tabla(tasa_total, "p2_tasa_total", dir_res)

# 2.a) Por tramo de edad
p2_edad <- p2 |>
  group_by(grupo_edad) |>
  summarise(n_muestra = n(),
            tasa_informalidad = weighted.mean(informal, pondera), .groups = "drop")
guardar_tabla(p2_edad, "p2_por_edad", dir_res)

# 2.b) Por región
p2_region <- p2 |>
  group_by(region) |>
  summarise(n_muestra = n(),
            tasa_informalidad = weighted.mean(informal, pondera), .groups = "drop") |>
  arrange(desc(tasa_informalidad))
guardar_tabla(p2_region, "p2_por_region", dir_res)

# 2.c) Por nivel educativo y sexo
p2_educ <- p2 |>
  filter(!is.na(educacion)) |>
  group_by(educacion, sexo) |>
  summarise(n_muestra = n(),
            tasa_informalidad = weighted.mean(informal, pondera), .groups = "drop")
guardar_tabla(p2_educ, "p2_por_educacion_sexo", dir_res)

# Mapa de calor edad x región
p2_edad_region <- p2 |>
  filter(grupo_edad != "Menores de 18") |>   # muy pocos casos
  group_by(region, grupo_edad) |>
  summarise(n_muestra = n(),
            tasa = weighted.mean(informal, pondera), .groups = "drop")
guardar_tabla(p2_edad_region, "p2_edad_region", dir_res)

g_p2_heat <- ggplot(p2_edad_region, aes(grupo_edad, region, fill = tasa)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.0f%%", 100 * tasa)), size = 3.5) +
  scale_fill_gradient(low = "white", high = "firebrick",
                      labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "Tasa de informalidad de los asalariados por región y edad",
       subtitle = "Informal = sin descuento jubilatorio",
       x = "Edad", y = NULL, fill = "Informalidad") +
  theme_minimal()
ggsave(file.path(dir_fig, "p2_informalidad_region_edad.pdf"), g_p2_heat,
       width = 8, height = 4)

# 2.d) Penalidad salarial: mediana del ingreso por hora formal vs.
#      informal, dentro de cada nivel educativo (para comparar
#      trabajadores parecidos)
p2_salarios <- p2 |>
  filter(ingreso_ocup > 0, pondiio > 0, !is.na(ingreso_hora), !is.na(educacion)) |>
  mutate(condicion = if_else(informal, "Informal", "Formal")) |>
  group_by(educacion, condicion) |>
  summarise(n_muestra = n(),
            mediana_hora = cuantil_ponderado(ingreso_hora, pondiio, 0.5),
            horas_medias = weighted.mean(horas_sem, pondiio),
            .groups = "drop")
guardar_tabla(p2_salarios, "p2_salarios_formal_informal", dir_res)

p2_penalidad <- p2_salarios |>
  select(educacion, condicion, mediana_hora) |>
  pivot_wider(names_from = condicion, values_from = mediana_hora) |>
  mutate(penalidad_pct = 100 * (Formal - Informal) / Formal)
guardar_tabla(p2_penalidad, "p2_penalidad", dir_res)

# Penalidad agregada (todos los niveles educativos juntos)
p2 |>
  filter(ingreso_ocup > 0, pondiio > 0, !is.na(ingreso_hora)) |>
  group_by(informal) |>
  summarise(mediana_hora = cuantil_ponderado(ingreso_hora, pondiio, 0.5)) |>
  print()

g_p2_sal <- ggplot(p2_salarios, aes(educacion, mediana_hora, fill = condicion)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Mediana del ingreso por hora de los asalariados: formales vs. informales",
       x = NULL, y = "Pesos por hora", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "bottom")
ggsave(file.path(dir_fig, "p2_salarios_formal_informal.pdf"), g_p2_sal,
       width = 8, height = 4.5)


############################################################
# PREGUNTA 3: Desigualdad del ingreso per cápita familiar por región
############################################################
# Universo: personas en hogares que declararon ingresos.
# Ponderador: PONDIH (el del ingreso per cápita familiar).
p3 <- eph |> filter(!is.na(ipcf), pondih > 0)

p3_region <- bind_rows(
  p3 |> group_by(region) |> resumen_ipcf() |> mutate(region = as.character(region)),
  p3 |> resumen_ipcf() |> mutate(region = "Total país")
) |>
  arrange(desc(gini))
guardar_tabla(p3_region, "p3_desigualdad_region", dir_res)

# Mediana relativa al total país (para comparar niveles entre regiones)
p3_region |>
  mutate(mediana_rel = ipcf_mediano / ipcf_mediano[region == "Total país"]) |>
  select(region, ipcf_mediano, mediana_rel) |>
  print()

# Curvas de Lorenz por región
lorenz <- p3 |>
  group_by(region) |>
  group_modify(~ curva_lorenz(.x$ipcf, .x$pondih)) |>
  ungroup()

g_p3_lorenz <- ggplot(lorenz, aes(prop_poblacion, prop_ingreso, colour = region)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_equal() +
  labs(title = "Curvas de Lorenz del ingreso per cápita familiar",
       subtitle = "Línea punteada: igualdad perfecta",
       x = "% acumulado de la población (de menor a mayor ingreso)",
       y = "% acumulado del ingreso", colour = NULL) +
  theme_minimal()
ggsave(file.path(dir_fig, "p3_lorenz.pdf"), g_p3_lorenz, width = 6.5, height = 5.5)

# Densidades del log del IPCF por región (forma de la distribución)
g_p3_dens <- p3 |>
  filter(ipcf > 0) |>
  ggplot(aes(ipcf, weight = pondih, colour = region)) +
  geom_density(linewidth = 0.7, adjust = 1.2) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(title = "Distribución del ingreso per cápita familiar por región",
       subtitle = "Densidades ponderadas por PONDIH (escala log)",
       x = "IPCF (pesos por mes, escala log)", y = "Densidad", colour = NULL) +
  theme_minimal()
# (ggplot normaliza los pesos dentro de cada región; el aviso de que el
#  ancho de banda se elige sin pesos es inofensivo y lo silenciamos)
suppressWarnings(
  ggsave(file.path(dir_fig, "p3_densidades.pdf"), g_p3_dens, width = 8, height = 4)
)

# Gini y mediana por región, en un gráfico
g_p3_gini <- p3_region |>
  mutate(region = fct_reorder(region, gini)) |>
  ggplot(aes(gini, region, fill = region == "Total país")) +
  geom_col() +
  geom_text(aes(label = sprintf("%.3f", gini)), hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c("steelblue", "grey40"), guide = "none") +
  coord_cartesian(xlim = c(0, 0.55)) +
  labs(title = "Coeficiente de Gini del IPCF por región", x = "Gini", y = NULL) +
  theme_minimal()
ggsave(file.path(dir_fig, "p3_gini.pdf"), g_p3_gini, width = 6.5, height = 3.5)

cat("\nAnálisis de datos terminado.\n")
