############################################################
# ANÁLISIS ECONOMÉTRICO con datos de gapminder
# Script PRINCIPAL (ejecuta las funciones de funciones.R)
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
#
# Cómo correrlo: con el working directory en la raíz del
# repositorio, correr el script completo de arriba hacia abajo.
############################################################

set.seed(2026)   # Semilla fija -> resultados reproducibles

suppressPackageStartupMessages({
  library(tidyverse)
  library(lmtest)     # bptest(): test de Breusch-Pagan (visto en clase)
  library(sandwich)   # vcovHC(): errores estándar robustos HC3
  library(car)        # vif(): factor de inflación de la varianza
})
source("analisis_econometrico/funciones.R")

dir_fig <- "analisis_econometrico/figuras"
dir_res <- "analisis_econometrico/resultados"
dir.create(dir_fig, showWarnings = FALSE)
dir.create(dir_res, showWarnings = FALSE)



############################################################
# 0) CARGA, LIMPIEZA Y CHEQUEOS DE LOS DATOS
############################################################
gapminder <- cargar_gapminder("analisis_econometrico/data/gapminder.csv")

# Estructura del panel: 193 países x 51 años (1960-2010), sin duplicados
glimpse(gapminder)
stopifnot(nrow(distinct(gapminder, country, year)) == nrow(gapminder))

# Faltantes por variable (después de recodificar "-" y -999 como NA)
faltantes <- gapminder |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA") |>
  filter(n_NA > 0)
guardar_tabla(faltantes, "faltantes", dir_res)

# ¿Qué países concentran los NA de esperanza de vida? (microestados)
gapminder |>
  filter(is.na(life_expectancy) | is.na(life_expectancy_female)) |>
  count(country) |>
  print()


############################################################
# PARTE 1 - INCISO 1: evolución del ingreso en Argentina
############################################################
argentina <- gapminder |>
  filter(country == "Argentina") |>
  mutate(t = year - 1960) |>        # t = 0 en 1960, ..., 50 en 2010
  select(year, t, income_per_person)

stopifnot(!anyNA(argentina$income_per_person), nrow(argentina) == 51)

g_arg <- ggplot(argentina, aes(year, income_per_person)) +
  geom_line(colour = "steelblue", linewidth = 0.9) +
  geom_point(colour = "steelblue", size = 1.2) +
  # Marcamos episodios de crisis para comentar la serie
  annotate("rect", xmin = c(1988, 1998), xmax = c(1990, 2002),
           ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "firebrick") +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Argentina: ingreso por persona, 1960-2010",
       subtitle = "Sombreado: hiperinflación (1988-90) y recesión/crisis de la convertibilidad (1998-2002)",
       x = "Año", y = "Ingreso por persona (USD PPA)") +
  theme_minimal()
ggsave(file.path(dir_fig, "p1_argentina_ingreso.pdf"), g_arg, width = 8, height = 4)

# Crecimiento promedio anual compuesto 1960-2010 y caídas más fuertes
crec_arg <- argentina |>
  mutate(crecimiento = crecimiento_interanual(income_per_person))
cat(sprintf("Crecimiento promedio anual compuesto 1960-2010: %.2f%%\n",
            100 * ((last(argentina$income_per_person) /
                    first(argentina$income_per_person))^(1/50) - 1)))
crec_arg |> arrange(crecimiento) |> head(5) |> print()


############################################################
# PARTE 1 - INCISO 2: train/test y regresiones polinómicas
############################################################
# Testeo = últimos 10 años (2001-2010). Entrenamiento = 1960-2000.
train_arg <- filter(argentina, year <= 2000)
test_arg  <- filter(argentina, year >  2000)

modelos_t <- list(
  "Lineal"    = ajustar_polinomio(train_arg, 1),
  "Grado 2"   = ajustar_polinomio(train_arg, 2),
  "Grado 10"  = ajustar_polinomio(train_arg, 10)
)
map(modelos_t, summary)

# Chequeo (como en clase): poly() ortogonal y potencias crudas dan las
# mismas predicciones en el modelo cuadrático
m_quad_crudo <- lm(income_per_person ~ t + I(t^2), data = train_arg)
stopifnot(isTRUE(all.equal(unname(fitted(m_quad_crudo)),
                           unname(fitted(modelos_t[["Grado 2"]])))))
coef(m_quad_crudo)   # coeficientes interpretables del modelo cuadrático

# Métricas en entrenamiento y testeo
metricas_t <- evaluar_modelos(modelos_t, train_arg, test_arg)
guardar_tabla(metricas_t, "p1_metricas_train_test", dir_res)

# Predicciones año a año en el período de testeo
pred_test <- test_arg |>
  mutate(lineal  = predict(modelos_t[["Lineal"]],   newdata = test_arg),
         grado2  = predict(modelos_t[["Grado 2"]],  newdata = test_arg),
         grado10 = predict(modelos_t[["Grado 10"]], newdata = test_arg))
guardar_tabla(pred_test, "p1_predicciones_test", dir_res)

# Gráfico 1: ajuste sobre todo el período (las curvas se extienden
# al período de testeo, sombreado)
grilla_t <- tibble(t = seq(0, 50, by = 0.25)) |> mutate(year = 1960 + t)
curvas_t <- predecir_grilla(modelos_t, grilla_t) |>
  mutate(modelo = factor(modelo, levels = names(modelos_t)))

datos_plot <- argentina |>
  mutate(conjunto = if_else(year <= 2000, "Entrenamiento", "Testeo"))

g_ajustes <- ggplot() +
  annotate("rect", xmin = 2000.5, xmax = 2010.5, ymin = -Inf, ymax = Inf,
           alpha = 0.10, fill = "grey40") +
  geom_point(data = datos_plot, aes(year, income_per_person, shape = conjunto),
             size = 1.8) +
  geom_line(data = curvas_t, aes(year, prediccion, colour = modelo),
            linewidth = 0.9) +
  coord_cartesian(ylim = c(5000, 25000)) +   # el grado 10 se dispara fuera de rango
  scale_shape_manual(values = c(16, 1)) +
  scale_colour_manual(values = c("darkgreen", "steelblue", "darkorange")) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Argentina: ajustes polinómicos del ingreso por persona sobre el tiempo",
       subtitle = "Entrenamiento 1960-2000 (puntos llenos); testeo 2001-2010 (puntos vacíos, zona sombreada)",
       x = "Año", y = "Ingreso por persona", colour = "Modelo", shape = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "p1_ajustes_polinomicos.pdf"), g_ajustes, width = 8, height = 4.8)

# Gráfico 2: zoom en el período de testeo, en escala libre, para ver
# hacia dónde extrapola cada modelo
pred_test_largo <- pred_test |>
  pivot_longer(c(lineal, grado2, grado10), names_to = "modelo",
               values_to = "prediccion") |>
  mutate(modelo = dplyr::recode(modelo, lineal = "Lineal", grado2 = "Grado 2",
                         grado10 = "Grado 10"),
         modelo = factor(modelo, levels = names(modelos_t)))

g_test <- ggplot(pred_test_largo, aes(year, prediccion, colour = modelo)) +
  geom_line(linewidth = 0.9) +
  geom_point(data = test_arg, aes(year, income_per_person), inherit.aes = FALSE,
             size = 2) +
  facet_wrap(~ modelo, scales = "free_y") +
  scale_colour_manual(values = c("darkgreen", "steelblue", "darkorange")) +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Conjunto de testeo (2001-2010): observado (puntos) vs. predicho (línea)",
       subtitle = "Ojo: cada panel tiene su propia escala vertical",
       x = "Año", y = "Ingreso por persona") +
  theme_minimal() + theme(legend.position = "none")
ggsave(file.path(dir_fig, "p1_testeo.pdf"), g_test, width = 9, height = 3.5)


############################################################
# PARTE 1 - INCISO 3: correlaciones con otros países sudamericanos
############################################################
paises <- c("Argentina", "Brazil", "Chile", "Uruguay", "Venezuela")

# (a) Niveles de ingreso
ingresos_ancho <- a_formato_ancho(gapminder, paises, "income_per_person")
stopifnot(!anyNA(ingresos_ancho))
cor_niveles <- cor(select(ingresos_ancho, -year))
print(round(cor_niveles, 3))

# (b) Crecimiento interanual Delta Y / Y (se pierde 1960)
crecimiento_ancho <- ingresos_ancho |>
  mutate(across(-year, crecimiento_interanual)) |>
  filter(year > 1960)
cor_crecimiento <- cor(select(crecimiento_ancho, -year))
print(round(cor_crecimiento, 3))

write_csv(as_tibble(cor_niveles, rownames = "pais"),
          file.path(dir_res, "p1_cor_niveles.csv"))
write_csv(as_tibble(cor_crecimiento, rownames = "pais"),
          file.path(dir_res, "p1_cor_crecimiento.csv"))

g_cor <- patchwork::wrap_plots(
  graficar_correlaciones(cor_niveles, "(a) Ingreso por persona (niveles)"),
  graficar_correlaciones(cor_crecimiento, "(b) Crecimiento interanual"),
  ncol = 2, guides = "collect")
ggsave(file.path(dir_fig, "p1_correlaciones.pdf"), g_cor, width = 11, height = 4.5)

# Series de los 5 países para acompañar la discusión
g_series <- gapminder |>
  filter(country %in% paises) |>
  group_by(country) |>
  mutate(crecimiento = 100 * crecimiento_interanual(income_per_person)) |>
  ungroup() |>
  select(year, country, `Ingreso por persona` = income_per_person,
         `Crecimiento interanual (%)` = crecimiento) |>
  pivot_longer(-c(year, country)) |>
  ggplot(aes(year, value, colour = country)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "Argentina y cuatro países sudamericanos", x = "Año",
       y = NULL, colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "p1_series_paises.pdf"), g_series, width = 10, height = 4)

# Volatilidad del crecimiento (desvío estándar) para la discusión
crecimiento_ancho |>
  summarise(across(-year, list(media = mean, desvio = sd))) |>
  pivot_longer(everything()) |>
  print(n = Inf)


############################################################
# PARTE 2: Esperanza de vida y género (año 2010)
############################################################
# Elegimos 2010: es el año más reciente del panel. Nos quedamos con
# los países con datos completos en las variables usadas en 5-8.
anio <- 2010
datos_anio <- gapminder |>
  filter(year == anio) |>
  drop_na(life_expectancy, life_expectancy_female, income_per_person)
cat("Países con datos completos en", anio, ":", nrow(datos_anio), "\n")

# Chequeo de consistencia: diferencia entre esperanza de vida
# femenina y total. Por definición la total es un promedio de la
# femenina y la masculina, así que debería ser > 0 casi siempre.
datos_anio <- mutate(datos_anio, dif = life_expectancy_female - life_expectancy)
summary(datos_anio$dif)
cat("Proporción de países con esperanza femenina < total:",
    round(mean(datos_anio$dif < 0), 3), "\n")
datos_anio |> arrange(dif) |>
  select(country, life_expectancy, life_expectancy_female,
         life_expectancy_male, dif) |>
  head(8) |> print()


############################################################
# INCISO 5: gráfico life_expectancy vs life_expectancy_female
############################################################
g_le <- ggplot(datos_anio, aes(life_expectancy_female, life_expectancy)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(aes(colour = world_region), alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", se = TRUE,
              linewidth = 0.7) +
  labs(title = sprintf("Esperanza de vida total vs. femenina (%d)", anio),
       subtitle = "Línea punteada: 45° (total = femenina). Línea negra: ajuste MCO",
       x = "Esperanza de vida femenina (años)", y = "Esperanza de vida total (años)",
       colour = "Región") +
  theme_minimal()
ggsave(file.path(dir_fig, "p2_le_vs_lef.pdf"), g_le, width = 8, height = 5)

cat("Correlación:", cor(datos_anio$life_expectancy,
                        datos_anio$life_expectancy_female), "\n")


############################################################
# INCISO 6: regresión simple life_expectancy ~ life_expectancy_female
############################################################
m_simple <- lm(life_expectancy ~ life_expectancy_female, data = datos_anio)
summary(m_simple)
guardar_tabla(tabla_coeficientes(m_simple), "p2_coef_simple", dir_res)
cat(sprintf("R^2 (summary) = %.4f | R^2 (manual) = %.4f | cor^2 = %.4f\n",
            summary(m_simple)$r.squared, r2_manual(m_simple),
            cor(datos_anio$life_expectancy, datos_anio$life_expectancy_female)^2))

# ¿La pendiente es 1? (si total y femenina se movieran uno a uno)
guardar_tabla(test_t_coeficiente(m_simple, "life_expectancy_female", 1),
              "p2_test_pendiente_1", dir_res)
confint(m_simple)


############################################################
# INCISO 7: H0: femenina = total  vs  H1: femenina > total
############################################################
# Son mediciones apareadas (mismo país), así que contrastamos sobre la
# diferencia d_i = femenina_i - total_i:  H0: E[d] = 0 vs H1: E[d] > 0.
test_apareado <- t.test(datos_anio$life_expectancy_female,
                        datos_anio$life_expectancy,
                        paired = TRUE, alternative = "greater")
print(test_apareado)

# Equivalente con regresión (como en clase): d ~ 1, la constante es la
# media de d; el p-valor unilateral es la mitad del bilateral si t > 0.
m_dif <- lm(dif ~ 1, data = datos_anio)
t_dif <- coef(summary(m_dif))[1, "t value"]
p_unilateral <- pt(t_dif, df = df.residual(m_dif), lower.tail = FALSE)
cat(sprintf("Media de d = %.3f, t = %.3f, p-valor unilateral = %.3g\n",
            coef(m_dif), t_dif, p_unilateral))

# Robustez: test de rangos con signo de Wilcoxon (no supone normalidad)
print(wilcox.test(datos_anio$life_expectancy_female, datos_anio$life_expectancy,
                  paired = TRUE, alternative = "greater"))

resumen_test <- tibble(
  n = nrow(datos_anio), media_dif = mean(datos_anio$dif),
  desvio_dif = sd(datos_anio$dif), t = unname(test_apareado$statistic),
  gl = unname(test_apareado$parameter), p_valor = test_apareado$p.value,
  ic95_inf = test_apareado$conf.int[1],
  prop_negativas = mean(datos_anio$dif < 0)
)
guardar_tabla(resumen_test, "p2_test_apareado", dir_res)

g_dif <- ggplot(datos_anio, aes(dif)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean(datos_anio$dif), colour = "firebrick") +
  labs(title = "Diferencia entre esperanza de vida femenina y total por país",
       subtitle = "Rojo: media. Punteada: 0 (hipótesis nula)",
       x = "Femenina - total (años)", y = "Cantidad de países") +
  theme_minimal()
ggsave(file.path(dir_fig, "p2_diferencias.pdf"), g_dif, width = 7, height = 4)


############################################################
# INCISO 8: regresión múltiple con income_per_person
############################################################
m_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person,
                 data = datos_anio)
summary(m_multiple)
guardar_tabla(tabla_coeficientes(m_multiple), "p2_coef_multiple", dir_res)

comparacion_r2 <- tibble(
  modelo = c("Simple", "Múltiple"),
  r2 = c(summary(m_simple)$r.squared, summary(m_multiple)$r.squared),
  r2_ajustado = c(summary(m_simple)$adj.r.squared, summary(m_multiple)$adj.r.squared),
  coef_lef = c(coef(m_simple)[2], coef(m_multiple)[2])
)
guardar_tabla(comparacion_r2, "p2_comparacion_r2", dir_res)

# Test F de modelos anidados: ¿agregar income_per_person mejora el ajuste?
print(anova(m_simple, m_multiple))

# Correlación entre los regresores (ayuda a entender el cambio en beta)
cat("cor(lef, income) =", cor(datos_anio$life_expectancy_female,
                              datos_anio$income_per_person), "\n")


############################################################
# INCISO 9: modelo de hasta tres covariables (sin lef)
############################################################
# Paso 1: variables excluidas y por qué (ver informe):
#   - life_expectancy_female (lo pide la consigna) y life_expectancy_male:
#     la total es, por construcción, un promedio de ambas -> "explicaría"
#     la variable con ella misma.
#   - country: identificador (una observación por país).
#   - income_groups: es una discretización de income_per_person.
#   - world_region y main_religion: categóricas con muchos niveles
#     (cada nivel consumiría una de las tres covariables); main_religion
#     además tiene NA. De world_region rescatamos sólo la región con un
#     comportamiento claramente distinto (África subsahariana) como dummy.
#   - year: constante dentro del corte transversal.

# Paso 2: construir las candidatas (incluye columnas nuevas)
datos_mod <- gapminder |>
  filter(year == anio) |>
  mutate(log_income          = log(income_per_person),
         log_child_mortality = log(child_mortality),
         log_population      = log(population),
         oecd                = as.integer(is_oecd),
         africa_subsahariana = as.integer(world_region == "sub_saharan_africa")) |>
  drop_na(life_expectancy, income_per_person, child_mortality,
          children_per_woman, population)
cat("Observaciones para el inciso 9:", nrow(datos_mod), "\n")

# Paso 3: ¿niveles o logaritmos? Comparamos la correlación de cada
# versión con life_expectancy y miramos los diagramas de dispersión.
cor_transformaciones <- tibble(
  variable = c("income_per_person", "log_income", "child_mortality",
               "log_child_mortality", "population", "log_population",
               "children_per_woman", "oecd", "africa_subsahariana"),
  cor_con_le = map_dbl(variable, ~ cor(datos_mod$life_expectancy, datos_mod[[.x]]))
)
guardar_tabla(cor_transformaciones, "p2_cor_candidatas", dir_res)

g_disp <- datos_mod |>
  select(life_expectancy, income_per_person, log_income, child_mortality,
         log_child_mortality, children_per_woman, log_population) |>
  pivot_longer(-life_expectancy) |>
  mutate(name = factor(name, levels = c("income_per_person", "log_income",
                                        "child_mortality", "log_child_mortality",
                                        "children_per_woman", "log_population"))) |>
  ggplot(aes(value, life_expectancy)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "firebrick") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "steelblue",
              linewidth = 0.6) +
  facet_wrap(~ name, scales = "free_x", ncol = 3) +
  labs(title = sprintf("Esperanza de vida (%d) vs. covariables candidatas", anio),
       subtitle = "Rojo: recta MCO. Azul: suavizado loess (para detectar no linealidad)",
       x = NULL, y = "Esperanza de vida") +
  theme_minimal()
ggsave(file.path(dir_fig, "p2_candidatas.pdf"), g_disp, width = 9, height = 5.5)

# Nos quedamos con la versión (nivel o log) más lineal de cada variable
candidatas <- c("log_income", "log_child_mortality", "children_per_woman",
                "log_population", "oecd", "africa_subsahariana")

# Matriz de correlación entre candidatas (multicolinealidad)
cor_candidatas <- cor(datos_mod[, candidatas])
print(round(cor_candidatas, 2))
g_cor_cand <- graficar_correlaciones(cor_candidatas,
                                     "Correlaciones entre covariables candidatas")
ggsave(file.path(dir_fig, "p2_cor_candidatas.pdf"), g_cor_cand, width = 7, height = 5.5)

# Paso 4: train/test 70/30 y búsqueda exhaustiva de 1 a 3 covariables
id_train  <- sample(seq_len(nrow(datos_mod)), size = round(0.7 * nrow(datos_mod)))
train_mod <- datos_mod[id_train, ]
test_mod  <- datos_mod[-id_train, ]

ranking <- comparar_subconjuntos(train_mod, test_mod, "life_expectancy",
                                 candidatas, max_cov = 3) |>
  arrange(ecm_test)
guardar_tabla(ranking, "p2_ranking_modelos", dir_res)

# Mejor modelo para cada cantidad de covariables
mejores_por_tamano <- ranking |> group_by(n_cov) |> slice_min(ecm_test, n = 1) |> ungroup()
guardar_tabla(mejores_por_tamano, "p2_mejores_por_tamano", dir_res)

# Paso 5: modelo elegido, re-estimado con todos los datos de 2010
formula_final <- reformulate(strsplit(ranking$covariables[1], " \\+ ")[[1]],
                             response = "life_expectancy")
m_final <- lm(formula_final, data = datos_mod)
print(formula_final)
summary(m_final)
guardar_tabla(tabla_coeficientes(m_final), "p2_coef_final", dir_res)

# Paso 6: diagnóstico del modelo elegido
print(vif(m_final))                 # multicolinealidad (VIF > 5-10 preocupa)
print(bptest(m_final))              # H0: homocedasticidad
coef_robustos <- coeftest(m_final, vcov = vcovHC(m_final, type = "HC3"))
print(coef_robustos)                # SE robustos a heterocedasticidad
write_csv(as_tibble(unclass(coef_robustos), rownames = "termino"),
          file.path(dir_res, "p2_coef_final_robustos.csv"))

pdf(file.path(dir_fig, "p2_diagnostico_final.pdf"), width = 9, height = 3.5)
par(mfrow = c(1, 3))
plot(fitted(m_final), resid(m_final), pch = 16, col = rgb(0, 0, 1, 0.4),
     xlab = "Ajustados", ylab = "Residuos", main = "Residuos vs. ajustados")
abline(h = 0, col = "red", lwd = 2)
qqnorm(resid(m_final), pch = 16, col = rgb(0, 0, 1, 0.4), main = "QQ-plot residuos")
qqline(resid(m_final), col = "red", lwd = 2)
plot(cooks.distance(m_final), type = "h", main = "Distancia de Cook",
     ylab = "D_i")
abline(h = 4 / nrow(datos_mod), col = "red", lty = 2)
par(mfrow = c(1, 1))
dev.off()

# Países más influyentes / peor ajustados
datos_mod |>
  mutate(residuo = resid(m_final), cook = cooks.distance(m_final)) |>
  arrange(desc(cook)) |>
  select(country, life_expectancy, residuo, cook) |>
  head(6) |> print()

# Robustez: Haití 2010 (terremoto) es un outlier extremo. Re-estimamos
# la regresión simple, la múltiple y el modelo final sin Haití.
sin_haiti <- filter(datos_anio, country != "Haiti")
robustez_haiti <- tibble(
  modelo = c("Simple (inciso 6)", "Múltiple (inciso 8)", "Final (inciso 9)"),
  r2_con_haiti = c(summary(m_simple)$r.squared, summary(m_multiple)$r.squared,
                   summary(m_final)$r.squared),
  r2_sin_haiti = c(
    summary(update(m_simple,   data = sin_haiti))$r.squared,
    summary(update(m_multiple, data = sin_haiti))$r.squared,
    summary(update(m_final,    data = filter(datos_mod, country != "Haiti")))$r.squared),
  coef_lef_sin_haiti = c(coef(update(m_simple, data = sin_haiti))[2],
                         coef(update(m_multiple, data = sin_haiti))[2], NA),
  p_income_sin_haiti = c(NA, coef(summary(update(m_multiple, data = sin_haiti)))[3, 4], NA)
)
guardar_tabla(robustez_haiti, "p2_robustez_haiti", dir_res)
print(summary(update(m_final, data = filter(datos_mod, country != "Haiti"))))

cat("\nAnálisis econométrico terminado.\n")
