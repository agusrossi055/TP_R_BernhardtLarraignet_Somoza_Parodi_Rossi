############################################################
# SIMULACIÓN: Demanda con preferencias Cobb-Douglas
# Script PRINCIPAL (ejecuta las funciones de funciones.R)
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
#
# Cómo correrlo: abrir el proyecto desde la carpeta raíz del
# repositorio (el working directory tiene que ser la raíz) y
# correr el script completo de arriba hacia abajo.
############################################################

set.seed(2026)   # Semilla fija -> resultados reproducibles

suppressPackageStartupMessages(library(tidyverse))
source("simulacion/funciones.R")

# Carpetas de salida
dir_fig <- "simulacion/figuras"
dir_res <- "simulacion/resultados"
dir.create(dir_fig, showWarnings = FALSE)
dir.create(dir_res, showWarnings = FALSE)



############################################################
# PARÁMETROS DEL EJERCICIO (inciso 3: los explicitamos)
############################################################
n_hogares <- 10000   # cantidad de hogares simulados
k         <- 10      # grados de libertad del ingreso: E[Y] = 10
p1        <- 2       # precio del bien 1
p2        <- 1       # precio del bien 2
alpha1    <- 0.4     # participación del bien 1 en el gasto
alpha2    <- 1 - alpha1
umbral_c  <- 1       # umbral de "bajo consumo" del bien 1 (= mitad de E[x1])
shock     <- 1.20    # el precio del bien 1 sube 20%


############################################################
# INCISO 1: Generación de ingresos y elección de k
############################################################
# Simulamos ingresos para varios k y comparamos media, desvío,
# coeficiente de variación (CV), asimetría y Gini con la teoría.
k_grilla <- c(3, 10, 30)

ingresos_k <- map_dfr(k_grilla, ~ tibble(k = .x, Y = simular_ingreso(n_hogares, .x)))

tabla_k <- ingresos_k |>
  group_by(k) |>
  summarise(
    media_sim  = mean(Y),
    media_teo  = first(k),
    desvio_sim = sd(Y),
    desvio_teo = sqrt(2 * first(k)),
    cv_sim     = sd(Y) / mean(Y),
    cv_teo     = sqrt(2 / first(k)),
    gini_sim   = gini(Y),
    .groups = "drop"
  )
print(tabla_k)
guardar_tabla(tabla_k, "inciso1_dispersion_k", dir_res)

# Gráfico: histograma simulado + densidad teórica chi^2 para cada k
curvas_teo <- map_dfr(k_grilla, ~ tibble(k = .x, Y = seq(0.01, 70, length.out = 500),
                                          densidad = dchisq(Y, df = .x)))

g_k <- ggplot(ingresos_k, aes(x = Y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80,
                 fill = "steelblue", alpha = 0.5) +
  geom_line(data = curvas_teo, aes(y = densidad), colour = "firebrick",
            linewidth = 0.8) +
  facet_wrap(~ k, scales = "free_y",
             labeller = label_both) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Ingreso simulado Y ~ Chi^2(k)",
       subtitle = "Histograma simulado (azul) y densidad teórica (rojo)",
       x = "Ingreso mensual Y", y = "Densidad") +
  theme_minimal()
ggsave(file.path(dir_fig, "inciso1_ingresos_k.pdf"), g_k, width = 9, height = 3.5)


############################################################
# INCISO 2: chequeo de la función de demanda
############################################################
# Verificamos con un caso a mano: Y = 10, p1 = 2, p2 = 1, alpha1 = 0.4
#   x1* = 0.4*10/2 = 2 ; x2* = 0.6*10/1 = 6 ; U* = 2^0.4 * 6^0.6
chequeo <- demanda_cd(Y = 10, p1 = 2, p2 = 1, alpha1 = 0.4, alpha2 = 0.6)
print(chequeo)
stopifnot(all.equal(chequeo$x1, 2), all.equal(chequeo$x2, 6),
          all.equal(chequeo$U, 2^0.4 * 6^0.6))
# También verificamos que se agota el presupuesto: p1*x1 + p2*x2 = Y
stopifnot(all.equal(p1 * chequeo$x1 + p2 * chequeo$x2, 10))


############################################################
# INCISO 3: Simulación base (preferencias homogéneas)
############################################################
Y <- simular_ingreso(n_hogares, k)   # ingresos de los 10.000 hogares
base <- simular_hogares(Y, p1, p2, alpha1)

resumen_base <- resumir_poblacion(base)
print(resumen_base)
guardar_tabla(resumen_base, "inciso3_resumen_base", dir_res)

# Valores teóricos para contrastar las medias simuladas:
#   E[x1] = alpha1*k/p1 ; E[x2] = alpha2*k/p2
#   E[U]  = k * (alpha1/p1)^alpha1 * (alpha2/p2)^alpha2   (U* es lineal en Y)
medias_teoricas <- tibble(
  variable  = c("x1", "x2", "U"),
  media_teo = c(alpha1 * k / p1, alpha2 * k / p2,
                k * (alpha1 / p1)^alpha1 * (alpha2 / p2)^alpha2)
) |>
  left_join(select(resumen_base, variable, media_sim = media), by = "variable")
print(medias_teoricas)
guardar_tabla(medias_teoricas, "inciso3_medias_teoricas", dir_res)

# Distribuciones empíricas de x1, x2 y U (un panel por variable)
g_base <- base |>
  select(x1, x2, U) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = c("x1", "x2", "U"))) |>
  ggplot(aes(x = valor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 70,
                 fill = "steelblue", alpha = 0.6) +
  geom_density(colour = "black", linewidth = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribuciones empíricas: caso base homogéneo",
       subtitle = sprintf("n = %d, k = %d, p1 = %g, p2 = %g, alpha1 = %g",
                          n_hogares, k, p1, p2, alpha1),
       x = NULL, y = "Densidad") +
  theme_minimal()
ggsave(file.path(dir_fig, "inciso3_distribuciones_base.pdf"), g_base,
       width = 9, height = 3.5)


############################################################
# INCISO 4: Probabilidad de bajo consumo
############################################################
# Estimación por simulación y valor exacto:
#   P(x1 < c) = P(Y < c*p1/alpha1) = pchisq(c*p1/alpha1, k)
p_bajo_sim <- prob_bajo_consumo(base, c = umbral_c, j = 1)
p_bajo_teo <- pchisq(umbral_c * p1 / alpha1, df = k)
cat(sprintf("P(x1 < %g): simulada = %.4f | teórica = %.4f\n",
            umbral_c, p_bajo_sim, p_bajo_teo))

# La función sirve para cualquier umbral y bien: armamos una grilla
grilla_umbral <- expand_grid(j = c(1, 2), c = c(0.5, 1, 2, 4)) |>
  mutate(
    p_sim = map2_dbl(c, j, ~ prob_bajo_consumo(base, c = .x, j = .y)),
    p_teo = pchisq(c * c(p1, p2)[j] / c(alpha1, alpha2)[j], df = k)
  )
print(grilla_umbral)
guardar_tabla(grilla_umbral, "inciso4_prob_bajo_consumo", dir_res)


############################################################
# INCISO 5: Shock de precios (p1' = 1.2 p1), mismos ingresos
############################################################
# Usamos EXACTAMENTE los mismos hogares (mismo vector Y): la única
# diferencia entre escenarios es el precio del bien 1.
post <- simular_hogares(Y, shock * p1, p2, alpha1)

comparacion_shock <- bind_rows(
  mutate(resumir_poblacion(base), escenario = "Pre"),
  mutate(resumir_poblacion(post), escenario = "Post")
) |>
  select(escenario, everything()) |>
  arrange(variable, desc(escenario))
print(comparacion_shock)
guardar_tabla(comparacion_shock, "inciso5_pre_post", dir_res)

# Cambio porcentual hogar por hogar: en Cobb-Douglas debería ser
#   x1: 1/1.2 - 1 = -16.7% para TODOS los hogares
#   x2: 0%
#   U : 1.2^(-alpha1) - 1 para TODOS los hogares
cambios <- tibble(
  variable = c("x1", "x2", "U"),
  cambio_medio_sim = c(mean(post$x1 / base$x1 - 1),
                       mean(post$x2 / base$x2 - 1),
                       mean(post$U  / base$U  - 1)),
  cambio_teo = c(1 / shock - 1, 0, shock^(-alpha1) - 1)
)
print(cambios)
guardar_tabla(cambios, "inciso5_cambios_porcentuales", dir_res)


############################################################
# INCISO 6: Visualización comparada pre/post
############################################################
g_shock_x1 <- graficar_pre_post(base, post, "x1",
                                "Consumo del bien 1 antes y después del shock",
                                umbral = umbral_c)
g_shock_U  <- graficar_pre_post(base, post, "U",
                                "Utilidad indirecta antes y después del shock")
ggsave(file.path(dir_fig, "inciso6_x1_pre_post.pdf"), g_shock_x1, width = 7, height = 4)
ggsave(file.path(dir_fig, "inciso6_U_pre_post.pdf"),  g_shock_U,  width = 7, height = 4)

# Masa de hogares con bajo consumo y utilidad media pre/post
bajo_consumo_shock <- tibble(
  escenario = c("Pre", "Post"),
  p_bajo_sim = c(prob_bajo_consumo(base, umbral_c, 1),
                 prob_bajo_consumo(post, umbral_c, 1)),
  p_bajo_teo = pchisq(umbral_c * c(p1, shock * p1) / alpha1, df = k),
  U_media    = c(mean(base$U), mean(post$U))
)
print(bajo_consumo_shock)
guardar_tabla(bajo_consumo_shock, "inciso6_bajo_consumo_y_U", dir_res)


############################################################
# INCISO 7: Heterogeneidad en preferencias, alpha1 ~ Beta(a, b)
############################################################
# Elegimos dos Betas con la MISMA media que el caso homogéneo
# (E[alpha1] = a/(a+b) = 0.4) pero distinta dispersión, para aislar
# el efecto de la heterogeneidad:
#   - Beta(4, 6):     heterogeneidad moderada (desvío ~ 0.15)
#   - Beta(0.8, 1.2): heterogeneidad alta     (desvío ~ 0.28)
# Los ingresos Y son los mismos de antes (números aleatorios comunes).
escenarios_beta <- tribble(
  ~escenario,                ~a,  ~b,
  "Beta(4, 6)",               4,   6,
  "Beta(0.8, 1.2)",         0.8, 1.2
)

# Para cada escenario sorteamos un alpha1 por hogar y repetimos 3-6
heterog <- escenarios_beta |>
  mutate(alpha1_hogar = map2(a, b, ~ rbeta(n_hogares, .x, .y)),
         pre  = map(alpha1_hogar, ~ simular_hogares(Y, p1,         p2, .x)),
         post = map(alpha1_hogar, ~ simular_hogares(Y, shock * p1, p2, .x)))

# Agregamos el caso homogéneo como referencia, con la misma estructura
todos <- bind_rows(
  tibble(escenario = "Homogéneo (alpha1 = 0.4)", pre = list(base), post = list(post)),
  select(heterog, escenario, pre, post)
) |>
  mutate(escenario = factor(escenario, levels = escenario))

# 7.a) Inciso 3 con heterogeneidad: medias y cuartiles de x1, x2, U
resumen_heterog <- todos |>
  mutate(resumen = map(pre, resumir_poblacion)) |>
  select(escenario, resumen) |>
  unnest(resumen) |>
  arrange(variable, escenario)
print(resumen_heterog)
guardar_tabla(resumen_heterog, "inciso7_resumen_heterogeneo", dir_res)

# Distribución de los alpha1 sorteados
g_alpha <- heterog |>
  select(escenario, alpha1_hogar) |>
  unnest(alpha1_hogar) |>
  ggplot(aes(x = alpha1_hogar, fill = escenario)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, alpha = 0.5,
                 position = "identity") +
  geom_vline(xintercept = alpha1, linetype = "dashed") +
  labs(title = "Distribución de alpha1 entre hogares",
       subtitle = "Línea punteada: caso homogéneo alpha1 = 0.4 (misma media)",
       x = "alpha1", y = "Densidad", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "inciso7_alpha1.pdf"), g_alpha, width = 7, height = 4)

# 7.b) Incisos 4-6 con heterogeneidad: bajo consumo, shock y utilidad
sensibilidad <- todos |>
  mutate(
    p_bajo_pre    = map_dbl(pre,  ~ prob_bajo_consumo(.x, umbral_c, 1)),
    p_bajo_post   = map_dbl(post, ~ prob_bajo_consumo(.x, umbral_c, 1)),
    x1_media_pre  = map_dbl(pre,  ~ mean(.x$x1)),
    x1_media_post = map_dbl(post, ~ mean(.x$x1)),
    U_media_pre   = map_dbl(pre,  ~ mean(.x$U)),
    U_media_post  = map_dbl(post, ~ mean(.x$U)),
    cambio_U_medio = map2_dbl(pre, post, ~ mean(.y$U / .x$U - 1)),
    # dispersión de la pérdida de utilidad entre hogares
    cambio_U_p10  = map2_dbl(pre, post, ~ quantile(.y$U / .x$U - 1, 0.10)),
    cambio_U_p90  = map2_dbl(pre, post, ~ quantile(.y$U / .x$U - 1, 0.90)),
    # Variación compensatoria: ingreso extra que deja al hogar con su
    # utilidad pre-shock. Con Cobb-Douglas VC_i = Y_i * (1.2^alpha1_i - 1).
    # Está en unidades monetarias -> comparable entre hogares con
    # distintas preferencias (la utilidad es ordinal y no lo es).
    vc_media      = map_dbl(pre, ~ mean(.x$Y * (shock^.x$alpha1 - 1))),
    vc_p90        = map_dbl(pre, ~ quantile(.x$Y * (shock^.x$alpha1 - 1), 0.90))
  ) |>
  select(-pre, -post)
print(sensibilidad)
guardar_tabla(sensibilidad, "inciso7_sensibilidad_shock", dir_res)

# Histogramas superpuestos de x1 pre/post para cada escenario
datos_x1 <- todos |>
  mutate(datos = map2(pre, post, ~ bind_rows(
    mutate(.x, momento = "Pre shock"),
    mutate(.y, momento = "Post shock (p1 +20%)")))) |>
  select(escenario, datos) |>
  unnest(datos) |>
  mutate(momento = factor(momento, levels = c("Pre shock", "Post shock (p1 +20%)")))

g_x1_heterog <- ggplot(datos_x1, aes(x = x1, fill = momento)) +
  geom_histogram(aes(y = after_stat(density)), bins = 80, alpha = 0.45,
                 position = "identity") +
  geom_vline(xintercept = umbral_c, linetype = "dashed") +
  facet_wrap(~ escenario, scales = "free_y") +
  coord_cartesian(xlim = c(0, 10)) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(title = "Consumo del bien 1 pre y post shock según heterogeneidad de preferencias",
       subtitle = sprintf("Línea punteada: umbral de bajo consumo c = %g", umbral_c),
       x = "x1", y = "Densidad", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "inciso7_x1_pre_post.pdf"), g_x1_heterog, width = 10, height = 4)

# Pérdida de utilidad por hogar en función de su alpha1:
# en Cobb-Douglas U_post/U_pre = 1.2^(-alpha1), así que quien más
# valora el bien 1 es quien más pierde.
g_perdida <- todos |>
  filter(escenario != "Homogéneo (alpha1 = 0.4)") |>
  mutate(datos = map2(pre, post, ~ tibble(alpha1 = .x$alpha1,
                                          cambio_U = .y$U / .x$U - 1))) |>
  select(escenario, datos) |>
  unnest(datos) |>
  ggplot(aes(x = 100 * cambio_U, fill = escenario)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, alpha = 0.5,
                 position = "identity") +
  geom_vline(xintercept = 100 * (shock^(-alpha1) - 1), linetype = "dashed") +
  labs(title = "Distribución del cambio en la utilidad indirecta entre hogares",
       subtitle = "Cada hogar pierde 1.2^(-alpha1) - 1. Línea punteada: caso homogéneo (-7.0%)",
       x = "Cambio en U* por hogar (%)", y = "Densidad", fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(dir_fig, "inciso7_perdida_utilidad.pdf"), g_perdida, width = 7, height = 4)

cat("\nSimulación terminada. Figuras en", dir_fig, "y tablas en", dir_res, "\n")
