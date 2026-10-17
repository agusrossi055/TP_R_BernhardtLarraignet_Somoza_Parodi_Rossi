############################################################
# SIMULACIÓN: Demanda con preferencias Cobb-Douglas
# Archivo de FUNCIONES (se cargan desde script_principal.R
# con source(); acá no se ejecuta nada).
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
############################################################


# ----------------------------------------------------------
# 1) Generación de ingresos
# ----------------------------------------------------------
# simular_ingreso(n, k)
#   Genera el ingreso mensual de n hogares suponiendo Y ~ Chi^2(k).
#   Momentos teóricos útiles para elegir k:
#     E[Y]   = k
#     Var[Y] = 2k          -> sd(Y) = sqrt(2k)
#     CV     = sqrt(2/k)   (dispersión relativa a la media)
#     Asimetría = sqrt(8/k)
#   Como E[Y] = k, k se elige para que el ingreso medio sea el
#   deseado (en las unidades monetarias que se usen).
simular_ingreso <- function(n, k) {
  # Chequeos de robustez (stop() con mensajes informativos)
  if (n < 1 || n != round(n)) stop("n debe ser un entero positivo.")
  if (k <= 0) stop("Los grados de libertad k deben ser positivos.")
  rchisq(n, df = k)
}


# ----------------------------------------------------------
# 2) Demanda óptima Cobb-Douglas
# ----------------------------------------------------------
# demanda_cd(Y, p1, p2, alpha1, alpha2)
#   Devuelve las demandas marshallianas
#     x1* = alpha1 * Y / p1 ,  x2* = alpha2 * Y / p2
#   y la utilidad indirecta U* = (x1*)^alpha1 * (x2*)^alpha2.
#   Está vectorizada: Y y alpha1/alpha2 pueden ser vectores (un
#   valor por hogar), lo que permite reutilizarla en el caso con
#   preferencias heterogéneas (inciso 7).
demanda_cd <- function(Y, p1, p2, alpha1, alpha2) {
  if (any(Y <= 0))               stop("El ingreso Y debe ser positivo.")
  if (p1 <= 0 || p2 <= 0)        stop("Los precios deben ser positivos.")
  if (any(alpha1 <= 0 | alpha1 >= 1) || any(alpha2 <= 0 | alpha2 >= 1))
    stop("alpha1 y alpha2 deben estar en (0, 1).")
  if (any(abs(alpha1 + alpha2 - 1) > 1e-10))
    stop("alpha1 + alpha2 debe ser igual a 1.")

  x1 <- alpha1 * Y / p1
  x2 <- alpha2 * Y / p2
  U  <- x1^alpha1 * x2^alpha2

  tibble::tibble(x1 = x1, x2 = x2, U = U)
}


# ----------------------------------------------------------
# 3) Simulación de una población de hogares
# ----------------------------------------------------------
# simular_hogares(Y, p1, p2, alpha1)
#   Envuelve a demanda_cd() para devolver una tabla con un hogar
#   por fila: ingreso, preferencias, demandas y utilidad.
#   Se le pasa el vector de ingresos YA simulado, así podemos usar
#   exactamente los mismos hogares antes y después del shock
#   ("números aleatorios comunes"): la única diferencia entre
#   escenarios es el precio, no el ruido de la simulación.
simular_hogares <- function(Y, p1, p2, alpha1) {
  alpha2 <- 1 - alpha1
  dplyr::bind_cols(
    tibble::tibble(hogar = seq_along(Y), Y = Y,
                   alpha1 = alpha1, alpha2 = alpha2),
    demanda_cd(Y, p1, p2, alpha1, alpha2)
  )
}


# ----------------------------------------------------------
# 4) Probabilidad de bajo consumo
# ----------------------------------------------------------
# prob_bajo_consumo(poblacion, c, j)
#   Estima P(x_j* < c) como la proporción de hogares simulados
#   cuyo consumo del bien j está por debajo del umbral c.
prob_bajo_consumo <- function(poblacion, c, j) {
  if (c <= 0)              stop("El umbral c debe ser positivo.")
  if (!(j %in% c(1, 2)))   stop("j debe ser 1 o 2.")
  columna <- paste0("x", j)
  mean(poblacion[[columna]] < c)
}


# ----------------------------------------------------------
# 5) Funciones auxiliares de resumen
# ----------------------------------------------------------
# resumir_variable(x): media, desvío y cuartiles de un vector
resumir_variable <- function(x) {
  tibble::tibble(
    media   = mean(x),
    desvio  = sd(x),
    min     = min(x),
    q1      = quantile(x, 0.25, names = FALSE),
    mediana = quantile(x, 0.50, names = FALSE),
    q3      = quantile(x, 0.75, names = FALSE),
    max     = max(x)
  )
}

# resumir_poblacion(poblacion): aplica resumir_variable a x1, x2 y U
resumir_poblacion <- function(poblacion) {
  poblacion |>
    dplyr::select(x1, x2, U) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable",
                        values_to = "valor") |>
    dplyr::group_by(variable) |>
    dplyr::summarise(resumir_variable(valor), .groups = "drop")
}

# gini(x): coeficiente de Gini (medida de desigualdad, 0 = igualdad)
gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}


# ----------------------------------------------------------
# 6) Gráficos
# ----------------------------------------------------------
# graficar_pre_post(pre, post, variable, titulo, umbral)
#   Histogramas superpuestos (en densidad) de una variable antes y
#   después del shock. Opcionalmente marca el umbral de bajo consumo.
graficar_pre_post <- function(pre, post, variable, titulo, umbral = NULL) {
  datos <- dplyr::bind_rows(
    dplyr::mutate(pre,  escenario = "Pre shock"),
    dplyr::mutate(post, escenario = "Post shock (p1 +20%)")
  ) |>
    dplyr::mutate(escenario = factor(escenario,
                  levels = c("Pre shock", "Post shock (p1 +20%)")))

  g <- ggplot2::ggplot(datos, ggplot2::aes(x = .data[[variable]],
                                           fill = escenario)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 80, alpha = 0.45,
                            position = "identity", colour = NA) +
    ggplot2::scale_fill_manual(values = c("steelblue", "firebrick")) +
    ggplot2::labs(title = titulo, x = variable, y = "Densidad",
                  fill = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(umbral)) {
    g <- g + ggplot2::geom_vline(xintercept = umbral, linetype = "dashed") +
      ggplot2::annotate("text", x = umbral, y = Inf, vjust = 1.5, hjust = -0.1,
                        label = paste0("c = ", umbral), size = 3.5)
  }
  g
}

# ----------------------------------------------------------
# Entrada / salida
# ----------------------------------------------------------
# guardar_tabla(tabla, nombre, carpeta)
#   Imprime la tabla en consola y la guarda como csv en `carpeta`
#   (las tablas del informe salen de estos archivos).
guardar_tabla <- function(tabla, nombre, carpeta) {
  print(tabla, n = Inf, width = Inf)
  readr::write_csv(tabla, file.path(carpeta, paste0(nombre, ".csv")))
}
