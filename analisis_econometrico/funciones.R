############################################################
# ANÁLISIS ECONOMÉTRICO con datos de gapminder
# Archivo de FUNCIONES (se cargan desde script_principal.R
# con source(); acá no se ejecuta nada).
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
############################################################


# ----------------------------------------------------------
# 0) Carga y limpieza
# ----------------------------------------------------------
# cargar_gapminder(ruta)
#   Lee el csv y corrige los problemas detectados en la
#   exploración inicial:
#   - life_expectancy_female viene como texto porque los
#     faltantes están codificados como "-"  -> NA y numérica.
#   - life_expectancy_male usa -999 como código de faltante -> NA.
#   - main_religion tiene mayúsculas inconsistentes
#     ("muslim"/"Muslim", "eastern   religions") -> se normaliza.
cargar_gapminder <- function(ruta) {
  readr::read_csv(ruta, na = c("", "NA", "-"), show_col_types = FALSE) |>
    dplyr::mutate(
      life_expectancy_female = as.numeric(life_expectancy_female),
      life_expectancy_male   = dplyr::na_if(life_expectancy_male, -999),
      main_religion = stringr::str_squish(stringr::str_to_lower(main_religion))
    )
}


# ----------------------------------------------------------
# 1) Métricas de error y bondad de ajuste
# ----------------------------------------------------------
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))
# MAPE: error absoluto medio en % del valor observado
mape <- function(obs, pred) 100 * mean(abs(obs - pred) / abs(obs))

# r2_manual(modelo): R^2 = 1 - SSres/SStot, calculado a mano para
# verificar el que reporta summary()
r2_manual <- function(modelo) {
  y <- stats::model.response(stats::model.frame(modelo))
  1 - sum(stats::residuals(modelo)^2) / sum((y - mean(y))^2)
}


# ----------------------------------------------------------
# 2) Parte 1: regresiones polinómicas en el tiempo
# ----------------------------------------------------------
# ajustar_polinomio(datos, grado)
#   Ajusta income_per_person ~ polinomio de grado `grado` en t.
#   Usamos poly() (base ortogonal) en vez de potencias crudas:
#   genera EXACTAMENTE las mismas predicciones (mismo subespacio,
#   como vimos en clase) pero evita los problemas numéricos de
#   elevar t a la décima potencia.
ajustar_polinomio <- function(datos, grado) {
  if (grado < 1) stop("El grado debe ser al menos 1.")
  stats::lm(income_per_person ~ poly(t, grado), data = datos)
}

# evaluar_modelos(modelos, train, test)
#   Para una lista con nombre de modelos devuelve RMSE / MAE / MAPE
#   en entrenamiento y en testeo, más el R^2 de entrenamiento.
evaluar_modelos <- function(modelos, train, test) {
  purrr::imap_dfr(modelos, function(m, nombre) {
    pred_test <- stats::predict(m, newdata = test)
    tibble::tibble(
      modelo     = nombre,
      r2_train   = summary(m)$r.squared,
      rmse_train = rmse(train$income_per_person, stats::fitted(m)),
      rmse_test  = rmse(test$income_per_person, pred_test),
      mae_test   = mae(test$income_per_person, pred_test),
      mape_test  = mape(test$income_per_person, pred_test)
    )
  })
}

# predecir_grilla(modelos, grilla)
#   Predicciones de cada modelo en una grilla de t (formato largo),
#   para graficar las curvas ajustadas.
predecir_grilla <- function(modelos, grilla) {
  purrr::imap_dfr(modelos, function(m, nombre) {
    dplyr::mutate(grilla, modelo = nombre,
                  prediccion = stats::predict(m, newdata = grilla))
  })
}


# ----------------------------------------------------------
# 3) Parte 1: correlaciones entre países
# ----------------------------------------------------------
# a_formato_ancho(datos, paises, variable)
#   Una columna por país y una fila por año.
a_formato_ancho <- function(datos, paises, variable) {
  datos |>
    dplyr::filter(country %in% paises) |>
    dplyr::select(year, country, valor = dplyr::all_of(variable)) |>
    tidyr::pivot_wider(names_from = country, values_from = valor) |>
    dplyr::arrange(year)
}

# crecimiento_interanual(x): (Y_t - Y_{t-1}) / Y_{t-1}
crecimiento_interanual <- function(x) (x - dplyr::lag(x)) / dplyr::lag(x)

# graficar_correlaciones(matriz, titulo)
#   Mapa de calor de una matriz de correlaciones con ggplot2.
graficar_correlaciones <- function(matriz, titulo) {
  as.data.frame(as.table(matriz)) |>
    stats::setNames(c("pais1", "pais2", "rho")) |>
    ggplot2::ggplot(ggplot2::aes(pais1, pais2, fill = rho)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", rho)), size = 3.5) +
    ggplot2::scale_fill_gradient2(low = "firebrick", mid = "white",
                                  high = "steelblue", limits = c(-1, 1)) +
    ggplot2::labs(title = titulo, x = NULL, y = NULL, fill = "Correlación") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}


# ----------------------------------------------------------
# 4) Parte 2: regresiones de esperanza de vida
# ----------------------------------------------------------
# tabla_coeficientes(modelo)
#   Coeficientes, errores estándar, estadístico t y p-valor en un tibble.
tabla_coeficientes <- function(modelo) {
  s <- summary(modelo)$coefficients
  tibble::tibble(termino = rownames(s), estimacion = s[, 1],
                 error_est = s[, 2], t = s[, 3], p_valor = s[, 4])
}

# test_t_coeficiente(modelo, termino, valor_h0)
#   Test t bilateral de H0: beta = valor_h0 (como en clase: con SE
#   clásicos, t = (beta_hat - valor_h0) / SE).
test_t_coeficiente <- function(modelo, termino, valor_h0) {
  s    <- summary(modelo)$coefficients
  beta <- s[termino, "Estimate"]
  se   <- s[termino, "Std. Error"]
  gl   <- stats::df.residual(modelo)
  t    <- (beta - valor_h0) / se
  tibble::tibble(termino = termino, h0 = valor_h0, beta = beta, se = se,
                 t = t, gl = gl, p_valor = 2 * stats::pt(-abs(t), df = gl))
}


# ----------------------------------------------------------
# 5) Parte 2: selección de modelo (inciso 9)
# ----------------------------------------------------------
# comparar_subconjuntos(train, test, respuesta, candidatas, max_cov)
#   Ajusta TODAS las regresiones lineales posibles con 1 hasta
#   max_cov covariables tomadas de `candidatas` (búsqueda exhaustiva
#   con combn) y reporta R^2, R^2 ajustado y error cuadrático medio
#   (ECM) en el conjunto de testeo, como hicimos en clase con wage1.
comparar_subconjuntos <- function(train, test, respuesta, candidatas, max_cov = 3) {
  combinaciones <- unlist(
    lapply(seq_len(max_cov), function(m) combn(candidatas, m, simplify = FALSE)),
    recursive = FALSE
  )
  purrr::map_dfr(combinaciones, function(covs) {
    formula <- stats::reformulate(covs, response = respuesta)
    m <- stats::lm(formula, data = train)
    tibble::tibble(
      covariables = paste(covs, collapse = " + "),
      n_cov       = length(covs),
      r2          = summary(m)$r.squared,
      r2_ajustado = summary(m)$adj.r.squared,
      ecm_test    = mean((test[[respuesta]] - stats::predict(m, newdata = test))^2)
    )
  })
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
