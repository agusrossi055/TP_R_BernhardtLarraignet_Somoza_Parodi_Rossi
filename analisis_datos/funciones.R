############################################################
# ANÁLISIS DE DATOS: Encuesta Permanente de Hogares (EPH)
# Archivo de FUNCIONES (se cargan desde script_principal.R
# con source(); acá no se ejecuta nada).
#
# Integrantes: Bernhardt Larraignet, Somoza, Parodi, Rossi
############################################################


# ----------------------------------------------------------
# 1) Carga y preparación de la base
# ----------------------------------------------------------
# preparar_eph(base)
#   Recibe la base usuaria individual de la EPH (el .txt del INDEC leído
#   con read_delim) y:
#   - selecciona y renombra las variables que usamos (como en el
#     ejercicio de clase),
#   - convierte los códigos numéricos en etiquetas legibles,
#   - marca como NA los códigos de "no sabe / no responde":
#       P21 = -9 (ingreso), PP3E_TOT = 999 (horas), DECIFR = 12 (IPCF).
#   Diccionario de variables (diseño de registro del INDEC):
#     CH04 sexo | CH06 edad | REGION región | NIVEL_ED nivel educativo
#     ESTADO condición de actividad (1 = ocupado)
#     CAT_OCUP categoría ocupacional (3 = asalariado)
#     P21 ingreso mensual de la ocupación principal
#     PP3E_TOT horas semanales trabajadas en la ocupación principal
#     PP07H ¿le hacen descuento jubilatorio? (1 = sí, 2 = no)
#     IPCF ingreso per cápita familiar | DECIFR decil del IPCF
#     PONDERA ponderador general | PONDIIO ponderador del ingreso de la
#     ocupación principal | PONDIH ponderador del IPCF
preparar_eph <- function(base) {
  base |>
    dplyr::select(CH04, CH06, REGION, NIVEL_ED, ESTADO, CAT_OCUP, P21,
                  PP3E_TOT, PP07H, IPCF, DECIFR, PONDERA, PONDIIO, PONDIH) |>
    dplyr::rename(sexo = CH04, edad = CH06, region = REGION,
                  nivel_ed = NIVEL_ED, estado = ESTADO, cat_ocup = CAT_OCUP,
                  ingreso_ocup = P21, horas_sem = PP3E_TOT,
                  aporte_jubilatorio = PP07H, ipcf = IPCF, decil_ipcf = DECIFR,
                  pondera = PONDERA, pondiio = PONDIIO, pondih = PONDIH) |>
    dplyr::mutate(
      sexo   = factor(sexo, levels = c(1, 2), labels = c("Varón", "Mujer")),
      region = factor(region, levels = c(1, 40, 41, 42, 43, 44),
                      labels = c("GBA", "NOA", "NEA", "Cuyo", "Pampeana",
                                 "Patagonia")),
      educacion = agrupar_educacion(nivel_ed),
      grupo_edad = agrupar_edad(edad),
      # Códigos de no respuesta -> NA
      ingreso_ocup = dplyr::if_else(ingreso_ocup == -9, NA_real_,
                                    as.numeric(ingreso_ocup)),
      horas_sem    = dplyr::if_else(horas_sem >= 999, NA_real_,
                                    as.numeric(horas_sem)),
      ipcf         = dplyr::if_else(decil_ipcf == 12, NA_real_, ipcf),
      # Ingreso por hora: mensual / (horas semanales * 30/7 semanas por mes)
      ingreso_hora = dplyr::if_else(horas_sem > 0,
                                    ingreso_ocup / (horas_sem * 30 / 7),
                                    NA_real_),
      informal = dplyr::case_when(aporte_jubilatorio == 1 ~ FALSE,
                                  aporte_jubilatorio == 2 ~ TRUE,
                                  TRUE ~ NA)
    )
}

# agrupar_educacion(nivel_ed): NIVEL_ED (1 a 7) en 5 categorías ordenadas
agrupar_educacion <- function(nivel_ed) {
  grupo <- dplyr::case_when(
    nivel_ed %in% c(7, 1, 2) ~ "Hasta primario completo",
    nivel_ed == 3            ~ "Secundario incompleto",
    nivel_ed == 4            ~ "Secundario completo",
    nivel_ed == 5            ~ "Superior incompleto",
    nivel_ed == 6            ~ "Superior completo",
    TRUE                     ~ NA_character_
  )
  factor(grupo, levels = c("Hasta primario completo", "Secundario incompleto",
                           "Secundario completo", "Superior incompleto",
                           "Superior completo"))
}

# agrupar_edad(edad): tramos de edad (adaptado del ejercicio de clase)
agrupar_edad <- function(edad) {
  grupo <- dplyr::case_when(
    edad < 18  ~ "Menores de 18",
    edad <= 25 ~ "18 a 25",
    edad <= 40 ~ "26 a 40",
    edad <= 60 ~ "41 a 60",
    TRUE       ~ "Más de 60"
  )
  factor(grupo, levels = c("Menores de 18", "18 a 25", "26 a 40", "41 a 60",
                           "Más de 60"))
}


# ----------------------------------------------------------
# 2) Estadísticos ponderados
# ----------------------------------------------------------
# La EPH es una muestra: cada persona representa a `w` personas de la
# población. Para estimar medias, medianas o tasas poblacionales hay
# que ponderar por el factor de expansión que corresponda.

# cuantil_ponderado(x, w, p): cuantil p de x con pesos w
#   (ordena x y busca el primer valor donde el peso acumulado
#   alcanza la proporción p del peso total)
cuantil_ponderado <- function(x, w, p = 0.5) {
  if (length(x) != length(w)) stop("x y w deben tener el mismo largo.")
  if (any(w < 0))             stop("Los pesos no pueden ser negativos.")
  orden <- order(x)
  x <- x[orden]
  w <- w[orden]
  acumulado <- cumsum(w) / sum(w)
  x[which(acumulado >= p)[1]]
}

# gini_ponderado(x, w): coeficiente de Gini con pesos
#   G = 1 - sum_i w_i (L_i + L_{i-1}), donde L es la curva de Lorenz
#   evaluada en cada observación y w los pesos normalizados.
gini_ponderado <- function(x, w) {
  orden <- order(x)
  x <- x[orden]
  w <- w[orden] / sum(w)
  lorenz <- cumsum(w * x) / sum(w * x)
  lorenz_previo <- c(0, lorenz[-length(lorenz)])
  1 - sum(w * (lorenz + lorenz_previo))
}

# curva_lorenz(x, w): tabla con la proporción acumulada de población
#   y de ingreso, para graficar la curva de Lorenz
curva_lorenz <- function(x, w) {
  orden <- order(x)
  x <- x[orden]
  w <- w[orden]
  tibble::tibble(
    prop_poblacion = c(0, cumsum(w) / sum(w)),
    prop_ingreso   = c(0, cumsum(w * x) / sum(w * x))
  )
}

# brecha_pct(varon, mujer): cuánto menos gana la mujer, en % del varón
brecha_pct <- function(varon, mujer) 100 * (varon - mujer) / varon

# resumen_ipcf(datos): resumen ponderado (con PONDIH) de la distribución
#   del ingreso per cápita familiar: media, mediana, P10, P90, P90/P10
#   y Gini. Respeta los grupos si `datos` viene agrupado (group_by).
resumen_ipcf <- function(datos) {
  datos |>
    dplyr::summarise(
      n_muestra = dplyr::n(),
      poblacion = sum(pondih),
      ipcf_medio   = weighted.mean(ipcf, pondih),
      ipcf_mediano = cuantil_ponderado(ipcf, pondih, 0.5),
      p10 = cuantil_ponderado(ipcf, pondih, 0.10),
      p90 = cuantil_ponderado(ipcf, pondih, 0.90),
      ratio_p90_p10 = p90 / p10,
      gini = gini_ponderado(ipcf, pondih),
      .groups = "drop"
    )
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
