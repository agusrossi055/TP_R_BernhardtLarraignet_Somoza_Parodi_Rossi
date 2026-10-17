# Examen domiciliario de R — UTDT

**Integrantes:** Máxima Bernhardt Larraignet · Pedro Somoza · Santiago Parodi · Agustín Nicolás Rossi
**Profesores:** Ian Evangelos Bounos · Pedro Dolanyi Plaza

Este repositorio contiene la resolución de los tres ejercicios obligatorios del examen:

| Ejercicio | Carpeta | Contenido |
|---|---|---|
| Análisis de datos | `analisis_datos/` | Encuesta Permanente de Hogares (INDEC): tres preguntas respondidas con tidyverse |
| Análisis econométrico | `analisis_econometrico/` | gapminder: regresiones sobre el tiempo, correlaciones, esperanza de vida y género |
| Simulación | `simulacion/` | Ejercicio 1: demanda con preferencias Cobb–Douglas |

El informe completo, con todas las explicaciones y resultados, es
**`informe_BernhardtLarraignet_Somoza_Parodi_Rossi.pdf`**.

## Estructura

```
.
├── README.md
├── TP_R_BernhardtLarraignet_Somoza_Parodi_Rossi.Rproj
├── informe_BernhardtLarraignet_Somoza_Parodi_Rossi.pdf
├── analisis_datos/
│   ├── script_principal.R      # ejecuta el análisis
│   ├── funciones.R             # sólo definiciones de funciones (se cargan con source())
│   ├── data/                   # datos de entrada
│   ├── figuras/                # gráficos generados por el script
│   └── resultados/             # tablas generadas por el script (csv)
├── analisis_econometrico/
│   ├── script_principal.R
│   ├── funciones.R
│   ├── data/gapminder.csv
│   ├── figuras/
│   └── resultados/
└── simulacion/
    ├── script_principal.R
    ├── funciones.R
    ├── data/                   # vacía: la simulación no usa datos externos
    ├── figuras/
    └── resultados/
```

En cada ejercicio, `funciones.R` **sólo define** funciones y `script_principal.R` las
carga con `source()` y las ejecuta. No se mezclan definiciones con ejecución.

## Cómo reproducir los resultados

1. Abrir el proyecto `TP_R_BernhardtLarraignet_Somoza_Parodi_Rossi.Rproj` en RStudio (eso
   deja el **working directory en la raíz del repositorio**), o hacer `setwd("ruta/al/repo")`. Todas las rutas de los scripts son
   relativas a la raíz.
2. Instalar los paquetes necesarios (una sola vez):

   ```r
   install.packages(c("tidyverse", "lmtest", "sandwich", "car", "patchwork", "scales"))
   ```

3. Correr cada script completo, de arriba hacia abajo:

   ```r
   source("analisis_datos/script_principal.R")
   source("analisis_econometrico/script_principal.R")
   source("simulacion/script_principal.R")
   ```

Cada script empieza con `set.seed(2026)`, así que los resultados (incluidas las
simulaciones y la partición entrenamiento/testeo) son idénticos en cada corrida. Las
figuras y tablas se regeneran en las carpetas `figuras/` y `resultados/` de cada ejercicio.

Versión usada: R 4.3.3 con tidyverse 2.0.0.

## Datos

- **Análisis de datos:** microdatos de la Encuesta Permanente de Hogares (EPH), base
  usuaria individual del **1er trimestre de 2026** (último publicado), descargada del INDEC
  (https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos, archivo
  `EPH_usu_1_Trim_2026_txt.zip`) y guardada como `analisis_datos/data/usu_individual_T126.txt`.
- **Análisis econométrico:** `gapminder.csv`. El script
  corrige los problemas de formato del archivo (faltantes codificados como `"-"` y `-999`,
  categorías con mayúsculas inconsistentes).
- **Simulación:** no usa datos externos; todo se genera con `set.seed()`.
