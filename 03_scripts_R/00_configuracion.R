# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Curso: CA-0204 Herramientas de Ciencia de Datos I
# Archivo: 00_configuracion.R
# ============================================================

# ------------------------------------------------------------
# 1. Opciones generales
# ------------------------------------------------------------

options(scipen = 999)

# ------------------------------------------------------------
# 2. Paquetes requeridos
# ------------------------------------------------------------

paquetes <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "haven"
)

paquetes_faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

invisible(
  lapply(
    paquetes,
    library,
    character.only = TRUE
  )
)

# ------------------------------------------------------------
# 3. Estructura de carpetas del proyecto
# ------------------------------------------------------------

carpetas <- c(
  "01_datos_raw",
  "01_datos_raw/INEC",
  "01_datos_raw/CFIA",
  "01_datos_raw/MEIC",
  "02_datos_procesados",
  "03_scripts_R",
  "04_graficos",
  "05_tablas",
  "06_presentacion",
  "07_documentos"
)

for (carpeta in carpetas) {
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = TRUE)
  }
}

# ------------------------------------------------------------
# 4. Verificaciones iniciales
# ------------------------------------------------------------

cat("\nProyecto configurado correctamente.\n")
cat("Directorio de trabajo:\n")
cat(getwd(), "\n\n")

cat("Carpetas disponibles:\n")
print(list.dirs(recursive = FALSE))

# ------------------------------------------------------------
# NOTA
# ------------------------------------------------------------
# Abrir siempre el proyecto mediante:
# Investigacion_Bienes_Raices_INEC.Rproj
#
# No se utiliza setwd() con una ruta absoluta. Esto mantiene el
# proyecto reproducible y permite que funcione aunque cambie la
# letra de unidad o se utilice en otra computadora.
# ============================================================
