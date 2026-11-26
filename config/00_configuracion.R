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
  "janitor"
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
  "cod",
  "data",
  "data/raw",
  "data/raw/INEC",
  "data/raw/CFIA",
  "data/procesados",
  "info",
  "info/figuras",
  "info/tablas",
  "info/presentacion",
  "info/documentos",
  "config"
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
# Individual_Ibarra_HerramientasDatosI_2026.Rproj
#
# No se utiliza setwd() con una ruta absoluta. Esto mantiene el
# proyecto reproducible y permite que funcione aunque cambie la
# letra de unidad o se utilice en otra computadora.
# ============================================================
