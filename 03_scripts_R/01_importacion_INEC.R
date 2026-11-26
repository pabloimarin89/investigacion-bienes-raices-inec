# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 01: Importación INEC - Estadísticas de la Construcción
# ============================================================

# Ejecutar configuración general
source("03_scripts_R/00_configuracion.R")

# ------------------------------------------------------------
# 1. Localizar automáticamente la base descargada del INEC
# ------------------------------------------------------------

carpeta_inec <- "01_datos_raw/INEC"

archivos_inec <- list.files(
  carpeta_inec,
  pattern = "\\.(csv|xlsx|xls|sav|dta)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(archivos_inec) == 0) {
  stop(
    paste0(
      "\nNo se encontró una base de datos del INEC en:\n",
      carpeta_inec,
      "\n\nDescargue la base de Estadísticas de la Construcción 2024 ",
      "desde el catálogo del INEC y guárdela en esa carpeta."
    )
  )
}

# Si existen varias bases, priorizar una que incluya 2024 en el nombre
archivo_2024 <- archivos_inec[
  grepl("2024", basename(archivos_inec), ignore.case = TRUE)
]

if (length(archivo_2024) == 1) {
  archivo_inec <- archivo_2024
} else if (length(archivos_inec) == 1) {
  archivo_inec <- archivos_inec
} else {
  cat("\nSe encontraron varios archivos posibles:\n")
  print(basename(archivos_inec))
  stop(
    "\nDeje solamente la base 2024 en la carpeta o incluya '2024' en su nombre."
  )
}

cat("\nArchivo seleccionado:\n", basename(archivo_inec), "\n")

# ------------------------------------------------------------
# 2. Importar según el formato del archivo
# ------------------------------------------------------------

extension <- tolower(tools::file_ext(archivo_inec))

construccion_2024 <- switch(
  extension,

  "csv" = readr::read_csv(
    archivo_inec,
    show_col_types = FALSE
  ),

  "xlsx" = readxl::read_excel(
    archivo_inec
  ),

  "xls" = readxl::read_excel(
    archivo_inec
  ),

  "sav" = haven::read_sav(
    archivo_inec
  ),

  "dta" = haven::read_dta(
    archivo_inec
  ),

  stop("Formato de archivo no reconocido: ", extension)
)

# Limpiar nombres de columnas
construccion_2024 <- construccion_2024 |>
  janitor::clean_names()

# ------------------------------------------------------------
# 3. Validaciones iniciales
# ------------------------------------------------------------

cat("\nDimensiones de la base:\n")
print(dim(construccion_2024))

cat("\nNombres de variables:\n")
print(names(construccion_2024))

cat("\nEstructura:\n")
dplyr::glimpse(construccion_2024)

# La versión pública oficial 2024 reporta 37.044 registros y 17 variables.
if (nrow(construccion_2024) != 37044) {
  warning(
    "La cantidad de filas no coincide con los 37.044 registros reportados ",
    "para la versión pública definitiva 2024. Revisar el archivo descargado."
  )
}

# Variables fundamentales que esperamos encontrar
variables_esperadas <- c(
  "anoper",
  "claper",
  "claobr",
  "pro_num_prov",
  "pc_num_cant",
  "num_obras",
  "arecon",
  "numpis",
  "numviv",
  "numapo",
  "numdor",
  "valobr",
  "matpis",
  "matpar",
  "mattec",
  "usoobr",
  "financ"
)

variables_faltantes <- setdiff(
  variables_esperadas,
  names(construccion_2024)
)

if (length(variables_faltantes) == 0) {
  cat("\nTodas las variables esperadas fueron encontradas.\n")
} else {
  warning(
    "Faltan estas variables esperadas: ",
    paste(variables_faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. Guardar una copia importada en formato RDS
# ------------------------------------------------------------

ruta_salida <- "02_datos_procesados/inec_construccion_2024_importada.rds"

saveRDS(
  construccion_2024,
  ruta_salida
)

cat(
  "\nImportación completada.\n",
  "Archivo procesado guardado en:\n",
  ruta_salida,
  "\n"
)

# ============================================================
# SIGUIENTE PASO:
# 02_limpieza_INEC.R
# ============================================================
