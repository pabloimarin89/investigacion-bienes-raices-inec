# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 01: Importación y homologación CFIA 2014-2024
# Fuente: CFIA - Administrador de Proyectos de Construcción (APC)
# ============================================================

source("config/00_configuracion.R")

# ------------------------------------------------------------
# 1. Definir período de estudio
# ------------------------------------------------------------

anios_estudio <- 2014:2024
carpeta_cfia <- "data/raw/CFIA"

# ------------------------------------------------------------
# 2. Localizar los archivos
# ------------------------------------------------------------

archivos_cfia <- tibble(
  anio = anios_estudio,
  archivo = file.path(
    carpeta_cfia,
    paste0(anios_estudio, ".xlsx")
  )
)

archivos_faltantes <- archivos_cfia |>
  filter(!file.exists(archivo))

if (nrow(archivos_faltantes) > 0) {
  stop(
    "Faltan los siguientes archivos del CFIA: ",
    paste(archivos_faltantes$anio, collapse = ", ")
  )
}

cat(
  "\nSe encontraron los 11 archivos del CFIA para 2014-2024.\n"
)

# ------------------------------------------------------------
# 3. Función para importar y homologar cada año
# ------------------------------------------------------------

leer_cfia <- function(archivo, anio) {

  datos <- readxl::read_excel(archivo) |>
    janitor::clean_names()

  # En 2019 la variable aparece como "Pisos".
  # Se homologa a "niveles" para toda la serie.
  if ("pisos" %in% names(datos) &&
      !"niveles" %in% names(datos)) {
    datos <- datos |>
      rename(niveles = pisos)
  }

  datos |>
    mutate(
      anio = anio,

      proyecto = as.character(proyecto),
      area = as.numeric(area),
      niveles = as.numeric(niveles),

      obra = str_to_upper(str_squish(as.character(obra))),
      sub_obra = str_to_upper(str_squish(as.character(sub_obra))),
      clasificacion = str_to_upper(
        str_squish(as.character(clasificacion))
      ),

      provincia = str_to_upper(
        str_squish(as.character(provincia))
      ),
      canton = str_to_upper(
        str_squish(as.character(canton))
      ),
      distrito = str_to_upper(
        str_squish(as.character(distrito))
      )
    )
}

# ------------------------------------------------------------
# 4. Consolidar todos los años
# ------------------------------------------------------------

cfia_raw <- purrr::map2_dfr(
  archivos_cfia$archivo,
  archivos_cfia$anio,
  leer_cfia
)

cat("\nDimensiones de la base CFIA consolidada:\n")
print(dim(cfia_raw))

cat("\nRegistros por año:\n")
print(
  cfia_raw |>
    count(anio, name = "registros")
)

# ------------------------------------------------------------
# 5. Validar estructura esperada
# ------------------------------------------------------------

variables_esperadas <- c(
  "proyecto",
  "exonerado",
  "area",
  "obra",
  "sub_obra",
  "fecha_proyecto",
  "provincia",
  "canton",
  "distrito",
  "unidad",
  "clasificacion",
  "niveles",
  "anio"
)

variables_faltantes <- setdiff(
  variables_esperadas,
  names(cfia_raw)
)

if (length(variables_faltantes) == 0) {
  cat("\nEstructura CFIA homologada correctamente.\n")
} else {
  warning(
    "Faltan variables: ",
    paste(variables_faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 6. Guardar base consolidada
# ------------------------------------------------------------

saveRDS(
  cfia_raw,
  "data/procesados/cfia_2014_2024_raw_homologada.rds"
)

cat(
  "\nBase CFIA 2014-2024 guardada correctamente.\n"
)

# ============================================================
# IMPORTANTE SOBRE LA VARIABLE NIVELES
# ============================================================
# El CFIA indica que una obra puede estar segmentada en varios
# usos/sectores. Por ello, para obtener la cantidad total de
# niveles de un proyecto deben agregarse los valores asociados
# al mismo código de proyecto.
#
# Además, el concepto de niveles incluye sótanos.
#
# Por esta razón, "niveles" NO se interpretará directamente
# como altura visible de cada registro individual.
# ============================================================
