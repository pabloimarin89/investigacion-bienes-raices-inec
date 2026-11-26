# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 02: Limpieza INEC - Estadísticas de la Construcción 2024
# ============================================================

source("03_scripts_R/00_configuracion.R")

# ------------------------------------------------------------
# 1. Cargar base importada
# ------------------------------------------------------------

ruta_entrada <- "02_datos_procesados/inec_construccion_2024_importada.rds"

if (!file.exists(ruta_entrada)) {
  stop(
    "No existe la base importada. Ejecute primero ",
    "03_scripts_R/01_importacion_INEC.R"
  )
}

construccion_2024 <- readRDS(ruta_entrada)

# ------------------------------------------------------------
# 2. Convertir códigos principales a formato numérico
# ------------------------------------------------------------

construccion_2024 <- construccion_2024 |>
  mutate(
    claper = as.integer(claper),
    claobr = as.integer(claobr),
    pro_num_prov = as.integer(pro_num_prov),
    pc_num_cant = as.integer(pc_num_cant),
    num_obras = as.numeric(num_obras),
    arecon = as.numeric(arecon),
    numpis = as.numeric(numpis),
    numviv = as.numeric(numviv),
    numapo = as.numeric(numapo),
    numdor = as.numeric(numdor),
    valobr = as.numeric(valobr)
  )

# ------------------------------------------------------------
# 3. Filtrar construcción residencial nueva
# ------------------------------------------------------------
# claper == 1 : Construcción
# claobr == 1 : Edificios residenciales

vivienda_2024 <- construccion_2024 |>
  filter(
    claper == 1,
    claobr == 1
  )

# ------------------------------------------------------------
# 4. Crear variables derivadas
# ------------------------------------------------------------

vivienda_2024 <- vivienda_2024 |>
  mutate(
    area_por_vivienda = if_else(
      numviv > 0,
      arecon / numviv,
      NA_real_
    ),

    dormitorios_por_vivienda = if_else(
      numviv > 0,
      numdor / numviv,
      NA_real_
    ),

    aposentos_por_vivienda = if_else(
      numviv > 0,
      numapo / numviv,
      NA_real_
    ),

    valor_m2 = if_else(
      arecon > 0,
      valobr / arecon,
      NA_real_
    ),

    viviendas_por_obra = if_else(
      num_obras > 0,
      numviv / num_obras,
      NA_real_
    ),

    tipo_proyecto_aprox = case_when(
      numviv == num_obras & numviv > 0 ~ "Una vivienda por obra",
      numviv > num_obras & numpis >= 2 ~ "Multifamiliar probable",
      TRUE ~ "Otro"
    )
  )

# ------------------------------------------------------------
# 5. Controles de calidad
# ------------------------------------------------------------

cat("\nRegistros residenciales nuevos:", nrow(vivienda_2024), "\n")

cat("\nResumen de variables principales:\n")
print(
  summary(
    vivienda_2024 |>
      select(
        num_obras,
        arecon,
        numpis,
        numviv,
        numapo,
        numdor,
        valobr,
        area_por_vivienda
      )
  )
)

cat("\nValores faltantes:\n")
print(
  vivienda_2024 |>
    summarise(
      across(
        c(
          num_obras,
          arecon,
          numpis,
          numviv,
          numapo,
          numdor,
          valobr
        ),
        ~ sum(is.na(.x))
      )
    )
)

# ------------------------------------------------------------
# 6. Guardar base limpia
# ------------------------------------------------------------

saveRDS(
  vivienda_2024,
  "02_datos_procesados/inec_vivienda_2024_limpia.rds"
)

write_csv(
  vivienda_2024,
  "02_datos_procesados/inec_vivienda_2024_limpia.csv"
)

cat("\nBase limpia guardada correctamente.\n")
