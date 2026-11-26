# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 06: Homologación INEC 2014-2024
# Fuente: INEC - Estadísticas de la Construcción
# ============================================================

source("03_scripts_R/00_configuracion.R")

# ------------------------------------------------------------
# 1. Archivos comparables seleccionados
# ------------------------------------------------------------

fuentes <- tibble::tribble(
  ~anio, ~archivo,
  2014, "reeconomestconstru2014-04_2.xlsx",
  2015, "reeconomestconstru2015-04_2.xlsx",
  2016, "reeconomestconstru2016-04_0_2.xlsx",
  2017, "reecocomestconstru2017-04_2.xlsx",
  2018, "reeconomestconstru2018-04_2.xlsx",
  2019, "reconstru-vivgruparea-anual2019_2.xlsx",
  2020, "reconstru-vivgruparea-anual2020_2.xlsx",
  2021, "recoonstru-vivgruparea-anual2021_1.xlsx",
  2022, "reCONSTRU- VIVGRUPAREA - anual2022.xlsx",
  2023, "reCONSTRU-VIVGRUPAREA-anual-2023.xlsx",
  2024, "reCONSTRU-VIVGRUPAREA-anual2024.xlsx"
) |>
  mutate(
    ruta = file.path("01_datos_raw/INEC", archivo)
  )

if (any(!file.exists(fuentes$ruta))) {
  stop(
    "Faltan archivos INEC: ",
    paste(
      fuentes$archivo[!file.exists(fuentes$ruta)],
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# 2. Funciones auxiliares
# ------------------------------------------------------------

normalizar <- function(x) {
  x |>
    as.character() |>
    stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_squish()
}

canon_grupo <- function(x) {

  s <- normalizar(x)

  dplyr::case_when(
    stringr::str_detect(s, "MENOS DE 40") ~ "Menos de 40",
    stringr::str_detect(s, "^DE 40.*70") ~ "40 a menos de 70",
    stringr::str_detect(s, "^DE 70.*100") ~ "70 a menos de 100",
    stringr::str_detect(s, "^DE 100.*150") ~ "100 a menos de 150",
    stringr::str_detect(s, "^DE 150.*200") ~ "150 a menos de 200",
    stringr::str_detect(s, "200 Y MAS") ~ "200 y más",
    TRUE ~ NA_character_
  )
}

orden_grupo <- c(
  "Menos de 40" = 1,
  "40 a menos de 70" = 2,
  "70 a menos de 100" = 3,
  "100 a menos de 150" = 4,
  "150 a menos de 200" = 5,
  "200 y más" = 6
)

es_ruido <- function(x) {

  s <- normalizar(x)

  if (is.na(s) || s == "") {
    return(TRUE)
  }

  if (stringr::str_detect(s, "^\\d+$")) {
    return(TRUE)
  }

  patrones <- c(
    "INSTITUTO NACIONAL",
    "AREA DE ESTADISTICAS",
    "ESTADISTICAS DE LA CONSTRUCCION",
    "^CUADRO",
    "CONSTRUCCION DE VIVIENDAS",
    "VIVIENDAS Y APARTAMENTOS CONSTRUIDOS",
    "^POR:",
    "^SEGUN:",
    "PROVINCIA - CANTON",
    "PROVINCIA, CANTON",
    "^ANO ",
    "COSTA RICA, DESDE",
    "NUMERO DE PERMISOS"
  )

  any(stringr::str_detect(s, patrones))
}

a_numero <- function(x) {
  suppressWarnings(as.numeric(x))
}

# ------------------------------------------------------------
# 3. Leer y homologar un archivo
# ------------------------------------------------------------

leer_inec <- function(anio, ruta) {

  datos <- readxl::read_excel(
    ruta,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  datos <- as.data.frame(datos)

  # Localizar la columna que contiene las etiquetas geográficas
  # y los grupos de área.
  puntajes <- purrr::map_dbl(
    seq_along(datos),
    function(j) {

      s <- normalizar(datos[[j]])

      sum(s == "COSTA RICA", na.rm = TRUE) * 20 +
        sum(s %in% c("PROVINCIA", "CANTON"), na.rm = TRUE) * 5 +
        sum(!is.na(canon_grupo(datos[[j]])), na.rm = TRUE) * 3
    }
  )

  col_etiqueta <- which.max(puntajes)

  current_provincia <- NA_character_
  current_nivel <- NA_character_
  current_entidad <- NULL

  entidades <- list()
  grupos <- list()

  contador_entidad <- 0
  contador_grupo <- 0

  for (i in seq_len(nrow(datos))) {

    etiqueta <- datos[[col_etiqueta]][i]
    s <- normalizar(etiqueta)

    if (!is.na(s) && s == "PROVINCIA") {
      current_nivel <- "Provincia"
      current_entidad <- NULL
      next
    }

    if (!is.na(s) && s == "CANTON") {
      current_nivel <- "Cantón"
      current_entidad <- NULL
      next
    }

    grupo <- canon_grupo(etiqueta)

    if (!is.na(grupo) && !is.null(current_entidad)) {

      contador_grupo <- contador_grupo + 1

      grupos[[contador_grupo]] <- tibble(
        anio = anio,
        nivel_geo = current_entidad$nivel_geo,
        provincia = current_entidad$provincia,
        canton = current_entidad$canton,
        grupo_area = grupo,
        orden_grupo = unname(orden_grupo[grupo]),
        permisos = a_numero(datos[[col_etiqueta + 1]][i]),
        viviendas = a_numero(datos[[col_etiqueta + 2]][i]),
        area_m2 = a_numero(datos[[col_etiqueta + 3]][i]),
        valor_fuente = a_numero(datos[[col_etiqueta + 4]][i]),
        fila_fuente = i,
        archivo_fuente = basename(ruta),
        imputado_cero = FALSE
      )

      next
    }

    if (es_ruido(etiqueta)) {
      next
    }

    permisos <- a_numero(datos[[col_etiqueta + 1]][i])
    viviendas <- a_numero(datos[[col_etiqueta + 2]][i])
    area <- a_numero(datos[[col_etiqueta + 3]][i])
    valor <- a_numero(datos[[col_etiqueta + 4]][i])

    if (is.na(permisos) || is.na(viviendas)) {
      next
    }

    if (!is.na(s) && s == "COSTA RICA") {

      nueva <- list(
        nivel_geo = "Costa Rica",
        provincia = NA_character_,
        canton = NA_character_
      )

    } else if (identical(current_nivel, "Provincia")) {

      current_provincia <- as.character(etiqueta)

      nueva <- list(
        nivel_geo = "Provincia",
        provincia = current_provincia,
        canton = NA_character_
      )

      current_nivel <- NA_character_

    } else if (identical(current_nivel, "Cantón")) {

      nueva <- list(
        nivel_geo = "Cantón",
        provincia = current_provincia,
        canton = as.character(etiqueta)
      )

      current_nivel <- NA_character_

    } else {

      next
    }

    contador_entidad <- contador_entidad + 1

    entidades[[contador_entidad]] <- tibble(
      anio = anio,
      nivel_geo = nueva$nivel_geo,
      provincia = nueva$provincia,
      canton = nueva$canton,
      permisos_total = permisos,
      viviendas_total = viviendas,
      area_total_m2 = area,
      valor_total_fuente = valor,
      archivo_fuente = basename(ruta)
    )

    current_entidad <- nueva
  }

  list(
    entidades = dplyr::bind_rows(entidades),
    grupos = dplyr::bind_rows(grupos)
  )
}

# ------------------------------------------------------------
# 4. Importar los 11 años
# ------------------------------------------------------------

resultado <- purrr::map2(
  fuentes$anio,
  fuentes$ruta,
  leer_inec
)

entidades <- purrr::map_dfr(
  resultado,
  "entidades"
)

grupos_observados <- purrr::map_dfr(
  resultado,
  "grupos"
)

# ------------------------------------------------------------
# 5. Completar categorías ausentes con cero
# ------------------------------------------------------------

grupos_esperados <- tibble(
  grupo_area = names(orden_grupo),
  orden_grupo = unname(orden_grupo)
)

base_homologada <- entidades |>
  select(
    anio,
    nivel_geo,
    provincia,
    canton,
    archivo_fuente
  ) |>
  tidyr::crossing(grupos_esperados) |>
  left_join(
    grupos_observados,
    by = c(
      "anio",
      "nivel_geo",
      "provincia",
      "canton",
      "archivo_fuente",
      "grupo_area",
      "orden_grupo"
    )
  ) |>
  mutate(
    imputado_cero = is.na(imputado_cero),
    permisos = tidyr::replace_na(permisos, 0),
    viviendas = tidyr::replace_na(viviendas, 0),
    area_m2 = tidyr::replace_na(area_m2, 0),
    valor_fuente = tidyr::replace_na(valor_fuente, 0)
  ) |>
  arrange(
    anio,
    nivel_geo,
    provincia,
    canton,
    orden_grupo
  )

# ------------------------------------------------------------
# 6. Validación contra totales INEC
# ------------------------------------------------------------

validacion <- base_homologada |>
  group_by(
    anio,
    nivel_geo,
    provincia,
    canton
  ) |>
  summarise(
    permisos_suma_grupos = sum(permisos, na.rm = TRUE),
    viviendas_suma_grupos = sum(viviendas, na.rm = TRUE),
    area_suma_grupos = sum(area_m2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    entidades,
    by = c(
      "anio",
      "nivel_geo",
      "provincia",
      "canton"
    )
  ) |>
  mutate(
    dif_permisos = permisos_suma_grupos - permisos_total,
    dif_viviendas = viviendas_suma_grupos - viviendas_total,
    dif_area = area_suma_grupos - area_total_m2,
    validacion_principal = if_else(
      dif_permisos == 0 &
        dif_viviendas == 0 &
        dif_area == 0,
      "OK",
      "REVISAR"
    )
  )

if (any(validacion$validacion_principal != "OK")) {
  stop(
    "La homologación presenta diferencias contra los totales INEC."
  )
}

# ------------------------------------------------------------
# 7. Bases de salida
# ------------------------------------------------------------

base_cantonal <- base_homologada |>
  filter(nivel_geo == "Cantón")

resumen_nacional <- base_homologada |>
  filter(nivel_geo == "Costa Rica") |>
  group_by(anio) |>
  summarise(
    permisos = sum(permisos),
    viviendas = sum(viviendas),
    area_m2 = sum(area_m2),
    viviendas_menor_70 = sum(
      viviendas[
        grupo_area %in% c(
          "Menos de 40",
          "40 a menos de 70"
        )
      ]
    ),
    participacion_menor_70 =
      viviendas_menor_70 / viviendas,
    .groups = "drop"
  )

# ------------------------------------------------------------
# 8. Guardar resultados
# ------------------------------------------------------------

readr::write_csv(
  base_homologada,
  "02_datos_procesados/INEC_construccion_homologada_2014_2024.csv"
)

readr::write_csv(
  base_cantonal,
  "02_datos_procesados/INEC_construccion_cantonal_2014_2024.csv"
)

saveRDS(
  base_homologada,
  "02_datos_procesados/INEC_construccion_homologada_2014_2024.rds"
)

readr::write_csv(
  validacion,
  "05_tablas/INEC_validacion_homologacion_2014_2024.csv"
)

readr::write_csv(
  resumen_nacional,
  "05_tablas/INEC_resumen_nacional_2014_2024.csv"
)

cat("\nHomologación INEC 2014-2024 completada.\n")
cat("Registros:", nrow(base_homologada), "\n")
cat(
  "Validaciones con error:",
  sum(validacion$validacion_principal != "OK"),
  "\n"
)

print(resumen_nacional)

# ============================================================
# NOTA SOBRE VALOR
# ============================================================
# La columna valor_fuente conserva el valor numérico tal como
# está almacenado en cada archivo original.
#
# Se detectó una diferencia de escala interna en 2014 respecto
# de los años siguientes. Por esa razón, valor_fuente NO debe
# utilizarse todavía como serie monetaria comparable.
#
# Permisos, viviendas y área sí fueron validados contra los
# totales originales para las 987 entidades geográficas/año.
# ============================================================
