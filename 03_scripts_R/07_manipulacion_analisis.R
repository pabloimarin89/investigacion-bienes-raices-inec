# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y VIVIENDA COMPACTA EN LA GAM
# Curso: CA-0204 Herramientas de Ciencia de Datos I
# Script 07: Manipulación y análisis de datos
# Fuentes: INEC (2014-2024) y CFIA-APC (2014-2024)
# ============================================================

source("03_scripts_R/00_configuracion.R")

# ------------------------------------------------------------
# 1. Cargar las bases procesadas
# ------------------------------------------------------------

ruta_inec <- "02_datos_procesados/INEC_construccion_homologada_2014_2024.rds"
ruta_cfia <- "02_datos_procesados/cfia_2014_2024_raw_homologada.rds"

if (!file.exists(ruta_inec)) {
  stop(
    "No existe la base INEC procesada. Ejecute primero ",
    "03_scripts_R/06_homologacion_INEC_2014_2024.R"
  )
}

if (!file.exists(ruta_cfia)) {
  stop(
    "No existe la base CFIA procesada. Ejecute primero ",
    "03_scripts_R/04_importacion_CFIA.R"
  )
}

inec <- readRDS(ruta_inec)
cfia <- readRDS(ruta_cfia)

# ------------------------------------------------------------
# 2. Homologar nombres geográficos
# ------------------------------------------------------------

normalizar_geo <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_squish() |>
    str_replace_all(
      "VASQUEZ DE CORONADO",
      "VAZQUEZ DE CORONADO"
    )
}

inec <- inec |>
  mutate(
    provincia = normalizar_geo(provincia),
    canton = normalizar_geo(canton)
  )

cfia <- cfia |>
  mutate(
    provincia = normalizar_geo(provincia),
    canton = normalizar_geo(canton),
    obra = normalizar_geo(obra),
    sub_obra = normalizar_geo(sub_obra),
    clasificacion = normalizar_geo(clasificacion)
  )

# ------------------------------------------------------------
# 3. Cantones utilizados como aproximación de la GAM
# ------------------------------------------------------------
# La delimitación oficial de la GAM incluye algunos cantones solo
# parcialmente. Como los cuadros públicos del INEC están disponibles
# a nivel cantonal, se utiliza el cantón completo como aproximación.

gam <- tribble(
  ~provincia, ~canton,
  "SAN JOSE", "SAN JOSE",
  "SAN JOSE", "ESCAZU",
  "SAN JOSE", "DESAMPARADOS",
  "SAN JOSE", "ASERRI",
  "SAN JOSE", "MORA",
  "SAN JOSE", "GOICOECHEA",
  "SAN JOSE", "SANTA ANA",
  "SAN JOSE", "ALAJUELITA",
  "SAN JOSE", "VAZQUEZ DE CORONADO",
  "SAN JOSE", "TIBAS",
  "SAN JOSE", "MORAVIA",
  "SAN JOSE", "MONTES DE OCA",
  "SAN JOSE", "CURRIDABAT",
  "ALAJUELA", "ALAJUELA",
  "ALAJUELA", "ATENAS",
  "ALAJUELA", "POAS",
  "CARTAGO", "CARTAGO",
  "CARTAGO", "PARAISO",
  "CARTAGO", "LA UNION",
  "CARTAGO", "ALVARADO",
  "CARTAGO", "OREAMUNO",
  "CARTAGO", "EL GUARCO",
  "HEREDIA", "HEREDIA",
  "HEREDIA", "BARVA",
  "HEREDIA", "SANTO DOMINGO",
  "HEREDIA", "SANTA BARBARA",
  "HEREDIA", "SAN RAFAEL",
  "HEREDIA", "SAN ISIDRO",
  "HEREDIA", "BELEN",
  "HEREDIA", "FLORES",
  "HEREDIA", "SAN PABLO"
)

# ------------------------------------------------------------
# 4. INEC: construir indicadores de tamaño por cantón y año
# ------------------------------------------------------------
# Variable principal del análisis: viviendas de menos de 40 m².
# El rango 40 a menos de 70 m² se conserva como categoría separada
# y descriptiva; no se suma con <40 m² en el indicador principal.

inec_canton <- inec |>
  filter(nivel_geo == "Cantón") |>
  select(
    anio,
    provincia,
    canton,
    grupo_area,
    permisos,
    viviendas,
    area_m2
  )

inec_canton_resumen <- inec_canton |>
  group_by(anio, provincia, canton) |>
  summarise(
    viviendas_menor_40 = sum(
      .data$viviendas[grupo_area == "Menos de 40"],
      na.rm = TRUE
    ),
    viviendas_40_70 = sum(
      .data$viviendas[grupo_area == "40 a menos de 70"],
      na.rm = TRUE
    ),
    viviendas_70_100 = sum(
      .data$viviendas[grupo_area == "70 a menos de 100"],
      na.rm = TRUE
    ),
    viviendas_100_150 = sum(
      .data$viviendas[grupo_area == "100 a menos de 150"],
      na.rm = TRUE
    ),
    viviendas_150_200 = sum(
      .data$viviendas[grupo_area == "150 a menos de 200"],
      na.rm = TRUE
    ),
    viviendas_200_mas = sum(
      .data$viviendas[grupo_area == "200 y más"],
      na.rm = TRUE
    ),
    permisos = sum(.data$permisos, na.rm = TRUE),
    viviendas = sum(.data$viviendas, na.rm = TRUE),
    area_m2 = sum(.data$area_m2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    participacion_menor_40 = if_else(
      viviendas > 0,
      viviendas_menor_40 / viviendas,
      NA_real_
    ),
    participacion_40_70 = if_else(
      viviendas > 0,
      viviendas_40_70 / viviendas,
      NA_real_
    ),
    participacion_200_mas = if_else(
      viviendas > 0,
      viviendas_200_mas / viviendas,
      NA_real_
    ),
    area_por_vivienda_m2 = if_else(
      viviendas > 0,
      area_m2 / viviendas,
      NA_real_
    )
  )

# Base INEC restringida a la GAM.
inec_gam <- inec_canton_resumen |>
  inner_join(
    gam,
    by = c("provincia", "canton")
  )

# Resumen anual de la GAM. Este es el eje temporal principal del estudio.
resumen_gam_anual <- inec_gam |>
  group_by(anio) |>
  summarise(
    viviendas = sum(viviendas, na.rm = TRUE),
    viviendas_menor_40 = sum(viviendas_menor_40, na.rm = TRUE),
    viviendas_40_70 = sum(viviendas_40_70, na.rm = TRUE),
    viviendas_200_mas = sum(viviendas_200_mas, na.rm = TRUE),
    area_m2 = sum(area_m2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    participacion_menor_40 = viviendas_menor_40 / viviendas,
    participacion_40_70 = viviendas_40_70 / viviendas,
    participacion_200_mas = viviendas_200_mas / viviendas,
    area_por_vivienda_m2 = area_m2 / viviendas
  )

# ------------------------------------------------------------
# 4.1 Punto de cambio exploratorio para la serie <40 m²
# ------------------------------------------------------------
# Para evitar escoger 2019 únicamente por inspección visual, se evalúan
# todos los cortes que dejan al menos tres años en cada segmento. Para
# cada corte se calcula la suma de cuadrados dentro de los dos períodos
# (SSE). El corte seleccionado es el que minimiza esa variabilidad interna.
# Este procedimiento es descriptivo/exploratorio: con solo 11 años no se
# interpreta como una prueba definitiva de ruptura estructural.

anios_gam <- sort(unique(resumen_gam_anual$anio))

cortes_candidatos <- anios_gam[
  3:(length(anios_gam) - 3)
]

evaluacion_cortes_gam <- purrr::map_dfr(
  cortes_candidatos,
  function(corte) {

    periodo_1 <- resumen_gam_anual |>
      filter(anio <= corte) |>
      pull(participacion_menor_40)

    periodo_2 <- resumen_gam_anual |>
      filter(anio > corte) |>
      pull(participacion_menor_40)

    sse_1 <- sum(
      (periodo_1 - mean(periodo_1))^2
    )

    sse_2 <- sum(
      (periodo_2 - mean(periodo_2))^2
    )

    tibble(
      corte_fin_periodo_1 = corte,
      inicio_periodo_2 = corte + 1,
      n_periodo_1 = length(periodo_1),
      n_periodo_2 = length(periodo_2),
      media_periodo_1 = mean(periodo_1),
      media_periodo_2 = mean(periodo_2),
      sse_periodo_1 = sse_1,
      sse_periodo_2 = sse_2,
      sse_total = sse_1 + sse_2
    )
  }
)

sse_sin_corte <- sum(
  (resumen_gam_anual$participacion_menor_40 -
     mean(resumen_gam_anual$participacion_menor_40))^2
)

mejor_corte_gam <- evaluacion_cortes_gam |>
  slice_min(
    sse_total,
    n = 1,
    with_ties = FALSE
  )

anio_inicio_segundo_periodo <- mejor_corte_gam |>
  pull(inicio_periodo_2)

reduccion_sse_corte <- 1 -
  (mejor_corte_gam$sse_total / sse_sin_corte)

etiqueta_periodo_1 <- paste0(
  min(anios_gam),
  "-",
  anio_inicio_segundo_periodo - 1
)

etiqueta_periodo_2 <- paste0(
  anio_inicio_segundo_periodo,
  "-",
  max(anios_gam)
)

resumen_gam_anual <- resumen_gam_anual |>
  mutate(
    periodo = if_else(
      anio < anio_inicio_segundo_periodo,
      etiqueta_periodo_1,
      etiqueta_periodo_2
    )
  )

# Comparación entre los dos períodos definidos por el corte exploratorio.
resumen_periodos_gam <- resumen_gam_anual |>
  group_by(periodo) |>
  summarise(
    viviendas_menor_40 = sum(viviendas_menor_40),
    viviendas = sum(viviendas),
    participacion_agregada_menor_40 =
      viviendas_menor_40 / viviendas,
    promedio_anual_menor_40 = mean(participacion_menor_40),
    mediana_anual_menor_40 = median(participacion_menor_40),
    .groups = "drop"
  )

# Máximos y mínimos de la participación de unidades <40 m² en la GAM.
pico_menor_40_gam <- resumen_gam_anual |>
  slice_max(
    participacion_menor_40,
    n = 1,
    with_ties = FALSE
  )

minimo_menor_40_gam <- resumen_gam_anual |>
  slice_min(
    participacion_menor_40,
    n = 1,
    with_ties = FALSE
  )

anio_pico_menor_40 <- pico_menor_40_gam |>
  pull(anio)

# Asociación entre el paso del tiempo y la participación <40 m².
# Pearson evalúa una tendencia lineal; Spearman evalúa una relación
# monotónica a partir de rangos y es menos sensible a saltos puntuales.
cor_tiempo_menor_40_pearson <- cor(
  resumen_gam_anual$anio,
  resumen_gam_anual$participacion_menor_40,
  method = "pearson"
)

cor_tiempo_menor_40_spearman <- cor(
  resumen_gam_anual$anio,
  resumen_gam_anual$participacion_menor_40,
  method = "spearman"
)

# ------------------------------------------------------------
# 5. Concentración territorial de las unidades <40 m² en la GAM
# ------------------------------------------------------------

ranking_menor_40_anual <- inec_gam |>
  group_by(anio) |>
  arrange(desc(viviendas_menor_40), .by_group = TRUE) |>
  mutate(rango = row_number()) |>
  ungroup()

concentracion_menor_40_gam <- ranking_menor_40_anual |>
  group_by(anio) |>
  summarise(
    viviendas_menor_40_gam = sum(viviendas_menor_40),
    canton_lider = first(canton),
    provincia_lider = first(provincia),
    viviendas_lider = first(viviendas_menor_40),
    participacion_lider = if_else(
      viviendas_menor_40_gam > 0,
      viviendas_lider / viviendas_menor_40_gam,
      NA_real_
    ),
    participacion_top3 = if_else(
      viviendas_menor_40_gam > 0,
      sum(head(viviendas_menor_40, 3)) /
        viviendas_menor_40_gam,
      NA_real_
    ),
    .groups = "drop"
  )

concentracion_pico <- concentracion_menor_40_gam |>
  filter(anio == anio_pico_menor_40)

ranking_menor_40_pico <- inec_gam |>
  filter(anio == anio_pico_menor_40) |>
  arrange(desc(viviendas_menor_40)) |>
  slice_head(n = 10)

# ------------------------------------------------------------
# 6. Composición de tamaños en la GAM
# ------------------------------------------------------------
# Los seis grupos del INEC se mantienen como categorías mutuamente
# excluyentes. El segmento 40-70 m² se interpreta descriptivamente,
# separado de la variable principal (<40 m²).

composicion_gam <- inec |>
  filter(nivel_geo == "Cantón") |>
  inner_join(
    gam,
    by = c("provincia", "canton")
  ) |>
  group_by(anio, grupo_area, orden_grupo) |>
  summarise(
    viviendas_grupo = sum(.data$viviendas, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(anio) |>
  mutate(
    participacion = viviendas_grupo / sum(viviendas_grupo)
  ) |>
  ungroup() |>
  mutate(
    grupo_area = factor(
      grupo_area,
      levels = c(
        "Menos de 40",
        "40 a menos de 70",
        "70 a menos de 100",
        "100 a menos de 150",
        "150 a menos de 200",
        "200 y más"
      )
    )
  )

# ------------------------------------------------------------
# 7. CFIA: identificar torres residenciales altas probables
# ------------------------------------------------------------
# Definición operativa: proyecto nuevo que contiene componentes de
# apartamento o condominio y cuya suma de niveles registrados supera 10.
# Se suman niveles por proyecto porque CFIA puede dividir una obra en varias
# filas o usos. Los niveles pueden incluir sótanos; por ello es un proxy.

tipos_torre <- c(
  "APARTAMENTO",
  "APARTAMENTO UNIFAMILIAR",
  "CONDOMINIO",
  "TRANSFORMACION A CONDOMINIO"
)

cfia_nuevo <- cfia |>
  filter(clasificacion == "NUEVO") |>
  mutate(
    componente_residencial =
      obra == "HABITACIONAL" &
      sub_obra %in% tipos_torre
  )

proyectos_cfia <- cfia_nuevo |>
  group_by(
    anio,
    proyecto,
    provincia,
    canton
  ) |>
  summarise(
    niveles_total = sum(niveles, na.rm = TRUE),
    tiene_componente_residencial = any(componente_residencial),
    area_residencial_m2 = sum(
      area[componente_residencial],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

torres_altas <- proyectos_cfia |>
  filter(
    tiene_componente_residencial,
    niveles_total > 10
  )

torres_canton_anio <- torres_altas |>
  count(
    anio,
    provincia,
    canton,
    name = "torres_mas_10"
  )

torres_anio_cr <- torres_altas |>
  count(anio, name = "torres_costa_rica")

torres_anio_gam <- torres_altas |>
  inner_join(
    gam,
    by = c("provincia", "canton")
  ) |>
  count(anio, name = "torres_gam")

resumen_torres_anio <- tibble(
  anio = 2014:2024
) |>
  left_join(torres_anio_cr, by = "anio") |>
  left_join(torres_anio_gam, by = "anio") |>
  mutate(
    torres_costa_rica = replace_na(torres_costa_rica, 0L),
    torres_gam = replace_na(torres_gam, 0L)
  )

ranking_torres_completo <- torres_altas |>
  count(
    provincia,
    canton,
    name = "torres_mas_10",
    sort = TRUE
  )

ranking_torres <- ranking_torres_completo |>
  slice_head(n = 10)

participacion_torres_san_jose <- ranking_torres_completo |>
  filter(
    provincia == "SAN JOSE",
    canton == "SAN JOSE"
  ) |>
  summarise(
    participacion = sum(torres_mas_10) / nrow(torres_altas)
  ) |>
  pull(participacion)

participacion_torres_top5 <- ranking_torres_completo |>
  slice_head(n = 5) |>
  summarise(
    participacion = sum(torres_mas_10) / nrow(torres_altas)
  ) |>
  pull(participacion)

# ------------------------------------------------------------
# 8. Cruce CFIA + INEC por cantón y año
# ------------------------------------------------------------

base_cruce <- inec_canton_resumen |>
  left_join(
    torres_canton_anio,
    by = c("anio", "provincia", "canton")
  ) |>
  mutate(
    torres_mas_10 = replace_na(torres_mas_10, 0L),
    tiene_torre_alta = torres_mas_10 > 0,
    torres_por_1000_permisos = if_else(
      permisos > 0,
      1000 * torres_mas_10 / permisos,
      NA_real_
    )
  ) |>
  left_join(
    gam |>
      mutate(en_gam = TRUE),
    by = c("provincia", "canton")
  ) |>
  mutate(
    en_gam = replace_na(en_gam, FALSE)
  )

base_cruce_gam <- base_cruce |>
  filter(en_gam)

proporcion_sin_torre_gam <- mean(
  base_cruce_gam$torres_mas_10 == 0,
  na.rm = TRUE
)

# ------------------------------------------------------------
# 9. Correlaciones exploratorias: torres y vivienda <40 m²
# ------------------------------------------------------------

cor_segura <- function(x, y, metodo = "pearson") {
  cor(
    x,
    y,
    method = metodo,
    use = "complete.obs"
  )
}

# La variable principal es <40 m². Se incluyen dos contrastes de contexto:
# vivienda 200+ m² y área autorizada promedio por vivienda.
correlaciones_gam <- tribble(
  ~variable, ~pearson, ~spearman,
  "Viviendas menores de 40 m²",
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_menor_40
  ),
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_menor_40,
    "spearman"
  ),

  "Viviendas de 200 m² o más",
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_200_mas
  ),
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_200_mas,
    "spearman"
  ),

  "Área construida por vivienda",
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$area_por_vivienda_m2
  ),
  cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$area_por_vivienda_m2,
    "spearman"
  )
)

# Perfil promedio con y sin torre alta. El rango 40-70 se conserva aquí
# únicamente como categoría descriptiva de la composición de tamaños.
comparacion_torre_gam <- base_cruce_gam |>
  mutate(
    grupo_torre = if_else(
      tiene_torre_alta,
      "Con torre >10 niveles",
      "Sin torre >10 niveles"
    )
  ) |>
  group_by(grupo_torre) |>
  summarise(
    participacion_menor_40 = mean(
      participacion_menor_40,
      na.rm = TRUE
    ),
    participacion_40_70 = mean(
      participacion_40_70,
      na.rm = TRUE
    ),
    participacion_200_mas = mean(
      participacion_200_mas,
      na.rm = TRUE
    ),
    area_por_vivienda_m2 = mean(
      area_por_vivienda_m2,
      na.rm = TRUE
    ),
    observaciones = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 10. Cambios de largo plazo 2014-2024 en vivienda <40 m²
# ------------------------------------------------------------

cambios_canton <- base_cruce_gam |>
  filter(anio %in% c(2014, 2024)) |>
  select(
    anio,
    provincia,
    canton,
    participacion_menor_40,
    participacion_40_70,
    participacion_200_mas,
    area_por_vivienda_m2
  ) |>
  pivot_wider(
    names_from = anio,
    values_from = c(
      participacion_menor_40,
      participacion_40_70,
      participacion_200_mas,
      area_por_vivienda_m2
    )
  ) |>
  mutate(
    cambio_menor_40_pp = 100 * (
      participacion_menor_40_2024 -
        participacion_menor_40_2014
    ),
    cambio_40_70_pp = 100 * (
      participacion_40_70_2024 -
        participacion_40_70_2014
    ),
    cambio_200_mas_pp = 100 * (
      participacion_200_mas_2024 -
        participacion_200_mas_2014
    ),
    cambio_area_m2 =
      area_por_vivienda_m2_2024 -
      area_por_vivienda_m2_2014
  )

torres_acumuladas_gam <- torres_altas |>
  inner_join(
    gam,
    by = c("provincia", "canton")
  ) |>
  count(
    provincia,
    canton,
    name = "torres_acumuladas"
  )

cambios_canton <- cambios_canton |>
  left_join(
    torres_acumuladas_gam,
    by = c("provincia", "canton")
  ) |>
  mutate(
    torres_acumuladas = replace_na(torres_acumuladas, 0L)
  )

cor_cambio_menor_40 <- cor_segura(
  cambios_canton$torres_acumuladas,
  cambios_canton$cambio_menor_40_pp
)

cor_cambio_menor_40_spearman <- cor_segura(
  cambios_canton$torres_acumuladas,
  cambios_canton$cambio_menor_40_pp,
  "spearman"
)

cor_cambio_sin_san_jose <- cambios_canton |>
  filter(
    !(provincia == "SAN JOSE" & canton == "SAN JOSE")
  ) |>
  summarise(
    correlacion = cor_segura(
      torres_acumuladas,
      cambio_menor_40_pp
    )
  ) |>
  pull(correlacion)

cor_cambio_sin_san_jose_spearman <- cambios_canton |>
  filter(
    !(provincia == "SAN JOSE" & canton == "SAN JOSE")
  ) |>
  summarise(
    correlacion = cor_segura(
      torres_acumuladas,
      cambio_menor_40_pp,
      "spearman"
    )
  ) |>
  pull(correlacion)

tabla_sensibilidad_menor_40 <- tibble(
  Escenario = c(
    "Todos los cantones GAM",
    "GAM sin cantón de San José"
  ),
  Pearson = round(
    c(cor_cambio_menor_40, cor_cambio_sin_san_jose),
    3
  ),
  Spearman = round(
    c(
      cor_cambio_menor_40_spearman,
      cor_cambio_sin_san_jose_spearman
    ),
    3
  )
)

# ------------------------------------------------------------
# 11. Tablas para el Quarto
# ------------------------------------------------------------

tabla_periodos_gam <- resumen_periodos_gam |>
  transmute(
    Período = periodo,
    `Viviendas <40 m²` = viviendas_menor_40,
    `Viviendas totales` = viviendas,
    `Participación agregada <40 m²` = participacion_agregada_menor_40,
    `Promedio anual <40 m²` = promedio_anual_menor_40,
    `Mediana anual <40 m²` = mediana_anual_menor_40
  )

tabla_evaluacion_cortes_gam <- evaluacion_cortes_gam |>
  mutate(
    reduccion_sse_vs_sin_corte =
      1 - sse_total / sse_sin_corte,
    corte = paste0(
      corte_fin_periodo_1,
      " / ",
      inicio_periodo_2
    )
  ) |>
  transmute(
    Corte = corte,
    `Años período 1` = n_periodo_1,
    `Años período 2` = n_periodo_2,
    `Media período 1` = media_periodo_1,
    `Media período 2` = media_periodo_2,
    SSE = sse_total,
    `Reducción SSE` = reduccion_sse_vs_sin_corte
  )

tabla_anios_clave_gam <- resumen_gam_anual |>
  filter(
    anio %in% unique(c(2014, anio_pico_menor_40, 2024))
  ) |>
  transmute(
    Año = anio,
    `Viviendas <40 m²` = viviendas_menor_40,
    `% <40 m²` = participacion_menor_40,
    `Viviendas 40-<70 m²` = viviendas_40_70,
    `% 40-<70 m²` = participacion_40_70,
    `200+ m²` = participacion_200_mas
  )

tabla_ranking_menor_40_pico <- ranking_menor_40_pico |>
  transmute(
    Provincia = str_to_title(str_to_lower(provincia)),
    Cantón = str_to_title(str_to_lower(canton)),
    `Viviendas <40 m²` = viviendas_menor_40,
    `Participación <40 m²` = participacion_menor_40
  )

tabla_torres_top <- ranking_torres |>
  transmute(
    Provincia = str_to_title(str_to_lower(provincia)),
    Cantón = str_to_title(str_to_lower(canton)),
    `Torres >10 niveles` = torres_mas_10
  )

tabla_correlaciones <- correlaciones_gam |>
  mutate(
    pearson = round(pearson, 3),
    spearman = round(spearman, 3)
  ) |>
  rename(
    Variable = variable,
    Pearson = pearson,
    Spearman = spearman
  )

# Valores escalares para la narrativa del Quarto.
participacion_lt40_2014 <- resumen_gam_anual |>
  filter(anio == 2014) |>
  pull(participacion_menor_40)

participacion_lt40_2024 <- resumen_gam_anual |>
  filter(anio == 2024) |>
  pull(participacion_menor_40)

participacion_lt40_pico <- pico_menor_40_gam |>
  pull(participacion_menor_40)

viviendas_lt40_pico <- pico_menor_40_gam |>
  pull(viviendas_menor_40)

promedio_lt40_2014_2018 <- resumen_periodos_gam |>
  filter(periodo == etiqueta_periodo_1) |>
  pull(promedio_anual_menor_40)

promedio_lt40_2019_2024 <- resumen_periodos_gam |>
  filter(periodo == etiqueta_periodo_2) |>
  pull(promedio_anual_menor_40)

mediana_lt40_2014_2018 <- resumen_periodos_gam |>
  filter(periodo == etiqueta_periodo_1) |>
  pull(mediana_anual_menor_40)

mediana_lt40_2019_2024 <- resumen_periodos_gam |>
  filter(periodo == etiqueta_periodo_2) |>
  pull(mediana_anual_menor_40)

# ------------------------------------------------------------
# 12. Gráficos
# ------------------------------------------------------------

# Figura 1: trayectoria anual de la participación <40 m² en la GAM.
grafico_menor_40_gam <- resumen_gam_anual |>
  ggplot(
    aes(
      x = anio,
      y = participacion_menor_40
    )
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_point(
    data = pico_menor_40_gam,
    size = 3
  ) +
  geom_text(
    data = pico_menor_40_gam,
    aes(
      label = paste0(
        anio,
        ": ",
        scales::percent(
          participacion_menor_40,
          accuracy = 0.1
        )
      )
    ),
    vjust = -0.8,
    size = 3.2
  ) +
  scale_x_continuous(breaks = 2014:2024) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA)
  ) +
  labs(
    x = "Año",
    y = "Participación de viviendas <40 m²"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Figura 2: composición anual de los seis rangos en la GAM.
grafico_composicion_gam <- composicion_gam |>
  ggplot(
    aes(
      x = factor(anio),
      y = participacion,
      fill = grupo_area
    )
  ) +
  geom_col(width = 0.82) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Año",
    y = "Participación de las viviendas",
    fill = "Grupo de área"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.major.x = element_blank()
  )

# Figura 3: concentración cantonal de las unidades <40 m² en el año pico.
grafico_menor_40_pico <- ranking_menor_40_pico |>
  mutate(
    canton_etiqueta = paste0(
      str_to_title(str_to_lower(canton)),
      " (",
      str_to_title(str_to_lower(provincia)),
      ")"
    )
  ) |>
  ggplot(
    aes(
      x = reorder(canton_etiqueta, viviendas_menor_40),
      y = viviendas_menor_40
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Viviendas <40 m²"
  ) +
  theme_minimal(base_size = 10)

# Figura 4: evolución anual de torres altas.
grafico_torres_anio <- resumen_torres_anio |>
  pivot_longer(
    c(torres_costa_rica, torres_gam),
    names_to = "ambito",
    values_to = "torres"
  ) |>
  mutate(
    ambito = recode(
      ambito,
      torres_costa_rica = "Costa Rica",
      torres_gam = "GAM"
    )
  ) |>
  ggplot(
    aes(
      x = anio,
      y = torres,
      linetype = ambito,
      shape = ambito
    )
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2014:2024) +
  labs(
    x = "Año",
    y = "Proyectos con más de 10 niveles",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Figura 5: perfil descriptivo con y sin torre alta en la GAM.
grafico_perfil_torre <- comparacion_torre_gam |>
  select(
    grupo_torre,
    participacion_menor_40,
    participacion_40_70,
    participacion_200_mas
  ) |>
  pivot_longer(
    -grupo_torre,
    names_to = "segmento",
    values_to = "participacion"
  ) |>
  mutate(
    segmento = recode(
      segmento,
      participacion_menor_40 = "Menos de 40 m²",
      participacion_40_70 = "40 a menos de 70 m²",
      participacion_200_mas = "200 m² o más"
    )
  ) |>
  ggplot(
    aes(
      x = segmento,
      y = participacion,
      fill = grupo_torre
    )
  ) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = NULL,
    y = "Participación promedio",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

# Figura 6: torres acumuladas y cambio de unidades <40 m².
grafico_cambio_torres <- cambios_canton |>
  ggplot(
    aes(
      x = torres_acumuladas,
      y = cambio_menor_40_pp
    )
  ) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  geom_text(
    data = cambios_canton |>
      arrange(desc(torres_acumuladas)) |>
      slice_head(n = 6),
    aes(label = str_to_title(str_to_lower(canton))),
    check_overlap = TRUE,
    vjust = -0.6,
    size = 3
  ) +
  labs(
    x = "Torres >10 niveles acumuladas, 2014-2024",
    y = "Cambio en viviendas <40 m² (puntos porcentuales)"
  ) +
  theme_minimal(base_size = 11)

# ------------------------------------------------------------
# 13. Guardar productos reproducibles
# ------------------------------------------------------------

readr::write_csv(
  inec_canton_resumen,
  "02_datos_procesados/INEC_indicadores_canton_2014_2024.csv"
)

readr::write_csv(
  resumen_gam_anual,
  "05_tablas/INEC_resumen_GAM_menor40_2014_2024.csv"
)

readr::write_csv(
  base_cruce,
  "02_datos_procesados/CFIA_INEC_torres_canton_2014_2024.csv"
)

readr::write_csv(
  torres_altas,
  "05_tablas/CFIA_torres_altas_2014_2024.csv"
)

readr::write_csv(
  ranking_torres,
  "05_tablas/CFIA_ranking_torres_canton.csv"
)

readr::write_csv(
  correlaciones_gam,
  "05_tablas/CFIA_INEC_correlaciones_GAM.csv"
)

readr::write_csv(
  cambios_canton,
  "05_tablas/CFIA_INEC_cambios_menor40_canton_GAM.csv"
)

readr::write_csv(
  concentracion_menor_40_gam,
  "05_tablas/INEC_concentracion_menor40_GAM.csv"
)

ggsave(
  "04_graficos/01_menor40_GAM_2014_2024.png",
  grafico_menor_40_gam,
  width = 8,
  height = 4.7,
  dpi = 300
)

ggsave(
  "04_graficos/02_composicion_tamanos_GAM.png",
  grafico_composicion_gam,
  width = 8,
  height = 4.7,
  dpi = 300
)

ggsave(
  "04_graficos/03_concentracion_menor40_anio_pico_GAM.png",
  grafico_menor_40_pico,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "04_graficos/04_torres_altas_por_anio.png",
  grafico_torres_anio,
  width = 8,
  height = 4.7,
  dpi = 300
)

ggsave(
  "04_graficos/05_perfil_tamano_torre_GAM.png",
  grafico_perfil_torre,
  width = 8,
  height = 4.7,
  dpi = 300
)

ggsave(
  "04_graficos/06_torres_vs_cambio_menor40_GAM.png",
  grafico_cambio_torres,
  width = 8,
  height = 4.7,
  dpi = 300
)

cat("\nManipulación y análisis completados correctamente.\n")
cat("Año pico de viviendas <40 m² en GAM:", anio_pico_menor_40, "\n")
cat(
  "Pearson año vs participación <40 m²:",
  round(cor_tiempo_menor_40_pearson, 3),
  "\n"
)
cat(
  "Spearman año vs participación <40 m²:",
  round(cor_tiempo_menor_40_spearman, 3),
  "\n"
)
cat(
  "Pearson torres acumuladas vs cambio <40 m²:",
  round(cor_cambio_menor_40, 3),
  "\n"
)
