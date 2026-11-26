# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y VIVIENDA COMPACTA EN LA GAM
# Curso: CA-0204 Herramientas de Ciencia de Datos I
# Script 03: Análisis final INEC + CFIA, 2014-2024
# ============================================================

source("config/00_configuracion.R")

# ------------------------------------------------------------
# 1. Cargar bases procesadas
# ------------------------------------------------------------

ruta_inec <- "data/procesados/INEC_construccion_homologada_2014_2024.rds"
ruta_cfia <- "data/procesados/cfia_2014_2024_raw_homologada.rds"

if (!file.exists(ruta_inec)) {
  stop("Falta la base INEC. Ejecute primero cod/02_homologacion_INEC.R")
}

if (!file.exists(ruta_cfia)) {
  stop("Falta la base CFIA. Ejecute primero cod/01_importacion_CFIA.R")
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
    str_replace_all("VASQUEZ DE CORONADO", "VAZQUEZ DE CORONADO")
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
# 3. Aproximación cantonal de la GAM
# ------------------------------------------------------------
# Los cuadros públicos del INEC están disponibles por cantón. Por ello,
# cuando un cantón pertenece parcialmente a la GAM se utiliza completo.

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
# 4. INEC: indicador principal de viviendas menores de 40 m²
# ------------------------------------------------------------

inec_canton_resumen <- inec |>
  filter(nivel_geo == "Cantón") |>
  group_by(anio, provincia, canton) |>
  summarise(
    viviendas_menor_40 = sum(
      .data$viviendas[grupo_area == "Menos de 40"],
      na.rm = TRUE
    ),
    viviendas = sum(.data$viviendas, na.rm = TRUE),
    permisos = sum(.data$permisos, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    participacion_menor_40 = if_else(
      viviendas > 0,
      viviendas_menor_40 / viviendas,
      NA_real_
    )
  )

inec_gam <- inec_canton_resumen |>
  inner_join(gam, by = c("provincia", "canton"))

resumen_gam_anual <- inec_gam |>
  group_by(anio) |>
  summarise(
    viviendas = sum(viviendas, na.rm = TRUE),
    viviendas_menor_40 = sum(viviendas_menor_40, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    participacion_menor_40 = viviendas_menor_40 / viviendas
  )

# ------------------------------------------------------------
# 5. Corte temporal exploratorio
# ------------------------------------------------------------
# Se prueban cortes que dejan al menos tres años en cada segmento.
# El criterio es minimizar la suma de cuadrados dentro de los períodos.

anios_gam <- sort(unique(resumen_gam_anual$anio))
cortes_candidatos <- anios_gam[3:(length(anios_gam) - 3)]

evaluacion_cortes_gam <- map_dfr(
  cortes_candidatos,
  function(corte) {
    periodo_1 <- resumen_gam_anual |>
      filter(anio <= corte) |>
      pull(participacion_menor_40)

    periodo_2 <- resumen_gam_anual |>
      filter(anio > corte) |>
      pull(participacion_menor_40)

    sse_1 <- sum((periodo_1 - mean(periodo_1))^2)
    sse_2 <- sum((periodo_2 - mean(periodo_2))^2)

    tibble(
      corte_fin_periodo_1 = corte,
      inicio_periodo_2 = corte + 1,
      n_periodo_1 = length(periodo_1),
      n_periodo_2 = length(periodo_2),
      media_periodo_1 = mean(periodo_1),
      media_periodo_2 = mean(periodo_2),
      sse_total = sse_1 + sse_2
    )
  }
)

sse_sin_corte <- sum(
  (resumen_gam_anual$participacion_menor_40 -
     mean(resumen_gam_anual$participacion_menor_40))^2
)

mejor_corte_gam <- evaluacion_cortes_gam |>
  slice_min(sse_total, n = 1, with_ties = FALSE)

anio_inicio_segundo_periodo <- mejor_corte_gam$inicio_periodo_2
reduccion_sse_corte <- 1 - mejor_corte_gam$sse_total / sse_sin_corte

etiqueta_periodo_1 <- paste0(min(anios_gam), "-", anio_inicio_segundo_periodo - 1)
etiqueta_periodo_2 <- paste0(anio_inicio_segundo_periodo, "-", max(anios_gam))

resumen_gam_anual <- resumen_gam_anual |>
  mutate(
    periodo = if_else(
      anio < anio_inicio_segundo_periodo,
      etiqueta_periodo_1,
      etiqueta_periodo_2
    )
  )

resumen_periodos_gam <- resumen_gam_anual |>
  group_by(periodo) |>
  summarise(
    promedio_anual_menor_40 = mean(participacion_menor_40),
    mediana_anual_menor_40 = median(participacion_menor_40),
    .groups = "drop"
  )

pico_menor_40_gam <- resumen_gam_anual |>
  slice_max(participacion_menor_40, n = 1, with_ties = FALSE)

anio_pico_menor_40 <- pico_menor_40_gam$anio

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
# 6. Concentración territorial en el año pico
# ------------------------------------------------------------

ranking_menor_40_pico <- inec_gam |>
  filter(anio == anio_pico_menor_40) |>
  arrange(desc(viviendas_menor_40))

concentracion_pico <- ranking_menor_40_pico |>
  summarise(
    viviendas_menor_40_gam = sum(viviendas_menor_40),
    participacion_lider = first(viviendas_menor_40) /
      viviendas_menor_40_gam,
    participacion_top3 = sum(head(viviendas_menor_40, 3)) /
      viviendas_menor_40_gam
  )

# ------------------------------------------------------------
# 7. CFIA: torres residenciales altas probables
# ------------------------------------------------------------
# Se considera torre alta probable un proyecto nuevo con componente
# de apartamento/condominio y más de 10 niveles acumulados.

tipos_torre <- c(
  "APARTAMENTO",
  "APARTAMENTO UNIFAMILIAR",
  "CONDOMINIO",
  "TRANSFORMACION A CONDOMINIO"
)

proyectos_cfia <- cfia |>
  filter(clasificacion == "NUEVO") |>
  mutate(
    componente_residencial =
      obra == "HABITACIONAL" & sub_obra %in% tipos_torre
  ) |>
  group_by(anio, proyecto, provincia, canton) |>
  summarise(
    niveles_total = sum(niveles, na.rm = TRUE),
    tiene_componente_residencial = any(componente_residencial),
    .groups = "drop"
  )

torres_altas <- proyectos_cfia |>
  filter(tiene_componente_residencial, niveles_total > 10)

torres_canton_anio <- torres_altas |>
  count(anio, provincia, canton, name = "torres_mas_10")

torres_gam <- torres_altas |>
  inner_join(gam, by = c("provincia", "canton"))

total_torres_cr <- nrow(torres_altas)
total_torres_gam <- nrow(torres_gam)

# ------------------------------------------------------------
# 8. Asociación entre torres y viviendas menores de 40 m²
# ------------------------------------------------------------

base_cruce_gam <- inec_gam |>
  left_join(
    torres_canton_anio,
    by = c("anio", "provincia", "canton")
  ) |>
  mutate(
    torres_mas_10 = replace_na(torres_mas_10, 0L)
  )

cor_segura <- function(x, y, metodo = "pearson") {
  cor(x, y, method = metodo, use = "complete.obs")
}

correlaciones_gam <- tibble(
  variable = "Viviendas menores de 40 m²",
  pearson = cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_menor_40,
    "pearson"
  ),
  spearman = cor_segura(
    base_cruce_gam$torres_mas_10,
    base_cruce_gam$participacion_menor_40,
    "spearman"
  )
)

# ------------------------------------------------------------
# 9. Robustez: cambio 2014-2024 por cantón
# ------------------------------------------------------------

cambios_canton <- base_cruce_gam |>
  filter(anio %in% c(2014, 2024)) |>
  select(anio, provincia, canton, participacion_menor_40) |>
  pivot_wider(
    names_from = anio,
    values_from = participacion_menor_40,
    names_prefix = "participacion_"
  ) |>
  mutate(
    cambio_menor_40_pp = 100 *
      (participacion_2024 - participacion_2014)
  )

torres_acumuladas_gam <- torres_gam |>
  count(provincia, canton, name = "torres_acumuladas")

cambios_canton <- cambios_canton |>
  left_join(torres_acumuladas_gam, by = c("provincia", "canton")) |>
  mutate(
    torres_acumuladas = replace_na(torres_acumuladas, 0L)
  )

cor_cambio_menor_40 <- cor_segura(
  cambios_canton$torres_acumuladas,
  cambios_canton$cambio_menor_40_pp,
  "pearson"
)

cor_cambio_menor_40_spearman <- cor_segura(
  cambios_canton$torres_acumuladas,
  cambios_canton$cambio_menor_40_pp,
  "spearman"
)

cambios_sin_san_jose <- cambios_canton |>
  filter(!(provincia == "SAN JOSE" & canton == "SAN JOSE"))

cor_cambio_sin_san_jose <- cor_segura(
  cambios_sin_san_jose$torres_acumuladas,
  cambios_sin_san_jose$cambio_menor_40_pp,
  "pearson"
)

cor_cambio_sin_san_jose_spearman <- cor_segura(
  cambios_sin_san_jose$torres_acumuladas,
  cambios_sin_san_jose$cambio_menor_40_pp,
  "spearman"
)

# ------------------------------------------------------------
# 10. Objetos para el informe
# ------------------------------------------------------------

tabla_evaluacion_cortes_gam <- evaluacion_cortes_gam |>
  mutate(
    `Reducción SSE` = 1 - sse_total / sse_sin_corte,
    Corte = paste0(corte_fin_periodo_1, " / ", inicio_periodo_2)
  ) |>
  transmute(
    Corte,
    `Años período 1` = n_periodo_1,
    `Años período 2` = n_periodo_2,
    `Media período 1` = media_periodo_1,
    `Media período 2` = media_periodo_2,
    SSE = sse_total,
    `Reducción SSE`
  )

tabla_correlaciones <- correlaciones_gam |>
  transmute(
    Variable = variable,
    Pearson = round(pearson, 3),
    Spearman = round(spearman, 3)
  )

participacion_lt40_2014 <- resumen_gam_anual |>
  filter(anio == 2014) |>
  pull(participacion_menor_40)

participacion_lt40_2024 <- resumen_gam_anual |>
  filter(anio == 2024) |>
  pull(participacion_menor_40)

participacion_lt40_pico <- pico_menor_40_gam$participacion_menor_40
viviendas_lt40_pico <- pico_menor_40_gam$viviendas_menor_40

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
# 11. Figuras finales
# ------------------------------------------------------------

grafico_menor_40_gam <- resumen_gam_anual |>
  ggplot(aes(x = anio, y = participacion_menor_40)) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 2.2
  ) +
  geom_point(
    data = pico_menor_40_gam,
    color = "#D95F02",
    size = 3.5
  ) +
  geom_label(
    data = pico_menor_40_gam,
    aes(
      label = paste0(
        anio,
        ": ",
        scales::percent(participacion_menor_40, accuracy = 0.1)
      )
    ),
    color = "#D95F02",
    fill = "white",
    label.size = 0.2,
    fontface = "bold",
    nudge_y = -0.006,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = 2014:2024,
    expand = expansion(add = c(0.2, 0.35))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.07),
    breaks = seq(0, 0.06, by = 0.02),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Año",
    y = "Participación de viviendas <40 m²"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

grafico_menor_40_pico <- ranking_menor_40_pico |>
  slice_head(n = 10) |>
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
  labs(x = NULL, y = "Viviendas <40 m²") +
  theme_minimal(base_size = 10)

grafico_cambio_torres <- cambios_canton |>
  ggplot(
    aes(
      x = torres_acumuladas,
      y = cambio_menor_40_pp
    )
  ) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
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
# 12. Guardar productos finales
# ------------------------------------------------------------

write_csv(
  resumen_gam_anual,
  "info/tablas/INEC_resumen_GAM_menor40_2014_2024.csv"
)

write_csv(
  concentracion_pico,
  "info/tablas/INEC_concentracion_menor40_GAM.csv"
)

write_csv(
  correlaciones_gam,
  "info/tablas/CFIA_INEC_correlaciones_GAM.csv"
)

write_csv(
  cambios_canton,
  "info/tablas/CFIA_INEC_cambios_menor40_canton_GAM.csv"
)

ggsave(
  "info/figuras/01_menor40_GAM_2014_2024.png",
  grafico_menor_40_gam,
  width = 8,
  height = 4.7,
  dpi = 300
)

ggsave(
  "info/figuras/02_concentracion_menor40_anio_pico_GAM.png",
  grafico_menor_40_pico,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "info/figuras/03_torres_vs_cambio_menor40_GAM.png",
  grafico_cambio_torres,
  width = 8,
  height = 4.7,
  dpi = 300
)

cat("\nAnálisis final completado correctamente.\n")
cat("Año pico <40 m²:", anio_pico_menor_40, "\n")
cat("Torres altas probables CR:", total_torres_cr, "\n")
cat("Torres altas probables GAM:", total_torres_gam, "\n")
