# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 03: Exploración inicial INEC 2024
# ============================================================

source("03_scripts_R/00_configuracion.R")

ruta_entrada <- "02_datos_procesados/inec_vivienda_2024_limpia.rds"

if (!file.exists(ruta_entrada)) {
  stop(
    "No existe la base limpia. Ejecute primero ",
    "03_scripts_R/02_limpieza_INEC.R"
  )
}

vivienda_2024 <- readRDS(ruta_entrada)

# ------------------------------------------------------------
# 1. Resumen nacional
# ------------------------------------------------------------

resumen_nacional <- vivienda_2024 |>
  filter(numviv > 0) |>
  summarise(
    permisos = n(),
    obras = sum(num_obras, na.rm = TRUE),
    viviendas = sum(numviv, na.rm = TRUE),
    area_total_m2 = sum(arecon, na.rm = TRUE),
    area_por_vivienda = area_total_m2 / viviendas,
    dormitorios_por_vivienda =
      sum(numdor, na.rm = TRUE) / viviendas,
    pisos_maximo = max(numpis, na.rm = TRUE)
  )

print(resumen_nacional)

write_csv(
  resumen_nacional,
  "05_tablas/resumen_nacional_2024.csv"
)

# ------------------------------------------------------------
# 2. Resumen por tipo aproximado de proyecto
# ------------------------------------------------------------

resumen_tipo <- vivienda_2024 |>
  filter(numviv > 0) |>
  group_by(tipo_proyecto_aprox) |>
  summarise(
    permisos = n(),
    viviendas = sum(numviv, na.rm = TRUE),
    area_total_m2 = sum(arecon, na.rm = TRUE),
    area_por_vivienda = area_total_m2 / viviendas,
    dormitorios_por_vivienda =
      sum(numdor, na.rm = TRUE) / viviendas,
    pisos_maximo = max(numpis, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(viviendas))

print(resumen_tipo)

write_csv(
  resumen_tipo,
  "05_tablas/resumen_tipo_proyecto_2024.csv"
)

# ------------------------------------------------------------
# 3. Resumen por cantón
# ------------------------------------------------------------

resumen_canton <- vivienda_2024 |>
  filter(numviv > 0) |>
  group_by(pc_num_cant) |>
  summarise(
    permisos = n(),
    viviendas = sum(numviv, na.rm = TRUE),
    area_total_m2 = sum(arecon, na.rm = TRUE),
    area_por_vivienda = area_total_m2 / viviendas,
    dormitorios_por_vivienda =
      sum(numdor, na.rm = TRUE) / viviendas,
    pisos_maximo = max(numpis, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(viviendas))

write_csv(
  resumen_canton,
  "05_tablas/resumen_canton_2024.csv"
)

# ------------------------------------------------------------
# 4. Gráfico de distribución del área por vivienda
# ------------------------------------------------------------

limite_99 <- quantile(
  vivienda_2024$area_por_vivienda,
  0.99,
  na.rm = TRUE
)

grafico_area <- vivienda_2024 |>
  filter(
    area_por_vivienda > 0,
    area_por_vivienda <= limite_99
  ) |>
  ggplot(aes(x = area_por_vivienda)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Área construida por vivienda",
    subtitle = "Construcciones residenciales nuevas, Costa Rica 2024",
    x = "Metros cuadrados por vivienda",
    y = "Número de permisos",
    caption = "Fuente: INEC, Estadísticas de la Construcción 2024"
  ) +
  theme_minimal()

print(grafico_area)

ggsave(
  "04_graficos/distribucion_area_vivienda_2024.png",
  grafico_area,
  width = 9,
  height = 6
)

# ------------------------------------------------------------
# 5. Gráfico por tipo aproximado de proyecto
# ------------------------------------------------------------

grafico_tipo <- resumen_tipo |>
  ggplot(
    aes(
      x = reorder(tipo_proyecto_aprox, viviendas),
      y = viviendas
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Viviendas autorizadas por tipo aproximado de proyecto",
    subtitle = "Costa Rica, 2024",
    x = NULL,
    y = "Número de viviendas",
    caption = "Fuente: INEC, Estadísticas de la Construcción 2024"
  ) +
  theme_minimal()

print(grafico_tipo)

ggsave(
  "04_graficos/viviendas_tipo_proyecto_2024.png",
  grafico_tipo,
  width = 9,
  height = 6
)

cat("\nExploración inicial completada.\n")
