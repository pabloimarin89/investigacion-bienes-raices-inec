# ============================================================
# INVESTIGACIÓN: VERTICALIZACIÓN Y TRANSFORMACIÓN DE LA VIVIENDA
# Script 05: Análisis preliminar CFIA 2014-2024
# ============================================================

source("03_scripts_R/00_configuracion.R")

ruta_cfia <- "02_datos_procesados/cfia_2014_2024_raw_homologada.rds"

if (!file.exists(ruta_cfia)) {
  stop(
    "No existe la base CFIA homologada. Ejecute primero ",
    "03_scripts_R/04_importacion_CFIA.R"
  )
}

cfia_raw <- readRDS(ruta_cfia)

# ------------------------------------------------------------
# 1. Definir GAM a nivel cantonal
# ------------------------------------------------------------
# El Plan GAM 2013-2030 comprende total o parcialmente
# 31 cantones. Esta primera versión utiliza una aproximación
# cantonal. Posteriormente podemos refinarla a nivel distrital.

gam_cantones <- tribble(
  ~provincia, ~canton,

  "SAN JOSE", "SAN JOSE",
  "SAN JOSE", "ESCAZU",
  "SAN JOSE", "DESAMPARADOS",
  "SAN JOSE", "ASERRI",
  "SAN JOSE", "MORA",
  "SAN JOSE", "GOICOECHEA",
  "SAN JOSE", "SANTA ANA",
  "SAN JOSE", "ALAJUELITA",
  "SAN JOSE", "VASQUEZ DE CORONADO",
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
) |>
  mutate(en_gam_aprox = TRUE)

cfia <- cfia_raw |>
  left_join(
    gam_cantones,
    by = c("provincia", "canton")
  ) |>
  mutate(
    en_gam_aprox = replace_na(en_gam_aprox, FALSE)
  )

# ------------------------------------------------------------
# 2. Filtrar construcción habitacional nueva
# ------------------------------------------------------------

cfia_residencial <- cfia |>
  filter(
    obra == "HABITACIONAL",
    clasificacion == "NUEVO"
  ) |>
  mutate(
    tipo_residencial = case_when(

      sub_obra %in% c(
        "APARTAMENTO",
        "APARTAMENTO UNIFAMILIAR"
      ) ~ "Apartamento",

      sub_obra %in% c(
        "CONDOMINIO",
        "TRANSFORMACION A CONDOMINIO"
      ) ~ "Condominio",

      sub_obra %in% c(
        "CASA",
        "CASA INTERÉS SOCIAL-EXONERADA",
        "CABAÑA",
        "CHALET"
      ) ~ "Casa",

      TRUE ~ "Otro habitacional"
    )
  )

# ------------------------------------------------------------
# 3. Resumen nacional por año
# ------------------------------------------------------------

resumen_cr <- cfia_residencial |>
  group_by(anio) |>
  summarise(
    area_residencial_m2 = sum(area, na.rm = TRUE),

    area_apartamentos_m2 =
      sum(
        area[tipo_residencial == "Apartamento"],
        na.rm = TRUE
      ),

    area_condominios_m2 =
      sum(
        area[tipo_residencial == "Condominio"],
        na.rm = TRUE
      ),

    proyectos_residenciales = n_distinct(proyecto),

    proyectos_apartamento =
      n_distinct(
        proyecto[tipo_residencial == "Apartamento"]
      ),

    proyectos_condominio =
      n_distinct(
        proyecto[tipo_residencial == "Condominio"]
      ),

    .groups = "drop"
  ) |>
  mutate(
    participacion_apartamentos =
      area_apartamentos_m2 / area_residencial_m2,

    participacion_multifamiliar =
      (area_apartamentos_m2 + area_condominios_m2) /
      area_residencial_m2
  )

write_csv(
  resumen_cr,
  "05_tablas/cfia_resumen_cr_2014_2024.csv"
)

# ------------------------------------------------------------
# 4. Resumen GAM aproximada por año
# ------------------------------------------------------------

resumen_gam <- cfia_residencial |>
  filter(en_gam_aprox) |>
  group_by(anio) |>
  summarise(
    area_residencial_m2 = sum(area, na.rm = TRUE),

    area_apartamentos_m2 =
      sum(
        area[tipo_residencial == "Apartamento"],
        na.rm = TRUE
      ),

    area_condominios_m2 =
      sum(
        area[tipo_residencial == "Condominio"],
        na.rm = TRUE
      ),

    proyectos_residenciales = n_distinct(proyecto),

    proyectos_apartamento =
      n_distinct(
        proyecto[tipo_residencial == "Apartamento"]
      ),

    proyectos_condominio =
      n_distinct(
        proyecto[tipo_residencial == "Condominio"]
      ),

    .groups = "drop"
  ) |>
  mutate(
    participacion_apartamentos =
      area_apartamentos_m2 / area_residencial_m2,

    participacion_multifamiliar =
      (area_apartamentos_m2 + area_condominios_m2) /
      area_residencial_m2
  )

write_csv(
  resumen_gam,
  "05_tablas/cfia_resumen_gam_2014_2024.csv"
)

# ------------------------------------------------------------
# 5. Agregación de niveles por proyecto
# ------------------------------------------------------------
# Según CFIA, la cantidad total de niveles debe agregarse por
# proyecto porque un proyecto puede estar dividido en varios
# usos/sectores. El concepto incluye sótanos.

niveles_proyecto <- cfia |>
  group_by(anio, proyecto) |>
  summarise(
    niveles_proyecto = if_else(
      all(is.na(niveles)),
      NA_real_,
      sum(niveles, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Proyectos que contienen apartamentos nuevos
proyectos_apartamento <- cfia_residencial |>
  filter(tipo_residencial == "Apartamento") |>
  distinct(
    anio,
    proyecto,
    provincia,
    canton,
    distrito,
    en_gam_aprox
  ) |>
  left_join(
    niveles_proyecto,
    by = c("anio", "proyecto")
  )

write_csv(
  proyectos_apartamento,
  "05_tablas/cfia_proyectos_apartamento_2014_2024.csv"
)

# ------------------------------------------------------------
# 6. Gráfico: área residencial por tipología en la GAM
# ------------------------------------------------------------

serie_tipo_gam <- cfia_residencial |>
  filter(
    en_gam_aprox,
    tipo_residencial %in% c(
      "Casa",
      "Apartamento",
      "Condominio"
    )
  ) |>
  group_by(anio, tipo_residencial) |>
  summarise(
    area_m2 = sum(area, na.rm = TRUE),
    .groups = "drop"
  )

grafico_area_tipo <- serie_tipo_gam |>
  ggplot(
    aes(
      x = anio,
      y = area_m2,
      linetype = tipo_residencial
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    breaks = 2014:2024
  ) +
  labs(
    title = "Área residencial nueva registrada en la GAM",
    subtitle = "Por tipología de obra, 2014-2024",
    x = "Año",
    y = "Metros cuadrados registrados",
    linetype = "Tipología",
    caption = paste(
      "Fuente: CFIA, APC.",
      "GAM aproximada a nivel cantonal."
    )
  ) +
  theme_minimal()

ggsave(
  "04_graficos/cfia_area_residencial_gam_2014_2024.png",
  grafico_area_tipo,
  width = 10,
  height = 6
)

# ------------------------------------------------------------
# 7. Gráfico: participación de apartamentos en la GAM
# ------------------------------------------------------------

grafico_participacion <- resumen_gam |>
  ggplot(
    aes(
      x = anio,
      y = participacion_apartamentos
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    breaks = 2014:2024
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    title = "Participación de apartamentos en el área residencial nueva",
    subtitle = "GAM, aproximación cantonal, 2014-2024",
    x = "Año",
    y = "Participación del área registrada",
    caption = "Fuente: CFIA, APC."
  ) +
  theme_minimal()

ggsave(
  "04_graficos/cfia_participacion_apartamentos_gam_2014_2024.png",
  grafico_participacion,
  width = 10,
  height = 6
)

cat("\nAnálisis preliminar CFIA completado.\n")
print(resumen_gam)

# ============================================================
# NOTA METODOLÓGICA
# ============================================================
# 1. El CFIA mide metros cuadrados tramitados/registrados:
#    representa intención o registro de construcción, no una
#    certificación de obra terminada.
#
# 2. La variable "niveles" incluye sótanos.
#
# 3. La GAM utilizada aquí es una aproximación por los 31
#    cantones del Plan GAM. El ámbito oficial incluye algunos
#    cantones solo parcialmente; una siguiente versión debe
#    trabajar a nivel de distrito.
# ============================================================
