# Investigación Bienes Raíces INEC

## Tema
Verticalización y transformación de la vivienda en la Gran Área Metropolitana de Costa Rica.

## Objetivo inicial
Analizar la evolución de la construcción residencial, el tamaño de las unidades habitacionales y el crecimiento de proyectos multifamiliares/verticales, utilizando principalmente datos del INEC y CFIA, con información del MEIC como complemento.

## Estructura del proyecto

- `01_datos_raw/`
  - `INEC/`: bases originales del INEC.
  - `CFIA/`: bases originales del CFIA/APC.
  - `MEIC/`: registros de proyectos inmobiliarios.
- `02_datos_procesados/`: bases limpias y transformadas.
- `03_scripts_R/`: código de R.
- `04_graficos/`: visualizaciones finales.
- `05_tablas/`: tablas de resultados.
- `06_presentacion/`: presentación del proyecto.
- `07_documentos/`: propuesta, notas metodológicas y documentación.

## Orden de ejecución

1. Abrir `Investigacion_Bienes_Raices_INEC.Rproj`.
2. Ejecutar `03_scripts_R/00_configuracion.R`.
3. Descargar la base de Estadísticas de la Construcción 2024 del INEC y guardarla en `01_datos_raw/INEC/`.
4. Ejecutar `03_scripts_R/01_importacion_INEC.R`.
5. Continuar con limpieza y análisis exploratorio.

## Regla de trabajo

Los archivos originales de las fuentes no se modifican. Toda transformación se guarda en `02_datos_procesados/`.

## Fuentes previstas

- INEC: Estadísticas de la Construcción, ENAHO y Censos.
- CFIA: Estadísticas APC.
- MEIC: Proyectos inmobiliarios autorizados para venta a plazo o ejecución futura.
