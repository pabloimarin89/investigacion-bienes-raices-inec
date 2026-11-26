# Proyecto individual - Herramientas de Datos I 2026

## Tema
**Verticalización y transformación del tamaño de la vivienda en Costa Rica:** viviendas menores de 40 m² y torres residenciales en la Gran Área Metropolitana (GAM), 2014-2024.

## Autor
Pablo Ibarra Marín

## Objetivo
Analizar la evolución de las viviendas autorizadas de menos de 40 m² en la GAM entre 2014 y 2024 y evaluar si su crecimiento se encuentra asociado con la presencia de proyectos residenciales de más de 10 niveles registrados en el CFIA.

## Estructura del proyecto
La estructura sigue el estándar de clase solicitado en la rúbrica:

- `cod/`: scripts finales de importación, homologación y análisis.
- `data/raw/`: archivos originales del INEC y CFIA; no se modifican.
- `data/procesados/`: bases derivadas que generan los scripts.
- `info/figuras/`: gráficos finales.
- `info/tablas/`: tablas de resultados y validación.
- `info/presentacion/`: material de apoyo para la exposición.
- `info/documentos/`: documentación complementaria.
- `config/`: configuración general y paquetes.

## Orden reproducible de ejecución
1. Abrir `Individual_Ibarra_HerramientasDatosI_2026.Rproj`.
2. Ejecutar `config/00_configuracion.R`.
3. Ejecutar `cod/01_importacion_CFIA.R`.
4. Ejecutar `cod/02_homologacion_INEC.R`.
5. Ejecutar `cod/03_analisis.R`.
6. Renderizar `Individual_Ibarra_HerramientasDatosI_2026.qmd`.

El proyecto utiliza rutas relativas y no utiliza `setwd()` con rutas absolutas.

## Fuentes
- INEC: Estadísticas de la Construcción, cuadros anuales 2014-2024.
- CFIA: registros del Administrador de Proyectos de Construcción (APC), 2014-2024.

## Reproducibilidad
Los archivos originales se conservan en `data/raw/`. Las transformaciones se generan por código y se guardan en `data/procesados/`, `info/tablas/` e `info/figuras/`. El informe oculta el código (`echo: false`) y remite al repositorio público para su revisión.

Repositorio: https://github.com/pabloimarin89/investigacion-bienes-raices-inec
