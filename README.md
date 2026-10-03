# Análisis de nacimientos en el Atlántico, 2024

Libro en **bookdown** con el análisis exploratorio de los nacimientos
registrados en el departamento del Atlántico en 2024, a partir de los
microdatos de Estadísticas Vitales (EEVV) del DANE.

Autores: Brandon Sanchez, Andres Florez — curso de Estadística Computacional.

## Estructura

- `index.Rmd`: presentación y configuración global.
- `01-introduccion.Rmd`: contexto (por redactar), objetivos y estructura del
  documento.
- `02-datos.Rmd`: fuente, selección de variables, limpieza y valores faltantes.
- `03-univariado.Rmd`: descripción individual de cada variable.
- `04-bivariado.Rmd`: relaciones entre pares de variables.
- `05-hipotesis.Rmd`: pruebas de hipótesis por objetivo (por redactar).
- `06-conclusiones.Rmd`: hallazgos principales (por redactar).
- `07-referencias.Rmd`: referencias.
- `R/`: `estilo.R` (paleta, tema ggplot, formatos y ayudas de estilo),
  `tablas.R` (estilo de tablas `gt`). Solo estilo: la lógica vive en cada capítulo.
- `data/`: `BD-EEVV-Nacimientos-2024.csv` (no se versiona, ver `.gitignore`).

## Reproducir el libro

1. Colocar el CSV del DANE en `data/BD-EEVV-Nacimientos-2024.csv`.
2. En R:
   ```r
   install.packages("bookdown")
   bookdown::render_book("index.Rmd")
   ```
3. La salida HTML se genera en `docs/` (lista para GitHub Pages).
