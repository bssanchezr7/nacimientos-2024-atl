# Análisis de nacimientos en el Atlántico, 2024

Libro en **bookdown** con el análisis exploratorio y las pruebas de hipótesis
sobre los nacimientos registrados en el departamento del Atlántico en 2024, a
partir de los microdatos de Estadísticas Vitales (EEVV) del DANE.

Libro publicado: <https://bssanchezr7.github.io/nacimientos-2024-atl/>

Autores: Brandon Sanchez, Andres Florez — curso de Estadística Computacional.

## Estructura

- `index.Rmd`: presentación, estructura del documento y configuración global.
- `01-introduccion.Rmd`: contexto y objetivos.
- `02-datos.Rmd`: fuente, población, variables y depuración de los datos.
- `03-univariado.Rmd`: descripción individual de cada variable.
- `04-bivariado.Rmd`: cruces asociados a cada objetivo.
- `05-hipotesis.Rmd`: pruebas de hipótesis por objetivo.
- `06-conclusiones.Rmd`: hallazgos principales (por redactar).
- `07-referencias.Rmd`: referencias.
- `R/`: `estilo.R` (paleta, tema ggplot, formatos y ayudas de estilo),
  `tablas.R` (estilo de tablas `gt`). Solo estilo: la lógica vive en cada capítulo.
- `data/`: `BD-EEVV-Nacimientos-2024.csv` (no se versiona, ver `.gitignore`).

## Reproducir el libro

1. Colocar el CSV del DANE en `data/BD-EEVV-Nacimientos-2024.csv`.
2. En R:
   ```r
   install.packages(c("bookdown", "tidyverse", "gt", "nortest", "car", "rstatix"))
   bookdown::render_book("index.Rmd")
   ```
3. La salida HTML se genera en `docs/`, que es la carpeta que publica GitHub
   Pages. Para actualizar el libro publicado hay que compilar y subir esa
   carpeta.
