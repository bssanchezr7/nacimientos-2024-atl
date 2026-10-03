# Estética común del proyecto: paleta, formato de números y tema de ggplot.
# Uso: source("R/estilo.R"); source("R/tablas.R")
# Este archivo es SOLO estilo: no arma datos ni calcula resúmenes.

library(tidyverse)
library(gt)

# PALETA ---------------------------------------------------------------------
# Un color principal (azul petróleo), su versión clara para lo secundario y
# un acento cálido que se usa solo para marcar referencias (media, mediana).
pal <- c(
  principal = "#2E5A6B",
  claro     = "#A9C1CA",
  acento    = "#C8764A",
  texto     = "#2B2B2B",
  gris      = "#7A7A7A",
  linea     = "#E3E3E3",
  fondo_tab = "#F3F6F7"
)

# Colores para grupos nominales: apagados y distinguibles entre sí
pal_cat <- c("#2E5A6B", "#C8764A", "#8AA89A", "#D9B26F", "#6E6A8F", "#A9C1CA")

# Escala secuencial (para variables ordinales o muchos niveles)
pal_seq <- function(n) colorRampPalette(c("#DDE7EB", "#6F97A6", "#1C3A45"))(n)

# Devuelve n colores: secuenciales si la variable es ordinal o no alcanza pal_cat
colores_grupo <- function(n, ordinal = FALSE) {
  if (ordinal || n > length(pal_cat)) pal_seq(n) else pal_cat[seq_len(n)]
}

# Texto blanco sobre fondos oscuros, oscuro sobre fondos claros
color_texto_sobre <- function(fondos) {
  rgb <- grDevices::col2rgb(fondos) / 255
  lum <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  ifelse(lum < 0.55, "white", pal[["texto"]])
}

fuente_dane <- "Fuente: DANE, Estadísticas Vitales 2024"
sub_atl     <- "Madres residentes en el Atlántico, 2024"

# FORMATO DE NÚMEROS (convención colombiana) --------------------------------
num <- function(x, acc = 1) scales::number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")
pct <- function(x, acc = 0.1) scales::percent(x, accuracy = acc, big.mark = ".", decimal.mark = ",")
# Sin decimales si el valor es entero, con `acc` si no
num_auto <- function(x, acc = 0.1) ifelse(x == round(x), num(x, 1), num(x, acc))

# TEMA GGPLOT ----------------------------------------------------------------
theme_proyecto <- function(base_size = 12, grid = "y") {
  t <- theme_minimal(base_size = base_size) +
    theme(
      text               = element_text(color = pal[["texto"]]),
      plot.title         = element_text(face = "bold", size = rel(1.25), margin = margin(b = 4)),
      plot.subtitle      = element_text(color = pal[["gris"]], margin = margin(b = 12)),
      plot.caption       = element_text(color = pal[["gris"]], size = rel(0.75), hjust = 0,
                                        margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title         = element_text(color = pal[["gris"]], size = rel(0.85)),
      axis.text          = element_text(color = pal[["texto"]]),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(color = pal[["linea"]], linewidth = 0.4),
      plot.margin        = margin(12, 16, 10, 12),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_text(color = pal[["gris"]], size = rel(0.85))
    )
  # Solo se deja la grilla del eje de valores
  if (grid == "y")    t <- t + theme(panel.grid.major.x = element_blank())
  if (grid == "x")    t <- t + theme(panel.grid.major.y = element_blank())
  if (grid == "none") t <- t + theme(panel.grid.major = element_blank())
  t
}

theme_set(theme_proyecto())
update_geom_defaults("bar",  list(fill = pal[["principal"]]))
update_geom_defaults("col",  list(fill = pal[["principal"]]))
update_geom_defaults("text", list(color = pal[["texto"]], size = 3.4))

# AYUDAS DE ESTILO PARA GRÁFICOS YA ARMADOS ----------------------------------
# Solo visten un ggplot existente: no cuentan, no resumen, no filtran.
# La lógica (count, summarise, mutate) vive en cada capítulo.

# Líneas de media (continua) y mediana (punteada) en color de acento.
# Recibe el vector ya disponible en el capítulo, p. ej. lineas_ref(df$X)
lineas_ref <- function(x) {
  ref <- tibble(tipo = c("Media", "Mediana"), valor = c(mean(x), median(x)))
  list(
    geom_vline(data = ref, aes(xintercept = valor, linetype = tipo),
               color = pal[["acento"]], linewidth = 0.7),
    scale_linetype_manual(values = c(Media = "solid", Mediana = "22"), name = NULL)
  )
}

# Punto de la media (rombo) para boxplots
punto_media <- function() {
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = pal[["acento"]], color = "white")
}

# Ejes con cortes enteros si la variable es discreta (evita 2,5 o 7,5)
es_discreta <- function(x) all(x == round(x)) && diff(range(x)) <= 60
cortes_para <- function(x) {
  if (!es_discreta(x)) return(waiver())
  \(l) { b <- scales::breaks_extended(7)(l); b[b == round(b)] }
}
escala_x <- function(x, ...) scale_x_continuous(breaks = cortes_para(x), labels = num_auto, ...)
escala_y <- function(x, ...) scale_y_continuous(breaks = cortes_para(x), labels = num_auto, ...)
