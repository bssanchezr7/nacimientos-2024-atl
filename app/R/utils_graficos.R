# Ayudantes de gráficos compartidos por los módulos: formato de porcentaje,
# silenciador del aviso de `text`, tema de los gráficos interactivos y
# conversión de ggplot a plotly.
#
# Los gráficos de ggplot (paleta y tema del libro, R/estilo.R) se convierten
# con `interactivo()`. Los que la conversión no resuelve bien se arman con
# plotly y se visten con `estilo_interactivo()`.

fmt_pct <- function(p, acc = 0.1) paste0(num(100 * p, acc), " %")

# ggplot2 4.0 avisa que `text` no es una estética de geom_col, pero plotly la
# usa como globo. Se silencia solo ese aviso concreto, no los demás.
callar_texto <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("unknown aesthetic", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

# Colores del tema claro/oscuro y ajustes comunes a todos los gráficos.
estilo_interactivo <- function(p, oscuro = FALSE, con_leyenda = FALSE, nota = NULL) {
  texto  <- if (oscuro) "#DDE7EB" else pal[["texto"]]
  tenue  <- if (oscuro) "#8FA6AE" else pal[["gris"]]
  fondo  <- if (oscuro) "#22404B" else "#FFFFFF"
  grilla <- if (oscuro) "#3A5C68" else pal[["linea"]]
  sin_fondo <- "rgba(0,0,0,0)"

  eje <- list(tickfont = list(color = texto), gridcolor = grilla,
              title = list(font = list(color = tenue)),
              # Sin marcas visibles, pero con su largo: así la etiqueta no
              # queda pegada al inicio de la barra.
              ticks = "outside", ticklen = 10,
              tickcolor = sin_fondo, automargin = TRUE, zeroline = FALSE)

  leyenda <- list(font = list(color = tenue, size = 11),
                  title = list(font = list(color = tenue, size = 11)))
  if (con_leyenda) {
    leyenda <- c(leyenda, list(orientation = "h", x = 0, xanchor = "left",
                               y = 1.02, yanchor = "bottom"))
  }

  # Pie de gráfico (por ejemplo "◆ = media"): plotly no tiene caption, va como
  # anotación en el margen inferior.
  margen <- list(t = if (con_leyenda) 90 else 44)
  if (!is.null(nota)) margen$b <- 78

  anotacion <- if (is.null(nota)) NULL else list(list(
    text = nota, x = 0, y = -0.16, xref = "paper", yref = "paper",
    xanchor = "left", yanchor = "top", showarrow = FALSE,
    font = list(color = tenue, size = 11)
  ))

  p |>
    plotly::layout(
      paper_bgcolor = sin_fondo,
      plot_bgcolor  = sin_fondo,
      title      = list(font = list(color = texto, size = 15),
                        x = 0, xanchor = "left"),
      font       = list(color = texto, size = 12,
                        family = "Source Sans 3, Segoe UI, Arial, sans-serif"),
      hoverlabel = list(bgcolor = fondo,
                        bordercolor = pal[["principal"]],
                        font = list(color = texto, size = 12)),
      legend     = leyenda,
      annotations = anotacion,
      margin     = if (length(margen)) margen else NULL,
      modebar    = list(bgcolor = sin_fondo, color = tenue, activecolor = texto),
      xaxis      = eje,
      yaxis      = eje
    ) |>
    plotly::config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      locale = "es",
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                 "toggleSpikelines", "hoverClosestCartesian",
                                 "hoverCompareCartesian")
    ) |>
    plotly::plotly_build() |>
    aligerar()
}

# Deja el widget en su forma mínima. `plotly_build()` guarda en `x$attrs` una
# copia de las trazas para seguir construyendo el gráfico en R, y repite los
# atributos escalares (como `hoverinfo`) una vez por punto. Shiny serializa
# ambas cosas, así que el widget viaja mucho más pesado de lo necesario.
aligerar <- function(p) {
  p$x$attrs <- NULL

  for (i in seq_along(p$x$data)) {
    globo <- unlist(p$x$data[[i]]$hoverinfo)
    if (length(globo) > 1 && length(unique(globo)) == 1) {
      p$x$data[[i]]$hoverinfo <- globo[[1]]
    }
  }

  p
}

# Convierte el ggplot en un widget interactivo.
interactivo <- function(g, oscuro = FALSE) {
  p <- plotly::ggplotly(g, tooltip = "text")

  con_leyenda <- any(vapply(p$x$data, function(z) isTRUE(z$showlegend),
                            logical(1)))

  # El build tiene que ir después de layout(): al revés, plotly reconstruye
  # desde el ggplot y descarta los colores del tema.
  p <- estilo_interactivo(p, oscuro, con_leyenda = con_leyenda)

  # Las líneas de media/mediana no llevan globo: el dato se lee en el gráfico.
  for (i in seq_along(p$x$data)) {
    if (identical(p$x$data[[i]]$name, "Media") ||
        identical(p$x$data[[i]]$name, "Mediana")) {
      p$x$data[[i]]$hoverinfo <- "skip"
    }
  }

  p
}
