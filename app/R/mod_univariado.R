# mod_univariado.R — pestaña "Univariado"
#
# El gráfico y la tabla dependen del tipo de la variable:
#   - Numérica: barras por valor con líneas de media y mediana; tabla de resumen.
#   - Factor:   barras horizontales ordenadas por frecuencia; tabla de n y %.
#
# Los gráficos se arman con ggplot y se convierten
# a interactivos con ggplotly al final, en `interactivo()` (utils_graficos.R).

# ---- gráficos --------------------------------------------------------------

grafico_numerica <- function(x, v) {
  frec <- tibble::tibble(valor = x) |>
    count(valor) |>
    mutate(globo = paste0("Valor: ", num_auto(valor),
                          "<br>Nacimientos: ", num(n),
                          " (", fmt_pct(n / sum(n)), ")"))

  ggplot(frec, aes(x = valor, y = n)) +
    geom_col(aes(text = globo), fill = pal[["principal"]], color = "white",
             linewidth = 0.3, width = 1) +
    lineas_ref(x) +
    escala_x(x) +
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.12))) +
    labs(title = etiquetas[[v]], x = NULL, y = "Nacimientos") +
    theme_proyecto()
}

grafico_factor <- function(x, v) {
  frec <- tibble::tibble(nivel = x) |>
    count(nivel) |>
    mutate(nivel = forcats::fct_reorder(nivel, n),
           globo = paste0(as.character(nivel),
                          "<br>Nacimientos: ", num(n),
                          " (", fmt_pct(n / sum(n)), ")"))

  ggplot(frec, aes(x = n, y = nivel)) +
    geom_col(aes(text = globo), width = 0.7, fill = pal[["principal"]]) +
    scale_x_continuous(labels = num, expand = expansion(mult = c(0, 0.08))) +
    labs(title = etiquetas[[v]], x = "Nacimientos", y = NULL) +
    theme_proyecto(grid = "x")
}

# ---- tablas ----------------------------------------------------------------

resumen_numerica <- function(x) {
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)

  tibble::tibble(
    Indicador = c("Nacimientos", "Media", "Desv. estándar", "Mediana",
                  "RIC (Q1–Q3)", "Mínimo", "Máximo"),
    Valor = c(num(length(x)),
              num(mean(x), 0.01),
              num(sd(x), 0.01),
              num_auto(q[2]),
              paste0(num_auto(q[1]), " – ", num_auto(q[3])),
              num_auto(min(x)),
              num_auto(max(x)))
  )
}

frecuencias <- function(x) {
  tb <- tibble::tibble(nivel = x) |> count(nivel)

  tibble::tibble(
    Categoría = as.character(tb$nivel),
    n = num(tb$n),
    `%` = fmt_pct(tb$n / sum(tb$n))
  )
}

# ---- módulo ----------------------------------------------------------------

mod_univariado_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(7, 5),
    card(card_header("Distribución"), plotly::plotlyOutput(ns("plot"), height = "460px")),
    card(card_header("Resumen"),      DT::DTOutput(ns("tabla")))
  )
}

# Controles en la franja lateral.
mod_univariado_controles <- function(id) {
  ns <- NS(id)

  selectInput(ns("var"), "Variable", choices = NULL)
}

mod_univariado_server <- function(id, datos, modo_oscuro = reactive("light")) {
  moduleServer(id, function(input, output, session) {

    observeEvent(datos(), {
      updateSelectInput(session, "var", choices = opciones(datos()))
    }, once = TRUE)

    output$plot <- plotly::renderPlotly({
      req(input$var)

      # El aviso de ggplot2 por la estética `text` nace al armar el geom, así
      # que el silenciador tiene que envolver también esa construcción.
      callar_texto({
        v <- input$var
        x <- datos()[[v]]

        g <- if (is.numeric(x)) grafico_numerica(x, v) else grafico_factor(x, v)
        interactivo(g, oscuro = identical(modo_oscuro(), "dark"))
      })
    })

    output$tabla <- DT::renderDT(server = FALSE, {
      req(input$var)
      v <- input$var
      x <- datos()[[v]]

      tb <- if (is.numeric(x)) resumen_numerica(x) else frecuencias(x)

      DT::datatable(tb, rownames = FALSE,
                    options = list(dom = "t", ordering = FALSE, pageLength = 30))
    })
  })
}
