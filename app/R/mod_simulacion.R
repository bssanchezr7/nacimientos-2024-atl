# Pestaña "Simulación": simulación de Monte Carlo del tamaño de muestra.
# ¿Qué habría concluido cada prueba con muchos menos nacimientos? Se toman
# muchas muestras aleatorias de la base, se repite la prueba en cada una y se
# cuenta en cuántas se rechaza H₀. El motor está en R/utils_simulacion.R.

# Tamaños de muestra de la curva de potencia
tamanos_curva <- c(50, 100, 200, 300, 500, 1000, 2000, 5000)

mod_simulacion_ui <- function(id) {
  ns <- NS(id)

  caja <- function(titulo, salida, detalle, icono) {
    value_box(titulo, textOutput(ns(salida)), textOutput(ns(detalle), inline = TRUE),
              showcase = icon(icono), class = "vb-suave")
  }

  tagList(
    p(class = "lead mt-2",
      "¿Qué se habría concluido con muchos menos nacimientos? La simulación toma muchas muestras al azar de la base, repite la prueba en cada una y cuenta en cuántas se rechaza H₀."),

    layout_column_wrap(
      width = "220px", fill = FALSE,
      caja("Muestras que rechazan H₀", "vb_rechazo", "vb_rechazo_det", "circle-check"),
      caja("Valor p mediano", "vb_p", "vb_p_det", "percent"),
      caja("Tamaño del efecto mediano", "vb_efecto", "vb_efecto_det", "ruler")
    ),

    card(
      fill = FALSE,
      card_header(icon("comment"), "Lectura"),
      card_body(uiOutput(ns("lectura")))
    ),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)), fill = FALSE,
      card(
        fill = FALSE,
        card_header(con_ayuda(
          "Valores p de las muestras",
          "Cada barra cuenta cuántas de las muestras simuladas dieron un valor p en ese rango. Las que quedan a la izquierda de la línea rechazan H₀."
        )),
        plotly::plotlyOutput(ns("hist_p"), height = "320px")
      ),
      card(
        fill = FALSE,
        card_header(con_ayuda(
          "Tamaño del efecto de las muestras",
          "Cómo varía el tamaño del efecto entre muestras. La línea marca el valor con la base completa."
        )),
        plotly::plotlyOutput(ns("hist_efecto"), height = "320px")
      )
    ),

    card(
      fill = FALSE,
      card_header(con_ayuda(
        "¿Desde qué tamaño de muestra se detecta la diferencia?",
        "Porcentaje de muestras que rechazan H₀ para varios tamaños de muestra (200 simulaciones por tamaño). Por convención se busca al menos un 80%."
      )),
      plotly::plotlyOutput(ns("curva"), height = "340px")
    )
  )
}

# Controles en la franja lateral
mod_simulacion_controles <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("objetivo"), "Objetivo",
                choices = setNames(names(objetivos_simulacion),
                                   vapply(objetivos_simulacion, `[[`, "", "etiqueta")),
                selected = "consultas_subregion"),
    sliderInput(ns("n"), "Nacimientos por muestra", min = 30, max = 5000,
                value = 300, step = 10, sep = ".", ticks = FALSE),
    selectInput(ns("repeticiones"), "Número de simulaciones",
                choices = c(200, 500, 1000), selected = 500),
    selectInput(ns("alfa"), "Nivel de significancia (α)",
                choices = c("0,01" = 0.01, "0,05" = 0.05, "0,10" = 0.10), selected = 0.05),
    numericInput(ns("semilla"), "Semilla", value = 2024, min = 1, step = 1),
    actionButton(ns("simular"), "Simular", icon = icon("shuffle"),
                 class = "btn-primary w-100")
  )
}

mod_simulacion_server <- function(id, datos, modo_oscuro = reactive("light")) {
  moduleServer(id, function(input, output, session) {

    objetivo  <- reactive(objetivos_simulacion[[input$objetivo]])
    poblacion <- reactive(objetivo()$poblacion(datos))
    completo  <- reactive(objetivo()$probar(poblacion()))
    oscuro    <- reactive(identical(modo_oscuro(), "dark"))

    # La simulación corre al pulsar "Simular" (y una vez al entrar). Guarda
    # los parámetros con que se hizo para que la lectura no se desfase si
    # luego se mueven los controles sin volver a simular.
    simulacion <- reactive({
      params <- list(objetivo = input$objetivo, n = input$n,
                     repeticiones = as.integer(input$repeticiones),
                     alfa = as.numeric(input$alfa), semilla = input$semilla %||% 2024)
      obj <- objetivos_simulacion[[params$objetivo]]
      pob <- obj$poblacion(datos)
      res <- simular(obj, pob, params$n, params$repeticiones, params$semilla)
      c(params, list(res = res, n_poblacion = nrow(pob), completo = obj$probar(pob)))
    }) |>
      bindEvent(input$simular, input$objetivo, ignoreNULL = FALSE)

    # Curva de potencia: depende solo del objetivo (α se aplica después)
    curva_p <- reactive({
      obj <- objetivo()
      pob <- poblacion()
      lapply(tamanos_curva, function(n) simular(obj, pob, n, 200, semilla = 2024)$p)
    }) |>
      bindCache(input$objetivo)

    rechazo <- function(p, alfa) mean(p < alfa, na.rm = TRUE)

    # ---- cajas de valor ------------------------------------------------------

    output$vb_rechazo <- renderText({
      s <- simulacion(); fmt_pct(rechazo(s$res$p, s$alfa))
    })
    output$vb_rechazo_det <- renderText({
      s <- simulacion()
      paste0("de ", num(s$repeticiones), " muestras de ", num(s$n), " nacimientos")
    })
    output$vb_p <- renderText(formato_p(median(simulacion()$res$p, na.rm = TRUE)))
    output$vb_p_det <- renderText(paste0("Base completa: ", formato_p(simulacion()$completo[["p"]])))
    output$vb_efecto <- renderText(num(median(simulacion()$res$efecto, na.rm = TRUE), 0.001))
    output$vb_efecto_det <- renderText({
      s <- simulacion()
      paste0(objetivos_simulacion[[s$objetivo]]$medida, " · base completa: ",
             num(s$completo[["efecto"]], 0.001))
    })

    # ---- lectura generada a partir de la simulación --------------------------

    output$lectura <- renderUI({
      s   <- simulacion()
      obj <- objetivos_simulacion[[s$objetivo]]
      r   <- rechazo(s$res$p, s$alfa)
      ef  <- median(s$res$efecto, na.rm = TRUE)
      ef_completo <- s$completo[["efecto"]]

      deteccion <- if (r >= 0.8) {
        "Con este tamaño de muestra la diferencia se detecta casi siempre."
      } else if (r >= 0.2) {
        paste("Con este tamaño de muestra la diferencia se detecta solo en una parte de",
              "las muestras: según cuál se hubiera tomado, la conclusión sería una u otra.")
      } else {
        paste("Con este tamaño de muestra la diferencia casi nunca se detecta: un resultado",
              "no significativo no querría decir que no exista, sino que la muestra es",
              "pequeña para verla.")
      }

      efecto <- if (abs(ef - ef_completo) <= 0.25 * abs(ef_completo)) {
        paste("Lo que cambia con el tamaño de la muestra es la capacidad de detectar la",
              "diferencia, no su magnitud.")
      } else {
        "Con muestras tan pequeñas, la estimación del tamaño del efecto también es imprecisa."
      }

      tagList(
        p(paste0(
          "Con muestras de ", num(s$n), " nacimientos, la prueba de ", obj$prueba,
          " rechazó H₀ en el ", fmt_pct(r), " de las ", num(s$repeticiones),
          " simulaciones (α = ", num(s$alfa, 0.01), "). Con la base completa (",
          num(s$n_poblacion), " nacimientos) se rechaza con ", formato_p(s$completo[["p"]]), "."
        )),
        p(deteccion),
        p(class = "mb-0", paste0(
          "El tamaño del efecto (", obj$medida, ") mediano fue ", num(ef, 0.001),
          ", frente a ", num(ef_completo, 0.001), " con la base completa (",
          obj$magnitud(ef_completo), "). ", efecto
        ))
      )
    })

    # ---- gráficos ------------------------------------------------------------

    linea_vertical <- function(x, color) {
      list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = color, width = 2, dash = "dash"))
    }

    output$hist_p <- plotly::renderPlotly({
      s <- simulacion()
      plotly::plot_ly(
        x = s$res$p, type = "histogram",
        xbins = list(start = 0, end = 1, size = 0.05),
        marker = list(color = pal[["principal"]], line = list(color = "white", width = 1)),
        hovertemplate = "p entre %{x}<br>%{y} muestras<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = list(text = "Valor p"), range = c(0, 1)),
          yaxis = list(title = list(text = "Muestras")),
          shapes = list(linea_vertical(s$alfa, pal[["acento"]])),
          bargap = 0.05
        ) |>
        estilo_interactivo(oscuro = oscuro(), nota = paste0("Línea: α = ", num(s$alfa, 0.01)))
    })

    output$hist_efecto <- plotly::renderPlotly({
      s <- simulacion()
      plotly::plot_ly(
        x = s$res$efecto, type = "histogram", nbinsx = 30,
        marker = list(color = pal[["claro"]], line = list(color = "white", width = 1)),
        hovertemplate = "%{x}<br>%{y} muestras<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = list(text = objetivos_simulacion[[s$objetivo]]$medida)),
          yaxis = list(title = list(text = "Muestras")),
          shapes = list(linea_vertical(s$completo[["efecto"]], pal[["acento"]])),
          bargap = 0.05
        ) |>
        estilo_interactivo(oscuro = oscuro(), nota = "Línea: valor con la base completa")
    })

    output$curva <- plotly::renderPlotly({
      alfa <- as.numeric(input$alfa)
      potencia <- vapply(curva_p(), rechazo, 0, alfa = alfa)
      n_actual <- simulacion()$n

      plotly::plot_ly() |>
        plotly::add_trace(
          x = tamanos_curva, y = potencia, type = "scatter", mode = "lines+markers",
          line = list(color = pal[["principal"]], width = 3),
          marker = list(color = pal[["principal"]], size = 8),
          text = paste0(num(tamanos_curva), " nacimientos: rechaza H₀ en el ", fmt_pct(potencia)),
          hoverinfo = "text", showlegend = FALSE
        ) |>
        plotly::layout(
          xaxis = list(title = list(text = "Nacimientos por muestra (escala logarítmica)"),
                       type = "log", tickvals = tamanos_curva, ticktext = num(tamanos_curva)),
          yaxis = list(title = list(text = "Muestras que rechazan H₀"), range = c(0, 1.05),
                       tickvals = seq(0, 1, 0.2), ticktext = fmt_pct(seq(0, 1, 0.2), 1)),
          shapes = list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = 0.8, y1 = 0.8,
                 line = list(color = pal[["gris"]], width = 1, dash = "dot")),
            linea_vertical(n_actual, pal[["acento"]])
          )
        ) |>
        estilo_interactivo(oscuro = oscuro(),
                           nota = "Punteada: 80% · Línea naranja: tamaño de la última simulación")
    })
  })
}
