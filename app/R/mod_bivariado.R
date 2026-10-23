# mod_bivariado.R — pestaña "Bivariado"
#
# Dos modos:
#   - Los seis análisis del capítulo 4, cada uno con su recorte, su tabla y su
#     gráfico.
#   - Exploración libre: una categórica como agrupador y cualquier variable
#     como desenlace.
#
# En ambos casos el resultado son dos familias de gráfico (cajas y barras al
# 100 %) y dos de tabla (resumen numérico y contingencia), que se definen al
# final del archivo.

# ---- datos -----------------------------------------------------------------

cortes_apgar  <- c(-Inf, 6, 7, 8, 9, 10)
niveles_apgar <- c("Menor que 7", "7", "8", "9", "10")

etiquetas_derivadas <- c(
  apgar1_cat = "Apgar al minuto",
  apgar2_cat = "Apgar a los 5 minutos",
  cambio     = "Puntaje a los cinco minutos frente al del minuto"
)

eje_x <- c(
  NUMCONSUL = "Número de consultas",
  APGAR1    = "Apgar al minuto",
  APGAR2    = "Apgar a los 5 minutos"
)

etiqueta_de <- function(v) {
  if (v %in% names(etiquetas)) etiquetas[[v]] else etiquetas_derivadas[[v]]
}

# Las categorías del Apgar y el cambio entre el minuto y los 5 minutos
preparar_bivariado <- function(d) {
  d |>
    mutate(
      apgar1_cat = cut(APGAR1, cortes_apgar, labels = niveles_apgar),
      apgar2_cat = cut(APGAR2, cortes_apgar, labels = niveles_apgar),
      cambio = factor(
        case_when(
          APGAR2 >  APGAR1 ~ "Mejoró",
          APGAR2 == APGAR1 ~ "Se mantuvo",
          TRUE             ~ "Empeoró"
        ),
        levels = c("Mejoró", "Se mantuvo", "Empeoró")
      )
    )
}

# ---- catálogo de análisis del libro ---------------------------------------

analisis_libro <- list(
  consultas_edad = list(
    etiqueta = "Consultas prenatales según edad",
    grupo    = "EDAD_MADRE", var = "NUMCONSUL",
    tabla    = "resumen", grafico = "cajas",
    titulo   = "Número de consultas prenatales según grupo de edad"
  ),
  cesarea_regimen = list(
    etiqueta = "Cesárea según régimen de salud",
    grupo    = "SEG_SOCIAL", var = "CESAREA",
    filtro   = c("Contributivo", "Subsidiado"),
    tabla    = "contingencia", grafico = "barras100",
    titulo   = "Parto por cesárea según régimen de seguridad social",
    leyenda  = "Cesárea", spanner = "Cesárea"
  ),
  consultas_subregion = list(
    etiqueta = "Consultas prenatales según subregión",
    grupo    = "SUBREGION", var = "NUMCONSUL",
    tabla    = "resumen", grafico = "cajas",
    titulo   = "Número de consultas prenatales según subregión de residencia"
  ),
  apgar_edad = list(
    etiqueta = "Apgar al minuto según edad",
    grupo    = "EDAD_MADRE", var = "APGAR1",
    var_grafico = "apgar1_cat",
    tabla    = "resumen_apgar", grafico = "barras100", ordinal = TRUE,
    titulo   = "Apgar al minuto según grupo de edad",
    leyenda  = "Apgar al minuto", spanner = "Apgar al minuto"
  ),
  apgar_tiempo = list(
    etiqueta = "Apgar a los 5 minutos según el minuto",
    grupo    = "apgar1_cat", var = "apgar2_cat",
    tabla    = "contingencia", grafico = "barras100", ordinal = TRUE,
    titulo   = "Apgar a los cinco minutos según Apgar al minuto",
    leyenda  = "Apgar a los cinco minutos",
    spanner  = "Apgar a los cinco minutos"
  ),
  cambio_apgar = list(
    etiqueta = "Cambio del Apgar (1 → 5 minutos)",
    grupo    = "cambio",
    tabla    = "frecuencia", grafico = NULL,
    titulo   = "Cambio del puntaje Apgar entre el primer y el quinto minuto"
  )
)

# ---- tablas ----------------------------------------------------------------

tabla_resumen <- function(x, g, nombre_grupo, con_bajo = FALSE) {
  tb <- tibble::tibble(g = as.factor(g), x = x) |>
    filter(!is.na(g), !is.na(x)) |>
    group_by(g) |>
    summarise(
      n = n(), media = mean(x), de = sd(x), mediana = median(x),
      q1 = quantile(x, 0.25), q3 = quantile(x, 0.75),
      min = min(x), max = max(x), p_bajo = mean(x < 7),
      .groups = "drop"
    )

  out <- tibble::tibble(
    grupo = as.character(tb$g),
    n = num(tb$n),
    Media = num(tb$media, 0.01),
    DE = num(tb$de, 0.01),
    Mediana = num_auto(tb$mediana),
    `Q1 – Q3` = paste0(num_auto(tb$q1), " – ", num_auto(tb$q3)),
    RIC = num_auto(tb$q3 - tb$q1),
    Mín = num_auto(tb$min),
    Máx = num_auto(tb$max)
  )
  if (con_bajo) out[["Apgar menor que 7"]] <- fmt_pct(tb$p_bajo, 0.01)

  names(out)[1] <- nombre_grupo
  out
}

tabla_contingencia <- function(g, x, nombre_grupo) {
  tb <- tibble::tibble(g = as.factor(g), x = as.factor(x)) |>
    filter(!is.na(g), !is.na(x)) |>
    count(g, x) |>
    tidyr::complete(g, x, fill = list(n = 0)) |>
    group_by(g) |>
    mutate(p = n / sum(n),
           celda = paste0(num(n), " (", fmt_pct(p), ")")) |>
    ungroup()

  anchas <- tb |>
    select(g, x, celda) |>
    tidyr::pivot_wider(names_from = x, values_from = celda)

  total <- tb |>
    group_by(g) |>
    summarise(n = sum(n), .groups = "drop") |>
    mutate(Total = paste0(num(n), " (", fmt_pct(n / sum(n)), ")")) |>
    select(g, Total)

  out <- left_join(anchas, total, by = "g")
  names(out)[1] <- nombre_grupo
  out
}

tabla_frecuencia <- function(g, nombre_grupo) {
  tb <- tibble::tibble(g = as.factor(g)) |>
    filter(!is.na(g)) |>
    count(g) |>
    mutate(p = n / sum(n))

  out <- tibble::tibble(
    grupo = as.character(tb$g),
    n = num(tb$n),
    `%` = fmt_pct(tb$p)
  )
  names(out)[1] <- nombre_grupo
  out
}

# DT no tiene tab_spanner: el encabezado de dos filas se arma a mano.
encabezado_spanner <- function(nombres, spanner) {
  htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, nombres[1]),
        th(colspan = length(nombres) - 2, spanner),
        th(rowspan = 2, nombres[length(nombres)])
      ),
      tr(lapply(nombres[seq(2, length(nombres) - 1)], th))
    )
  ))
}

# ---- gráficos --------------------------------------------------------------

# Etiquetas "grupo  (n = X)" con el primer nivel arriba. Se formatea una vez por
# grupo y se reparte con match(): con 26 mil filas, formatear cada fila cuesta
# casi un segundo.
# Nombre del grupo con su n en una segunda línea más pequeña. En una sola
# línea ("De 20 a 34 años  (n = 19.437)") la etiqueta del eje es tan ancha que
# plotly la recorta por la izquierda.
etiqueta_con_n <- function(g, n) {
  paste0(g, "<br><span style='font-size:11px'>n = ", num(n), "</span>")
}

# Margen izquierdo mínimo según la primera línea de la etiqueta más larga. El
# margen automático de plotly se queda corto por unos píxeles y recorta el
# texto; con este piso, plotly igual puede ampliarlo si hace falta.
margen_etiquetas <- function(etiquetas) {
  primera_linea <- sub("<br>.*", "", as.character(etiquetas))
  list(l = 7 * max(nchar(primera_linea)) + 24)
}

etiquetas_grupo <- function(g, n) {
  g <- as.factor(g)

  cuentas <- tibble::tibble(g = g, n = n) |>
    distinct() |>
    arrange(g)

  etiquetas <- etiqueta_con_n(cuentas$g, cuentas$n)
  factor(etiquetas[match(as.character(g), as.character(cuentas$g))],
         levels = rev(etiquetas))
}

# Cajas con cuartiles precalculados en R: ggplotly enviaría los 26 mil puntos
# al navegador y le haría calcular los cuartiles en cada render.
grafico_cajas <- function(x, g, titulo, var, oscuro = FALSE) {
  tb <- tibble::tibble(x = x, g = as.factor(g)) |>
    filter(!is.na(g), !is.na(x))

  res <- tb |>
    group_by(g) |>
    summarise(
      n = n(), q1 = quantile(x, 0.25), mediana = median(x), q3 = quantile(x, 0.75),
      media = mean(x), minimo = min(x), maximo = max(x),
      li = max(min(x), q1 - 1.5 * IQR(x)),
      ls = min(max(x), q3 + 1.5 * IQR(x)),
      .groups = "drop"
    ) |>
    arrange(g) |>
    mutate(
      pos = row_number(),
      etiqueta = etiqueta_con_n(g, n),
      globo = paste0(
        etiqueta,
        "<br>Media: ", num(media, 0.01),
        "<br>Mediana: ", num_auto(mediana),
        "<br>Q1 – Q3: ", num_auto(q1), " – ", num_auto(q3),
        "<br>RIC: ", num_auto(q3 - q1),
        "<br>Mín: ", num_auto(minimo), " · Máx: ", num_auto(maximo)
      )
    )

  # Los atípicos van como puntos aparte: las estadísticas precalculadas no los
  # derivan. Solo se envían sus coordenadas, sin globo.
  atipicos <- tb |>
    inner_join(res |> select(g, li, ls, pos), by = "g") |>
    filter(x < li | x > ls)

  etiqueta_x <- if (var %in% names(eje_x)) eje_x[[var]] else etiqueta_de(var)

  eje_valores <- list(title = list(text = etiqueta_x), showgrid = TRUE)
  cortes <- cortes_para(x)
  if (is.function(cortes)) {
    marcas <- cortes(range(x))
    eje_valores$tickmode <- "array"
    eje_valores$tickvals <- marcas
    eje_valores$ticktext <- num_auto(marcas)
  }

  plotly::plot_ly() |>
    plotly::add_trace(
      type = "box", orientation = "h", y = res$pos,
      q1 = res$q1, median = res$mediana, q3 = res$q3,
      lowerfence = res$li, upperfence = res$ls,
      fillcolor = pal[["claro"]], line = list(color = pal[["principal"]]),
      hoverinfo = "skip", showlegend = FALSE
    ) |>
    plotly::add_trace(
      x = atipicos$x, y = atipicos$pos, type = "scatter", mode = "markers",
      hoverinfo = "skip", showlegend = FALSE,
      marker = list(color = pal[["gris"]], size = 4, opacity = 0.35)
    ) |>
    plotly::add_trace(
      x = res$media, y = res$pos, type = "scatter", mode = "markers",
      text = res$globo, hoverinfo = "text", showlegend = FALSE,
      marker = list(symbol = "diamond", size = 9, color = pal[["acento"]],
                    line = list(color = "white", width = 1))
    ) |>
    plotly::layout(
      title = list(text = titulo),
      xaxis = eje_valores,
      yaxis = list(title = "", showgrid = FALSE, autorange = "reversed",
                   tickmode = "array", tickvals = res$pos,
                   ticktext = res$etiqueta),
      margin = margen_etiquetas(res$etiqueta)
    ) |>
    estilo_interactivo(oscuro = oscuro, nota = "◆ = media")
}

# Barras al 100 % armadas directamente en plotly: la conversión desde ggplot2
# apila los segmentos en orden inverso y mezcla la leyenda con el título.
grafico_barras100 <- function(g, x, titulo, leyenda, ordinal = FALSE,
                              oscuro = FALSE) {
  tb <- tibble::tibble(g = as.factor(g), x = as.factor(x)) |>
    filter(!is.na(g), !is.na(x)) |>
    count(g, x) |>
    tidyr::complete(g, x, fill = list(n = 0)) |>
    group_by(g) |>
    mutate(p = n / sum(n), n_g = sum(n),
           globo = paste0(g, "<br>", x, ": ", num(n), " (", fmt_pct(p), ")")) |>
    ungroup() |>
    mutate(etiqueta = etiquetas_grupo(g, n_g))

  niveles <- levels(tb$x)
  colores <- setNames(colores_grupo(length(niveles), ordinal = ordinal), niveles)

  p <- plotly::plot_ly()
  for (nivel in niveles) {
    d <- tb[tb$x == nivel, , drop = FALSE]
    p <- p |>
      plotly::add_trace(
        x = d$p, y = d$etiqueta, type = "bar", orientation = "h",
        name = nivel, text = d$globo, hoverinfo = "text",
        textposition = "none",
        marker = list(color = colores[[nivel]],
                      line = list(color = "white", width = 0.5))
      )
  }

  p |>
    plotly::layout(
      barmode = "stack",
      title = list(text = titulo),
      xaxis = list(range = c(0, 1), tickvals = seq(0, 1, 0.25),
                   ticktext = fmt_pct(seq(0, 1, 0.25), 1),
                   showgrid = FALSE, title = ""),
      yaxis = list(showgrid = FALSE, title = ""),
      legend = list(title = list(text = leyenda), traceorder = "normal"),
      margin = margen_etiquetas(levels(tb$etiqueta))
    ) |>
    estilo_interactivo(oscuro = oscuro, con_leyenda = TRUE)
}

# ---- módulo ----------------------------------------------------------------

mod_bivariado_ui <- function(id) {
  ns <- NS(id)

  # fill = FALSE: cada tarjeta toma su alto natural. Si no, el gráfico y la
  # tabla se reparten el alto de la pestaña y el gráfico queda aplastado.
  layout_columns(
    col_widths = 12, fill = FALSE,
    conditionalPanel(
      condition = sprintf("input['%s'] != 'cambio_apgar'", ns("analisis")),
      card(card_header("Gráfico"), plotly::plotlyOutput(ns("plot"), height = "400px"))
    ),
    card(card_header("Tabla"), DT::DTOutput(ns("tabla")))
  )
}

# Controles en la franja lateral.
mod_bivariado_controles <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      ns("analisis"), "Análisis",
      choices = c(
        setNames(names(analisis_libro),
                 vapply(analisis_libro, `[[`, "", "etiqueta")),
        "Exploración libre" = "libre"
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'libre'", ns("analisis")),
      selectInput(ns("grupo"), "Agrupar por", choices = NULL),
      selectInput(ns("var"), "Variable", choices = NULL)
    )
  )
}

mod_bivariado_server <- function(id, datos, modo_oscuro = reactive("light")) {
  moduleServer(id, function(input, output, session) {

    d <- reactive(preparar_bivariado(datos()))

    observeEvent(datos(), {
      categoricas <- vars_categoricas(datos())
      updateSelectInput(session, "grupo",
                        choices = setNames(categoricas, etiquetas[categoricas]))
      updateSelectInput(session, "var", choices = opciones(datos()))
    }, once = TRUE)

    # Un análisis del libro o uno armado con los dos selectores libres.
    analisis <- reactive({
      req(input$analisis)
      if (!identical(input$analisis, "libre")) return(analisis_libro[[input$analisis]])

      req(input$grupo, input$var)
      numerica <- is.numeric(d()[[input$var]])

      list(
        grupo    = input$grupo,
        var      = input$var,
        tabla    = if (numerica) "resumen" else "contingencia",
        grafico  = if (numerica) "cajas" else "barras100",
        titulo   = paste0(etiqueta_de(input$var), " según ",
                          tolower(etiqueta_de(input$grupo))),
        leyenda  = etiqueta_de(input$var),
        spanner  = etiqueta_de(input$var)
      )
    })

    datos_analisis <- reactive({
      a  <- analisis()
      dd <- d()

      if (!is.null(a$filtro)) {
        dd <- dd |>
          filter(.data[[a$grupo]] %in% a$filtro) |>
          droplevels()
      }

      dd
    })

    # En el modo libre se puede elegir la misma variable en los dos selectores.
    mismo_campo <- function(a) identical(a$grupo, a$var)

    output$plot <- plotly::renderPlotly({
      a  <- analisis()
      req(a$grafico)
      validate(need(!mismo_campo(a), "Elige dos variables distintas."))

      dd <- datos_analisis()
      oscuro <- identical(modo_oscuro(), "dark")

      if (identical(a$grafico, "cajas")) {
        grafico_cajas(dd[[a$var]], dd[[a$grupo]], a$titulo, a$var, oscuro)
      } else {
        v <- if (!is.null(a$var_grafico)) a$var_grafico else a$var
        grafico_barras100(dd[[a$grupo]], dd[[v]], a$titulo, a$leyenda,
                          isTRUE(a$ordinal), oscuro)
      }
    })

    output$tabla <- DT::renderDT(server = FALSE, {
      a  <- analisis()
      dd <- datos_analisis()
      validate(need(!mismo_campo(a), "Elige dos variables distintas."))

      tb <- switch(
        a$tabla,
        resumen      = tabla_resumen(dd[[a$var]], dd[[a$grupo]],
                                     etiqueta_de(a$grupo)),
        resumen_apgar = tabla_resumen(dd[[a$var]], dd[[a$grupo]],
                                      etiqueta_de(a$grupo), con_bajo = TRUE),
        contingencia = tabla_contingencia(dd[[a$grupo]], dd[[a$var]],
                                          etiqueta_de(a$grupo)),
        frecuencia   = tabla_frecuencia(dd[[a$grupo]], etiqueta_de(a$grupo))
      )

      encabezado <- if (identical(a$tabla, "contingencia")) {
        encabezado_spanner(names(tb), a$spanner)
      }

      args <- list(tb, rownames = FALSE,
                   options = list(dom = "t", ordering = FALSE, pageLength = 30))
      if (!is.null(encabezado)) args$container <- encabezado

      do.call(DT::datatable, args)
    })
  })
}
