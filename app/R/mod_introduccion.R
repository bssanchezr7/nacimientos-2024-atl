# Indicadores que se pueden ver en el mapa. Los porcentajes van de 0 a 100.
indicadores_mapa <- tibble::tribble(
  ~id,            ~etiqueta,                           ~es_pct, ~acc, ~descripcion,
  "n",            "Nacimientos",                       FALSE,   1,    "Número de nacimientos de madres residentes en el municipio.",
  "cesarea",      "% de cesáreas",                     TRUE,    0.1,  "Porcentaje de nacimientos por cesárea.",
  "consultas",    "Promedio de consultas prenatales",  FALSE,   0.1,  "Número promedio de consultas prenatales por nacimiento.",
  "consultas8",   "% con 8 o más consultas",           TRUE,    0.1,  "Porcentaje de nacimientos cuya madre tuvo al menos ocho consultas prenatales.",
  "adolescentes", "% de madres adolescentes",          TRUE,    0.1,  "Porcentaje de nacimientos de madres de 10 a 19 años.",
  "apgar_bajo",   "% con Apgar al minuto menor que 7", TRUE,    0.1,  "Porcentaje de recién nacidos con un puntaje Apgar menor que 7 al minuto de vida."
)

colores_mapa <- c("#DDE7EB", "#6F97A6", "#1C3A45")

# Fondos de CARTO.
fondos_mapa <- list(
  claro     = c("light_nolabels",   "light_only_labels"),
  detallado = c("voyager_nolabels", "voyager_only_labels"),
  oscuro    = c("dark_nolabels",    "dark_only_labels")
)

url_carto <- function(estilo) {
  paste0("https://basemaps.cartocdn.com/rastertiles/", estilo, "/{z}/{x}/{y}{r}.png?key=", clave_carto)
}

atribucion_carto <- paste(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  '&copy; <a href="https://carto.com/attributions">CARTO</a>'
)

# Agrega los fondos como grupos y deja visible solo `inicial`.
agregar_fondos <- function(mapa, inicial) {
  if (!nzchar(clave_carto)) return(addProviderTiles(mapa, providers$Esri.WorldGrayCanvas))

  for (fondo in names(fondos_mapa)) {
    mapa <- mapa |>
      addTiles(url_carto(fondos_mapa[[fondo]][1]), group = fondo, attribution = atribucion_carto) |>
      addTiles(url_carto(fondos_mapa[[fondo]][2]), group = fondo,
               options = tileOptions(pane = "etiquetas"))
  }
  hideGroup(mapa, setdiff(names(fondos_mapa), inicial))
}

# Hallazgos principales por objetivo
hallazgos <- list(
  list("Consultas según la edad", "user-clock",
       "Las madres adolescentes tuvieron menos consultas prenatales que las de 20 a 34 años y las de 35 o más."),
  list("Cesáreas según el régimen", "hospital",
       "La cesárea fue casi igual de frecuente en el régimen contributivo y en el subsidiado: cerca de tres de cada cuatro nacimientos."),
  list("Consultas según la subregión", "map-location-dot",
       "El promedio de consultas baja de Barranquilla a los municipios fuera del área metropolitana, aunque en poca magnitud."),
  list("Apgar según la edad", "heart-pulse",
       "La vitalidad del recién nacido en su primer minuto casi no cambia según la edad de la madre."),
  list("Apgar al minuto y a los cinco", "arrow-trend-up",
       "La mayoría de los recién nacidos sube su puntaje Apgar entre el primer y el quinto minuto, y casi ninguno lo baja.")
)

mod_introduccion_ui <- function(id) {
  ns <- NS(id)

  caja <- function(titulo, ayuda, salida, icono) {
    value_box(con_ayuda(titulo, ayuda), textOutput(ns(salida)),
              showcase = icon(icono), class = "vb-suave")
  }

  ayuda_apgar <- "El puntaje Apgar califica de 0 a 10 el estado del recién nacido al minuto y a los cinco minutos de vida; 7 o más se considera normal."

  tarjetas_hallazgos <- lapply(seq_along(hallazgos), function(i) {
    h <- hallazgos[[i]]
    titulo <- paste0(i, ". ", h[[1]])
    card(
      card_header(icon(h[[2]]), if (grepl("Apgar", titulo)) con_ayuda(titulo, ayuda_apgar) else titulo),
      card_body(p(h[[3]]))
    )
  })

  tagList(
    p(class = "lead mt-2",
      "Atención prenatal, tipo de parto y vitalidad del recién nacido en los nacimientos de madres residentes en el Atlántico durante 2024, a partir de las Estadísticas Vitales del DANE."),

    layout_column_wrap(
      width = "200px", fill = FALSE,
      caja("Nacimientos analizados",
           "Nacimientos de 2024 cuya madre residía en el Atlántico, sin los registros con datos faltantes.",
           "vb_n", "baby"),
      caja("Por cesárea",
           "La OMS señala que, por encima del 10 %, más cesáreas ya no reducen la mortalidad de madres y recién nacidos.",
           "vb_cesarea", "hospital"),
      caja("Consultas prenatales en promedio",
           "La OMS recomienda al menos ocho contactos con el personal de salud durante el embarazo.",
           "vb_consultas", "stethoscope"),
      caja("Madres adolescentes",
           "Madres de 10 a 19 años.",
           "vb_adolescentes", "person")
    ),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), xl = c(8, 4)), fill = FALSE,
      card(
        full_screen = TRUE,
        card_header(con_ayuda(
          "Indicadores por municipio de residencia de la madre",
          "Cada nacimiento se ubica en el municipio donde residía la madre, no donde ocurrió el parto. Las líneas blancas gruesas separan las cuatro subregiones."
        )),
        card_body(
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12, 12), md = c(5, 4, 3)), fill = FALSE,
            selectInput(ns("indicador"), "Indicador", width = "100%",
                        choices = setNames(indicadores_mapa$id, indicadores_mapa$etiqueta)),
            # Las opciones se llenan en el servidor a partir de los municipios
            selectInput(ns("municipio"), "Ir a un municipio", width = "100%",
                        choices = c("Todo el Atlántico" = "")),
            if (nzchar(clave_carto)) {
              selectInput(ns("fondo"), "Fondo", width = "100%",
                          choices = c("Automático" = "auto", "Claro" = "claro",
                                      "Detallado" = "detallado", "Oscuro" = "oscuro"))
            }
          ),
          leafletOutput(ns("mapa"), height = 480)
        )
      ),
      div(
        card(
          card_header("Sobre el indicador"),
          card_body(uiOutput(ns("detalle")))
        ),
        card(
          card_header(icon("location-dot"), "Ficha del municipio"),
          card_body(uiOutput(ns("ficha")))
        )
      )
    ),

    h5(class = "mt-3", "Qué encontramos"),
    p(class = "text-muted",
      "Las cinco comparaciones mostraron diferencias estadísticamente significativas, aunque en su mayoría pequeñas."),
    do.call(layout_column_wrap, c(list(width = "220px", fill = FALSE), tarjetas_hallazgos))
  )
}

mod_introduccion_server <- function(id, datos, municipios, modo_oscuro = reactive("light")) {
  moduleServer(id, function(input, output, session) {

    resumir <- function(df) {
      df |>
        summarise(
          n            = n(),
          cesarea      = 100 * mean(CESAREA == "Sí"),
          consultas    = mean(NUMCONSUL),
          consultas8   = 100 * mean(NUMCONSUL >= 8),
          adolescentes = 100 * mean(EDAD_MADRE == "Adolescente"),
          apgar_bajo   = 100 * mean(APGAR1 < 7),
          .groups = "drop"
        )
    }

    departamento  <- resumir(datos)
    por_municipio <- municipios |>
      left_join(datos |> group_by(CODMUNRE, SUBREGION) |> resumir(), by = "CODMUNRE")

    output$vb_n            <- renderText(num(departamento$n))
    output$vb_cesarea      <- renderText(fmt_pct(departamento$cesarea / 100))
    output$vb_consultas    <- renderText(num(departamento$consultas, 0.1))
    output$vb_adolescentes <- renderText(fmt_pct(departamento$adolescentes / 100))

    # Contorno de cada subregión, para trazar sus límites sobre el mapa
    subregiones <- por_municipio |>
      group_by(SUBREGION) |>
      summarise(.groups = "drop")

    caja_atl <- st_bbox(municipios)

    updateSelectInput(session, "municipio", choices = c(
      "Todo el Atlántico" = "",
      with(arrange(st_drop_geometry(municipios), MUNICIPIO), setNames(CODMUNRE, MUNICIPIO))
    ))

    indicador <- reactive(indicadores_mapa[indicadores_mapa$id == input$indicador, ])

    # En "Automático" el fondo sigue al interruptor de modo oscuro de la app
    fondo <- reactive({
      elegido <- input$fondo %||% "auto"
      if (elegido != "auto") return(elegido)
      if (identical(modo_oscuro(), "dark")) "oscuro" else "claro"
    })

    formato <- function(x, ind) if (ind$es_pct) fmt_pct(x / 100, ind$acc) else num(x, ind$acc)

    # Acerca el mapa a una geometría (un municipio o todo el departamento)
    volar_a <- function(mapa, caja) {
      flyToBounds(mapa, caja[["xmin"]], caja[["ymin"]], caja[["xmax"]], caja[["ymax"]],
                  options = list(maxZoom = 11, padding = c(20, 20)))
    }

    dibujar_municipios <- function(mapa, ind) {
      valores <- por_municipio[[ind$id]]

      paleta <- if (ind$id == "n") {
        colorBin(colores_mapa, valores, bins = c(0, 250, 500, 1000, 5000, 15000))
      } else {
        colorNumeric(colores_mapa, valores)
      }

      etiquetas <- sprintf(
        "<strong>%s</strong><br><span style='color:#7A7A7A'>%s</span><br>%s: <strong>%s</strong>%s",
        por_municipio$MUNICIPIO, por_municipio$SUBREGION, ind$etiqueta, formato(valores, ind),
        if (ind$id == "n") "" else paste0("<br>Nacimientos: ", num(por_municipio$n))
      ) |> lapply(htmltools::HTML)

      leyenda <- function(type, cuts, p) {
        if (type == "bin") return(paste(num(head(cuts, -1)), "–", num(cuts[-1])))
        if (ind$es_pct) fmt_pct(cuts / 100, 1) else num(cuts, ind$acc)
      }

      mapa |>
        clearGroup("municipios") |>
        addPolygons(
          data = por_municipio, layerId = ~CODMUNRE, group = "municipios",
          fillColor = paleta(valores), fillOpacity = 0.8,
          color = "white", weight = 1,
          label = etiquetas,
          highlightOptions = highlightOptions(weight = 3, color = "#C8764A", bringToFront = TRUE),
          options = pathOptions(pane = "municipios")
        ) |>
        addLegend("bottomright", pal = paleta, values = valores, layerId = "leyenda",
                  title = ind$etiqueta, labFormat = leyenda, opacity = 0.85)
    }

    output$mapa <- renderLeaflet({

      leaflet(options = leafletOptions(
        minZoom = 8,
        maxBounds = list(c(caja_atl[["ymin"]] - 0.5, caja_atl[["xmin"]] - 0.6),
                         c(caja_atl[["ymax"]] + 0.5, caja_atl[["xmax"]] + 0.6))
      )) |>
        addMapPane("municipios",  zIndex = 410) |>
        addMapPane("subregiones", zIndex = 420) |>
        addMapPane("seleccion",   zIndex = 430) |>
        addMapPane("etiquetas",   zIndex = 450) |>
        agregar_fondos(isolate(fondo())) |>
        dibujar_municipios(isolate(indicador())) |>
        addPolygons(
          data = subregiones, group = "subregiones",
          fill = FALSE, color = "white", weight = 3.5, opacity = 1,
          options = pathOptions(pane = "subregiones", interactive = FALSE)
        ) |>
        addMiniMap(
          tiles = if (nzchar(clave_carto)) url_carto("light_all") else providers$Esri.WorldGrayCanvas,
          position = "topright", width = 110, height = 110,
          zoomLevelOffset = -5, toggleDisplay = TRUE
        ) |>
        addScaleBar("bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
        addEasyButton(easyButton(
          icon = "fa-home", title = "Volver a todo el Atlántico",
          onClick = JS(sprintf("function(btn, map) { Shiny.setInputValue('%s', Date.now()); }",
                               session$ns("volver")))
        )) |>
        fitBounds(caja_atl[["xmin"]], caja_atl[["ymin"]], caja_atl[["xmax"]], caja_atl[["ymax"]])
    })

    observeEvent(indicador(), ignoreInit = TRUE, {
      dibujar_municipios(leafletProxy("mapa"), indicador())
    })

    observeEvent(fondo(), ignoreInit = TRUE, {
      leafletProxy("mapa") |>
        hideGroup(names(fondos_mapa)) |>
        showGroup(fondo())
    })

    # Un clic en el mapa elige el municipio en la lista; la lista es la que
    # manda el resaltado y el acercamiento
    observeEvent(input$mapa_shape_click, {
      updateSelectInput(session, "municipio", selected = input$mapa_shape_click$id)
    })

    observeEvent(input$volver, {
      updateSelectInput(session, "municipio", selected = "")
      volar_a(leafletProxy("mapa"), caja_atl)
    })

    observeEvent(input$municipio, ignoreInit = TRUE, {
      mapa <- leafletProxy("mapa") |> clearGroup("seleccion")
      if (input$municipio == "") return(volar_a(mapa, caja_atl))

      elegido <- por_municipio[por_municipio$CODMUNRE == input$municipio, ]
      mapa |>
        addPolygons(
          data = elegido, group = "seleccion",
          fill = FALSE, color = "#C8764A", weight = 4, opacity = 1,
          options = pathOptions(pane = "seleccion", interactive = FALSE)
        ) |>
        volar_a(st_bbox(elegido))
    })

    output$ficha <- renderUI({
      if (is.null(input$municipio) || input$municipio == "") {
        return(p(class = "text-muted mb-0",
                 "Haga clic en un municipio del mapa o elíjalo en la lista para ver todos sus indicadores."))
      }

      m <- st_drop_geometry(por_municipio[por_municipio$CODMUNRE == input$municipio, ])
      filas <- lapply(which(indicadores_mapa$id != "n"), function(i) {
        ind <- indicadores_mapa[i, ]
        tags$tr(
          tags$td(ind$etiqueta),
          tags$td(class = "text-end fw-bold", formato(m[[ind$id]], ind)),
          tags$td(class = "text-end text-muted", formato(departamento[[ind$id]], ind))
        )
      })

      tagList(
        h5(class = "mb-0", m$MUNICIPIO),
        p(class = "text-muted", as.character(m$SUBREGION), " · ", num(m$n), " nacimientos"),
        tags$table(
          class = "table table-sm mb-2",
          tags$thead(tags$tr(tags$th(""), tags$th(class = "text-end", "Municipio"),
                             tags$th(class = "text-end", "Atlántico"))),
          tags$tbody(filas)
        ),
        if (m$n < 100) {
          p(class = "text-muted small mb-0", icon("triangle-exclamation"),
            "Con menos de 100 nacimientos, los porcentajes pueden cambiar mucho con pocos casos.")
        }
      )
    })

    output$detalle <- renderUI({
      ind     <- indicador()
      valores <- por_municipio[[ind$id]]
      i_max   <- which.max(valores)
      i_min   <- which.min(valores)

      tagList(
        p(ind$descripcion),
        p(class = "text-muted mb-0", "Atlántico"),
        p(class = "fs-3 fw-bold", formato(departamento[[ind$id]], ind)),
        p("Más alto: ", strong(por_municipio$MUNICIPIO[i_max]), paste0("(", formato(valores[i_max], ind), ")")),
        p("Más bajo: ", strong(por_municipio$MUNICIPIO[i_min]), paste0("(", formato(valores[i_min], ind), ")")),
        if (ind$id != "n") {
          p(class = "text-muted small",
            "En los municipios con pocos nacimientos un porcentaje o un promedio puede cambiar mucho con pocos casos. Al pasar el cursor por el mapa se ve cuántos nacimientos tiene cada uno.")
        }
      )
    })
  })
}
