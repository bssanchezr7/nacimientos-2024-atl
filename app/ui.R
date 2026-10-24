library(shiny)
library(bslib)

ui <- page_sidebar(
  title = div(
    class = "d-flex align-items-center w-100",
    span("Nacimientos en el Atlántico · 2024"),
    input_dark_mode(id = "modo_oscuro", class = "ms-auto")
  ),
  window_title = "Nacimientos en el Atlántico · 2024",
  theme = tema_app(),
  lang = "es",

  sidebar = sidebar(
    width = 260,
    h6("Filtros"),
    conditionalPanel(
      condition = "input.pestana === 'univariado'",
      mod_univariado_controles("univariado")
    ),
    conditionalPanel(
      condition = "input.pestana === 'bivariado'",
      mod_bivariado_controles("bivariado")
    ),
    conditionalPanel(
      condition = "input.pestana === 'hipotesis'",
      mod_hipotesis_controles("hipotesis")
    ),
    if (mostrar_simulacion) {
      conditionalPanel(
        condition = "input.pestana === 'simulacion'",
        mod_simulacion_controles("simulacion")
      )
    },
    conditionalPanel(
      condition = paste0("input.pestana !== 'univariado' && ",
                         "input.pestana !== 'bivariado' && ",
                         "input.pestana !== 'hipotesis' && ",
                         "input.pestana !== 'simulacion'"),
      p(class = "text-muted", "Esta pestaña no tiene filtros.")
    )
  ),

  useBusyIndicators(),

  navset_card_tab(
    id = "pestana",
    nav_panel("Introducción", value = "introduccion",
              mod_introduccion_ui("introduccion")),
    nav_panel("Objetivos", value = "objetivos",
              mod_objetivos_ui("objetivos")),
    nav_panel("Datos y metodología", value = "metodologia",
              mod_metodologia_ui("metodologia")),
    nav_panel("Univariado", value = "univariado",
              mod_univariado_ui("univariado")),
    nav_panel("Bivariado", value = "bivariado",
              mod_bivariado_ui("bivariado")),
    nav_panel("Hipótesis", value = "hipotesis",
              mod_hipotesis_ui("hipotesis")),
    if (mostrar_simulacion) {
      nav_panel("Simulación", value = "simulacion",
                mod_simulacion_ui("simulacion"))
    }
  )
)
