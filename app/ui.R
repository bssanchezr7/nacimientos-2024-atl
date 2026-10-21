library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Nacimientos en el Atlántico · 2024",

  sidebar = sidebar(
    width = 300,
    h6("Filtros"),
    p(class = "text-muted", "Pendiente de contenido.")
  ),

  navset_card_tab(
    id = "pestana",
    nav_panel("Introducción", value = "introduccion",
              mod_introduccion_ui("introduccion")),
    nav_panel("Datos y metodología", value = "metodologia",
              mod_metodologia_ui("metodologia")),
    nav_panel("Univariado", value = "univariado",
              mod_univariado_ui("univariado")),
    nav_panel("Bivariado", value = "bivariado",
              mod_bivariado_ui("bivariado")),
    nav_panel("Hipótesis", value = "hipotesis",
              mod_hipotesis_ui("hipotesis"))
  )
)
