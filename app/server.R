server <- function(input, output, session) {
  mod_introduccion_server("introduccion", datos, municipios,
                          modo_oscuro = reactive(input$modo_oscuro))
  mod_metodologia_server("metodologia", datos)
  mod_univariado_server("univariado", reactive(datos),
                        modo_oscuro = reactive(input$modo_oscuro))
  mod_bivariado_server("bivariado", reactive(datos),
                       modo_oscuro = reactive(input$modo_oscuro))
  mod_hipotesis_server("hipotesis")
}
