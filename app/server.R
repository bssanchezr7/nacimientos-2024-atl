server <- function(input, output, session) {
  mod_metodologia_server("metodologia")
  mod_univariado_server("univariado")
  mod_bivariado_server("bivariado")
  mod_hipotesis_server("hipotesis")
}
