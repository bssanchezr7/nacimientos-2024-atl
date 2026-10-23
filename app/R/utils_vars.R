# Variables de la app.

etiquetas <- c(
  NUMCONSUL  = "Consultas prenatales",
  APGAR1     = "Apgar al minuto",
  APGAR2     = "Apgar a los 5 minutos",
  EDAD_MADRE = "Edad de la madre",
  TIPO_PARTO = "Tipo de parto",
  SEG_SOCIAL = "Régimen de seguridad social",
  SUBREGION  = "Subregión",
  CESAREA    = "Cesárea"
)

vars_disponibles <- function(d) intersect(names(etiquetas), names(d))

vars_numericas <- function(d) {
  Filter(function(v) is.numeric(d[[v]]), vars_disponibles(d))
}

vars_categoricas <- function(d) {
  Filter(function(v) is.factor(d[[v]]) || is.character(d[[v]]), vars_disponibles(d))
}

# selectInput espera un vector con nombres: valor = código, nombre = etiqueta.
opciones <- function(d) {
  v <- c(vars_numericas(d), vars_categoricas(d))
  setNames(v, etiquetas[v])
}
