# Texto con un ícono de ayuda que muestra una explicación al pasar el cursor
con_ayuda <- function(texto, ayuda) {
  span(texto, tooltip(icon("circle-info", class = "ayuda"), ayuda))
}
