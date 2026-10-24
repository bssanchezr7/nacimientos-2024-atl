# Pestaña "Objetivos": objetivo general y objetivos específicos (capítulo 1
# del libro). Cada objetivo específico enlaza con su prueba en "Hipótesis".

objetivo_general <- paste(
  "Analizar si la atención prenatal, el tipo de parto y la vitalidad del recién nacido",
  "fueron similares para todas las madres residentes en el departamento del Atlántico",
  "durante 2024, o si variaron según su edad, su régimen de salud y el lugar donde viven,",
  "a partir de las Estadísticas Vitales del DANE."
)

# `hipotesis` es el valor del objetivo en el selector de la pestaña Hipótesis
objetivos_especificos <- list(
  list(
    hipotesis = "consultas_edad", icono = "user-clock",
    texto = paste("Comparar el número de consultas prenatales de las madres adolescentes",
                  "con el de las madres de 20 a 34 años y con el de las de 35 años o más,",
                  "para determinar si las más jóvenes tienen un menor seguimiento de su embarazo."),
    respuesta = "Número de consultas prenatales",
    grupos = "Grupo de edad de la madre"
  ),
  list(
    hipotesis = "cesarea_regimen", icono = "hospital",
    texto = paste("Investigar si las cesáreas son igual de frecuentes entre las madres del",
                  "régimen contributivo y las del régimen subsidiado, o si el tipo de afiliación",
                  "en salud marca una diferencia en la forma en que termina el parto."),
    respuesta = "Parto por cesárea (sí o no)",
    grupos = "Régimen de salud de la madre"
  ),
  list(
    hipotesis = "consultas_subregion", icono = "map-location-dot",
    texto = paste("Analizar si el control prenatal es desigual dentro del departamento,",
                  "comparando las consultas de las madres que residen en Barranquilla, en",
                  "Soledad, en el resto del área metropolitana y en los demás municipios."),
    respuesta = "Número de consultas prenatales",
    grupos = "Subregión de residencia de la madre"
  ),
  list(
    hipotesis = "apgar_edad", icono = "heart-pulse",
    texto = paste("Examinar si la vitalidad del recién nacido en su primer minuto de vida,",
                  "medida con el puntaje Apgar, cambia según la edad de la madre (adolescente,",
                  "de 20 a 34 años, o de 35 años o más)."),
    respuesta = "Puntaje Apgar al minuto",
    grupos = "Grupo de edad de la madre"
  ),
  list(
    hipotesis = "apgar_tiempo", icono = "arrow-trend-up",
    texto = paste("Verificar si los recién nacidos mejoran su condición entre el primer y el",
                  "quinto minuto de vida, comparando su puntaje Apgar en ambos momentos."),
    respuesta = "Puntaje Apgar",
    grupos = "Minuto 1 frente a minuto 5 del mismo recién nacido"
  )
)

# Botón que lleva a la pestaña Hipótesis con el objetivo ya elegido
boton_ver_prueba <- function(objetivo) {
  tags$button(
    type = "button", class = "btn btn-sm btn-outline-primary",
    onclick = sprintf(paste0(
      "document.querySelector('a[data-value=\"hipotesis\"]').click();",
      "var s = document.getElementById('hipotesis-objetivo');",
      "if (s && s.selectize) s.selectize.setValue('%s');"
    ), objetivo),
    "Ver la prueba", icon("arrow-right")
  )
}

mod_objetivos_ui <- function(id) {
  tarjetas <- lapply(seq_along(objetivos_especificos), function(i) {
    o <- objetivos_especificos[[i]]
    card(
      fill = FALSE,
      card_header(icon(o$icono), paste("Objetivo específico", i)),
      card_body(
        p(o$texto),
        p(class = "small text-muted mb-0",
          strong("Se compara: ", .noWS = "after"), o$respuesta, br(),
          strong("Según: ", .noWS = "after"), o$grupos)
      ),
      card_footer(boton_ver_prueba(o$hipotesis))
    )
  })

  tagList(
    card(
      fill = FALSE, class = "mt-2 border-start border-4 border-primary",
      card_header(icon("bullseye"), "Objetivo general"),
      card_body(p(class = "lead mb-0", objetivo_general))
    ),
    h5(class = "mt-3", "Objetivos específicos"),
    do.call(layout_column_wrap, c(list(width = "300px", fill = FALSE), tarjetas))
  )
}
