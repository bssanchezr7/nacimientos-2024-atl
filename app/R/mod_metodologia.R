# Pestaña "Datos y metodología": fuente, depuración, variables y faltantes.

# Variables tomadas de la base del DANE (capítulo 2 del libro)
variables_dane <- tibble::tribble(
  ~Variable,    ~`Qué registra`,                             ~Tipo,                          ~Valores,
  "NUMCONSUL",  "Número de consultas prenatales",            "Numérica discreta",            "Número entero, desde 0",
  "APGAR1",     "Puntaje Apgar al minuto de vida",           "Numérica discreta (puntaje)",  "De 0 a 10",
  "APGAR2",     "Puntaje Apgar a los cinco minutos de vida", "Numérica discreta (puntaje)",  "De 0 a 10",
  "TIPO_PARTO", "Tipo de parto",                             "Categórica nominal",           "Espontáneo, cesárea, instrumentado",
  "SEG_SOCIAL", "Régimen de seguridad social de la madre",   "Categórica nominal",           "Contributivo, subsidiado, excepción, especial, no asegurado",
  "EDAD_MADRE", "Edad de la madre, en grupos de cinco años", "Categórica ordinal",           "Nueve grupos, de 10–14 a 50–54 años",
  "CODMUNRE",   "Municipio de residencia de la madre",       "Categórica nominal",           "Los 23 municipios del Atlántico"
)

# Variables construidas a partir de las anteriores
variables_construidas <- tibble::tribble(
  ~Variable,                 ~`Se construye a partir de`, ~Categorías,                                                                                                                         ~Objetivo,
  "Subregión de residencia", "CODMUNRE",                  "Barranquilla; Soledad; resto del área metropolitana (Galapa, Malambo y Puerto Colombia); resto del departamento (18 municipios)", "3",
  "Grupo de edad",           "EDAD_MADRE",                "Adolescente (10 a 19 años); de 20 a 34 años; de 35 años o más",                                                                   "1 y 4",
  "Cesárea",                 "TIPO_PARTO",                "Sí; no (parto espontáneo o instrumentado)",                                                                                       "2"
)

# Tabla HTML sencilla con el estilo de Bootstrap. Las columnas de `codigo` se
# muestran como código (nombres de variables del DANE).
tabla_simple <- function(df, codigo = character()) {
  celda <- function(col, valor) if (col %in% codigo) tags$td(tags$code(valor)) else tags$td(valor)
  tags$table(
    class = "table table-sm align-middle mb-0",
    tags$thead(tags$tr(lapply(names(df), tags$th))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(lapply(names(df), function(col) celda(col, df[[col]][i])))
    }))
  )
}

# Un paso de la depuración: cifra grande, qué representa y un detalle
paso_depuracion <- function(cifra, titulo, detalle, icono) {
  div(class = "paso-depuracion",
      div(class = "paso-icono", icon(icono)),
      div(div(class = "paso-cifra", cifra),
          div(class = "paso-titulo", titulo),
          div(class = "paso-detalle", detalle)))
}

mod_metodologia_ui <- function(id) {
  ns <- NS(id)

  tagList(
    p(class = "lead mt-2",
      "De dónde salen los datos, cómo se depuraron y qué variables se usan en el análisis."),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)), fill = FALSE,
      card(
        card_header(icon("database"), "Fuente y población"),
        card_body(
          p("Los datos provienen de las ", strong("Estadísticas Vitales del DANE", .noWS = "outside"),
            ": el registro de los certificados de nacido vivo que se diligencian por cada nacimiento en Colombia, publicados como microdatos anonimizados de acceso abierto. Se usó la base de 2024."),
          p("Se seleccionaron los nacimientos cuyas ", strong("madres residían en el Atlántico", .noWS = "outside"),
            ", sin importar dónde ocurrió el parto."),
          p(class = "mb-0", "Cada registro corresponde a un ", strong("recién nacido", .noWS = "outside"),
            ", no a una madre: un parto múltiple aparece una vez por cada hijo.")
        )
      ),
      card(
        card_header(icon("filter"), "Depuración de los datos"),
        card_body(uiOutput(ns("depuracion")))
      )
    ),

    layout_columns(
      col_widths = breakpoints(sm = c(12, 12), lg = c(8, 4)), fill = FALSE,
      navset_card_underline(
        title = tagList(icon("table-list"), "Variables"),
        nav_panel("Tomadas del DANE",
                  tabla_simple(variables_dane, codigo = "Variable")),
        nav_panel("Construidas para el análisis",
                  tabla_simple(variables_construidas, codigo = "Se construye a partir de"))
      ),
      card(
        card_header(icon("circle-question"), "Valores faltantes"),
        card_body(uiOutput(ns("faltantes")))
      )
    )
  )
}

mod_metodologia_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {

    depuracion <- attr(datos, "depuracion")
    faltantes  <- attr(datos, "faltantes")

    # ---- depuración y faltantes ----------------------------------------------

    output$depuracion <- renderUI({
      eliminados <- depuracion$n_atlantico - depuracion$n_final
      tagList(
        paso_depuracion(num(depuracion$n_nacional), "Nacimientos registrados en Colombia en 2024",
                        "Base completa de las Estadísticas Vitales.", "earth-americas"),
        div(class = "paso-flecha", icon("arrow-down")),
        paso_depuracion(num(depuracion$n_atlantico), "Con madre residente en el Atlántico",
                        paste0(fmt_pct(depuracion$n_atlantico / depuracion$n_nacional), " del total nacional."),
                        "location-dot"),
        div(class = "paso-flecha", icon("arrow-down")),
        paso_depuracion(num(depuracion$n_final), "Sin datos faltantes: la base del análisis",
                        paste0("Se eliminaron ", num(eliminados), " registros (",
                               fmt_pct(eliminados / depuracion$n_atlantico, 0.01), ")."),
                        "circle-check")
      )
    })

    output$faltantes <- renderUI({
      con_faltantes <- faltantes[faltantes$n > 0, ]
      tabla <- tibble::tibble(
        Variable = faltantes$variable,
        Faltantes = num(faltantes$n),
        `%` = fmt_pct(faltantes$pct, 0.01)
      )
      tagList(
        p("Siguiendo el diccionario del DANE, el código 99 (dato no registrado) se trató como faltante. ",
          if (nrow(con_faltantes) > 0) {
            paste0("Solo ", paste(con_faltantes$variable, collapse = " y "),
                   " tuvieron faltantes. Como no superan el 5 %, esos registros se eliminaron.")
          }),
        tabla_simple(tabla, codigo = "Variable")
      )
    })
  })
}
