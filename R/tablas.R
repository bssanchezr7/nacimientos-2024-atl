# Estilo del proyecto para tablas gt (requiere R/estilo.R).
# Recibe tablas ya armadas en el capítulo y solo les aplica el acabado.

# ESTILO BASE ----------------------------------------------------------------

estilo_tabla <- function(tab, titulo, subtitulo = sub_atl) {
  tab |>
    tab_header(title = md(paste0("**", titulo, "**")), subtitle = subtitulo) |>
    tab_source_note(fuente_dane) |>
    opt_table_font(font = "Arial") |>
    tab_options(
      table.border.top.color            = "white",
      heading.align                     = "left",
      heading.title.font.size           = px(16),
      heading.subtitle.font.size        = px(12),
      heading.border.bottom.color       = pal[["principal"]],
      heading.border.bottom.width       = px(2),
      column_labels.font.weight         = "bold",
      column_labels.background.color    = pal[["fondo_tab"]],
      column_labels.border.bottom.color = pal[["principal"]],
      row_group.font.weight             = "bold",
      row_group.background.color        = pal[["fondo_tab"]],
      row_group.border.top.color        = pal[["linea"]],
      table_body.hlines.color           = pal[["linea"]],
      table_body.border.bottom.color    = pal[["principal"]],
      table.font.size                   = px(13),
      source_notes.font.size            = px(11),
      footnotes.font.size               = px(11),
      data_row.padding                  = px(4)
    ) |>
    tab_style(style = cell_text(color = pal[["gris"]]),
              locations = list(cells_source_notes(), cells_footnotes()))
}

# Resaltar la fila "Total" (las tablas bivariadas la guardan en la columna Nivel)
estilo_total <- function(tab) {
  tab |>
    tab_style(style = list(cell_text(weight = "bold"),
                           cell_borders(sides = "top", color = pal[["principal"]])),
              locations = cells_body(rows = Nivel == "Total"))
}

nota_resumen <- "x̄ (S): media (desviación estándar); Me (RIC): mediana (rango intercuartílico)."

formato_p <- function(p) if (p < 0.001) "p < 0,001" else paste("p =", num(p, 0.001))

# Este archivo es SOLO estilo: viste tablas gt ya armadas.
# Los resúmenes (summarise, count, table, chisq.test) viven en cada capítulo.
