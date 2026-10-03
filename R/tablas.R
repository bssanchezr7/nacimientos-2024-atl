# Tablas descriptivas con el estilo del proyecto (requiere R/estilo.R).
# Las variables se pasan como texto y con etiquetas legibles, por ejemplo:
#   etq_num <- c(APGAR1 = "APGAR al minuto", N_EMB = "Número de embarazos")

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

# Resumen de una variable numérica como texto: x̄ (S) | Me (RIC) | mín – máx
resumen_num <- function(x) {
  tibble(
    n        = length(x),
    media_de = paste0(num(mean(x), 0.01), " (", num(sd(x), 0.01), ")"),
    me_ric   = paste0(num_auto(median(x)), " (", num_auto(IQR(x)), ")"),
    rango    = paste0(num_auto(min(x)), " – ", num_auto(max(x)))
  )
}

etiquetas_resumen <- list(n = "n", media_de = "x̄ (S)",
                          me_ric = "Me (RIC)", rango = "Mín – Máx")

# UNIVARIADO -----------------------------------------------------------------

# Todas las numéricas en una tabla: una fila por variable
tabla_numericas <- function(data, etiquetas,
                            titulo = "Variables numéricas: medidas descriptivas") {
  map(names(etiquetas), \(v) resumen_num(data[[v]])) |>
    list_rbind() |>
    mutate(Variable = unname(etiquetas), .before = 1) |>
    gt() |>
    fmt_number(columns = n, decimals = 0, sep_mark = ".", dec_mark = ",") |>
    cols_label(.list = etiquetas_resumen) |>
    cols_align("left", columns = Variable) |>
    cols_align("center", columns = -Variable) |>
    estilo_tabla(titulo) |>
    tab_footnote(nota_resumen)
}

# Todas las categóricas en una tabla, agrupadas por variable: n (%)
# `ordinales`: variables cuyo orden natural se respeta (no se ordenan por n)
tabla_categoricas <- function(data, etiquetas, ordinales = character(),
                              titulo = "Variables categóricas: frecuencias") {
  map(names(etiquetas), \(v) {
    d <- data |>
      count(Categoría = .data[[v]], name = "n") |>
      mutate(celda = paste0(num(n), " (", pct(n / sum(n)), ")"),
             Variable = etiquetas[[v]])
    if (!v %in% ordinales) d <- arrange(d, desc(n))
    mutate(d, Categoría = as.character(Categoría))
  }) |>
    list_rbind() |>
    select(Variable, Categoría, celda) |>
    gt(groupname_col = "Variable") |>
    cols_label(celda = "n (%)") |>
    cols_align("center", columns = celda) |>
    estilo_tabla(titulo)
}

# BIVARIADO: CATEGÓRICA vs NUMÉRICA -----------------------------------------

# Filas = niveles de la categórica; un bloque de columnas por cada numérica
tabla_cat_num <- function(data, grupo, etiqueta_grupo, etiquetas_num,
                          titulo = paste("Medidas descriptivas según", tolower(etiqueta_grupo))) {
  data <- mutate(data, across(all_of(grupo), \(g) fct_drop(as_factor(g))))

  resumir <- function(d) {
    map(names(etiquetas_num), \(v) {
      resumen_num(d[[v]]) |>
        select(-n) |>
        rename_with(\(c) paste0(v, "__", c))
    }) |>
      list_cbind() |>
      mutate(n = nrow(d), .before = 1)
  }

  por_grupo <- data |>
    group_by(Nivel = .data[[grupo]]) |>
    group_modify(\(d, k) resumir(d)) |>
    ungroup() |>
    mutate(Nivel = as.character(Nivel))
  total <- resumir(data) |> mutate(Nivel = "Total", .before = 1)

  tab <- bind_rows(por_grupo, total) |>
    gt() |>
    fmt_number(columns = n, decimals = 0, sep_mark = ".", dec_mark = ",") |>
    cols_label(Nivel = etiqueta_grupo)

  for (v in names(etiquetas_num)) {
    cols <- paste0(v, "__", c("media_de", "me_ric", "rango"))
    tab <- tab |>
      tab_spanner(label = etiquetas_num[[v]], columns = all_of(cols)) |>
      cols_label(.list = setNames(etiquetas_resumen[-1], cols))
  }

  tab |>
    cols_align("left", columns = Nivel) |>
    cols_align("center", columns = -Nivel) |>
    estilo_tabla(titulo) |>
    estilo_total() |>
    tab_footnote(nota_resumen)
}

# BIVARIADO: CATEGÓRICA vs CATEGÓRICA ---------------------------------------

# Tabla de contingencia con celdas n (%).
# pct_por = "fila" (cada fila suma 100 %), "columna" o "total".
# Incluye totales y, en la nota, la prueba χ² de independencia.
tabla_contingencia <- function(data, fila, columna, etiqueta_fila, etiqueta_columna,
                               pct_por = c("fila", "columna", "total"),
                               titulo = paste(etiqueta_fila, "según", tolower(etiqueta_columna))) {
  pct_por <- match.arg(pct_por)
  data <- mutate(data, across(all_of(c(fila, columna)), \(g) fct_drop(as_factor(g))))

  tabla <- table(data[[fila]], data[[columna]])
  n     <- sum(tabla)
  prop  <- switch(pct_por,
                  fila    = prop.table(tabla, 1),
                  columna = prop.table(tabla, 2),
                  total   = tabla / n)

  celdas <- matrix(paste0(num(as.vector(tabla)), " (", pct(as.vector(prop)), ")"),
                   nrow = nrow(tabla), dimnames = dimnames(tabla))

  tot_fila <- as.vector(rowSums(tabla))
  tot_col  <- as.vector(colSums(tabla))
  d <- as_tibble(celdas, rownames = "Nivel") |>
    mutate(Total = paste0(num(tot_fila), " (", pct(tot_fila / n), ")"))
  fila_total <- c(Nivel = "Total",
                  setNames(paste0(num(tot_col), " (", pct(tot_col / n), ")"), colnames(tabla)),
                  Total = paste0(num(n), " (100,0%)"))
  d <- bind_rows(d, as_tibble_row(fila_total))

  chi <- suppressWarnings(chisq.test(tabla))
  aviso <- if (any(chi$expected < 5)) " Hay frecuencias esperadas < 5: interpretar con cautela." else ""
  nota_chi <- paste0("Prueba χ² de independencia: χ²(", chi$parameter, ") = ",
                     num(chi$statistic, 0.01), "; ", formato_p(chi$p.value), ".", aviso)
  nota_pct <- switch(pct_por,
                     fila    = "Porcentajes por fila.",
                     columna = "Porcentajes por columna.",
                     total   = "Porcentajes sobre el total.")

  d |>
    gt() |>
    cols_label(Nivel = etiqueta_fila) |>
    tab_spanner(label = etiqueta_columna, columns = all_of(colnames(tabla))) |>
    cols_align("left", columns = Nivel) |>
    cols_align("center", columns = -Nivel) |>
    estilo_tabla(titulo) |>
    estilo_total() |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = Total)) |>
    tab_footnote(nota_pct) |>
    tab_footnote(nota_chi)
}

# BIVARIADO: NUMÉRICA vs NUMÉRICA -------------------------------------------

# Matriz de correlaciones (triángulo inferior) coloreada de -1 a 1
tabla_correlacion <- function(data, etiquetas, metodo = c("spearman", "pearson"),
                              titulo = "Correlación entre variables numéricas") {
  metodo <- match.arg(metodo)
  m <- cor(data[names(etiquetas)], method = metodo)
  m[upper.tri(m, diag = TRUE)] <- NA
  dimnames(m) <- list(unname(etiquetas), unname(etiquetas))

  d <- as_tibble(m, rownames = "Variable")
  cols <- unname(etiquetas)

  tab <- d |>
    gt() |>
    fmt_number(columns = all_of(cols), decimals = 2, dec_mark = ",") |>
    sub_missing(columns = all_of(cols), missing_text = "") |>
    data_color(columns = all_of(cols), domain = c(-1, 1), na_color = "white",
               palette = c(pal[["acento"]], "white", pal[["principal"]])) |>
    cols_align("left", columns = Variable) |>
    cols_align("center", columns = all_of(cols))
  for (i in seq_along(cols)) {
    tab <- sub_missing(tab, columns = all_of(cols[i]), rows = i, missing_text = "1")
  }

  nombre <- if (metodo == "spearman") "ρ de Spearman" else "r de Pearson"
  tab |>
    estilo_tabla(titulo) |>
    tab_footnote(paste0("Coeficiente: ", nombre, ". Azul = asociación positiva; ",
                        "terracota = negativa."))
}
