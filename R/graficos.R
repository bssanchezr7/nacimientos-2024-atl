# Gráficos con el estilo del proyecto (requiere R/estilo.R).
# Las variables se pasan sin comillas: graf_histograma(df_atl, APGAR1, ...)

# AUXILIARES -----------------------------------------------------------------

# Líneas de media (continua) y mediana (punteada) en color de acento
lineas_ref <- function(x) {
  ref <- tibble(tipo = c("Media", "Mediana"), valor = c(mean(x), median(x)))
  list(
    geom_vline(data = ref, aes(xintercept = valor, linetype = tipo),
               color = pal[["acento"]], linewidth = 0.7),
    scale_linetype_manual(values = c(Media = "solid", Mediana = "22"), name = NULL)
  )
}

# Punto de la media (rombo) para boxplots
punto_media <- function() {
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = pal[["acento"]], color = "white")
}

es_discreta <- function(x) all(x == round(x)) && diff(range(x)) <= 60

# Ejes con cortes enteros si la variable es discreta (evita 2,5 o 7,5)
cortes_para <- function(x) {
  if (!es_discreta(x)) return(waiver())
  \(l) { b <- scales::breaks_extended(7)(l); b[b == round(b)] }
}
escala_x <- function(x, ...) scale_x_continuous(breaks = cortes_para(x), labels = num_auto, ...)
escala_y <- function(x, ...) scale_y_continuous(breaks = cortes_para(x), labels = num_auto, ...)

# Suavizado por defecto: más alto en variables discretas para no dibujar "dientes"
adjust_auto <- function(x) if (es_discreta(x)) 2.5 else 1

# Leyenda en una columna a la derecha cuando hay muchos niveles
leyenda_niveles <- function(n) {
  if (n > 6) theme(legend.position = "right", legend.justification = "center") else NULL
}

# Rota las etiquetas del eje x si son largas
ejes_legibles <- function(etiquetas) {
  if (max(nchar(as.character(etiquetas))) > 10) guides(x = guide_axis(angle = 35)) else NULL
}

# UNIVARIADO: NUMÉRICAS ------------------------------------------------------

# Histograma. Para enteros con pocos valores usa una barra por valor.
graf_histograma <- function(data, var, titulo, eje_x, binwidth = NULL) {
  x <- pull(data, {{ var }})
  discreta <- es_discreta(x)
  if (is.null(binwidth)) binwidth <- if (discreta) 1 else 2 * IQR(x) / length(x)^(1/3)

  ggplot(data, aes(x = {{ var }})) +
    geom_histogram(binwidth = binwidth, center = if (discreta) 0 else NULL,
                   fill = pal[["principal"]], color = "white", linewidth = 0.3) +
    lineas_ref(x) +
    escala_x(x) +
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.05))) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = "Nacimientos",
         caption = fuente_dane)
}

# Curva de densidad. `adjust` > 1 suaviza (útil en variables discretas).
graf_densidad <- function(data, var, titulo, eje_x, adjust = NULL) {
  x <- pull(data, {{ var }})
  adjust <- adjust %||% adjust_auto(x)
  ggplot(data, aes(x = {{ var }})) +
    geom_density(adjust = adjust, bounds = range(x), fill = pal[["claro"]],
                 color = pal[["principal"]], alpha = 0.6, linewidth = 0.8) +
    lineas_ref(x) +
    escala_x(x) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = "Densidad",
         caption = fuente_dane)
}

# Caja y bigotes horizontal; el rombo marca la media
graf_boxplot <- function(data, var, titulo, eje_x) {
  x <- pull(data, {{ var }})
  ggplot(data, aes(x = {{ var }}, y = "")) +
    geom_boxplot(width = 0.4, fill = pal[["claro"]], color = pal[["principal"]],
                 outlier.color = pal[["gris"]], outlier.alpha = 0.4, outlier.size = 1) +
    punto_media() +
    escala_x(x) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = NULL,
         caption = paste0(fuente_dane, "  ·  ◆ = media")) +
    theme_proyecto(grid = "x")
}

# Pareto: frecuencias de mayor a menor + porcentaje acumulado.
# Sirve para numéricas discretas y para categóricas.
graf_pareto <- function(data, var, titulo, eje_x = NULL) {
  d <- data |>
    count(cat = {{ var }}, name = "n") |>
    arrange(desc(n)) |>
    mutate(cat  = fct_inorder(as.character(cat)),
           acum = cumsum(n) / sum(n))
  tope <- max(d$n)

  ggplot(d, aes(x = cat)) +
    geom_col(aes(y = n), width = 0.75, fill = pal[["principal"]]) +
    geom_hline(yintercept = 0.8 * tope, linetype = "22", color = pal[["gris"]],
               linewidth = 0.4) +
    geom_line(aes(y = acum * tope, group = 1), color = pal[["acento"]], linewidth = 0.8) +
    geom_point(aes(y = acum * tope), color = pal[["acento"]], size = 1.8) +
    scale_y_continuous(labels = num, expand = expansion(mult = c(0, 0.05)),
                       sec.axis = sec_axis(~ . / tope, name = "% acumulado",
                                           labels = \(v) pct(v, 1))) +
    ejes_legibles(d$cat) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = "Nacimientos",
         caption = paste0(fuente_dane, "  ·  Línea punteada: 80 % acumulado"))
}

# UNIVARIADO: CATEGÓRICAS ----------------------------------------------------

# Barras horizontales con n (%), la moda resaltada.
# ordenar = FALSE respeta el orden de los niveles (variables ordinales).
graf_barras <- function(data, var, titulo, ordenar = TRUE) {
  d <- data |>
    count(cat = {{ var }}, name = "n") |>
    mutate(pct = n / sum(n), moda = n == max(n))
  d <- if (ordenar) mutate(d, cat = fct_reorder(cat, n)) else mutate(d, cat = fct_rev(cat))

  ggplot(d, aes(x = n, y = cat, fill = moda)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(num(n), "  (", pct(pct), ")")), hjust = -0.08) +
    scale_fill_manual(values = c(`TRUE` = pal[["principal"]], `FALSE` = pal[["claro"]]),
                      guide = "none") +
    scale_x_continuous(labels = num, expand = expansion(mult = c(0, 0.25))) +
    labs(title = titulo, subtitle = sub_atl, x = "Nacimientos", y = NULL,
         caption = fuente_dane) +
    theme_proyecto(grid = "x")
}

# BIVARIADO: CATEGÓRICA vs NUMÉRICA -----------------------------------------

# Densidades superpuestas por grupo, con la mediana de cada grupo punteada.
# Se excluyen grupos con menos de `min_n` casos (su densidad no es confiable).
graf_densidad_grupos <- function(data, var, grupo, titulo, eje_x, etiqueta_grupo,
                                 ordinal = FALSE, min_n = 30, adjust = NULL) {
  x <- pull(data, {{ var }})
  adjust <- adjust %||% adjust_auto(x)
  d <- data |>
    mutate(g = fct_drop(as_factor({{ grupo }}))) |>
    add_count(g, name = "n_g")
  excluidos <- d |> filter(n_g < min_n) |> distinct(g) |> pull(g) |> as.character()
  d <- d |> filter(n_g >= min_n) |> mutate(g = fct_drop(g))
  medianas <- d |> summarise(me = median({{ var }}), .by = g)
  colores <- setNames(colores_grupo(nlevels(d$g), ordinal), levels(d$g))

  nota <- if (length(excluidos)) {
    paste0("  ·  Excluidos por n < ", min_n, ": ", paste(excluidos, collapse = ", "))
  } else ""

  ggplot(d, aes(x = {{ var }}, fill = g, color = g)) +
    geom_density(adjust = adjust, bounds = range(x), alpha = 0.2, linewidth = 0.8) +
    geom_vline(data = medianas, aes(xintercept = me, color = g),
               linetype = "22", linewidth = 0.6, show.legend = FALSE) +
    scale_fill_manual(values = colores, name = etiqueta_grupo) +
    scale_color_manual(values = colores, name = etiqueta_grupo) +
    escala_x(x) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    leyenda_niveles(nlevels(d$g)) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = "Densidad",
         caption = paste0(fuente_dane, "  ·  Líneas punteadas: medianas", nota))
}

# Cajas y bigotes por grupo (horizontal), con n en la etiqueta y rombo = media.
# ordenar = TRUE ordena los grupos por mediana; FALSE respeta el orden (ordinales).
graf_boxplot_grupos <- function(data, var, grupo, titulo, eje_x, ordenar = TRUE) {
  x <- pull(data, {{ var }})
  d <- data |>
    mutate(g = fct_drop(as_factor({{ grupo }}))) |>
    add_count(g, name = "n_g") |>
    mutate(g_lab = paste0(g, "  (n = ", num(n_g), ")"),
           g_lab = fct_reorder(g_lab, as.integer(g)))
  d <- if (ordenar) {
    mutate(d, g_lab = fct_reorder(g_lab, {{ var }}, .fun = median))
  } else {
    mutate(d, g_lab = fct_rev(g_lab))
  }

  ggplot(d, aes(x = {{ var }}, y = g_lab)) +
    geom_boxplot(width = 0.55, fill = pal[["claro"]], color = pal[["principal"]],
                 outlier.color = pal[["gris"]], outlier.alpha = 0.35, outlier.size = 0.9) +
    punto_media() +
    escala_x(x) +
    labs(title = titulo, subtitle = sub_atl, x = eje_x, y = NULL,
         caption = paste0(fuente_dane, "  ·  ◆ = media")) +
    theme_proyecto(grid = "x")
}

# BIVARIADO: CATEGÓRICA vs CATEGÓRICA ---------------------------------------

# Barras 100 % apiladas: composición de `columna` dentro de cada nivel de `fila`.
# ordinal = TRUE usa escala secuencial para `columna`.
graf_barras_apiladas <- function(data, fila, columna, titulo, etiqueta_columna,
                                 ordinal = FALSE, min_etiqueta = 0.05) {
  d <- data |>
    mutate(f = fct_drop(as_factor({{ fila }})), c = fct_drop(as_factor({{ columna }}))) |>
    count(f, c, name = "n") |>
    mutate(p = n / sum(n), n_f = sum(n), .by = f) |>
    mutate(f_lab = fct_rev(fct_reorder(paste0(f, "  (n = ", num(n_f), ")"), as.integer(f))))
  colores <- setNames(colores_grupo(nlevels(d$c), ordinal), levels(d$c))
  d <- mutate(d, txt = color_texto_sobre(colores[as.character(c)]))

  ggplot(d, aes(x = p, y = f_lab, fill = c)) +
    geom_col(position = position_fill(reverse = TRUE), width = 0.7,
             color = "white", linewidth = 0.3) +
    geom_text(aes(label = if_else(p >= min_etiqueta, pct(p, 1), ""), color = txt),
              position = position_fill(vjust = 0.5, reverse = TRUE), size = 3.1) +
    scale_color_identity() +
    scale_fill_manual(values = colores, name = etiqueta_columna) +
    scale_x_continuous(labels = \(v) pct(v, 1), expand = expansion(0)) +
    labs(title = titulo, subtitle = sub_atl, x = NULL, y = NULL, caption = fuente_dane) +
    theme_proyecto(grid = "none") +
    leyenda_niveles(nlevels(d$c))
}

# BIVARIADO: NUMÉRICA vs NUMÉRICA -------------------------------------------

# Dispersión con recta de tendencia y ρ de Spearman en el subtítulo.
# tipo = "conteo" dibuja un punto por combinación con tamaño = frecuencia,
# que se lee mejor cuando ambas variables son enteras (muchos puntos repetidos).
graf_dispersion <- function(data, x, y, titulo, eje_x, eje_y,
                            tipo = c("puntos", "conteo")) {
  tipo <- match.arg(tipo)
  vx <- pull(data, {{ x }})
  vy <- pull(data, {{ y }})
  rho <- cor(vx, vy, method = "spearman")

  capa <- if (tipo == "puntos") {
    geom_jitter(width = 0.25, height = 0.25, alpha = 0.08, size = 0.8,
                color = pal[["principal"]])
  } else {
    geom_count(aes(size = after_stat(n)), color = pal[["principal"]], alpha = 0.7)
  }

  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    capa +
    geom_smooth(method = "lm", formula = y ~ x, color = pal[["acento"]],
                fill = pal[["acento"]], alpha = 0.2, linewidth = 0.9) +
    scale_size_area(max_size = 9, labels = num, name = "Nacimientos") +
    escala_x(vx) +
    escala_y(vy) +
    labs(title = titulo,
         subtitle = paste0(sub_atl, "  ·  ρ de Spearman = ", num(rho, 0.01)),
         x = eje_x, y = eje_y, caption = fuente_dane) +
    theme_proyecto(grid = "both")
}

# Mapa de calor de la matriz de correlaciones (triángulo inferior)
graf_correlacion <- function(data, etiquetas, metodo = c("spearman", "pearson"),
                             titulo = "Correlación entre variables numéricas") {
  metodo <- match.arg(metodo)
  m <- cor(data[names(etiquetas)], method = metodo)
  niveles <- unname(etiquetas)
  d <- as_tibble(m, rownames = "v1") |>
    pivot_longer(-v1, names_to = "v2", values_to = "r") |>
    mutate(i = match(v1, names(etiquetas)), j = match(v2, names(etiquetas))) |>
    filter(i > j) |>
    mutate(v1 = factor(etiquetas[v1], levels = rev(niveles)),
           v2 = factor(etiquetas[v2], levels = niveles))

  nombre <- if (metodo == "spearman") "ρ de Spearman" else "r de Pearson"
  ggplot(d, aes(x = v2, y = v1, fill = r)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(aes(label = num(r, 0.01), color = abs(r) > 0.5), size = 4) +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = pal[["texto"]]), guide = "none") +
    scale_fill_gradient2(low = pal[["acento"]], mid = "white", high = pal[["principal"]],
                         limits = c(-1, 1), name = nombre, labels = \(v) num(v, 0.1)) +
    guides(x = guide_axis(angle = 25)) +
    coord_fixed() +
    labs(title = titulo, subtitle = sub_atl, x = NULL, y = NULL, caption = fuente_dane) +
    theme_proyecto(grid = "none")
}
