# mod_hipotesis.R — pestaña "Hipótesis"

# ---- formato ---------------------------------------------------------------

formato_p <- function(p) {
  ifelse(p < 0.001, "p < 0,001", paste0("p = ", num(p, 0.001)))
}

# Magnitud del efecto con los umbrales de Cohen (1988).
magnitud <- function(valor, cortes) {
  ifelse(valor < cortes[1], "despreciable",
         ifelse(valor < cortes[2], "pequeño",
                ifelse(valor < cortes[3], "mediano", "grande")))
}
magnitud_r   <- function(r) magnitud(r, c(0.10, 0.30, 0.50))
magnitud_eta <- function(e) magnitud(e, c(0.01, 0.06, 0.14))

# r de Mann-Whitney: |Z| / √N, con el Z de los rangos medios corregido por
# empates.
r_wilcox <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x  <- x[ok]
  g  <- droplevels(as.factor(g[ok]))
  n1 <- sum(g == levels(g)[1])
  n2 <- sum(g == levels(g)[2])
  N  <- n1 + n2
  u  <- unname(wilcox.test(x ~ g, exact = FALSE)$statistic)
  t  <- table(rank(x))
  z  <- (u - n1 * n2 / 2) /
    sqrt(n1 * n2 / 12 * (N + 1 - sum(t^3 - t) / (N * (N - 1))))
  abs(z) / sqrt(N)
}

# r de Wilcoxon pareado: |Z| / √N, con los ceros dentro del ranking (método
# de Pratt).
r_wilcox_pareado <- function(dif) {
  r  <- rank(abs(dif))
  rn <- r[dif != 0]
  v  <- sum(r[dif > 0])
  z  <- (v - sum(rn) / 2) / sqrt(sum(rn^2) / 4)
  abs(z) / sqrt(length(dif))
}

# Tabla de resultados
tabla_dt <- function(tb) {
  primera  <- as.character(tb[[1]])
  etiqueta <- !all(grepl("^[0-9.,]+$", primera))
  centro   <- if (etiqueta) seq_len(ncol(tb) - 1) else "_all"

  DT::datatable(
    tb, rownames = FALSE, class = "compact",
    options = list(
      dom = "t", ordering = FALSE, pageLength = 30, scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center text-nowrap", targets = centro)
      )
    )
  )
}

# ---- catálogo de objetivos -------------------------------------------------

objetivos_hipotesis <- list(

  consultas_edad = list(
    etiqueta = "Consultas según edad",
    numero   = 1,
    titulo   = "consultas prenatales según la edad de la madre",
    pregunta = paste("Se busca saber si las madres adolescentes tienen menos",
                     "consultas prenatales que las madres de 20 a 34 años y",
                     "que las de 35 años o más."),

    analizar = function(d) {
      niveles <- c("Adolescente", "De 20 a 34 años", "De 35 años o más")
      pares   <- list(niveles[c(1, 2)], niveles[c(1, 3)])
      meta    <- vapply(pares, paste, "", collapse = " – ")

      par_de <- function(p) {
        d |>
          filter(EDAD_MADRE %in% p) |>
          mutate(EDAD_MADRE = fct_drop(EDAD_MADRE))
      }

      lil <- d |>
        group_by(g = EDAD_MADRE) |>
        group_modify(~ {
          r <- nortest::lillie.test(.x$NUMCONSUL)
          tibble::tibble(n = nrow(.x), D = unname(r$statistic), p = r$p.value)
        }) |>
        ungroup()

      lev <- lapply(pares, function(p) {
        car::leveneTest(NUMCONSUL ~ EDAD_MADRE, data = par_de(p), center = "median")
      })
      f_lev <- vapply(lev, function(x) unname(x[1, 2]), 0)
      p_lev <- vapply(lev, function(x) unname(x[1, 3]), 0)

      wil <- lapply(pares, function(p) {
        rstatix::wilcox_test(NUMCONSUL ~ EDAD_MADRE, data = par_de(p),
                             paired = FALSE)
      })
      res_w <- tibble::tibble(
        Comparación = meta,
        n1 = vapply(wil, function(x) x$n1, 0),
        n2 = vapply(wil, function(x) x$n2, 0),
        U  = vapply(wil, function(x) x$statistic, 0),
        p  = vapply(wil, function(x) x$p, 0)
      ) |>
        mutate(p_adj = p.adjust(p, method = "bonferroni"))

      r_ef <- vapply(pares, function(p) {
        dd <- par_de(p)
        r_wilcox(dd$NUMCONSUL, dd$EDAD_MADRE)
      }, 0)

      med   <- vapply(niveles, function(n) median(d$NUMCONSUL[d$EDAD_MADRE == n]), 0)
      media <- vapply(niveles, function(n) mean(d$NUMCONSUL[d$EDAD_MADRE == n]), 0)

      list(
        pasos = list(
          list(
            titulo  = "Prueba de normalidad",
            h0      = "las consultas del grupo siguen distribución normal",
            h1      = "no la siguen",
            tabla   = tibble::tibble(
              `Grupo de edad` = as.character(lil$g),
              n = num(lil$n),
              D = num(lil$D, 0.001),
              `Valor p` = formato_p(lil$p)
            ),
            lectura = paste0(
              "En los tres grupos se rechaza la normalidad (",
              formato_p(max(lil$p)), "), por lo que se decide usar la prueba ",
              "no paramétrica de Wilcoxon (Mann-Whitney) por pares."
            )
          ),
          list(
            titulo  = "Prueba de homogeneidad de varianzas",
            h0      = "igual varianza entre los dos grupos",
            h1      = "varianza distinta",
            tabla   = tibble::tibble(
              Comparación = meta,
              F = num(f_lev, 0.01),
              `Valor p` = formato_p(p_lev)
            ),
            lectura = paste0(
              "En la comparación con las madres de 20 a 34 años se rechaza la ",
              "homogeneidad (", formato_p(p_lev[1]), "), por lo que su prueba ",
              "de Wilcoxon se interpreta solo como un desplazamiento global, ",
              "P(X > Y) ≠ 0,5. En la comparación con las de 35 años o más no ",
              "hay evidencia estadísticamente significativa para rechazarla (",
              formato_p(p_lev[2]), "), por lo que admite una lectura como ",
              "diferencia de medianas."
            )
          ),
          list(
            titulo  = "Comparación adolescentes frente a adultas (Wilcoxon–Mann–Whitney)",
            h0      = "P(X<sub>adol</sub> &gt; Y<sub>adulta</sub>) = 0,5 (misma distribución)",
            h1      = "P(X<sub>adol</sub> &gt; Y<sub>adulta</sub>) ≠ 0,5",
            tabla   = tibble::tibble(
              Comparación = meta,
              `n adolescentes` = num(res_w$n1),
              `n grupo comparado` = num(res_w$n2),
              U = num(res_w$U),
              `Valor p` = formato_p(res_w$p),
              `Valor p ajustado (Bonferroni)` = formato_p(res_w$p_adj)
            ),
            lectura = paste0(
              "En ambas comparaciones se rechaza H₀ (",
              formato_p(max(res_w$p_adj)), " en el peor caso). Se concluye un ",
              "desplazamiento hacia menos consultas en las adolescentes frente ",
              "a los dos grupos adultos."
            )
          ),
          list(
            titulo  = "Tamaño del efecto",
            tabla   = tibble::tibble(
              Comparación = meta,
              r = num(r_ef, 0.001),
              Magnitud = magnitud_r(r_ef)
            ),
            lectura = paste0(
              "Las adolescentes presentaron menos consultas que las madres de ",
              "20 a 34 años (mediana ", num_auto(med[1]), " frente a ",
              num_auto(med[2]), "; medias ", num(media[1], 0.01), " frente a ",
              num(media[2], 0.01), "; r = ", num(r_ef[1], 0.001), ", efecto ",
              magnitud_r(r_ef[1]), ") y que las de 35 años o más (mediana ",
              num_auto(med[1]), " frente a ", num_auto(med[3]), "; medias ",
              num(media[1], 0.01), " frente a ", num(media[3], 0.01), "; r = ",
              num(r_ef[2], 0.001), ", efecto ", magnitud_r(r_ef[2]), ")."
            ),
            pie = "r = Z / √N. Umbrales: 0,10 pequeño; 0,30 mediano; 0,50 grande."
          )
        ),
        conclusion = paste(
          "De esto se concluye que las madres adolescentes sí tienen un menor",
          "número de consultas prenatales, y que la diferencia es más marcada",
          "frente a las madres de 35 años o más."
        )
      )
    }
  ),

  cesarea_regimen = list(
    etiqueta = "Cesáreas según régimen",
    numero   = 2,
    titulo   = "cesáreas según el régimen de salud",
    pregunta = paste("Se busca saber si las cesáreas son igual de frecuentes en",
                     "el régimen contributivo y en el subsidiado."),

    analizar = function(d) {
      dd <- d |>
        filter(SEG_SOCIAL %in% c("Contributivo", "Subsidiado")) |>
        mutate(SEG_SOCIAL = fct_drop(SEG_SOCIAL))

      tab <- table(dd$SEG_SOCIAL, dd$CESAREA)
      chi <- stats::chisq.test(tab)
      dif <- stats::prop.test(tab[, "Sí"], rowSums(tab))
      v   <- sqrt(unname(chi$statistic) / sum(tab))

      esp     <- chi$expected
      dif_pts <- 100 * diff(rev(dif$estimate))

      list(
        pasos = list(
          list(
            titulo  = "Condición de la prueba",
            tabla   = tibble::tibble(
              Régimen = rownames(esp),
              Sí = num(esp[, "Sí"], 1),
              No = num(esp[, "No"], 1)
            ),
            lectura = paste0(
              "La menor frecuencia esperada es ", num(min(esp), 1),
              ", por lo que la condición se cumple."
            ),
            pie = paste(
              "La prueba chi-cuadrado de homogeneidad requiere que todas las",
              "frecuencias esperadas sean mayores o iguales que 5."
            )
          ),
          list(
            titulo  = "Prueba de homogeneidad de proporciones",
            h0      = "p<sub>C</sub> = p<sub>S</sub> (la proporción de cesáreas es la misma en los dos regímenes)",
            h1      = "p<sub>C</sub> ≠ p<sub>S</sub> (la proporción de cesáreas es distinta)",
            tabla   = tibble::tibble(
              Contributivo = pct(unname(dif$estimate[1])),
              Subsidiado = pct(unname(dif$estimate[2])),
              `Diferencia (IC 95 %)` = paste0(
                num(dif_pts, 0.1), " puntos (",
                num(100 * dif$conf.int[1], 0.1), " a ",
                num(100 * dif$conf.int[2], 0.1), ")"
              ),
              `χ²` = num(unname(chi$statistic), 0.01),
              gl = unname(chi$parameter),
              `Valor p` = formato_p(chi$p.value)
            ),
            lectura = paste0(
              "Se rechaza la hipótesis nula (", formato_p(chi$p.value), "): la ",
              "proporción de cesáreas no es igual en los dos regímenes. En el ",
              "contributivo fue del ", pct(unname(dif$estimate[1])), " y en el ",
              "subsidiado del ", pct(unname(dif$estimate[2])), ", una ",
              "diferencia de ", num(dif_pts, 0.1), " puntos porcentuales."
            )
          ),
          list(
            titulo  = "Tamaño del efecto",
            tabla   = tibble::tibble(
              Medida = "V de Cramér",
              Valor = num(v, 0.01)
            ),
            lectura = paste0(
              "Si bien existe una diferencia, el tamaño del efecto indica que es ",
              "despreciable: la V de Cramér resultante fue de ", num(v, 0.01),
              ", por debajo del 0,10 que se considera un efecto pequeño (Cohen, 1988)."
            ),
            pie = "Umbrales: 0,10 pequeño; 0,30 mediano; 0,50 grande."
          )
        ),
        conclusion = paste(
          "De esto se concluye que, aunque el porcentaje de cesáreas es mayor en",
          "el régimen contributivo, la diferencia entre los dos regímenes es",
          "mínima."
        )
      )
    }
  ),

  consultas_subregion = list(
    etiqueta = "Consultas según subregión",
    numero   = 3,
    titulo   = "consultas prenatales según la subregión de residencia",
    pregunta = paste("Se busca saber si el número de consultas prenatales es",
                     "desigual entre las subregiones de residencia de la madre."),

    analizar = function(d) {
      lil <- d |>
        group_by(g = SUBREGION) |>
        group_modify(~ {
          r <- nortest::lillie.test(.x$NUMCONSUL)
          tibble::tibble(n = nrow(.x), D = unname(r$statistic), p = r$p.value)
        }) |>
        ungroup()

      lev   <- car::leveneTest(NUMCONSUL ~ SUBREGION, data = d, center = "median")
      f_lev <- unname(lev[1, 2])
      p_lev <- unname(lev[1, 3])
      gl    <- paste(lev[1, 1], "y", num(lev[2, 1]))

      kw  <- rstatix::kruskal_test(NUMCONSUL ~ SUBREGION, data = d)
      eta <- rstatix::kruskal_effsize(NUMCONSUL ~ SUBREGION, data = d)

      # Post hoc de Kruskal-Wallis: qué pares de subregiones difieren
      dunn    <- rstatix::dunn_test(d, NUMCONSUL ~ SUBREGION, p.adjust.method = "bonferroni")
      sin_dif <- dunn[dunn$p.adj >= 0.05, ]
      pares_sin_dif <- paste(sin_dif$group1, "y", sin_dif$group2, collapse = "; ")

      list(
        pasos = list(
          list(
            titulo  = "Prueba de normalidad",
            h0      = "las consultas de la subregión siguen distribución normal",
            h1      = "no la siguen",
            tabla   = tibble::tibble(
              Subregión = as.character(lil$g),
              n = num(lil$n),
              D = num(lil$D, 0.001),
              `Valor p` = formato_p(lil$p)
            ),
            lectura = paste0(
              "En las cuatro subregiones se rechaza la normalidad (",
              formato_p(max(lil$p)), "), por lo que se decide usar la prueba ",
              "no paramétrica de Kruskal-Wallis."
            )
          ),
          list(
            titulo  = "Prueba de homogeneidad de varianzas",
            h0      = "igual varianza entre las subregiones",
            h1      = "varianza distinta en al menos una",
            tabla   = tibble::tibble(
              F = num(f_lev, 0.01),
              gl = gl,
              `Valor p` = formato_p(p_lev)
            ),
            lectura = paste0(
              "Se rechaza la homogeneidad (", formato_p(p_lev), "), por lo que ",
              "la prueba de Kruskal-Wallis se interpreta como una diferencia ",
              "global entre distribuciones y no como una diferencia de ",
              "medianas en sentido estricto."
            )
          ),
          list(
            titulo  = "Prueba de Kruskal-Wallis",
            h0      = "las consultas de las cuatro subregiones provienen de la misma distribución",
            h1      = "al menos una subregión proviene de una distribución distinta",
            tabla   = tibble::tibble(
              `χ²` = num(kw$statistic, 0.01),
              gl = kw$df,
              `Valor p` = formato_p(kw$p)
            ),
            lectura = paste0(
              "Se rechaza la hipótesis nula (", formato_p(kw$p), "): el control ",
              "prenatal no fue igual en las cuatro subregiones."
            )
          ),
          list(
            titulo  = "Comparaciones por pares (prueba de Dunn)",
            h0      = "las consultas de las dos subregiones provienen de la misma distribución",
            h1      = "provienen de distribuciones distintas",
            tabla   = tibble::tibble(
              Comparación = paste(dunn$group1, "–", dunn$group2),
              Z = num(dunn$statistic, 0.01),
              `Valor p ajustado (Bonferroni)` = formato_p(dunn$p.adj)
            ),
            lectura = if (nrow(sin_dif) == 0) {
              "Todos los pares de subregiones difieren entre sí."
            } else {
              paste0("Todos los pares de subregiones difieren entre sí, salvo ",
                     pares_sin_dif, " (", paste(formato_p(sin_dif$p.adj), collapse = "; "), ").")
            },
            pie = paste(
              "Kruskal-Wallis indica que al menos una subregión difiere, pero no cuáles.",
              "La prueba de Dunn compara cada par usando los mismos rangos de",
              "Kruskal-Wallis; como son seis comparaciones, los valores p se ajustan",
              "con la corrección de Bonferroni."
            )
          ),
          list(
            titulo  = "Tamaño del efecto",
            tabla   = tibble::tibble(
              Medida = "Eta cuadrado",
              Valor = num(eta$effsize, 0.001)
            ),
            lectura = paste0(
              "El tamaño del efecto fue ", magnitud_eta(eta$effsize),
              ": el eta cuadrado resultante fue de ", num(eta$effsize, 0.001),
              " (Cohen, 1988)."
            ),
            pie = "Umbrales: 0,01 pequeño; 0,06 mediano; 0,14 grande."
          )
        ),
        conclusion = paste0(
          "De esto se concluye que la desigualdad entre subregiones existe, ",
          "aunque es de poca magnitud",
          if (nrow(sin_dif) > 0) paste0(", y que solo ", pares_sin_dif, " no se diferencian entre sí") else "",
          "."
        )
      )
    }
  ),

  apgar_edad = list(
    etiqueta = "Apgar según edad",
    numero   = 4,
    titulo   = "Apgar al minuto según la edad de la madre",
    pregunta = paste("Se busca saber si el puntaje Apgar al minuto cambia según",
                     "la edad de la madre, comparando los tres grupos de edad."),

    analizar = function(d) {
      lil <- d |>
        group_by(g = EDAD_MADRE) |>
        group_modify(~ {
          r <- nortest::lillie.test(.x$APGAR1)
          tibble::tibble(n = nrow(.x), D = unname(r$statistic), p = r$p.value)
        }) |>
        ungroup()

      lev   <- car::leveneTest(APGAR1 ~ EDAD_MADRE, data = d, center = "median")
      f_lev <- unname(lev[1, 2])
      p_lev <- unname(lev[1, 3])
      gl    <- paste(lev[1, 1], "y", num(lev[2, 1]))

      kw  <- rstatix::kruskal_test(APGAR1 ~ EDAD_MADRE, data = d)
      eta <- rstatix::kruskal_effsize(APGAR1 ~ EDAD_MADRE, data = d)

      niveles <- levels(d$EDAD_MADRE)
      post <- stats::pairwise.wilcox.test(d$APGAR1, d$EDAD_MADRE,
                                          exact = FALSE,
                                          p.adjust.method = "bonferroni")$p.value
      pares  <- list(niveles[c(1, 2)], niveles[c(1, 3)], niveles[c(2, 3)])
      p_par  <- vapply(pares, function(p) post[p[2], p[1]], 0)

      list(
        pasos = list(
          list(
            titulo  = "Prueba de normalidad",
            h0      = "el puntaje Apgar del grupo sigue una distribución normal",
            h1      = "no la sigue",
            tabla   = tibble::tibble(
              `Grupo de edad` = as.character(lil$g),
              n = num(lil$n),
              D = num(lil$D, 0.001),
              `Valor p` = formato_p(lil$p)
            ),
            lectura = paste0(
              "En los tres grupos se rechaza la normalidad (",
              formato_p(max(lil$p)), "), por lo que se decide usar una prueba ",
              "no paramétrica."
            )
          ),
          list(
            titulo  = "Prueba de homogeneidad de varianzas",
            h0      = "la varianza del puntaje es igual en los tres grupos",
            h1      = "al menos un grupo tiene una varianza distinta",
            tabla   = tibble::tibble(
              F = num(f_lev, 0.01),
              gl = gl,
              `Valor p` = formato_p(p_lev)
            ),
            lectura = paste0(
              "Se rechaza la igualdad de varianzas (", formato_p(p_lev), "), por ",
              "lo que el resultado de la prueba siguiente se interpreta como una ",
              "diferencia en la distribución del puntaje y no estrictamente en ",
              "su mediana."
            )
          ),
          list(
            titulo  = "Prueba de Kruskal-Wallis",
            h0      = "los tres grupos de edad tienen la misma distribución del puntaje",
            h1      = "al menos un grupo tiene una distribución distinta",
            tabla   = tibble::tibble(
              `χ²` = num(kw$statistic, 0.01),
              gl = kw$df,
              `Valor p` = formato_p(kw$p)
            ),
            lectura = paste0(
              "Se rechaza la hipótesis nula (", formato_p(kw$p), "): el puntaje ",
              "Apgar al minuto no se distribuye igual en los tres grupos de edad."
            )
          ),
          list(
            titulo  = "Comparaciones por pares",
            tabla   = tibble::tibble(
              Comparación = vapply(pares, paste, "", collapse = " – "),
              `Valor p ajustado (Bonferroni)` = formato_p(p_par)
            ),
            lectura = paste0(
              "Los nacimientos de madres adolescentes difieren de los de madres ",
              "de 20 a 34 años (", formato_p(p_par[1]), ") y de los de 35 años ",
              "o más (", formato_p(p_par[2]), "), mientras que entre estos dos ",
              "últimos grupos no hay diferencia (", formato_p(p_par[3]), ")."
            ),
            pie = paste(
              "Para identificar qué grupos difieren se compara cada par con la",
              "prueba de Wilcoxon, ajustando los valores p con la corrección de",
              "Bonferroni."
            )
          ),
          list(
            titulo  = "Tamaño del efecto",
            tabla   = tibble::tibble(
              Medida = "Eta cuadrado",
              Valor = num(eta$effsize, 0.001)
            ),
            lectura = paste0(
              "Si bien existe una diferencia, el tamaño del efecto indica que es ",
              "despreciable: el eta cuadrado resultante fue de ",
              num(eta$effsize, 0.001), ", por debajo del 0,01 que se considera ",
              "un efecto pequeño (Cohen, 1988)."
            ),
            pie = "Umbrales: 0,01 pequeño; 0,06 mediano; 0,14 grande."
          )
        ),
        conclusion = paste(
          "De esto se concluye que, aunque los recién nacidos de madres",
          "adolescentes tienen puntajes ligeramente más bajos, el Apgar al",
          "minuto es prácticamente igual en los tres grupos de edad."
        )
      )
    }
  ),

  apgar_tiempo = list(
    etiqueta = "Apgar: minuto y 5 minutos",
    numero   = 5,
    titulo   = "Apgar al minuto y a los cinco minutos",
    pregunta = paste("Se busca saber si los recién nacidos mejoran su condición",
                     "entre el primer y el quinto minuto de vida."),
    detalle  = paste("Como las dos mediciones son del mismo recién nacido, los",
                     "datos son pareados y se trabaja con la diferencia entre el",
                     "puntaje a los cinco minutos y el del primer minuto."),

    analizar = function(d) {
      dif <- d$APGAR2 - d$APGAR1
      lil <- nortest::lillie.test(dif)

      largo <- d |>
        mutate(id = row_number()) |>
        select(id, APGAR1, APGAR2) |>
        tidyr::pivot_longer(c(APGAR2, APGAR1), names_to = "momento",
                            values_to = "apgar") |>
        mutate(momento = factor(momento, levels = c("APGAR2", "APGAR1")))

      wil <- rstatix::wilcox_test(apgar ~ momento, data = largo,
                                  paired = TRUE, alternative = "greater")
      ef  <- r_wilcox_pareado(dif)

      list(
        pasos = list(
          list(
            titulo  = "Prueba de normalidad",
            h0      = "las diferencias entre los dos puntajes siguen una distribución normal",
            h1      = "no la siguen",
            tabla   = tibble::tibble(
              n = num(length(dif)),
              D = num(unname(lil$statistic), 0.001),
              `Valor p` = formato_p(lil$p.value)
            ),
            lectura = paste0(
              "Se rechaza la normalidad de las diferencias (",
              formato_p(lil$p.value), "), por lo que se decide usar la prueba no ",
              "paramétrica de Wilcoxon para muestras pareadas. Al tratarse de ",
              "datos pareados no se requiere igualdad de varianzas."
            )
          ),
          list(
            titulo  = "Prueba de Wilcoxon para muestras pareadas",
            h0      = "la mediana de las diferencias es igual a cero (el puntaje no cambia)",
            h1      = "la mediana de las diferencias es mayor que cero (el puntaje mejora)",
            tabla   = tibble::tibble(
              `Pares analizados` = num(length(dif)),
              `Pares con cambio de puntaje` = num(sum(dif != 0)),
              V = num(wil$statistic),
              `Valor p` = formato_p(wil$p)
            ),
            lectura = paste0(
              "Se rechaza la hipótesis nula (", formato_p(wil$p), "): el puntaje ",
              "Apgar a los cinco minutos es mayor que el del primer minuto."
            )
          ),
          list(
            titulo  = "Tamaño del efecto",
            tabla   = tibble::tibble(
              r = num(ef, 0.001),
              Magnitud = magnitud_r(ef)
            ),
            lectura = paste0(
              "El tamaño del efecto fue ", magnitud_r(ef), " (r = ",
              num(ef, 0.001), ")."
            ),
            pie = "r = Z / √N. Umbrales: 0,10 pequeño; 0,30 mediano; 0,50 grande."
          )
        ),
        conclusion = paste(
          "De esto se concluye que los recién nacidos sí mejoran su condición",
          "entre el primer y el quinto minuto de vida: aunque la mayoría mantiene",
          "su puntaje, casi todos los que cambian lo hacen hacia un valor más alto."
        )
      )
    }
  )
)

# ---- módulo ----------------------------------------------------------------

mod_hipotesis_ui <- function(id) {
  ns <- NS(id)

  # fill = FALSE: cada tarjeta toma su alto natural. Si no, las tarjetas se
  # reparten el alto de la pestaña y las tablas y los textos quedan recortados.
  layout_columns(
    col_widths = 12, fill = FALSE,
    uiOutput(ns("contenido"))
  )
}

# Controles en la franja lateral.
mod_hipotesis_controles <- function(id) {
  ns <- NS(id)

  selectInput(
    ns("objetivo"), "Objetivo",
    choices = setNames(names(objetivos_hipotesis),
                       vapply(objetivos_hipotesis, `[[`, "", "etiqueta"))
  )
}

mod_hipotesis_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resultados <- reactive({
      req(input$objetivo)
      objetivos_hipotesis[[input$objetivo]]$analizar(datos())
    })

    # Un DTOutput por paso (el objetivo con más pasos tiene cinco). La UI solo
    # dibuja los del objetivo elegido; los demás no se llegan a dibujar.
    for (i in seq_len(5)) {
      local({
        k <- i
        output[[paste0("paso", k)]] <- DT::renderDT(server = FALSE, {
          res <- resultados()
          req(length(res$pasos) >= k)
          tabla_dt(res$pasos[[k]]$tabla)
        })
      })
    }

    output$contenido <- renderUI({
      obj <- objetivos_hipotesis[[input$objetivo]]
      res <- resultados()

      pasos <- lapply(seq_along(res$pasos), function(i) {
        paso <- res$pasos[[i]]
        card(
          class = "mb-3", fill = FALSE,
          card_header(paso$titulo),
          # fillable = FALSE: el widget de DT entra como ítem flexible de 400 px
          # y dejaría un hueco bajo la tabla, que solo mide lo que mide.
          card_body(
            fillable = FALSE,
            if (!is.null(paso$h0)) p(class = "mb-1", tags$strong("H₀: "), HTML(paso$h0)),
            if (!is.null(paso$h1)) p(class = "mb-3", tags$strong("H₁: "), HTML(paso$h1)),
            DT::DTOutput(ns(paste0("paso", i))),
            p(class = "mt-3 mb-0", paso$lectura)
          ),
          if (!is.null(paso$pie)) card_footer(class = "text-muted small", paso$pie)
        )
      })

      tagList(
        card(
          class = "mb-3", fill = FALSE,
          card_header(paste0("Objetivo ", obj$numero, ": ", obj$titulo)),
          card_body(
            fillable = FALSE,
            p(class = "lead", obj$pregunta),
            if (!is.null(obj$detalle)) p(class = "mb-0", obj$detalle)
          )
        ),
        pasos,
        card(
          class = "border-start border-4 border-primary", fill = FALSE,
          card_header("Conclusión"),
          card_body(fillable = FALSE, p(class = "mb-0", res$conclusion))
        )
      )
    })
  })
}
