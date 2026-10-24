# Motor de la simulación de Monte Carlo (pestaña "Simulación").
# Para cada objetivo se define la población que se muestrea y una prueba que
# devuelve el valor p y el tamaño del efecto. La simulación toma muchas
# muestras aleatorias de un tamaño dado y repite la prueba en cada una.

# Z de Mann-Whitney con corrección por empates, a partir del estadístico U
z_mann_whitney <- function(u, x, n1, n2) {
  N <- n1 + n2
  t <- table(x)
  (u - n1 * n2 / 2) / sqrt(n1 * n2 / 12 * (N + 1 - sum(t^3 - t) / (N * (N - 1))))
}

objetivos_simulacion <- list(

  consultas_edad = list(
    etiqueta = "1. Consultas: adolescentes frente a 20 a 34 años",
    prueba   = "Wilcoxon–Mann–Whitney",
    medida   = "r",
    magnitud = function(e) magnitud_r(e),
    poblacion = function(d) {
      d |>
        filter(EDAD_MADRE %in% c("Adolescente", "De 20 a 34 años")) |>
        mutate(g = droplevels(EDAD_MADRE)) |>
        select(x = NUMCONSUL, g)
    },
    probar = function(m) {
      n1 <- sum(m$g == "Adolescente"); n2 <- nrow(m) - n1
      if (n1 < 2 || n2 < 2) return(c(p = NA, efecto = NA))
      w <- suppressWarnings(wilcox.test(x ~ g, data = m, exact = FALSE))
      c(p = w$p.value,
        efecto = abs(z_mann_whitney(unname(w$statistic), m$x, n1, n2)) / sqrt(nrow(m)))
    }
  ),

  cesarea_regimen = list(
    etiqueta = "2. Cesáreas: contributivo frente a subsidiado",
    prueba   = "chi-cuadrado de homogeneidad",
    medida   = "V de Cramér",
    magnitud = function(e) magnitud_r(e),
    poblacion = function(d) {
      d |>
        filter(SEG_SOCIAL %in% c("Contributivo", "Subsidiado")) |>
        transmute(g = droplevels(SEG_SOCIAL), x = CESAREA)
    },
    probar = function(m) {
      tab <- table(m$g, m$x)
      if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) return(c(p = NA, efecto = NA))
      chi <- suppressWarnings(chisq.test(tab))
      c(p = chi$p.value, efecto = sqrt(unname(chi$statistic) / nrow(m)))
    }
  ),

  consultas_subregion = list(
    etiqueta = "3. Consultas según la subregión",
    prueba   = "Kruskal-Wallis",
    medida   = "eta cuadrado",
    magnitud = function(e) magnitud_eta(e),
    poblacion = function(d) d |> select(x = NUMCONSUL, g = SUBREGION),
    probar = function(m) probar_kruskal(m)
  ),

  apgar_edad = list(
    etiqueta = "4. Apgar al minuto según la edad",
    prueba   = "Kruskal-Wallis",
    medida   = "eta cuadrado",
    magnitud = function(e) magnitud_eta(e),
    poblacion = function(d) d |> select(x = APGAR1, g = EDAD_MADRE),
    probar = function(m) probar_kruskal(m)
  ),

  apgar_tiempo = list(
    etiqueta = "5. Apgar al minuto y a los cinco minutos",
    prueba   = "Wilcoxon pareada",
    medida   = "r",
    magnitud = function(e) magnitud_r(e),
    poblacion = function(d) d |> transmute(dif = APGAR2 - APGAR1),
    probar = function(m) {
      if (all(m$dif == 0)) return(c(p = NA, efecto = NA))
      w <- suppressWarnings(wilcox.test(m$dif, alternative = "greater", exact = FALSE))
      c(p = w$p.value, efecto = r_wilcox_pareado(m$dif))
    }
  )
)

# Kruskal-Wallis con eta cuadrado = (H − k + 1) / (n − k), como rstatix
probar_kruskal <- function(m) {
  g <- droplevels(as.factor(m$g))
  k <- nlevels(g)
  if (k < 2) return(c(p = NA, efecto = NA))
  kw <- kruskal.test(m$x, g)
  c(p = kw$p.value, efecto = (unname(kw$statistic) - k + 1) / (nrow(m) - k))
}

# Repite la prueba en `repeticiones` muestras aleatorias simples (sin
# reemplazo) de tamaño `n` tomadas de la población del objetivo.
simular <- function(objetivo, poblacion, n, repeticiones, semilla = 2024) {
  set.seed(semilla)
  n <- min(n, nrow(poblacion))
  res <- vapply(seq_len(repeticiones), function(i) {
    objetivo$probar(poblacion[sample.int(nrow(poblacion), n), , drop = FALSE])
  }, c(p = 0, efecto = 0))
  tibble::tibble(p = res["p", ], efecto = res["efecto", ])
}
