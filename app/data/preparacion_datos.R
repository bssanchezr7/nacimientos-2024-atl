# Preparación de los datos de la app.
# Replica la depuración del capítulo 2 del libro (02-datos.Rmd) y guarda el
# resultado en data/nacimientos_atl.rds
# Se corre una sola vez (o cuando cambie la depuración), con el directorio de
# trabajo en app/:
#   source("data/preparacion_datos.R")
#
# Requiere el CSV del DANE en la carpeta data/ de la raíz del repositorio

library(tidyverse)

ruta_csv <- "../data/BD-EEVV-Nacimientos-2024.csv"
ruta_rds <- "data/nacimientos_atl.rds"

if (!file.exists(ruta_csv)) {
  stop("No se encontró ", normalizePath(ruta_csv, mustWork = FALSE),
       ".\nColoque el CSV del DANE en la carpeta data/ de la raíz del ",
       "repositorio y corra este script con el directorio de trabajo en app/.")
}

# CARGA ----------------------------------------------------------------------
# Solo las columnas que se usan. Todo se lee como texto para conservar los
# códigos con ceros a la izquierda ("08", "001"); las conversiones van abajo.
df <- read_csv(ruta_csv,
               locale = locale(encoding = "ISO-8859-1"),
               col_select = c(CODPTORE, APGAR1, APGAR2, TIPO_PARTO, SEG_SOCIAL,
                              NUMCONSUL, CODMUNRE, EDAD_MADRE),
               col_types = cols(.default = col_character()))

# POBLACIÓN: madres residentes en el Atlántico --------------------------------
df_atl <- df |>
  filter(CODPTORE == "08") |>
  select(APGAR1, APGAR2, TIPO_PARTO, SEG_SOCIAL, NUMCONSUL, CODMUNRE, EDAD_MADRE)

# LIMPIEZA: códigos a categorías y 99 como faltante ---------------------------
df_atl <- df_atl |>
  mutate(
    across(c(APGAR1, APGAR2, NUMCONSUL),
           ~ na_if(as.integer(.x), 99L)),
    TIPO_PARTO = factor(as.integer(TIPO_PARTO),
                        levels = 1:4,
                        labels = c("Espontáneo", "Cesárea", "Instrumentado", "Ignorado")),
    SEG_SOCIAL = factor(as.integer(SEG_SOCIAL),
                        levels = 1:5,
                        labels = c("Contributivo", "Subsidiado", "Excepción",
                                   "Especial", "No asegurado")),
    EDAD_MADRE = na_if(as.integer(EDAD_MADRE), 99L),
    # Los nueve grupos quinquenales del DANE se agrupan en tres
    EDAD_MADRE = factor(EDAD_MADRE,
                        levels = 1:9,
                        labels = c("Adolescente",        # 1: 10-14 años
                                   "Adolescente",        # 2: 15-19 años
                                   "De 20 a 34 años",    # 3: 20-24 años
                                   "De 20 a 34 años",    # 4: 25-29 años
                                   "De 20 a 34 años",    # 5: 30-34 años
                                   "De 35 años o más",   # 6: 35-39 años
                                   "De 35 años o más",   # 7: 40-44 años
                                   "De 35 años o más",   # 8: 45-49 años
                                   "De 35 años o más"))  # 9: 50-54 años
  )

# FALTANTES: se eliminan (menos del 5 % de los registros) ---------------------
n_antes <- nrow(df_atl)
df_atl <- df_atl |> drop_na()

# VARIABLES CONSTRUIDAS -------------------------------------------------------
df_atl <- df_atl |>
  mutate(
    SUBREGION = case_when(
      CODMUNRE == "001"                    ~ "Barranquilla",
      CODMUNRE == "758"                    ~ "Soledad",
      CODMUNRE %in% c("296", "433", "573") ~ "Resto del área metropolitana",
      TRUE                                 ~ "Resto del departamento"
    ),
    SUBREGION = factor(SUBREGION,
                       levels = c("Barranquilla", "Soledad",
                                  "Resto del área metropolitana",
                                  "Resto del departamento")),
    CESAREA = factor(if_else(TIPO_PARTO == "Cesárea", "Sí", "No"),
                     levels = c("Sí", "No"))
  )

# GUARDADO -------------------------------------------------------------------
# Se guardan también los conteos de la depuración para la pestaña de metodología
attr(df_atl, "depuracion") <- list(
  n_nacional  = nrow(df),
  n_atlantico = n_antes,
  n_final     = nrow(df_atl)
)

saveRDS(df_atl, ruta_rds)

message("Guardado ", ruta_rds, ": ", nrow(df_atl), " registros (",
        n_antes - nrow(df_atl), " eliminados por faltantes de ", n_antes, ").")
