# Preparación del mapa de la app.
# Descarga los límites municipales de Colombia publicados por geoBoundaries
# (tomados del Marco Geoestadístico Nacional del DANE, licencia CC BY 4.0),
# se queda con los 23 municipios del Atlántico, les asigna el código que usa la
# base de nacimientos (CODMUNRE) y guarda el resultado en data/municipios_atl.rds.
#
# Se corre una sola vez, con el directorio de trabajo en app/:
#   source("data/preparacion_mapa.R")

library(tidyverse)
library(sf)

url_geo  <- "https://github.com/wmgeolab/geoBoundaries/raw/9469f09/releaseData/gbOpen/COL/ADM2/geoBoundaries-COL-ADM2_simplified.geojson"
ruta_rds <- "data/municipios_atl.rds"

# Código del municipio en la base (CODMUNRE), nombre oficial y nombre con el
# que aparece en geoBoundaries (el de Barranquilla viene recortado)
municipios_atl <- tribble(
  ~CODMUNRE, ~MUNICIPIO,           ~nombre_geo,
  "001",     "Barranquilla",       "Distrito Especial, Industrial Y Portuario De Barr*",
  "078",     "Baranoa",            "Baranoa",
  "137",     "Campo de la Cruz",   "Campo De La Cruz",
  "141",     "Candelaria",         "Candelaria",
  "296",     "Galapa",             "Galapa",
  "372",     "Juan de Acosta",     "Juan De Acosta",
  "421",     "Luruaco",            "Luruaco",
  "433",     "Malambo",            "Malambo",
  "436",     "Manatí",             "Manatí",
  "520",     "Palmar de Varela",   "Palmar De Varela",
  "549",     "Piojó",              "Piojó",
  "558",     "Polonuevo",          "Polonuevo",
  "560",     "Ponedera",           "Ponedera",
  "573",     "Puerto Colombia",    "Puerto Colombia",
  "606",     "Repelón",            "Repelón",
  "634",     "Sabanagrande",       "Sabanagrande",
  "638",     "Sabanalarga",        "Sabanalarga",
  "675",     "Santa Lucía",        "Santa Lucía",
  "685",     "Santo Tomás",        "Santo Tomás",
  "758",     "Soledad",            "Soledad",
  "770",     "Suan",               "Suan",
  "832",     "Tubará",             "Tubará",
  "849",     "Usiacurí",           "Usiacurí"
)

archivo <- tempfile(fileext = ".geojson")
download.file(url_geo, archivo, mode = "wb", quiet = TRUE)
colombia <- st_read(archivo, quiet = TRUE)

# Algunos nombres se repiten en otros departamentos (Candelaria, Sabanalarga,
# Puerto Colombia): primero se dejan solo los municipios cuyo interior cae en
# una caja que encierra al Atlántico, y luego se cruzan por nombre
centros <- st_coordinates(suppressWarnings(st_point_on_surface(st_geometry(colombia))))
en_atlantico <- between(centros[, "X"], -75.35, -74.65) & between(centros[, "Y"], 10.20, 11.15)

mapa <- colombia[en_atlantico, ] |>
  inner_join(municipios_atl, by = c("shapeName" = "nombre_geo")) |>
  select(CODMUNRE, MUNICIPIO) |>
  st_make_valid()

if (nrow(mapa) != nrow(municipios_atl)) {
  stop("Se esperaban ", nrow(municipios_atl), " municipios y se encontraron ", nrow(mapa),
       ": ", paste(setdiff(municipios_atl$MUNICIPIO, mapa$MUNICIPIO), collapse = ", "))
}

saveRDS(mapa, ruta_rds)

message("Guardado ", ruta_rds, ": ", nrow(mapa), " municipios.")
