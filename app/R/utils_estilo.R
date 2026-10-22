# Paleta, formatos y tema de ggplot del libro (R/estilo.R).

for (.ruta in c("../R/estilo.R", "R/estilo.R")) {
  if (file.exists(.ruta)) {
    source(.ruta, local = TRUE)
    break
  }
}
rm(.ruta)
