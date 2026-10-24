library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)

if (interactive()) options(shiny.autoreload = TRUE)
if (file.exists(".Renviron")) readRenviron(".Renviron")
clave_carto <- Sys.getenv("CARTO_API_KEY")

mostrar_simulacion <- FALSE

datos      <- readRDS("data/nacimientos_atl.rds")
municipios <- readRDS("data/municipios_atl.rds")
