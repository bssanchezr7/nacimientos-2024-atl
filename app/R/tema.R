# Tema visual de la app: Bootstrap 5 con la paleta del libro (R/estilo.R del
# bookdown). Para explorar cambios en vivo: bslib::run_with_themer("app")
tema_app <- function() {
  bs_theme(
    version   = 5,
    primary   = "#2E5A6B",
    secondary = "#A9C1CA",
    info      = "#6F97A6",
    success   = "#8AA89A",
    warning   = "#D9B26F",
    danger    = "#C8764A",
    base_font    = font_collection(font_google("Source Sans 3", local = FALSE), "Segoe UI", "Arial", "sans-serif"),
    heading_font = font_collection(font_google("Source Sans 3", local = FALSE), "Segoe UI", "Arial", "sans-serif"),
    "border-radius" = "0.6rem"
  ) |>
    bs_add_rules("
      /* Cajas de valor en tono suave, con su versión para el modo oscuro */
      .bslib-value-box.vb-suave {
        --bslib-value-box-bg: #E8EFF2;
        --bslib-value-box-color: #2E5A6B;
        --bslib-value-box-border-color: #D6E4E9;
      }
      [data-bs-theme='dark'] .bslib-value-box.vb-suave {
        --bslib-value-box-bg: #1F3A44;
        --bslib-value-box-color: #DDE7EB;
        --bslib-value-box-border-color: #2E5A6B;
      }
      /* bslib pinta los íconos con un degradado azul-morado; aquí toman el color de la caja */
      .bslib-value-box.default.vb-suave .value-box-showcase > i[class] {
        background: none;
        -webkit-text-fill-color: currentColor;
      }

      /* Modo oscuro: el azul petróleo no se lee sobre fondo oscuro, así que
         enlaces y pestañas pasan a los tonos claros de la paleta */
      [data-bs-theme='dark'] {
        --bs-link-color: #A9C1CA;
        --bs-link-color-rgb: 169, 193, 202;
        --bs-link-hover-color: #DDE7EB;
        --bs-link-hover-color-rgb: 221, 231, 235;
      }
      [data-bs-theme='dark'] .nav-tabs {
        --bs-nav-tabs-link-active-bg: var(--bs-body-bg);
        --bs-nav-tabs-link-active-color: var(--bs-emphasis-color);
      }

      /* Mapa: la capa de nombres va encima de los municipios, pero no debe
         bloquear el cursor ni los clics sobre ellos */
      .leaflet-etiquetas-pane { pointer-events: none; }

      /* Ícono de ayuda junto a un título */
      .ayuda {
        color: var(--bs-secondary-color);
        cursor: help;
        margin-left: 0.3rem;
        font-size: 0.85em;
      }
    ")
}
