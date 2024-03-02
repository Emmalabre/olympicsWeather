#' Visualisation de la météo par heures
#'
#' Cette fonction permet de visualiser les données de prévisions météorologiques par heures en fonction de la latitude et de la longitude ou d'une adresse.
#'
#' @param x Latitude (numérique) ou adresse (caractère).
#' @param y Longitude (numérique). Obligatoire uniquement si x est la latitude.
#'
#' @return des graphiques ggplot.
#'
#' @export
#' @import tibble
#' @import jsonlite
#' @import httr2
#' @import tidygeocoder
#' @import dplyr
#' @import devtools
#' @import ggplot2
#' @import tidyr
#'
#' @examples
#' visualiser_forecast_h(42.35, 2.65)
#' visualiser_forecast_h("Paris")

visualiser_forecast_h <- function(x, y = NULL) {
  get_gps_coordonnee <- function(adresse) {
    adresse_tibble <- tibble::tribble(
      ~addr,
      adresse
    ) |>
      tidygeocoder::geocode(addr)

    lat <- adresse_tibble$lat
    long <- adresse_tibble$long

    coordonnees <- c(lat, long)
    return(coordonnees)
  }

  if (is.numeric(x) && is.numeric(y)) {

    data <- get_forecast(x, y)
  } else if (is.character(x)) {

    coordonnees <- get_gps_coordonnee(x)
    data <- get_forecast(coordonnees[1], coordonnees[2])
  } else {
    stop("Les arguments ne sont ni des coordonnées numériques ni une adresse valide.")
  }

  data_long <- tidyr::pivot_longer(data, cols = c(temperature_celsius, apparent_temperature_celsius, precipitation_proba, precipitation), names_to = "variable", values_to = "value")

  indices_a_afficher <- c(1, 97, 194, 291, 388, 481, 577)
  breaks_labels <- data_long |>
    dplyr::slice(indices_a_afficher) |>
    dplyr::pull(date_heure)

  ggplot2::ggplot(data_long, ggplot2::aes(x = date_heure, y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::labs(title = "Prévisions Météo",
                  x = "Date et Heure",
                  y = "Valeur") +
    ggplot2::scale_x_discrete(breaks = breaks_labels, labels = breaks_labels) +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal()
}


