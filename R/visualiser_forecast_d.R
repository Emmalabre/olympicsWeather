#' Visualisation de la météo par jours
#'
#' Cette fonction permet de visualiser les données de prévisions météorologiques par jours en fonction de la latitude et de la longitude ou d'une adresse.
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
#' visualiser_forecast_d(42.35, 2.65)
#' visualiser_forecast_d("Paris")

visualiser_forecast_d <- function(x, y = NULL) {
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

  data_long$date_heure <- as.POSIXct(data_long$date_heure, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  data_long$date <- as.Date(data_long$date_heure)
  data_long$day_of_week <- weekdays(data_long$date)

  data_aggregated <- data_long |>
    group_by(date, day_of_week, variable) |>
    summarise(value = mean(value, na.rm = TRUE))

  ggplot2::ggplot(data_aggregated, aes(x = paste(date, day_of_week), y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::geom_text(aes(label = round(value, 1)), position = position_stack(vjust = 0.5), size = 3) +
    ggplot2::labs(title = "Prévisions Météo",
                  x = "Date et Jour",
                  y = "Valeur") +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


