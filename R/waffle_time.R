
#' Represent dates in a waffle plot
#'
#' Based on Isabella Benabaye's code: https://github.com/isabellabenabaye/life-chart
#'
#' Function represents date ranges - or eras - using a waffle plot. 
#' Returns a ggplot object, so can be edited as needed. 
#' Returned with minimal clutter
#' 
#' 
#'
#' @param date_thresholds A vector of class "date". First date is your zero point. Remaining dates upper threshold of era.
#' @param date_cats A vector of class "character". Labels for the eras, length one fewer than date_cats
#' @param nrows  Integer setting the number of rows in your waffle
#' 
#' @return ggplot object 
#' @export
#'
#' @examples
#' date_thresholds <-
#' as.Date(
#'   c(
#'     "1981-01-01",
#'     "2008-09-01",
#'     "2010-09-01",
#'     "2012-10-01",
#'     "2016-11-01",
#'     "2017-03-01",
#'     "2020-02-01",
#'     "2021-01-01"
#'   )
#' )
#' date_cats <- c("era1", "era2", "era3", "era4", "era5", "era6", "era7")
#'waffle_time(date_thresholds, date_cats, 12) 

waffle_time <- function(date_thresholds, date_cats, nrows) {
  
  # Create the data-----
  life_data <-
    tibble::tibble(date = seq.Date(
      from = as.Date("1987-01-01"),
      to = as.Date("2087-12-01"),
      by = "month"
    ))
  life_data <- tibble::rowid_to_column(life_data, "row_name")
  
  # Use cut to break the time period into classifications
  threshold_months <-
    sapply(date_thresholds, function(x)
      lubridate::interval(date_thresholds[1], x) %/% months(1))
  
  life_data <-
    dplyr::mutate_at(life_data,
                     dplyr::vars(era = row_name),
                     cut,
                     breaks = threshold_months,
                     labels  = date_cats)
  
  # # Waffle chart-----
  life_in_months <-
    dplyr::count(life_data, era) ## the count of each era is the number of months in that era
  
 waffle_plot <- 
   ggplot2::ggplot(life_in_months, ggplot2::aes(fill = era, values = n)) +
    waffle::geom_waffle(
      color = "#F7F7F7",
      n_rows = nrows,
      size = 1,
      flip = FALSE
    ) + ## make each row a year/12 months
    ggplot2::coord_equal() +
    ggplot2::labs (NULL) +
    ggplot2::theme(
      legend.position =  "none",
      rect = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    ) +
    waffle::theme_enhance_waffle()
  
  return(waffle_plot)
  #scale_fill_manual(name = "", values = c("#EF476F","#FCA311","#FFD166","#0EAD69","#4ECDC4","#118AB2")) +  ## assign colors to the eras
  
}
