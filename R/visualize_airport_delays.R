#'@title Visualize ariport Flight delays
#'@name  visualize_airport_delays
#'@description In this you can see that which flight is delay at what is its mean by its lat lng
#'@field no argument
#'@export

visualize_airport_delays <- function()
{

  flights <- nycflights13::flights
  airports <- nycflights13::airports

  #qyery to combine data
  mean_data <- dplyr::summarise(dplyr::group_by(flights, dest), M = mean(arr_delay))
  #group_by data by dest
  library(dplyr)

  #get lat and lng
  combine_data <- inner_join(airports,mean_data, by = c("faa" =  "dest"))

  library(ggplot2)
  p<- ggplot(combine_data, aes(x=combine_data$lat, y=combine_data$lon)) +
    geom_point(na.rm = TRUE) + theme_gray() +
    labs(title="Average Flight Delays",subtitle="Longitude vs. Latitude",
         x="Latitude", y="Longitude")+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
  return(p)

}

# visualize_airport_delays()
