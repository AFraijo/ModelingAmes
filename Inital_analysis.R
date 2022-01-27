## Initial Set up
options(scipen = 8)
set.seed(8675309)
library(dplyr)
library(ggplot2)

## I like the black and white theme
theme_set(theme_bw())

## Initial data exploration
data(ames, package = "modeldata")

## Tidymodels book uses 50 bins
ggplot(ames, aes(x = Sale_Price)) + 
     geom_histogram(bins = 100) + 
     labs(title = "Histogram of Houses Sold",
          x = "Sales Price",
          y = "Number of Houses Sold")

## Log-Scale, don't need this for the initial modeling
## will be useful for second set of models
ggplot(ames, aes(x = Sale_Price)) + 
     geom_histogram(bins = 100) + 
     scale_x_log10() +
     labs(title = "Histogram of Houses Sold",
          x = "Log Base 10 of Sales Price",
          y = "Number of Houses Sold")

## Map
library(ggmap)


OSM_Ames <- get_stamenmap(
     bbox = c(left = -93.6984000, bottom = 41.9864000,
              right = -93.5687000, top = 42.0675000),
     zoom = 15,
     maptype = "toner")

saveRDS(OSM_Ames, "Stamen_Ames.rds") ##Save for later
##OSM_Ames <- readRDS("Stamen_Ames.rds")

ggmap(OSM_Ames) + 
     geom_point(data = ames,
                aes(x = Longitude, y = Latitude, col = Neighborhood),
                size = 1.5, alpha = .2) + 
     theme(legend.position = "bottom", 
           legend.title = element_blank(),
           rect = element_blank(),
           axis.ticks = element_blank(),
           axis.text.x = element_blank(), 
           axis.text.y = element_blank()) + 
     labs(title = "Map of Houses Sold in Ames, IA",
          x = element_blank(),
          y = element_blank())


ames %>% group_by(Neighborhood) %>% 
     summarise(Ave_Price = mean(Sale_Price)) %>% 
     knitr::kable()

