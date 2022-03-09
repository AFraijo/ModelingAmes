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


##OSM_Ames <- get_stamenmap(
##     bbox = c(left = -93.6984000, bottom = 41.9864000,
##              right = -93.5687000, top = 42.0675000),
##     zoom = 15,
##     maptype = "toner")

##saveRDS(OSM_Ames, "Stamen_Ames.rds") ##Save for later
OSM_Ames <- readRDS("Stamen_Ames.rds")

##Saved both images as 837 X 792
##Create map of houses sold colored by neighborhood
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

##Create map of houses sold color by price
ggmap(OSM_Ames) + 
        geom_point(data = ames,
                   aes(x = Longitude, y = Latitude, col = Sale_Price),
                   size = 1.5, alpha = .5) + 
        scale_color_viridis_c() +
        theme(legend.position = "bottom", 
              legend.title = element_blank(),
              rect = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(),
              legend.key.width = unit(2,"cm")) + 
        labs(title = "Map of Houses Sold in Ames, IA",
             x = element_blank(),
             y = element_blank())

## Average cost of a house by neighborhood
ames %>% group_by(Neighborhood) %>% 
        summarise(Ave_Price = mean(Sale_Price)) %>% 
        arrange(desc(Ave_Price)) %>% 
        knitr::kable()

## We don't need every variable, so let's do some feature engineering

## Bathrooms, while it is likely basement versus non-basement matters at the 
## margins, let's not worry about that
## Additionally, let's assume what matters is the time since a renovation happened
## We will only consider the total SF of finish indoor area
ames <- ames %>% mutate(Full_Bath = Bsmt_Full_Bath + Full_Bath,
                        Half_Bath = Bsmt_Half_Bath + Half_Bath,
                        Yrs_since_Reno = Year_Sold - Year_Remod_Add,
                        Size = Gr_Liv_Area + BsmtFin_SF_1 + BsmtFin_SF_2)

## per the notes for the data, let's drop the houses with more than 4000SF 
## living area, as they are anomolous
ames <- ames %>% filter(Gr_Liv_Area < 4000)

## Let use the Variables we created, plus some others note:
## 1. zoning an neighborhood are going to be related, so let's only use neighborhood
## 2. Garage Cars will tell us enough info about if it exists/size
## 3. Central air is important
ames <- ames %>% select(Sale_Price, Neighborhood, Yrs_since_Reno, Full_Bath, Half_Bath,
                        Bldg_Type, Bedroom_AbvGr, Lot_Area, Overall_Cond, Size,
                        Central_Air, Garage_Cars)


## Set up test and training data
library(tidymodels)

## I am gonna assume I don't need to log10 for this. That could be wrong
## Also, I am going to use Neighborhood as strata instead of price

Split <- initial_split(ames, prop = .75, strata = Neighborhood)
Train <- training(Split)
Test <- testing(Split)

## If I understand this correctly, I don't need to preprocess for Ranger
## as long as I am using a workflow (might be a good idea to do it yourself in
## important work)

## set up model in tidyr, I think this works
## Specification
RF_spec <- rand_forest(mode = "regression",
                       mtry = tune(),
                       trees = 1000,
                       min_n = tune()) %>% 
           set_engine("ranger", regularization.factor = tune("regularization"))

## Workflow
RF_workflow <- workflow() %>% 
                add_formula(Sale_Price ~.) %>% 
                add_model(RF_spec)

## Let's try tuning the hyperparameters
ames_folds <- vfold_cv(Train)

doParallel::registerDoParallel()

tune_RF <- tune_grid(
        RF_workflow,
        resamples = ames_folds,
        grid = 20
)

tune_RF %>% collect_metrics() %>% filter(.metric == "rmse") %>% 
        select(mean, std_err, mtry, min_n, regularization) %>% 
        pivot_longer(mtry:regularization,
                     values_to = "value",
                     names_to = "parameter"
        ) %>%
        ggplot(aes(value, mean, color = parameter)) +
        geom_point(show.legend = FALSE) +
        facet_wrap(~parameter, scales = "free_x") +
        labs(x = NULL, y = "RMSE")


