install.packages("pak")
pak::pak("syunhong/seg")
pak::pak("pablogguz/ineAtlas")

library(tidyverse)
library(sf)
library(ineAtlas)
library(seg)

# fixing issues
seg_fixed <- new.env(parent = asNamespace("seg"))
seg_fixed$CRS <- sf::st_crs

for (name in c("spseg", "localenv")){
  fun <- get(name, envir = asNamespace("seg"))
  environment(fun) <- seg_fixed
  seg_fixed[[name]] <- fun
}

# fixing issue n.2: useC = FALSE

options(scipen = 999)

City <- "08019"
District <- "01"
Year <- 2022

scales <- c(250, 500)

income <- get_atlas("distribution_sex", "tract")
population <- get_atlas("demographics", "tract")


income <- income %>%
  filter(mun_code == City,
         year == Year,
         sex == "total")

population <- population %>%
  filter(mun_code == City,
         year == Year) %>%
  select(tract_code, population)

head(population)

# join income and population by census tract

data <- left_join(income, population,
                  by = "tract_code")

shares <- data %>%
  transmute(
    tract_code, population,
    below_5000 = equivinc_below_5000/100,
    below_7500 = equivinc_below_7500/100,
    below_10000 = equivinc_below_10000/100,
    below_40_median = equivinc_below_40p_median/100,
    below_50_median = equivinc_below_50p_median/100,
    below_60_median = equivinc_below_60p_median/100,
    below_140_median = 1 - equivinc_above_140p_median / 100,
    below_160_median = 1 - equivinc_above_160p_median / 100,
    below_200_median = 1 - equivinc_above_200p_median / 100
    
  )

tracts <- get_tract_geom(Year) |>
  filter(substr(tract_code, 1, 5) == City)

spatial_data <- inner_join(tracts, shares,
                           by = "tract_code")

spatial_data <- spatial_data |>
  st_make_valid() |>
  st_transform(25831)

ggplot() +
  geom_sf(data = spatial_data) +
  theme_minimal()

spatial_data <- spatial_data %>%
  mutate(district = substr(tract_code, 6, 7))


study_area <- spatial_data %>%
  filter(district == District)


# from polygons to points

points <- st_point_on_surface(st_geometry(study_area))
coords <- st_coordinates(points) 

area_shares <- st_drop_geometry(study_area)
thresholds <- names(area_shares)[grepl("^below_",names(area_shares))]


# Calculate spatial Inf theory Indices (SITI)
SITI_results <- data.frame()

for (scale in scales) {
  for (threshold in thresholds) {
    
    database <- data.frame(
      Below = area_shares$population * area_shares[[threshold]],
      Above = area_shares$population * (1 - area_shares[[threshold]])
    )
    
    if (p > 0 && p < 1) {
      message("Scale ", scale, "m, ", threshold, " ...")
      
      
      p <- sum(database$Below) / sum(area_shares$population)
      
      SITI_result <- seg_fixed$spseg(
        x = coords,
        data = database,
        method = "information",
        smoothing = "kernel",
        sigma = scale,
        useC = FALSE, # see issue fix 2
        negative.rm = FALSE
      )
      
      SITI_results <- rbind(SITI_results, data.frame(
        Threshold = threshold,
        Group = p,
        Scale = scale,
        Value = SITI_result@h
      ))
    }
  }
}

SITI_results


ggplot(
  SITI_results, aes(Group, Value,
                    colour = factor(Scale))) +
  geom_line() +
  geom_point() +
  labs(x = "Population rank p (share of people below threshold)",
       y = "Spatial information theory index H",
       colour = "Scale (m)") +
  theme_minimal()
