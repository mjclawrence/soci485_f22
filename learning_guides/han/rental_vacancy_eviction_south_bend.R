library(tidyverse)
library(tidycensus)
library(DT)

han <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/han_data_complete_pop.csv")
zip_codes <- han$GEOID


v19 <- load_variables(2019, "acs5")

# rental vacancy rate = 
# vacant year-round units for rent /
# renter occupied units + vacant year-round units rented but awaiting occupancy + vacant year-round units for rent

# B25004_002 /
# (B25003_003 + B25004_003 + B25004_002)

housing <- get_acs(geography = "zcta",
                           zcta = zip_codes,
                           state = c("NY", "PA", "TX", "IN", "AZ"), 
                   year = 2019,
                   survey = "acs5",
                   variables = c(units_for_rent = "B25004_002",
                                 renter_occupied_units = "B25003_003",
                                 rented_awaiting_occupancy = "B25004_003"),
                   output = "wide") |> 
  mutate(rental_vacancy_rate = (units_for_rentE / (renter_occupied_unitsE + rented_awaiting_occupancyE + units_for_rentE))*100) |> 
  select(GEOID, rental_vacancy_rate) |> 
  mutate(GEOID = as.numeric(GEOID))

han <- left_join(han, housing)

library(weights)
wtd.cor(han$rental_vacancy_rate, han$evict_pct, weight = han$population)

han |> 
  filter(xsite == "SOUTH BEND") |> 
  summarise(cor = wtd.cor(rental_vacancy_rate, evict_pct, population))

han |> 
  filter(xsite == "SOUTH BEND") |> 
  mutate(black_pct_cat = ifelse(black_pct<2, "Less Than Two % Black",
                                ifelse(black_pct >= 2 & black_pct <= 10, "Two - Ten % Black",
                                       ifelse(black_pct >10 & black_pct<=20, "Ten - Twenty % Black",
                                              "Greater Than Twenty % Black")))) |> 
  select(GEOID, black_pct_cat, black_pct, rental_vacancy_rate, evict_pct, median_hh_income) |> 
  datatable(rownames = FALSE,
            extensions = 'Buttons', options = list(
              dom = 'Btip', # see explanation below
              buttons = list(list(extend = 'copy', title = NULL))))


  ggplot(aes(x = rental_vacancy_rate, 
             y = evict_pct,
             size = black_pct,
             color = median_hh_income)) +
  geom_point() + geom_smooth(method = "lm") + facet_wrap(~xsite, scales = "free") +
  scale_color_viridis_c(option = "magma")
