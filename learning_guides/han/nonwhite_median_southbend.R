library(tidyverse)
library(weights)


han <- read_csv("https://raw.githubusercontent.com/mjclawrence/soci485_f22/master/data/han_data_complete_pop.csv")




han |> 
  filter(xsite=="SOUTH BEND") |> 
  cor(evict_pct, white_pct)

sb <- han |> 
  filter(xsite == "SOUTH BEND")

 
wtd.quantile(sb$nonwhite_pct, sb$population, probs = c(0.25, 0.5, 0.75))
summary(sb$nonwhite_pct)



wtd.cor(sb$evict_pct, sb$nonwhite_pct, weight = sb$population)
wtd.cor(sb$evict_pct, sb$black_pct, weight = sb$population)
wtd.cor(sb$evict_pct, sb$hisp_pct, weight = sb$population)
wtd.cor(sb$evict_pct, sb$other_pct, weight = sb$population)
wtd.cor(sb$evict_pct, sb$asian_pct, weight = sb$population)

sb |> 
  #filter(nonwhite_pct > 15) |> 
  ggplot(aes(x = nonwhite_pct, y = evict_pct, size = hisp_pct, group = hisp_pct)) + 
  geom_point() +
  #geom_smooth(method = "lm") + 
  #scale_size_continuous(labels = dollar_format()) + 
  theme(legend.position = "bottom")


sb |> 
  mutate(median_nonwhite = ifelse(nonwhite_pct>14.9 & nonwhite_pct<50, "Nonwhite Pct Above Median",
                                  ifelse(nonwhite_pct>=50, "Nonwhite Pct Above 50%",
                                  "Nonwhite Pct Below Median"))) |> 
  select(GEOID, evict_pct, nonwhite_pct, median_nonwhite, white_pct, black_pct, hisp_pct, asian_pct, other_pct) |> 
  datatable(escape = TRUE,
            rownames = FALSE,
            extensions = 'Buttons', options = list(
              dom = 'Btip', # see explanation below
              buttons = list(list(extend = 'copy', title = NULL))))
  

