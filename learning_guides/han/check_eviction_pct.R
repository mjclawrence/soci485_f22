library(tidyverse)

phx_tracts_weekly <- read_csv("https://evictionlab.org/uploads/phoenix_weekly_2020_2021.csv")

# March 15, 2020 = Week 12
# June 1, 2021 = Week 75


zip_85035_weekly <- phx_tracts_weekly |> 
  filter(GEOID %in% c("04013112402",
         "04013112401",
         "04013112509",
         "04013109900",
         "04013112302",
         "04013112301",
         "04013082028",
         "04013112507",
         "04013112503",
         "04013112504",
         "04013112505",
         "04013112502")) |> 
  filter(week %in% 12:75)

sum(zip_85035_weekly$filings_2020)

# That doesn't look right. 
# Eviction count is much lower than the 2871 which original file shows.

# What date ranges look closer?
zip_85035_weekly <- phx_tracts_weekly |> 
  filter(GEOID %in% c("04013112402",
                      "04013112401",
                      "04013112509",
                      "04013109900",
                      "04013112302",
                      "04013112301",
                      "04013082028",
                      "04013112507",
                      "04013112503",
                      "04013112504",
                      "04013112505",
                      "04013112502")) |> 
  filter(week %in% 1:75)

sum(zip_85035_weekly$filings_2020)

### Check with another Phoenix zip code

### 85017 count in original data is 1827

zip_85017_weekly <- phx_tracts_weekly |> 
  filter(GEOID %in% c("04013106801",
                      "04013107201",
                      "04013107300",
                      "04013109001",
                      "04013107102",
                      "04013112100",
                      "04013107101",
                      "04013107000",
                      "04013109102",
                      "04013109101",
                      "04013109200",
                      "04013110100",
                      "04013107202",
                      "04013106900",
                      "04013116900")) |> 
  filter(week %in% 3:75)

sum(zip_85017_weekly$filings_2020)


## Monthly

phx_tracts_monthly <- read_csv("https://evictionlab.org/uploads/phoenix_monthly_2020_2021.csv")

zip_85017_monthly <- tracts_monthly |> 
  filter(GEOID %in% c("04013106801",
                      "04013107201",
                      "04013107300",
                      "04013109001",
                      "04013107102",
                      "04013112100",
                      "04013107101",
                      "04013107000",
                      "04013109102",
                      "04013109101",
                      "04013109200",
                      "04013110100",
                      "04013107202",
                      "04013106900",
                      "04013116900")) |> 
  filter(month %in% c("01/2020",
                      "02/2020",
                      "03/2020",
                      "04/2020",
                      "05/2020",
                      "06/2020",
                      "07/2020",
                      "08/2020",
                      "09/2020",
                      "10/2020",
                      "11/2020",
                      "12/2020",
                      "01/2021",
                      "02/2021",
                      "03/2021",
                      "04/2021",
                      "05/2021"))
sum(zip_85017_monthly$filings_2020)

zip_85035_monthly <- phx_tracts_monthly |> 
  filter(GEOID %in% c("04013112402",
                      "04013112401",
                      "04013112509",
                      "04013109900",
                      "04013112302",
                      "04013112301",
                      "04013082028",
                      "04013112507",
                      "04013112503",
                      "04013112504",
                      "04013112505",
                      "04013112502")) |> 
  filter(month %in% c("01/2020",
                      "02/2020",
                      "03/2020",
                      "04/2020",
                      "05/2020",
                      "06/2020",
                      "07/2020",
                      "08/2020",
                      "09/2020",
                      "10/2020",
                      "11/2020",
                      "12/2020",
                      "01/2021",
                      "02/2021",
                      "03/2021",
                      "04/2021",
                      "05/2021",
                      "06/2021"))
sum(zip_85035_monthly$filings_2020)


## Check Indianapolis
ind_tracts_monthly <- read_csv("https://evictionlab.org/uploads/indianapolis_monthly_2020_2021.csv")

zip_46214_monthly <- ind_tracts_monthly |> 
  filter(GEOID %in% c("18097340112",
                      "18097340114",
                      "18097340113",
                      "18097340101",
                      "18097340110",
                      "18097341902",
                      "18097340901",
                      "18097340109",
                      "18097340111",
                      "18097340102")) |> 
  filter(month %in% c("03/2020",
                      "04/2020",
                      "05/2020",
                      "06/2020",
                      "07/2020",
                      "08/2020",
                      "09/2020",
                      "10/2020",
                      "11/2020",
                      "12/2020",
                      "01/2021",
                      "02/2021",
                      "03/2021",
                      "04/2021",
                      "05/2021",
                      "06/2021"))
sum(zip_46214_monthly$filings_2020)

ind_tracts_weekly <- read_csv("https://evictionlab.org/uploads/indianapolis_weekly_2020_2021.csv")

zip_46214_weekly <- ind_tracts_weekly |> 
  filter(GEOID %in% c("18097340112",
                      "18097340114",
                      "18097340113",
                      "18097340101",
                      "18097340110",
                      "18097341902",
                      "18097340901",
                      "18097340109",
                      "18097340111",
                      "18097340102")) |> 
  filter(week %in% c(8:76))
sum(zip_46214_weekly$filings_2020)
