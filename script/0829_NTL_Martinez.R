library("tidyverse")
gdp <- read_csv("data/WDI_2014_11.csv")
head(gdp) 
df_gdp <- gdp %>% 
  filter(`Indicator Name` == "GDP (constant LCU)") %>% 
  select(`Country Name`, "1992":"2013") %>% 
  pivot_longer(cols = "1992":"2013", names_to = "year", values_to = "GDP")
  # drop_na(GDP)

fiw <- read_csv("data/FIW.csv")
head(fiw)
df_fiw <- fiw %>% 
  mutate()

