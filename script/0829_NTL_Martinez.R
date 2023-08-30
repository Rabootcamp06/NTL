library(tidyverse)

#GDP データ加工
df_gdp <- gdp %>% 
  filter(`Indicator Name` == "GDP (constant LCU)") %>% 
  select(`Country Name`, "1992":"2013") %>% 
  pivot_longer(cols = "1992":"2013", names_to = "year", values_to = "GDP")
  # drop_na(GDP)

# 政治的自由　データ加工
fiw <- read_csv("data/FIW.csv")
head(fiw)
df_fiw <- fiw
name_list <- names(df_fiw) %>% 
  sort()

# cl, pr, statusの変数ごとにdf抽出
df_Cl <- fiw %>% 
  select(all_of(name_list)) %>% 
  select(cntry_name,"cl_1992":"cl_2013") %>%
  pivot_longer(cols = "cl_1992":"cl_2013", names_to = "year", values_to = "Cl") %>% 
  mutate(year =  as.numeric(str_replace_all(df_Cl$year, pattern = "cl_", replacement = "")))  
  
# df_Cl2 <- str_replace_all(df_Cl$year, pattern = "cl_", replacement = " ") str_replace_allはベクトルを返す

df_Pr <- fiw %>% 
  select(all_of(name_list)) %>% 
  select(cntry_name,"pr_1992":"pr_2013") %>%
  pivot_longer(cols = "pr_1992":"pr_2013", names_to = "year", values_to = "Pr") %>% 
  mutate(year =  as.numeric(str_replace_all(df_Pr$year, pattern = "pr_", replacement = "")))

df_Status <- fiw %>% 
  select(all_of(name_list)) %>% 
  select(cntry_name,"status_1992":"status_2013") %>%
  pivot_longer(cols = "status_1992":"status_2013", names_to = "year", values_to = "Status") %>% 
  mutate(year =  as.numeric(str_replace_all(df_Status$year, pattern = "status_", replacement = "")))

# df_fiw <- cbind(df_Cl, df_Pr, df_Status)　cbindよりもleft_joinを使う

df_fiw <- left_join(df_Cl, df_Pr,  by = c("cntry_name","year")) %>% 
  left_join(df_Status,by = c("cntry_name","year")) %>% 
  mutate(Cl=as.numeric(Cl),Pr=as.numeric(Pr),Status=as.factor(Status)) %>% 
  mutate(fiw = (Cl+Pr)/2 - 1)

df_fiw_binary <- read_csv("data/FIW_electoral_democracy.csv") %>% 
  select(country, "democracy1992":"democracy2013") %>% 
  pivot_longer(cols = "democracy1992":"democracy2013", names_to = "year", values_to = "democracy") %>% 
  mutate(democracy = if_else(democracy == "Yes", 1, 0)) %>% 
  mutate(year =  as.numeric(str_replace_all(df_fiw_binary$year, pattern = "democracy", replacement = "")))

setwd("C:/Users/Owner/Documents/NTL_Martinez/NTL/data/DMSP-OLS")
csv_list <- list.files(pattern = "*.csv")
ntl <- do.call(rbind, lapply(csv_list, function(x) read.csv(x, header=TRUE, stringsAsFactors = FALSE)))

setwd("C:/Users/Owner/Documents/NTL_Martinez/NTL")
ntl <- read_csv("data/ntl.csv")
df_ntl <- ntl %>% 
  filter(between(year, 1992, 2013)) %>% 
  select(-c("gdp14_growth","cons_gdp"))

df_gdp  <- df_gdp %>% 
  rename("country" = `Country Name`) %>% 
  mutate(year = as.numeric(year))

df_fiw  <- df_fiw %>% 
  rename("country" = `cntry_name`)
df_ntl  <- df_ntl %>% 
  rename("country" = `countryname`)

df <- left_join(df_fiw_binary, df_gdp, by = c("country", "year")) %>% 
  left_join(df_fiw, by = c("country", "year")) %>% 
  left_join(df_ntl, by = c("country", "year")) %>% 
  select(-Cl, -Pr) %>% 
  mutate(logGDP = log(GDP)) %>% 
  group_by(country) %>% 
  mutate(GDP_growth = logGDP - dplyr::lag(logGDP, 1)) %>% 
  mutate(ntl_growth = lndn13 - dplyr::lag(lndn13, 1))

df2 <- df %>% 
  group_by(country) %>% 
  mutate(GDP_growth_avg = mean(GDP_growth, na.rm = TRUE)) %>% 
  mutate(ntl_growth_avg = mean(ntl_growth, na.rm = TRUE)) %>% 
  filter(year == 2013)
  
ggplot(df2, aes(x = ntl_growth_avg, y = GDP_growth_avg, color = democracy))+
  geom_point()


write(df, )

# 可視化

