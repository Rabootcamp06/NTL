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



