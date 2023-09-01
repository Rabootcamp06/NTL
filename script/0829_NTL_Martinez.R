# install.packages("haven")
library(tidyverse)
library(haven)

# stata data --------------------------------------------------------------

stata <- read_dta("data/Estimations.dta")

# 解析 ----------------------------------------------------------------------


#GDP データ加工
gdp <- read_csv("data/WDI_2014_11.csv")
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
  mutate(year =  as.numeric(str_replace_all(year, pattern = "cl_", replacement = "")))  
  
# df_Cl2 <- str_replace_all(df_Cl$year, pattern = "cl_", replacement = " ") str_replace_allはベクトルを返す

df_Pr <- fiw %>% 
  select(all_of(name_list)) %>% 
  select(cntry_name,"pr_1992":"pr_2013") %>%
  pivot_longer(cols = "pr_1992":"pr_2013", names_to = "year", values_to = "Pr") %>% 
  mutate(year =  as.numeric(str_replace_all(year, pattern = "pr_", replacement = "")))

df_Status <- fiw %>% 
  select(all_of(name_list)) %>% 
  select(cntry_name,"status_1992":"status_2013") %>%
  pivot_longer(cols = "status_1992":"status_2013", names_to = "year", values_to = "Status") %>% 
  mutate(year =  as.numeric(str_replace_all(year, pattern = "status_", replacement = "")))

# df_fiw <- cbind(df_Cl, df_Pr, df_Status)　cbindよりもleft_joinを使う

df_fiw <- left_join(df_Cl, df_Pr,  by = c("cntry_name","year")) %>% 
  left_join(df_Status,by = c("cntry_name","year")) %>% 
  mutate(Cl=as.numeric(Cl),Pr=as.numeric(Pr),Status=as.factor(Status)) %>% 
  mutate(fiw = (Cl+Pr)/2 - 1)

nrow(is.na(df_fiw$Cl))

sum(!is.na(Cl))

df_fiw_binary <- read_csv("data/FIW_electoral_democracy.csv") %>% 
  select(country, "democracy1992":"democracy2013") %>% 
  pivot_longer(cols = "democracy1992":"democracy2013", names_to = "year", values_to = "democracy") %>% 
  mutate(democracy = if_else(democracy == "Yes", 1, 0)) %>% 
  mutate(year =  as.numeric(str_replace_all(year, pattern = "democracy", replacement = "")))

# setwd("C:/Users/Owner/Documents/NTL_Martinez/NTL/data/DMSP-OLS")
# csv_list <- list.files(pattern = "*.csv")
# ntl <- do.call(rbind, lapply(csv_list, function(x) read.csv(x, header=TRUE, stringsAsFactors = FALSE)))
# 
# setwd("C:/Users/Owner/Documents/NTL_Martinez/NTL")

# ntl <- read_csv("data/ntl.csv")
# df_ntl <- ntl %>% 
#   filter(between(year, 1992, 2013)) %>% 
#   select(-c("gdp14_growth","cons_gdp"))

ntl <- stata %>% 
  select("countryname", "year", "dn13_growth", "gdp14_growth", "lndn13", "gdp14_growth")

# df_ntl <- ntl %>% 
#   filter(between(year, 1992, 2013)) %>% 
#   select(-c("gdp14_growth","cons_gdp"))

df_ntl <- ntl %>% 
  filter(between(year, 1992, 2013))

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
  mutate(GDP_growth = logGDP - dplyr::lag(logGDP, 1)) 
 # mutate(ntl_growth = lndn13 - dplyr::lag(lndn13, 1))

df3 <- left_join(df_fiw_binary, df_gdp, by = c("country", "year")) %>% 
  left_join(df_fiw, by = c("country", "year")) %>% 
  left_join(df_ntl, by = c("country", "year")) %>% 
  select(-Cl, -Pr) %>% 
  mutate(logGDP = log(GDP)) %>% 
  group_by(country) %>% 
  mutate(GDP_growth = logGDP - dplyr::lag(logGDP, 1)) 

df2 <- df3 %>% 
  group_by(country) %>% 
  mutate(GDP_growth_avg = mean(GDP_growth, na.rm = TRUE)) %>% 
  mutate(ntl_growth_avg = mean(dn13_growth, na.rm = TRUE)) %>% 
  filter(year == 2010) 


write.csv(df,"data/repdata.csv",row.names = F)
write.csv(df2,"data/repdata_ave.csv",row.names = F)



# 可視化
# shape = as.factor(democracy)
# scale_shape_manual(values = c(0,2))

ggplot(df2, aes(x = ntl_growth_avg, y = GDP_growth_avg, shape = as.factor(democracy)))+
  scale_x_continuous(limits = c(-0.3,0.3))+
  scale_shape_manual(values = c(0,2))+
  geom_point()

stata %>% select(abdn13)

ggplot(df, aes(x = dn13_growth, y = gdp14_growth, shape = as.factor(democracy), group = country))+
  scale_shape_manual(values = c(0,2))+
  geom_point()

# 試しにafgの平均とってみる
df_afg <- df %>% 
  filter(country == "Afghanistan") %>% 
  select(year, dn13_growth, lndn13)
#meanとずれてないか確認
(df_afg$lndn13[22] - df_afg$lndn13[1])/22
mean(df_afg$dn13_growth, na.rm = T)
(exp(df_afg$lndn13[2]) - exp(df_afg$lndn13[1]))/exp(df_afg$lndn13[1])

#Table1 fixed effect
library(plm)
p_4 <- pdata.frame(df, index= c("country", "year"))
fe_4 <- plm(logGDP ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, data = p_4, model = "within")
summary(fe_4)

summary(df)
stata2 <- stata %>% 
  select("countryname","year","lndn13", "fiw", "lngdp14") %>% 
  filter(year >= 1992 & year<=2013)

summary(stata2)

#- pdata.frame(stata, index= c("countryname", "year"))
stata1 <- stata %>% 
  select("countryname","year","lngdp14","lndn13","fiw") %>%
  drop_na(fiw)


fe_4_stata <- plm(lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, data = stata, model = "within", effect = "twoway",index= c("countryname", "year"))
summary(fe_4_stata)

fe_3_stata <- plm(lngdp14 ~ lndn13 + fiw + lndn13*fiw, data = stata, model = "within", effect = "twoway",index= c("countryname", "year"))
summary(fe_3_stata)

fe_2_stata <- plm(lngdp14 ~ lndn13 + fiw, data = stata, model = "within", effect = "twoway",index= c("countryname", "year"))
summary(fe_2_stata)

fe_1_stata <- plm(lngdp14 ~ lndn13 , data = stata1, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_1_stata)

?coeftest
library(lmtest)
library(sandwich)
coeftest(stata, df= Inf, vcov = vcovHC(fe_1_stata,type = "HC0") )

stata %>% 
  filter(dn13_growth >= -0.3 & dn13_growth <= 0.5) %>% 
  drop_na(autocracyFH) %>% 

ggplot(aes(x = dn13_growth, y = gdp14_growth,    shape = as.factor(autocracyFH)))+
  # scale_shape_manual(values = c(0,2))+
  geom_point()



stata_arranged <- stata %>%
  group_by(autocracyFH) %>% 
  arrange(dn13_growth) %>%
  mutate(bin = cut_number(row_number(), n=20))

n <- nrow(stata_arranged)