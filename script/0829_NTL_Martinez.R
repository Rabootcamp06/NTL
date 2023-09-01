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

##　どのデータ？
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
# Table1 regression
stata1 <- stata %>% 
  select("countryname","year","lngdp14","lndn13","fiw") %>%
  drop_na(fiw)


fe_4_stata <- plm(lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, data = stata, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_4_stata)

fe_3_stata <- plm(lngdp14 ~ lndn13 + fiw + lndn13*fiw, data = stata, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_3_stata)

fe_2_stata <- plm(lngdp14 ~ lndn13 + fiw, data = stata, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_2_stata)

fe_1_stata <- plm(lngdp14 ~ lndn13 , data = stata1, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_1_stata)

fe_5_stata <- plm(lngdp14 ~ lndn13 + Dpfree + Dnfree + lndn13*Dpfree + lndn13*Dnfree, data = stata, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_5_stata)

fe_6_stata <- plm(lngdp14 ~ lndn13 + autocracyFH + autocracyFH * lndn13, data = stata, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_6_stata)

#自分たちのデータでtable1用データ生成
stata_col7 <- stata %>% 
  filter(year == 1992 | year == 1993 | year == 2012 | year == 2013) %>% 
  select(countryname, lndn13, lngdp14, fiw, year) %>% 
  mutate(yeard = ifelse(year == 1992 | year == 1993, 0, 1)) %>% 
  group_by(countryname, yeard) %>% 
  mutate(lndn13_av = mean(lndn13,na.rm = T), lngdp14_av = mean(lngdp14,na.rm = T), fiw_av = mean(fiw,na.rm = T)) %>% 
  filter(year == 1992 | year == 2012) %>% 
  group_by(countryname) %>%
  mutate(ntl = mean(lndn13_av), gdp = mean(lngdp14_av), demo = mean(fiw_av)) %>%
  filter(!(is.na(ntl) | is.na(gdp) | is.na(demo)))
  # filter(!(is.na(lndn13_av) | is.na(lngdp14_av) | is.na(fiw_av)))

#long run dataを利用してデータ生成
stata_lr <- stata %>% 
  select("countryname", "year", "lndn13_lr", "lngdp14_lr", "fiw_lr") %>% 
  filter(year == 1992 | year == 2012) %>% 
  group_by(countryname) %>%
  mutate(ntl = mean(lndn13_lr), gdp = mean(lngdp14_lr), demo = mean(fiw_lr)) %>%
  filter(!(is.na(ntl) | is.na(gdp) | is.na(demo)))

fe_7_stata <- plm(lngdp14_lr~ lndn13_lr + fiw_lr + I(fiw_lr^2) + lndn13_lr * fiw_lr, data = stata_lr, model = "within", effect = "twoways",index= c("countryname", "year"))
summary(fe_7_stata)

# modelsummaryとstergazerとtexregは失敗：苦闘
library(lmtest)
library(sandwich)
res = list()
res <- c(res, coeftest(fe_1_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_2_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_3_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_4_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_5_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_6_stata, vcov = vcovHC))
res <- c(res, coeftest(fe_7_stata, vcov = vcovHC))
library(texreg)
library(modelsummary)
msummary(res, "latex")
screenreg(res[[1]], stars = NULL)

#編集前
#res = list()
#res <- c(res, fe_1_stata)
#res <- c(res, fe_2_stata)   
#res <- c(res, fe_3_stata)
#res <- c(res, fe_4_stata)
#res <- c(res, fe_5_stata)
#res <- c(res, fe_6_stata)
#res <- c(res, fe_7_stata)
library(texreg)
#screenreg(res[[1]], stars = NULL)

models <- list("model1" = fe_1_stata,
               "model2" = fe_2_stata,
               "model3" = fe_3_stata,
               "model4" = fe_4_stata,
               "model5" = fe_5_stata,
               "model6" = fe_6_stata,
               "model7" = fe_7_stata)
screenreg(models, stars = NULL)
htmlreg(models, stars = NULL)


res1 <- coeftest(fe_1_stata, vcov = vcovHC)
res2 <- coeftest(fe_2_stata, vcov = vcovHC)
res3 <- coeftest(fe_3_stata, vcov = vcovHC)
res4 <- coeftest(fe_4_stata, vcov = vcovHC)
res5 <- coeftest(fe_5_stata, vcov = vcovHC)
res6 <- coeftest(fe_6_stata, vcov = vcovHC)
res7 <- coeftest(fe_7_stata, vcov = vcovHC)
models2 <- list("model1" = res1,
               "model2" = res2,
               "model3" = res3,
               "model4" = res4,
               "model5" = res5,
               "model6" = res6,
               "model7" = res7)
screenreg(models2, stars = NULL)
htmlreg(models2, stars = NULL, digits = 3)

stata %>% 
  filter(dn13_growth >= -0.3 & dn13_growth <= 0.5) %>% 
  drop_na(autocracyFH) %>% 

ggplot(aes(x = dn13_growth, y = gdp14_growth,    shape = as.factor(autocracyFH)))+
  # scale_shape_manual(values = c(0,2))+
  geom_point()



stata_arranged <- stata %>%
  group_by(autocracyFH) %>% 
  arrange(dn13_growth) %>% 
  mutate(bin = cut_number(row_number(), n = 20))

n_auto <- stata %>% 
  filter(autocracyFH == 0) %>% 
  drop_na("dn13_growth", "gdp14_growth") %>% 
  arrange(dn13_growth) %>% 
  mutate(bin = cut_number(row_number(), n = 20))

n_demo <- stata %>% 
  filter(autocracyFH == 1) %>% 
  drop_na("dn13_growth", "gdp14_growth") %>% 
  arrange(dn13_growth) %>% 
  mutate(bin = cut_number(row_number(), n = 20))

demo_bin2 <- n_auto %>% 
  group_by(bin) %>% 
  summarise(mean_gdp = mean(gdp14_growth, na.rm = TRUE),
            mean_dn = mean(dn13_growth, na.rm = TRUE)) %>% 
  mutate(auto = 0)

demo_bin <- n_demo %>% 
  group_by(bin) %>% 
  summarise(mean_gdp = mean(gdp14_growth, na.rm = TRUE),
            mean_dn = mean(dn13_growth, na.rm = TRUE)) %>% 
  mutate(auto = 1)

demo_bin_bind <- rbind(demo_bin, demo_bin2)

ggplot(demo_bin_bind, aes(x = mean_dn, y = mean_gdp, shape = as.factor(auto)))+
  scale_shape_manual(values = c(1,2))+
  scale_x_continuous(limits = c(-0.3,0.4))+
  scale_y_continuous(limits = c(0.02,0.07))+
  geom_point()

ggplot(data = demo_bin_bind, aes(x = mean_dn*100, y = mean_gdp*100, shape = as.factor(auto))) +
  geom_point(aes(color = as.factor(auto))) +
  geom_smooth(aes(color = as.factor(auto)), method = lm, linetype = "dashed", se = FALSE) +
  labs(title = "GDP GROWTH ESTIMATES IN AUTOCRACIES", x = "Growth of Lights Digital Number(%)", y = "GDP growth(%)")+
  scale_color_hue(name = "", labels = c("0" = "Democracy", "1" ="Autocracy")) +
  scale_shape_manual(name = NULL, values = c(16, 17), labels = c("Democracy","Autocracy")) +
  scale_x_continuous(limits = c(-30,40))+
  scale_y_continuous(limits = c(2,7))+
  #annotate("text", x = 16, y = 9.5, label = "Autocracy") +
  #annotate("text", x = 16, y = 5.5, label = "Democracy") +
  theme_light()


