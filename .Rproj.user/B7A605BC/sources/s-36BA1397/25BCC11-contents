library(fredr)
library(ggplot2)
library(TTR)
library(vars)
library(tidyverse)
library(urca)
library(eurostat)
library(corrr)


fredr_set_key("18c2830f79155831d5c485d84472811f")


df_e <- get_eurostat(id = "namq_10_gdp")
frx <- get_eurostat(id = "ert_bil_eur_q") %>%
  filter(statinfo == "AVG", currency == "GBP") %>%
  select("date" = time, values)

# user varaibles 
time_to <- as.Date("2020-01-01")
time_from <- as.Date("1970-01-01")
n_roc <- 1
countries <- c("UK", "DE")

## data wrangling ###########

df_fred <- fredr(series_id = "DEUPROINDMISMEI") %>%                    # Production of Total Industry in Germany
  left_join(fredr(series_id = "POLPROINDMISMEI"), by = "date") %>%    # production of total industry in pol
  select(date, "prod_ger" = value.x, "prod_pol" = value.y) %>%
  left_join(fredr(series_id = "LMUNRRTTPLM156S"), by = "date") %>%    # unemployment pol
  left_join(fredr(series_id = "LMUNRRTTDEM156S"), by = "date") %>%     # unemployment ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("u_pol" = value.x, "u_ger" = value.y) %>%
  left_join(fredr(series_id = "IR3TIB01PLM156N"), by = "date") %>%    # interest rate pol
  left_join(fredr(series_id = "IR3TIB01DEM156N"), by = "date") %>%     # interest rate ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("r_pol" = value.x, "r_ger" = value.y) %>%
  left_join(fredr("POLCPALTT01IXNBM"), by  = "date") %>%               # inflation pol
  left_join(fredr("DEUCPIALLMINMEI"), by = "date") %>%                 # inflation ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("pi_pol" = value.x, "pi_ger" = value.y) %>%
  left_join(fredr("CCRETT01DEM661N"), by = "date") %>%                 # real fx ger
  left_join(fredr("CCRETT01PLM661N"), by = "date") %>%                # real fx pol
  select(-c(series_id.x, series_id.y)) %>%
  rename("real_fx_pol" = value.y, "real_fx_ger" = value.x) %>%
  left_join(frx, by  = "date") %>%                                     # eur/pln exchange rate
  rename("fx" = values) %>%
  filter(between(date, time_from, time_to)) 


df <- filter(df_e, s_adj == "SCA" & unit == "CLV05_MNAC" &  geo %in% countries & 
         na_item %in% c("B1GQ", "P3", "P5G", "P6", "P7") & time <= time_to) %>%
  mutate(na_item = recode(na_item, B1GQ = "gdp")) %>%
  select(na_item, geo,"date" = time, values) %>%
  pivot_wider(names_from = c(na_item, geo), values_from = values) %>%
  arrange(date) %>%
  left_join(df_fred, by  = "date") %>%
  drop_na() %>%
  mutate_at(vars(prod_ger, prod_pol, real_fx_ger, real_fx_pol, fx, pi_pol, pi_ger, fx,
                 gdp_UK, gdp_DE, P3_UK, P3_DE, P5G_UK, P5G_DE, P6_UK, P6_DE, P7_UK, P7_DE), 
            function(x) { ROC(x, n = n_roc)}) %>%
  mutate_at(vars(u_pol, u_ger, r_pol, r_ger), 
            function(x) {c(NA, diff(x, n = n_roc))}) %>%
  drop_na()

  
  
##### data viz #########

correlate(df[,-1]) %>%
  rplot()

pivot_longer(df, cols = -date) %>%
  ggplot() +
  geom_line(aes(x = date, y = value))+
  facet_wrap(~ name, scales = "free")

ggplot(df, aes(x = gdp_DE, y = gdp_UK)) +
  geom_point() +
  geom_smooth(method = "lm")


###### modelling #######

apply(df[,-1],2, adf.test)


exo_vars_full <- select(df,u_pol, u_ger, r_pol, r_ger, pi_pol, pi_ger, real_fx_ger, real_fx_pol,
                   prod_ger, prod_pol, P3_DE, P3_PL, P5G_DE, P5G_PL, P6_DE, P6_PL, P7_DE, P7_PL, fx)
exo_vars <-  select(df, fx, P3_UK, P3_DE)
endo_vars <- select(df, gdp_UK, gdp_DE)

VARselect(y = endo_vars, exogen = exo_vars)

model <- VAR(y = endo_vars, exogen = exo_vars, p = 1)
summary(model)

model_rest <- restrict(model, method = "ser")
summary(model_rest)

causality(model, cause = "prod_pol")
causality(model, cause = "gdp_UK")
causality(model_rest, cause = "gdp_DE")

irf(model,impulse = "gdp_EU15", response = "gdp_UK",
    ortho = TRUE, cumulative = FALSE) %>% 
  plot()

irf(model,impulse = "gdp_UK", response = "gdp_PL",
    ortho = TRUE, cumulative = FALSE) %>% 
  plot()

colnames(df)

mutate(df, gdp_DE_l = lag(gdp_DE), gdp_PL_l = lag(gdp_PL)) %>%
  drop_na() %>%
  .[,-1] %>%
  cor() %>%
  as.data.frame() %>%
  select(gdp_DE_l, gdp_PL_l, gdp_DE, gdp_PL)

