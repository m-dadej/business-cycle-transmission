library(fredr)
library(ggplot2)
library(TTR)
library(vars)
library(tidyverse)
library(urca)
library(eurostat)
library(tseries)

fredr_set_key("18c2830f79155831d5c485d84472811f")

# user varaibles 
time_to <- as.Date("2020-01-01")
time_from <- as.Date("2000-01-01")
n_roc <- 1


# data wrangling
frx <- get_eurostat(id = "ert_bil_eur_m") %>%                          # exchange rates eur/pln
  filter(statinfo == "AVG", currency == "PLN") %>%
  select("date" = time, values)

df <- fredr(series_id = "DEUPROINDMISMEI") %>%                         # Production of Total Industry in Germany
  right_join(fredr(series_id = "POLPROINDMISMEI"), by = "date") %>%    # production of total industry in pol
  select(date, "prod_ger" = value.x, "prod_pol" = value.y) %>%
  right_join(fredr(series_id = "LMUNRRTTPLM156S"), by = "date") %>%    # unemployment pol
  left_join(fredr(series_id = "LMUNRRTTDEM156S"), by = "date") %>%     # unemployment ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("u_pol" = value.x, "u_ger" = value.y) %>%
  right_join(fredr(series_id = "IR3TIB01PLM156N"), by = "date") %>%    # interest rate pol
  left_join(fredr(series_id = "IR3TIB01DEM156N"), by = "date") %>%     # interest rate ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("r_pol" = value.x, "r_ger" = value.y) %>%
  left_join(fredr("POLCPALTT01IXNBM"), by  = "date") %>%               # inflation pol
  left_join(fredr("DEUCPIALLMINMEI"), by = "date") %>%                 # inflation ger
  select(-c(series_id.x, series_id.y)) %>%
  rename("pi_pol" = value.x, "pi_ger" = value.y) %>%
  left_join(fredr("CCRETT01DEM661N"), by = "date") %>%                 # real fx ger
  right_join(fredr("CCRETT01PLM661N"), by = "date") %>%                # real fx pol
  select(-c(series_id.x, series_id.y)) %>%
  rename("real_fx_pol" = value.y, "real_fx_ger" = value.x) %>%
  left_join(frx, by  = "date") %>%                                     # eur/pln exchange rate
  rename("fx" = values) %>%
  drop_na() %>%
  mutate_at(vars(prod_ger, prod_pol, real_fx_ger, real_fx_pol, fx, pi_pol, pi_ger), 
            function(x) { ROC(x, n = n_roc)}) %>%
  mutate_at(vars(u_pol, u_ger, r_pol, r_ger), 
            function(x) {c(NA, diff(x, n = n_roc))}) %>%
  filter(between(date, time_from, time_to)) %>%
  drop_na() 

# data visualization

pivot_longer(df, cols = -date) %>%
  ggplot() +
  geom_line(aes(x = date, y = value))+
  facet_wrap(~ name, scales = "free")

ggplot(df, aes(prod_pol, prod_ger)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df, aes(r_pol + 1, r_ger + 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()



# modelling 

apply(df[,-1],2, adf.test)

exo_vars_full <- select(df, u_pol, u_ger,  r_pol,   r_ger, pi_pol,   pi_ger, real_fx_ger, real_fx_pol, fx)
exo_vars <- select(df, fx, r_pol, r_ger)
endo_vars <- select(df, prod_pol, prod_ger)

VARselect(y = endo_vars,  exogen = exo_vars)
model <- VAR(y = endo_vars, exogen = exo_vars_full, p = 1)
summary(model)

model_rest <- restrict(model, method = "ser")
summary(model_rest)


rest_mat <- matrix(c(1, 1, 1, 1, 1, 1, 0, 0, 0, 
              1, 0, 1, 0, 0, 1, 0, 1, 1), nrow = 2)
model_rest_man <- restrict(model, resmat = rest_mat)
summary(model_rest)

plot(model)
causality(model, cause = "prod_pol")
causality(model, cause = "prod_ger")
causality(model_rest, cause = "prod_ger")

irf(model,impulse = "prod_ger", ortho = FALSE, cumulative = FALSE) %>% 
  plot()



