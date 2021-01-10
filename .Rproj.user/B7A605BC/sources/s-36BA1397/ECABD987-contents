library(fredr)
library(ggplot2)
library(TTR)
library(vars)
library(tidyverse)
library(urca)
library(eurostat)
library(corrr)
library(gghighlight)
library(ggrepel)
library(readxl)
library(wbstats)
library(xtable)

fredr_set_key("18c2830f79155831d5c485d84472811f")

df_e <- get_eurostat(id = "namq_10_gdp")
frx <- get_eurostat(id = "ert_bil_eur_q") %>%
  filter(statinfo == "AVG", currency == "GBP") %>%
  select("date" = time, values)

# user varaibles 
time_to <- as.Date("2019-12-01")
time_from <- as.Date("1970-01-01")
n_roc <- 1

`%nin%` <- Negate(`%in%`)

## data wrangling ############

df <- filter(df_e, s_adj == "SCA" & unit == "CLV05_MNAC" &  geo %in% countries & 
         na_item %in% c("B1GQ")) %>%
  mutate(na_item = recode(na_item, B1GQ = "gdp")) %>%
  select(na_item, geo,"date" = time, values) %>%
  pivot_wider(names_from = c(na_item, geo), values_from = values) %>%
  arrange(date) %>%
  left_join(frx, by  = "date") %>%
  left_join(fredr("CRDQFRAPABIS"), by = "date") %>%            # credit for non-financial
  left_join(fredr("QGBPAMUSDA"), by = "date") %>%
  select(-c(series_id.x, series_id.y)) %>%
  rename("credit_fr" = value.x, "credit_uk" = value.y) %>%
  left_join(fredr("CCRETT02GBQ661N"), by = "date") %>%         # real exchange rate 
  left_join(fredr("CCRETT02FRQ661N"), by = "date") %>%
  select(-c(series_id.x, series_id.y)) %>%
  rename("real_fx_uk" = value.x, "real_fx_fr" = value.y) %>%
  left_join(fredr("GBRCPIALLMINMEI"), by = "date") %>%         # inflation CPI
  left_join(fredr("FRACPIALLQINMEI"), by = "date") %>%
  select(-c(series_id.x, series_id.y)) %>%
  rename("pi_uk" = value.x, "pi_fr" = value.y) %>%
  left_join(fredr("IRLTLT01GBM156N"), by = "date") %>%         # long term interest rate
  left_join(fredr("IRLTLT01FRQ156N"), by = "date") %>%
  select(-c(series_id.x, series_id.y)) %>%
  rename("lt_r_uk" = value.x, "lt_r_fr" = value.y) %>%
  left_join(fredr("LMUNRLTTFRQ647S"), by = "date") %>%         # unemployment rate
  left_join(fredr("LMUNRLTTGBM647S"), by = "date") %>%
  select(-c(series_id.x, series_id.y)) %>%
  rename("u_fr" = value.x, "u_uk" = value.y) %>%
  left_join(fredr("IRSTCI01GBM156N"), by = "date") %>%         # short term rate
  left_join(fredr("IRSTCI01FRQ156N"), by = "date") %>% 
  select(-c(series_id.x, series_id.y)) %>%
  rename("st_r_uk" = value.x, "st_r_fr" = value.y) %>%
  mutate_at(vars(gdp_FR, gdp_UK, values, credit_fr, credit_uk, real_fx_uk, real_fx_fr), function(x){ROC(x, type = "discrete")}) %>%
  mutate_at(vars(lt_r_uk, lt_r_fr, u_fr, u_uk, st_r_uk, st_r_fr, 
                 pi_uk, pi_fr ),
            function(x){c(NA, diff(x))}) %>%
  drop_na() %>%
  filter(between(date, time_from, time_to)) 

# worldbank data

df_wb <- wb_data("NY.GDP.MKTP.PP.KD")%>%
                select("gdp" = NY.GDP.MKTP.PP.KD, date, country) %>%
                drop_na() %>%
                group_by(country) %>%
                summarise(gdp_delta = ROC(gdp, n = 1, type = "discrete"), gdp, date) %>%
                drop_na() %>%
                ungroup()

df_fdi <- wb_data("BX.KLT.DINV.WD.GD.ZS") %>%          
            select("fdi_inf" = BX.KLT.DINV.WD.GD.ZS, date, country) %>%
            drop_na() %>%
            group_by(country) %>%
            summarise(dfi_delta = c(NA, diff(fdi_inf)), fdi_inf, date) %>%
            drop_na() %>%
            ungroup()

  
# ons data

uk_exp <- read_excel("uk_export.xlsx")
uk_imp <- read_excel("uk_import.xlsx")
uk_dist <- read_excel("Distance-exports.xlsx")
usa_fdi <- read_excel("fdi_to_usa.xlsx")
usa_export <- read_excel("usa_export.xls")

# data wrangling

correlations <- select(df_wb, country, date, gdp_delta) %>%
  drop_na() %>%
  group_by(country) %>%
  summarise(country, date, gdp_delta, n_obs = n()) %>%
  ungroup() %>%
  filter(n_obs > 20) %>%
  drop_na() %>%
  pivot_wider(names_from = country, values_from =  gdp_delta) %>%
  select(-c(date, n_obs)) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  select(UK_cor = `United Kingdom`,
         usa_cor = 'United States') %>%
  mutate(country = row.names(.)) 

# warnings of NAs introduced is fine!


cor_trade_df <- mutate_all(uk_imp, as.numeric) %>% # Here is a source of the warnings. as.numeric changes strings to NA which is fine
  apply(2, function(x){mean(x, na.rm = TRUE)}) %>%
  as.data.frame() %>%
  select("UK_imp" = '.') %>%
  mutate(country = row.names(.)) %>%
  filter(country != "date") %>%
  full_join(filter(df_wb, date == 2016), by = "country") %>%
  mutate(share = UK_imp/gdp) %>%
  left_join(correlations, by = "country") %>%
  full_join(select(uk_dist, "country" = name, 
                   "dist" = `Great Circle Distance from London`,
                   "uk_exp_16" = `Total exports from UK 2016`),
            by = "country") %>%
  filter(gdp > 1e+10) %>%
  merge(df_fdi, by = c("country", "date"))

df_fdi <- filter(cor_trade_df, date == 2016) %>%
  left_join(usa_fdi, by = "country") %>%
  mutate(share_fdi = fdi_usa/ gdp)

#### descriptive statistics ######

  cor(df$gdp_FR, df$gdp_UK)
  
  lm(UK_cor ~ log10(share) + log10(dist), data = cor_trade_df) %>% summary()

### data viz #########

filtered_cycle <- mutate(df, trend_fr = mFilter::cffilter(gdp_FR)$trend,
           trend_uk = mFilter::cffilter(gdp_UK)$trend,
       cycle_fr = mFilter::cffilter(gdp_FR)$cycle) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = trend_uk, color = "UK")) +
  geom_line(aes(y = trend_fr, color = "France")) +
  geom_hline(yintercept = 0, linetype = 3) +
  labs(title = "Business cycle of France and UK",
       subtitle = "Christiano-Fitzgerald filter applied to GDP growth rate",
       x = "", y = "") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("coral2", "steelblue3"))

ggsave(filtered_cycle, filename = "filtered_cycle.pdf",
       width = 7, height = 4)

cross_section_plot <- ggplot(cor_trade_df, aes(x = share * 100, y = UK_cor)) +
                          geom_point(size = 2, aes(color = log10(dist))) +
                          scale_x_log10() +
                          geom_smooth(method = "loess", span = 1.9) +
                          scale_colour_viridis_c() +
                          labs(x = "Share of export to UK as GDP PPP (%)", 
                               y = expression(" "*Delta*" ln GDP correlation with UK"),
                               title = "Business cycle comovement and trade", 
                               color = "KM distance (logs)") +
                          geom_text_repel(aes(label = ifelse(country %in% c("France", "Australia", "New Zealand"), 
                                                             country, "")), 
                                              nudge_x = -.3,
                                              nudge_y = .1,
                                              point.padding = unit(0.5, 'lines')) +
                          theme_minimal() +
                          theme(plot.margin = unit(rep(0.5, 4), "cm")) 
  
  
  ggsave(cross_section_plot, filename = "cross_section_corr2.pdf",
         width = 7, height = 4)
  
usa_fdi_plot <- filter(cor_trade_df, date == 2016) %>%
          left_join(usa_fdi, by = "country") %>%
          left_join(usa_export, by  = "country") %>%
          mutate(share_fdi_gdp = fdi_usa/ gdp,
                 share_fdi_exp = fdi_usa / export_usa) %>%
          ggplot(aes(x = share_fdi_exp, y = usa_cor)) +
          geom_point() +
          scale_x_log10() +
          geom_smooth(method = "loess", span = 1.5) +
          geom_text_repel(aes(label = ifelse(share_fdi_exp > 15, country, "")), 
                          nudge_x = -.3,
                          point.padding = unit(0.7, 'lines')) +
          theme_minimal() +
          labs(title = "FDI activity and business cycle synchronization",
               x = "FDI / export to USA",
               y = expression(" "*Delta*" ln GDP correlation with USA"))
                          
ggsave(usa_fdi_plot, filename = "usa_fdi_plot.pdf",
       width = 6, height = 4)          

gdp_fr_uk_plot <- ggplot(df, aes(x = gdp_FR, y = gdp_UK)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Quarterly GDP growth between UK and France",
       y = expression(" "*Delta*" ln GDP of UK"),
       x = expression(" "*Delta*" ln GDP of France")) +
  scale_x_continuous(label = scales::percent) +
  scale_y_continuous(label = scales::percent) +
  theme_minimal()

ggsave(gdp_fr_uk_plot, filename = "gdp_fr_uk.pdf",
       width = 5, height = 4)

globalization <- select(df_wb, - gdp) %>%
  group_by(date) %>%
  summarise(dev = sd(gdp_delta, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date)) +
    geom_line(aes(y = dev)) +
    geom_point(aes(y = dev)) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_continuous(labels = function(x) paste0(x *100, " p.p")) +
    labs(x = "", y = expression("deviation of "*Delta*" ln GDP"),
         title = "Global business cycle comovement") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal()
    
ggsave(globalization, filename = "globalization.pdf",
       width = 5, height = 3)

######## econometric modelling ##########

apply(df[,-1], 2, PP.test)

set.seed(1)

exo_vars_full <- select(df, lt_r_uk, lt_r_fr, u_fr, u_uk, st_r_uk, st_r_fr,
                   values, credit_fr, credit_uk, real_fx_uk, real_fx_fr, 
                   pi_uk, pi_fr)

exo_vars <- select(df, values, credit_fr, credit_uk, st_r_uk, st_r_fr, pi_uk, pi_fr)
endo_vars <- select(df, gdp_UK, gdp_FR)

VARselect(y = endo_vars,exogen =  exo_vars)
model <- VAR(endo_vars, exogen = exo_vars, p = 1)

rest_mat_main <- matrix(c(1,1,1,1,0,0,1,0,1,0,
                     1,1,1,1,0,0,0,1,0,1), nrow = 2, byrow=TRUE)

model_rest1 <- restrict(model, method = "man", resmat = rest_mat_main) 

summary(model_rest1)

arch.test(model_rest1,  multivariate.only = FALSE)
stability(model_rest1) %>% plot()
serial.test(model_rest1)
logLik(model_rest1)

xtable(summary(model_rest1)$varresult$gdp_UK)
xtable(summary(model_rest1)$varresult$gdp_FR)
summary(model_rest1)$varresult$gdp_UK
residuals(model_rest1) %>% cor()

causality(model_rest1, cause = "gdp_FR")
causality(model_rest1, cause = "gdp_UK")

irf_uk_rest2 <- irf(model_rest1, response = "gdp_FR", impulse = "gdp_UK", ortho = FALSE, runs = 10^4)
irf_uk_rest2_cum <- irf(model_rest1, response = "gdp_FR", impulse = "gdp_UK", ortho = FALSE, 
                        runs = 10^4, cumulative = TRUE)

irf_fr_rest2 <- irf(model_rest1, response = "gdp_UK", impulse = "gdp_FR",ortho = FALSE, runs = 10^4)
irf_fr_rest2_cum <- irf(model_rest1, response = "gdp_UK", impulse = "gdp_FR", ortho = FALSE, 
                    runs = 10^4, cumulative = TRUE)

######## IRF viz ##########

irf2df <- function(x){
  
  df <- data.frame(central = as.numeric(x$irf[[1]]),
                   lb      = as.numeric(x$Lower[[1]]),
                   ub      = as.numeric(x$Upper[[1]]))
  
}

irf_uk_df <- irf2df(irf_uk_rest2)
irf_fr_df <- irf2df(irf_fr_rest2)
irf_fr_cum_df <- irf2df(irf_fr_rest2_cum)
irf_uk_cum_df <- irf2df(irf_uk_rest2_cum)


france_irf_plot <- ggplot(irf_fr_df, aes(x = 1:nrow(irf_fr_df))) +
        geom_line(aes(y = central, color = "central path"), size = 1.1) +
        geom_point(aes(y = central, color = "central path"), size = 2) +
        geom_ribbon(aes(ymin = lb, ymax = ub, fill = "95% confidence interval"), alpha = 0.4) +
        scale_x_continuous(labels = paste("t + ", 1:nrow(irf_fr_df)), breaks = 1:nrow(irf_fr_df)) +
        geom_hline(yintercept = 0, linetype = 2) +
        coord_cartesian(ylim = c(min(irf_fr_df, na.rm = TRUE), max(irf_fr_df, na.rm = TRUE))) +
        labs(title = "Impulse response function of France", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45),
              legend.title = element_blank(), legend.position = "none") +
        scale_color_manual(values = c("steelblue3")) +
        scale_fill_manual(values = c("steelblue3"))

uk_irf_plot <-  ggplot(irf_uk_df, aes(x = 1:nrow(irf_fr_df))) +
  geom_line(aes(y = central, color = "central path"), size = 1.1) +
  geom_point(aes(y = central, color = "central path"), size = 2) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = "95% confidence interval"), alpha = 0.4) +
  scale_x_continuous(labels = paste("t + ", 1:nrow(irf_uk_df)), breaks = 1:nrow(irf_uk_df)) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(ylim = c(min(irf_uk_df, na.rm = TRUE), max(irf_uk_df, na.rm = TRUE))) +
  labs(title = "Impulse response function of United Kingdom", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank(), legend.position = "none") +
  scale_color_manual(values = c("coral2")) +
  scale_fill_manual(values = c("coral2"))

uk_irf_cum_plot <- ggplot(irf_uk_cum_df, aes(x = 1:nrow(irf_uk_cum_df))) +
  geom_line(aes(y = central, color = "central path"), size = 1.1) +
  geom_point(aes(y = central, color = "central path"), size = 2) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = "95% confidence interval"), alpha = 0.4) +
  scale_x_continuous(labels = paste("t + ", 1:nrow(irf_uk_cum_df)), breaks = 1:nrow(irf_uk_cum_df)) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(ylim = c(min(irf_uk_cum_df, na.rm = TRUE), max(irf_uk_cum_df, na.rm = TRUE))) +
  labs(title = "Cumulative IRF", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank(), legend.position = "right") +
  scale_color_manual(values = c("coral2")) +
  scale_fill_manual(values = c("coral2"))

fr_irf_cum_plot <- ggplot(irf_fr_cum_df, aes(x = 1:nrow(irf_fr_cum_df))) +
  geom_line(aes(y = central, color = "central path"), size = 1.1) +
  geom_point(aes(y = central, color = "central path"), size = 2) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = "95% confidence interval"), alpha = 0.4) +
  scale_x_continuous(labels = paste("t + ", 1:nrow(irf_fr_cum_df)), breaks = 1:nrow(irf_fr_cum_df)) +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_cartesian(ylim = c(min(irf_fr_cum_df, na.rm = TRUE), max(irf_fr_cum_df, na.rm = TRUE))) +
  labs(title = "Cumulative IRF", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        legend.title = element_blank(), legend.position = "right") +
  scale_color_manual(values = c("steelblue3")) +
  scale_fill_manual(values = c("steelblue3"))


irfs_plot <- gridExtra::grid.arrange(uk_irf_plot, uk_irf_cum_plot,
                                     france_irf_plot, fr_irf_cum_plot)

ggsave(irfs_plot, filename = "irf_plot.pdf", width = 9, height = 6)
