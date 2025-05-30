################################### Libraries #################################
library(ggplot2)
library(dplyr) 
library(tidyr)
library(tidyverse)
library(modelsummary)
library(fixest)
library(plm)
library(did)
library(didimputation)
library(bacondecomp)
library(did2s)
library(here)
library(haven)
library(HonestDiD)
library(broom)
library(readxl)
library(combinat)


################################### DOWNLOAD A DATA  ###############################
# Set working directory and download a data
setwd("~/Диплом/Выборки для R")
dataset <- read.csv("imputed_2022.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
state_interest <- read_excel("C:/Users/annaz/Documents/Диплом/Госучастие.xlsx")
dataset <- dataset %>%
  mutate(state_interest = if_else(REGN %in% state_interest$REGN, 1, 0))

is.pbalanced(dataset, index = c("REGN", "DATE"))
sum(is.na(dataset))

################################### DATA PREPARATION, TIME VARIABLES  ###############################
### 1. Create a period variable t for each bank reflecting the periods from the beginning of time in dataset
# Create a vector of all possible periods (t) in dataset from 2019Q1 to 2021Q4
dataset$t <- with(dataset, case_when(
  year == 2019 & quarter == 1 ~ 1,
  year == 2019 & quarter == 2 ~ 2,
  year == 2019 & quarter == 3 ~ 3,
  year == 2019 & quarter == 4 ~ 4,
  year == 2020 & quarter == 1 ~ 5,
  year == 2020 & quarter == 2 ~ 6,
  year == 2020 & quarter == 3 ~ 7,
  year == 2020 & quarter == 4 ~ 8,
  year == 2021 & quarter == 1 ~ 9,
  year == 2021 & quarter == 2 ~ 10,
  year == 2021 & quarter == 3 ~ 11,
  year == 2021 & quarter == 4 ~ 12,
  year == 2023 & quarter == 2 ~ 13,
  year == 2023 & quarter == 3 ~ 14,
  year == 2023 & quarter == 4 ~ 15,
  year == 2024 & quarter == 1 ~ 16,
  year == 2024 & quarter == 2 ~ 17,
  year == 2024 & quarter == 3 ~ 18,
  year == 2024 & quarter == 4 ~ 19,
  TRUE ~ NA_real_
))

### 2. Create a column with the info about the first year and quarter the SANCTIONS were started
# Sorting data by bank and date
dataset <- dataset %>%
  arrange(REGN, year, quarter) %>%
  group_by(REGN) %>%
  mutate(
    year_start = if_else(SANCTIONS_2022 == 1, year, NA_integer_),
    quarter_start = if_else(SANCTIONS_2022 == 1, quarter, NA_integer_)
  ) %>%
  mutate(
    year_start = if (any(SANCTIONS_2022 == 1)) first(na.omit(year_start)) else NA_integer_,
    quarter_start = if (any(SANCTIONS_2022 == 1)) first(na.omit(quarter_start)) else NA_integer_
  ) %>%
  ungroup()
# fill NaN with 0 for the banks that were never got SANCTIONS
dataset$year_start[is.na(dataset$year_start)] <- 0
dataset$quarter_start[is.na(dataset$quarter_start)] <- 0


### 3. Mark groups of banks as a number which reflects the first period t when the sanctions were started
dataset <- dataset %>%
  group_by(REGN) %>%
  mutate(
    first_sanction_t = if_else(year_start != 0 & year == year_start & quarter == quarter_start, t, NA_integer_)
  ) %>%
  mutate(
    g = if (any(!is.na(first_sanction_t))) first(na.omit(first_sanction_t)) else NA_integer_
  ) %>%
  ungroup() %>%
  mutate(g = replace_na(g, 0))

# Delete fictive variable first_sanction_t
dataset <- dataset %>%
  select(-first_sanction_t)

summary(dataset$g)
unique(dataset$g)
length(unique(dataset$g))

result <- dataset %>%
  group_by(g) %>%
  summarize(unique_count = n_distinct(REGN))
print(result)

### 4. Event-time variable that marks the periods from the beginning of the treatment
dataset$p<-dataset$t-dataset$g
summary(dataset$p)

### 5.SANCTIONS 2014 Q1 (t=9)
# the first sanctions were implemented at 2014 Q1 (t=9)б let's generate period in event-study scale from -8 to 19
dataset$period<-dataset$t-13
summary(dataset$period)

#dummy variable "after" refers to the periods after 2014Q1: period > 0 (t>9). 
# "SANCTIONS_2014" dummy refers to the group of banks which got sanctions after 2014Q1.
dataset$after <- ifelse(dataset$period >= 0 & dataset$t >= 13, 1, 0)

# 6. There are groups that were sanctioned at 2014 and still in dataset. Let's drop from control group the observations
# that consists of banks which were sanctioned at 2014 but at 2022 - not.
data_not_double_sanctions <- dataset[dataset$year <= 2021 & (dataset$year == 2021 & dataset$quarter > 4) & 
                                       dataset$SANCTIONS == 1, ]

data_not_double_sanctions <- dataset %>%
  filter(year <= 2021,
         !(year == 2021 & quarter > 4),
         SANCTIONS == 1, SANCTIONS_2022 ==0)
## There are no such observations!

# Set up panel data structure
pdataframe <- pdata.frame(dataset, index = c("REGN", "period"))
pdataframe %>% filter(g > 0) %>% count()


################################### TESTING FOR PARALLEL TREND ASSUMPTION: TWFE model ####################################
## Check for multicollinearity using covariance matrix
names(pdataframe)
vars <- pdataframe[, c("ROA", "ROE", "NIM", "Z.score", "Z.score_RWA", "CAR", 
                       "NPLR", "LTD", "CIR", "loan_share", "dep_to_assets", "deposit_share", 
                       "Bank_Size", "Leverage", "Asset_Turnover", "Net_Profit_Margin", "Tax_Burder", 
                       "Н1.0", "Н2", "Н3", "Н4", "Н7", "sys_importance", "state_interest")]

cor_matrix <- cor(vars, method = "pearson")
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%
  filter(abs(Freq) > 0.5) %>%
  arrange(desc(abs(Freq)))

print(cor_df)
setwd("~/Диплом/R")
writexl::write_xlsx(cor_df, "correlation_matrix.xlsx")

## Multicollinearity above 50% (but less than 69%): 
# CAR_YoY  and Z.score_YoY
# Н1.0_YoY  and Z.score_RWA_YoY
# loan_share_YoY and CAR_YoY
# loan_share_YoY and Z.score_YoY
# Leverage_YoY and Bank_Size_YoY 
# deposit_share_YoY and dep_to_assets_YoY 
# Н7_YoY and Н4_YoY 

# corrects for autocorrelation in the residuals
# 1. Dynamic TWFE without covariates 
# 1. Dynamic TWFE without covariates 
dep_vars <- c("ROE", "ROA", "NIM", "LTD", "loan_share", "dep_to_assets", 
              "deposit_share","NPLR", "Н7", "Bank_Size","Leverage","CAR","Z.score", "Z.score_RWA",
              "Н1.0","CIR","Н2", "Н3", "Н4", 
              "Asset_Turnover", "Net_Profit_Margin", "Tax_Burder")

drop_coef<- c("p::1:SANCTIONS_2022","p::2:SANCTIONS_2022","p::3:SANCTIONS_2022","p::4:SANCTIONS_2022","p::5:SANCTIONS_2022",
              "p::6:SANCTIONS_2022","p::7:SANCTIONS_2022", "p::8:SANCTIONS_2022","p::9:SANCTIONS_2022","p::10:SANCTIONS_2022",
              "p::11:SANCTIONS_2022","p::12:SANCTIONS_2022","p::13:SANCTIONS_2022", "p::14:SANCTIONS_2022","p::15:SANCTIONS_2022",
              "p::16:SANCTIONS_2022","p::17:SANCTIONS_2022","p::18:SANCTIONS_2022", "p::19:SANCTIONS_2022")

# Strong PTA test without covariates
# Empty data.frame
results <- data.frame( variable = character(), p_value = numeric(),
                       stringsAsFactors = FALSE)

for (var in dep_vars) {
  model <- feols(as.formula(paste(var, "~ factor(p):SANCTIONS_2022| REGN + t + quarter")),
                 data = pdataframe,
                 vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", 
                                            lag = 1, ssc(fixef.K = "none")))
  
  wald_test <- wald(model, "SANCTIONS_2022", drop = drop_coef)
  p_value <- ifelse(is.na(wald_test), NA, wald_test)
  
  results <- rbind(results, data.frame(
    variable = var,
    p_value = p_value))
}

setwd("~/Диплом/R")
writexl::write_xlsx(results, "wald_test_results.xlsx")

# Формула с явным указанием периодов
twfe <- feols(ROE ~ factor(p):SANCTIONS_2022| REGN + t + quarter, data = pdataframe, 
              vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
etable(twfe, vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none"))) 
coefplot(twfe)
wald(twfe, "SANCTIONS_2022", drop= drop_coef)


# 2. Sun&Abraham estimator without covatiates:
pdataframe$REGN <- as.numeric(as.character(pdataframe$REGN))
pdataframe$t <- as.numeric(pdataframe$t)
pdataframe$period <- as.numeric(pdataframe$period)
pdataframe$g <- as.numeric(pdataframe$g)

#PTA test Sun&Abraham estimator without covatiates
results_sab <- data.frame( variable = character(), p_value = numeric(),
                           stringsAsFactors = FALSE)

for (var in dep_vars) {
  model <- feols(as.formula(paste(var, "~ sunab(g, period) + SANCTIONS_2022 | REGN + t")),
                 data = pdataframe,
                 vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", 
                                            lag = 1, ssc(fixef.K = "none")))
  
  wald_test <- wald(model, "SANCTIONS_2022", drop = drop_coef)
  p_value <- ifelse(is.na(wald_test), NA, wald_test)
  
  results_sab <- rbind(results_sab, data.frame(
    variable = var,
    p_value = p_value))
}

setwd("~/Диплом/R")
writexl::write_xlsx(results_sab, "wald_test_results_sab.xlsx")


#res_sunab = feols(deposit_share_YoY ~ sunab(g, period) + SANCTIONS_2022| REGN + t, pdataframe,
#                  vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
#etable(res_sunab, vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none"))) 
#iplot(res_sunab)
#wald(res_sunab, "SANCTIONS_2022", drop= drop_coef)



# 3. Dynamic TWFE with covariates:
dep_vars <- c("ROE", "ROA", "NIM", "LTD", "loan_share", "dep_to_assets", 
              "deposit_share","NPLR", "Н7", "Z.score")

covariates <- c("Bank_Size_LAG1",  "sys_importance", "state_interest", "Leverage_LAG1", "Н1.0_LAG1",
                "CIR_LAG1",  "Н2_LAG1", "Н3_LAG1", "Н4_LAG1")

results_cov <- data.frame( variable = character(), p_value = numeric(),
                           stringsAsFactors = FALSE)


for (var in dep_vars) {
  formula <- as.formula(
    paste(var,"~ factor(p):SANCTIONS_2022 +", paste(covariates, collapse = " + "),"| REGN + t + quarter"))
  
  model <- feols(formula, data = pdataframe,
                 vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
  
  wald_test <- wald(model, "SANCTIONS_2022", drop = drop_coef)
  
  results_cov <- rbind(results_cov, data.frame(
    variable = var,
    p_value = ifelse(is.na(wald_test), NA, wald_test)))
  
}

setwd("~/Диплом/R")
writexl::write_xlsx(results_cov, "wald_test_results_cov.xlsx")

# 4. Sun&Abraham estimator with covatiates:
results_sancov <- data.frame( variable = character(), p_value = numeric(),
                              stringsAsFactors = FALSE)

for (var in dep_vars) {
  formula <- as.formula(
    paste(var,"~ sunab(g, period) + SANCTIONS_2022 + ", paste(covariates, collapse = " + "),"| REGN + t"))
  
  model <- feols(formula, data = pdataframe,
                 vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
  
  wald_test <- wald(model, "SANCTIONS_2022", drop = drop_coef)
  
  results_sancov <- rbind(results_sancov, data.frame(
    variable = var,
    p_value = ifelse(is.na(wald_test), NA, wald_test)))
  
}

setwd("~/Диплом/R")
writexl::write_xlsx(results_sancov, "wald_test_results_sancov.xlsx")

## for suspicious variables
res_sunab = feols(LTD_YoY ~ sunab(g, period) + SANCTIONS_2022 + Bank_Size_YoY + sys_importance + Leverage_YoY + Н1.0_YoY +
                    CIR_YoY + Asset_Turnover_YoY + Net_Profit_Margin_YoY + Tax_Burder_YoY| REGN + t, pdataframe,
                  vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
etable(res_sunab, vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none"))) 
iplot(res_sunab)
#PTA test Sun&Abraham estimator with covatiates
wald(res_sunab, "SANCTIONS_2022", drop= drop_coef)


################################### SIMPLE PANEL DD estimator WITH COVARIATE: TWFE model ####################################
## Check for multicollinearity using covariance matrix
names(pdataframe)
setwd("~/Диплом/R")
vars <- pdataframe[, c("ROE", "ROA", "NIM","LTD", "loan_share", "dep_to_assets", "NPLR", "Н7", "Z.score", 
                       "sys_importance", "state_interest", "Bank_Size_LAG1", "Leverage_LAG1", "CAR_LAG1","CIR_LAG1", 
                       "Н1.0_LAG1","Н2_LAG1", "Н3_LAG1")]

cor_matrix <- cor(vars, method = "pearson")
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%
  filter(abs(Freq) > 0.5) %>%
  arrange(desc(abs(Freq)))

print(cor_df)

writexl::write_xlsx(cor_df, "correlation_matrix.xlsx")

#static model with varying time of treatment TWFE model
# Panel fixed effect model (within) + controls covariates for dummy after = 1 if year after 2022 Q1  
model_TWFE <- plm(Z.score ~ after*SANCTIONS_2022_LAG1 + sys_importance + state_interest + Bank_Size_LAG1 + 
                    CIR_LAG1 + Leverage_LAG1 + Н3_LAG1 + quarter_2 + quarter_3 + quarter_4,
                  data = pdataframe, model = "within", effect = "twoways")
summary(model_TWFE)

# Dynamic ATT: EVENT STUDY TWFE model##:
model_TWFE_dyn <- plm(Z.score ~ factor(period):SANCTIONS_2022_LAG1 + sys_importance + state_interest + Bank_Size_LAG1 + CIR_LAG1 + 
                        Z.score_RWA_LAG1 + Leverage_LAG1 + Н3_LAG1 ,
                      data = pdataframe, model = "within", effect = "twoways",
                      #vcov = vcovHC(x, type = "HC1"))
                      vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
summary(model_TWFE_dyn)

################################### Gardner (2021), did2s: Two-Stage Difference-in-Differences ############
# Did2s  https://arxiv.org/pdf/2109.05913.pdf by Kyle Butts and John Gardner
# https://kylebutts.github.io/did2s/
# this is a fixest command (feols), ref=F (FALSE) <-> NT + NYT as control group
# STATIC-models
# Returns ATT as in (Gardner 2021)
static = did2s(
  data = pdataframe,
  yname = "Z.score",
  treatment = "SANCTIONS_2022_LAG1", 
  first_stage = ~ sys_importance  + Bank_Size_LAG1 + 
    CIR_LAG1 + Leverage_LAG1 + Н3_LAG1| REGN + period + quarter,
  second_stage = ~ i(SANCTIONS_2022_LAG1, ref = TRUE),
  cluster_var = "REGN",
  verbose = TRUE)

fixest::esttable(static)
summary(static)

# Dynamic event-study ATT(p) 
dynamic_p = did2s(
  data = pdataframe,
  yname = "Z.score",
  treatment = "SANCTIONS_2022_LAG1", 
  first_stage = ~ sys_importance  + Bank_Size_LAG1 + 
    CIR_LAG1 + Leverage_LAG1 + Н3_LAG1| REGN + period + quarter,
  second_stage = ~ i(period, ref = c(-1, Inf)),
  cluster_var = "REGN",
  verbose = TRUE)
summary(dynamic_p)

# Plot results of the dynamic event-study ATT(p)
pdataframe$period <- as.character(pdataframe$period)
fixest::iplot(
  dynamic_p, 
  main = "ROE Event study: Staggered treatment",
  xlab = "Relative period to treatment",
  col = "steelblue",  ref.line = -0.5)


################################### Borusyak, Jaravel, and Spiess (2021) Estimator method ############################################
# This method is useful when the initial time of treatment is different for different groups.
# The goal of didimputation is to estimate TWFE models without running into the problem of staggered treatment adoption.

# Static panel FE. Returns ATT
out <- did_imputation(data = dataset, yname = "Z.score", gname = "g",
                      tname = "t", idname = "REGN")
summary(out)
# Static panel FE with covariates. Returns ATT
out <- did_imputation(data = dataset, yname = "Z.score", gname = "g",
                      tname = "t", idname = "REGN",
                      first_stage = ~ sys_importance  + Bank_Size_LAG1 + 
                        CIR_LAG1 + Z.score_RWA_LAG1 + Leverage_LAG1 + Н3_LAG1 | REGN + period + quarter)
summary(out)


#event_study. Returns ATT(t)
es <- did_imputation(data = dataset, yname = "Z.score", gname = "g",
                     tname = "t", idname = "REGN", horizon=TRUE, pretrends = -12:0,
                     first_stage = ~ 0| REGN + period + quarter)
print(es, n = Inf)
#event_study. Returns ATT(t) with covariates
es <- did_imputation(data = dataset, yname = "Z.score", gname = "g",
                     tname = "t", idname = "REGN", horizon=TRUE, pretrends = -12:0,
                     first_stage = ~ sys_importance + Bank_Size_LAG1 +
                       CIR_LAG1 + Н3_LAG1 + Н4_LAG1 + Z.score_RWA_LAG1| REGN + period + quarter)
print(es, n = Inf)

############################################# 
############################################################# Sun&Abraham estimator #############:
#library fixest for ATT implements Sun&Abraham estimator without covariates
pdataframe$REGN <- as.numeric(as.character(pdataframe$REGN))
pdataframe$t <- as.numeric(pdataframe$t)
pdataframe$g <- as.numeric(pdataframe$g)
res_sunab = feols(Z.score ~ sunab(g, t) + SANCTIONS_2022_LAG1| REGN + t + quarter, pdataframe,
                  vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
etable(res_sunab, vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none"))) 
iplot(res_sunab)
#PTA test Sun&Abraham estimator with covatiates
wald(res_sunab, "SANCTIONS_2022_LAG1", drop= drop_coef)


# with covariates
res_sunab = feols(Z.score ~sunab(g, t, ref.p = c(.F + 0:6, -12:-1)) + SANCTIONS_2022_LAG1 + sys_importance  + Bank_Size_LAG1 + 
                    CIR_LAG1 + Leverage_LAG1 + Н3_LAG1| REGN + t + quarter, pdataframe,
                  vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none")))
etable(res_sunab, vcov = function(x) vcov_NW(x, unit = "REGN", time = "period", lag = 1, ssc(fixef.K = "none"))) 

iplot(list(res_sunab))
#coefplot(res_sunab)
summary(res_sunab, agg = "ATT")
summary(res_sunab, agg = "cohort")


################################### Callaway and Sant’Anna’s estimator ##############################
library(did)
library(BMisc)
library(remotes)
remotes::install_github("bcallaway11/twfeweights")
library(twfeweights)
pdataframe$REGN <- as.numeric(as.character(pdataframe$REGN))

df_filtered <- pdataframe[pdataframe$g == 0 | pdataframe$g > 13, ]
subset(df_filtered, g == 11)

# Callaway and Sant'Anna, regression adjustment, X_{g-1}, Z
## ------------------------------------------------------------------------------
cs_x <- att_gt(
  yname = "ROE",
  tname = "t",
  idname = "REGN",
  gname = "g",
  xformla = ~ sys_importance  + Bank_Size_LAG1 + 
    CIR_LAG1 + Z.score_RWA_LAG1 + Leverage_LAG1 + Н3_LAG1| REGN + t + quarter,
  control_group = "notyettreated",  # "notyettreated"
  base_period = "universal",
  est_method = "reg",
  data = pdataframe
)
cs_x_res <- aggte(cs_x, type = "group", na.rm = TRUE)
summary(cs_x_res)

# event study
cs_x_dyn <- aggte(cs_x, type = "dynamic", na.rm = TRUE)
ggdid(cs_x_dyn)




















