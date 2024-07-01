



#####################################################
#################### Hannes Bey #####################
##### Master Thesis Script World Value Survey #######
#####################################################




#### Load packages ####
library(tidyverse)
library(haven)
library(car)   
library(psych)
library(RcmdrMisc)
library(lavaan)
library(sjlabelled)
library(mice)
library(md)
library(VIM)
library(broom)
library(mifa)
library(kableExtra)
library(flextable)
library(MVN)
library(sjmisc)
library(mifa)
library(stargazer)

####################
# DATA PREPARATION #
####################

#### loading and subseting data 
# loading data
WVS_TimeSeries_4_0 <- read_dta("WVS_TimeSeries_4_0.dta")
dat_WVS_TS <- WVS_TimeSeries_4_0

# subset time series for Brazil 
dat_WVS_TS <- dat_WVS_TS %>% filter(COUNTRY_ALPHA == "BRA")

## Check data availability
# IV1: Religion
table(dat_WVS_TS$F025, dat_WVS_TS$S020, useNA = "always") # religious denomination - major groups 1991 - 2018
table(dat_WVS_TS$F025_WVS, dat_WVS_TS$S020, useNA = "always") # religious denomination - detailed list 1991 - 2018
table(dat_WVS_TS$F028, dat_WVS_TS$S020, useNA = "always") # religious services attendance 1991 - 2018

# IV: Income 
table(dat_WVS_TS$X047_WVS, dat_WVS_TS$S020) # income

# Controls & Demographics
table(dat_WVS_TS$X051, dat_WVS_TS$S020, useNA = "always") # ethnic group
table(dat_WVS_TS$X025R, dat_WVS_TS$S020, useNA = "always") # education 
table(dat_WVS_TS$X001, dat_WVS_TS$S020, useNA = "always") # gender 
table(dat_WVS_TS$X003, dat_WVS_TS$S020, useNA = "always") # age 
table(dat_WVS_TS$X048WVS, dat_WVS_TS$S020, useNA = "always") # state 
table(dat_WVS_TS$X049, dat_WVS_TS$S020, useNA = "always") # town size

# Sexual Morality Scale 
table(dat_WVS_TS$F118, dat_WVS_TS$S020, useNA = "always") # homosexuality never justifiable = 1; always justifiable = 10
table(dat_WVS_TS$F119, dat_WVS_TS$S020, useNA = "always") # prostitution
table(dat_WVS_TS$F120, dat_WVS_TS$S020, useNA = "always") # abortion
table(dat_WVS_TS$F121, dat_WVS_TS$S020, useNA = "always") # divorce
table(dat_WVS_TS$F135A, dat_WVS_TS$S020, useNA = "always") # sex before marriage
table(dat_WVS_TS$F132, dat_WVS_TS$S020, useNA = "always") # having casual sex

# Inital Redistributive Attitudes Scale 
table(dat_WVS_TS$E035, dat_WVS_TS$S020, useNA = "always") # income inequality 
table(dat_WVS_TS$E036, dat_WVS_TS$S020, useNA = "always") # private vs public ownership 
table(dat_WVS_TS$E037, dat_WVS_TS$S020, useNA = "always") # government provision
table(dat_WVS_TS$E039, dat_WVS_TS$S020, useNA = "always") # competition
table(dat_WVS_TS$E040, dat_WVS_TS$S020, useNA = "always") # meritocracy 
table(dat_WVS_TS$E069_05, dat_WVS_TS$S020, useNA = "always") # trust in labour unions 1 = a great deal; 4 = not very much
table(dat_WVS_TS$E069_13, dat_WVS_TS$S020, useNA = "always") # trust in companies 

# Alternative Redistributive Attitudes Scale
table(dat_WVS_TS$E233A, dat_WVS_TS$S020, useNA = "always") # The state makes people’s incomes equal
table(dat_WVS_TS$E227, dat_WVS_TS$S020, useNA = "always") # People receive state aid for unemployment.
table(dat_WVS_TS$E224, dat_WVS_TS$S020, useNA = "always") # Governments tax the rich and subsidize the poor.

# DV: Voting and deology 
table(dat_WVS_TS$E263, dat_WVS_TS$S020, useNA = "always") # votes in local election
table(dat_WVS_TS$E264, dat_WVS_TS$S020, useNA = "always") # votes in national election
table(dat_WVS_TS$E179_WVS7LOC, dat_WVS_TS$S020, useNA = "always") # party vote (TO MANY NAs!)
table(dat_WVS_TS$E033, dat_WVS_TS$S020, useNA = "always") # ideology

#### select variables
dat_TS_cl <- dat_WVS_TS %>% 
  dplyr::select(S020, S007, X051, X025R, X001, X003, X047_WVS, X048WVS, X049, X050C, # demographics
                E263, E264, E179_WVS7LOC, E033, # dependent variables: voting, ideology
                F025, A006, A065, F028, F028B, F200, F201, F025_WVS, # independent variables: religion 
                F118, F119, F120, F121, F135A, F132, # independent variables: sexual morality
                E035, E036, E037, E039, E040, E069_05, E069_13, # independent variables: economic values
                E233A, E227, E224) # economic values  

dat_TS_cl <- dat_TS_cl %>% dplyr::rename(ID = S007, year = S020, # ID
                                   ethnic = X051, edu = X025R, gndr = X001, age = X003, income = X047_WVS, state = X048WVS, twn_size = X049, ur_ru = X050C, # demographics
                                   vot_loc = E263, vot_nat = E264, vot_party = E179_WVS7LOC, idldgy_right = E033, # dependent variables: voting
                                   rel_dom = F025, rel_imp = A006, rel_memb = A065, chr_att = F028, pray_fr = F028B, nor_good = F200, dth_lf = F201, rel_dom_ext = F025_WVS,  # independent variables: religion 
                                   hmsxl = F118, prost = F119, abort = F120, divorce = F121, sex_mar = F135A, sex_cas = F132,
                                   inc_eq = E035, priv_ow = E036, gov_prov = E037, comp = E039, merit = E040, trust_lun = E069_05, trust_comp = E069_13,
                                   s_tax_rich = E233A, s_rec_aid = E227, s_inc_eq = E224)

#### Data Cleaning
# getting an overview of the data 
head(dat_TS_cl)

# filter for relevant years 
dat_TS_cl <- dat_TS_cl %>% filter(year == 2014 | year == 2018)

# recode numeric values for NAs into "NA" 
dat_TS_cl[dat_TS_cl == -1] <- NA
dat_TS_cl[dat_TS_cl == -2] <- NA
dat_TS_cl[dat_TS_cl == -3] <- NA
dat_TS_cl[dat_TS_cl == -4] <- NA
dat_TS_cl[dat_TS_cl == -5] <- NA

# recoding continous variables as numeric
dat_TS_cl <- dat_TS_cl %>% mutate_at(c('age', 'income', 'idldgy_right', 'rel_imp', 'chr_att',  'pray_fr', 
                                  'hmsxl', 'prost', 'abort', 'divorce', 'sex_mar', 
                                  'inc_eq' ,'priv_ow' , 'gov_prov', 'comp', 'merit', 'trust_lun', 'trust_comp',
                                  's_tax_rich', 's_rec_aid', 's_inc_eq'), as.numeric)

# recoding categorical variables as factor
dat_TS_cl <- dat_TS_cl %>% mutate_at(c('ethnic', 'edu', 'rel_memb', 'gndr', 'rel_dom', 'year', 'twn_size', 'state'), as.factor)

## recoding numeric variables
dat_TS_cl <- dat_TS_cl %>%
  mutate(
    # reverse coding church attendance, membership, and importance 
    chr_att = dplyr::recode(chr_att, '1' = '8', '2' = '7', '3' = '6', '4' = '5', '5' = '4', # reverse coded so that higher values indicate more redistribution
                            '6' = '3', "7" = '2', '8' = '1'),
    rel_memb = dplyr::recode(rel_memb, '0' = '0', '1' = '1', '2' = '2'),
    rel_imp = dplyr::recode(rel_imp, '1' = '4', '2' = '3', '3' = '2', '4' = '1'),
    # religious denomination 
    rel_dom = dplyr::recode(rel_dom, '0' = '0', '1' = '1', '2' = '2', '3' = '3',
                            '4' = '3', '5' = '3', '6' = '3', '7' = '3', '8' =  '3',
                            '9' = '3'),
    # recoding economic values
    inc_eq = dplyr::recode(inc_eq, '1' = '10', '2' = '9', '3' = '8',   # reverse coded so that higher values indicate more redistribution
                           '4' = '7', '5' = '6', '6' = '5', '7' = '4',
                           '8' = '3', '9' = '2', '10' = '1'),
    priv_ow = dplyr::recode(priv_ow, '1' = '1', '2' = '2', '3' = '3',
                            '4' = '4', '5' = '5', '6' = '6', '7' = '7',
                            '8' = '8', '9' = '9', '10' = '10'),
    gov_prov = dplyr::recode(gov_prov, '1' = '10', '2' = '9', '3' = '8',   # reverse coded so that higher values indicate more redistribution
                             '4' = '7', '5' = '6', '6' = '5', '7' = '4',
                             '8' = '3', '9' = '2', '10' = '1'),
    comp = dplyr::recode(comp, '1' = '1', '2' = '2', '3' = '3',
                         '4' = '4', '5' = '5', '6' = '6', '7' = '7',
                         '8' = '8', '9' = '9', '10' = '10'),
    merit = dplyr::recode(merit, '1' = '1', '2' = '2', '3' = '3',
                          '4' = '4', '5' = '5', '6' = '6', '7' = '7',
                          '8' = '8', '9' = '9', '10' = '10'))

## recode extensive religious denomination variable (just to check if it matches the recoded WVS variable)
table(dat_WVS_TS$F025_WVS, dat_WVS_TS$S020, useNA = "always") # religious denomination - detailed list 1991 - 2018
table(dat_TS_cl$rel_dom_ext, useNA = "always") 

# extract labels
get_labels(dat_WVS_TS$F025_WVS, drop.unused = TRUE, values = 'n')
get_labels(dat_TS_cl$rel_dom_ext, drop.unused = TRUE, values = 'n')

# recode as factor
dat_TS_cl <- dat_TS_cl %>% 
  mutate(rel_dom_ext = as.factor(rel_dom_ext))

# add factor levels
levels(dat_TS_cl$rel_dom_ext) <- c("Catholic", "Catholic", "Trad_prot", 
                                   "Evangelical", "Evangelical", "Other", 
                                   "Other", "Other",  "Other" , "Other",
                                   "Afro_bras",  "Spiritism", 
                                   "Non-religious")

# inspect and compare results with initial variable (they are the same, so I continued with the initial rel_dom variable)
table(dat_TS_cl$rel_dom_ext, useNA = "always")
table(dat_TS_cl$rel_dom, useNA = "always") 

## add factor levels to other factor variables
levels(dat_TS_cl$gndr) <- c("male", "female")
levels(dat_TS_cl$ethnic) <- c("White", "Black", "Brown", "Asian", "Indigenous", "Other")
levels(dat_TS_cl$rel_dom) <- c("None", "Catholic", "Protestant/Evangelical", "Other")
levels(dat_TS_cl$ethnic) <- c("White", "Black", "Brown", "Asian", "Indigenous", "Other")
levels(dat_TS_cl$rel_memb) <- c("None", "Passive", "Active")
levels(dat_TS_cl$edu) <- c("Lower", "Middle", "Upper")
levels(dat_TS_cl$twn_size) <- c("<2000", "2-5000", "5-10000", "10-20000", "20-50000", "50-100000", "100000-500000", ">500000")


## recoding relevant variables as numeric
dat_TS_cl <- dat_TS_cl %>% mutate_at(c('inc_eq' ,'chr_att', 'priv_ow' , 'gov_prov', 'comp', 'merit',
                                 'rel_imp', 'hmsxl', 'prost', 'abort', 'divorce', 'sex_mar', 'sex_cas', 
                                 'inc_eq' ,'priv_ow' , 'gov_prov', 'comp', 'merit', 'trust_lun', 'trust_comp'), as.numeric)

## creat final dataset for EFA and Regression analyses ##
dat_final <- dat_TS_cl %>% 
  dplyr::select(hmsxl, abort, sex_mar, prost, divorce, chr_att,
                s_tax_rich, s_inc_eq, s_rec_aid, 
                rel_dom, income,
                edu, age, state, ethnic, gndr, state, year, ID, twn_size,
                idldgy_right)



#######################
# Exploratory Analysis#
#######################



## Exploring Items for Factor Analysis
# select relevant variables
dat_exp_fa <- dat_TS_cl %>% dplyr::select('rel_dom', 'chr_att', 'income', 'edu', 'age', 'state', 'ethnic', 'gndr', 'hmsxl', 'abort', 'sex_mar', 'prost', 'divorce', 
                                           'inc_eq' ,'priv_ow' , 'gov_prov', 'comp', 'merit', 'trust_lun', 'trust_comp',
                                           's_tax_rich', 's_inc_eq', 's_rec_aid')

# impute values using MICE 
dat_exp_fa_imp <- mice::complete(mice::mice(dat_exp_fa, print = FALSE, m = 20, seed = 12345))

# dataframe for cor_matrix
dat_exp_fa_cor <- dat_exp_fa_imp %>% dplyr::select('hmsxl', 'abort', 'sex_mar', 'prost', 'divorce', 
                                          'inc_eq' ,'priv_ow' , 'gov_prov', 'comp', 'merit', 'trust_lun', 'trust_comp',
                                          's_tax_rich', 's_inc_eq', 's_rec_aid')

# inspect correlation matrix 
write.csv(cor(dat_exp_fa_cor), file="Correlation_Values_total.csv")



######################
# Main ANALYSIS: EFA #
######################


#### Exploratory Factor Analysis ####
# create dataset for factor analysis variables
dat_TS_cl_fa <- dat_final %>% 
  dplyr::select(hmsxl, abort, sex_mar, prost, divorce, 
                s_tax_rich, s_inc_eq, s_rec_aid, 
                rel_dom, chr_att, income,
                edu, age, state, ethnic, gndr, twn_size)

# impute missing values using mifa (based on MICE)
mi <- mifa(
  dat_TS_cl_fa,
  cov_vars =c(hmsxl, abort, sex_mar, prost, divorce, 
              s_tax_rich, s_inc_eq, s_rec_aid),
  ci = "fieller", 
  conf = 0.95,
  m = 20,
  seed = 12345
)
mi

# inspect principal components
mi # two principal components explain more than 50% of the variance

## calculate KMO and normality test
# calculated complete imputed dataset 
dat_TS_cl_imp <- mice::complete(mice::mice(dat_TS_cl_fa, print = FALSE, m = 20, seed = 12345))

# testing whether FA is appropriate with KMO and Sphericity test
dat_TS_cl_imp_KMO <- dat_TS_cl_imp %>% 
  dplyr::select('hmsxl', 'abort', 'sex_mar', 'prost', 'divorce', 's_tax_rich', 's_inc_eq', 's_rec_aid')
KMO(dat_TS_cl_imp_KMO) # 0.70 = acceptable 
cortest.bartlett(dat_TS_cl_imp_KMO) # successfully rejected the null

# testing for multivariate normality
mvn(dat_TS_cl_imp_KMO, covariance = TRUE, multivariatePlot = "qq", mvnTest = "mardia") # normality violation detected

## run factor analysis for 1-4 factor solution with principal axis factoring 
fit1 <- fa(mi$cov_combined, n.obs = nrow(dat_TS_cl_fa), nfactors = 1,  fm = "pa", rotate = "oblimin")
fit2 <- fa(mi$cov_combined, n.obs = nrow(dat_TS_cl_fa), nfactors = 2,  fm = "pa", rotate = "oblimin")
fit3 <- fa(mi$cov_combined, n.obs = nrow(dat_TS_cl_fa), nfactors = 3,  fm = "pa", rotate = "oblimin")
fit4 <- fa(mi$cov_combined, n.obs = nrow(dat_TS_cl_fa), nfactors = 4,  fm = "pa", rotate = "oblimin", SMC=FALSE)

# inspect different factor loading structures 
fit1$loadings
fit2$loadings
fit3$loadings
fit4$loadings

## creat table with factor loadings, communalities and descriptives for two factor solution
# one factor solution
write.csv(fit1$communality, file = "communalities1.csv")
write.csv(fit1$loadings, file = "loadings1.csv")
# two factor solution 
write.csv(fit2$communality, file = "communalities2.csv")
write.csv(fit2$loadings, file = "loadings2.csv")
# three factor solution 
write.csv(fit3$communality, file = "communalities3.csv")
write.csv(fit3$loadings, file = "loadings3.csv")
# four factor solution 
write.csv(fit4$communality, file = "communalities4.csv")
write.csv(fit4$loadings, file = "loadings4.csv")

# Calculate mean and SD
summary_df <- dat_TS_cl_imp %>%
  summarise(across(c('hmsxl', 'abort', 'sex_mar', 'prost', 'divorce', 's_tax_rich', 's_inc_eq', 's_rec_aid'), list(mean = ~mean(.), sd = ~sd(.))))
write.csv(summary_df, file = "summary_df.csv")

### calculate inter-item correlations
## sexual morality
# create df
sex_mor_df <- dat_TS_cl_imp %>% 
  dplyr::select('hmsxl', 'abort', 'sex_mar', 'prost', 'divorce')

# Compute the correlation matrix
cor_matrix_sex_mor <- cor(sex_mor_df)

# Extract the upper triangle of the correlation matrix, excluding the diagonal
upper_tri_SM <- cor_matrix_sex_mor[upper.tri(cor_matrix_sex_mor)]

# Calculate the mean of the upper triangular values 
mean(upper_tri_SM) # Mr = 0.360

## redistributive attitudes
# create df
red_att_df <- dat_TS_cl_imp %>% 
  dplyr::select('s_tax_rich', 's_inc_eq', 's_rec_aid')

# Compute the correlation matrix
cor_matrix_red_att <- cor(red_att_df)

# Extract the upper triangle of the correlation matrix, excluding the diagonal
upper_tri_RA <- cor_matrix_red_att[upper.tri(cor_matrix_red_att)]

# Calculate the mean of the upper triangular values = 0.212
mean(upper_tri_RA) # Mr = 0.212

## Calculate mean and SD of the scales
# create scales
dat_TS_cl_imp <- dat_TS_cl_imp %>% mutate(sex_mor = rowMeans(cbind(hmsxl, abort, sex_mar, divorce, prost)))
dat_TS_cl_imp <- dat_TS_cl_imp %>% mutate(red_att = rowMeans(cbind(s_tax_rich, s_inc_eq, s_rec_aid)))

# Sexual Morality
mean(dat_TS_cl_imp$sex_mor) # 4.503941
SD(dat_TS_cl_imp$sex_mor) # 2.218795

# Redistributive Attitudes
mean(dat_TS_cl_imp$red_att) # 5.892447
SD(dat_TS_cl_imp$red_att) # 2.279107

## check correlations with church attendance and income
cor(dat_TS_cl_imp$sex_mor, dat_TS_cl_imp$chr_att)
cor(dat_TS_cl_imp$red_att, dat_TS_cl_imp$income)



####################################
# Main Analysis: Regression Models #
####################################



## Impute data for regression analysis 
# Perform the MICE imputation
dat_final_imp <- mice::mice(dat_final, m = 20, print = FALSE, seed = 12345)

# inspecting results of missing values: no trends detected in imputed means 
plot(dat_final_imp) 

# Complete the imputed datasets and store them in a list
dat_final_imp_sets <- mice::complete(dat_final_imp, action = "long", include = TRUE) %>%
  group_by(.imp) %>%
  nest()

# Add and normalize new variables for red_att and sex_more in each dataset and creaty dummy for church attendance 
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(sex_mor = hmsxl + abort + sex_mar + divorce + prost,
                             sex_mor = scale(sex_mor, center = FALSE, scale = max(abs(sex_mor), na.rm = TRUE)),
                             red_att = s_tax_rich + s_inc_eq + s_rec_aid,
                             red_att = scale(red_att, center = FALSE, scale = max(abs(red_att), na.rm = TRUE)),
                             chr_att_DV = scale(chr_att, center = FALSE, scale = max(abs(chr_att), na.rm = TRUE))))) %>%
  mutate(data = map(data, ~ .x %>%
                    mutate(chr_att_gr = case_when(
                      chr_att <= 5   ~ "Less than once a month",          
                      chr_att >= 6 ~ "At least once a month"))  %>% 
                    mutate(chr_att_gr = as.factor(chr_att_gr)))) %>%
  mutate(data = map(data, ~ .x %>% 
                    mutate(chr_att_gr = relevel(chr_att_gr, ref = "Less than once a month"))))
    
# specific numeric predictors to standardize 
cols_to_standardize <- c("income", "age", "chr_att")

# rescale all numeric predictors 
dat_final_imp_sets <- dat_final_imp_sets %>% 
  mutate(data = map(data, ~ .x %>%
                      mutate(across(all_of(cols_to_standardize), ~ (. - mean(., na.rm = TRUE)) / (2 * sd(., na.rm = TRUE))))))

#### Descriptives ####

## create complete dataset
dat_final_imp_comp <- mice::complete(dat_final_imp)
dat_final_imp_comp <- dat_final_imp_comp  %>%
  mutate(chr_att_gr = case_when(
                      chr_att <= 5   ~ "Less than once a month",          
                      chr_att >= 6 ~ "At least once a month")) %>% 
  mutate(chr_att_gr = as.factor(chr_att_gr)) %>%
  mutate(chr_att_gr = relevel(chr_att_gr, ref = "Less than once a month"))

# select relevant variables 
dat_final_imp_comp_desc <- dat_final_imp_comp %>%
  dplyr::select('hmsxl', 'abort', 'sex_mar', 'prost', 'divorce', 's_tax_rich', 's_inc_eq', 's_rec_aid')

# calculate mean and SD of continuous variables
means <- sapply(dat_final_imp_comp_desc, mean)
sds <- sapply(dat_final_imp_comp_desc, sd)

# Combine means and standard deviations into a single data frame
summary_stats <- data.frame(Mean = means, SD = sds)

# Use stargazer to create a summary table
stargazer(summary_stats, type = "text", summary = FALSE, out = "summary_table.csv")

#### Mean Plots
### Plots for Religious Denomination
# Group means and SDs
group_means <- dat_final_imp_comp %>%
  mutate(sex_mor = rowMeans(cbind(hmsxl, abort, sex_mar, divorce, prost)),
         red_att = rowMeans(cbind(s_tax_rich, s_inc_eq, s_rec_aid)))  %>%
  group_by(rel_dom) %>%
  summarise(mean_hmsxl = mean(hmsxl),
            SD_hmsxl = SD(hmsxl),
            mean_sex_mar = mean(sex_mar),
            SD_sex_mar = SD(sex_mar),
            mean_abort = mean(abort),
            SD_abort = SD(abort),
            mean_prost = mean(prost),
            SD_prost = SD(prost),
            mean_divorce = mean(divorce),
            SD_divorce = SD(divorce),
            mean_s_tax_rich = mean(s_tax_rich),
            SD_s_tax_rich = SD(s_tax_rich),
            mean_s_inc_eq = mean(s_inc_eq),
            SD_s_inc_eq = SD(s_inc_eq),
            mean_s_rec_aid = mean(s_rec_aid),
            SD_s_rec_aid = SD(s_rec_aid),
            mean_sex_mor = mean(sex_mor),
            SD_sex_mor = SD(sex_mor),
            mean_red_att = mean(red_att),
            SD_red_att = SD(red_att),
            rel_dom_n = n())

# group sizes religion
group_sizes <- data.frame(
  rel_dom = c("Catholic", "Protestant/Evangelical", "None"),
  n = c(1632, 907, 509)  # 
)

# pivot group means to long format
group_means_long <- group_means %>%
  filter(rel_dom != "Other") %>%  
  dplyr::select(rel_dom, mean_hmsxl, mean_abort, mean_sex_mar, mean_prost, mean_divorce, mean_s_tax_rich, mean_s_inc_eq, mean_s_rec_aid, mean_sex_mor, mean_red_att)  %>%
  pivot_longer(
    cols = -rel_dom,  
    names_to = "variable",
    values_to = "mean"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  left_join(group_sizes, by = "rel_dom") 

# pivot standard deviations to long format
group_SDs_long <- group_means %>%
  filter(rel_dom != "Other") %>%  
  dplyr::select(rel_dom, SD_hmsxl, SD_abort, SD_sex_mar, SD_prost, SD_divorce, SD_s_tax_rich, SD_s_inc_eq, SD_s_rec_aid, SD_sex_mor, SD_red_att)  %>%
  pivot_longer(
    cols = -rel_dom,  
    names_to = "variable",
    values_to = "SD"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  dplyr::select(-measure)

# merge SDs and means again
df_plots <- inner_join(group_means_long, group_SDs_long, by = c("rel_dom", "metric"))

# calculating se and confidence intervals for error bars
df_plots <- df_plots %>%
  mutate(
    se = SD / sqrt(n),
    se_lower = mean - se,
    se_upper = mean + se,
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
    ci_lower = mean - ic,
    ci_upper = mean + ic
  )

## Sexual Morality Plot
# relabel variables and labels
df_plots_sex_mor <- df_plots %>%
  filter(metric == "hmsxl" | metric == "abort" | metric == "sex_mar" | metric == "prost"| metric == "divorce" | metric == "sex_mor") %>%
  rename(Religious_Denomination = rel_dom) %>% 
  mutate(metric = fct_recode(metric,
                        "Homosexuality" = "hmsxl",
                        "Abortion" = "abort",
                        "Sex before Marriage" = "sex_mar",
                        "Prostitution" = "prost",
                        "Divorce" = "divorce",
                        "Total Sexual Morality Score" = "sex_mor"), 
         ) %>%
  mutate(Religious_Denomination = fct_relevel(Religious_Denomination, "Catholic", "Protestant/Evangelical", "None"))

# construct mean plot
plot <- ggplot(df_plots_sex_mor, aes(x = mean, y = metric, fill = Religious_Denomination)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Religious Denominations",
       x = "",
       y = "",
       fill = "Religious Denomination") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Sexual Morality Score", 
                                               expression(bold("Total Sexual Morality Score")), 
                                               x)) +
  theme_minimal() 
plot

## Redistributive attitudes plot Plots_inc_eq, SD_s_rec_aid, SD_sex_mor
# relabel variables and labels
df_plots_red_att <- df_plots %>%
  filter(metric == "s_tax_rich" | metric == "s_inc_eq" | metric == "s_rec_aid" | metric == "red_att") %>%
  rename(Religious_Denomination = rel_dom) %>% 
  mutate(metric = fct_recode(metric,
                             "Government should tax the rich" = "s_tax_rich",
                             "Govnerment should reduce inequality" = "s_inc_eq",
                             "Government should provide unemployment benefits" = "s_rec_aid",
                             "Total Redistributive Attitudes Score" = "red_att"), 
  )

# construct mean plot
plot <- ggplot(df_plots_red_att, aes(x = mean, y = metric, fill = Religious_Denomination)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Religious Denominations",
       x = "",
       y = "",
       fill = "Religious Denomination") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Redistributive Attitudes Score", 
                                               expression(bold("Total Redistributive Attitudes Score")), 
                                               x)) +
  theme_minimal() 
plot

### Plots for church attendance 
## Create group means and SDs
group_means <- dat_final_imp_comp %>%
  mutate(sex_mor = rowMeans(cbind(hmsxl, abort, sex_mar, divorce, prost)),
         red_att = rowMeans(cbind(s_tax_rich, s_inc_eq, s_rec_aid)))  %>%
  group_by(chr_att_gr) %>%   
  summarise(mean_hmsxl = mean(hmsxl),
            SD_hmsxl = SD(hmsxl),
            mean_sex_mar = mean(sex_mar),
            SD_sex_mar = SD(sex_mar),
            mean_abort = mean(abort),
            SD_abort = SD(abort),
            mean_prost = mean(prost),
            SD_prost = SD(prost),
            mean_divorce = mean(divorce),
            SD_divorce = SD(divorce),
            mean_s_tax_rich = mean(s_tax_rich),
            SD_s_tax_rich = SD(s_tax_rich),
            mean_s_inc_eq = mean(s_inc_eq),
            SD_s_inc_eq = SD(s_inc_eq),
            mean_s_rec_aid = mean(s_rec_aid),
            SD_s_rec_aid = SD(s_rec_aid),
            mean_sex_mor = mean(sex_mor),
            SD_sex_mor = SD(sex_mor),
            mean_red_att = mean(red_att),
            SD_red_att = SD(red_att),
            group_size = n())

# group sizes religion
group_sizes <- data.frame(
  chr_att_gr = c("Less than once a month", "At least once a month"),
  n = c(1183, 2065)  # 
)

# pivot group means to long format
group_means_long <- group_means %>%
  dplyr::select(chr_att_gr, mean_hmsxl, mean_abort, mean_sex_mar, mean_prost, mean_divorce, mean_s_tax_rich, mean_s_inc_eq, mean_s_rec_aid, mean_sex_mor, mean_red_att)  %>%
  pivot_longer(
    cols = -chr_att_gr,  
    names_to = "variable",
    values_to = "mean"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  left_join(group_sizes, by = "chr_att_gr") 

# pivot standard deviations to long format
group_SDs_long <- group_means %>%
  dplyr::select(chr_att_gr, SD_hmsxl, SD_abort, SD_sex_mar, SD_prost, SD_divorce, SD_s_tax_rich, SD_s_inc_eq, SD_s_rec_aid, SD_sex_mor, SD_red_att)  %>%
  pivot_longer(
    cols = -chr_att_gr,  
    names_to = "variable",
    values_to = "SD"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  dplyr::select(-measure)

# merge SDs and means again
df_plots <- inner_join(group_means_long, group_SDs_long, by = c("chr_att_gr", "metric"))

# calculating se and confidence intervals for error bars
df_plots <- df_plots %>%
  mutate(
    se = SD / sqrt(n),
    se_lower = mean - se,
    se_upper = mean + se,
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
    ci_lower = mean - ic,
    ci_upper = mean + ic
  )

## Sexual Morality Plot
# relabel variables and labels
df_plots_sex_mor <- df_plots %>%
  filter(metric == "hmsxl" | metric == "abort" | metric == "sex_mar" | metric == "prost"| metric == "divorce" | metric == "sex_mor") %>%
  rename(Church_Attendance = chr_att_gr) %>% 
  mutate(metric = fct_recode(metric,
                             "Homosexuality" = "hmsxl",
                             "Abortion" = "abort",
                             "Sex before Marriage" = "sex_mar",
                             "Prostitution" = "prost",
                             "Divorce" = "divorce",
                             "Total Sexual Morality Score" = "sex_mor"), 
  )

# construct mean plot
plot <- ggplot(df_plots_sex_mor, aes(x = mean, y = metric, fill = Church_Attendance)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Church Attendance",
       x = "",
       y = "",
       fill = "Church Attendance") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Sexual Morality Score", 
                                               expression(bold("Total Sexual Morality Score")), 
                                               x)) +
  theme_minimal() 
plot

## Redistributive attitudes plot Plots_inc_eq, SD_s_rec_aid, SD_sex_mor
# relabel variables and labels
df_plots_red_att <- df_plots %>%
  filter(metric == "s_tax_rich" | metric == "s_inc_eq" | metric == "s_rec_aid" | metric == "red_att") %>%
  rename(Church_Attendance = chr_att_gr) %>% 
  mutate(metric = fct_recode(metric,
                             "Total Redistributive Attitudes Score" = "red_att",
                             "Government should tax the rich" = "s_tax_rich",
                             "Govnerment should reduce inequality" = "s_inc_eq",
                             "Government should provide unemployment benefits" = "s_rec_aid"), 
  )

plot <- ggplot(df_plots_red_att, aes(x = mean, y = metric, fill = Church_Attendance)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Church Attendance",
       x = "",
       y = "",
       fill = "Church Attendance") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Redistributive Attitudes Score", 
                                               expression(bold("Total Redistributive Attitudes Score")), 
                                               x)) +
  theme_minimal() 
plot

### Plots for Income
group_means <- dat_final_imp_comp %>%
  mutate(sex_mor = rowMeans(cbind(hmsxl, abort, sex_mar, divorce, prost)),
         red_att = rowMeans(cbind(s_tax_rich, s_inc_eq, s_rec_aid)))  %>%
  mutate(inc_gr = case_when(
    income <= 3  ~ "Low",
    income >= 4 & income <= 6 ~ "Medium",
    income > 6  ~ "High")) %>% # 
  group_by(inc_gr) %>%   
  summarise(mean_hmsxl = mean(hmsxl),
            SD_hmsxl = SD(hmsxl),
            mean_sex_mar = mean(sex_mar),
            SD_sex_mar = SD(sex_mar),
            mean_abort = mean(abort),
            SD_abort = SD(abort),
            mean_prost = mean(prost),
            SD_prost = SD(prost),
            mean_divorce = mean(divorce),
            SD_divorce = SD(divorce),
            mean_s_tax_rich = mean(s_tax_rich),
            SD_s_tax_rich = SD(s_tax_rich),
            mean_s_inc_eq = mean(s_inc_eq),
            SD_s_inc_eq = SD(s_inc_eq),
            mean_s_rec_aid = mean(s_rec_aid),
            SD_s_rec_aid = SD(s_rec_aid),
            mean_sex_mor = mean(sex_mor),
            SD_sex_mor = SD(sex_mor),
            mean_red_att = mean(red_att),
            SD_red_att = SD(red_att),
            group_size = n())

# group sizes religion
group_sizes <- data.frame(
  inc_gr = c("High", "Medium", "Low"),
  n = c(442, 1193, 1613)  # 
)

# pivot group means to long format
group_means_long <- group_means %>%
  dplyr::select(inc_gr, mean_hmsxl, mean_abort, mean_sex_mar, mean_prost, mean_divorce, mean_s_tax_rich, mean_s_inc_eq, mean_s_rec_aid, mean_sex_mor, mean_red_att)  %>%
  pivot_longer(
    cols = -inc_gr,  
    names_to = "variable",
    values_to = "mean"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  left_join(group_sizes, by = "inc_gr") 

# pivot standard deviations to long format
group_SDs_long <- group_means %>%
  dplyr::select(inc_gr, SD_hmsxl, SD_abort, SD_sex_mar, SD_prost, SD_divorce, SD_s_tax_rich, SD_s_inc_eq, SD_s_rec_aid, SD_sex_mor, SD_red_att)  %>%
  pivot_longer(
    cols = -inc_gr,  
    names_to = "variable",
    values_to = "SD"
  )  %>%
  separate(variable, into = c("measure", "metric"), sep = "_", extra = "merge") %>%
  dplyr::select(-measure)

# merge SDs and means again
df_plots <- inner_join(group_means_long, group_SDs_long, by = c("inc_gr", "metric"))

# calculating se and confidence intervals for error bars
df_plots <- df_plots %>%
  mutate(
    se = SD / sqrt(n),
    se_lower = mean - se,
    se_upper = mean + se,
    ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
    ci_lower = mean - ic,
    ci_upper = mean + ic
  )

## Sexual Morality Plot
# relabel variables and labels
df_plots_sex_mor <- df_plots %>%
  filter(metric == "hmsxl" | metric == "abort" | metric == "sex_mar" | metric == "prost"| metric == "divorce" | metric == "sex_mor") %>%
  rename(Income_Group = inc_gr) %>% 
  mutate(Income_Group = as.factor(Income_Group)) %>% 
  mutate(metric = fct_recode(metric,
                             "Homosexuality" = "hmsxl",
                             "Abortion" = "abort",
                             "Sex before Marriage" = "sex_mar",
                             "Prostitution" = "prost",
                             "Divorce" = "divorce",
                             "Total Sexual Morality Score" = "sex_mor")) %>% 
  mutate(Income_Group = fct_relevel(Income_Group, "High", "Medium", "Low"))
# construct mean plot
plot <- ggplot(df_plots_sex_mor, aes(x = mean, y = metric, fill = Income_Group)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Income Group",
       x = "",
       y = "",
       fill = "Income Group") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Sexual Morality Score", 
                                               expression(bold("Total Sexual Morality Score")), 
                                               x)) +
  theme_minimal() 
plot


## Redistributive attitudes plot Plots_inc_eq, SD_s_rec_aid, SD_sex_mor
# relabel variables and labels
df_plots_red_att <- df_plots %>%
  filter(metric == "s_tax_rich" | metric == "s_inc_eq" | metric == "s_rec_aid" | metric == "red_att") %>%
  rename(Income_Group = inc_gr) %>% 
  mutate(metric = fct_recode(metric,
                             "Government should tax the rich" = "s_tax_rich",
                             "Govnerment should reduce inequality" = "s_inc_eq",
                             "Government should provide unemployment benefits" = "s_rec_aid",
                             "Total Redistributive Attitudes Score" = "red_att") 
  ) %>% 
  mutate(Income_Group = fct_relevel(Income_Group, "High", "Medium", "Low"),
         metric = fct_relevel(metric, "Total Redistributive Attitudes Score" , "Government should tax the rich", "Govnerment should reduce inequality", "Government should provide unemployment benefits")
         )


plot <- ggplot(df_plots_red_att, aes(x = mean, y = metric, fill = Income_Group)) +  # Assuming 'metric' differentiates bars within each 'rel_dom'
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), 
                width = 0.25, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Group Means by Income Group",
       x = "",
       y = "",
       fill = "Income Group") +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(limits = c(0, 10),  # Set the y-axis to range from 0 to 1
                     breaks = seq(0, 10, by = 1)) +
  scale_y_discrete(labels = function(x) ifelse(x == "Total Redistributive Attitudes Score", 
                                               expression(bold("Total Redistributive Attitudes Score")), 
                                               x)) +
  theme_minimal() 
plot


#### Run regressions ####
# set reference group to either Catholic or None 
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(data = map(data, ~ .x %>%
                      mutate(rel_dom = relevel(rel_dom, ref = "None")))) # exchange for "Catholic" and run models again 

### Fit models to each imputed dataset
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(
    # Model 0: base models 
    lm_red_att_base = map(data, ~ lm(red_att ~ chr_att + rel_dom + income, data = .)),
    lm_sex_more_base = map(data, ~ lm(sex_mor ~ chr_att + rel_dom + income, data = .)),
    # Models 1: Redistributive attitudes
    lm_red_att = map(data, ~ lm(red_att ~  rel_dom + income 
                                + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # adding church attendance
    lm_red_att_chr = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # with interaction income*chr_att
    lm_red_att_int1 = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size  + state 
                                    + income*chr_att, data = .)),
    # with interaction income*rel_dom
    lm_red_att_int2 = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size + state
                                    + income*rel_dom, data = .)),
    # Models 2: Sexual morality
    lm_sex_mor = map(data, ~ lm(sex_mor ~ rel_dom + income 
                                + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # adding church attendance 
    lm_sex_mor_chr = map(data, ~ lm(sex_mor ~  chr_att + rel_dom + income 
                                + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # with interaction rel_dom*chr_att
    lm_sex_mor_int = map(data, ~ lm(sex_mor ~  chr_att + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size + state
                                    + chr_att*rel_dom, data = .)),
    # Model 3: Effect of rel_dom und church attendance 
    lm_chr_att = map(data, ~ lm(chr_att_DV ~  rel_dom + income +
                                    + age + gndr + year + ethnic + edu + twn_size + state, data = .)))


## create model summaries
dat_final_imp_sets <- dat_final_imp_sets %>%
  mutate(lm_red_att_base_summary = map(lm_red_att_base, broom::tidy),
         lm_sex_more_base_summary = map(lm_sex_more_base, broom::tidy),
         lm_red_att_summary = map(lm_red_att, broom::tidy),
         lm_red_att_int1_summary = map(lm_red_att_int1, broom::tidy),
         lm_red_att_int2_summary = map(lm_red_att_int2, broom::tidy),
         lm_sex_mor_summary = map(lm_sex_mor, broom::tidy),
         lm_sex_mor_int_summary = map(lm_sex_mor_int, broom::tidy),
         lm_idldgy_right_summary = map(lm_idldgy_right, broom::tidy),
         lm_idldgy_right_int = map(lm_idldgy_right_int, broom::tidy)
         )

### create pooled analysis 
## lm_red_att_base
# pool coefficients
lm_red_att_base_pol <- pool(dat_final_imp_sets$lm_red_att_base)
lm_red_att_base_pol_t <- broom::tidy(lm_red_att_base_pol)
lm_red_att_base_pol_t
# extract estimates, standard errors and p-values 
lm_red_att_base_pol_t <- lm_red_att_base_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "\n(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_base_pol_t <- lm_red_att_base_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_base_pol_t, file = "lm_red_att_base_pol_t.csv")
lm_red_att_base_pol_t

# calculate pooled r^2
mean(lm_red_att_base_pol$glanced$r.squared)

## lm_sex_mor_base
# pool coefficients
lm_sex_more_base_pol <- pool(dat_final_imp_sets$lm_sex_more_base)
lm_sex_more_base_pol_t <- broom::tidy(lm_sex_more_base_pol)

# extract estimates, standard errors and p-values 
lm_sex_more_base_pol_t <- lm_sex_more_base_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , "\n(", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_sex_more_base_pol_t <- lm_sex_more_base_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_sex_more_base_pol_t, file = "lm_sex_more_base_pol_t.csv")
lm_sex_more_base_pol_t

# calculate pooled r^2
mean(lm_sex_more_base_pol$glanced$r.squared)

## lm_red_att with controls
# pool coefficients
lm_red_att_pol <- pool(dat_final_imp_sets$lm_red_att)
lm_red_att_pol_t <- broom::tidy(lm_red_att_pol)
lm_red_att_pol_t

# extract estimates, standard errors and p-values 
lm_red_att_pol_t <- lm_red_att_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 
lm_red_att_pol_t

# prepare table for output
lm_red_att_pol_t <- lm_red_att_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_pol_t, file = "lm_red_att_pol_t.csv")
lm_red_att_pol_t

# calculate pooled r^2
mean(lm_red_att_pol$glanced$r.squared)

## lm_red_att with controls + church attendance
# pool coefficients
lm_red_att_chr_pol <- pool(dat_final_imp_sets$lm_red_att_chr)
lm_red_att_chr_pol_t <- broom::tidy(lm_red_att_chr_pol)
lm_red_att_chr_pol_t

# extract estimates, standard errors and p-values 
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 
lm_red_att_chr_pol_t

# prepare table for output
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_chr_pol_t, file = "lm_red_att_chr_pol_t.csv")
lm_red_att_chr_pol_t

# calculate pooled r^2
mean(lm_red_att_chr_pol$glanced$r.squared)

## lm red_att with interaction 1
# pool coefficients
lm_red_att_int1_pol <- pool(dat_final_imp_sets$lm_red_att_int1)
lm_red_att_int1_pol1_t <- broom::tidy(lm_red_att_int1_pol)

# extract estimates, standard errors and p-values 
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int1_pol1_t, file = "lm_red_att_int1_pol1_t.csv")
lm_red_att_int1_pol1_t
# calculate pooled r^2
mean(lm_red_att_int1_pol$glanced$r.squared)

## lm red_att with interaction 12
# pool coefficients
lm_red_att_int2_pol <- pool(dat_final_imp_sets$lm_red_att_int2)
lm_red_att_int2_pol_t <- broom::tidy(lm_red_att_int2_pol)
# extract estimates, standard errors and p-values 
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int2_pol_t, file = "lm_red_att_int2_pol_t.csv")
lm_red_att_int2_pol_t
# calculate pooled r^2
mean(lm_red_att_int2_pol$glanced$r.squared)

## lm_sex_mor with controls
# pool coefficients
lm_sex_mor_pol <- pool(dat_final_imp_sets$lm_sex_mor)
lm_sex_mor_t <- broom::tidy(lm_sex_mor_pol)
# extract estimates, standard errors and p-values 
lm_sex_mor_t <- lm_sex_mor_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_sex_mor_t <- lm_sex_mor_t %>% dplyr::select(term, estimate)
write.csv(lm_sex_mor_t, file = "lm_sex_mor_t.csv")
lm_sex_mor_t
# calculate pooled r^2
mean(lm_sex_mor_pol$glanced$r.squared)


## lm_sex_mor with controls + church attendance
# pool coefficients
lm_sex_mor_chr_pol <- pool(dat_final_imp_sets$lm_sex_mor_chr)
lm_sex_mor_chr_t <- broom::tidy(lm_sex_mor_chr_pol)
# extract estimates, standard errors and p-values 
lm_sex_mor_chr_t <- lm_sex_mor_chr_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_sex_mor_chr_t <- lm_sex_mor_chr_t %>% dplyr::select(term, estimate)
write.csv(lm_sex_mor_chr_t, file = "lm_sex_mor_chr_t")
lm_sex_mor_chr_t

# calculate pooled r^2
mean(lm_sex_mor_chr_pol$glanced$r.squared)

# predicting church attendance lm_chr_att
lm_chr_att_pol <- pool(dat_final_imp_sets$lm_chr_att)
lm_chr_att_pol_t <- broom::tidy(lm_chr_att_pol)
# extract estimates, standard errors and p-values 
lm_chr_att_pol_t <- lm_chr_att_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_chr_att_pol_t <- lm_chr_att_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_chr_att_pol_t, file = "lm_chr_att_pol_t.csv")
lm_chr_att_pol_t

# calculate pooled r^2
mean(lm_chr_att_pol$glanced$r.squared)


#### Robustness checks ####
## Categorical measure for church attendance 
# Run models with categorical measure - chr_att_gr
dat_final_imp_sets_rob <- dat_final_imp_sets %>%
  mutate(
    # Model 1: Redistributive attitudes with  church attendance
    lm_red_att_chr = map(data, ~ lm(red_att ~ chr_att_gr + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # with interaction income*chr_att
    lm_red_att_int1 = map(data, ~ lm(red_att ~ chr_att_gr + rel_dom + income 
                                     + age + gndr + year + ethnic + edu + twn_size  + state 
                                     + income*chr_att_gr, data = .)),
    # with interaction income*rel_dom
    lm_red_att_int2 = map(data, ~ lm(red_att ~ chr_att_gr + rel_dom + income 
                                     + age + gndr + year + ethnic + edu + twn_size + state
                                     + income*rel_dom, data = .)),
    # Model 2: Sexual morality with church attendance 
    lm_sex_mor_chr = map(data, ~ lm(sex_mor ~  chr_att_gr + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size + state, data = .)))

## lm_red_att with controls + church attendance
# pool coefficients
lm_red_att_chr_pol <- pool(dat_final_imp_sets_rob$lm_red_att_chr)
lm_red_att_chr_pol_t <- broom::tidy(lm_red_att_chr_pol)
lm_red_att_chr_pol_t

# extract estimates, standard errors and p-values 
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 
lm_red_att_chr_pol_t

# prepare table for output
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_chr_pol_t, file = "lm_red_att_chr_pol_t.csv")
lm_red_att_chr_pol_t

# calculate pooled r^2
mean(lm_red_att_chr_pol$glanced$r.squared)

## lm red_att with interaction 1
# pool coefficients
lm_red_att_int1_pol <- pool(dat_final_imp_sets_rob$lm_red_att_int1)
lm_red_att_int1_pol1_t <- broom::tidy(lm_red_att_int1_pol)

# extract estimates, standard errors and p-values 
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int1_pol1_t, file = "lm_red_att_int1_pol1_t.csv")
lm_red_att_int1_pol1_t
# calculate pooled r^2
mean(lm_red_att_int1_pol$glanced$r.squared)

## lm red_att with interaction 12
# pool coefficients
lm_red_att_int2_pol <- pool(dat_final_imp_sets_rob$lm_red_att_int2)
lm_red_att_int2_pol_t <- broom::tidy(lm_red_att_int2_pol)
# extract estimates, standard errors and p-values 
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int2_pol_t, file = "lm_red_att_int2_pol_t.csv")
lm_red_att_int2_pol_t
# calculate pooled r^2
mean(lm_red_att_int2_pol$glanced$r.squared)

## lm_sex_mor with controls + church attendance
# pool coefficients
lm_sex_mor_chr_pol <- pool(dat_final_imp_sets_rob$lm_sex_mor_chr)
lm_sex_mor_chr_t <- broom::tidy(lm_sex_mor_chr_pol)
# extract estimates, standard errors and p-values 
lm_sex_mor_chr_t <- lm_sex_mor_chr_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_sex_mor_chr_t <- lm_sex_mor_chr_t %>% dplyr::select(term, estimate)
write.csv(lm_sex_mor_chr_t, file = "lm_sex_mor_chr_t.csv")
lm_sex_mor_chr_t

# calculate pooled r^2
mean(lm_sex_mor_chr_pol$glanced$r.squared)


## Interaction effects: Income*Church_att / Income*rel_dom
### Fit models to each imputed dataset
dat_final_imp_sets_ <- dat_final_imp_sets %>%
  mutate(
    # Models 1: Redistributive attitudes bas model with church attendance
    lm_red_att_chr = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                    + age + gndr + year + ethnic + edu + twn_size + state, data = .)),
    # with interaction income*chr_att
    lm_red_att_int1 = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                     + age + gndr + year + ethnic + edu + twn_size  + state 
                                     + income*chr_att, data = .)),
    # with interaction income*rel_dom
    lm_red_att_int2 = map(data, ~ lm(red_att ~ chr_att + rel_dom + income 
                                     + age + gndr + year + ethnic + edu + twn_size + state
                                     + income*rel_dom, data = .)))

### create pooled analysis
## lm_red_att with controls
# pool coefficients
lm_red_att_pol <- pool(dat_final_imp_sets$lm_red_att)
lm_red_att_pol_t <- broom::tidy(lm_red_att_pol)
lm_red_att_pol_t

## lm_red_att with controls + church attendance
# pool coefficients
lm_red_att_chr_pol <- pool(dat_final_imp_sets$lm_red_att_chr)
lm_red_att_chr_pol_t <- broom::tidy(lm_red_att_chr_pol)
lm_red_att_chr_pol_t

# extract estimates, standard errors and p-values 
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 
lm_red_att_chr_pol_t

# prepare table for output
lm_red_att_chr_pol_t <- lm_red_att_chr_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_chr_pol_t, file = "lm_red_att_chr_pol_t.csv")
lm_red_att_chr_pol_t

# calculate pooled r^2
mean(lm_red_att_chr_pol$glanced$r.squared)

## lm red_att with interaction 1
# pool coefficients
lm_red_att_int1_pol <- pool(dat_final_imp_sets$lm_red_att_int1)
lm_red_att_int1_pol1_t <- broom::tidy(lm_red_att_int1_pol)

# extract estimates, standard errors and p-values 
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int1_pol1_t <- lm_red_att_int1_pol1_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int1_pol1_t, file = "lm_red_att_int1_pol1_t.csv")
lm_red_att_int1_pol1_t
# calculate pooled r^2
mean(lm_red_att_int1_pol$glanced$r.squared)

## lm red_att with interaction 12
# pool coefficients
lm_red_att_int2_pol <- pool(dat_final_imp_sets$lm_red_att_int2)
lm_red_att_int2_pol_t <- broom::tidy(lm_red_att_int2_pol)
# extract estimates, standard errors and p-values 
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    estimate = paste0(round(estimate, 3),stars , " (", round(std.error, 3), ")")  # Append stars to estimates
  ) 

# prepare table for output
lm_red_att_int2_pol_t <- lm_red_att_int2_pol_t %>% dplyr::select(term, estimate)
write.csv(lm_red_att_int2_pol_t, file = "lm_red_att_int2_pol_t.csv")
lm_red_att_int2_pol_t
# calculate pooled r^2
mean(lm_red_att_int2_pol$glanced$r.squared)





