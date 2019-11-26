regressors_economic <- c("family_size"
                ,"harvester_members_household"
                ,"expenses"
                ,"yarsha_income_2074"
                ,"highest_price"
                ,"average_price"
                ,"lowest_price"
                ,"method_curing"
                ,"time_curing"
                ,"perception"
                ,"low_price"
                ,"low_productivity"
                ,"weak_coordination"
                ,"packing"
                ,"suitable_month"
                ,"customer_preference"
                ,"benefits"
                ,"demerits"
                ,"geophysical"
                ,"price_variation")

regressors_social <- c("respondent_gender"
                       ,"respondent_age"
                       ,"respondent_education"
                       ,"started_harvesting"
                       ,"time_collection")

  
yarsha_income_model_eco <- lm(reformulate(response = "total_income_2074", 
                                      termlabels = regressors_economic), 
                          data = yarsa)

# # economic model
# model anova
yarsha_income_model_eco %>% 
  anova() %>% 
  broom::tidy() %>% 
  write_csv("./outputs/anova_economic_model.csv", "")

# mode coefficients
yarsha_income_model_eco_sum <- summary(yarsha_income_model_eco)
yarsha_income_model_eco_sum %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression_economic_model.csv")


yarsha_income_model_socioeco <- lm(reformulate(response = "total_income_2074", 
                                      termlabels = c(regressors_economic, 
                                                     regressors_social)), 
                          data = yarsa)

# # socio-economic model
# model anova
yarsha_income_model_socioeco %>% 
  anova() %>% 
  broom::tidy() %>% 
  write_csv("./outputs/anova_socio_economic_model.csv", "")

# mode coefficients
yarsha_income_model_socioeco_sum <- summary(yarsha_income_model_socioeco)
yarsha_income_model_socioeco_sum %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression_socio_economic_model.csv")

yarsha_income_model_eco %>% 
  summary() %>% 
  magrittr::extract(c("adj.r.squared", "fstatistic", "p-value")) %>% 
  map(1) %>% 
  as_tibble() %>% 
  write_csv("./outputs/summary_stat_economic.csv", "")

yarsha_income_model_socioeco %>% 
  summary() %>% 
  magrittr::extract(c("adj.r.squared", "fstatistic")) %>% 
  map(1) %>% 
  as_tibble() %>% 
  write_csv("./outputs/summary_stat_socio_economic.csv", "")
