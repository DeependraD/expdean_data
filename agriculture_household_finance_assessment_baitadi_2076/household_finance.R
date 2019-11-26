# load libraries
require(tidyverse)
require(tidymodels)
library(gridExtra)

# import data
hh_finance_data <- readxl::read_xlsx("./data/data.xlsx")
hh_finance_data <- hh_finance_data %>% 
  rename_all(function(x)str_replace_all(x, "\ ", "_")) %>% 
  rename_all(function(x)str_replace_all(x, c("\\ " = "_", "\\(" = "_", 
                                             "\\)" = "", "\\/" = "_per_"))) %>%
  mutate(Farm_size = if_else(!is.na(Land_lease_size), 
                             Land_own_size + Land_lease_size, 
                             Land_own_size)) %>% 
  mutate(Farm_size_category = cut(`Farm_size`, 
                                  breaks = c(0, 10, 20, 30, 40, 50), 
                                  labels = c("0-10", "10-20", "20-30", "30-40", "40-50")) %>% 
           fct_explicit_na(na_level = "Unknown")) %>%
  mutate(Yearly_earning_cat = cut(`Yearly_earning`, 
                                  breaks = c(0, 150000, 300000, 700000), 
                                  labels = c("0-150000", "150000-300000", "300000-700000"))) %>% 
  mutate(Total_irrigated_cat = cut(`Total_irrigated`, 
                                   breaks = c(0, 2.5, 5), 
                                   labels = c("0-2.5", "2.5-5"), 
                                   include.lowest = T)) %>% 
  mutate(Household_size_cat = cut(`Household_size`, 
                                  breaks = c(0, 5, 10, 15), 
                                  labels = c("0-5", "5-10", "10-15"), 
                                  include.lowest = T)) %>% 
  mutate(Staple_purchase_months_cat = cut(`Staple_purchase_months`, 
                                          breaks = c(0, 2, 4, 6), 
                                          labels = c("0-2", "2-4", "4-6"), 
                                          include.lowest = T)) %>% 
  mutate(Saving_cat_yearly_bin = as_factor(if_else(Saving_value_yearly == 0, 
                                                   "No saving", "Has saving"))) %>% 
  mutate(Saving_cat_yearly_mul = cut(`Saving_value_yearly`, 
                                     breaks = c(0, 50000, 100000, 500000), 
                                     labels = c("0-50000", "50000-100000", 
                                                "100000-Above")) %>% 
           fct_explicit_na(na_level = "No saving")) %>% 
  mutate(Loan_received_cat = as_factor(if_else(Loan_received == 0, 
                                               "No loan received", "Loan received"))) %>% 
  mutate_at("Saving_financial_institution", function(x)factor(x, 
                levels = c("None", "Bank", "Cooperative", "Relative"),
                labels = c("None", "Bank", "Cooperative", "Relative"))) %>% 
  mutate_at(c("Gender", "Ethnicity", 
              "Education", "Occupation", 
              "Irrigation_source", "Annual_cropping_summ_win", 
              "Annual_cropping_spring", "Barrier_saving", 
              "Domestic_external_transaction", "Transaction_purpose", 
              "Loan_application_institution", "Loan_purpose", 
              "Loan_avoid_factor"), as.factor)

# # descriptive summary
# descriptive plots
# # count is short-hand for group_by() and count()
# hh_finance_data %>%
#   group_by(Education_status) %>% 
#   tally()

# Frequency distribution table
freqdist <- hh_finance_data %>%
  select(c("Gender", "Ethnicity", "Age", "Education", 
           "Occupation", "Household_size", "Staple_purchase_months")) %>% 
  mutate(Age = cut(Age, 
                   breaks = c(0, 25, 50, 80), 
                   labels = c("0-25", "26-50", "51-80")), 
         `Household_size` = cut(`Household_size`, 
                             breaks = c(0, 5, 10, 15, 20), 
                             labels = c("1-5", "5-10", "10-15", "15-20")), 
         `Staple_purchase_months` = cut(`Staple_purchase_months`, 
                             breaks = c(1, 4, 8, 12), 
                             labels = c("1-4", "4-8", "8-12"))) %>% 
  mutate_at("Staple_purchase_months", list(~replace_na(as.character(.), "No"))) %>% 
  gather(key = "Socio-economic variables", value = "Category") %>% 
  group_by(`Socio-economic variables`) %>% 
  count(Category, name = "Frequency") %>% 
  mutate(Percentage = Frequency/sum(Frequency)*100) %>% 
  ungroup() %>% 
  mutate_at("Socio-economic variables", 
            function(x)str_replace_all(x, "_", "\\ "))

# # write to csv
# freqdist %>%
#   write_csv("./outputs/hh_baitadi_socio_economic_descriptive.csv", " ")

# faceted pie
# Create a basic bar
freqdis_split <- freqdist %>%
  group_split(`Socio-economic variables`) %>%
  set_names(map(., ~unique(.x$`Socio-economic variables`)))

fpie <- map2(.x = freqdis_split,
             .y = names(freqdis_split),
             .f = ~ ggplot(.x,
                           aes(x="", y=Frequency, fill=Category)) +
               geom_bar(stat="identity", width=1) +
               # Convert to pie (polar coordinates) and add labels
               coord_polar("y", start=0) +
               geom_text(aes(label = paste0(round(Percentage), "%")),
                         position = position_stack(vjust = 0.5), size = 4) +
               # Add color scale (hex colors)
               # scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858",
               #                            "#F6AE2D", "#F26419")) +
               # use fill_viridis_d() for discrete values
               scale_fill_viridis_d() +
               # faceting will add label to individual plot at least
               # facet_wrap(~`Socio-economic variables`) +
               # Remove labels and add title
               labs(x = NULL, y = NULL,
                    fill = NULL,
                    title = .y) +
               # Tidy up the theme
               theme_classic() +
               theme(axis.line = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     plot.title = element_text(hjust = 0.5, color = "#666666"))
)

# # arrange pie plots in grid and compose a file
# ggsave(filename = "./socio_economic_household_finance.png",
#        arrangeGrob(grobs = fpie, ncol = 3,
#                    top = "Percentage of response category for socio-economic variables in household samples"),
#        width = 14, height = 12, device = "png", dpi = 240) ## save plot

# # separate files for each pie
# walk2(fpie, names(freqdis_split),
#       ~ggsave(filename = paste("./outputs/", .y, "_pie.png", sep = ""),
#               .x, width = 4, height = 3, device = "png", dpi = 240))

# frequency distribution of socio-economic variables grouped by landholding
hh_finance_by_holding <- hh_finance_data %>% 
  # group_by(Farm_size_category) %>% # to group by farm size category
  # summarise_if(is.numeric, list(~mean(., na.rm = TRUE))) %>% 
  select_if(is.factor) %>% 
  gather(key = "Catvars", value = "Value", -Farm_size_category) %>% 
  group_by(Farm_size_category, Catvars, Value) %>%
  tally() %>%
  ungroup() %>% 
  group_split(Catvars) %>%
  set_names(map_chr(., ~unique(.x$Catvars))) %>% 
  # map(~rename(., !!unique(.$Catvars) := "Value")) %>% 
  map(. %>% select(-Catvars)) %>% 
  map2(.x = ., 
       .y = names(.), 
       ~.x %>% rename(!!.y := "Value"))

# # write to external file
# walk2(hh_finance_by_holding,
#       names(hh_finance_by_holding),
#       .f = ~write_csv(.x, path = paste0("./outputs/", .y, "_by_holding", ".csv", collapse = ""),
#                       na = ""))

### Inferential statistics
# # model formulation
# general linear model
# yearly earning model
yearly_earning_form <- reformulate(response = "Yearly_earning", 
                                   c("Gender", 
                                     "Ethnicity", 
                                     "Age", 
                                     "Education", 
                                     "Occupation",
                                     "Household_size",
                                     "Farm_size",
                                     "Total_irrigated",
                                     "Staple_purchase_months",
                                     "Saving_cat_yearly_bin",
                                     "Saving_financial_institution",
                                     "Loan_received_cat"
                                   ))

yearly_earning_model <- lm(yearly_earning_form, 
                  data = hh_finance_data)

# coefficients
yearly_earning_model %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression/earning_estimates.csv", "")

# anova
anova(yearly_earning_model) %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression/earning_anova.csv", "")

# yearly saving model
yearly_saving_form <- reformulate(response = "Saving_value_yearly", 
                                  c("Ethnicity", 
                                    "Education", 
                                    "Occupation",
                                    "Household_size",
                                    "Farm_size",
                                    "Yearly_earning",
                                    "Total_irrigated",
                                    "Loan_received_cat",
                                    "Staple_purchase_months",
                                    "Saving_financial_institution"))
yearly_saving_model <- lm(yearly_saving_form, 
                          data = hh_finance_data)

yearly_saving_model %>% 
  anova() %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression/yearly_saving_anova.csv", na = "")

yearly_saving_model %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression/yearly_saving_estimates.csv", na = "")

# multinomial and random forest model comparison
yearly_saving_mul_form <- reformulate(response = "Saving_cat_yearly_mul", 
                                      c("Ethnicity", 
                                        "Education", 
                                        "Occupation",
                                        "Household_size",
                                        "Farm_size",
                                        "Yearly_earning",
                                        "Total_irrigated",
                                        "Loan_received_cat",
                                        "Staple_purchase_months",
                                        "Saving_financial_institution"))

yearly_saving_model_multinom <- nnet::multinom(yearly_saving_mul_form, 
               data = hh_finance_data)

yearly_saving_model_ranforest <- randomForest::randomForest(yearly_saving_mul_form, 
                           data = hh_finance_data)

# logit model
yearly_saving_bin_form <- reformulate(response = "Saving_cat_yearly_bin", 
                                      c("Ethnicity", 
                                        "Education", 
                                        "Occupation",
                                        "Household_size",
                                        "Farm_size",
                                        "Yearly_earning",
                                        "Total_irrigated",
                                        "Loan_received_cat",
                                        "Staple_purchase_months",
                                        "Saving_financial_institution"))

  yearly_saving_bin_model <- stats::glm(formula = yearly_saving_bin_form, 
           data = hh_finance_data, 
           family = stats::binomial(link = "logit"))

yearly_saving_bin_form <- reformulate(response = "Saving_cat_yearly_bin", 
                                      c("Ethnicity",
                                        "Education",
                                        "Occupation",
                                        "Household_size",
                                        "Farm_size",
                                        "Yearly_earning_cat",
                                        "Staple_purchase_months",
                                        "Loan_received_cat"
                                        # "Saving_financial_institution"
                                      ))

yearly_saving_bin_model <- stats::glm(formula = yearly_saving_bin_form, 
                                      data = hh_finance_data, 
                                      family = stats::binomial(link = "logit"))

yearly_saving_bin_model %>% anova()

(yearly_saving_bin_model %>% summary())$coefficients

# tidymodel glm fitting
mlogreg <- logistic_reg() %>% 
  set_engine("glm")

fit(mlogreg, yearly_saving_mul_form, 
    data = hh_finance_data) %>% 
  broom::tidy()
