# library loading
require(tidyverse)
require(haven)
library(gridExtra)
options(scipen = 10)
options(digits = 10)

# # load data
# yarsa <- haven::read_sav("./data/yarsa.sav")
# yarsa <- yarsa[1:60,]
# save("yarsa", file = "./data/yarsa.RData")
# write_excel_csv(yarsa, "./data/yarsa.csv", " ")
# load("./data/yarsa.RData")

yarsa <- readxl::read_xlsx("./data/yarsa.xlsx")
yarsa_variables <- readxl::read_xlsx("./data/yarsa_variable_details.xlsx")

# head scan
yarsa %>% head(4)

# # find anomalies
# yarsa %>% 
#   map(unique)

yarsa %>% colnames()

# convert types
yarsa <- yarsa %>% 
  mutate_at(c("respondent_gender", "respondent_ethnicity", "respondent_education", 
              "suitable_month", "frequency", "satisfaction", "registration", 
              "preference", "condition", "middlemen", "benefits", "demerits", 
              "responsibility", "device", "curing", "time_curing", "method_curing", 
              "storage", "packing", "customer_preference", "selling", "perception", 
              "conflict", "insect_pest", "low_productivity", "labour", "geophysical", 
              "low_price", "price_variation", "weak_coordination", "insufficient_storage", 
              "less_awareness", "problem_middleman"), 
            as.factor) %>%
  rename_all(function(x)str_replace_all(x, "\ ", "_")) %>% 
  rename_all(function(x)str_replace_all(x, c("\\ " = "_", "\\(" = "_", 
                                             "\\)" = "", "\\/" = "_per_"))) %>% 
  select(-ID)

# # categorize price information
# map((yarsa %>% 
#       select(highest_price, average_price, lowest_price)), 
#     ~range(., na.rm = T))
# 
# yarsa <- yarsa %>% 
#   mutate(highest_price_cgroup = cut(highest_price, breaks = c(10000, 15000, 20000, 25000), 
#                                     labels = c("Rs.10000-Rs.15000", "Rs.15001-Rs.20000", 
#                                                "Rs.20001-Rs.25000"))) %>% 
#   mutate(average_price_cgroup = cut(average_price, breaks = c(5000, 10000, 15000, 20000), 
#                                     labels = c("Rs.5000-Rs.10000", "Rs.10001-Rs.15000", 
#                                                "Rs.15001-Rs.20000"))) %>% 
#   mutate(lowest_price_cgroup = cut(lowest_price, breaks = c(1000, 5000, 10000, 15000, 20000), 
#                                    labels = c("Rs.1000-Rs.5000", "Rs.5001-Rs.10000",
#                                               "Rs.10001-Rs.15000", "Rs.15001-Rs.20000")))

# # descriptive summary
yarsa_variables <- yarsa_variables %>%
  filter(!is.na(Coding)) %>% 
  mutate(`Variable name` = str_replace_all(`Variable name`, "\ ", "_")) %>% 
  group_split(`Variable name`)

# coding information packing
yarsa_variables <- yarsa_variables %>% 
  set_names(nm = map_chr(.x = yarsa_variables, ~ .x$`Variable name` %>% unique())) %>% 
  map(~.x %>% select(-`Variable name`))

# recode levels with description unloading
# for multiple columns
yarsa_recoded <- map_dfc(.x = names(yarsa_variables), 
                                       ~ recode(yarsa[, .x, drop = TRUE], 
                                                !!!map((yarsa_variables %>% 
                                                          pluck(.x))$`Coding description`, c) %>% 
                                                  set_names((yarsa_variables %>% 
                                                               pluck(.x))$Coding))) %>% 
  magrittr::set_colnames(names(yarsa_variables))

# any missing
yarsa_recoded %>% map(~which(is.na(.x)))

# replace data with recoded values
yarsa[, names(yarsa_variables)] <- yarsa_recoded
yarsa

# any numeric are missing?
yarsa %>% 
  select_if(is.numeric) %>% 
  map(~which(is.na(.x)))

# diagnose if any variables need to be omitted
# factor variables have how many levels each
yarsa %>% 
  map_chr(class) %>% 
  enframe(name = "variable", value = "class") %>% 
  filter(class == "factor") %>% 
  left_join((map_dbl(yarsa, ~length(unique(.x))) %>% 
               enframe(name = "variable", value = "levels")))

# # # descriptive plots
# # tally education status
# tally_education <- yarsa %>% 
#   # group_by(Education_status) %>% 
#   count(respondent_education, name = "Frequency")
# 
# # Create a basic bar
# pie <- ggplot(tally_education, 
#               aes(x="", y=Frequency, fill=respondent_education)) + 
#   geom_bar(stat="identity", width=1) +
#   # Convert to pie (polar coordinates) and add labels
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(round(Frequency/sum(Frequency)*100), "%")), 
#             position = position_stack(vjust = 0.5)) +
#   # Add color scale (hex colors)
#   scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", 
#                              "#F6AE2D", "#F26419", "#a02384")) +
#   # Remove labels and add title
#   labs(x = NULL, y = NULL, 
#        fill = NULL, 
#        title = "Percentage of repondents surveyed falling under different literary groups") + 
#   # Tidy up the theme
#   theme_classic() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666"))
# # pie

# # grouping by respondent_education and expressing mean of
# # numeric variable as percentage proportion
# percentage_grouped_education <- group_by(yarsa, .dots = c("respondent_education")) %>%
#   summarize_if(is.numeric, list(~mean(., na.rm = TRUE))) %>%
#   mutate_if(is.numeric, list(~./sum(.)*100))
# 
# ggplot(percentage_grouped_education, 
#        aes('', `harvester_members_household`)) +
#   geom_col(position = 'fill',
#            color = 'black',
#            width = 1,
#            aes(fill = factor(respondent_education))) +
#   # facet_wrap(~cyl, labeller = "label_both") +
#   geom_label(aes(label = paste0(round(harvester_members_household), "%"), 
#                  group = factor(respondent_education)),
#              position = position_fill(vjust = 0.5),
#              color = 'black',
#              size = 5,
#              show.legend = FALSE) +
#   ggtitle("Percentage of total members participating in Yarsa 
#           harvest among sampled households by literacy group") +
#   ylab(NULL) +
#   coord_polar(theta = "y") +
#   scale_fill_discrete("Literacy status" 
#                       # labels = c(NULL)
#   ) +
#   theme_classic() + 
#   theme(axis.line = element_blank(),
#         # axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.y = element_blank(),
#         plot.title = element_text(hjust = 0.5, color = "#666666"))

# Frequency distribution table
freqdist <- yarsa %>%
  select(which(map_lgl(., ~is.factor(.x)))) %>% 
  gather(key = "Socio-economic variables", value = "Category") %>% 
  group_by(`Socio-economic variables`) %>% 
  count(Category, name = "Frequency") %>% 
  mutate(Percentage = Frequency/sum(Frequency)*100) %>% 
  ungroup() %>% 
  mutate_at("Socio-economic variables", 
            function(x)str_replace_all(x, "_", "\\ "))

# write to csv
freqdist %>%
  write_csv("./outputs/yarsa_socio_economic_descriptive.csv", "")

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
               ggrepel::geom_text_repel(aes(label = paste0(round(Percentage), "%")),
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

# # arrange pie plots in grid
# # grid.arrange(grobs = fpie, ncol = 3,
# #              top = "Percentage of response category for socio-economic variables in household samples") ## display plot
# ggsave(filename = "./outputs/yarsa_socio_economic_graph.png",
#        arrangeGrob(grobs = fpie, ncol = 4,
#                    top = "Percentage response in each category of
#                    socio-economic variables among surveyed individuals"),
#        width = 18, height = 16, device = "png", dpi = 300) ## save plot

# # Inferential summary
# idvs
# which variables are numeric
yarsa %>% 
  map_lgl(~class(.) == "numeric") %>%
  which() %>% 
  names()

# # extract numeric summeries
# yarsa %>% 
#   select_if(is.numeric) %>% 
#   summarise_all(list(mean = ~mean(.), 
#                      sd = ~sd(.))) %>% 
#   gather(key = "Variable", value = "Statistic") %>% 
#   separate(col = "Variable", c("Variable", "Statistic_summary"), sep = "_$") %>%
#   write_csv("./outputs/numeric_summary.csv", "")

# now summarize numeric variables grouped by income
yarsa <- yarsa %>% 
  mutate(yarsha_income_2073_cgroup=cut(yarsha_income_2073, 
                                       c(10000, 100000, 500000, 
                                         1000000, 2500000, 5000000,
                                         10000000), 
                                       labels = c("10 Thousand-100 Thousand", "100 Thousand-500 Thousand", 
                                                  "500 Thousand-1 Million", "1 Million-25 Million",
                                                  "25 Million-50 Million", "50 Million-100 Million")))
yarsa %>% 
  group_by(yarsha_income_2073_cgroup) %>% 
  summarise_if(is.numeric, list(~mean(., na.rm = TRUE)), na.rm = TRUE)

# frequency distribution of socio-economic variables grouped by income of 2073
yarsha_by_2073_income <- yarsa %>% 
  mutate_at("yarsha_income_2073_cgroup", function(x)forcats::fct_explicit_na(x)) %>% 
  select_if(is.factor) %>%
  gather(key = "Categorical variable", value = "Value", -yarsha_income_2073_cgroup) %>% 
  group_by(yarsha_income_2073_cgroup, `Categorical variable`, Value) %>%
  tally() %>%
  ungroup() %>% 
  group_split(`Categorical variable`) %>%
  set_names(map_chr(., ~unique(.x$`Categorical variable`))) %>% 
  # map(~rename(., !!unique(.$Catvars) := "Value")) %>% 
  map(. %>% select(-`Categorical variable`)) %>% 
  map2(.x = ., 
       .y = names(.), 
       ~.x %>% rename(!!.y := "Value"))

# # write to external file
# walk2(yarsha_by_2073_income,
#       names(yarsha_by_2073_income),
#       .f = ~write_csv(.x, path = paste0("./outputs/", .y, "_by_income", ".csv", collapse = ""),
#                       na = ""))
# zip(zipfile = "outputs/yarsa_socio_economic_by_income.zip",
#     files = list.files("./outputs/", pattern = "_by_income.csv", full.names = T))

# model formulation
yarsha_idvs <- setdiff(yarsa %>% 
                         map_lgl(~class(.) %in% c("numeric", "factor")) %>%
                         which() %>% 
                         names(), 
                       c("yarsha_income_2074", "yarsha_income_2073",
                         "yarsha_income_2073_cgroup",
                         "highest_price", "average_price", "lowest_price", 
                         "respondent_name" 
                       ))

yarsha_income_model <- lm(reformulate(response = "yarsha_income_2074", 
                                      termlabels = yarsha_idvs), 
                          data = yarsa)

yarsha_income_model <- lm(reformulate(response = "yarsha_income_2074", 
                                      termlabels = c("respondent_age", "respondent_gender", "respondent_ethnicity", 
                                                     "family_size", "respondent_education", "harvester_members_household", 
                                                     "started_harvesting", "suitable_month", "collection_last_year", 
                                                     "distance", "time_coll", "expenses", "frequency", "satisfaction", 
                                                     "registration", "preference", "condition", "middlemen", "benefits", 
                                                     # "demerits", # contrast setting problem 
                                                     "responsibility", "device", "curing", "time_curing", 
                                                     "method_curing", "storage", "packing", "customer_preference", 
                                                     "selling", "total_income_2074", "total_income_2073", "perception", 
                                                     "conflict", "insect_pest", "low_productivity", "labour", "weather", 
                                                     "low_price", "price_variation", "weak_coordination", "insufficient_storage", 
                                                     "less_awareness", "problem_middleman"
                                                     # "highest_price_cgroup", 
                                                     # "average_price_cgroup", 
                                                     # "lowest_price_cgroup"
                                                     )), 
                          data = yarsa)

# too much variables will lead to deficit degrees of freedom
# model summary
summary(yarsha_income_model)

# anova
anova(yarsha_income_model)
