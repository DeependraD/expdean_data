# load libraries
require(tidyverse)
library(gridExtra)

# import data
banana_tikapur_coding <- readxl::read_xlsx("./data/data.xlsx", "coding_description")
banana_tikapur_data <- readxl::read_xlsx("./data/data.xlsx", "data")
banana_tikapur_data <- banana_tikapur_data %>% 
  mutate_at(c("Education status", "Gender of respondents", 
              "Source of income", "Membership in group or cooperatives", 
              "Training received", "Propagation", "Farm size category", 
              "Marketing channel"), as.factor) %>% 
  mutate(Yield = `Total area(Katta)`*41400/30) %>% 
  rename_all(function(x)str_replace_all(x, "\ ", "_")) %>% 
  rename_all(function(x)str_replace_all(x, c("\\ " = "_", "\\(" = "_", 
                                             "\\)" = "", "\\/" = "_per_"))) %>% 
  select(-`S.N`)

banana_return_t <- readxl::read_xlsx("./cost_benefit_analysis_importable.xlsx", "cb_ratio")
banana_cost_t <- readxl::read_xlsx("./cost_benefit_analysis_importable.xlsx", "cost_estimates", skip = 1)
banana_cost_est_agg <- readxl::read_xlsx("./cost_benefit_analysis_importable.xlsx", "cost_estimates_aggregation")

# # descriptive summary
banana_tikapur_coding <- banana_tikapur_coding %>%
  mutate(Variables = str_replace_all(Variables, "\ ", "_")) %>% 
  group_split(Variables)

# coding information packing
banana_tikapur_coding <- banana_tikapur_coding %>% 
  set_names(nm = map_chr(.x = banana_tikapur_coding, ~ .x$Variables %>% unique())) %>% 
  map(~.x %>% select(-Variables))

# recode levels with description unloading
# for multiple columns
banana_tikapur_data_recoded <- map_dfc(.x = names(banana_tikapur_coding), 
        ~ recode(banana_tikapur_data[, .x, drop = TRUE], 
                 !!!map((banana_tikapur_coding %>% 
                           pluck(.x))$Description, c) %>% 
                   set_names((banana_tikapur_coding %>% 
                                pluck(.x))$Coding))) %>% 
  magrittr::set_colnames(names(banana_tikapur_coding))

# replace data with recoded values
banana_tikapur_data[, names(banana_tikapur_coding)] <- banana_tikapur_data_recoded
banana_tikapur_data

# descriptive plots
# # count is short-hand for group_by() and count()
# banana_tikapur_data %>%
#   group_by(Education_status) %>% 
#   tally()

# tally education status
tally_education <- banana_tikapur_data %>% 
  # group_by(Education_status) %>% 
  count(Education_status, name = "Frequency")

# Create a basic bar
pie <- ggplot(tally_education, 
              aes(x="", y=Frequency, fill=Education_status)) + 
  geom_bar(stat="identity", width=1) +
  # Convert to pie (polar coordinates) and add labels
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Frequency/sum(Frequency)*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  # Add color scale (hex colors)
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", 
                             "#F6AE2D", "#F26419")) +
  # Remove labels and add title
  labs(x = NULL, y = NULL, 
       fill = NULL, 
       title = "Percentage of sampled repondents by education") + 
  # Tidy up the theme
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))
# pie

# grouping by education_status and expressing mean of
# numeric variable as percentage proportion
percentage_grouped_education <- group_by(banana_tikapur_data, .dots = c("Education_status")) %>%
  summarize_if(is.numeric, funs(mean(., na.rm = TRUE))) %>%
  mutate_if(is.numeric, funs(./sum(.)*100))

ggplot(percentage_grouped_education, 
                aes('', `Total_area_Katta`)) +
  geom_col(position = 'fill',
           color = 'black',
           width = 1,
           aes(fill = factor(Education_status))) +
  # facet_wrap(~cyl, labeller = "label_both") +
  geom_label(aes(label = paste0(round(Total_area_Katta), "%"), group = factor(Education_status)),
             position = position_fill(vjust = 0.5),
             color = 'black',
             size = 5,
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  ylab("Literacy status") +
  scale_fill_discrete("Literacy status" 
                      # labels = c(NULL)
                      ) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))

# Frequency distribution table
freqdist <- banana_tikapur_data %>%
  select(-Total_area_Katta, -Banana_cultivated_area_Katta, 
         -contains("Cost"),
         -Yield, 
         -c(`quantity_banana_dozen`:`return_banana_calculated`)
         ) %>% 
  mutate(Age = cut(Age, 
                   breaks = c(0, 25, 50, 75), 
                   labels = c("0-25", "26-50", "51-75")), 
         `Family_size` = cut(`Family_size`, 
                             breaks = c(0, 3, 5, 7, 9), 
                             labels = c("1-3", "4-5", "6-7", "8-9"))) %>% 
  gather(key = "Socio-economic variables", value = "Category") %>% 
  group_by(`Socio-economic variables`) %>% 
  count(Category, name = "Frequency") %>% 
  mutate(Percentage = Frequency/sum(Frequency)*100) %>% 
  ungroup() %>% 
  mutate_at("Socio-economic variables", 
            function(x)str_replace_all(x, "_", "\\ "))

# # write to csv
# freqdist %>%
#   write_csv("./outputs/banana_tikapur_socio_economic_descriptive.csv", " ")

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

# # arrange pie plots in grid
# # grid.arrange(grobs = fpie, ncol = 3,
# #              top = "Percentage of response category for socio-economic variables in household samples") ## display plot
# ggsave(filename = "./outputs/banana_tikapur_socio_economic_graph.png",
#        arrangeGrob(grobs = fpie, ncol = 3,
#                    top = "Percentage of response category for socio-economic variables in household samples"),
#        width = 14, height = 12, device = "png", dpi = 240) ## save plot

# # Inferential summary
# idvs
yield_idvs <- setdiff(colnames(banana_tikapur_data), c("Total_area_Katta", "Banana_cultivated_area_Katta", 
                                                       "Yield", "Farm_size_category", "return_banana_calculated"))

# add variable cost data
# here we use the column from original dataset to create a 
# list column which has cost estimates
# the cost estimate is now currently undiscounted but soon 
# discounted form of this estimation will be included
# for discounting
# use the following discounting factor/rate (to multiply with the cost or return)
# 1 + 1/(1+r)^n

banana_tikapur_data_costest <- banana_tikapur_data %>% 
  mutate(id = row_number()) %>% 
  group_split(id) %>% 
  map_df(~mutate(., 
                 cost_estimate_total = list(tibble(
                   Particulars = banana_cost_t$Particulars, 
                   cost_type = banana_cost_t$`Cost type`, 
                   cost = case_when(
                     banana_cost_t$Particulars == "Sapling" ~ .$`Cost_planting_materials`,
                     banana_cost_t$Particulars == "Fertilizer_Urea" ~ .$`Cost_fertilizers`/4,
                     banana_cost_t$Particulars == "Fertilizer_DAP" ~ .$`Cost_fertilizers`/4,
                     banana_cost_t$Particulars == "Manure" ~ .$`Cost_fertilizers`/4,
                     banana_cost_t$Particulars == "Bordeaux mixture, micro-nutrients and soil treatments" ~ .$`Cost_micronutrients`,
                     banana_cost_t$Particulars == "Plant protection chemical" ~ .$`Cost_pesticides`,
                     banana_cost_t$Particulars == "Fencing labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Field preparation labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Preplanting irrigation labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Planting labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Layout labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Postplanting irrigation labor" ~ .$`Cost_labour`/6,
                     banana_cost_t$Particulars == "Field preparation draft" ~ .$`Cost_draft`,
                     banana_cost_t$Particulars == "Marketing and transport" ~ .$`Cost_marketing_and_transport`,
                     TRUE ~ as.numeric(rowSums((banana_cost_t %>% 
                                                  select(Y1_Value_in_NRs, Y2_Value_in_NRs, Y3_Value_in_NRs)), 
                                               na.rm = TRUE))
                   )
                 )
                 ))
  )

# now obtain cost summation and bc ratio
banana_tikapur_data_costest <- banana_tikapur_data_costest %>% 
  mutate(cost_total = map_dbl(cost_estimate_total, ~(sum(.x$cost, na.rm = T)))) %>%
  mutate(bc_ratio = return_banana_obtained/cost_total)

# now classify bc ratio and numeric variables grouped by landholding
banana_tikapur_data_costest %>% 
  group_by(Farm_size_category) %>% 
  summarise_if(is.numeric, funs(mean), na.rm = TRUE)

# frequency distribution of socio-economic variables grouped by landholding
banana_tikapur_by_holding <- banana_tikapur_data_costest %>% 
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
# walk2(banana_tikapur_by_holding, 
#       names(banana_tikapur_by_holding), 
#       .f = ~write_csv(.x, path = paste0("./outputs/", .y, "_by_holding", ".csv", collapse = ""), 
#                       na = ""))

# model formulation
yield_model <- lm(reformulate(response = "Yield", yield_idvs), 
                  data = banana_tikapur_data)

# model summary
yieldsum <- summary(yield_model)

yieldsum$coefficients %>%
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  as_tibble() %>% 
  write_csv("./outputs/regression/regression_analysis_estimates.csv", "")

# anova
anova(yield_model) %>% 
  broom::tidy() %>% 
  write_csv("./outputs/regression/regression_anova.csv", "")
