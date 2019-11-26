# load library
require(tidyverse)
require(agricolae)

# import data
mandarin_ta <- readxl::read_xlsx("./data/mandarin_TA.xlsx", skip = 6) %>% 
  mutate_at(c("Replication", "Treatment"), as.factor)

mandarin_tss <- readxl::read_xlsx("./data/mandarin_tss.xlsx") %>% 
  mutate_at(c("Replication", "Treatment"), as.factor)

mandarin_firmness <- readxl::read_xlsx("./data/mandarin_firmness.xlsx") %>% 
  mutate_at(c("Replication", "Treatment"), as.factor)

mandarin_pwl <- readxl::read_xlsx("./data/mandarin_physiological_weight_loss.xlsx", skip = 4) %>% 
  mutate_at(c("Replication", "Treatment"), as.factor)

# mandarin_pwl <- mandarin_pwl %>% 
#   gather(key = "day", 
#          value = "physiological_weight_loss", 
#          -Replication, -Treatment) %>% 
#   separate(col = "day", into = c("day", "sample"), sep = "_") %>% 
#   mutate_at("day", function(x)str_replace_all(x, "\\ ", ""))
# 
# # mandarin_pwl sample aggregation
# mandarin_pwl_agg <- mandarin_pwl %>% 
#   group_by(Replication, Treatment, day) %>% 
#   summarise_if(is.numeric, mean) %>% 
#   spread(key = day, value = "physiological_weight_loss")

# mandarin_pwl for pwl data only
mandarin_pwl <- mandarin_pwl %>% 
  select(Replication, Treatment, PWL1_refine, PWL5_refine, PWL10_refine, PWL15_refine, PWL20_refine, PWL25_refine)

# numeric variable names for firmness
mandarin_firmness_numerics <- colnames(mandarin_firmness) %>%
  str_subset("_", negate = TRUE) %>%
  setdiff(c("Replication", "Treatment"))

# numeric variable names for pwl_agg 
mandarin_pwl_agg_numerics <- colnames(mandarin_pwl) %>%
  setdiff(c("Replication", "Treatment"))

# numeric variable names for pwl_agg 
mandarin_pwl_agg_numerics <- colnames(mandarin_pwl) %>%
  setdiff(c("Replication", "Treatment"))

# numeric variable names for ta
mandarin_ta_numerics <- colnames(mandarin_ta) %>%
  setdiff(c("Replication", "Treatment"))

## mapping firmness
# mapping all variables
model_variables <- map(mandarin_firmness_numerics, 
                       ~lm(reformulate(termlabels = c("Treatment", "Replication"), 
                                       response = .x), 
                           data = mandarin_firmness)) %>% 
  set_names(mandarin_firmness_numerics)

# model anova
mandarin_firmness_tanova <- map(model_variables, 
                 ~ anova(.x) %>% broom::tidy())

# # save anova to external files
# walk2(.x = (model_variables %>% 
#               map(~anova(.x) %>% broom::tidy())), 
#       .y = names(model_variables), 
#       .f = ~write_csv(x = .x, path = paste("./output/anova_firmness/", .y, ".csv", sep = "")))

# sample refers to the replication so it's 
# main effects is  not useful
# model means and summary
map_inner2 <- function(y)map_dfr(list("Treatment"), 
                                 function(x)agricolae::LSD.test(y, trt = x, 
                                                                DFerror = df.residual(y), 
                                                                MSerror = deviance(y)/df.residual(y), 
                                                                group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = 2, `groups`, `std`:`Q75`))

mandarin_firmness_means <- map(model_variables, map_inner2)

# make LSD values dataframe
mandarin_LSDval <- model_variables %>% 
  map_dfc(~(agricolae::LSD.test(.x, "Treatment", console = F))$statistics$LSD) %>%
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "LSD")

# ensure that column to join by does not contain duplicates
mandarin_firmness_means <- mandarin_firmness_means %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(mandarin_firmness_numerics, each = 11),
                                 c("mean", "group", "std", "r", "LCL", 
                                   "UCL", "Min", "Max", "Q25", "Q50", "Q75"))))

# cvs of all traits
mandarin_firmness_cvs <- model_variables %>% 
  map_dbl(agricolae::cv.model) %>% 
  enframe(name = "trait", value = "cv")

# select only mean and groups
mandarin_firmness_means <- mandarin_firmness_means %>% 
  select(Treatment, contains("mean"), contains("group"))

# transform cv to appendable form 
mandarin_firmness_cvs <- mandarin_firmness_cvs %>% 
  remove_rownames() %>% 
  column_to_rownames("trait") %>% 
  t() %>% 
  as_tibble() %>% 
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "CV") # name the column to match treatment col

# transform and add f-value to dataset
mandarin_firmness_fvalues <- map_dfc(mandarin_firmness_tanova, ~.x %>% 
                        filter(term != "Replication") %>% # remove replication term
                        na.omit() %>% 
                        pull("statistic")) %>% # remove residual term
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = c("F-value (Treatment)"))

# bind cvs to the last row
mandarin_firmness_means_aug <- bind_rows(mandarin_firmness_means, 
                          mandarin_firmness_cvs, 
                          mandarin_firmness_fvalues, 
                          mandarin_LSDval)

# save to external files
mandarin_firmness_means_aug %>% write_csv("./mandarin_firmness_mean_table_final.csv", na = "")

## mapping pwl_agg
# mapping all variables
# model_variables <- map(mandarin_pwl_agg_numerics, 
#                        ~lm(reformulate(termlabels = c("Treatment", "Replication"), 
#                                        response = .x), 
#                            data = mandarin_pwl_agg)) %>% 
#   set_names(mandarin_pwl_agg_numerics)

model_variables <- map(mandarin_pwl_agg_numerics, 
                       ~lm(reformulate(termlabels = c("Treatment", "Replication"), 
                                       response = .x), 
                           data = mandarin_pwl)) %>% 
  set_names(mandarin_pwl_agg_numerics)

# model anova
mandarin_pwl_agg_tanova <- map(model_variables, 
                                ~ anova(.x) %>% broom::tidy())

# # save anova to external files
# walk2(.x = (model_variables %>% 
#               map(~anova(.x) %>% broom::tidy())), 
#       .y = names(model_variables), 
#       .f = ~write_csv(x = .x, path = paste("./output/anova_pwl/", .y, ".csv", sep = "")))

# sample refers to the replication so it's 
# main effects is  not useful
# model means and summary
map_inner2 <- function(y)map_dfr(list("Treatment"), 
                                 function(x)agricolae::duncan.test(y, trt = x, 
                                                                   DFerror = df.residual(y),
                                                                   MSerror = deviance(y)/df.residual(y),
                                                                   group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = 2, `groups`, `std`:`Q75`))

mandarin_pwl_agg_means <- map(model_variables, map_inner2)

# make LSD values dataframe
mandarin_LSDval <- model_variables %>% 
  map_dfc(~(agricolae::LSD.test(.x, "Treatment", console = F))$statistics$LSD) %>%
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "LSD")

# ensure that column to join by does not contain duplicates
mandarin_pwl_agg_means <- mandarin_pwl_agg_means %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(mandarin_pwl_agg_numerics, each = 9),
                                 c("mean", "group", "std", "r", 
                                   "Min", "Max", "Q25", "Q50", "Q75"))))

# cvs of all traits
mandarin_pwl_agg_cvs <- model_variables %>% 
  map_dbl(agricolae::cv.model) %>% 
  enframe(name = "trait", value = "cv")

# select only mean and groups
mandarin_pwl_agg_means <- mandarin_pwl_agg_means %>% 
  select(Treatment, contains("mean"), contains("group"))

# transform cv to appendable form 
mandarin_pwl_agg_cvs <- mandarin_pwl_agg_cvs %>% 
  remove_rownames() %>% 
  column_to_rownames("trait") %>% 
  t() %>% 
  as_tibble() %>% 
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "CV") # name the column to match treatment col

# transform and add f-value to dataset
mandarin_pwl_agg_fvalues <- map_dfc(mandarin_pwl_agg_tanova, ~.x %>% 
                                       filter(term != "Replication") %>% # remove replication term
                                       na.omit() %>% 
                                       pull("statistic")) %>% # remove residual term
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = c("F-value (Treatment)"))

# bind cvs to the last row
mandarin_pwl_agg_means_aug <- bind_rows(mandarin_pwl_agg_means, 
                                        mandarin_pwl_agg_cvs, 
                                        mandarin_pwl_agg_fvalues, 
                                        mandarin_LSDval)

# save to external files
mandarin_pwl_agg_means_aug %>% write_csv("./mandarin_pwl_agg_mean_table_final.csv", na = "")

## mapping ta
# mapping all variables
model_variables <- map(mandarin_ta_numerics, 
                       ~lm(reformulate(termlabels = c("Treatment", "Replication"), 
                                       response = .x), 
                           data = mandarin_ta)) %>% 
  set_names(mandarin_ta_numerics)

# model anova
mandarin_ta_tanova <- map(model_variables, 
                               ~ anova(.x) %>% broom::tidy())

# # save anova to external files
# walk2(.x = (model_variables %>% 
#               map(~anova(.x) %>% broom::tidy())), 
#       .y = names(model_variables), 
#       .f = ~write_csv(x = .x, path = paste("./output/anova_ta/", .y, ".csv", sep = "")))

# sample refers to the replication so it's 
# main effects is  not useful
# model means and summary
map_inner2 <- function(y)map_dfr(list("Treatment"), 
                                 function(x)agricolae::duncan.test(y, trt = x, 
                                                                   DFerror = df.residual(y),
                                                                   MSerror = deviance(y)/df.residual(y),
                                                                   group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = 2, `groups`, `std`:`Q75`))

mandarin_ta_means <- map(model_variables, map_inner2)

# make LSD values dataframe
mandarin_LSDval <- model_variables %>% 
  map_dfc(~(agricolae::LSD.test(.x, "Treatment", console = F))$statistics$LSD) %>%
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "LSD")

# ensure that column to join by does not contain duplicates
mandarin_ta_means <- mandarin_ta_means %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(mandarin_ta_numerics, each = 9),
                                 c("mean", "group", "std", "r", "Min", "Max", "Q25", "Q50", "Q75"))))

# cvs of all traits
mandarin_ta_cvs <- model_variables %>% 
  map_dbl(agricolae::cv.model) %>% 
  enframe(name = "trait", value = "cv")

# select only mean and groups
mandarin_ta_means <- mandarin_ta_means %>% 
  select(Treatment, contains("mean"), contains("group"))

# transform cv to appendable form 
mandarin_ta_cvs <- mandarin_ta_cvs %>% 
  remove_rownames() %>% 
  column_to_rownames("trait") %>% 
  t() %>% 
  as_tibble() %>% 
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "CV") # name the column to match treatment col

# transform and add f-value to dataset
mandarin_ta_fvalues <- map_dfc(mandarin_ta_tanova, ~.x %>% 
                                      filter(term != "Replication") %>% # remove replication term
                                      na.omit() %>% 
                                      pull("statistic")) %>% # remove residual term
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = c("F-value (Treatment)"))

# bind cvs to the last row
mandarin_ta_means_aug <- bind_rows(mandarin_ta_means, 
                                   mandarin_ta_cvs, 
                                   mandarin_ta_fvalues, 
                                   mandarin_LSDval)

# save to external files
mandarin_ta_means_aug %>% write_csv("./mandarin_ta_mean_table.csv", na = "")

## mapping tss
# mapping all variables
mandarin_tss <- mandarin_tss %>% select(Replication, Treatment, contains("mean"))

# numeric variable names for firmness
mandarin_tss_numerics <- colnames(mandarin_tss) %>%
  str_subset("mean", negate = FALSE) %>%
  setdiff(c("Replication", "Treatment"))

model_variables <- map(mandarin_tss_numerics, 
                       ~lm(reformulate(termlabels = c("Treatment", "Replication"), 
                                       response = .x), 
                           data = mandarin_tss)) %>% 
  set_names(mandarin_tss_numerics)

# model anova
mandarin_tss_tanova <- map(model_variables, 
                               ~ anova(.x) %>% broom::tidy())

# sample refers to the replication so it's 
# main effects is  not useful
# model means and summary
map_inner2 <- function(y)map_dfr(list("Treatment"), 
                                 function(x)agricolae::duncan.test(y, trt = x, 
                                                                   DFerror = df.residual(y),
                                                                   MSerror = deviance(y)/df.residual(y),
                                                                   group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = 2, `groups`, `std`:`Q75`))

mandarin_tss_means <- map(model_variables, map_inner2)

# make LSD values dataframe
mandarin_LSDval <- model_variables %>% 
  map_dfc(~(agricolae::LSD.test(.x, "Treatment", console = F))$statistics$LSD) %>%
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "LSD")

# ensure that column to join by does not contain duplicates
mandarin_tss_means <- mandarin_tss_means %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(mandarin_tss_numerics, each = 9),
                                 c("mean", "group", "std", "r", 
                                   "Min", "Max", "Q25", "Q50", "Q75"))))

# cvs of all traits
mandarin_tss_cvs <- model_variables %>% 
  map_dbl(agricolae::cv.model) %>% 
  enframe(name = "trait", value = "cv")

# select only mean and groups
mandarin_tss_means <- mandarin_tss_means %>% 
  select(Treatment, contains("mean"), contains("group"))

# transform cv to appendable form 
mandarin_tss_cvs <- mandarin_tss_cvs %>% 
  remove_rownames() %>% 
  column_to_rownames("trait") %>% 
  t() %>% 
  as_tibble() %>% 
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "CV") # name the column to match treatment col

# transform and add f-value to dataset
mandarin_tss_fvalues <- map_dfc(mandarin_tss_tanova, ~.x %>% 
                                      filter(term != "Replication") %>% # remove replication term
                                      na.omit() %>% 
                                      pull("statistic")) %>% # remove residual term
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = c("F-value (Treatment)"))

# bind cvs to the last row
mandarin_tss_means_aug <- bind_rows(mandarin_tss_means, 
                                        mandarin_tss_cvs, 
                                        mandarin_tss_fvalues, 
                                        mandarin_LSDval)

# save to external files
mandarin_tss_means_aug %>% write_csv("./mandarin_tss_mean_table_final.csv", na = "")

# # save anova to external files
# walk2(.x = (model_variables %>% 
#         map(~anova(.x) %>% broom::tidy())), 
#       .y = names(model_variables), 
#       .f = ~write_csv(x = .x, path = paste("./output/anova_tss/", .y, ".csv", sep = "")))
