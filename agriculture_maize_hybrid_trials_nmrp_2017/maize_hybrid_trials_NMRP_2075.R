require(agricolae)
require(tidyverse)

hm_s1 <- readxl::read_xlsx("./Hybrid_trials_NMRP_2075.xlsx", sheet = "B3655", "A2:R70")
hm_s2 <- readxl::read_xlsx("./Hybrid_trials_NMRP_2075.xlsx", sheet = "B2923", "A2:S50")
hm_s3 <- readxl::read_xlsx("./Hybrid_trials_NMRP_2075.xlsx", sheet = "WCYN_51", "A2:S38")
hm_s4 <- readxl::read_xlsx("./Hybrid_trials_NMRP_2075.xlsx", sheet = "WCWN-49", "A2:S44")
hm_s5 <- readxl::read_xlsx("./Hybrid_trials_NMRP_2075.xlsx", sheet = "NP!&B365017", range = "A7:L25")

# are there any na values
which(is.na(hm_s1))

# change classess and pre-processing
hm_s1 <- hm_s1 %>% 
  mutate_at(c("Plot", "Rep", "Block", "Entry"), as.factor) %>% 
  mutate(DOP = as.Date("1900-01-01") + lubridate::days(DOP)-lubridate::days(2)) %>% 
  mutate(`Days_to_male_flower` = if_else(!is.na(as.numeric(`Days Flwr.  Male`)), 
                                        as.Date("1900-01-01") + lubridate::days(as.numeric(`Days Flwr.  Male`))-lubridate::days(2),
                                        as.Date(`Days Flwr.  Male`, format = "%d-%b-%Y"))) %>%
  mutate(`Days_to_female_flower` = if_else(!is.na(as.numeric(`Days Flwr. Fem.`)), 
                                        as.Date("1900-01-01") + lubridate::days(as.numeric(`Days Flwr. Fem.`))-lubridate::days(2),
                                        as.Date(`Days Flwr. Fem.`, format = "%d-%b-%Y"))) %>% 
  select(-`Days Flwr.  Male`, -`Days Flwr. Fem.`) %>% 
  mutate_at(c("pht", "eht"), as.numeric)

# run random effects model variants: reml and variance component models
mo1_reml <- agricolae::PBIB.test(block = hm_s1$Block, trt = hm_s1$Entry, replication = hm_s1$Rep, 
                                 k = 17, y = hm_s1$pht, method = "REML", alpha = 0.05, 
                                 console = TRUE, group = TRUE)

mo1_vc <- agricolae::PBIB.test(block = hm_s1$Block, trt = hm_s1$Entry, replication = hm_s1$Rep, 
                               k = 17, y = hm_s1$pht, method = "VC", alpha = 0.05, 
                               console = TRUE, group = TRUE)

mo1_reml$ANOVA
mo1_vc$ANOVA

mo1_reml$statistics
mo1_vc$statistics

mo1_reml$means
mo1_vc$means

bind_cols(pht_reml = mo1_reml$means$`hm_s1$pht.adj`, pht_vc = mo1_vc$means$`hm_s1$pht.adj`)
broom::tidy(mo1_reml$model)


# create custom fixed effects variance component model and fit it
with(hm_s1, aov(pht ~ Rep + Entry + Rep/Block)) %>% anova() # a peek at anova
hml_s1aov <- with(hm_s1, aov(pht ~ Rep + Entry + Rep/Block)) # save aov object

# perform lsd test
lsd_ob <- LSD.test(hml_s1aov, trt = "Entry", DFerror = anova(hml_s1aov)[4, 1], 
         MSerror = anova(hml_s1aov)[4, 3], alpha = 0.05, 
         p.adj = "none", group = TRUE, console = TRUE)

## automate the custom fixed effects variance component writing ##
numeric_vars <- names(which(map_chr(hm_s1, class) == "numeric"))
numeric_vars <- numeric_vars %>% setdiff("moisture (%)")

# save list of aovs
aov_alpha_lat <- map(numeric_vars, ~with(hm_s1, aov(get(.x) ~ Rep + Entry + Rep/Block)))

# save list of anovas
anova_alpha_lat <- map(numeric_vars, function(x) {
  anova_raw <- with(hm_s1, aov(get(x) ~ Rep + Entry + Rep/Block)) %>% 
    anova()
  attr(anova_raw, which = "heading")[2] <- paste("Response: ", x, sep = "")
  return(anova_raw)
}
)

# write a single file of anova to csv
walk2(.x = anova_alpha_lat,
      .y = numeric_vars,
      .f = ~.x %>% 
        rownames_to_column(var = "Factors") %>% 
        add_row() %>% 
        add_column(.before = 1,
                   Trait = c(.y, rep(NA, times = (nrow(.)-1)))) %>%
        write_csv(path = "anova_test_sheet4.csv",
                  # row.names = FALSE,
                  na = "", append = TRUE, col_names = TRUE))

# write a single file of means to csv
group_means_alpha_lat <- map_dfc(aov_alpha_lat, function(x) {
  lsd_ob <- LSD.test(x, trt = "Entry", DFerror = anova(x)[4, 1], 
                     MSerror = anova(x)[4, 3], alpha = 0.05, 
                     p.adj = "bonferroni", group = TRUE, console = FALSE)
  lsd_mean_groups <- lsd_ob$groups %>% 
    rownames_to_column(var = "Entry") %>% 
    rename("Mean" = "get(.x)")
  return(lsd_mean_groups)
})

group_means_alpha_lat %>% 
  magrittr::set_colnames(paste(c("Entry_", "Mean_", "group_"), rep(numeric_vars, each = 3), sep = "")) %>% 
  write_csv(path = "mean_group_sheet4.csv", 
            na = "", col_names = TRUE)

# write a single file for model cv
map_dbl(aov_alpha_lat, cv.model) %>% 
  as_data_frame() %>% 
  add_column(.before = 1, numeric_vars) %>% 
  rename("cv_value" = "value") %>% 
  write_csv(path = "cv_sheet1.csv", 
            na = "", col_names = TRUE)
