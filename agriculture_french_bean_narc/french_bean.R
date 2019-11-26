# library load
library(tidyverse)
require(agricolae)

# import dataset
french1 <- readxl::read_xlsx("./french_bean.xlsx")

# rename vars tidily
french1 <- french1 %>% 
  rename_all(function(x)str_replace_all(x, 
                                        c("/" = "_per_", "%" = "_percent_", 
                                          "\\(" = "_", "\\)" = "_", "__" = "_", 
                                          " _"="_", "_$" = ""))) %>% 
  rename_all(function(x)str_squish(x))

# colnames
french1 %>% colnames()

# mutate types
french1 <- french1 %>% 
  mutate_at(c("Year", "Replication", "Genotype"), as.factor)

# model formulation
yield_ha_model <- lm(reformulate(c("Year","Replication", "Genotype", "Year:Genotype"), 
               response = "Dry grain yield_mt_per_ha"), 
   data = french1)

yield_ha_an <- anova(yield_ha_model) %>% broom::tidy()
yield_ha_an

yield_mean <- map_dfr(list("Year", "Genotype", 
                           c("Year", "Genotype")), function(y){LSD.test(yield_ha_model, y, 
                                                                        DFerror = yield_ha_an$df[5], 
                                                                        MSerror = yield_ha_an$meansq[5], 
                                                                        alpha = 0.05, group = TRUE, console = F) %>% 
                               keep(names(.) == "means" | names(.) == "groups") %>%
                               map(~ .x %>% rownames_to_column("Treatment")) %>% 
                               reduce(left_join, by = "Treatment") %>%
                               select(Treatment, mean = 2, `groups` 
                                      # `std`:`Q75`
                               )})

# yield cv model
yield_cv <- cv.model(yield_ha_model) %>% 
  enframe(name = NULL, value = "mean") %>% 
  add_column("Treatment" = "CV")

# transform and add f-value to dataset
yield_fvalues <- map_dfc(list(yield_ha_an), ~.x %>% 
                        filter(term != "Replication") %>% # remove replication term 
                        na.omit() %>% 
                        pull("statistic")) %>% # remove residual term
  magrittr::set_colnames("mean") %>% 
  add_column(.before = 1, "Treatment" = c("F-value (Year)", "F-value (Genotype)", 
                                          "F-value (Year:Genotype)"))

# bind cvs to the last row
yield_means_aug <- bind_rows(yield_mean, yield_cv, yield_fvalues) 

# save to external files
yield_means_aug %>% write_csv("./yield_mean_table.csv", na = "")
