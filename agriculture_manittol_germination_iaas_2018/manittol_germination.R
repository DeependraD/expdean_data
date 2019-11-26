# load libraries
require(tidyverse)

mannitol <- readxl::read_xlsx("./data/mannitol_germination/upa.xlsx")

mannitol <- mannitol %>% 
  mutate_at(c("Replication", "Mannitol conc_percent", "Salt conc_Mpa"), as.factor)

numeric_trait_names <- names(which(map_lgl(mannitol, is.numeric)))

# rcbd model for all variables
mannitol_crd <- map(numeric_trait_names,
                     .f = ~ lm(get(.x) ~ `Mannitol conc_percent` + `Salt conc_Mpa`,
                               data = mannitol))
mannitol_crd <- set_names(mannitol_crd, numeric_trait_names)
# mean separation
# could be done using LSD.test, duncan.test, SNK.test or HSD.test
# LSD.test allows for p-value adjustment
mannitol_anova <- map(mannitol_crd, anova)

map_inner2 <- function(y)map_dfr(list("Mannitol conc_percent", "Salt conc_Mpa"),
                                 function(x)agricolae::duncan.test(y, trt = x, DFerror = anova(y)[[1]][[3]], 
                                                                   group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = `get(.x).x`, `groups`, `std`:`Q75`))
agricolae::LSD.test(mannitol_crd[[3]], "Mannitol conc_percent", DFerror = 66, p.adj = "bonferroni", group = TRUE, console = TRUE)
mannitol_fitnmean <- map(mannitol_crd[3:length(mannitol_crd)], map_inner2)

# ensure that column to join by does not contain duplicates
mannitol_fitnmean <- mannitol_fitnmean %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(numeric_trait_names[-c(1,2)], each = 9),
                                 c("mean", "group", "std", "r", "Min", "Max", "Q25", "Q50", "Q75"))))

# rename_all(funs(str_replace_all(., "_", "\\ ")))

# cvs of all traits
mannitol_cvs <- data.frame(trait = numeric_trait_names[-c(1, 2)],
                          cv = mannitol_crd[-c(1, 2)] %>%
                            map_dbl(agricolae::cv.model))

# save to external files
mannitol_cvs %>% write_csv("./upa_analysis_cv.csv", na = "")
mannitol_fitnmean %>% write_csv("./upa_analysis_mean_comparison.csv", na = "")

