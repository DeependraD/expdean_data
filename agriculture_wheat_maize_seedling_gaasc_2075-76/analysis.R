# load libraries
require(tidyverse)
require(gridExtra)
set.seed(9845333)

# read in data
wm_twofac <- readxl::read_xlsx("./data/data.xlsx")

# import and variable renaming
wm_twofac <- readxl::read_xlsx("./data/data.xlsx") %>% 
  mutate_at(vars(Day, Crop, Dosage), factor) %>% 
  rename_all(function(x)stringr::str_squish(x)) %>% 
  rename_all(function(x)stringi::stri_replace_all_fixed(x, 
                                                        c(" ", "/", "%",  "(", ")"), 
                                                        c("_", "_per_", "_percent_", "_", "_"), 
                                                        vectorize_all = FALSE)) %>% 
  rename_all(function(x)stringr::str_replace_all(x, "__", "_")) %>% 
  rename_all(function(x)stringr::str_replace_all(x, "_$", ""))

# separate the samples
wm_twofac <- wm_twofac %>% 
  mutate(
    Sample = str_extract(Crop, "[:digit:]"),
    Crop = str_extract(Crop, "[:alpha:]")
  )

# encoding
factorA_crop <- c("Wheat", "Maize")
factorB_dosage <- c("L01", "L02", "L03", "L04", "L05")

factorA_crop <- map(1:2, as.character) %>% 
  set_names(factorA_crop) # make a list for argument unpacking later on
factorB_dosage <- map(1:5, as.character) %>% 
  set_names(factorB_dosage)

# expand.grid(factorA_crop, factorB_dosage)
# treatment_grps <- crossing(factorA_crop, factorB_dosage) # for atomic vector
treatment_combn <- cross_df(list(Crop = names(factorA_crop), 
                                 Dosage = names(factorB_dosage))) %>% 
  tidyr::unite("Treatment combination", sep = "+", remove = TRUE)

fb_2factorial <- agricolae::design.ab(c(2, 5), 
                                      r = 3, 
                                      serie = 2, 
                                      design = "rcbd", 
                                      first = TRUE, 
                                      randomization = TRUE)

fb_2factorial_book <- fb_2factorial$book %>%
  mutate(row = table(block) %>% map(seq_len) %>% reduce(c)) %>% 
  mutate(Crop = fct_recode(A, !!!factorA_crop)) %>% # argument unpacking/dot dot dots
  mutate(Dosage = fct_recode(B, !!!factorB_dosage)) %>%
  select(-c(A, B)) %>% 
  mutate("Treatment combination" = paste0(Crop, "+", Dosage), 
         Rep = block)

# encoding for observation dataset
factorA_crop <- c("Wheat", "Maize")
factorB_dosage <- c("L01", "L02", "L03", "L04", "L05")

factorA_crop <- map(c("w", "m"), as.character) %>% 
  set_names(factorA_crop) # make a list for argument unpacking later on
factorB_dosage <- map(paste0("T", 1:5), as.character) %>% 
  set_names(factorB_dosage)

# recode observation dataset
wm_twofac <- wm_twofac %>% 
  mutate(Crop = fct_recode(Crop, !!!factorA_crop)) %>% # argument unpacking/dot dot dots
  mutate(Dosage = fct_recode(Dosage, !!!factorB_dosage))

# save name of numeric only variables
numeric_trait_names <- wm_twofac %>%
  map_lgl(is.numeric) %>% 
  which() %>% names()

# model weight (example)
model_weight <- lm(Weight~Crop/Dosage/Sample, data = wm_twofac)

# mapping all variables
model_variables <- map(numeric_trait_names, 
                       ~lm(reformulate(termlabels = c("Crop", "Dosage", "Sample", 
                                                     "Day", "Crop:Dosage"), 
                                       response = .x), 
                           data = wm_twofac)) %>% 
  set_names(numeric_trait_names)

# model anova
wm_tanova <- map(model_variables[c("Dead", "Alive", "Weight")], 
                      ~ anova(.x) %>% broom::tidy())

# sample refers to the replication so it's 
# main effects is  not useful
# model means and summary
map_inner2 <- function(y)map_dfr(list("Crop", "Dosage", "Day",
                                      c("Crop", "Dosage")
                                      ),
                                 function(x)agricolae::duncan.test(y, trt = x, DFerror = anova(y)[[1]][[3]], 
                                                                   group = T) %>%
                                   keep(names(.) == "means" | names(.) == "groups") %>%
                                   map(~ .x %>% rownames_to_column("Treatment")) %>%
                                   reduce(left_join, by = "Treatment") %>%
                                   select(Treatment, mean = 2, `groups`, `std`:`Q75`))

wm_means <- map(model_variables, map_inner2)

# ensure that column to join by does not contain duplicates
wm_means <- wm_means %>%
  reduce(left_join, by = "Treatment") %>%
  magrittr::set_colnames(c("Treatment",
                           paste(rep(numeric_trait_names, each = 9),
                                 c("mean", "group", "std", "r", "Min", "Max", "Q25", "Q50", "Q75"))))

# rename_all(funs(str_replace_all(., "_", "\\ ")))

# cvs of all traits
wm_cvs <- data.frame(trait = numeric_trait_names,
                           cv = model_variables %>%
                             map_dbl(agricolae::cv.model))

# select only mean and groups
wm_means <- wm_means %>% 
  select(Treatment, contains("mean"), contains("group"))

# transform cv to appendable form 
wm_cvs <- wm_cvs %>% 
  remove_rownames() %>% 
  column_to_rownames("trait") %>% 
  t() %>% 
  as_tibble() %>% 
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = "CV") # name the column to match treatment col

# transform and add f-value to dataset
wm_fvalues <- map_dfc(wm_tanova, ~.x %>% 
                        filter(term != "Sample") %>% # remove replication term 
                        na.omit() %>% 
                        pull("statistic")) %>% # remove residual term
  rename_all(function(x)str_c(x, " mean")) %>% 
  add_column(.before = 1, "Treatment" = c("F-value (crop)", "F-value (Dosage)", 
                                          "F-value (Day)", "F-value (Crop:Dosage)"))

# bind cvs to the last row
wm_means_aug <- bind_rows(wm_means, wm_cvs, wm_fvalues) 
  
# save to external files
wm_means_aug %>% write_csv("./wheatmaize_mean_table.csv", na = "")

## Correlation analysis of traits
# This function annotates the correlation coefficients
# with the stars corresponding to the level of significance
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results : "html" or "latex" (default)
# align: a vector of alignment values as is specified in xtable(x, align)

large <- function(x){
  paste0('{\\footnotesize{\\bfseries ', x, '}}')
}

corstars <-function(x, align, caption, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html", comment = FALSE)
    else print(xtable(Rnew, auto = TRUE,  align = align, caption = caption), type="latex",
               comment = FALSE, size = "\\footnotesize",
               sanitize.colnames.function = large,
               booktabs = TRUE, tabular.environment="longtable",
               floating=FALSE)
  }
}

# crop yield and agro-morphology
cortable <- corstars(wm_twofac %>%
                       select_at(which(map_lgl(., is.numeric))))
# # write to csv
# cortable %>% rownames_to_column(var = "traits") %>% write_csv("correlation_papaya_ramesh.csv")

# # html
# cortable %>%
#   knitr::kable(format = "html",
#                caption = "Pearson correlation coefficients of Post harvest traits",
#                booktabs = TRUE, align = "l"
#                # longtable = TRUE
#                ) %>%
#   kableExtra::kable_styling(font_size = 14, bootstrap_options = c("striped", "responsive")) %>%
#   # kableExtra::column_spec(1:11, width = "0.11\\\\textwidth") %>%
#   kableExtra::row_spec(0, bold = TRUE) %>%
#   kableExtra::column_spec(1, bold = TRUE) %>%
#   kableExtra::footnote(general = "p < .0001: **** ; p < .001: *** ; p < .01: ** ; p < .05: *",
#                        threeparttable = TRUE, escape = FALSE)
cortable <- cortable %>%
  rename_all(function(x)stringr::str_trunc(x, width = 4,
                                           side = "c", ellipsis = "")) %>%
  rownames_to_column("Trait") %>%
  mutate(Trait = str_replace_all(Trait, "_", "\\ "))

# # Write analysis summary outputs

# # write a single file to csv
# two_fac_gmeans_df %>% write_csv(path = "./output/papaya_dmrt.csv")

# # write to csv
# dmrt_files <- wm_twofac %>%
#   map_lgl(is.numeric) %>%
#   which() %>%
#   names() %>%
#   stringr::str_replace_all("/", "_") # filename contains "\", to fix the error
# ifelse(dir.exists("./dmrt_results/"),
#        "The directory already exists",
#        dir.create("./dmrt_results/"))
#
# walk2(two_fac_gmeans, paste("./dmrt_results/", dmrt_files, ".csv", sep = ""),
#       .f = ~ write_csv(.x, .y))

# zip all files
# dmrt_dir_file_paths <- dir("./dmrt_results/", full.names = TRUE)
# zip(zipfile = "dmrt_all_traits.zip", files = dmrt_dir_file_paths)

# # write to csv
# ifelse(dir.exists("./anova_results/"),
#        "The directory already exists",
#        dir.create("./anova_results/"))
#
# walk2(two_fac_tanova, paste("./anova_results/", dmrt_files, ".csv", sep = ""), # "dmrt_files" is just filenames
#       .f = ~ write_csv(.x, .y, na = ""))

# zip all files
# anova_dir_file_paths <- dir("./anova_results/", full.names = TRUE)
# zip(zipfile = "anova_all_traits.zip", files = anova_dir_file_paths)
