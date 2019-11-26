require(tidyverse)
library(lattice)
library(car)

narayan_severity <- readxl::read_xlsx("./data_narayan.xlsx", "severity")
narayan_yield <- readxl::read_xlsx("./data_narayan.xlsx", sheet = "yield")

# narayan_data <- narayan_data %>% 
#   mutate(Treatment_full = str_extract(Treatment, "(?<=\\().*.(?=\\))"))

narayan_severity <- narayan_severity %>%
  mutate(`Disease_severity` = if_else(`Disease_severity` == 0, NA_real_, `Disease_severity`)) %>% 
  mutate_at(c("Main_plot", "Sub_plot", "Plant_part", "Rep"), as_factor) %>% 
  select(-Treatment, -Treatment_full) %>% 
  spread(key = `Plant_part`, value = `Disease_severity`, drop = TRUE)

narayan_data <- left_join(x = narayan_severity, y = narayan_yield, by = ("Plot")) %>% 
  select(-"Rep.y") %>% 
  rename(Rep = "Rep.x")

narayan_data <- narayan_data %>% 
  mutate(Yield = str_extract(Yield, "\\d*")) %>% 
  mutate_at("Yield", as.numeric)

narayan_data_split <- lm(Yield ~ Main_plot * Sub_plot + Rep/Main_plot, data = narayan_data)
anova(narayan_data_split)

with(narayan_data, xyplot(Yield ~ Sub_plot | Main_plot, groups = Rep, aspect = "xy", type = "o", 
                       main = "Effect of Management practice on Yield broken down by Fungicide used"))
