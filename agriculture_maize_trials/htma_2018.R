# import libraries
require(tidyverse)

# import data
htma1 <- readxl::read_xlsx("./HTMA_2018.xlsx", sheet = "Sheet1", skip = 7)
htma2 <- readxl::read_xlsx("./HTMA_2018.xlsx", sheet = "Sheet2")
htma3 <- readxl::read_xlsx("./HTMA_2018.xlsx", sheet = "Sheet3")

# join the metadata sheets
htma_meta <- left_join(htma2, htma3)
rm(htma2, htma3)

# final dataset
htma1 <- left_join(htma_meta, htma1)
