install.packages('dbplot')



# testantdo

library(dbplot)
source("scripts/00_setup.R")
source(here::here("scripts", "02_trata_base.R"))

base <- obter_base_tratada(pipeline = FALSE)

base |>
  dbplot_bar(SECTOR)




