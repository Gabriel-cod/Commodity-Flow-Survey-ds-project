# O Quarto inicia o R em reports/, onde o .Rprofile da raiz nao e lido: sem
# isto o renv fica inativo (knitr e rmarkdown so existem na biblioteca do renv).
# RENV_PROJECT e obrigatorio: sem ele o renv cria reports/renv/.
Sys.setenv(RENV_PROJECT = normalizePath(".."))
source("../renv/activate.R")
