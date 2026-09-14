# Roda a suite de testes:  Rscript tests/testthat.R
library(testthat)
source(here::here("scripts", "00_setup.R"))
testthat::test_dir(here::here("tests", "testthat"))
