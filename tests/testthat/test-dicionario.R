test_that("rotular junta codigos mesmo com tipos diferentes (int vs char)", {
  dados <- data.frame(SECTOR = c(42L, 31L, 99L))
  dicionario <- data.frame(
    VARIABLE    = c("SECTOR", "SECTOR"),
    VALUE_CODE  = c("42", " 31"),
    VALUE_LABEL = c("Wholesale trade", "Manufacturing")
  )

  res <- rotular(dados, dicionario, "SECTOR")

  expect_equal(res$SECTOR_desc, c("Wholesale trade", "Manufacturing", NA))
  expect_equal(nrow(res), nrow(dados))
})
