dicionario <- data.frame(
  VARIABLE    = c("ORIG_STATE", "ORIG_STATE", "TEMP_CNTRL_YN", "TEMP_CNTRL_YN"),
  VALUE_CODE  = c("01", " 24", "Y", "N"),
  VALUE_LABEL = c("Alabama", "Maryland", "Yes", "No")
)

test_that("codigos em texto recebem o rotulo; sem par vira NA (T-R1)", {
  dados <- data.frame(ORIG_STATE = c("01", "24", "99"))
  res <- rotular(dados, dicionario, "ORIG_STATE")

  expect_equal(res$ORIG_STATE_desc, c("Alabama", "Maryland", NA))
  expect_equal(nrow(res), nrow(dados))
})

test_that("TEMP_CNTL_YN usa o nome TEMP_CNTRL_YN do dicionario (T-R2)", {
  res <- rotular(data.frame(TEMP_CNTL_YN = c("N", "Y")), dicionario, "TEMP_CNTL_YN")
  expect_equal(res$TEMP_CNTL_YN_desc, c("No", "Yes"))
})

test_that("coluna numerica gera erro (T-R3)", {
  expect_error(rotular(data.frame(ORIG_STATE = c(1L, 24L)), dicionario, "ORIG_STATE"),
               "deve chegar como texto")
})

test_that("variavel sem rotulo no dicionario gera erro (T-R4)", {
  expect_error(rotular(data.frame(ORIG_MA = "12580"), dicionario, "ORIG_MA"),
               "use a coluna \\*_CFS_AREA")
})
