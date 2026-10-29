# Simula o formato de saida do knitr e devolve o texto gerado por tabela().
tabela_em <- function(formato, ...) {
  antes <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = antes))
  knitr::opts_knit$set(rmarkdown.pandoc.to = formato)
  paste(tabela(data.frame(x = 1234.5), ...), collapse = "\n")
}

test_that("tabela: legenda vira titulo no Typst e fica no kable nos outros formatos", {
  typst <- tabela_em("typst", caption = 'Peso "bruto" de SHIPMT_WGHT')
  expect_match(typst, "```{=typst}", fixed = TRUE)
  expect_match(typst, 'Peso \\"bruto\\" de SHIPMT_WGHT', fixed = TRUE)
  expect_match(typst, "1.234,5", fixed = TRUE)

  html <- tabela_em("html", caption = "Legenda")
  expect_false(grepl("{=typst}", html, fixed = TRUE))
  expect_match(html, "Legenda", fixed = TRUE)

  expect_false(grepl("{=typst}", tabela_em("typst"), fixed = TRUE))
})
