# Formatacao de numeros e tabelas no padrao brasileiro, para relatorios e graficos.

n_fmt <- function(x, d = 0) {
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, scientific = FALSE)
}

pct <- function(x, total, d = 2) paste0(n_fmt(100 * x / total, d), "%")

num_br <- function(x) format(x, digits = 15, decimal.mark = ",", scientific = FALSE)

tabela <- function(df, ...) {
  knitr::kable(df, format.args = list(big.mark = ".", decimal.mark = ","), ...)
}
