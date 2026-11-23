# Formatacao de numeros e tabelas no padrao brasileiro, para relatorios e graficos.

n_fmt <- function(x, d = 0) {
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, scientific = FALSE)
}

pct <- function(x, total, d = 2) paste0(n_fmt(100 * x / total, d), "%")

num_br <- function(x) format(x, digits = 15, decimal.mark = ",", scientific = FALSE)

tabela <- function(df, ..., caption = NULL) {
  typst <- identical(knitr::pandoc_to(), "typst")
  t <- knitr::kable(df, format.args = list(big.mark = ".", decimal.mark = ","),
                    caption = if (!typst) caption, ...)
  if (!typst || is.null(caption)) return(t)
  # O Quarto descarta no Typst a legenda de tabela sem rotulo tbl-; vira um
  # titulo colado na tabela. String Typst (e nao markup) para "_" nao virar italico.
  titulo <- sprintf('```{=typst}\n#block(sticky: true, above: 12pt, below: 5pt, text(weight: 600, size: 0.9em, fill: rgb("%s"), "%s"))\n```',
                    COR[["secundario"]], gsub('(["\\\\])', "\\\\\\1", caption))
  structure(c("", titulo, "", t), format = "pipe", class = "knitr_kable")
}

# Valor-p legivel: com n grande quase todos ficam abaixo de 0,001. Espaco
# inquebravel para o "<" nao ficar sozinho numa linha da tabela.
p_fmt <- function(p) vapply(p, function(x) if (x < 0.001) "<\u00a00,001" else n_fmt(x, 3), "")
