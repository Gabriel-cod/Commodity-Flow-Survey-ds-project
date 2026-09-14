source("renv/activate.R")
# Carregado automaticamente ao abrir o projeto no RStudio / rodar Rscript na raiz.

# Repositorio de pacotes. No Linux usamos o Posit Package Manager, que serve
# binarios pre-compilados (instalacao em segundos em vez de minutos); nos demais
# sistemas o CRAN ja entrega binario proprio.
repo_cran <- if (Sys.info()[["sysname"]] == "Linux") {
  "https://packagemanager.posit.co/cran/__linux__/noble/latest"
} else {
  "https://cloud.r-project.org"
}

options(
  repos = c(CRAN = repo_cran),
  stringsAsFactors = FALSE,
  scipen = 999,          # evita notacao cientifica em valores de frete
  digits = 7,
  dplyr.summarise.inform = FALSE
)

# Locale de numeros/datas para relatorios em pt-BR (silencioso se indisponivel).
invisible(suppressWarnings(try(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"), silent = TRUE)))

if (interactive()) {
  message("Projeto: Commodity Flow Survey 2022 (CFS/PUMS)")
  message("Rode source('scripts/00_setup.R') para checar pacotes e carregar helpers.")
}
