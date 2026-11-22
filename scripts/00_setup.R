# 00_setup.R -------------------------------------------------------------
# Ponto de entrada de toda sessao: checa pacotes e carrega as funcoes de R/.
# Uso:  source("scripts/00_setup.R")

pacotes <- c(
  "here",        # caminhos relativos a raiz do projeto
  "config",      # ambientes do config.yml
  "tidyverse",   # dplyr, ggplot2, tidyr, ...
  "readxl",      # dicionario .xlsx
  "DBI",         # interface generica de banco
  "dbplyr",      # dplyr sobre SQL
  "duckdb",      # banco analitico local (ambiente local_duckdb)
  "RPostgres",   # banco externo Postgres (ambiente postgres)
  "rsample",     # Check-in 3: divisao treino/teste
  "yardstick",   # Check-in 3: metricas do classificador
  "probably",    # Check-in 3: desempenho por limiar
  "marginaleffects" # Check-in 3: probabilidades por perfil
)

faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando)) {
  message("Instalando pacotes ausentes: ", paste(faltando, collapse = ", "))
  install.packages(faltando)
}

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

# Carrega todas as funcoes do projeto.
invisible(lapply(list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE), source))

message("Setup OK | ambiente: ", Sys.getenv("R_CONFIG_ACTIVE", "default"))
