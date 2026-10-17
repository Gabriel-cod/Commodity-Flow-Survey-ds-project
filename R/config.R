# Acesso a configuracao do projeto (config.yml) e aos caminhos padrao.

#' Le o config.yml do ambiente ativo (R_CONFIG_ACTIVE).
cfg <- function(...) {
  valor <- config::get(file = here::here("config.yml"))
  chaves <- c(...)
  for (k in chaves) valor <- valor[[k]]
  valor
}

#' Caminho absoluto a partir da raiz do projeto.
#' caminho("base_tratada") -> <raiz>/data/processed/cfs_tratada.duckdb
caminho <- function(chave) {
  here::here(cfg("caminhos", chave))
}
