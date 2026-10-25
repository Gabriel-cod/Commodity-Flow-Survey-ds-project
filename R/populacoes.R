# Populacoes de analise e familias de modal (FSD 31.3 e 31.4). Nenhuma linha
# sai da base: cada analise filtra a sua populacao na consulta.

POPULACOES <- tibble::tribble(
  ~id,           ~regra,                                              ~descricao,
  "completa",    "todas as linhas",                                   "Base completa",
  "geografica",  "!fl_exportacao & !fl_origem_suprimida",             "Sem exportações e sem origem suprimida",
  "mercadoria",  "!fl_sctg_colapsado",                                "Sem SCTG colapsado",
  "origem_area", "!fl_origem_suprimida & !fl_origem_ma_suprimida",    "Com área CFS de origem identificada"
)

filtrar_populacao <- function(dados, populacao) {
  switch(populacao,
    completa = dados,
    geografica = dplyr::filter(dados, !fl_exportacao, !fl_origem_suprimida),
    mercadoria = dplyr::filter(dados, !fl_sctg_colapsado),
    origem_area = dplyr::filter(dados, !fl_origem_suprimida, !fl_origem_ma_suprimida),
    stop("População desconhecida: ", populacao, call. = FALSE)
  )
}

# A ordem e a das legendas.
FAMILIAS_MODAL <- list(
  "Caminhão" = c("111", "112"),
  "Retirada pelo cliente" = "113",
  "Ferrovia" = "12",
  "Hidrovia" = c("131", "132", "133"),
  "Aéreo" = "14",
  "Duto" = "15",
  "Encomenda" = "21",
  "Multimodal" = c("22", "23", "24", "25"),
  "Outros ou desconhecido" = c("16", "30")
)

# CASE montado so com as constantes acima; a coluna nao e gravada na base.
com_familia_modal <- function(dados) {
  casos <- vapply(names(FAMILIAS_MODAL), function(f) {
    sprintf("WHEN MODE IN (%s) THEN '%s'", paste0("'", FAMILIAS_MODAL[[f]], "'", collapse = ", "), f)
  }, character(1))
  dplyr::mutate(dados, familia_modal = dplyr::sql(paste("CASE", paste(casos, collapse = " "), "END")))
}
