# Funcoes para aplicar o dicionario de variaveis do CFS a base.

#' Le o dicionario (xlsx) do Census.
ler_dicionario <- function(arquivo = caminho("dicionario")) {
  readxl::read_excel(arquivo)
}

#' Extrai o de-para (codigo -> rotulo) de uma variavel do dicionario.
#' O codigo vem como texto no dicionario e como inteiro na base; por isso a
#' juncao e sempre feita com as duas pontas convertidas para character.
de_para <- function(dicionario, variavel, sufixo = "_desc") {
  dicionario |>
    dplyr::filter(.data$VARIABLE == variavel) |>
    dplyr::select(dplyr::all_of(c("VALUE_CODE", "VALUE_LABEL"))) |>
    dplyr::mutate(VALUE_CODE = trimws(as.character(.data$VALUE_CODE))) |>
    dplyr::distinct() |>
    stats::setNames(c(variavel, paste0(variavel, sufixo)))
}

#' Adiciona a coluna de rotulo de `variavel` ao data.frame.
#' Resolve o problema relatado em docs/notas: char vs int gerando NA no join.
rotular <- function(dados, dicionario, variavel, sufixo = "_desc") {
  mapa <- de_para(dicionario, variavel, sufixo)
  names(mapa)[1] <- ".chave_join"

  dados |>
    dplyr::mutate(.chave_join = trimws(as.character(.data[[variavel]]))) |>
    dplyr::left_join(mapa, by = ".chave_join") |>
    dplyr::select(-".chave_join")
}

#' Quantos codigos ficaram sem rotulo apos o join (controle de qualidade).
checar_rotulos <- function(dados, coluna_rotulo) {
  faltando <- sum(is.na(dados[[coluna_rotulo]]))
  message(coluna_rotulo, ": ", faltando, " linhas sem rotulo (",
          round(100 * faltando / nrow(dados), 2), "%)")
  invisible(faltando)
}
