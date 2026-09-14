# Leitura/escrita de arquivos. Sempre via here::here(), nunca setwd().

#' Le CSV grande com data.table::fread (nao derruba a memoria como read.csv).
ler_csv <- function(arquivo, ...) {
  stopifnot(file.exists(arquivo))
  data.table::fread(arquivo, ...)
}

#' Escreve CSV criando o diretorio se necessario.
escrever_csv <- function(dados, arquivo, ...) {
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(dados, arquivo, ...)
  message("Gravado: ", arquivo, " (", nrow(dados), " linhas)")
  invisible(arquivo)
}

#' Salva figura em outputs/figures com tamanho padronizado.
salvar_figura <- function(plot, nome, largura = 8, altura = 5, dpi = 300) {
  arquivo <- here::here("outputs", "figures", paste0(nome, ".png"))
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(arquivo, plot, width = largura, height = altura, dpi = dpi)
  message("Figura salva: ", arquivo)
  invisible(arquivo)
}
