# Tema, paleta e rotulos dos graficos (ggplot2), na identidade "Manifesto de
# carga" (docs/referencias/local/identidade-visual.md). Cinza e o contexto; o
# marinho e o laranja ficam para o que o titulo conta (Storytelling with Data).

COR <- c(
  papel = "#F4F1EA",
  cartao = "#FBF9F4",
  kraft = "#E7DCC6",
  marinho = "#0C2340",
  laranja = "#E8641E",
  laranja_texto = "#B7440E",
  secundario = "#3E5266",
  rotulo = "#5E6B78",
  linha = "#D8D0C0"
)

# Caminhao e encomenda, as duas historias dos graficos, nas cores da identidade;
# as demais familias em neutros que alternam claro e escuro na ordem de
# FAMILIAS_MODAL (vizinhas no empilhamento), validados para daltonismo.
PALETA_MODAL <- c(
  "Caminhão" = "#0C2340",
  "Retirada pelo cliente" = "#A9B8C6",
  "Ferrovia" = "#56677A",
  "Hidrovia" = "#C9D3DC",
  "Aéreo" = "#7F8FA0",
  "Duto" = "#3E5266",
  "Encomenda" = "#E8641E",
  "Multimodal" = "#CBB991",
  "Outros ou desconhecido" = "#8C7B5E"
)

# Rampa sequencial de um so tom (marinho), do claro ao escuro, para mapas de calor.
RAMPA_SEQUENCIAL <- c("#E4E9EF", "#C2CDD8", "#9BADBF", "#7189A3", "#4A6584", "#2A4463", "#0C2340")

COR_SERIE <- c("Sem fator" = "#0C2340", "Com fator" = "#E8641E")

FONTES_CFS <- here::here("reports", "fontes")

# Registra a IBM Plex Sans do projeto para os dispositivos ragg; sem isso os
# graficos usam a fonte padrao do sistema.
registrar_fontes_cfs <- function(pasta = FONTES_CFS) {
  arquivo <- function(estilo) file.path(pasta, paste0("IBMPlexSans-", estilo, ".ttf"))
  systemfonts::register_font("IBM Plex Sans", plain = arquivo("Regular"), bold = arquivo("SemiBold"),
                             italic = arquivo("Italic"), bolditalic = arquivo("SemiBold"))
  options(cfs.fonte_graficos = "IBM Plex Sans")
  invisible("IBM Plex Sans")
}

tema_cfs <- function(base_size = 9, fonte = getOption("cfs.fonte_graficos", "")) {
  ggplot2::theme_minimal(base_size = base_size, base_family = fonte) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = COR[["papel"]], colour = NA),
      text = ggplot2::element_text(colour = COR[["marinho"]]),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), margin = ggplot2::margin(b = 4)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      # margem direita maior: o ultimo rotulo do eixo x nao corta
      plot.margin = ggplot2::margin(10, 24, 10, 4),
      plot.subtitle = ggplot2::element_text(colour = COR[["secundario"]], margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(colour = COR[["rotulo"]], size = ggplot2::rel(0.8), hjust = 0,
                                           margin = ggplot2::margin(t = 8)),
      axis.text = ggplot2::element_text(colour = COR[["secundario"]]),
      axis.title = ggplot2::element_text(colour = COR[["rotulo"]], size = ggplot2::rel(0.9)),
      panel.grid.major = ggplot2::element_line(colour = COR[["linha"]], linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, colour = COR[["marinho"]])
    )
}

rotulo_num <- function(...) scales::label_number(big.mark = ".", decimal.mark = ",", ...)

# labs() com quebra de linha: titulo e subtitulo longos nao cortam no PDF.
rotulos_cfs <- function(titulo, subtitulo = NULL, rodape = NULL, ..., largura = 80) {
  quebrar <- function(x, n) if (is.null(x)) NULL else stringr::str_wrap(x, n)
  ggplot2::labs(title = quebrar(titulo, largura), subtitle = quebrar(subtitulo, round(largura * 1.15)),
                caption = quebrar(rodape, round(largura * 1.35)), ...)
}

# Pares (sem fator, com fator) em formato longo, com a legenda de COR_SERIE.
series_fator <- function(df) {
  tidyr::pivot_longer(df, c(pct_linhas, pct_remessas), names_to = "serie", values_to = "pct") |>
    dplyr::mutate(serie = factor(ifelse(serie == "pct_linhas", "Sem fator", "Com fator"), names(COR_SERIE)))
}

LEGENDA_SERIES <- "Sem fator: % das linhas da amostra. Com fator: % das remessas estimadas (soma do WGT_FACTOR)."

rodape_cfs <- function(populacao) {
  descricao <- POPULACOES$descricao[POPULACOES$id == populacao]
  paste0("Fonte: CFS 2022 PUMS (Census/BTS). População: ", sub("^(.)", "\\L\\1", descricao, perl = TRUE), ".")
}
