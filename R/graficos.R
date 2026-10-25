# Tema, paleta e rotulos dos graficos (ggplot2). Paleta categorica de 8 cores
# validada para daltonismo na ordem dada (pares vizinhos); a 9a familia,
# residual, fica em cinza neutro em vez de uma cor gerada.

PALETA_MODAL <- c(
  "Caminhão" = "#2a78d6",
  "Retirada pelo cliente" = "#eb6834",
  "Ferrovia" = "#1baf7a",
  "Hidrovia" = "#eda100",
  "Aéreo" = "#e87ba4",
  "Duto" = "#008300",
  "Encomenda" = "#6250d6",
  "Multimodal" = "#e34948",
  "Outros ou desconhecido" = "#898781"
)

# Rampa sequencial de um so tom (azul), do claro ao escuro, para mapas de calor.
RAMPA_SEQUENCIAL <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")

COR_SERIE <- c("Sem fator" = "#2a78d6", "Com fator" = "#eb6834")

tema_cfs <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#fcfcfb", colour = NA),
      text = ggplot2::element_text(colour = "#0b0b0b"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "#52514e"),
      plot.caption = ggplot2::element_text(colour = "#898781", hjust = 0),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      panel.grid.major = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = "left",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

rotulo_num <- function(...) scales::label_number(big.mark = ".", decimal.mark = ",", ...)
