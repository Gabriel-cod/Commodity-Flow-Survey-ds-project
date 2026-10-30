# 07_secao6_descobertas.R -------------------------------------------------
# Secao 6 do Check-in 2: as tres descobertas escolhidas pelo Gabriel em
# 29/10/2026 (FSD 31.7). So define objetos. Reaproveita as Secoes 4 e 5 e
# soma, numa consulta so, o peso por familia de modal e faixa de distancia.

MEDIDAS_CARGA <- c(remessas = "Remessas", toneladas = "Toneladas")
LEGENDA_CARGA <- paste("Sem fator: % das linhas e % do peso somado da amostra.",
                       "Com fator: % das remessas estimadas e % das toneladas estimadas (peso × WGT_FACTOR).")

calcular_secao6 <- function(base, res) {
  carga <- .carga_modal_faixa(base)
  list(
    fator_retrato = .fator_retrato(res),
    modal_carga = .participacoes(carga, "familia_modal") |>
      dplyr::mutate(familia_modal = factor(familia_modal, names(FAMILIAS_MODAL))),
    distancia_carga = .participacoes(carga, "faixa") |>
      dplyr::mutate(faixa = factor(faixa, FAIXAS_DISTANCIA)),
    perto_de_casa = .perto_de_casa(res)
  )
}

graficos_secao6 <- function(res) {
  modal <- function(familia, coluna) .pct(res$modal_carga, "familia_modal", familia, coluna)
  perto <- function(coluna) .pct(res$distancia_carga, "faixa", FAIXAS_DISTANCIA[1], coluna)
  caminhao <- dplyr::filter(res$fator_retrato, familia_modal == "Caminhão")
  list(
    fator_retrato = .grafico_medianas(res$fator_retrato, sprintf(
      "Com o fator, a remessa mediana do caminhão cai de %s para %s lb",
      n_fmt(caminhao$mediana_sem_fator), n_fmt(caminhao$mediana_com_fator))),
    modal_carga = .grafico_carga(res$modal_carga, "familia_modal", sprintf(
      "Encomenda é %s das remessas estimadas e %s das toneladas; caminhão, %s e %s",
      modal("Encomenda", "pct_remessas"), modal("Encomenda", "pct_toneladas"),
      modal("Caminhão", "pct_remessas"), modal("Caminhão", "pct_toneladas"))),
    distancia_carga = .grafico_carga(res$distancia_carga, "faixa", sprintf(
      "%s das toneladas estimadas viajam até 100 km, contra %s das remessas",
      perto("pct_toneladas"), perto("pct_remessas")))
  )
}

.carga_modal_faixa <- function(base) {
  com_faixa_distancia(com_familia_modal(base)) |>
    dplyr::group_by(familia_modal, faixa) |>
    dplyr::summarise(linhas = dplyr::n(), remessas = sum(WGT_FACTOR, na.rm = TRUE),
                     toneladas_amostra = sum(peso_t, na.rm = TRUE),
                     toneladas = sum(peso_t * WGT_FACTOR, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(linhas = as.numeric(linhas))
}

.participacoes <- function(carga, coluna) {
  carga |>
    dplyr::group_by(dplyr::across(dplyr::all_of(coluna))) |>
    dplyr::summarise(dplyr::across(c(linhas, remessas, toneladas_amostra, toneladas), sum), .groups = "drop") |>
    dplyr::mutate(dplyr::across(c(linhas, remessas, toneladas_amostra, toneladas),
                                ~ 100 * .x / sum(.x), .names = "pct_{.col}")) |>
    tibble::add_column(populacao = "completa", .before = 1)
}

# Participacao nas linhas e nas remessas, com a mediana do peso sem e com fator.
.fator_retrato <- function(res) {
  medianas <- res$secao5$peso_modal |>
    dplyr::filter(p == 0.5) |>
    dplyr::select(familia_modal, mediana_sem_fator = sem_fator, mediana_com_fator = com_fator)
  res$secao4$modal |>
    dplyr::select(populacao, familia_modal, linhas, remessas, pct_linhas, pct_remessas) |>
    dplyr::inner_join(medianas, by = "familia_modal") |>
    dplyr::arrange(familia_modal)
}

.perto_de_casa <- function(res) {
  total <- dplyr::filter(res$secao5$intraestadual, ORIG_STATE == "Total")
  mo <- res$secao5$maior_origem
  tibble::tibble(
    populacao = "geografica",
    medida = c("% das toneladas que ficam no estado de origem", "Destinos cuja maior origem é o próprio estado"),
    sem_fator = c(total$pct_toneladas_amostra, sum(mo$mesmo_estado_sem_fator)),
    com_fator = c(total$pct_toneladas, sum(mo$mesmo_estado_com_fator)),
    de = c(100, nrow(mo))
  )
}

.pct <- function(df, coluna, valor, medida) {
  x <- df[[medida]][df[[coluna]] == valor]
  .rotulo_pct(if (length(x)) x else 0)
}

# Um valor por vez: n_fmt num vetor alinha com espacos e so aceita um nsmall.
# Abaixo de 0,05% o arredondamento daria "0,0%", que parece zero.
.rotulo_pct <- function(x) {
  vapply(x, function(v) if (v > 0 && v < 0.05) "<0,1%" else paste0(n_fmt(v, if (v < 1) 1 else 0), "%"), "")
}

# Halteres: o ponto marinho e a mediana da amostra, o laranja a das remessas estimadas.
.grafico_medianas <- function(fr, titulo) {
  dados <- fr |>
    tidyr::pivot_longer(c(mediana_sem_fator, mediana_com_fator), names_to = "serie", values_to = "v") |>
    dplyr::mutate(serie = factor(ifelse(serie == "mediana_sem_fator", "Sem fator", "Com fator"), names(COR_SERIE)),
                  familia_modal = forcats::fct_rev(familia_modal))
  ggplot2::ggplot(dados, ggplot2::aes(v, familia_modal)) +
    ggplot2::geom_line(ggplot2::aes(group = familia_modal), colour = COR[["linha"]], linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(colour = serie), size = 2.2) +
    ggplot2::geom_text(ggplot2::aes(label = vapply(v, n_fmt, ""), colour = serie,
                                    vjust = ifelse(serie == "Sem fator", -0.9, 1.9)),
                       size = 2.6, show.legend = FALSE) +
    ggplot2::scale_x_log10(labels = rotulo_num()) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    rotulos_cfs(titulo,
                paste("Peso da remessa mediana de cada família de modal.",
                      "Sem fator: linhas da amostra. Com fator: remessas estimadas (pesos do WGT_FACTOR)."),
                rodape_cfs(fr$populacao[1]), x = "Peso mediano (lb, escala log)", y = NULL) +
    tema_cfs()
}

# Duas medidas lado a lado (remessas e toneladas), cada uma sem e com fator.
.grafico_carga <- function(df, categoria, titulo) {
  dados <- df |>
    tidyr::pivot_longer(c(pct_linhas, pct_remessas, pct_toneladas_amostra, pct_toneladas),
                        names_to = "coluna", values_to = "pct") |>
    dplyr::mutate(
      medida = factor(MEDIDAS_CARGA[ifelse(grepl("toneladas", coluna), "toneladas", "remessas")], MEDIDAS_CARGA),
      serie = factor(ifelse(coluna %in% c("pct_linhas", "pct_toneladas_amostra"), "Sem fator", "Com fator"),
                     names(COR_SERIE)),
      rotulo = forcats::fct_rev(.data[[categoria]]),
      texto = .rotulo_pct(pct))
  ggplot2::ggplot(dados, ggplot2::aes(pct, rotulo, fill = serie)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8, reverse = TRUE), width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = texto, colour = serie),
                       position = ggplot2::position_dodge(width = 0.8, reverse = TRUE),
                       hjust = -0.15, size = 2.3, show.legend = FALSE) +
    ggplot2::facet_wrap(~medida, nrow = 1) +
    ggplot2::scale_fill_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_x_continuous(labels = rotulo_num(suffix = "%"), expand = ggplot2::expansion(c(0, 0.25))) +
    rotulos_cfs(titulo, LEGENDA_CARGA, rodape_cfs(df$populacao[1]), x = "% do total", y = NULL) +
    tema_cfs() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.spacing.x = ggplot2::unit(1.2, "lines"))
}
