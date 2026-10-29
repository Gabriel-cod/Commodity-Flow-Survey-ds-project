# 05_secao4_distribuicoes.R -----------------------------------------------
# Secao 4 do Check-in 2: distribuicoes das variaveis (FSD 31.7). So define
# objetos. calcular_secao4() devolve tabelas pequenas, calculadas no DuckDB sem
# amostragem; graficos_secao4() monta os ggplot a partir delas.

LARGURA_LOG <- 0.1
PROBS_ACUMULADA <- seq(0, 1, by = 0.005)
MARCAS_KM <- c(100, 500, 1000)
N_MERCADORIAS <- 15
N_AREAS <- 20
GRUPO_COLAPSADO <- "Colapsado ou suprimido"

calcular_secao4 <- function(base) {
  dicionario <- ler_dicionario()
  # Area com rotulo; as N maiores por linhas
  areas <- function(coluna, populacao) {
    frequencias(base, coluna, populacao) |>
      rotular(dicionario, coluna, sufixo = "_rotulo") |>
      dplyr::rename(codigo = dplyr::all_of(coluna), rotulo = dplyr::all_of(paste0(coluna, "_rotulo"))) |>
      dplyr::slice_head(n = N_AREAS)
  }
  faixas <- base |>
    dplyr::mutate(faixa = dplyr::case_when(
      dist_km <= 100 ~ "Até 100 km", dist_km <= 500 ~ "De 100 a 500 km",
      dist_km <= 1000 ~ "De 500 a 1.000 km", TRUE ~ "Mais de 1.000 km")) |>
    frequencias("faixa") |>
    dplyr::arrange(faixa) |>
    dplyr::mutate(pct_linhas_acum = cumsum(pct_linhas), pct_remessas_acum = cumsum(pct_remessas))

  sctg <- frequencias(base, "SCTG") |>
    dplyr::mutate(SCTG = ifelse(SCTG == "00" | grepl("-", SCTG), GRUPO_COLAPSADO, SCTG)) |>
    dplyr::group_by(populacao, SCTG) |>
    dplyr::summarise(dplyr::across(c(linhas, remessas, valor, toneladas, pct_linhas, pct_remessas), sum),
                     .groups = "drop") |>
    dplyr::arrange(dplyr::desc(linhas))
  mercadoria <- dplyr::bind_rows(
    dplyr::slice_head(dplyr::filter(sctg, SCTG != GRUPO_COLAPSADO), n = N_MERCADORIAS),
    dplyr::filter(sctg, SCTG == GRUPO_COLAPSADO)
  ) |>
    dplyr::left_join(de_para(dicionario, "SCTG", "_rotulo"), by = "SCTG") |>
    dplyr::mutate(SCTG_rotulo = dplyr::coalesce(SCTG_rotulo, SCTG))

  list(
    peso = histograma_log(base, "SHIPMT_WGHT", LARGURA_LOG),
    valor = histograma_log(base, "SHIPMT_VALUE", LARGURA_LOG),
    distancia = quantis(base, "dist_km", PROBS_ACUMULADA),
    distancia_faixas = faixas,
    modal = frequencias(com_familia_modal(base), "familia_modal") |>
      dplyr::mutate(familia_modal = factor(familia_modal, names(FAMILIAS_MODAL))),
    mercadoria = mercadoria,
    setor = rotular(frequencias(base, "SECTOR"), dicionario, "SECTOR", sufixo = "_rotulo"),
    origem = areas("ORIG_CFS_AREA", "origem_area"),
    destino = areas("DEST_CFS_AREA", "geografica")
  )
}

graficos_secao4 <- function(res) {
  list(
    peso = .grafico_histograma(res$peso, "Peso da remessa (lb, escala log)",
                               "A remessa típica pesa poucas libras, e poucas remessas pesam toneladas"),
    valor = .grafico_histograma(res$valor, "Valor da remessa (US$, escala log)",
                                "O valor das remessas vai de US$ 1 a dezenas de milhões"),
    distancia = .grafico_acumulada(res$distancia),
    modal = .grafico_barras(res$modal, "familia_modal",
                            "Encomendas são a maioria das remessas estimadas; caminhões, das linhas"),
    mercadoria = .grafico_pontos(res$mercadoria, "SCTG_rotulo",
                                 paste("As", N_MERCADORIAS, "mercadorias com mais linhas, e o grupo colapsado")),
    setor = .grafico_barras(res$setor, "SECTOR_rotulo",
                            "Atacado envia mais linhas; varejo, mais remessas estimadas"),
    origem = .grafico_pontos(res$origem, "rotulo", paste("As", N_AREAS, "áreas CFS de origem com mais linhas")),
    destino = .grafico_pontos(res$destino, "rotulo", paste("As", N_AREAS, "áreas CFS de destino com mais linhas"))
  )
}

.grafico_histograma <- function(h, eixo_x, titulo) {
  dados <- h |>
    dplyr::filter(!is.na(inicio)) |>
    dplyr::mutate(pct_linhas = 100 * linhas / sum(linhas), pct_remessas = 100 * remessas / sum(remessas)) |>
    series_fator()
  ggplot2::ggplot(dados) +
    ggplot2::geom_rect(ggplot2::aes(xmin = inicio, xmax = fim, ymin = 0, ymax = pct, fill = serie),
                       colour = COR[["papel"]], linewidth = 0.2) +
    ggplot2::facet_wrap(~serie, ncol = 1) +
    ggplot2::scale_x_log10(labels = rotulo_num()) +
    ggplot2::scale_fill_manual(values = COR_SERIE, guide = "none") +
    rotulos_cfs(titulo, LEGENDA_SERIES, paste(rodape_cfs(h$populacao[1]), "Faixas de 0,1 em log10."),
                x = eixo_x, y = "% em cada faixa") +
    tema_cfs()
}

.grafico_acumulada <- function(q) {
  dados <- tidyr::pivot_longer(q, c(sem_fator, com_fator), names_to = "serie", values_to = "km") |>
    dplyr::mutate(serie = factor(ifelse(serie == "sem_fator", "Sem fator", "Com fator"), names(COR_SERIE)))
  ggplot2::ggplot(dados, ggplot2::aes(km, 100 * p, colour = serie)) +
    ggplot2::geom_vline(xintercept = MARCAS_KM, colour = COR[["rotulo"]], linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_step(linewidth = 0.7, direction = "vh") +
    ggplot2::scale_x_log10(labels = rotulo_num(), breaks = c(1, 10, MARCAS_KM, 5000)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 100, 25)) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    rotulos_cfs(sprintf("Na mediana, a remessa viaja %s km em linha reta (%s km com o fator)",
                        n_fmt(q$sem_fator[q$p == 0.5]), n_fmt(q$com_fator[q$p == 0.5])),
                paste(LEGENDA_SERIES, "Linhas tracejadas em 100, 500 e 1.000 km."), rodape_cfs(q$populacao[1]),
                x = "Distância em linha reta (km, escala log)", y = "% acumulado de remessas") +
    tema_cfs()
}

.grafico_barras <- function(df, categoria, titulo) {
  dados <- series_fator(df)
  if (!is.factor(dados[[categoria]])) {
    dados[[categoria]] <- stats::reorder(dados[[categoria]], dados$pct * (dados$serie == "Sem fator"))
  } else {
    dados[[categoria]] <- forcats::fct_rev(dados[[categoria]])
  }
  ggplot2::ggplot(dados, ggplot2::aes(pct, .data[[categoria]], fill = serie)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8, reverse = TRUE), width = 0.75) +
    ggplot2::scale_fill_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_x_continuous(labels = rotulo_num(suffix = "%"), expand = ggplot2::expansion(c(0, 0.05))) +
    rotulos_cfs(titulo, LEGENDA_SERIES, rodape_cfs(df$populacao[1]), x = "% do total", y = NULL) +
    tema_cfs()
}

.grafico_pontos <- function(df, categoria, titulo) {
  dados <- series_fator(df) |>
    dplyr::mutate(rotulo = stringr::str_trunc(.data[[categoria]], 50),
                  rotulo = stats::reorder(rotulo, pct * (serie == "Sem fator")))
  ggplot2::ggplot(dados, ggplot2::aes(pct, rotulo)) +
    ggplot2::geom_line(ggplot2::aes(group = rotulo), colour = COR[["linha"]], linewidth = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = serie), size = 2.4) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_x_continuous(labels = rotulo_num(suffix = "%")) +
    rotulos_cfs(titulo, LEGENDA_SERIES, rodape_cfs(df$populacao[1]), x = "% do total da população", y = NULL) +
    tema_cfs()
}
