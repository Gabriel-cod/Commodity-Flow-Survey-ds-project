# 06_secao5_relacoes.R ----------------------------------------------------
# Secao 5 do Check-in 2: relacoes entre variaveis (FSD 31.7). So define
# objetos. calcular_secao5() devolve tabelas pequenas, calculadas no DuckDB sem
# amostragem; graficos_secao5() monta os ggplot a partir delas.

LARGURA_CALOR <- 0.25
PROBS_CAIXA <- c(0.1, 0.25, 0.5, 0.75, 0.9)
N_MERCADORIAS_MODAL <- 15
GRUPO_DEMAIS_SCTG <- "Demais mercadorias"
MEDIDAS_MODAL_SCTG <- c(linhas = "Linhas (sem fator)", remessas = "Remessas estimadas",
                        toneladas = "Toneladas estimadas")

calcular_secao5 <- function(base) {
  modal <- com_familia_modal(base)
  list(
    peso_distancia = .mapa_calor(base),
    correlacao = .correlacao_logs(base),
    peso_modal = .caixas(modal, "SHIPMT_WGHT"),
    distancia_modal = .caixas(modal, "dist_km"),
    modal_sctg = .modal_sctg(modal),
    intraestadual = .intraestadual(base),
    maior_origem = .maior_origem(base)
  )
}

graficos_secao5 <- function(res) {
  list(
    peso_distancia = .grafico_calor(res$peso_distancia, res$correlacao),
    peso_modal = .grafico_caixas(res$peso_modal, "Peso da remessa (lb, escala log)", "pesa", "lb"),
    distancia_modal = .grafico_caixas(res$distancia_modal, "Distância em linha reta (km, escala log)",
                                      "viaja", "km"),
    modal_sctg = .grafico_modal_sctg(res$modal_sctg),
    intraestadual = .grafico_intraestadual(res$intraestadual)
  )
}

# Celulas de log10(peso) x log10(distancia); toda remessa tem os dois > 0.
.mapa_calor <- function(base) {
  con <- dbplyr::remote_con(base)
  largura <- format(LARGURA_CALOR, scientific = FALSE)
  r <- DBI::dbGetQuery(con, sprintf("
    WITH d AS (%s)
    SELECT floor(log10(SHIPMT_WGHT) / %s + 1e-9) AS classe_peso,
           floor(log10(dist_km) / %s + 1e-9) AS classe_dist,
           count(*) AS linhas, sum(WGT_FACTOR) AS remessas
    FROM d WHERE SHIPMT_WGHT > 0 AND dist_km > 0 GROUP BY ALL",
    .sql_populacao(base, "completa"), largura, largura))
  tibble::tibble(
    populacao = "completa",
    peso_inicio = 10^(r$classe_peso * LARGURA_CALOR),
    peso_fim = 10^((r$classe_peso + 1) * LARGURA_CALOR),
    dist_inicio = 10^(r$classe_dist * LARGURA_CALOR),
    dist_fim = 10^((r$classe_dist + 1) * LARGURA_CALOR),
    linhas = as.numeric(r$linhas),
    remessas = r$remessas
  ) |>
    dplyr::mutate(pct_linhas = 100 * linhas / sum(linhas), pct_remessas = 100 * remessas / sum(remessas))
}

# Pearson sem fator: a versao ponderada ainda nao foi validada (CLAUDE.md).
.correlacao_logs <- function(base) {
  con <- dbplyr::remote_con(base)
  r <- DBI::dbGetQuery(con, sprintf("
    WITH d AS (%s)
    SELECT corr(log10(SHIPMT_WGHT), log10(dist_km)) AS pearson, count(*) AS linhas
    FROM d WHERE SHIPMT_WGHT > 0 AND dist_km > 0",
    .sql_populacao(base, "completa")))
  tibble::tibble(populacao = "completa", variaveis = "log10(peso) x log10(distância)",
                 linhas = as.numeric(r$linhas), pearson_sem_fator = r$pearson)
}

.caixas <- function(modal, coluna) {
  quantis(modal, coluna, PROBS_CAIXA, grupo = "familia_modal") |>
    dplyr::mutate(variavel = .env$coluna, familia_modal = factor(familia_modal, names(FAMILIAS_MODAL)),
                  .after = "populacao")
}

# Participacao de cada familia de modal dentro de cada mercadoria; as que
# ficam fora das N com mais linhas viram um grupo so.
.modal_sctg <- function(modal) {
  fr <- frequencias(modal, c("SCTG", "familia_modal"), populacao = "mercadoria")
  maiores <- fr |>
    dplyr::group_by(SCTG) |>
    dplyr::summarise(linhas = sum(linhas)) |>
    dplyr::slice_max(linhas, n = N_MERCADORIAS_MODAL, with_ties = FALSE) |>
    dplyr::pull(SCTG)
  rotulos <- de_para(ler_dicionario(), "SCTG", "_rotulo")
  fr |>
    dplyr::mutate(SCTG = ifelse(SCTG %in% maiores, SCTG, GRUPO_DEMAIS_SCTG)) |>
    dplyr::group_by(populacao, SCTG, familia_modal) |>
    dplyr::summarise(dplyr::across(c(linhas, remessas, toneladas), sum), .groups = "drop_last") |>
    dplyr::mutate(pct_linhas = 100 * linhas / sum(linhas), pct_remessas = 100 * remessas / sum(remessas),
                  pct_toneladas = 100 * toneladas / sum(toneladas)) |>
    dplyr::ungroup() |>
    dplyr::left_join(rotulos, by = "SCTG") |>
    dplyr::mutate(SCTG_rotulo = dplyr::coalesce(SCTG_rotulo, SCTG),
                  familia_modal = factor(familia_modal, names(FAMILIAS_MODAL)))
}

# Toneladas por par de estados na populacao geografica. Sem fator: soma do peso
# da amostra; com fator: toneladas estimadas (soma de peso x fator).
.pares_estados <- function(base) {
  filtrar_populacao(base, "geografica") |>
    dplyr::group_by(ORIG_STATE, DEST_STATE) |>
    dplyr::summarise(linhas = dplyr::n(), remessas = sum(WGT_FACTOR, na.rm = TRUE),
                     toneladas_amostra = sum(peso_t, na.rm = TRUE),
                     toneladas = sum(peso_t * WGT_FACTOR, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::mutate(linhas = as.numeric(linhas))
}

.intraestadual <- function(base) {
  pares <- .pares_estados(base)
  resumir <- function(df) {
    dplyr::summarise(df,
      linhas_total = sum(linhas),
      pct_linhas = 100 * sum(linhas[ORIG_STATE == DEST_STATE]) / sum(linhas),
      pct_remessas = 100 * sum(remessas[ORIG_STATE == DEST_STATE]) / sum(remessas),
      pct_toneladas_amostra = 100 * sum(toneladas_amostra[ORIG_STATE == DEST_STATE]) / sum(toneladas_amostra),
      pct_toneladas = 100 * sum(toneladas[ORIG_STATE == DEST_STATE]) / sum(toneladas),
      .groups = "drop")
  }
  por_estado <- resumir(dplyr::group_by(pares, ORIG_STATE)) |>
    rotular(ler_dicionario(), "ORIG_STATE", sufixo = "_rotulo")
  dplyr::bind_rows(dplyr::mutate(resumir(pares), ORIG_STATE = "Total", ORIG_STATE_rotulo = "Estados Unidos"),
                   por_estado) |>
    tibble::add_column(populacao = "geografica", .before = 1)
}

# Para cada destino, a origem com mais toneladas, sem e com fator.
.maior_origem <- function(base) {
  pares <- .pares_estados(base)
  maior <- function(coluna, sufixo) {
    pares |>
      dplyr::group_by(DEST_STATE) |>
      dplyr::mutate(pct = 100 * .data[[coluna]] / sum(.data[[coluna]])) |>
      dplyr::slice_max(pct, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(DEST_STATE, origem = ORIG_STATE, pct, mesmo_estado = ORIG_STATE == DEST_STATE) |>
      dplyr::rename_with(~ paste0(.x, sufixo), -DEST_STATE)
  }
  estados <- de_para(ler_dicionario(), "ORIG_STATE", "_rotulo")
  nome <- function(codigo) estados$ORIG_STATE_rotulo[match(codigo, estados$ORIG_STATE)]
  dplyr::inner_join(maior("toneladas_amostra", "_sem_fator"), maior("toneladas", "_com_fator"),
                    by = "DEST_STATE") |>
    dplyr::mutate(DEST_STATE_rotulo = nome(DEST_STATE), origem_rotulo_sem_fator = nome(origem_sem_fator),
                  origem_rotulo_com_fator = nome(origem_com_fator), .after = "DEST_STATE") |>
    tibble::add_column(populacao = "geografica", .before = 1) |>
    dplyr::arrange(dplyr::desc(pct_com_fator))
}

.grafico_calor <- function(m, correlacao) {
  dados <- series_fator(m) |>
    dplyr::filter(pct > 0)
  ggplot2::ggplot(dados) +
    ggplot2::geom_rect(ggplot2::aes(xmin = dist_inicio, xmax = dist_fim, ymin = peso_inicio, ymax = peso_fim,
                                    fill = pct)) +
    ggplot2::facet_wrap(~serie) +
    ggplot2::scale_x_log10(labels = rotulo_num()) +
    ggplot2::scale_y_log10(labels = rotulo_num()) +
    ggplot2::scale_fill_gradientn(colours = RAMPA_SEQUENCIAL, transform = "log10",
                                  breaks = 10^c(-4, -2, 0), labels = function(x) paste0(vapply(x, num_br, ""), "%"),
                                  name = "% em cada célula") +
    rotulos_cfs(sprintf("Peso e distância têm correlação %s (Pearson dos logs = %s, sem fator)",
                        .forca_correlacao(correlacao$pearson_sem_fator),
                        n_fmt(correlacao$pearson_sem_fator, 2)),
                paste(LEGENDA_SERIES, "Cor em escala log."),
                paste(rodape_cfs(m$populacao[1]), "Células de 0,25 em log10. A distância vem em milhas",
                      "inteiras, por isso a coluna vazia entre 1,8 e 3,2 km."),
                x = "Distância em linha reta (km, escala log)", y = "Peso (lb, escala log)") +
    tema_cfs() +
    ggplot2::theme(legend.key.width = ggplot2::unit(2.5, "lines"))
}

.forca_correlacao <- function(r) {
  forca <- if (abs(r) < 0.1) "desprezível" else if (abs(r) < 0.4) "fraca" else if (abs(r) < 0.7) "moderada" else "forte"
  paste(if (r < 0) "negativa" else "positiva", forca)
}

# Caixas montadas a mao: caixa do Q1 ao Q3, traco na mediana e haste do P10 ao
# P90. y numerico para pôr as duas series lado a lado.
.grafico_caixas <- function(q, eixo_x, verbo, unidade) {
  largas <- q |>
    tidyr::pivot_longer(c(sem_fator, com_fator), names_to = "serie", values_to = "v") |>
    dplyr::mutate(serie = factor(ifelse(serie == "sem_fator", "Sem fator", "Com fator"), names(COR_SERIE)),
                  p = paste0("p", round(100 * p))) |>
    tidyr::pivot_wider(id_cols = c(familia_modal, serie), names_from = p, values_from = v) |>
    dplyr::mutate(y = length(FAMILIAS_MODAL) + 1 - as.integer(familia_modal) +
                    ifelse(serie == "Sem fator", 0.18, -0.18))
  medianas <- dplyr::filter(largas, serie == "Sem fator")
  # empates na mediana entram todos no titulo
  extremo <- function(f) {
    v <- f(medianas$p50)
    list(v = n_fmt(v), quem = paste(tolower(medianas$familia_modal[medianas$p50 == v]), collapse = " e "))
  }
  maior <- extremo(max)
  menor <- extremo(min)
  titulo <- sprintf("A remessa mediana %s %s %s por %s e %s %s por %s (sem fator)", verbo,
                    maior$v, unidade, maior$quem, menor$v, unidade, menor$quem)
  ggplot2::ggplot(largas, ggplot2::aes(y = y, colour = serie)) +
    ggplot2::geom_segment(ggplot2::aes(x = p10, xend = p90, yend = y), linewidth = 0.4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = p25, xmax = p75, ymin = y - 0.14, ymax = y + 0.14, fill = serie),
                       linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = p50, xend = p50, y = y - 0.14, yend = y + 0.14),
                          colour = "#0b0b0b", linewidth = 0.7) +
    ggplot2::scale_x_log10(labels = rotulo_num()) +
    ggplot2::scale_y_continuous(breaks = rev(seq_along(FAMILIAS_MODAL)), labels = names(FAMILIAS_MODAL)) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_fill_manual(values = scales::alpha(COR_SERIE, 0.35), name = NULL) +
    rotulos_cfs(titulo,
                paste("Caixa do 1º ao 3º quartil, traço na mediana, haste do 10º ao 90º percentil.",
                      "Sem fator: linhas da amostra. Com fator: remessas estimadas (pesos do WGT_FACTOR)."),
                rodape_cfs(q$populacao[1]), x = eixo_x, y = NULL) +
    tema_cfs() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

.grafico_modal_sctg <- function(ms) {
  ordem <- ms |>
    dplyr::filter(familia_modal == "Caminhão") |>
    dplyr::arrange(pct_toneladas) |>
    dplyr::pull(SCTG_rotulo)
  ordem <- c(setdiff(unique(ms$SCTG_rotulo), ordem), ordem)
  dados <- ms |>
    tidyr::pivot_longer(c(pct_linhas, pct_remessas, pct_toneladas), names_to = "medida", values_to = "pct") |>
    dplyr::mutate(medida = factor(MEDIDAS_MODAL_SCTG[sub("pct_", "", medida)], MEDIDAS_MODAL_SCTG),
                  rotulo = factor(stringr::str_trunc(SCTG_rotulo, 40), unique(stringr::str_trunc(ordem, 40))),
                  familia_modal = forcats::fct_rev(familia_modal))
  lider <- function(coluna) {
    ms |>
      dplyr::group_by(SCTG) |>
      dplyr::slice_max(.data[[coluna]], n = 1, with_ties = FALSE) |>
      dplyr::pull(familia_modal) |>
      table()
  }
  ton <- lider("pct_toneladas")
  rem <- lider("pct_remessas")
  n_grupos <- dplyr::n_distinct(ms$SCTG)
  quantos <- function(n) if (n == n_grupos) paste("todos os", n_grupos) else paste(n, "dos", n_grupos)
  titulo <- sprintf("Em toneladas, o caminhão lidera em %s grupos de mercadoria; em remessas, a encomenda lidera em %s",
                    quantos(ton[["Caminhão"]]), quantos(rem[["Encomenda"]]))
  ggplot2::ggplot(dados, ggplot2::aes(pct, rotulo, fill = familia_modal)) +
    ggplot2::geom_col(width = 0.8, colour = "#fcfcfb", linewidth = 0.15) +
    ggplot2::facet_wrap(~medida, nrow = 1) +
    ggplot2::scale_fill_manual(values = PALETA_MODAL, name = NULL, breaks = names(FAMILIAS_MODAL)) +
    ggplot2::scale_x_continuous(labels = rotulo_num(suffix = "%"), breaks = c(25, 50, 75),
                                expand = ggplot2::expansion(0)) +
    rotulos_cfs(titulo,
                paste("Participação de cada família de modal dentro de cada mercadoria, em três medidas.",
                      "Mercadorias ordenadas pela participação do caminhão nas toneladas."),
                rodape_cfs(ms$populacao[1]), x = "% dentro da mercadoria", y = NULL) +
    tema_cfs() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1, "lines")) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3))
}

.grafico_intraestadual <- function(ie) {
  total <- dplyr::filter(ie, ORIG_STATE == "Total")
  dados <- dplyr::filter(ie, ORIG_STATE != "Total") |>
    tidyr::pivot_longer(c(pct_toneladas_amostra, pct_toneladas), names_to = "serie", values_to = "pct") |>
    dplyr::mutate(serie = factor(ifelse(serie == "pct_toneladas_amostra", "Sem fator", "Com fator"),
                                 names(COR_SERIE)),
                  rotulo = stats::reorder(ORIG_STATE_rotulo, pct * (serie == "Com fator")))
  ggplot2::ggplot(dados, ggplot2::aes(pct, rotulo)) +
    ggplot2::geom_vline(xintercept = total$pct_toneladas, colour = "#898781", linetype = "dashed",
                        linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(group = rotulo), colour = "#c3c2b9", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = serie), size = 1.8) +
    ggplot2::scale_colour_manual(values = COR_SERIE, name = NULL) +
    ggplot2::scale_x_continuous(labels = rotulo_num(suffix = "%"), limits = c(0, 100)) +
    rotulos_cfs(sprintf("%s das toneladas estimadas não saem do estado de origem",
                        paste0(n_fmt(total$pct_toneladas), "%")),
                paste("% das toneladas com destino no mesmo estado de origem.",
                      "Sem fator: soma do peso da amostra. Com fator: toneladas estimadas (peso × WGT_FACTOR).",
                      "Linha tracejada: total dos EUA, com fator."),
                rodape_cfs(ie$populacao[1]), x = "% das toneladas que ficam no estado", y = NULL) +
    tema_cfs() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.5))
}
