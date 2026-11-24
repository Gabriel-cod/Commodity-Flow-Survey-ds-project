# 23_parte_b1_logistica_simples.R -----------------------------------------
# Check-in 3, Parte B1 (secao 5, modelo simples): regressao logistica de
# caminhao (evento "Sim") em ln(peso), no treino (FSD 32.5). So define
# objetos; o cache fica em 25_resultados_checkin3.R.

FORMULA_B1 <- caminhao ~ ln_peso

# Faixas fixas de peso (lb): as de igual contagem partiriam os empates em 1 lb.
CORTES_PESO_B1 <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 50000, Inf)

# Ponto 5.5: o mesmo "dobrar o peso" (mesma razao de chances) em tres pontos
# da curva, para mostrar que a variacao na probabilidade muda.
PESOS_DOBRA_B1 <- c(1, 20, 2000)

# Acima de 10 mil lb a curva observada cai: a composicao dos modais explica.
CORTES_PESADAS_B1 <- c(10000, 50000, Inf)

calcular_parte_b1 <- function(dados, res = NULL) {
  treino <- dados$treino
  m <- stats::glm(FORMULA_B1, family = stats::binomial(), data = treino)

  coeficientes <- broom::tidy(m, conf.int = TRUE) |>
    dplyr::mutate(rotulo = rotular_termo(term), rc = exp(estimate), rc_inf = exp(conf.low),
                  rc_sup = exp(conf.high), .after = term)
  peso <- dplyr::filter(coeficientes, term == "ln_peso")

  list(
    formula = paste(deparse(FORMULA_B1), collapse = ""),
    evento = .evento_b1(dados),
    coeficientes = coeficientes,
    ajuste = .ajuste_b1(m),
    # Dobrar o peso soma log(2) a ln(peso): a RC vira exp(b * log 2).
    rc_dobro = tibble::tibble(rc = exp(peso$estimate * log(2)), rc_inf = exp(peso$conf.low * log(2)),
                              rc_sup = exp(peso$conf.high * log(2))),
    dobra = .dobra_peso(m, PESOS_DOBRA_B1),
    efeito_medio = .efeito_medio_b1(m),
    faixas = .faixas_peso(m, treino),
    pesadas = .composicao_pesadas(treino),
    curva = .curva_b1(m, treino)
  )
}

.evento_b1 <- function(dados) {
  purrr::map_dfr(dados[c("treino", "teste")], .id = "parte", function(d) {
    tibble::tibble(linhas = nrow(d), sim = sum(d$caminhao == "Sim"), pct_sim = 100 * mean(d$caminhao == "Sim"))
  })
}

.ajuste_b1 <- function(m) {
  g <- broom::glance(m)
  tibble::tibble(n = stats::nobs(m), deviance_nula = g$null.deviance, deviance = g$deviance,
                 # Quanto da deviance do modelo nulo o peso explica (pseudo-R2 de McFadden).
                 pseudo_r2 = 1 - g$deviance / g$null.deviance, aic = g$AIC)
}

.dobra_peso <- function(m, pesos) {
  novos <- data.frame(ln_peso = log(c(pesos, 2 * pesos)))
  p <- unname(stats::predict(m, novos, type = "response"))
  n <- length(pesos)
  tibble::tibble(peso_lb = pesos, peso_dobro_lb = 2 * pesos, prob = p[seq_len(n)],
                 prob_dobro = p[n + seq_len(n)], dif_pp = 100 * (prob_dobro - prob),
                 chance = prob / (1 - prob), chance_dobro = prob_dobro / (1 - prob_dobro))
}

# Variacao media da probabilidade quando o peso de cada remessa do treino dobra.
.efeito_medio_b1 <- function(m) {
  # O glm roda dentro de uma funcao: os dados vao junto para o marginaleffects
  # nao procurar o treino no ambiente.
  m <- marginaleffects::set_modeldata(m, m$data)
  e <- marginaleffects::avg_comparisons(m, variables = list(ln_peso = log(2)))
  tibble::tibble(pp = 100 * e$estimate, pp_inf = 100 * e$conf.low, pp_sup = 100 * e$conf.high)
}

# Proporcao observada de caminhao por faixa de peso contra a media prevista.
.faixas_peso <- function(m, treino) {
  treino |>
    dplyr::mutate(prev = stats::fitted(m),
                  faixa = cut(exp(ln_peso), CORTES_PESO_B1, right = TRUE, dig.lab = 6)) |>
    dplyr::group_by(faixa) |>
    dplyr::summarise(n = dplyr::n(), peso_medio_lb = exp(mean(ln_peso)),
                     observada = mean(caminhao == "Sim"), prevista = mean(prev), .groups = "drop") |>
    dplyr::mutate(faixa = as.character(faixa))
}

.composicao_pesadas <- function(treino) {
  treino |>
    dplyr::mutate(faixa = cut(exp(ln_peso), CORTES_PESADAS_B1, dig.lab = 6)) |>
    dplyr::filter(!is.na(faixa)) |>
    dplyr::count(faixa, modal) |>
    dplyr::group_by(faixa) |>
    dplyr::mutate(pct = 100 * n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(faixa = as.character(faixa))
}

.curva_b1 <- function(m, treino) {
  grade <- data.frame(ln_peso = seq(min(treino$ln_peso), max(treino$ln_peso), length.out = 200))
  dplyr::mutate(tibble::as_tibble(grade), prob = stats::predict(m, grade, type = "response"))
}

# --- Graficos ---------------------------------------------------------------

graficos_parte_b1 <- function(res_b1, base_modelagem) {
  rodape <- rodape_modelagem(base_modelagem, "treino")
  faixas <- dplyr::mutate(res_b1$faixas, ln_peso = log(peso_medio_lb))
  list(
    curva = ggplot2::ggplot(res_b1$curva, ggplot2::aes(ln_peso, 100 * prob)) +
      ggplot2::geom_line(colour = COR[["marinho"]], linewidth = 1) +
      ggplot2::geom_point(data = faixas, ggplot2::aes(y = 100 * observada, size = n),
                          colour = COR[["laranja"]], alpha = 0.85) +
      ggplot2::scale_size_area(max_size = 5, guide = "none") +
      eixo_peso() +
      ggplot2::scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      rotulos_cfs("Probabilidade de sair de caminhão × peso",
                  "Linha: logística simples. Pontos: proporção observada em cada faixa de peso (área proporcional ao número de remessas).",
                  rodape, x = "Peso (lb, escala log)", y = "De caminhão") + tema_cfs()
  )
}
