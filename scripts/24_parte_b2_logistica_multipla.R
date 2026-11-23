# 24_parte_b2_logistica_multipla.R ----------------------------------------
# Check-in 3, Parte B2 (secao 5, modelo multiplo, e secao 6): logistica
# multipla de caminhao no treino e o classificador no teste (FSD 32.5). So
# define objetos; o cache fica em 25_resultados_checkin3.R.

FORMULA_B2 <- caminhao ~ ln_peso + ln_dist + ln_valor + setor + perigosa + temperatura

# Perfis bem diferentes para o ponto 5.5 (a validar pelo grupo).
PERFIS_B2 <- tibble::tribble(
  ~perfil,                                ~peso_lb, ~dist_km, ~valor_usd, ~setor,                       ~perigosa, ~temperatura,
  "Encomenda do varejo",                  5,        1500,     100,        "Comércio varejista",         "Não",     "Não",
  "Carga da indústria",                   500,      300,      2000,       "Indústria de transformação", "Não",     "Não",
  "Atacado com temperatura controlada",   2000,     100,      3000,       "Comércio atacadista",        "Não",     "Sim"
)

# Limiar padrao e o segundo, escolhido pelo grupo depois da ROC (ponto 6.5):
# 0,40, porque perder uma remessa de caminhao (FN) e o erro mais grave.
LIMIARES_B2 <- c(padrao = 0.5, alternativo = 0.4)
GRADE_LIMIARES_B2 <- seq(0.1, 0.9, 0.05)

# Metricas do classificador; o evento e o 2o nivel ("Sim"), dai event_level.
METRICAS_B2 <- c("accuracy", "sens", "spec", "precision")

calcular_parte_b2 <- function(dados, res = NULL) {
  treino <- dados$treino
  teste <- dados$teste
  m <- stats::glm(FORMULA_B2, family = stats::binomial(), data = treino)
  # O glm roda dentro de uma funcao: os dados vao junto para o marginaleffects.
  m <- marginaleffects::set_modeldata(m, treino)
  simples <- stats::glm(caminhao ~ ln_peso, family = stats::binomial(), data = treino)

  prev <- list(treino = tibble::tibble(caminhao = treino$caminhao, prob = unname(stats::fitted(m))),
               teste = tibble::tibble(caminhao = teste$caminhao,
                                      prob = unname(stats::predict(m, teste, type = "response"))))

  list(
    formula = paste(deparse(FORMULA_B2), collapse = ""),
    coeficientes = broom::tidy(m, conf.int = TRUE) |>
      dplyr::mutate(rotulo = rotular_termo(term), rc = exp(estimate), rc_inf = exp(conf.low),
                    rc_sup = exp(conf.high), .after = term),
    ajuste = .ajuste_b2(simples, m),
    jornal = .jornal_b2(m, treino),
    # Explica a troca de lado do valor: remessas caras tambem sao pesadas.
    cor_valor_peso = stats::cor(treino$ln_valor, treino$ln_peso),
    perfis = .perfis_b2(m, PERFIS_B2),
    efeito_medio = .efeito_medio_b2(m),
    matrizes = purrr::map_dfr(LIMIARES_B2, ~ .matriz_confusao(prev$teste, .x), .id = "limiar_nome"),
    metricas = .metricas_b2(prev$teste),
    limiares = .desempenho_limiares(prev$teste),
    auc = purrr::map_dfr(prev, .id = "parte", ~ yardstick::roc_auc(.x, caminhao, prob, event_level = "second")),
    roc = purrr::map_dfr(prev, .id = "parte", .curva_roc)
  )
}

.ajuste_b2 <- function(simples, multipla) {
  modelos <- list(simples = simples, multipla = multipla)
  aj <- purrr::map_dfr(modelos, .id = "modelo", function(m) {
    g <- broom::glance(m)
    tibble::tibble(n = stats::nobs(m), coeficientes = length(stats::coef(m)), deviance_nula = g$null.deviance,
                   deviance = g$deviance, pseudo_r2 = 1 - g$deviance / g$null.deviance, aic = g$AIC)
  })
  # Teste da razao de verossimilhanca: o multiplo contra o simples (encaixados).
  rv <- stats::anova(simples, multipla, test = "Chisq")
  aj$gl_rv <- c(NA, rv$Df[2])
  aj$qui2_rv <- c(NA, rv$Deviance[2])
  aj$p_rv <- c(NA, rv$`Pr(>Chi)`[2])
  aj
}

# O "estudante" do ISLR 4.3.4: a razao de chances de cada variavel sozinha
# contra a do modelo multiplo, que desconta as demais.
.jornal_b2 <- function(m, treino) {
  termos <- c(ln_peso = "ln_peso", ln_dist = "ln_dist", ln_valor = "ln_valor",
              perigosa = "perigosaSim", temperatura = "temperaturaSim")
  purrr::imap_dfr(termos, function(termo, variavel) {
    sozinha <- stats::glm(stats::reformulate(variavel, "caminhao"), family = stats::binomial(), data = treino)
    tibble::tibble(term = termo, rotulo = rotular_termo(termo), rc_sozinha = exp(stats::coef(sozinha)[[termo]]),
                   rc_multipla = exp(stats::coef(m)[[termo]]))
  })
}

.novos_perfis <- function(m, perfis) {
  dplyr::mutate(perfis, ln_peso = log(peso_lb), ln_dist = log(dist_km), ln_valor = log(valor_usd),
                setor = factor(setor, m$xlevels$setor), perigosa = factor(perigosa, SIM_NAO),
                temperatura = factor(temperatura, SIM_NAO))
}

# Probabilidade de cada perfil e quanto ela muda quando o peso dobra: a mesma
# razao de chances da saltos diferentes na probabilidade (ponto 5.5).
.perfis_b2 <- function(m, perfis) {
  novos <- .novos_perfis(m, perfis)
  p <- marginaleffects::predictions(m, newdata = novos)
  d <- marginaleffects::comparisons(m, newdata = novos, variables = list(ln_peso = log(2)))
  perfis |>
    dplyr::mutate(prob = p$estimate, prob_inf = p$conf.low, prob_sup = p$conf.high,
                  dobra_pp = 100 * d$estimate, dobra_pp_inf = 100 * d$conf.low, dobra_pp_sup = 100 * d$conf.high)
}

# Variacao media da probabilidade no treino: peso dobrado e temperatura controlada.
.efeito_medio_b2 <- function(m) {
  purrr::map_dfr(list(peso_dobro = list(ln_peso = log(2)), temperatura = "temperatura"), .id = "efeito", function(v) {
    e <- marginaleffects::avg_comparisons(m, variables = v)
    tibble::tibble(pp = 100 * e$estimate, pp_inf = 100 * e$conf.low, pp_sup = 100 * e$conf.high)
  })
}

classificar_b2 <- function(prob, limiar) factor(ifelse(prob >= limiar, "Sim", "Não"), levels = SIM_NAO)

.matriz_confusao <- function(prev, limiar) {
  classe <- classificar_b2(prev$prob, limiar)
  tibble::tibble(limiar = limiar,
                 vp = sum(classe == "Sim" & prev$caminhao == "Sim"), fp = sum(classe == "Sim" & prev$caminhao == "Não"),
                 fn = sum(classe == "Não" & prev$caminhao == "Sim"), vn = sum(classe == "Não" & prev$caminhao == "Não"))
}

metricas_classe_b2 <- function(caminhao, classe) {
  d <- tibble::tibble(caminhao = caminhao, classe = classe)
  purrr::map_dfr(METRICAS_B2, function(nome) {
    f <- getExportedValue("yardstick", nome)
    tibble::tibble(metrica = nome, valor = f(d, caminhao, classe, event_level = "second")$.estimate)
  })
}

# Uma linha por classificador: os limiares do modelo e a regua da classe mais
# frequente do treino ("Nao"), que nunca preve caminhao.
.metricas_b2 <- function(prev) {
  regras <- c(purrr::map(LIMIARES_B2, ~ classificar_b2(prev$prob, .x)),
              list(regua = factor(rep("Não", nrow(prev)), levels = SIM_NAO)))
  # A precisao da regua nao existe (nenhum "Sim" previsto): o yardstick avisa e da NA.
  suppressWarnings(purrr::map_dfr(regras, ~ metricas_classe_b2(prev$caminhao, .x), .id = "classificador")) |>
    tidyr::pivot_wider(names_from = metrica, values_from = valor)
}

.desempenho_limiares <- function(prev) {
  ms <- do.call(yardstick::metric_set, lapply(METRICAS_B2, getExportedValue, ns = "yardstick"))
  probably::threshold_perf(prev, caminhao, prob, thresholds = GRADE_LIMIARES_B2, metrics = ms,
                           event_level = "second") |>
    dplyr::select(limiar = .threshold, metrica = .metric, valor = .estimate) |>
    tidyr::pivot_wider(names_from = metrica, values_from = valor)
}

# A curva completa tem um ponto por remessa; 200 pontos bastam para o grafico.
.curva_roc <- function(prev) {
  curva <- yardstick::roc_curve(prev, caminhao, prob, event_level = "second")
  curva[unique(round(seq(1, nrow(curva), length.out = 200))), c("specificity", "sensitivity")]
}

# --- Graficos ---------------------------------------------------------------

graficos_parte_b2 <- function(res_b2, base_modelagem) {
  auc <- setNames(res_b2$auc$.estimate, res_b2$auc$parte)
  roc <- dplyr::mutate(res_b2$roc, parte = factor(c(treino = "Treino", teste = "Teste")[parte], c("Treino", "Teste")))
  lim <- tidyr::pivot_longer(res_b2$limiares, -limiar, names_to = "metrica", values_to = "valor") |>
    dplyr::mutate(metrica = factor(c(accuracy = "Acurácia", sens = "Sensibilidade", spec = "Especificidade",
                                     precision = "Precisão")[metrica], c("Acurácia", "Sensibilidade", "Especificidade", "Precisão")))
  cores <- c(COR[["marinho"]], COR[["laranja"]], RAMPA_SEQUENCIAL[[4]], PALETA_MODAL[["Outros ou desconhecido"]])
  list(
    roc = ggplot2::ggplot(roc, ggplot2::aes(1 - specificity, sensitivity, colour = parte, linetype = parte)) +
      ggplot2::geom_abline(colour = COR[["rotulo"]], linetype = "dotted") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_colour_manual(values = cores[c(2, 1)], name = NULL) +
      ggplot2::scale_linetype_manual(values = c("dashed", "solid"), name = NULL) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(100 * x, "%")) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
      ggplot2::coord_equal() +
      rotulos_cfs("Curva ROC da logística múltipla",
                  sprintf("AUC de %s no treino e %s no teste. A diagonal é um classificador ao acaso (AUC 0,5).",
                          n_fmt(auc[["treino"]], 3), n_fmt(auc[["teste"]], 3)),
                  rodape_modelagem(base_modelagem, "teste"),
                  x = "1 − especificidade (falsos positivos)", y = "Sensibilidade", largura = 60) +
      tema_cfs() + ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.8, 0.2)),
    limiares = ggplot2::ggplot(lim, ggplot2::aes(limiar, 100 * valor, colour = metrica)) +
      ggplot2::geom_vline(xintercept = LIMIARES_B2, colour = COR[["rotulo"]], linetype = "dashed") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_colour_manual(values = cores, name = NULL) +
      ggplot2::scale_x_continuous(breaks = seq(0.1, 0.9, 0.1), labels = function(x) n_fmt(x, 1)) +
      ggplot2::scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      rotulos_cfs("As métricas do classificador em cada limiar",
                  "Abaixo do limiar, o modelo diz \"não sai de caminhão\". Linha tracejada: limiares usados no texto.",
                  rodape_modelagem(base_modelagem, "teste"), x = "Limiar de probabilidade", y = NULL) +
      tema_cfs() + ggplot2::theme(legend.position = "bottom")
  )
}
