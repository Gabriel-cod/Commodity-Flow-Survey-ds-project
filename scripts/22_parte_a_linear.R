# 22_parte_a_linear.R ------------------------------------------------------
# Check-in 3, Parte A (secoes 2 a 4): regressao linear multipla de ln(valor)
# no treino, avaliada no teste (FSD 32.4). So define objetos; o cache fica em
# 25_resultados_checkin3.R.

FORMULAS_A <- list(
  nulo = ln_valor ~ 1,
  inicial = ln_valor ~ ln_peso + modal + setor,
  ampliado = ln_valor ~ ln_peso + modal + setor + ln_dist + perigosa + temperatura,
  # Escolhida pelo diagnostico: a inclinacao dos residuos em ln(peso) muda de
  # setor para setor (decisao de 21/11; o quadratico em ln(peso) nao ajudou).
  modificado = ln_valor ~ ln_peso + modal + setor + ln_dist + perigosa + temperatura + ln_peso:setor
)

# Perfis do ponto 4 (propostos pelo agente, validados pelo grupo no PDF).
PERFIS_A <- tibble::tribble(
  ~perfil,                ~peso_lb, ~dist_km, ~setor,                       ~modal,      ~perigosa, ~temperatura,
  "Carga da indústria",   500,      300,      "Indústria de transformação", "Caminhão",  "Não",     "Não",
  "Encomenda do varejo",  5,        1500,     "Comércio varejista",         "Encomenda", "Não",     "Não"
)

calcular_parte_a <- function(dados, res = NULL) {
  treino <- dados$treino
  teste <- dados$teste
  modelos <- lapply(FORMULAS_A, lm, data = treino)
  final <- modelos$modificado

  list(
    formulas = vapply(FORMULAS_A, function(f) paste(deparse(f, width.cutoff = 500), collapse = ""), ""),
    coeficientes = purrr::map_dfr(modelos[-1], .coeficientes, .id = "modelo"),
    ajuste = purrr::map_dfr(modelos, .ajuste, teste = teste, .id = "modelo"),
    jornal = .jornal(modelos$ampliado, treino),
    # Explicam as trocas de sinal do jornal: o peso confunde distancia e perigosa.
    correlacoes = list(dist_peso = stats::cor(treino$ln_dist, treino$ln_peso),
                       razao_peso_perigosa = exp(diff(tapply(treino$ln_peso, treino$perigosa, mean)))[[1]]),
    anova = purrr::map_dfr(list(inicial_ampliado = c("inicial", "ampliado"),
                                ampliado_modificado = c("ampliado", "modificado")),
                           function(par) broom::tidy(anova(modelos[[par[1]]], modelos[[par[2]]]))[2, ],
                           .id = "comparacao"),
    diagnostico = .diagnostico(modelos$ampliado, treino),
    diagnostico_resumo = purrr::map_dfr(modelos[c("ampliado", "modificado")], .resumo_diagnostico, .id = "modelo"),
    atipicos = .atipicos(modelos$ampliado, treino),
    inclinacao_setor = .inclinacao_residuo(modelos$ampliado, treino),
    gvif = gvif(modelos$ampliado),
    perfis = .perfis(final, PERFIS_A),
    ponderado = .comparar_ponderado(final, treino),
    curvas_setor = .curvas_setor(final, treino)
  )
}

.coeficientes <- function(m) {
  broom::tidy(m, conf.int = TRUE) |>
    dplyr::mutate(rotulo = rotular_termo(term), efeito_pct = efeito_pct(estimate), .after = term)
}

.ajuste <- function(m, teste) {
  g <- broom::glance(m)
  tibble::tibble(n = stats::nobs(m), coeficientes = length(coef(m)), r2 = g$r.squared,
                 r2_ajustado = g$adj.r.squared, rse = g$sigma,
                 rmse_teste = sqrt(mean((teste$ln_valor - predict(m, teste))^2)),
                 f = g$statistic, gl_f = g$df, p_f = g$p.value)
}

# O "jornal" do ISLR 3.2: o coeficiente de cada variavel sozinha contra o do
# modelo ampliado, que desconta as demais.
.jornal <- function(m, treino) {
  termos <- c(ln_dist = "ln_dist", perigosa = "perigosaSim", temperatura = "temperaturaSim")
  purrr::imap_dfr(termos, function(termo, variavel) {
    sozinha <- coef(lm(stats::reformulate(variavel, "ln_valor"), data = treino))[[termo]]
    tibble::tibble(term = termo, rotulo = rotular_termo(termo), sozinha = sozinha, ampliado = coef(m)[[termo]])
  })
}

# Uma linha por remessa do treino: so as colunas que os graficos usam.
.diagnostico <- function(m, treino) {
  tibble::tibble(ajustado = fitted(m), residuo = resid(m), padronizado = rstandard(m),
                 studentizado = rstudent(m), alavancagem = hatvalues(m), cook = cooks.distance(m),
                 ln_peso = treino$ln_peso, setor = treino$setor)
}

.resumo_diagnostico <- function(m) {
  n <- stats::nobs(m)
  p1 <- length(coef(m))
  r <- resid(m)
  tibble::tibble(
    n = n, alavancagem_media = p1 / n,
    studentizado_3 = sum(abs(rstudent(m)) > 3),
    alavancagem_3x = sum(hatvalues(m) > 3 * p1 / n), alavancagem_max = max(hatvalues(m)),
    cook_4n = sum(cooks.distance(m) > 4 / n), cook_max = max(cooks.distance(m)),
    assimetria = mean(r^3) / mean(r^2)^1.5, curtose = mean(r^4) / mean(r^2)^2
  )
}

# As 10 remessas com maior |residuo studentizado|, na escala original.
.atipicos <- function(m, treino) {
  treino |>
    dplyr::mutate(studentizado = rstudent(m), previsto_usd = exp(fitted(m))) |>
    dplyr::slice_max(abs(studentizado), n = 10) |>
    dplyr::transmute(studentizado, valor_usd = exp(ln_valor), previsto_usd, peso_lb = exp(ln_peso),
                     dist_km, setor, modal)
}

# Inclinacao dos residuos do ampliado em ln(peso), por setor: perto de zero em
# todos quer dizer que uma inclinacao so serve; longe, pede a interacao.
.inclinacao_residuo <- function(m, treino) {
  treino |>
    dplyr::mutate(residuo = resid(m)) |>
    dplyr::group_by(setor) |>
    dplyr::summarise(linhas = dplyr::n(), inclinacao = coef(lm(residuo ~ ln_peso))[[2]], .groups = "drop")
}

.perfis <- function(m, perfis) {
  novos <- perfis |>
    dplyr::mutate(ln_peso = log(peso_lb), ln_dist = log(dist_km)) |>
    niveis_modelagem()
  conf <- predict(m, novos, interval = "confidence")
  pred <- predict(m, novos, interval = "prediction")
  dplyr::bind_cols(perfis, tibble::tibble(
    ln_previsto = conf[, "fit"], ic_inf = conf[, "lwr"], ic_sup = conf[, "upr"],
    ip_inf = pred[, "lwr"], ip_sup = pred[, "upr"])) |>
    dplyr::mutate(dplyr::across(c(ln_previsto, ic_inf, ic_sup, ip_inf, ip_sup), exp, .names = "{.col}_usd"))
}

# Comparacao validada pelo Gabriel (22/11): so os coeficientes do ajuste
# ponderado sao lidos; os erros-padrao dele nao valem para pesos amostrais.
.comparar_ponderado <- function(m, treino) {
  ponderado <- lm(formula(m), data = treino, weights = WGT_FACTOR)
  tibble::tibble(term = names(coef(m)), rotulo = rotular_termo(term),
                 sem_fator = unname(coef(m)), com_fator = unname(coef(ponderado)))
}

# ln(valor) previsto contra ln(peso), por setor, no caso de referencia das
# demais variaveis: mostra o que a interacao faz.
.curvas_setor <- function(m, treino) {
  faixa <- stats::quantile(treino$ln_peso, c(0.02, 0.98))
  grade <- tidyr::expand_grid(setor = levels(treino$setor), ln_peso = seq(faixa[[1]], faixa[[2]], length.out = 40)) |>
    dplyr::mutate(modal = REFERENCIAS[["modal"]], perigosa = "Não", temperatura = "Não",
                  ln_dist = stats::median(treino$ln_dist)) |>
    niveis_modelagem()
  dplyr::mutate(grade, ln_previsto = predict(m, grade))
}

# --- Graficos ---------------------------------------------------------------

# Largura do texto (caracteres) nos graficos de diagnostico, dois por linha.
LARGURA_LADO <- 44

graficos_parte_a <- function(res_a, base_modelagem) {
  d <- res_a$diagnostico
  rodape <- rodape_modelagem(base_modelagem, "treino")
  # Os 4 diagnosticos ficam lado a lado: texto mais estreito para caber.
  rotulos_lado <- function(titulo, sub, ...) rotulos_cfs(titulo, sub, rodape, ..., largura = LARGURA_LADO)
  resumo <- dplyr::filter(res_a$diagnostico_resumo, modelo == "ampliado")
  pontos <- function(...) ggplot2::geom_point(alpha = 0.05, size = 0.5, colour = COR[["marinho"]], shape = 16, ...)
  faixas <- function(dados, x, y) {
    m <- medias_faixa(dados, x, y)
    list(ggplot2::geom_line(data = m, ggplot2::aes(x, y), colour = COR[["laranja"]], linewidth = 0.7),
         ggplot2::geom_point(data = m, ggplot2::aes(x, y), colour = COR[["laranja"]], size = 1.1))
  }

  list(
    residuos = ggplot2::ggplot(d, ggplot2::aes(ajustado, residuo)) + pontos() +
      ggplot2::geom_hline(yintercept = 0, colour = COR[["secundario"]], linewidth = 0.4) +
      faixas(d, "ajustado", "residuo") +
      rotulos_lado("Resíduos × valores ajustados", "Laranja: média dos resíduos em 20 faixas de igual contagem. Sem padrão, a linha fica no zero.",
                  x = "ln(valor) ajustado", y = "Resíduo") + tema_cfs(),

    escala_locacao = ggplot2::ggplot(d, ggplot2::aes(ajustado, sqrt(abs(padronizado)))) + pontos() +
      faixas(dplyr::mutate(d, raiz = sqrt(abs(padronizado))), "ajustado", "raiz") +
      rotulos_lado("Escala-locação", "Raiz do |resíduo padronizado|. Linha laranja subindo ou descendo = variância que muda (funil).",
                  x = "ln(valor) ajustado", y = "√|resíduo padronizado|") + tema_cfs(),

    qq = .grafico_qq(d$padronizado, rodape),

    alavancagem = ggplot2::ggplot(d, ggplot2::aes(alavancagem, studentizado)) +
      ggplot2::geom_point(ggplot2::aes(colour = cook > 4 / resumo$n), alpha = 0.25, size = 0.6, shape = 16) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = COR[["rotulo"]], `TRUE` = COR[["laranja"]]),
                                   labels = c("Cook ≤ 4/n", "Cook > 4/n"), name = NULL) +
      ggplot2::geom_hline(yintercept = c(-3, 3), colour = COR[["secundario"]], linetype = "22", linewidth = 0.4) +
      ggplot2::geom_vline(xintercept = 3 * resumo$alavancagem_media, colour = COR[["secundario"]], linetype = "22", linewidth = 0.4) +
      rotulos_lado("Alavancagem × resíduo studentizado",
                  sprintf("Tracejados: |studentizado| = 3 e alavancagem = 3·(p+1)/n = %s. Cook máximo = %s.",
                          n_fmt(3 * resumo$alavancagem_media, 5), n_fmt(resumo$cook_max, 4)),
                  x = "Alavancagem (hat)", y = "Resíduo studentizado") + tema_cfs(),

    residuo_setor = ggplot2::ggplot(d, ggplot2::aes(ln_peso, residuo)) +
      ggplot2::geom_point(alpha = 0.04, size = 0.4, colour = COR[["marinho"]], shape = 16) +
      ggplot2::geom_hline(yintercept = 0, colour = COR[["secundario"]], linewidth = 0.3) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = COR[["laranja"]], linewidth = 0.7) +
      ggplot2::facet_wrap(~setor, ncol = 4, labeller = ggplot2::label_wrap_gen(22)) +
      eixo_peso(passo = 2) +
      rotulos_cfs("Resíduos do modelo ampliado × peso, por setor",
                  "Reta laranja: inclinação dos resíduos em ln(peso). Retas inclinadas = o peso pesa diferente em cada setor (interação).",
                  rodape, x = "Peso (lb, escala log)", y = "Resíduo") + tema_cfs(),

    curvas_setor = ggplot2::ggplot(res_a$curvas_setor, ggplot2::aes(ln_peso, ln_previsto, colour = setor == REFERENCIAS[["setor"]],
                                                                    group = setor)) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_text(data = .fim_curvas(res_a$curvas_setor), ggplot2::aes(y = y_rotulo, label = setor), hjust = 0, nudge_x = 0.1, size = 2.6, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = COR[["rotulo"]], `TRUE` = COR[["laranja"]]), guide = "none") +
      ggplot2::scale_x_continuous(breaks = log(10^(0:5)), labels = function(b) rotulo_num()(round(exp(b))),
                                  expand = ggplot2::expansion(mult = c(0.02, 0.45))) +
      eixo_valor() +
      rotulos_cfs("Valor previsto × peso, por setor (modelo modificado)",
                  "Remessa de caminhão, não perigosa, sem temperatura, na distância mediana. Laranja: a referência (atacado).",
                  rodape, x = "Peso (lb, escala log)") + tema_cfs()
  )
}

# Ponto final de cada curva, com o rotulo afastado dos vizinhos.
.fim_curvas <- function(curvas) {
  fim <- dplyr::ungroup(dplyr::slice_max(dplyr::group_by(curvas, setor), ln_peso, n = 1))
  dplyr::mutate(fim, y_rotulo = espalhar(ln_previsto, 0.055 * diff(range(curvas$ln_previsto))))
}

# QQ normal em 2.000 quantis: o mesmo desenho dos 70 mil pontos, mais leve.
.grafico_qq <- function(r, rodape) {
  p <- stats::ppoints(2000)
  d <- tibble::tibble(teorico = stats::qnorm(p), amostral = stats::quantile(r, p, names = FALSE))
  ggplot2::ggplot(d, ggplot2::aes(teorico, amostral)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = COR[["laranja"]], linewidth = 0.6) +
    ggplot2::geom_point(colour = COR[["marinho"]], size = 0.7, shape = 16) +
    rotulos_cfs("QQ normal dos resíduos padronizados",
                "Pontos sobre a reta laranja = resíduos normais. Pontas que se afastam = caudas mais pesadas que a normal.",
                rodape, x = "Quantil da normal", y = "Quantil dos resíduos", largura = LARGURA_LADO) + tema_cfs()
}
