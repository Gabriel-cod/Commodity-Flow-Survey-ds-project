source(here::here("scripts", "02_trata_base.R"))
source(here::here("scripts", "25_resultados_checkin3.R"))
source(here::here("scripts", "21_base_modelagem.R"))
source(here::here("scripts", "22_parte_a_linear.R"))
source(here::here("scripts", "23_parte_b1_logistica_simples.R"))
source(here::here("scripts", "24_parte_b2_logistica_multipla.R"))

base_fixture <- function(env = parent.frame()) {
  base <- suppressMessages(obter_base_tratada(pipeline = TRUE, origem = test_path("fixtures", "cfs_mini.csv")))
  withr::defer(fechar_base(base), envir = env)
  base
}

# Amostra sintetica no formato de sortear_modelagem(), para a divisao.
amostra_sintetica <- function(n = 1000, semente = 1) {
  withr::with_seed(semente, tibble::tibble(
    ln_valor = rnorm(n, 5), ln_peso = rnorm(n, 4), ln_dist = rnorm(n, 6), dist_km = exp(ln_dist),
    setor = sample(names(SETORES), n, TRUE), modal = sample(unique(MODAIS), n, TRUE),
    perigosa = sample(SIM_NAO, n, TRUE, c(0.9, 0.1)), temperatura = sample(SIM_NAO, n, TRUE, c(0.8, 0.2)),
    caminhao = sample(SIM_NAO, n, TRUE, c(0.7, 0.3)), WGT_FACTOR = runif(n, 1, 100)
  )) |> fatorar_modelagem()
}

test_that("variaveis da modelagem na fixture (T-M1)", {
  base <- base_fixture()
  con <- dbplyr::remote_con(base)
  d <- DBI::dbGetQuery(con, sql_modelagem(base))
  orig <- dplyr::collect(base) |> dplyr::filter(!fl_sctg_colapsado, MODE != "30")

  expect_equal(nrow(d), nrow(orig))
  expect_equal(nrow(d), 17)
  expect_setequal(unique(d$modal), c("Caminhão", "Encomenda", "Aéreo", MODAL_AGRUPADO))
  expect_equal(d$caminhao == "Sim", orig$MODE %in% c("111", "112"))
  expect_equal(d$perigosa == "Sim", orig$HAZMAT %in% c("H", "P"))
  expect_equal(d$temperatura == "Sim", orig$TEMP_CNTL_YN == "Y")
  expect_equal(d$ln_valor, log(orig$SHIPMT_VALUE))
  expect_equal(d$ln_peso, log(orig$SHIPMT_WGHT))
  expect_equal(d$ln_dist, log(orig$dist_km))
  # 113 conta como nao (veiculo do cliente)
  expect_equal(unname(MODAIS["113"]), "Retirada pelo cliente")
  expect_false("113" %in% MODOS_CAMINHAO)
})

test_that("sorteio e divisao reprodutiveis (T-M2)", {
  base <- base_fixture()
  con <- dbplyr::remote_con(base)
  sql <- sql_modelagem(base)
  expect_identical(sortear_modelagem(con, sql, 10, 7), sortear_modelagem(con, sql, 10, 7))
  expect_equal(nrow(sortear_modelagem(con, sql, 10, 7)), 10)

  a <- amostra_sintetica()
  p1 <- dividir_modelagem(a, 0.7, 2026)
  p2 <- dividir_modelagem(a, 0.7, 2026)
  expect_identical(p1, p2)
  expect_false(identical(p1, dividir_modelagem(a, 0.7, 1)))
  expect_equal(nrow(p1$treino) + nrow(p1$teste), nrow(a))
  expect_equal(nrow(p1$treino) / nrow(a), 0.7, tolerance = 0.01)
  expect_equal(nrow(dplyr::intersect(p1$treino, p1$teste)), 0)
  expect_equal(mean(p1$treino$caminhao == "Sim"), mean(p1$teste$caminhao == "Sim"), tolerance = 0.01)
  # nao mexe no gerador de quem chamou
  set.seed(5); x <- runif(1); set.seed(5); dividir_modelagem(a, 0.7, 2026)
  expect_equal(runif(1), x)
})

test_that("niveis e referencias fixos, mesmo com categoria ausente (T-M3)", {
  d <- fatorar_modelagem(tibble::tibble(setor = "21", modal = "Aéreo", perigosa = "Não",
                                        temperatura = "Sim", caminhao = "Não"))
  expect_equal(levels(d$setor)[1], "Comércio atacadista")
  expect_length(levels(d$setor), 7)
  expect_equal(levels(d$modal)[1], "Caminhão")
  expect_length(levels(d$modal), 6)
  for (v in c("perigosa", "temperatura", "caminhao")) expect_equal(levels(d[[v]]), c("Não", "Sim"))
  # o evento da logistica e o 2o nivel (event_level = "second")
  expect_equal(levels(d$caminhao)[2], "Sim")
})

test_that("cache do Check-in 3 grava, reaproveita e recalcula (T-M5)", {
  base <- base_fixture()
  pasta <- withr::local_tempdir()
  # So a base: a fixture tem linhas de menos para os modelos (T-M6 usa amostra sintetica).
  r1 <- suppressMessages(obter_resultados_checkin3(base, "base_modelagem", pasta = pasta))
  b <- r1$base_modelagem
  expect_equal(b$populacao$linhas, 17)
  expect_equal(b$populacao$fora, 3)
  expect_equal(b$populacao$ausentes, 0)
  expect_equal(sum(b$resumo$linhas), 17)
  expect_true(file.exists(file.path(pasta, "base_modelagem.rds")))

  expect_message(r2 <- obter_resultados_checkin3(base, "base_modelagem", pasta = pasta), NA)
  expect_identical(r1, r2)

  # outro script (chave diferente) recalcula
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(readLines(here::here("scripts", "21_base_modelagem.R")), "# mudou"), script)
  expect_message(obter_resultados_checkin3(base, "base_modelagem", pasta = pasta, scripts = script), "Calculando")
})

test_that("parte A ajusta os 4 modelos e devolve as tabelas do relatorio (T-M6)", {
  a <- amostra_sintetica(4000)
  # Resposta com relacao conhecida: elasticidade 0,6 e perigosa -0,5.
  a$ln_valor <- withr::with_seed(2, 1 + 0.6 * a$ln_peso - 0.5 * (a$perigosa == "Sim") + rnorm(nrow(a)))
  partes <- dividir_modelagem(a, 0.7, 1)
  r <- calcular_parte_a(partes)

  expect_equal(r$ajuste$modelo, names(FORMULAS_A))
  expect_equal(r$ajuste$coeficientes, c(1, 13, 16, 22))
  expect_equal(r$ajuste$n, rep(nrow(partes$treino), 4))
  expect_true(all(diff(r$ajuste$r2[-1]) >= 0))
  coef_amp <- dplyr::filter(r$coeficientes, modelo == "ampliado")
  expect_equal(coef_amp$estimate[coef_amp$term == "ln_peso"], 0.6, tolerance = 0.05)
  expect_equal(coef_amp$estimate[coef_amp$term == "perigosaSim"], -0.5, tolerance = 0.3)
  expect_equal(nrow(r$jornal), 3)
  expect_equal(r$anova$comparacao, c("inicial_ampliado", "ampliado_modificado"))
  expect_equal(nrow(r$diagnostico), nrow(partes$treino))
  expect_equal(nrow(r$perfis), nrow(PERFIS_A))
  expect_true(all(r$perfis$ip_inf_usd < r$perfis$ic_inf_usd & r$perfis$ic_sup_usd < r$perfis$ip_sup_usd))
  expect_equal(r$ponderado$term, names(coef(lm(FORMULAS_A$modificado, partes$treino))))
  expect_equal(nrow(r$curvas_setor), length(SETORES) * 40)

  bm <- list(treino = partes$treino, modelagem = list(tamanho_amostra = 4000, semente = 1))
  g <- graficos_parte_a(r, bm)
  expect_true(all(vapply(g, ggplot2::is_ggplot, TRUE)))
})

test_that("parte B1 recupera a razao de chances e devolve as tabelas do relatorio (T-M6)", {
  a <- amostra_sintetica(4000)
  # Evento com logit conhecido: -2 + 0,5 * ln(peso), ou RC = e^0,5 por unidade.
  pr <- stats::plogis(-2 + 0.5 * a$ln_peso)
  a$caminhao <- factor(SIM_NAO[1 + withr::with_seed(3, stats::rbinom(nrow(a), 1, pr))], levels = SIM_NAO)
  partes <- dividir_modelagem(a, 0.7, 1)
  r <- calcular_parte_b1(partes)

  expect_equal(r$evento$parte, c("treino", "teste"))
  expect_equal(r$evento$linhas, c(nrow(partes$treino), nrow(partes$teste)))
  expect_equal(r$ajuste$n, nrow(partes$treino))
  # O evento modelado e o 2o nivel ("Sim"): com o nivel trocado o sinal inverteria.
  expect_equal(r$coeficientes$estimate[r$coeficientes$term == "ln_peso"], 0.5, tolerance = 0.15)
  expect_equal(r$coeficientes$rc, exp(r$coeficientes$estimate))
  expect_equal(r$rc_dobro$rc, exp(0.5 * log(2)), tolerance = 0.1)
  # Mesma RC em todos os pesos: a chance sempre multiplica pelo mesmo fator.
  expect_equal(r$dobra$chance_dobro / r$dobra$chance, rep(r$rc_dobro$rc, nrow(r$dobra)))
  expect_true(r$efeito_medio$pp > 0 && r$efeito_medio$pp_inf < r$efeito_medio$pp)
  expect_equal(sum(r$faixas$n), nrow(partes$treino))
  expect_true(all(r$faixas$prevista >= 0 & r$faixas$prevista <= 1))
  expect_equal(nrow(r$curva), 200)

  bm <- list(treino = partes$treino, modelagem = list(tamanho_amostra = 4000, semente = 1))
  g <- graficos_parte_b1(r, bm)
  expect_true(all(vapply(g, ggplot2::is_ggplot, TRUE)))
})

test_that("metricas do classificador iguais a conta da matriz de confusao (T-M4)", {
  # 4 VP, 1 FN, 2 FP, 3 VN: o evento e "Sim", o 2o nivel.
  real <- factor(c(rep("Sim", 5), rep("Não", 5)), levels = SIM_NAO)
  prob <- c(0.9, 0.8, 0.7, 0.6, 0.2, 0.55, 0.51, 0.3, 0.2, 0.1)
  prev <- tibble::tibble(caminhao = real, prob = prob)
  mc <- .matriz_confusao(prev, 0.5)
  expect_equal(unlist(mc[c("vp", "fp", "fn", "vn")]), c(vp = 4, fp = 2, fn = 1, vn = 3))
  m <- metricas_classe_b2(real, classificar_b2(prob, 0.5))
  # Com o evento no 1o nivel, sens e spec sairiam trocadas (3/5 e 4/5).
  esperado <- c(accuracy = 7 / 10, sens = 4 / 5, spec = 3 / 5, precision = 4 / 6)
  expect_equal(setNames(m$valor, m$metrica), esperado)
  expect_equal(classificar_b2(c(0.49, 0.5), 0.5), factor(c("Não", "Sim"), levels = SIM_NAO))
})

test_that("parte B2 ajusta o multiplo e mede o classificador no teste (T-M6)", {
  a <- amostra_sintetica(4000)
  # Evento com logit conhecido: peso aumenta, temperatura controlada aumenta.
  pr <- stats::plogis(-2 + 0.5 * a$ln_peso + 1 * (a$temperatura == "Sim"))
  a$caminhao <- factor(SIM_NAO[1 + withr::with_seed(4, stats::rbinom(nrow(a), 1, pr))], levels = SIM_NAO)
  partes <- dividir_modelagem(a, 0.7, 1)
  r <- calcular_parte_b2(partes)

  expect_equal(nrow(r$coeficientes), 12)
  expect_equal(r$coeficientes$rc[r$coeficientes$term == "temperaturaSim"], exp(1), tolerance = 0.2)
  expect_equal(r$ajuste$modelo, c("simples", "multipla"))
  expect_equal(r$ajuste$gl_rv[2], 10)
  expect_equal(r$jornal$term, c("ln_peso", "ln_dist", "ln_valor", "perigosaSim", "temperaturaSim"))
  expect_equal(nrow(r$perfis), nrow(PERFIS_B2))
  expect_true(all(r$perfis$prob_inf <= r$perfis$prob & r$perfis$prob <= r$perfis$prob_sup))
  expect_equal(r$efeito_medio$efeito, c("peso_dobro", "temperatura"))
  # A matriz soma o teste, e as metricas do limiar 0,5 saem dela.
  mc <- r$matrizes[r$matrizes$limiar_nome == "padrao", ]
  expect_equal(mc$vp + mc$fp + mc$fn + mc$vn, nrow(partes$teste))
  met <- r$metricas[r$metricas$classificador == "padrao", ]
  expect_equal(met$sens, mc$vp / (mc$vp + mc$fn))
  expect_equal(met$spec, mc$vn / (mc$vn + mc$fp))
  # Um limiar mais baixo diz "caminhao" mais vezes: sensibilidade nao cai, especificidade nao sobe.
  expect_equal(r$matrizes$limiar_nome, names(LIMIARES_B2))
  alt <- r$metricas[r$metricas$classificador == "alternativo", ]
  expect_gte(alt$sens, met$sens)
  expect_lte(alt$spec, met$spec)
  # A regua (sempre "Nao") tem sensibilidade 0 e acuracia = proporcao de "Nao" no teste.
  regua <- r$metricas[r$metricas$classificador == "regua", ]
  expect_equal(regua$sens, 0)
  expect_equal(regua$accuracy, mean(partes$teste$caminhao == "Não"))
  expect_equal(r$limiares$limiar, GRADE_LIMIARES_B2)
  expect_equal(r$auc$parte, c("treino", "teste"))
  expect_true(all(r$auc$.estimate > 0.5))

  bm <- list(treino = partes$treino, teste = partes$teste, modelagem = list(tamanho_amostra = 4000, semente = 1))
  g <- graficos_parte_b2(r, bm)
  expect_true(all(vapply(g, ggplot2::is_ggplot, TRUE)))
})

