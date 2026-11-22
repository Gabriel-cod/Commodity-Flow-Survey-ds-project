source(here::here("scripts", "02_trata_base.R"))
source(here::here("scripts", "25_resultados_checkin3.R"))
source(here::here("scripts", "21_base_modelagem.R"))

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
  r1 <- suppressMessages(obter_resultados_checkin3(base, pasta = pasta))
  b <- r1$base_modelagem
  expect_equal(b$populacao$linhas, 17)
  expect_equal(b$populacao$fora, 3)
  expect_equal(b$populacao$ausentes, 0)
  expect_equal(sum(b$resumo$linhas), 17)
  expect_true(file.exists(file.path(pasta, "base_modelagem.rds")))

  expect_message(r2 <- obter_resultados_checkin3(base, pasta = pasta), NA)
  expect_identical(r1, r2)

  # outro script (chave diferente) recalcula
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(readLines(here::here("scripts", "21_base_modelagem.R")), "# mudou"), script)
  expect_message(obter_resultados_checkin3(base, pasta = pasta, scripts = script), "Calculando")
})
