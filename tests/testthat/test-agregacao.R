source(here::here("scripts", "02_trata_base.R"))
source(here::here("scripts", "10_resultados.R"))

abrir_fixture <- function(env = parent.frame()) {
  base <- suppressMessages(obter_base_tratada(pipeline = TRUE, origem = test_path("fixtures", "cfs_mini.csv")))
  withr::defer(fechar_base(base), envir = env)
  base
}

quantil_ponderado <- function(v, f, p) {
  o <- order(v)
  v[o][which(cumsum(f[o]) >= p * sum(f) * (1 - 1e-12))[1]]
}

test_that("quantis sem fator sao o tipo 7 do R, com e sem grupo (T-A1)", {
  base <- abrir_fixture()
  df <- dplyr::collect(base)
  probs <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)

  q <- quantis(base, "SHIPMT_VALUE", probs)
  expect_equal(q$sem_fator, unname(quantile(df$SHIPMT_VALUE, probs, type = 7)))
  expect_equal(q$populacao, rep("completa", length(probs)))

  qg <- quantis(base, "SHIPMT_WGHT", probs, grupo = "SECTOR")
  for (s in unique(df$SECTOR)) {
    esperado <- unname(quantile(df$SHIPMT_WGHT[df$SECTOR == s], probs, type = 7))
    expect_equal(qg$sem_fator[qg$SECTOR == s], esperado, info = s)
  }
})

test_that("quantis com fator: menor valor com acumulado >= p * total (T-A2)", {
  base <- abrir_fixture()
  df <- dplyr::collect(base)
  probs <- c(0.1, 0.25, 0.5, 0.75, 1)

  q <- quantis(base, "SHIPMT_DIST_GC", probs)
  esperado <- vapply(probs, function(p) quantil_ponderado(df$SHIPMT_DIST_GC, df$WGT_FACTOR, p), numeric(1))
  expect_equal(q$com_fator, esperado)
  expect_equal(q$remessas[1], sum(df$WGT_FACTOR))

  geo <- df[!df$fl_exportacao & !df$fl_origem_suprimida, ]
  qp <- quantis(base, "SHIPMT_WGHT", 0.5, populacao = "geografica")
  expect_equal(qp$linhas, nrow(geo))
  expect_equal(qp$com_fator, quantil_ponderado(geo$SHIPMT_WGHT, geo$WGT_FACTOR, 0.5))
})

test_that("resumo numerico bate com o R, sem e com fator (T-A3)", {
  base <- abrir_fixture()
  df <- dplyr::collect(base)
  v <- df$SHIPMT_VALUE
  f <- df$WGT_FACTOR
  media_f <- sum(f * v) / sum(f)

  r <- resumo_numerico(base, "SHIPMT_VALUE")
  sem <- setNames(r$sem_fator, r$medida)
  com <- setNames(r$com_fator, r$medida)
  expect_equal(sem[c("n", "ausentes", "media", "dp", "minimo", "maximo")],
               c(n = 20, ausentes = 0, media = mean(v), dp = sd(v), minimo = min(v), maximo = max(v)))
  expect_equal(sem[["mediana"]], median(v))
  expect_equal(unname(sem[c("q1", "q3")]), unname(quantile(v, c(0.25, 0.75))))
  expect_equal(com[["n"]], sum(f))
  expect_equal(com[["media"]], media_f)
  expect_equal(com[["dp"]], sqrt(sum(f * (v - media_f)^2) / sum(f)))
  expect_equal(com[["mediana"]], quantil_ponderado(v, f, 0.5))
})

test_that("histograma em log10 soma n e poe cada valor na classe certa (T-A4)", {
  base <- abrir_fixture()
  df <- dplyr::collect(base)

  h <- histograma_log(base, "SHIPMT_WGHT", largura = 0.5)
  expect_equal(sum(h$linhas), nrow(df))
  expect_equal(sum(h$remessas), sum(df$WGT_FACTOR))
  classe <- floor(log10(df$SHIPMT_WGHT) / 0.5 + 1e-9)
  esperado <- as.numeric(table(classe))
  expect_equal(h$linhas, esperado)
  expect_equal(h$inicio, 10^(sort(unique(classe)) * 0.5))
  # 10 e potencia exata e abre a classe dela
  expect_equal(h$linhas[h$inicio == 10], sum(df$SHIPMT_WGHT >= 10 & df$SHIPMT_WGHT < 10^1.5))

  hg <- histograma_log(base, "SHIPMT_WGHT", largura = 1, grupo = "SECTOR")
  expect_equal(sum(hg$linhas[hg$SECTOR == "31-33"]), sum(df$SECTOR == "31-33"))
})

test_that("frequencias somam 100% no total e dentro de cada grupo (T-A5)", {
  base <- abrir_fixture()
  df <- dplyr::collect(base)

  fr <- frequencias(base, "MODE")
  expect_equal(sum(fr$pct_linhas), 100)
  expect_equal(sum(fr$pct_remessas), 100)
  expect_equal(fr$linhas[fr$MODE == "112"], sum(df$MODE == "112"))
  expect_equal(fr$valor[fr$MODE == "14"], sum(df$SHIPMT_VALUE[df$MODE == "14"] * df$WGT_FACTOR[df$MODE == "14"]))
  expect_equal(sum(fr$toneladas), sum(df$peso_t * df$WGT_FACTOR))
  expect_true(all(diff(fr$linhas) <= 0))

  fd <- frequencias(com_familia_modal(base), c("SECTOR", "familia_modal"), dentro_de = "SECTOR")
  somas <- tapply(fd$pct_remessas, fd$SECTOR, sum)
  expect_equal(as.numeric(somas), rep(100, length(somas)))

  fm <- frequencias(base, "SCTG", populacao = "mercadoria")
  expect_false(any(fm$SCTG %in% c("00", "01-05")))
  expect_equal(unique(fm$populacao), "mercadoria")
})

test_that("familias de modal cobrem os 16 codigos do dicionario, sem repetir (T-A6)", {
  codigos <- de_para(ler_dicionario(), "MODE")$MODE
  todos <- unlist(FAMILIAS_MODAL, use.names = FALSE)
  expect_length(codigos, 16)
  expect_setequal(todos, codigos)
  expect_false(anyDuplicated(todos) > 0)
  expect_identical(names(PALETA_MODAL), names(FAMILIAS_MODAL))

  base <- abrir_fixture()
  fam <- dplyr::collect(dplyr::distinct(com_familia_modal(base), MODE, familia_modal))
  expect_equal(fam$familia_modal[fam$MODE == "133"], "Hidrovia")
  expect_equal(fam$familia_modal[fam$MODE == "30"], "Outros ou desconhecido")
  expect_false(anyNA(fam$familia_modal))
})

test_that("populacoes tiram as linhas da regra e id desconhecido e erro (T-A7)", {
  base <- abrir_fixture()
  n <- function(id) dplyr::pull(dplyr::count(filtrar_populacao(base, id)))
  expect_equal(n("completa"), 20)
  expect_equal(n("geografica"), 18)
  expect_equal(n("mercadoria"), 18)
  expect_equal(n("origem_area"), 18)
  expect_setequal(POPULACOES$id, c("completa", "geografica", "mercadoria", "origem_area"))
  expect_error(filtrar_populacao(base, "inexistente"), "População desconhecida")
})

test_that("obter_resultados grava, reaproveita e recalcula quando a chave muda (T-A8)", {
  pasta <- withr::local_tempdir()
  scripts <- file.path(pasta, paste0("s", 2:6, ".R"))
  escrever <- function(i, extra = "") {
    writeLines(sprintf("calcular_secao%d <- function(base, res) list(v = runif(1), antes = names(res)) %s",
                       i + 1, extra), scripts[i])
  }
  for (i in 1:5) escrever(i)
  obter <- function(...) suppressMessages(obter_resultados(NULL, pasta = file.path(pasta, "cache"),
                                                           scripts = scripts, ...))

  r1 <- obter(secoes = 2)
  expect_named(r1, "secao2")
  expect_true(file.exists(file.path(pasta, "cache", "secao2.rds")))
  expect_identical(obter(secoes = 2), r1)
  expect_false(identical(obter(secoes = 2, forcar = TRUE)$secao2$v, r1$secao2$v))

  r6 <- obter(secoes = 6)
  expect_named(r6, "secao6")
  expect_identical(r6$secao6$antes, paste0("secao", 2:5))
  expect_identical(obter(secoes = 6), r6)

  # mudar uma secao anterior muda a chave da dependente
  escrever(2, "# mudou")
  expect_false(identical(obter(secoes = 6)$secao6$v, r6$secao6$v))
  expect_length(list.files(file.path(pasta, "cache"), "tmp"), 0)
})
