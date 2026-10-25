source(here::here("scripts", "02_trata_base.R"))
source(here::here("scripts", "10_resultados.R"))

# Calcula as secoes pedidas na fixture e devolve tambem a fixture no R.
res_secoes <- function(secoes) {
  base <- suppressMessages(obter_base_tratada(pipeline = TRUE, origem = test_path("fixtures", "cfs_mini.csv")))
  on.exit(fechar_base(base))
  res <- suppressMessages(obter_resultados(base, secoes, pasta = withr::local_tempdir(), forcar = TRUE))
  c(res, list(df = dplyr::collect(base)))
}

esperar_populacao <- function(secao) {
  for (nome in names(secao)) {
    tab <- secao[[nome]]
    if (!is.data.frame(tab)) next
    expect_true("populacao" %in% names(tab), info = nome)
    expect_true(all(tab$populacao %in% POPULACOES$id), info = nome)
  }
}

test_that("secao 2: dimensao, colunas e populacoes (T-S2)", {
  r <- res_secoes(2)
  s2 <- r$secao2
  df_secoes <- r$df
  esperar_populacao(s2)
  valor <- setNames(s2$dimensao$valor, s2$dimensao$medida)
  expect_equal(valor[["Linhas na base tratada"]], 20)
  expect_equal(valor[["Linhas removidas"]], 0)
  expect_equal(valor[["Colunas na base tratada"]], 29)
  expect_equal(unname(valor[c("Colunas originais", "Colunas derivadas", "Flags de qualidade")]), c(18, 2, 9))
  expect_identical(s2$colunas$coluna, COLUNAS_TRATADAS)
  expect_equal(s2$populacoes$linhas, c(20, 18, 18, 18))
  expect_equal(s2$populacoes$remessas[1], sum(df_secoes$WGT_FACTOR))
  expect_equal(s2$populacoes$pct_linhas[1], 100)
})

test_that("secao 3: numericas, frequencias, raras e areas (T-S3)", {
  r <- res_secoes(3)
  s3 <- r$secao3
  df_secoes <- r$df
  esperar_populacao(s3)

  med <- dplyr::filter(s3$numericas, coluna == "dist_km", medida == "mediana")
  expect_equal(med$sem_fator, median(df_secoes$dist_km))
  expect_setequal(unique(s3$numericas$unidade), c("lb", "t", "US$", "mi", "km"))

  modal <- dplyr::filter(s3$frequencias, variavel == "MODE")
  expect_equal(sum(modal$linhas), 20)
  expect_equal(modal$rotulo[modal$codigo == "21"], "Parcel, U.S.P.S. or courier")

  # 10 maiores + demais somam a variavel inteira
  maiores <- dplyr::filter(s3$maiores, variavel == "SCTG")
  expect_lte(nrow(maiores), 11)
  expect_equal(sum(maiores$pct_linhas), 100)
  expect_true(any(startsWith(maiores$rotulo, "Demais")))

  expect_equal(s3$raras$motivo[s3$raras$codigo == "00"], "Mercadoria suprimida")
  expect_equal(s3$raras$motivo[s3$raras$codigo == "01-05"], "SCTG colapsado em faixa")
  expect_equal(s3$raras$linhas[s3$raras$variavel == "MODE"], sum(df_secoes$MODE == "30"))

  orig <- dplyr::filter(s3$areas, variavel == "ORIG_CFS_AREA")
  expect_equal(orig$categorias, dplyr::n_distinct(df_secoes$ORIG_CFS_AREA))
  expect_equal(orig$suprimidas_linhas, sum(endsWith(df_secoes$ORIG_CFS_AREA, "-00000")))
})

test_that("secao 4: distribuicoes e graficos (T-S4)", {
  source(here::here("scripts", "05_secao4_distribuicoes.R"), local = TRUE)
  r <- res_secoes(4)
  s4 <- r$secao4
  df_secoes <- r$df
  esperar_populacao(s4)

  expect_equal(sum(s4$peso$linhas), 20)
  expect_equal(sum(s4$valor$remessas), sum(df_secoes$WGT_FACTOR))
  expect_equal(s4$distancia$sem_fator[s4$distancia$p == 0.5], median(df_secoes$dist_km))
  expect_equal(tail(s4$distancia_faixas$pct_linhas_acum, 1), 100)
  expect_equal(s4$distancia_faixas$linhas[s4$distancia_faixas$faixa == "Até 100 km"],
               sum(df_secoes$dist_km <= 100))
  expect_equal(sum(s4$modal$pct_remessas), 100)
  expect_equal(sum(s4$mercadoria$linhas[s4$mercadoria$SCTG == GRUPO_COLAPSADO]),
               sum(df_secoes$SCTG == "00" | grepl("-", df_secoes$SCTG)))
  expect_equal(unique(s4$origem$populacao), "origem_area")
  expect_equal(unique(s4$destino$populacao), "geografica")
  expect_lte(nrow(s4$origem), N_AREAS)

  g <- graficos_secao4(s4)
  expect_length(g, 8)
  expect_true(all(vapply(g, ggplot2::is_ggplot, logical(1))))
  # monta cada grafico de verdade, sem abrir dispositivo
  for (nome in names(g)) expect_no_error(ggplot2::ggplot_build(g[[nome]]), message = nome)
})

test_that("secao 5: relacoes e graficos (T-S5)", {
  source(here::here("scripts", "06_secao5_relacoes.R"), local = TRUE)
  r <- res_secoes(5)
  s5 <- r$secao5
  df_secoes <- r$df
  esperar_populacao(s5)

  expect_equal(sum(s5$peso_distancia$linhas), 20)
  expect_equal(sum(s5$peso_distancia$pct_remessas), 100)
  expect_equal(s5$correlacao$pearson_sem_fator,
               cor(log10(df_secoes$SHIPMT_WGHT), log10(df_secoes$dist_km)))

  med <- dplyr::filter(s5$peso_modal, p == 0.5, familia_modal == "Caminhão")
  caminhao <- df_secoes$MODE %in% FAMILIAS_MODAL[["Caminhão"]]
  expect_equal(med$sem_fator, median(df_secoes$SHIPMT_WGHT[caminhao]))

  # dentro de cada mercadoria as familias somam 100%, nas tres medidas
  somas <- dplyr::summarise(dplyr::group_by(s5$modal_sctg, SCTG),
                            dplyr::across(c(pct_linhas, pct_remessas, pct_toneladas), sum))
  expect_true(all(abs(as.matrix(somas[, -1]) - 100) < 1e-9))
  expect_false(any(s5$modal_sctg$SCTG %in% c("00", "01-05")))

  geo <- df_secoes[!df_secoes$fl_exportacao & !df_secoes$fl_origem_suprimida, ]
  mesmo <- geo$ORIG_STATE == geo$DEST_STATE
  total <- dplyr::filter(s5$intraestadual, ORIG_STATE == "Total")
  expect_equal(total$linhas_total, nrow(geo))
  expect_equal(total$pct_toneladas, 100 * sum((geo$peso_t * geo$WGT_FACTOR)[mesmo]) /
                 sum(geo$peso_t * geo$WGT_FACTOR))
  expect_equal(nrow(s5$maior_origem), dplyr::n_distinct(geo$DEST_STATE))

  g <- graficos_secao5(s5)
  expect_length(g, 5)
  for (nome in names(g)) expect_no_error(ggplot2::ggplot_build(g[[nome]]), message = nome)
})
