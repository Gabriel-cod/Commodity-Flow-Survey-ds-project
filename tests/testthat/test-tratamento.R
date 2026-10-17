source(here::here("scripts", "02_trata_base.R"))

fixture <- function(nome = "cfs_mini.csv") test_path("fixtures", nome)
md5_fixture <- tools::md5sum(fixture())
mtime_fixture <- file.mtime(fixture())

# Base tratada da fixture, ja trazida para o R e fechada.
coletar <- function(...) {
  base <- suppressMessages(obter_base_tratada(...))
  on.exit(fechar_base(base))
  dplyr::arrange(dplyr::collect(base), SHIPMT_ID)
}

test_that("codigos voltam como texto, identicos ao CSV (T-T1)", {
  df <- coletar(origem = fixture())
  bruto <- read.csv(fixture(), colClasses = "character")

  for (col in names(LARGURAS_CODIGO)) {
    expect_type(df[[col]], "character")
    expect_identical(df[[col]], bruto[[col]], info = col)
  }
  expect_identical(df$ORIG_MA[2:4], c("220", "00000", "00000"))
})

test_that("fator sem casa decimal e lido como numero (T-T2)", {
  df <- coletar(origem = fixture())
  expect_equal(df$WGT_FACTOR[df$SHIPMT_ID == "00000010"], 1000537)
})

test_that("base tem o mesmo numero de linhas do CSV (T-T3)", {
  expect_equal(nrow(coletar(origem = fixture())), 20)
})

test_that("base tem exatamente as colunas tratadas, na ordem (T-T4)", {
  base <- suppressMessages(obter_base_tratada(origem = fixture()))
  on.exit(fechar_base(base))
  expect_identical(colnames(base), COLUNAS_TRATADAS)
})

test_that("cada flag marca exatamente as linhas projetadas na fixture (T-T5)", {
  df <- coletar(origem = fixture())
  ids_verdadeiros <- function(flag) as.integer(df$SHIPMT_ID[df[[flag]]])

  expect_identical(ids_verdadeiros("fl_sctg_colapsado"), c(3L, 4L))
  expect_identical(ids_verdadeiros("fl_origem_ma_suprimida"), 3L)
  expect_identical(ids_verdadeiros("fl_origem_suprimida"), 4L)
  expect_identical(ids_verdadeiros("fl_exportacao"), 5L)
  expect_identical(ids_verdadeiros("fl_intra_area"), c(2L, 5L, 9L))
  expect_identical(ids_verdadeiros("fl_valor_teto"), c(6L, 7L))
  expect_identical(ids_verdadeiros("fl_fator_teto"), 8L)
  expect_identical(ids_verdadeiros("fl_fator_menor_1"), 9L)
  # grupo SCTG 35 tem razoes 1..10: so as pontas passam dos percentis 0,1/99,9
  expect_identical(ids_verdadeiros("fl_razao_atipica"), c(11L, 20L))
})

test_that("flags sao logicas e nunca nulas (T-T6)", {
  df <- coletar(origem = fixture())
  for (flag in FLAGS) {
    expect_type(df[[flag]], "logical")
    expect_false(anyNA(df[[flag]]), info = flag)
  }
})

test_that("derivadas convertem para toneladas metricas e km (T-T7)", {
  df <- coletar(origem = fixture())
  expect_equal(df$peso_t, df$SHIPMT_WGHT * LIBRA_EM_T)
  expect_equal(df$dist_km, df$SHIPMT_DIST_GC * MILHA_EM_KM)
  expect_equal(df$peso_t[1], 100 * 0.45359237 / 1000)
})

test_that("pipeline = TRUE nao grava o destino (T-F1)", {
  destino <- file.path(withr::local_tempdir(), "base.duckdb")
  coletar(origem = fixture(), destino = destino)
  expect_false(file.exists(destino))
})

test_that("fechar_base apaga o arquivo temporario (T-F2)", {
  base <- suppressMessages(obter_base_tratada(origem = fixture()))
  arquivo <- dbplyr::remote_con(base)@driver@dbdir
  expect_true(file.exists(arquivo))
  fechar_base(base)
  expect_false(file.exists(arquivo))
})

test_that("pipeline invalido gera erro (T-F9)", {
  expect_error(obter_base_tratada(pipeline = NA), "pipeline deve ser TRUE ou FALSE")
  expect_error(obter_base_tratada(pipeline = "sim"), "pipeline deve ser TRUE ou FALSE")
})

test_that("cabecalho fora de ordem gera erro sem criar arquivo (T-F12)", {
  antes <- list.files(tempdir())
  expect_error(obter_base_tratada(origem = fixture("cfs_mini_cabecalho_trocado.csv")),
               "Cabeçalho inesperado")
  expect_setequal(list.files(tempdir()), antes)
})

test_that("codigo com largura invalida gera erro citando a coluna (T-F13)", {
  expect_error(
    suppressMessages(obter_base_tratada(origem = fixture("cfs_mini_estado_curto.csv"))),
    "ORIG_STATE = 1"
  )
  expect_length(ls(.registro()), 0)
})

test_that("o CSV de origem nunca e alterado (T-F8)", {
  expect_identical(tools::md5sum(fixture()), md5_fixture)
  expect_identical(file.mtime(fixture()), mtime_fixture)
})
