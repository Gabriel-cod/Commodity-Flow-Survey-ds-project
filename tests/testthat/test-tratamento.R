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

test_that("limite de memoria do DuckDB e convertido para GiB", {
  expect_equal(.em_gib("24.8 GiB"), 24.8)
  expect_equal(.em_gib("512 MiB"), 0.5)
  expect_equal(.em_gib("2 TiB"), 2048)
  expect_equal(.em_gib("8 GB"), 8e9 / 1024^3)
  expect_error(.em_gib("muito"), "formato desconhecido")
})

test_that("uma passada com memoria suficiente; senao limite de metade da RAM", {
  expect_null(.limite_memoria(24.8))
  expect_null(.limite_memoria(MEMORIA_UM_PASSO_GIB))
  # 12,8 GiB de padrao = 16 GiB de RAM -> limite de 8 GiB
  expect_equal(.limite_memoria(12.8), 8)
  expect_equal(.limite_memoria(6.2), 3.8)
})

test_that("modo por partes gera a mesma base que uma passada", {
  arquivo <- withr::local_tempfile(fileext = ".duckdb")
  con <- .abrir(arquivo)
  withr::defer(desconectar_bd(con))
  DBI::dbExecute(con, "SET memory_limit = '2GiB'")

  expect_message(.construir(con, fixture()), "excedente em disco")
  por_partes <- dplyr::arrange(dplyr::collect(dplyr::tbl(con, "cfs_tratada")), SHIPMT_ID)
  expect_equal(por_partes, coletar(origem = fixture()))
})

# Base gravada (pipeline = FALSE) -------------------------------------------

gravar <- function(...) suppressMessages(obter_base_tratada(pipeline = FALSE, ...))
metadados <- function(base) DBI::dbGetQuery(dbplyr::remote_con(base), "SELECT * FROM metadados")
novo_destino <- function(env = parent.frame()) {
  file.path(withr::local_tempdir(.local_envir = env), "base.duckdb")
}

# Copia das regras com TETO_VALOR = 1: muda o hash e marca fl_valor_teto em tudo.
regras_editadas <- function(env = parent.frame()) {
  copia <- withr::local_tempfile(fileext = ".R", .local_envir = env)
  writeLines(sub("^TETO_VALOR\\s*<-.*$", "TETO_VALOR <- 1", readLines(ARQUIVO_REGRAS)), copia)
  copia
}

test_that("FALSE grava o destino com metadados (T-F3)", {
  destino <- novo_destino()
  base <- gravar(origem = fixture(), destino = destino)
  withr::defer(fechar_base(base))
  meta <- metadados(base)

  expect_true(file.exists(destino))
  expect_identical(meta$hash_regras, hash_regras())
  expect_equal(meta$n_linhas, 20)
  expect_identical(meta$arquivo_origem, "tests/testthat/fixtures/cfs_mini.csv")
})

test_that("base atual nao e reprocessada (T-F4)", {
  destino <- novo_destino()
  b1 <- gravar(origem = fixture(), destino = destino)
  antes <- metadados(b1)$processado_em
  fechar_base(b1)

  b2 <- gravar(origem = fixture(), destino = destino)
  withr::defer(fechar_base(b2))
  expect_equal(metadados(b2)$processado_em, antes)
})

test_that("regras editadas reprocessam e valem na base (T-F5)", {
  destino <- novo_destino()
  b1 <- gravar(origem = fixture(), destino = destino)
  antes <- metadados(b1)
  fechar_base(b1)

  copia <- regras_editadas()
  b2 <- gravar(origem = fixture(), destino = destino, arquivo_regras = copia)
  withr::defer(fechar_base(b2))
  depois <- metadados(b2)

  expect_identical(depois$hash_regras, hash_regras(copia))
  expect_true(depois$processado_em > antes$processado_em)
  expect_true(all(dplyr::collect(b2)$fl_valor_teto))
})

test_that("falha durante a construcao deixa o destino intacto (T-F6)", {
  destino <- novo_destino()
  fechar_base(gravar(origem = fixture(), destino = destino))
  md5 <- tools::md5sum(destino)

  expect_error(gravar(origem = fixture("cfs_mini_valor_invalido.csv"), destino = destino,
                      arquivo_regras = regras_editadas()))
  expect_identical(tools::md5sum(destino), md5)
  expect_length(list.files(dirname(destino), pattern = "\\.tmp-|\\.wal$"), 0)
})

test_that("destino na pasta da origem ou em data/raw gera erro (T-F7)", {
  pasta <- withr::local_tempdir()
  origem <- file.path(pasta, "cfs.csv")
  file.copy(fixture(), origem)
  destino <- file.path(pasta, "sub", "base.duckdb")
  expect_error(gravar(origem = origem, destino = destino), "pasta dos dados brutos")
  expect_false(file.exists(destino))

  destino_raw <- here::here("data", "raw", "nao_deve_existir.duckdb")
  expect_error(gravar(origem = fixture(), destino = destino_raw), "pasta dos dados brutos")
  expect_length(list.files(here::here("data", "raw"), pattern = "nao_deve_existir"), 0)
})

test_that("base aberta na sessao nao impede ver as regras novas (T-F10)", {
  destino <- novo_destino()
  b1 <- gravar(origem = fixture(), destino = destino)

  copia <- regras_editadas()
  b2 <- gravar(origem = fixture(), destino = destino, arquivo_regras = copia)
  withr::defer(fechar_base(b2))

  expect_identical(metadados(b2)$hash_regras, hash_regras(copia))
  expect_true(all(dplyr::collect(b2)$fl_valor_teto))
  expect_error(dplyr::collect(b1))
})

test_that("outra origem com o mesmo destino reprocessa (T-F11)", {
  destino <- novo_destino()
  fechar_base(gravar(origem = fixture(), destino = destino))

  b2 <- gravar(origem = fixture("cfs_mini_b.csv"), destino = destino)
  withr::defer(fechar_base(b2))
  expect_equal(metadados(b2)$n_linhas, 19)
})

test_that("fechar_base nao apaga a base gravada (T-F14)", {
  destino <- novo_destino()
  fechar_base(gravar(origem = fixture(), destino = destino))
  expect_true(file.exists(destino))
})

test_that("registro sobrevive a um novo source() do arquivo (T-F15)", {
  destino <- novo_destino()
  copia <- withr::local_tempfile(fileext = ".R")
  file.copy(ARQUIVO_REGRAS, copia)
  gravar(origem = fixture(), destino = destino, arquivo_regras = copia)

  writeLines(sub("^TETO_VALOR\\s*<-.*$", "TETO_VALOR <- 1", readLines(copia)), copia)
  source(here::here("scripts", "02_trata_base.R"))
  b2 <- gravar(origem = fixture(), destino = destino, arquivo_regras = copia)
  withr::defer(fechar_base(b2))
  expect_true(all(dplyr::collect(b2)$fl_valor_teto))
})

test_that("o CSV de origem nunca e alterado (T-F8)", {
  expect_identical(tools::md5sum(fixture()), md5_fixture)
  expect_identical(file.mtime(fixture()), mtime_fixture)
})
