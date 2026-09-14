# Conexao com banco de dados externo.
#
# Regras:
#   - credenciais SO vem de variaveis de ambiente (.Renviron), nunca hardcoded;
#   - o bloco ativo do config.yml e escolhido por R_CONFIG_ACTIVE;
#   - toda conexao aberta deve ser fechada (use `with_bd()` quando possivel).

#' Abre conexao DBI conforme o driver do ambiente ativo.
#' @return objeto DBIConnection
conectar_bd <- function(banco = cfg("banco")) {
  driver <- banco$driver

  con <- switch(
    driver,
    duckdb = {
      arquivo <- here::here(banco$arquivo)
      dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
      # shared_home = TRUE guarda extensoes em ~/.duckdb entre sessoes;
      # argumento nao existe em versoes antigas do pacote, dai o fallback.
      drv <- tryCatch(
        duckdb::duckdb(shared_home = TRUE),
        error = function(e) duckdb::duckdb()
      )
      DBI::dbConnect(drv, dbdir = arquivo, read_only = FALSE)
    },
    postgres = {
      obrigatorios <- c("host", "dbname", "user", "password")
      faltando <- obrigatorios[vapply(banco[obrigatorios], function(x) !nzchar(x %||% ""), logical(1))]
      if (length(faltando)) {
        stop("Faltam credenciais no .Renviron: ", paste(faltando, collapse = ", "), call. = FALSE)
      }
      DBI::dbConnect(
        RPostgres::Postgres(),
        host     = banco$host,
        port     = banco$port,
        dbname   = banco$dbname,
        user     = banco$user,
        password = banco$password
      )
    },
    stop("Driver nao suportado: ", driver, call. = FALSE)
  )

  message("Conectado ao banco (", driver, ").")
  con
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fecha a conexao com seguranca.
desconectar_bd <- function(con) {
  if (is.null(con) || !DBI::dbIsValid(con)) return(invisible(NULL))
  if (inherits(con, "duckdb_connection")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
  invisible(NULL)
}

#' Executa um bloco com a conexao aberta e garante o fechamento.
#' with_bd(function(con) DBI::dbListTables(con))
with_bd <- function(f, ...) {
  con <- conectar_bd(...)
  on.exit(desconectar_bd(con), add = TRUE)
  f(con)
}

#' Identificador qualificado por schema (evita SQL colado a mao).
tabela_id <- function(tabela, schema = cfg("banco", "schema")) {
  if (is.null(schema) || !nzchar(schema)) DBI::Id(table = tabela)
  else DBI::Id(schema = schema, table = tabela)
}

#' Consulta parametrizada. NUNCA use paste() para montar SQL com input externo.
#' consultar(con, "SELECT * FROM cfs.amostra WHERE SECTOR = $1", list(42L))
consultar <- function(con, sql, params = NULL) {
  DBI::dbGetQuery(con, sql, params = params)
}

#' Grava um data.frame em tabela do banco (sobrescreve por padrao).
gravar_tabela <- function(con, dados, tabela, sobrescrever = TRUE) {
  DBI::dbWriteTable(con, tabela_id(tabela), as.data.frame(dados), overwrite = sobrescrever)
  message("Tabela gravada: ", tabela, " (", nrow(dados), " linhas)")
  invisible(tabela)
}

#' Referencia lazy para usar dplyr direto no banco (sem trazer tudo para a RAM).
#' tbl_bd(con, "amostra") |> filter(SECTOR == 42) |> collect()
tbl_bd <- function(con, tabela, schema = cfg("banco", "schema")) {
  sem_schema <- is.null(schema) || !nzchar(schema) || identical(schema, "main")
  if (sem_schema) dplyr::tbl(con, tabela)
  else dplyr::tbl(con, dbplyr::in_schema(schema, tabela))
}
