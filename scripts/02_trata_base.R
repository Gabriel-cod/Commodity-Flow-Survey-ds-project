# 02_trata_base.R --------------------------------------------------------
# Tratamento unico da CFS 2022 PUMS: tipos, flags de qualidade e derivadas,
# sem remover linhas e sem nunca escrever no CSV original.
#
# So define objetos (nada roda ao carregar). Uso, depois do 00_setup.R:
#   source(here::here("scripts", "02_trata_base.R"))
#   base <- obter_base_tratada()      # tabela lazy do DuckDB
#   fechar_base(base)

# Ordem e tipos do cabecalho do CSV. Codigos sao texto: preservam o zero a
# esquerda ("01", "00000", "07") e nunca passam por inferencia de tipo.
TIPOS_CFS <- c(
  SHIPMT_ID      = "VARCHAR",
  ORIG_STATE     = "VARCHAR",
  ORIG_MA        = "VARCHAR",
  ORIG_CFS_AREA  = "VARCHAR",
  DEST_STATE     = "VARCHAR",
  DEST_MA        = "VARCHAR",
  DEST_CFS_AREA  = "VARCHAR",
  SECTOR         = "VARCHAR",
  SCTG           = "VARCHAR",
  MODE           = "VARCHAR",
  SHIPMT_VALUE   = "BIGINT",
  SHIPMT_WGHT    = "BIGINT",
  SHIPMT_DIST_GC = "BIGINT",
  TEMP_CNTL_YN   = "VARCHAR",
  EXPORT_YN      = "VARCHAR",
  EXPORT_CNTRY   = "VARCHAR",
  HAZMAT         = "VARCHAR",
  WGT_FACTOR     = "DOUBLE"
)

# Larguras validas medidas na base completa. A metropole (MA) tem 3 digitos
# nas areas tipo CSA e 5 nas tipo MSA: nunca completar com zeros.
LARGURAS_CODIGO <- list(
  SHIPMT_ID     = 8,
  ORIG_STATE    = 2,
  DEST_STATE    = 2,
  ORIG_MA       = c(3, 5),
  DEST_MA       = c(3, 5),
  ORIG_CFS_AREA = c(6, 8),
  DEST_CFS_AREA = c(6, 8),
  SECTOR        = c(2, 5),
  SCTG          = c(2, 5),
  MODE          = c(2, 3),
  TEMP_CNTL_YN  = 1,
  EXPORT_YN     = 1,
  EXPORT_CNTRY  = 1,
  HAZMAT        = 1
)

TETO_VALOR      <- 30000000
TETO_FATOR      <- 3588000
PERCENTIS_RAZAO <- c(0.001, 0.999)
LIBRA_EM_T      <- 0.00045359237
MILHA_EM_KM     <- 1.609344

FLAGS <- c(
  "fl_sctg_colapsado", "fl_origem_ma_suprimida", "fl_origem_suprimida", "fl_exportacao",
  "fl_intra_area", "fl_valor_teto", "fl_fator_teto", "fl_fator_menor_1", "fl_razao_atipica"
)
COLUNAS_TRATADAS <- c(names(TIPOS_CFS), "peso_t", "dist_km", FLAGS)

ARQUIVO_REGRAS <- here::here("scripts", "02_trata_base.R")

hash_regras <- function(arquivo_regras = ARQUIVO_REGRAS) {
  if (!file.exists(arquivo_regras)) {
    stop("Arquivo de regras não encontrado: ", .relativo(arquivo_regras), call. = FALSE)
  }
  unname(tools::md5sum(arquivo_regras))
}

obter_base_tratada <- function(pipeline = TRUE,
                               origem = caminho("raw"),
                               destino = caminho("base_tratada"),
                               arquivo_regras = ARQUIVO_REGRAS) {
  if (!is.logical(pipeline) || length(pipeline) != 1 || is.na(pipeline)) {
    stop("pipeline deve ser TRUE ou FALSE", call. = FALSE)
  }
  if (pipeline) .obter_temporaria(origem, arquivo_regras)
  else .obter_gravada(origem, destino, arquivo_regras)
}

fechar_base <- function(base) {
  con <- if (inherits(base, "tbl_lazy")) dbplyr::remote_con(base) else base
  id <- .achar_registro(con)
  entrada <- if (!is.null(id)) get(id, envir = .registro())

  desconectar_bd(con)
  if (!is.null(id)) rm(list = id, envir = .registro())
  if (identical(entrada$tipo, "temporario")) {
    unlink(c(entrada$arquivo, paste0(entrada$arquivo, ".wal")))
  }
  invisible(NULL)
}

# Internas ----------------------------------------------------------------

.obter_temporaria <- function(origem, arquivo_regras) {
  .exigir_origem(origem)
  regras <- .carregar_regras(arquivo_regras)
  regras$.conferir_cabecalho(origem)

  arquivo <- tempfile("cfs_tratada_", fileext = ".duckdb")
  con <- .abrir(arquivo)
  .registrar(con, arquivo, "temporario")
  n <- tryCatch(regras$.construir(con, origem), error = function(e) {
    fechar_base(con)
    stop(e)
  })

  message("Base tratada | origem: processamento (temporário) | hash: ", regras$hash,
          " | linhas: ", n)
  dplyr::tbl(con, "cfs_tratada")
}

.obter_gravada <- function(origem, destino, arquivo_regras) {
  hash <- hash_regras(arquivo_regras)

  if (file.exists(destino)) {
    con <- .abrir(destino, read_only = TRUE)
    .registrar(con, destino, "gravada")
    meta <- .ler_metadados(con)
    if (.base_atual(con, meta, hash, origem)) {
      message("Base tratada | origem: gravada (atual) | hash: ", hash,
              " | processada em: ", format(meta$processado_em), " | linhas: ", meta$n_linhas)
      return(dplyr::tbl(con, "cfs_tratada"))
    }
    fechar_base(con)
  }
  message("Base gravada ausente ou desatualizada: reprocessando")

  regras <- .gravar(origem, destino, arquivo_regras)

  con <- .abrir(destino, read_only = TRUE)
  .registrar(con, destino, "gravada")
  meta <- .ler_metadados(con)
  # No mesmo processo, uma conexao aberta fora do registro faz o duckdb
  # reaproveitar a instancia antiga e reler a versao anterior do arquivo.
  if (is.null(meta) || !identical(meta$hash_regras, regras$hash)) {
    fechar_base(con)
    stop("A base reaberta tem hash diferente do atual; feche as bases abertas com ",
         "fechar_base() ou reinicie o R", call. = FALSE)
  }
  message("Base tratada | origem: gravada (reprocessada) | hash: ", regras$hash,
          " | linhas: ", meta$n_linhas)
  dplyr::tbl(con, "cfs_tratada")
}

.base_atual <- function(con, meta, hash, origem) {
  if (is.null(meta) || !identical(meta$hash_regras, hash)) return(FALSE)
  n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM cfs_tratada")$n
  if (meta$n_linhas != n) return(FALSE)

  if (!file.exists(origem)) {
    warning("origem ausente: não foi possível conferir a origem da base gravada", call. = FALSE)
    return(TRUE)
  }
  identical(meta$arquivo_origem, .relativo(origem)) &&
    meta$origem_tamanho == file.size(origem) &&
    meta$origem_modificado_em == .mtime_s(origem)
}

.ler_metadados <- function(con) {
  if (!"metadados" %in% DBI::dbListTables(con)) return(NULL)
  DBI::dbGetQuery(con, "SELECT * FROM metadados")
}

# Grava num temporario ao lado do destino e so troca por rename depois do
# CHECKPOINT: o destino fica completo ou intacto, nunca pela metade.
.gravar <- function(origem, destino, arquivo_regras) {
  .exigir_origem(origem)
  .conferir_destino(destino, origem)

  abertas <- .conexoes_do_arquivo(destino)
  if (length(abertas)) {
    message("Fechando ", length(abertas),
            " conexão(ões) abertas da base gravada antes de reprocessar")
    for (con in abertas) fechar_base(con)
  }

  regras <- .carregar_regras(arquivo_regras)
  regras$.conferir_cabecalho(origem)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(destino, ".tmp-", Sys.getpid())
  unlink(c(tmp, paste0(tmp, ".wal")))
  con <- .abrir(tmp)
  concluido <- FALSE
  on.exit(if (!concluido) {
    desconectar_bd(con)
    unlink(c(tmp, paste0(tmp, ".wal")))
  })

  n <- regras$.construir(con, origem)
  DBI::dbExecute(con, "
    CREATE TABLE metadados (
      hash_regras VARCHAR, processado_em TIMESTAMP, n_linhas BIGINT,
      arquivo_origem VARCHAR, origem_tamanho BIGINT, origem_modificado_em BIGINT
    )")
  DBI::dbExecute(con, "INSERT INTO metadados VALUES (?, now()::TIMESTAMP, ?, ?, ?, ?)",
                 params = list(regras$hash, n, .relativo(origem),
                               file.size(origem), .mtime_s(origem)))
  DBI::dbExecute(con, "CHECKPOINT")
  desconectar_bd(con)

  if (!file.rename(tmp, destino)) {
    stop("Não foi possível substituir ", .relativo(destino), call. = FALSE)
  }
  concluido <- TRUE
  regras
}

.conferir_destino <- function(destino, origem) {
  proibidas <- c(.abs(dirname(origem)), .abs(here::here("data", "raw")))
  if (any(startsWith(.abs(destino), paste0(proibidas, "/")))) {
    stop("O destino não pode ficar na pasta dos dados brutos", call. = FALSE)
  }
}

# Segundos inteiros: a data com fracao de segundo nao sobrevive a ida e volta
# por um TIMESTAMP do DuckDB, e a comparacao daria sempre falso.
.mtime_s <- function(arquivo) floor(as.numeric(file.mtime(arquivo)))

# Carrega as regras do proprio arquivo cujo hash vai ser usado: editar o
# arquivo sem um novo source() nao pode gerar base com regra velha e hash novo.
# O pai e o ambiente onde este arquivo foi carregado, nao o frame da chamada,
# para as regras nao enxergarem variaveis locais (origem, destino, con).
.carregar_regras <- function(arquivo_regras) {
  hash <- hash_regras(arquivo_regras)
  regras <- new.env(parent = environment(obter_base_tratada))
  sys.source(arquivo_regras, envir = regras)
  if (!identical(hash_regras(arquivo_regras), hash)) {
    stop("O arquivo de regras mudou durante o processamento; rode de novo.", call. = FALSE)
  }
  regras$hash <- hash
  regras
}

.construir <- function(con, origem) {
  colunas <- paste(sprintf("'%s': '%s'", names(TIPOS_CFS), TIPOS_CFS), collapse = ", ")
  leitura <- sprintf("read_csv(%s, header = true, columns = {%s})",
                     DBI::dbQuoteString(con, normalizePath(origem)), colunas)

  DBI::dbExecute(con, sprintf("
    CREATE TABLE cfs_tratada AS
    WITH bruto AS (
      SELECT *, SHIPMT_VALUE::DOUBLE / NULLIF(SHIPMT_WGHT, 0) AS razao FROM %s
    ),
    -- percentis da razao valor/peso dentro de cada SCTG, sem ponderacao
    limites AS (
      SELECT SCTG,
             quantile_cont(razao, %s) AS razao_min,
             quantile_cont(razao, %s) AS razao_max
      FROM bruto GROUP BY SCTG
    )
    SELECT %s,
      SHIPMT_WGHT * %s AS peso_t,
      SHIPMT_DIST_GC * %s AS dist_km,
      coalesce(SCTG = '00' OR contains(SCTG, '-'), false) AS fl_sctg_colapsado,
      coalesce(ORIG_MA = '00000' AND ORIG_STATE <> '00', false) AS fl_origem_ma_suprimida,
      coalesce(ORIG_STATE = '00', false) AS fl_origem_suprimida,
      coalesce(EXPORT_YN = 'Y', false) AS fl_exportacao,
      coalesce(ORIG_CFS_AREA = DEST_CFS_AREA, false) AS fl_intra_area,
      coalesce(SHIPMT_VALUE >= %s, false) AS fl_valor_teto,
      coalesce(WGT_FACTOR >= %s, false) AS fl_fator_teto,
      coalesce(WGT_FACTOR < 1, false) AS fl_fator_menor_1,
      coalesce(razao < razao_min OR razao > razao_max, false) AS fl_razao_atipica
    FROM bruto JOIN limites USING (SCTG)",
    leitura, .num(PERCENTIS_RAZAO[1]), .num(PERCENTIS_RAZAO[2]),
    paste(names(TIPOS_CFS), collapse = ", "), .num(LIBRA_EM_T), .num(MILHA_EM_KM),
    .num(TETO_VALOR), .num(TETO_FATOR)
  ))

  colunas_lidas <- DBI::dbListFields(con, "cfs_tratada")
  if (!identical(colunas_lidas, COLUNAS_TRATADAS)) {
    stop("Colunas inesperadas na base tratada.",
         "\n  lidas:     ", paste(colunas_lidas, collapse = ","),
         "\n  esperadas: ", paste(COLUNAS_TRATADAS, collapse = ","), call. = FALSE)
  }

  n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM cfs_tratada")$n
  n_csv <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", leitura))$n
  if (n != n_csv) {
    stop("A base tratada tem ", n, " linhas, mas o CSV tem ", n_csv, call. = FALSE)
  }
  .conferir_larguras(con)
  n
}

# read_csv com columns= nomeia as colunas pela posicao, sem erro: um cabecalho
# fora de ordem trocaria SHIPMT_VALUE e SHIPMT_WGHT em silencio.
.conferir_cabecalho <- function(origem) {
  lido <- strsplit(readLines(origem, n = 1, warn = FALSE), ",", fixed = TRUE)[[1]]
  if (!identical(lido, names(TIPOS_CFS))) {
    stop("Cabeçalho inesperado em ", .relativo(origem),
         "\n  lido:     ", paste(lido, collapse = ","),
         "\n  esperado: ", paste(names(TIPOS_CFS), collapse = ","), call. = FALSE)
  }
}

.conferir_larguras <- function(con) {
  fora <- vapply(names(LARGURAS_CODIGO), function(col) {
    sprintf("sum((length(%s) NOT IN (%s))::INT) AS %s",
            col, paste(LARGURAS_CODIGO[[col]], collapse = ", "), col)
  }, character(1))
  contagem <- unlist(DBI::dbGetQuery(con, paste(
    "SELECT", paste(fora, collapse = ", "), "FROM cfs_tratada"
  )))
  contagem <- contagem[contagem > 0]
  if (length(contagem)) {
    stop("Códigos com largura inválida: ",
         paste(names(contagem), contagem, sep = " = ", collapse = "; "), call. = FALSE)
  }
}

.num <- function(x) format(x, scientific = FALSE, digits = 15)

.exigir_origem <- function(origem) {
  if (!file.exists(origem)) {
    stop("Arquivo de origem não encontrado: ", .relativo(origem), call. = FALSE)
  }
}

.abrir <- function(arquivo, read_only = FALSE) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = arquivo, read_only = read_only)
}

# Caminho absoluto mesmo para arquivo que ainda nao existe (normalizePath so
# resolve o que existe).
.abs <- function(caminho) {
  if (file.exists(caminho)) return(normalizePath(caminho))
  file.path(.abs(dirname(caminho)), basename(caminho))
}

# Caminho relativo a raiz do projeto, ou so o nome do arquivo se estiver fora.
.relativo <- function(caminho) {
  raiz <- paste0(normalizePath(here::here()), "/")
  abs <- .abs(caminho)
  if (startsWith(abs, raiz)) substring(abs, nchar(raiz) + 1) else basename(abs)
}

# Registro das conexoes abertas por obter_base_tratada(). Fica numa opcao da
# sessao, e nao numa variavel deste arquivo, para sobreviver a um novo source().
.registro <- function() {
  registro <- getOption("cfs.registro_conexoes")
  if (is.null(registro)) {
    registro <- new.env()
    options(cfs.registro_conexoes = registro)
  }
  registro
}

.registrar <- function(con, arquivo, tipo) {
  assign(basename(tempfile("con")),
         list(con = con, arquivo = .abs(arquivo), tipo = tipo),
         envir = .registro())
}

.conexoes_do_arquivo <- function(arquivo) {
  registro <- .registro()
  entradas <- mget(ls(registro, all.names = TRUE), envir = registro)
  Filter(Negate(is.null), lapply(entradas, function(e) {
    if (e$tipo == "gravada" && e$arquivo == .abs(arquivo)) e$con
  }))
}

.achar_registro <- function(con) {
  registro <- .registro()
  for (id in ls(registro, all.names = TRUE)) {
    if (identical(get(id, envir = registro)$con, con)) return(id)
  }
  NULL
}
