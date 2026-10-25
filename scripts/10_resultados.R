# 10_resultados.R ---------------------------------------------------------
# Resultados das Secoes 2 a 8 com cache em .rds (FSD 31.8). So define objetos.
# Uso, depois de 00_setup.R e 02_trata_base.R:
#   base <- obter_base_tratada()
#   res <- obter_resultados(base)
#   fechar_base(base)

SCRIPTS_SECOES <- c(
  secao2 = "03_secao2_dimensao.R",
  secao3 = "04_secao3_descritiva.R",
  secao4 = "05_secao4_distribuicoes.R",
  secao5 = "06_secao5_relacoes.R",
  secao6 = "07_secao6_descobertas.R",
  secao7 = "08_secao7_problemas.R",
  secao8 = "09_secao8_sintese.R"
)

# A partir da Secao 6, cada secao usa os resultados das anteriores.
SECOES_DEPENDENTES <- c("secao6", "secao7", "secao8")

obter_resultados <- function(base, secoes = 2:8, pasta = caminho("resultados"), forcar = FALSE,
                             scripts = here::here("scripts", SCRIPTS_SECOES)) {
  names(scripts) <- names(SCRIPTS_SECOES)[seq_along(scripts)]
  pedidas <- paste0("secao", secoes)
  ultima <- max(match(pedidas, names(scripts)))
  # As dependentes precisam de todas as anteriores, mesmo que nao pedidas.
  a_obter <- if (any(pedidas %in% SECOES_DEPENDENTES)) names(scripts)[seq_len(ultima)] else pedidas
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

  base_chave <- c(hash_regras(), .md5_arquivos(list.files(here::here("R"), "\\.R$", full.names = TRUE)))
  res <- list()
  chaves <- character()
  for (secao in a_obter) {
    extra <- if (secao %in% SECOES_DEPENDENTES) chaves else character()
    chaves[[secao]] <- .md5_texto(c(base_chave, .md5_arquivos(scripts[[secao]]), extra))
    res[[secao]] <- .obter_secao(base, secao, scripts[[secao]], chaves[[secao]], res, pasta, forcar)
  }
  res[pedidas]
}

.obter_secao <- function(base, secao, script, chave, res, pasta, forcar) {
  arquivo <- file.path(pasta, paste0(secao, ".rds"))
  if (!forcar && file.exists(arquivo)) {
    guardado <- readRDS(arquivo)
    if (identical(guardado$chave, chave)) return(guardado$resultado)
  }
  message("Calculando ", secao, "...")
  amb <- new.env(parent = globalenv())
  sys.source(script, envir = amb)
  calcular <- get(paste0("calcular_", secao), envir = amb)
  resultado <- if (length(formals(calcular)) >= 2) calcular(base, res) else calcular(base)
  temporario <- paste0(arquivo, ".tmp-", Sys.getpid())
  saveRDS(list(chave = chave, resultado = resultado), temporario)
  file.rename(temporario, arquivo)
  resultado
}

.md5_arquivos <- function(arquivos) unname(tools::md5sum(sort(arquivos)))

.md5_texto <- function(x) {
  arquivo <- tempfile()
  on.exit(unlink(arquivo))
  writeLines(x, arquivo)
  unname(tools::md5sum(arquivo))
}
