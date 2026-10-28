# 10_resultados.R ---------------------------------------------------------
# Resultados das Secoes 2 a 8 com cache em .rds (FSD 31.8). So define objetos.
# Uso, depois de 00_setup.R e 02_trata_base.R:
#   base <- obter_base_tratada()
#   res <- obter_resultados(base)
#   fechar_base(base)
# Para rodar tudo de uma vez, use scripts/executar.R.

# Carregado sozinho, traz o setup e o tratamento para nao falhar em caminho() ou hash_regras().
if (!exists("caminho", mode = "function")) source(here::here("scripts", "00_setup.R"))
if (!exists("hash_regras", mode = "function")) source(here::here("scripts", "02_trata_base.R"))

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

# Secoes cujo script ja define calcular_secaoN(); as outras ainda estao em construcao.
secoes_prontas <- function(scripts = here::here("scripts", SCRIPTS_SECOES)) {
  pronta <- vapply(seq_along(scripts), function(i) {
    padrao <- paste0("^calcular_", names(SCRIPTS_SECOES)[i], " <- function")
    file.exists(scripts[i]) && any(grepl(padrao, readLines(scripts[i], warn = FALSE)))
  }, logical(1))
  as.integer(sub("secao", "", names(SCRIPTS_SECOES)[pronta]))
}

# Le "2:5", "2,3,4" ou "5" sem avaliar codigo; vazio devolve as secoes prontas.
ler_secoes <- function(texto, prontas = secoes_prontas()) {
  if (!length(texto) || is.na(texto) || !nzchar(trimws(texto))) return(prontas)
  partes <- strsplit(gsub("\\s", "", texto), ",")[[1]]
  if (!all(grepl("^[0-9]+(:[0-9]+)?$", partes))) {
    stop("Secoes invalidas: '", texto, "'. Use, por exemplo, 2:5 ou 2,3,4.", call. = FALSE)
  }
  secoes <- unlist(lapply(strsplit(partes, ":"), function(p) seq(as.integer(p[1]), as.integer(p[length(p)]))))
  fora <- setdiff(secoes, prontas)
  if (length(fora)) {
    stop("Secoes sem script pronto: ", paste(fora, collapse = ", "),
         ". Prontas: ", paste(prontas, collapse = ", "), ".", call. = FALSE)
  }
  sort(unique(secoes))
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
