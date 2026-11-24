# 25_resultados_checkin3.R -------------------------------------------------
# Resultados do Check-in 3 com cache em .rds (FSD 32.6). So define objetos.
# Uso, depois de 00_setup.R e 02_trata_base.R:
#   base <- obter_base_tratada()
#   res <- obter_resultados_checkin3(base)
#   fechar_base(base)

if (!exists(".obter_secao", mode = "function")) source(here::here("scripts", "10_resultados.R"))

# Em ordem: cada parte usa as anteriores (todas usam a base de modelagem).
SCRIPTS_CHECKIN3 <- c(
  base_modelagem = "21_base_modelagem.R",
  parte_a = "22_parte_a_linear.R",
  parte_b1 = "23_parte_b1_logistica_simples.R",
  parte_b2 = "24_parte_b2_logistica_multipla.R"
)

obter_resultados_checkin3 <- function(base, partes = names(SCRIPTS_CHECKIN3),
                                      pasta = file.path(caminho("resultados"), "checkin3"), forcar = FALSE,
                                      scripts = here::here("scripts", SCRIPTS_CHECKIN3)) {
  names(scripts) <- names(SCRIPTS_CHECKIN3)[seq_along(scripts)]
  a_obter <- names(scripts)[seq_len(max(match(partes, names(scripts))))]
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

  # A semente e os tamanhos mudam a amostra, entao entram na chave.
  base_chave <- c(hash_regras(), .md5_arquivos(list.files(here::here("R"), "\\.R$", full.names = TRUE)),
                  deparse(cfg("modelagem")))
  res <- list()
  chaves <- character()
  for (parte in a_obter) {
    chaves[[parte]] <- .md5_texto(c(base_chave, .md5_arquivos(scripts[[parte]]), chaves))
    # A base de modelagem le o DuckDB; as partes so precisam da amostra.
    dados <- if (parte == "base_modelagem") base else res$base_modelagem
    res[[parte]] <- .obter_secao(dados, parte, scripts[[parte]], chaves[[parte]], res, pasta, forcar)
  }
  res[partes]
}
