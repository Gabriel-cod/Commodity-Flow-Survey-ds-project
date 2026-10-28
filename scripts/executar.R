# executar.R --------------------------------------------------------------
# Ponto de entrada unico: carrega tudo na ordem certa, calcula as secoes e fecha a base.
# Uso, na raiz do projeto (depois de renv::restore() uma vez):
#   Rscript scripts/executar.R          # todas as secoes prontas
#   Rscript scripts/executar.R 2:5      # so as Secoes 2 a 5 (aceita tambem 2,3,4)
# ou abrir no RStudio e clicar em Source (todas as secoes prontas).

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "02_trata_base.R"))
source(here::here("scripts", "10_resultados.R"))

SECOES <- ler_secoes(commandArgs(trailingOnly = TRUE)[1])

base <- obter_base_tratada()
# O finally fecha a conexao mesmo se uma secao falhar.
res <- tryCatch(obter_resultados(base, SECOES), finally = fechar_base(base))

message("Pronto: ", paste(names(res), collapse = ", "), " | cache em ", caminho("resultados"))
