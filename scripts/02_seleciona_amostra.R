# 02_seleciona_amostra.R -------------------------------------------------
# Sorteia a amostra de trabalho a partir do PUMS completo.
# Entrada: data/raw/cfs_2022_pums.csv
# Saida:   data/interim/amostra.csv
#
# Rodar exige RAM suficiente para o arquivo bruto (~12 GB recomendados).
# A amostra ja gerada esta em data/interim, entao normalmente nao e preciso
# repetir este passo; a seed do config.yml garante reprodutibilidade.

source(here::here("scripts", "00_setup.R"))

dados <- ler_csv(caminho("raw"))

set.seed(cfg("seed"))
sorteio <- sample(nrow(dados), cfg("tamanho_amostra"))
amostra <- dados[sorteio]

rm(dados); gc()  # libera a base completa da memoria

escrever_csv(amostra, caminho("amostra"))
