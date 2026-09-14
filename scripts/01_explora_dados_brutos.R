# 01_explora_dados_brutos.R ----------------------------------------------
# Primeira olhada no PUMS completo do CFS 2022.
# Entrada: data/raw/cfs_2022_pums.csv (baixado, nao versionado)
# Saida:   nenhuma (exploratorio)

source(here::here("scripts", "00_setup.R"))

# fread lida bem com bases grandes; read.csv trava maquinas com pouca RAM.
dados <- ler_csv(caminho("raw"))

head(dados)
tail(dados)
colnames(dados)
nrow(dados)
ncol(dados)
anyNA(dados)
glimpse(dados)
