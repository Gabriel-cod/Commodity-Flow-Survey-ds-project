# 03_verifica_amostra.R --------------------------------------------------
# Diagnostico da amostra: estrutura, faltantes, distribuicoes.
# Entrada: data/interim/amostra.csv
# Saida:   outputs/figures/histograma_*.png

source(here::here("scripts", "00_setup.R"))

df <- ler_csv(caminho("amostra"))

glimpse(df)     # linhas, colunas e tipos
head(df)
tail(df)

colSums(is.na(df))  # colunas com faltantes: a principio nenhuma
summary(df)         # colunas SHIPMT_* tem muitos valores extremos -> escala log
colnames(df)

# Valor da mercadoria (log10 por causa da cauda longa)
g_valor <- ggplot(df, aes(x = SHIPMT_VALUE)) +
  scale_x_log10() +
  geom_histogram(bins = 50) +
  labs(x = "Valor do embarque (US$, escala log10)", y = "Frequencia") +
  theme_minimal()

salvar_figura(g_valor, "histograma_valor_mercadoria")

# Peso do embarque
g_peso <- ggplot(df, aes(x = SHIPMT_WGHT)) +
  scale_x_log10() +
  geom_histogram(bins = 50) +
  labs(x = "Peso do embarque (lb, escala log10)", y = "Frequencia") +
  theme_minimal()

salvar_figura(g_peso, "histograma_peso_mercadoria")

# SECTOR comeca em 21 (extracao/industria); ver dicionario para os rotulos.
df |> count(SECTOR, sort = TRUE) |> head(10)
