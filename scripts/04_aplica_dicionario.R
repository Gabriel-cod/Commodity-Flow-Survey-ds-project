# 04_aplica_dicionario.R -------------------------------------------------
# Junta os rotulos do dicionario do Census a amostra.
# Entrada: data/interim/amostra.csv + data/external/dicionario.xlsx
# Saida:   data/processed/amostra_dicio.csv
#
# Nota (docs/notas/2025-09-12-nota-dicionario.md): o dicionario traz os codigos
# como texto e a base como inteiro; `rotular()` normaliza os dois lados antes
# do join, o que elimina os NA que apareciam na primeira tentativa.

source(here::here("scripts", "00_setup.R"))

df <- ler_csv(caminho("amostra"))
dicionario <- ler_dicionario()

glimpse(dicionario)
unique(dicionario$VARIABLE)  # variaveis disponiveis para rotular

df <- df |>
  rotular(dicionario, "SECTOR") |>   # setor da industria
  rotular(dicionario, "SCTG")        # tipo de mercadoria

checar_rotulos(df, "SECTOR_desc")
checar_rotulos(df, "SCTG_desc")

glimpse(df)
escrever_csv(df, caminho("amostra_dicio"))
