# 01_explora_dados_brutos.R ----------------------------------------------
# Primeira olhada no PUMS completo do CFS 2022.
# Entrada: data/raw/cfs_2022_pums.csv (baixado, nao versionado)
# Saida:   nenhuma (exploratorio)
#
# O CSV tem ~2,7 GB / 37,5 milhoes de linhas: carregar tudo com fread exige
# ~12 GB de RAM. Aqui o DuckDB le o arquivo direto do disco e so traz para o R
# o resultado de cada consulta. As consultas sao escritas em dplyr (dbplyr
# traduz para SQL) ou em SQL puro via consultar().

source(here::here("scripts", "00_setup.R"))

# Banco DuckDB em memoria, so para esta sessao (nao mexe no data/cfs.duckdb).
# Lembre de fechar no fim; on.exit() nao funciona fora de funcao.
con <- DBI::dbConnect(duckdb::duckdb(shared_home = TRUE))

# View sobre o CSV: nada e copiado, cada consulta varre o arquivo de novo.
# SHIPMT_ID e forcado a texto para preservar os zeros a esquerda ("00000001").
DBI::dbExecute(con, sprintf(
  "CREATE VIEW pums AS
   SELECT * FROM read_csv_auto('%s', types = {'SHIPMT_ID': 'VARCHAR'})",
  caminho("raw")
))

pums <- tbl(con, "pums")  # referencia lazy: nao carrega nada ainda

# Estrutura -------------------------------------------------------------
consultar(con, "DESCRIBE pums")             # colunas e tipos inferidos
pums |> head(10) |> collect()
pums |> count() |> collect()                # numero de linhas

# Valores ausentes por coluna -------------------------------------------
pums |>
  summarise(across(everything(), ~ sum(as.integer(is.na(.x)), na.rm = TRUE))) |>
  collect() |>
  pivot_longer(everything(), names_to = "coluna", values_to = "n_na")

# Resumo das variaveis numericas -----------------------------------------
consultar(con, "SUMMARIZE pums")            # min, max, media, quantis, nulos

# Cardinalidade das variaveis categoricas --------------------------------
pums |>
  summarise(
    n_setores  = n_distinct(SECTOR),
    n_sctg     = n_distinct(SCTG),
    n_modais   = n_distinct(MODE),
    n_estados  = n_distinct(ORIG_STATE),
    n_areas    = n_distinct(ORIG_CFS_AREA)
  ) |>
  collect()

# Distribuicao por setor e por modal -------------------------------------
# arrange() vem depois do collect(): em SQL, ORDER BY dentro de subconsulta
# e ignorado, entao ordenar no banco antes do mutate() nao teria efeito.
pums |>
  count(SECTOR) |>
  mutate(pct = 100 * n / sum(n, na.rm = TRUE)) |>
  collect() |>
  arrange(desc(n)) |>
  print(n = Inf)

pums |>
  count(MODE) |>
  collect() |>
  arrange(desc(n)) |>
  print(n = 20)

# Flags sim/nao ----------------------------------------------------------
pums |> count(TEMP_CNTL_YN) |> collect()
pums |> count(EXPORT_YN)    |> collect()
pums |> count(HAZMAT)       |> collect()

# Amostra pequena para inspecao interativa no R --------------------------
# (USING SAMPLE e do DuckDB; a amostra oficial do projeto vem do script 02)
amostra_rapida <- consultar(con, "SELECT * FROM pums USING SAMPLE 100000 ROWS")
glimpse(amostra_rapida)
summary(amostra_rapida)

DBI::dbDisconnect(con, shutdown = TRUE)
