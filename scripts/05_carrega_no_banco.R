# 05_carrega_no_banco.R --------------------------------------------------
# Publica a base tratada no banco de dados.
# Entrada: data/processed/amostra_dicio.csv + data/external/dicionario.xlsx
# Saida:   tabelas `amostra` e `dicionario` no banco do ambiente ativo
#
# Ambiente ativo = R_CONFIG_ACTIVE no .Renviron:
#   local_duckdb -> arquivo local data/cfs.duckdb (default, nao precisa de rede)
#   postgres     -> banco externo, credenciais via .Renviron

source(here::here("scripts", "00_setup.R"))

amostra    <- ler_csv(caminho("amostra_dicio"))
dicionario <- ler_dicionario()

con <- conectar_bd()
on.exit(desconectar_bd(con), add = TRUE)

gravar_tabela(con, amostra, "amostra")
gravar_tabela(con, dicionario, "dicionario")

DBI::dbListTables(con)

# Exemplo de consulta que roda no banco e so traz o resultado para a RAM:
tbl_bd(con, "amostra") |>
  group_by(SECTOR_desc) |>
  summarise(n = n(), valor_medio = mean(SHIPMT_VALUE, na.rm = TRUE)) |>
  arrange(desc(n)) |>
  collect() |>
  print(n = 10)

desconectar_bd(con)
