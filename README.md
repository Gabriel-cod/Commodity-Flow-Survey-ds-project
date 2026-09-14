# Commodity Flow Survey 2022 — projeto de Data Science em R

Análise da **Commodity Flow Survey (CFS) 2022 — Public Use Microdata (PUMS)**
do U.S. Census Bureau: fluxos de embarques por setor, tipo de mercadoria,
modal, valor, peso e distância.

## Começando

```r
# 1. abra commodity-flow-survey.Rproj no RStudio (define a raiz do projeto)
# 2. o renv se ativa sozinho; instale as versões travadas do lockfile:
renv::restore()
# 3. carregue as funções do projeto:
source("scripts/00_setup.R")
```

Para usar banco de dados externo, copie `.Renviron.example` para `.Renviron`
e preencha as credenciais (o `.Renviron` **não** vai para o git).

## Ambiente reprodutível (renv)

As versões de todos os pacotes estão travadas em `renv.lock` (R 4.6.1, 114
pacotes). Todo mundo do time roda exatamente as mesmas versões.

| Situação | Comando |
|---|---|
| Clonou o repo / lockfile mudou | `renv::restore()` |
| Instalou um pacote novo | `renv::install("pacote")` e depois `renv::snapshot()` |
| Conferir se lockfile e biblioteca batem | `renv::status()` |

`renv.lock` é versionado; `renv/library/` não (o próprio renv cuida disso em
`renv/.gitignore`). No Linux o `.Rprofile` aponta para o Posit Package Manager,
que entrega binários prontos — instalação em segundos em vez de compilar.

## Estrutura

```
.
├── commodity-flow-survey.Rproj
├── renv.lock               # versões travadas dos pacotes (renv)
├── config.yml              # parâmetros e ambientes (seed, caminhos, banco)
├── .Renviron.example       # modelo de credenciais (copie para .Renviron)
├── R/                      # funções reutilizáveis (sem efeito colateral)
│   ├── config.R            #   cfg() e caminho()
│   ├── io.R                #   ler_csv(), escrever_csv(), salvar_figura()
│   ├── banco.R             #   conectar_bd(), consultar(), tbl_bd(), ...
│   └── dicionario.R        #   ler_dicionario(), rotular(), checar_rotulos()
├── scripts/                # pipeline numerado, roda na ordem
│   ├── 00_setup.R
│   ├── 01_explora_dados_brutos.R
│   ├── 02_seleciona_amostra.R
│   ├── 03_verifica_amostra.R
│   ├── 04_aplica_dicionario.R
│   └── 05_carrega_no_banco.R
├── data/
│   ├── raw/                # PUMS original, imutável, não versionado
│   ├── interim/            # amostra sorteada
│   ├── processed/          # base pronta para análise
│   └── external/           # dicionário do Census (versionado)
├── outputs/
│   ├── figures/            # gráficos gerados
│   └── tables/             # tabelas exportadas
├── reports/                # relatórios .qmd / .Rmd
├── docs/
│   ├── referencias/        # guia do CFS, descrição das variáveis
│   └── notas/              # anotações do time
└── tests/                  # testthat
```

## Pipeline

| Script | Entrada | Saída |
|---|---|---|
| `01_explora_dados_brutos.R` | `data/raw/cfs_2022_pums.csv` | — (exploratório) |
| `02_seleciona_amostra.R` | `data/raw/cfs_2022_pums.csv` | `data/interim/amostra.csv` |
| `03_verifica_amostra.R` | `data/interim/amostra.csv` | `outputs/figures/*.png` |
| `04_aplica_dicionario.R` | amostra + dicionário | `data/processed/amostra_dicio.csv` |
| `05_carrega_no_banco.R` | base tratada | tabelas `amostra` e `dicionario` no banco |

O arquivo bruto `data/raw/cfs_2022_pums.csv` **não** está no repositório
(tamanho + regra de dados brutos). Baixe do Census e coloque nessa pasta.
Se você só quer analisar, comece pelo passo 03: a amostra já está em
`data/interim/`.

## Banco de dados

A conexão é abstraída em `R/banco.R` e o destino é escolhido por ambiente
(`R_CONFIG_ACTIVE` no `.Renviron`, blocos em `config.yml`):

- `local_duckdb` (padrão): arquivo local `data/cfs.duckdb`, sem rede, ótimo
  para consultas analíticas em cima do CSV grande;
- `postgres`: banco externo, credenciais lidas de variáveis de ambiente.

```r
source("scripts/00_setup.R")

con <- conectar_bd()
DBI::dbListTables(con)

# dplyr direto no banco — o SQL roda lá, só o resultado vem para a RAM
tbl_bd(con, "amostra") |>
  filter(SECTOR == 42) |>
  count(SCTG_desc, sort = TRUE) |>
  collect()

desconectar_bd(con)
```

Regras: credenciais só em `.Renviron`, nunca no código; SQL com input externo
sempre parametrizado (`consultar(con, "... WHERE x = $1", list(valor))`);
toda conexão aberta é fechada (`with_bd()` faz isso automaticamente).

## Convenções

- caminhos sempre com `here::here()` / `caminho()`; nunca `setwd()`;
- CSV grande sempre com `data.table::fread`, não `read.csv`;
- `data/raw` é imutável: nada é escrito lá;
- toda saída (figura, tabela, base tratada) é reproduzível rodando os scripts;
- funções ficam em `R/`, execução fica em `scripts/`;
- seed e tamanho de amostra vêm do `config.yml`, não hardcoded.

## Testes

```bash
Rscript tests/testthat.R
```
