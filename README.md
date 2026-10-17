# Commodity Flow Survey 2022 — projeto de Data Science em R

Análise da **Commodity Flow Survey (CFS) 2022 — Public Use Microdata (PUMS)**
do U.S. Census Bureau: fluxos de remessas por setor, tipo de mercadoria,
modal, valor, peso e distância. Cada linha do arquivo é uma remessa, e o
`WGT_FACTOR` diz quantas remessas reais ela representa.

## Começando

```r
# 1. abra commodity-flow-survey.Rproj no RStudio (define a raiz do projeto)
# 2. o renv se ativa sozinho; instale as versões travadas do lockfile:
renv::restore()
# 3. carregue as funções do projeto e o tratamento da base:
source("scripts/00_setup.R")
source(here::here("scripts", "02_trata_base.R"))
```

O arquivo bruto `data/raw/cfs_2022_pums.csv` **não** está no repositório
(tamanho + regra de dados brutos). Baixe do Census e coloque nessa pasta.

## A base tratada

`obter_base_tratada()` lê o CSV original e devolve **sempre a base completa
tratada** (37.576.546 linhas) como tabela lazy do DuckDB: as consultas dplyr
rodam no banco e só o resultado vem para a RAM.

```r
base <- obter_base_tratada(pipeline = FALSE)

base |>
  filter(!fl_exportacao) |>
  count(MODE, sort = TRUE) |>
  collect()

fechar_base(base)   # sempre feche: libera a conexão (e apaga o temporário)
```

O tratamento:

- define os tipos explicitamente: códigos (estado, metrópole, área, SCTG,
  modal) ficam como **texto**, com o zero à esquerda; a metrópole tem 3 ou 5
  dígitos e nunca é completada com zeros;
- acrescenta `peso_t` (toneladas métricas) e `dist_km`, mantendo as unidades
  originais;
- acrescenta 9 flags de qualidade (`fl_sctg_colapsado`, `fl_origem_suprimida`,
  `fl_exportacao`, `fl_valor_teto`, `fl_razao_atipica`, ...);
- **não remove nenhuma linha**: cada análise filtra pelas flags o que precisar.

O parâmetro `pipeline` só escolhe **de onde** vem a base:

| `pipeline` | Comportamento |
|---|---|
| `TRUE` (padrão hoje) | processa o CSV num arquivo DuckDB temporário (~25 s), sem gravar nada em `data/processed/` |
| `FALSE` | usa `data/processed/cfs_tratada.duckdb` se ela estiver atual; se faltar ou se as regras de `scripts/02_trata_base.R` mudaram, reprocessa e regrava |

Depois da validação do professor (pós Check-in 2), o padrão passa a ser
`FALSE` e `data/processed/cfs_tratada.duckdb` vira a fonte oficial das
análises.

**Memória:** numa passada, o tratamento usa ~15 GB de RAM. Em máquinas com
menos memória a função detecta isso sozinha, limita o DuckDB a metade da RAM e
processa por partes, gravando o excedente em disco: mais lento (minutos), mesmo
resultado.

### Fator de ponderação

O `WGT_FACTOR` é o número de remessas que a linha representa. Ele entra **só
em agregações** (ex.: `sum(WGT_FACTOR)` = número estimado de remessas;
`sum(SHIPMT_VALUE * WGT_FACTOR)` = valor total), nunca como coluna por linha.
Contar linhas descreve o arquivo sorteado, não a população de remessas.

## Relatório da Seção 1 (Check-in 2)

```bash
quarto render reports/secao1_qualidade.qmd   # gera .html e .pdf em reports/
```

Requer o [Quarto](https://quarto.org) instalado. O `reports/.Rprofile` ativa o
renv do projeto durante a renderização. Os arquivos gerados não são
versionados.

## Ambiente reprodutível (renv)

As versões de todos os pacotes estão travadas em `renv.lock` (R 4.6.1).

| Situação | Comando |
|---|---|
| Clonou o repo / lockfile mudou | `renv::restore()` |
| Instalou um pacote novo | `renv::install("pacote")` e depois `renv::snapshot()` |
| Conferir se lockfile e biblioteca batem | `renv::status()` |

`renv.lock` é versionado; `renv/library/` não. No Linux o `.Rprofile` aponta
para o Posit Package Manager, que entrega binários prontos.

## Estrutura

```
.
├── commodity-flow-survey.Rproj
├── renv.lock               # versões travadas dos pacotes (renv)
├── config.yml              # caminhos e ambientes (raw, base_tratada, banco)
├── .Renviron.example       # modelo de credenciais (copie para .Renviron)
├── R/                      # funções reutilizáveis (sem efeito colateral)
│   ├── config.R            #   cfg() e caminho()
│   ├── banco.R             #   conectar_bd(), consultar(), tbl_bd(), ...
│   └── dicionario.R        #   ler_dicionario(), rotular(), checar_rotulos()
├── scripts/
│   ├── 00_setup.R          # pacotes + funções de R/
│   ├── 01_explora_dados_brutos.R  # primeira olhada no CSV completo
│   └── 02_trata_base.R     # obter_base_tratada(), fechar_base()
├── data/
│   ├── raw/                # CSV original, nunca escrito, não versionado
│   ├── processed/          # base tratada (cfs_tratada.duckdb), não versionada
│   └── external/           # dicionário do Census (versionado)
├── reports/                # relatórios Quarto
├── outputs/                # figuras e tabelas geradas
├── docs/                   # guias oficiais e notas do time
└── tests/                  # testthat (com CSVs sintéticos em fixtures/)
```

## Rótulos

A base guarda só códigos. Para exibir, aplique o dicionário em resultados já
agregados:

```r
dic <- ler_dicionario()
base |> count(SCTG) |> collect() |> rotular(dic, "SCTG")
```

`ORIG_MA`, `DEST_MA` e `SHIPMT_ID` não têm rótulo no dicionário: para nomes de
área use `ORIG_CFS_AREA` / `DEST_CFS_AREA`.

## Banco de dados externo

A conexão genérica fica em `R/banco.R`, com o destino escolhido por ambiente
(`R_CONFIG_ACTIVE` no `.Renviron`, blocos em `config.yml`): `local_duckdb`
(padrão) ou `postgres` (credenciais só em variáveis de ambiente). Para usar o
Postgres, copie `.Renviron.example` para `.Renviron` e preencha (o `.Renviron`
**não** vai para o git).

## Convenções

- `data/raw` nunca é escrito: nenhum script grava lá;
- caminhos sempre com `here::here()` / `caminho()`; nunca `setwd()`;
- CSV grande sempre pelo DuckDB, com tipos explícitos, nunca na RAM inteiro;
- funções ficam em `R/` (e o tratamento em `scripts/02_trata_base.R`, que só
  define funções);
- toda saída (base tratada, relatório) é reproduzível pelo código.

## Testes

```bash
Rscript tests/testthat.R
```
