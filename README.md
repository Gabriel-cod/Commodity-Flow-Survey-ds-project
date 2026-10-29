# Commodity Flow Survey 2022 — Grupo 6

**Trabalho do Grupo 6 de Introdução à Ciência de Dados (UFU, 2026/02) sobre as remessas de carga
dos Estados Unidos em 2022. Este repositório guarda o código que prepara os dados, os relatórios de
cada check-in e o motivo de cada decisão tomada.**

- **Equipe:** Gabriel Alves Silva (líder), Gustavo Barco Pereira Santos e Mirella Garcia.
- **Professor:** Dr. G.
- **Disciplina:** Introdução à Ciência de Dados, UFU, 2026/02.

| Entrega | Prazo | Situação |
|---|---|---|
| Check-in 1: conhecendo a base | 30/09/2026 | Entregue |
| Check-in 2: explorando e descrevendo a base | 26/10/2026 | Em andamento. A Seção 1 está pronta e as Seções 2 a 8 vêm a seguir |

## Por onde começar

| Quem você é | O que ler primeiro |
|---|---|
| O professor, querendo entender o que foi feito e como | [1. O projeto](#1-o-projeto), depois [3. Linha do tempo](#3-linha-do-tempo-das-entregas) |
| O Gustavo, ou quem vai continuar a análise | [Como continuar a partir daqui](#como-continuar-a-partir-daqui) |
| Quem só quer rodar o código | [4. Instalação e uso](#4-instalação-e-uso) |
| Quem encontrou um termo estranho | [5. Glossário](#5-glossário) |

---

## 1. O projeto

### 1.1 A base de dados

**É uma tabela enorme em que cada linha é uma remessa de carga enviada por uma empresa dos EUA em
2022. Ela diz de onde a carga saiu, para onde foi, o que levava, por qual meio, quanto valia, quanto
pesava e a que distância foi.**

A pesquisa se chama *Commodity Flow Survey* (CFS). Ela é feita a cada 5 anos pelo Census Bureau,
o órgão de estatística dos EUA, junto com o BTS, o órgão de estatística de transportes. Usamos a
versão pública de 2022, chamada **PUMS**.

| Medida | Valor |
|---|---|
| Linhas, uma por remessa | 37.576.546 |
| Colunas | 18 |
| Tamanho do arquivo | cerca de 2,8 GB, num único CSV |
| Período | só o ano de 2022, sem série histórica |
| Cobertura | os 50 estados e o Distrito de Columbia |

As 18 colunas respondem a cinco perguntas sobre cada remessa:

| Pergunta | Colunas |
|---|---|
| **Onde?** (origem e destino) | `ORIG_STATE`, `ORIG_MA`, `ORIG_CFS_AREA`, `DEST_STATE`, `DEST_MA`, `DEST_CFS_AREA` |
| **O quê?** (a carga e quem envia) | `SCTG` (tipo de mercadoria), `SECTOR` (setor da empresa), `HAZMAT` (material perigoso), `TEMP_CNTL_YN` (controle de temperatura) |
| **Como?** (transporte e exportação) | `MODE` (modal: caminhão, trem, avião…), `EXPORT_YN`, `EXPORT_CNTRY` |
| **Quanto?** | `SHIPMT_VALUE` (US$), `SHIPMT_WGHT` (libras), `SHIPMT_DIST_GC` (milhas em linha reta) |
| **Quantas como esta?** | `WGT_FACTOR` (fator de ponderação) |

Há ainda `SHIPMT_ID`, que é só um número de identificação.

#### Cada linha vale por várias remessas

**O arquivo não traz todas as remessas de 2022, só uma parte sorteada. O `WGT_FACTOR` diz por
quantas remessas reais cada linha sorteada responde.**

Uma linha com `WGT_FACTOR = 250` representa cerca de 250 remessas com aquelas mesmas
características em 2022. Por isso, contar linhas descreve o sorteio, não os EUA. Para falar dos EUA, o
fator entra nas somas: por exemplo, o número estimado de remessas é a soma de `WGT_FACTOR`.

O fator **nunca** multiplica o peso ou o valor de uma linha isolada. "Peso × fator" numa única
linha faria parecer que uma carga gigante viajou uma vez, quando na verdade uma carga daquele peso
viajou muitas vezes. Ele só entra em totais e contagens; os detalhes estão na
[Regra 4](#24-regras-que-valem-para-todo-o-projeto).

#### O que a base não tem

**Algumas perguntas não têm resposta nesta base, e isso moldou as perguntas do grupo.**

- **Não há custo de frete.** `SHIPMT_VALUE` é o valor da mercadoria, não o preço do transporte.
- **Não há importações:** só remessas que saem de empresas nos EUA.
- **Não há fazendas.** A agricultura só aparece pelo tipo de mercadoria.
- **Não há trimestre nem rota real.** Toda estimativa é anual, e a distância é em linha reta.
- **Nas exportações, o destino é o porto ou a fronteira de saída**, não o país final.

### 1.2 O objetivo e as perguntas

**O grupo quer entender como a carga se organiza no território dos EUA. Três perguntas dividem
esse objetivo em partes.**

| | Pergunta | Variáveis principais |
|---|---|---|
| **P1 — Fluxos** | De onde a carga sai, para onde vai e o que circula em cada fluxo? | origem, destino, mercadoria, peso |
| **P2 — Dependência** | Quais destinos dependem de origens distantes ou de uma única origem? | destino, origem, mercadoria, distância |
| **P3 — Modal** | Como peso e distância se relacionam com o meio de transporte usado? | peso, modal, mercadoria, distância |

A P1 é a base das outras duas. A P2 olha os fluxos da P1 pelo lado de quem recebe. A P3 descreve o
modal **usado**, não o mais barato, porque a base não tem custo de frete.

**As cinco variáveis de interesse**, escolhidas no Check-in 1, são `ORIG_CFS_AREA`,
`DEST_CFS_AREA`, `SCTG`, `MODE` e `SHIPMT_WGHT`. A distância (`SHIPMT_DIST_GC`) ficou de fora das
cinco porque é consequência do par origem–destino, mas continua sendo usada na P2 e na P3.

---

## 2. Planejamento e arquitetura

### 2.1 Ferramentas

**Tudo é feito em R. Um banco de dados embutido, o DuckDB, lida com o arquivo grande, e o Quarto
transforma código em relatório.**

| Ferramenta | Para que serve aqui | Por que esta |
|---|---|---|
| **R 4.6.1** | Linguagem de todo o projeto | É a linguagem da disciplina |
| **renv** | Trava a versão de cada pacote em `renv.lock` | O mesmo código dá o mesmo resultado no computador de qualquer pessoa |
| **DuckDB** | Lê o CSV e responde a consultas sem carregar a base inteira na memória | 37,5 milhões de linhas não cabem com folga na memória de um notebook comum. O PySpark foi descartado porque é feito para terabytes |
| **dplyr + dbplyr** | Consultas escritas em dplyr rodam dentro do DuckDB | É o dplyr ensinado na disciplina, sem precisar escrever SQL |
| **Quarto** | Gera os relatórios em HTML e PDF | Cada número do relatório é calculado na hora a partir dos dados; nada é copiado à mão |
| **testthat** | Testes automáticos das regras de tratamento | As regras são conferidas em arquivos pequenos de exemplo, sem depender da base real |
| **Git / GitHub** | Histórico e compartilhamento do código | Cada mudança fica registrada, com quem fez e por quê |

### 2.2 Como os dados passam pelo projeto

**O arquivo original nunca é alterado. Uma função lê esse arquivo, prepara os dados e entrega uma
versão tratada, que é o ponto de partida de todas as análises.**

```
data/raw/cfs_2022_pums.csv          o original do Census, só leitura
          │
          ▼
obter_base_tratada()                em scripts/02_trata_base.R
  1. lê cada coluna com o tipo certo
  2. confere linhas, colunas e códigos (se algo não bate, para com erro)
  3. cria colunas em unidades métricas e marcações de qualidade
          │
          ▼
base tratada no DuckDB              data/processed/cfs_tratada.duckdb
          │
          ├──► relatórios Quarto    reports/
          └──► análises do grupo    consultas dplyr
```

Por que foi feito assim:

- **O original fica intacto.** Assim, qualquer pessoa pode conferir os resultados com o arquivo do
  Census. A impressão digital (MD5) do CSV é `1acc7e53e869de3fffccc419495f6e8d` e não mudou em
  nenhum momento.
- **O tratamento é uma função só.** O professor, o Gustavo e o relatório usam exatamente as mesmas
  regras, e uma correção feita num lugar vale para todos.
- **Nenhuma linha é removida.** Cada análise decide o que excluir e diz isso na própria tabela, como
  o professor pede na Seção 2 do Check-in 2.

### 2.3 Estrutura de pastas

**O código fica em `R/` e `scripts/`, os dados em `data/` e os relatórios em `reports/`.**

```
.
├── README.md               este arquivo
├── commodity-flow-survey.Rproj
├── renv.lock               versões travadas dos pacotes
├── config.yml              caminhos dos arquivos e configuração do banco
├── .Renviron.example       modelo de credenciais, só para o banco externo opcional
├── R/                      funções de apoio
│   ├── config.R            cfg() e caminho(): leem o config.yml
│   ├── banco.R             conexão com banco de dados
│   └── dicionario.R        ler_dicionario() e rotular(): nomes legíveis dos códigos
├── scripts/
│   ├── 00_setup.R          carrega os pacotes e as funções de R/
│   ├── 01_explora_dados_brutos.R   primeira olhada no CSV completo (Check-in 1)
│   └── 02_trata_base.R     o tratamento: obter_base_tratada() e fechar_base()
├── data/
│   ├── raw/                CSV original. Nunca é escrito e não vai para o Git
│   ├── processed/          base tratada (cfs_tratada.duckdb). Não vai para o Git
│   └── external/           dicionário oficial do Census (vai para o Git)
├── reports/                relatórios Quarto (.qmd); o HTML e o PDF gerados não vão para o Git
├── outputs/                figuras e tabelas exportadas
├── docs/referencias/       guia oficial da CFS 2022 e descrição das variáveis
└── tests/                  testes automáticos, com CSVs de exemplo em fixtures/
```

### 2.4 Regras que valem para todo o projeto

**Cinco regras valem para todo o código e todo o texto do grupo. Cada uma existe para evitar um erro
específico.**

1. **`data/raw/` nunca é escrito.**
   *Por quê:* o original é a referência para conferir tudo. Se ele mudar, nada mais pode ser
   verificado.
2. **Nenhuma linha é removida da base tratada.** As exclusões são feitas em cada análise, e cada
   tabela ou gráfico diz qual população usa.
   *Por quê:* uma exclusão que serve para uma pergunta pode estar errada para outra.
3. **Códigos são texto, nunca número.**
   *Por quê:* lido como número, o estado `01` (Alabama) vira `1`, e o código de metrópole suprimida
   `00000` vira `0`. A metrópole tem 3 **ou** 5 dígitos, por isso nunca se completa com zeros.
4. **O `WGT_FACTOR` só entra em somas e contagens, nunca multiplicado linha a linha.** Todo uso novo
   de ponderação, e toda interpretação nova de uma variável, é combinado com o Gabriel antes de ir
   para código ou texto.
   *Por quê:* este foi o erro de interpretação mais fácil de cometer no projeto (veja a
   [seção 1.1](#cada-linha-vale-por-várias-remessas)).
5. **Os caminhos são sempre relativos**, com `here::here()` ou `caminho()`, e nunca se usa
   `setwd()`.
   *Por quê:* o mesmo código precisa funcionar em qualquer computador, sem ajustes.

Além disso, nenhuma senha ou credencial vai para o repositório. Quando existirem, ficam só num
`.Renviron` local.

---

## 3. Linha do tempo das entregas

**Cada check-in tem um bloco abaixo, em ordem. Cada bloco diz o que foi entregue, as decisões
tomadas e por quê, e o que mudou depois.**

### Check-in 1 — Conhecendo a base (30/09/2026)

**Entregue. O grupo apresentou o que é a base, o que cada linha representa, um dicionário parcial,
as perguntas que guiam o trabalho e uma lista de problemas e dúvidas.**

**O que foi feito:**

- **Estrutura do repositório** (14/09/2026): R, renv e DuckDB. No começo, um fluxo de scripts
  sorteava uma amostra de 200 mil linhas para explorar mais rápido.
- **Exploração do CSV completo**, em `scripts/01_explora_dados_brutos.R`.
- **Apresentação em seis partes:**
  1. contexto e cobertura;
  2. unidade e estrutura;
  3. dicionário parcial (15 das 18 variáveis);
  4. variáveis de interesse e as perguntas P1, P2 e P3;
  5. problemas e dúvidas;
  6. fontes.

**Os principais problemas levantados:**

- **Proteções de sigilo do Census.** Para que nenhuma empresa seja reconhecida, o arquivo público
  passou por cinco alterações:
  - **sorteio** das remessas;
  - **teto** no valor (US$ 30 milhões);
  - **ruído** leve no valor e no peso;
  - **arredondamento** para números inteiros;
  - **colapsamento**, isto é, a troca de um detalhe por algo mais genérico. Por exemplo, "Chicago"
    vira só "Illinois".
- **Limites da base**, os mesmos da [seção 1.1](#o-que-a-base-não-tem):
  - o valor não é frete;
  - não há importações nem fazendas;
  - a exportação para no porto;
  - a revenda é contada de novo a cada trecho;
  - o fator de ponderação entra em todas as somas.

**O que mudou depois:**

- **Três afirmações foram corrigidas**, ao medir a base completa no Check-in 2 (veja
  [Correções ao Check-in 1](#correções-ao-check-in-1)).
- **A amostra de 200 mil linhas foi abandonada.** Com o DuckDB, a base inteira é tratada em minutos.
  A amostra criava uma segunda camada de sorteio por cima da do Census, o que complicava a conta dos
  totais sem necessidade. Os scripts que a produziam (02 a 05) foram removidos.

### Check-in 2 — Explorando e descrevendo a base (prazo 26/10/2026)

**Em andamento. O professor pede oito seções. A primeira, sobre preparação e qualidade dos dados,
está pronta. As outras sete usam a mesma base tratada.**

A pergunta central do check-in é *"o que conseguimos aprender sobre os dados antes de
modelá-los?"*. Ainda não entram modelos: nada de regressão, classificação, *clustering* ou testes de
hipóteses.

| Seção | Tema | Situação |
|---|---|---|
| 1 | Preparação e qualidade dos dados | **Pronta**: `reports/secao1_qualidade.qmd` (e no `reports/checkin2.qmd`) |
| 2 | Dimensão da base após a preparação | **Calculada**: `scripts/03_secao2_dimensao.R` |
| 3 | Análise descritiva de 8 a 10 variáveis, incluindo as 5 do Check-in 1 | **Calculada**: `scripts/04_secao3_descritiva.R` |
| 4 | Visualização das distribuições | **Calculada**: `scripts/05_secao4_distribuicoes.R` |
| 5 | Relações entre variáveis (pelo menos quatro) | **Calculada**: `scripts/06_secao5_relacoes.R` |
| 6 | Três descobertas interessantes | A fazer |
| 7 | Problemas identificados (atualiza a lista do Check-in 1) | A fazer |
| 8 | O que aprendemos sobre a base | A fazer |

#### Seção 1 — Preparação e qualidade dos dados ✓

**A base inteira, com 37,5 milhões de linhas, foi verificada nos 10 pontos pedidos pelo professor.
Nada foi apagado: cada problema virou uma marcação, que cada análise usa para incluir ou excluir
aquelas linhas.**

- **Onde ver:** o relatório `reports/secao1_qualidade.qmd`, que gera HTML e PDF.
- **De onde vêm os números:** todos são calculados na hora a partir da base tratada. Os números
  abaixo são os da versão processada em 17/10/2026.

##### A função de tratamento

**Uma única função, `obter_base_tratada()`, entrega sempre a mesma base preparada, para quem a
chamar: o professor, o Gustavo ou o próprio relatório.**

Ela segue quatro passos:

1. **Lê o CSV declarando o tipo de cada coluna.** Os códigos são lidos como texto, com o zero à
   esquerda. Esse é o primeiro passo do tratamento, porque todo o resto depende dele.
2. **Confere o resultado.** Verifica se a base tem a mesma quantidade de linhas do CSV, se as colunas
   estão na ordem esperada e se cada código tem uma largura válida. Se algo não bate, a função **para
   com erro** em vez de entregar uma base errada.
3. **Acrescenta colunas novas.**
   - Duas colunas em unidades métricas: `peso_t` (toneladas) e `dist_km` (quilômetros).
   - Nove **marcações de qualidade**, as colunas `fl_*`. Uma marcação é uma coluna de sim/não, como
     um post-it colado na linha dizendo "esta remessa é uma exportação". A linha continua lá; quem
     não quer exportações filtra por essa coluna.
4. **Devolve a base** como uma tabela do DuckDB, pronta para consultas em dplyr.

O parâmetro `pipeline` só escolhe **de onde** vem a base:

- **`pipeline = FALSE`** é o padrão. A função guarda a base em `data/processed/` e a reaproveita
  nas próximas vezes. Ela **refaz a base sozinha** em três casos:
  - quando ela não existe;
  - quando o arquivo de regras mudou;
  - quando o CSV mudou.

  Ela percebe que as regras mudaram por uma "impressão digital" (*hash*) do arquivo
  `scripts/02_trata_base.R`, gravada junto com a base. E percebe que o CSV mudou pelo tamanho, pela
  data e, se a data for outra, pela impressão digital do próprio CSV. Assim, **uma cópia da base
  feita em outro computador vale**, sem ser refeita.
- **`pipeline = TRUE`** refaz tudo do zero num arquivo temporário e o apaga ao fechar. Serve para
  conferir o tratamento, não para o dia a dia: são uns 3,5 minutos a cada chamada.

**Memória.** A base é montada em blocos de 1 milhão de linhas, com o DuckDB limitado a 512 MiB de
memória e 1 núcleo. O pico fica abaixo de 1 GiB e a montagem leva uns 3,5 minutos, em qualquer
computador com 4 GB de RAM ou mais. Os arquivos temporários ficam em `data/interim/`, no disco do
projeto, e as consultas sobre a base também têm limite: o que passa dele vai para o disco.

*Por quê:* a função precisa funcionar no computador de qualquer integrante e no do professor, não
só no de quem a escreveu, e esses computadores podem ter só 4 GB. A primeira versão limitava a
memória a metade da RAM, e a máquina do Gustavo travou. A segunda montava em blocos maiores, mas
ainda travou com `pipeline = TRUE`: o arquivo temporário ia para o `/tmp`, que em muitos Linux fica
na memória, e algumas contas (mediana, percentis, contagem de distintos) ignoravam o limite.
Medimos tudo simulando um computador com 1,5 GiB livres, sem swap: a montagem, o reaproveitamento e
o relatório da Seção 1 cabem, e os números são idênticos aos de antes.

##### Os 10 itens verificados

**Cada item abaixo diz o que encontramos, o que decidimos e por quê.**

**1. Tipos das variáveis e erros de importação**

- **Encontramos:** se o tipo das colunas fosse adivinhado, os códigos de estado, mercadoria e
  metrópole virariam números e perderiam o zero à esquerda. Por exemplo, o código de metrópole
  suprimida `00000` viraria `0`.
- **Decidimos:** declarar o tipo de cada coluna, sem adivinhação:
  - os códigos como texto;
  - valor, peso e distância como números inteiros;
  - o fator de ponderação como número decimal.

  A leitura é estrita: uma linha que não converte interrompe o processamento. Depois da leitura, a
  largura de cada código é conferida, e hoje há 0 códigos fora do padrão.
- **Por quê:** um erro silencioso aqui contaminaria todas as análises seguintes, sem aviso.

**2. Códigos de ausente, ignorado ou não aplicável**

- **Encontramos:**
  - **mercadoria escondida por sigilo:** 49.820 linhas com SCTG `00` e 48.814 com faixas, como
    `01-05`;
  - **origem escondida:** 1.589 linhas com só o estado visível (`NN-00000`) e 2.407 sem nem o estado
    (`00-00000`);
  - **modal desconhecido:** 545.337 linhas com `MODE = 30`;
  - **não aplicável:** `EXPORT_CNTRY = N` nas remessas que não são exportação.
- **Decidimos:** manter todas essas linhas e marcá-las (`fl_sctg_colapsado`,
  `fl_origem_ma_suprimida`, `fl_origem_suprimida`).
- **Por quê:** é informação escondida pelo sigilo, não um erro. Essas linhas entram nos totais e
  saem só dos cortes em que atrapalham.

Um cuidado: o código `99999` de metrópole **não** é ausência. Ele significa "restante do estado",
uma área real.

**3. Valores ausentes**

- **Encontramos:** nenhum valor nulo e nenhum campo vazio em nenhuma das 18 colunas.
- **Decidimos:** não há nada a preencher nem a remover.
- **Por quê:** a ausência de informação já vem codificada (item 2).

**4. Valores impossíveis ou suspeitos**

- **Encontramos:**
  - **nenhum zero** em valor, peso ou distância (o mínimo é 1);
  - **79 valores ≥ US$ 30 milhões**: 69 exatamente no teto e 10 acima dele;
  - **469 fatores de ponderação** no teto do guia (3.588.000);
  - **7.220 fatores menores que 1**;
  - **74.554 remessas com razão valor/peso atípica**, entre as 0,1% mais baratas ou as 0,1% mais
    caras por libra dentro da própria mercadoria.
- **Decidimos:** manter tudo e marcar (`fl_valor_teto`, `fl_fator_teto`, `fl_fator_menor_1`,
  `fl_razao_atipica`).
- **Por quê:**
  - o Census ajustou valor e peso preservando os totais, então cortar os extremos mudaria os totais;
  - o fator menor que 1 está previsto no guia oficial (nota 5);
  - a razão é comparada dentro de cada mercadoria porque ouro e areia têm preços por quilo muito
    diferentes.

**5. Duplicações**

- **Encontramos:** o `SHIPMT_ID` nunca se repete, mas 623.356 linhas (1,66%) são idênticas a outra
  em todas as demais 17 colunas.
- **Decidimos:** manter essas linhas.
- **Por quê:** o Census não trata esse caso, então esta é uma **inferência do grupo**, a partir de
  como a pesquisa é feita:
  - cada empresa informa até 40 remessas por semana;
  - cada parada de uma rota conta como uma remessa;
  - valor, peso e distância são arredondados.

  Assim, duas remessas diferentes da mesma empresa podem ficar iguais no arquivo. Não há como
  confirmar, porque a base não traz um identificador da empresa.

**6. Unidades de medida**

- **Encontramos:**
  - o valor em US$, como valor da mercadoria na origem, não do frete;
  - o peso em libras;
  - a distância em milhas, em linha reta.
- **Decidimos:** manter as unidades originais e criar `peso_t` (toneladas métricas) e `dist_km`.
- **Por quê:** as originais servem para comparar com as tabelas oficiais, e as métricas são mais
  fáceis de ler no Brasil.

**7. Categorias com poucos registros**

- **Encontramos:**
  - o modal "outros modos únicos" (`MODE 16`) tem só 84 linhas, e "Grandes Lagos" (`132`) tem 133;
  - entre as mercadorias, a faixa agregada `20-24` tem 172 linhas, e minérios metálicos (`14`) têm
    561.
- **Decidimos:** não agrupar nada. Nas análises, cada comparação com categorias pequenas vai trazer
  a confiabilidade da estimativa. Ela é medida pelo coeficiente de variação (CV), calculado com o
  método oficial do guia, mais um número mínimo de linhas. Os valores desses limites serão
  definidos nas análises.
- **Por quê:** agrupar apagaria diferenças que podem ser reais. É melhor mostrar a categoria com o
  aviso de incerteza.

**8. Necessidade de combinar arquivos**

- **Encontramos:** a base é um arquivo único. A única combinação é com o dicionário oficial, e só
  para mostrar o nome de cada código.
- **Decidimos:** aplicar os nomes **só na hora de exibir**, com `rotular()`. A base guarda apenas
  os códigos. O dicionário chama uma coluna de `TEMP_CNTRL_YN` e a base a chama de `TEMP_CNTL_YN`, e
  a função faz essa tradução.
- **Por quê:** guardar só os códigos deixa a base menor e evita gravar um nome errado em milhões
  de linhas.

**9. Variáveis derivadas**

- **Encontramos:** as análises vão precisar de unidades métricas e de um jeito simples de separar
  os casos especiais.
- **Decidimos:** criar 2 colunas métricas e 9 marcações (a lista completa está na
  [seção 4.3](#43-as-colunas-novas-da-base-tratada)). Não há colunas ponderadas por linha.
- **Por quê:** veja o item 10.

**10. Pesos amostrais**

- **Encontramos:**
  - a soma de `WGT_FACTOR` dá **28.859.608.542 remessas estimadas** em 2022, representadas por
    37.576.546 linhas;
  - a **mediana do peso de uma remessa** é de 20 libras contando linhas, mas de 3 libras usando o
    fator.

  A diferença vem das encomendas (`MODE 21`). Elas são 44% das linhas, mas 59% das remessas
  estimadas, e são leves.
- **Decidimos:** usar o fator só em agregações e mostrar as distribuições por remessa **com e sem
  o fator, lado a lado**.
- **Por quê:** sem o fator, o resultado descreve o sorteio. Com ele, descreve os EUA. Os dois juntos
  mostram ao leitor o tamanho do efeito.

##### Como conferimos que está certo

**Testes automáticos garantem as regras, e um número de controle publicado pelo próprio Census
garante que a leitura e a soma estão corretas.**

- **Testes automáticos:** 110 verificações rodam em arquivos pequenos de exemplo. Elas cobrem os
  tipos, cada marcação, a reutilização da base, a memória e a proteção do arquivo original.
- **Checagem de Maryland:** o guia oficial publica um resultado de controle. Somando o valor das
  remessas que saíram de Maryland, com o fator de ponderação, o total deve ser de
  US$ 198.598.788.295, em 612.317 linhas. A nossa base dá **as mesmas 612.317 linhas e
  US$ 198.598.788.344**. A diferença de US$ 49 em quase 200 bilhões é arredondamento numérico. Se
  alguma coluna tivesse sido lida errado, esse número não bateria.
- **Original intacto:** o MD5 do CSV é o mesmo de antes do tratamento.

##### Correções ao Check-in 1

**Medir a base completa corrigiu três afirmações da primeira apresentação.**

- **Zeros:** dissemos que zero em peso, valor ou distância podia ser efeito de arredondamento.
  Medindo, não há nenhum zero: o mínimo é 1.
- **Teto de valor:** dissemos que o valor é cortado em US$ 30 milhões. O corte não é rígido: 10
  linhas passam do teto.
- **Colapsamento:** dissemos que cerca de 100 mil remessas tiveram a origem trocada pelo estado. Na
  verdade:
  - o colapsamento atinge quase só a **mercadoria**, em 98.634 linhas;
  - a origem é colapsada em só 3.996 linhas;
  - em 2.407 dessas, nem o estado de origem é conhecido.

##### Dúvidas em aberto

- Os 10 valores acima de US$ 30 milhões são ruído legítimo do Census ou um erro do arquivo? A
  documentação não permite decidir.
- Por que o dicionário traz códigos que não aparecem na base? Uma hipótese é que não houve remessas
  nesses casos.
- Quais serão os limites de confiabilidade (o CV máximo e o mínimo de linhas por célula)? Eles serão
  definidos nas análises.

##### Como continuar a partir daqui

**Para fazer as Seções 2 a 8: abra a base tratada com três linhas de R, filtre pelas marcações
conforme a pergunta e use o fator de ponderação só em somas, combinando antes com o Gabriel.**

**Passo 1 — Preparar o computador (só uma vez)**

1. **Atualize o repositório:** `git pull`.
2. **Abra o projeto:** `commodity-flow-survey.Rproj`, no RStudio ou no Positron.
3. **Instale os pacotes nas versões travadas:** `renv::restore()`.
4. **Coloque o CSV do Census em `data/raw/cfs_2022_pums.csv`.** Para conferir que é o arquivo
   certo, rode `tools::md5sum("data/raw/cfs_2022_pums.csv")`; o resultado deve ser
   `1acc7e53e869de3fffccc419495f6e8d`.
5. **Apague os arquivos do fluxo antigo, se ainda existirem:** `data/interim/amostra.csv`,
   `data/processed/amostra_dicio.csv` e `data/cfs.duckdb`. Eles eram do fluxo da amostra e não são mais usados.

**Passo 2 — Abrir a base**

```r
source("scripts/00_setup.R")
source(here::here("scripts", "02_trata_base.R"))

base <- obter_base_tratada(pipeline = FALSE)
```

Na primeira vez, a função monta a base e a grava em `data/processed/`, em uns 3,5 minutos. Nas vezes
seguintes, ela abre na hora. Para pular a montagem, copie o arquivo `data/processed/cfs_tratada.duckdb`
de quem já tem a base, com o mesmo commit e o mesmo CSV.

**Passo 3 — Consultar**

A `base` é "preguiçosa": as consultas rodam dentro do DuckDB, e só o `collect()` traz o resultado
para o R. **Nunca dê `collect()` na base inteira**, porque são 37,5 milhões de linhas. Resuma
primeiro, depois colete. Atenção: `group_by()` sozinho não resume nada; sem um `summarise()` ou
`count()` depois dele, o `collect()` traz todas as linhas e passa de 10 GB. Para espiar a base, use
`view(base)` (minúsculo, mostra 1.000 linhas) ou `base |> head(100) |> collect()`.

```r
# Seção 2 — dimensão: linhas da base e da população de uma análise
base |> count() |> collect()
base |> filter(!fl_exportacao, !fl_origem_suprimida) |> count() |> collect()

# Seção 3 — numérica, sem fator: mediana e quartis do peso
base |>
  summarise(n = n(),
            mediana = median(peso_t, na.rm = TRUE),
            q1 = quantile(peso_t, 0.25, na.rm = TRUE),
            q3 = quantile(peso_t, 0.75, na.rm = TRUE)) |>
  collect()

# Seção 3 — categórica, com o nome de cada código
dic <- ler_dicionario()
base |> count(MODE, sort = TRUE) |> collect() |> rotular(dic, "MODE")

# Totais para os EUA (usos do fator já validados): remessas, valor e toneladas
base |>
  group_by(MODE) |>
  summarise(remessas_estimadas = sum(WGT_FACTOR),
            valor_total        = sum(SHIPMT_VALUE * WGT_FACTOR),
            toneladas          = sum(peso_t * WGT_FACTOR)) |>
  collect()

fechar_base(base)   # sempre, ao terminar
```

**Passo 4 — Escolher a população de cada análise**

**Cada tabela ou gráfico precisa dizer quais linhas usa.** As marcações tornam isso uma linha de
`filter()`.

| Situação | O que fazer |
|---|---|
| Totais e distribuições gerais | Base completa, sem filtro |
| Cortes geográficos, como a P2 | Excluir `fl_exportacao` (o destino é o porto) e `fl_origem_suprimida` (a origem é desconhecida) |
| Origem por área CFS | As linhas `fl_origem_ma_suprimida` só têm o estado, não a área. Combine com o Gabriel se ficam à parte ou saem |
| Por mercadoria | As linhas `fl_sctg_colapsado` têm SCTG `00` ou uma faixa, não uma mercadoria única. Mostre-as como grupo à parte, ou exclua e diga isso |
| Valores extremos (`fl_valor_teto`, `fl_razao_atipica`) | Ficam nos totais. Em gráficos, pode valer mostrar com e sem eles |

Quando uma regra não estiver na tabela, decida com o Gabriel e acrescente a linha aqui.

**Passo 5 — Cuidados que o professor vai cobrar**

- **Ponderação:** todo uso novo do `WGT_FACTOR` é combinado com o Gabriel antes. Os já validados são
  a contagem de remessas, o valor total e o peso total, sempre em soma.
- **Com e sem o fator:** as distribuições por remessa aparecem nas duas versões, lado a lado.
- **Categorias pequenas:** junto da estimativa vai a confiabilidade (CV), assim que os limites forem
  definidos.
- **Gráficos:** precisam de título informativo, eixos com unidade, legenda e o nome das categorias
  (use `rotular()`), não o código.

**Passo 6 — Onde escrever e como entregar**

- **Um relatório por seção:** a sugestão é seguir o modelo da Seção 1, num arquivo
  `reports/secaoN_<tema>.qmd`, copiando o bloco `setup` do topo de `reports/secao1_qualidade.qmd`.
  Esse bloco carrega as funções, abre a base com `pipeline = FALSE` e define as funções de formatação
  de números. O bloco `fechar` do fim fecha a base.
- **Para gerar o HTML e o PDF:** `quarto render reports/secaoN_<tema>.qmd`.
- **Se mexer em `scripts/02_trata_base.R`:** rode os testes (`Rscript tests/testthat.R`). A base
  gravada será refeita sozinha na próxima chamada. Mudanças nas regras de tratamento são combinadas
  antes, porque alteram os números da Seção 1.
- **Ao concluir a seção:** acrescente um bloco para ela neste README, logo abaixo da Seção 1 e no
  mesmo formato:
  1. o resumo em uma frase;
  2. o que foi feito;
  3. as decisões e por quê;
  4. como continuar.

#### Seção 2 — Dimensão da base ✓

**A base tratada tem as mesmas 37.576.546 linhas do arquivo original e 29 colunas. Cada análise
usa uma de quatro "populações", e a tabela da seção diz quantas linhas e remessas cada uma tem.**

- **O que foi feito:** o script `scripts/03_secao2_dimensao.R` conta as linhas do arquivo e da base,
  separa as colunas em 18 originais, 2 novas (peso em toneladas e distância em km) e 9 marcações, e
  mede cada população.
- **As quatro populações:**

  | População | Quais linhas | Linhas | % das linhas |
  |---|---|---|---|
  | Completa | todas | 37.576.546 | 100% |
  | Geográfica | sem exportações e sem origem suprimida | 36.742.268 | 97,8% |
  | Mercadoria | sem SCTG colapsado | 37.477.912 | 99,7% |
  | Origem por área | com a área CFS de origem identificada | 37.572.550 | 99,99% |

- **Decisões e por quê:**
  - nenhuma linha é apagada: cada análise escolhe a população que faz sentido para a pergunta, e a
    tabela diz qual usou, como o professor pede;
  - na população "origem por área" saem as linhas com a metrópole de origem escondida pelo Census
    (decisão do Gabriel) e também as com o estado escondido, porque nelas a área também não aparece.
- **O que mudou:** as regras das populações ficaram num lugar só (`R/populacoes.R`). Antes, cada
  análise escreveria o próprio filtro.
- **Como continuar:** para usar uma população, `filtrar_populacao(base, "geografica")`.

#### Seção 3 — Descritiva das 8 variáveis ✓

**Peso, valor e distância são muito assimétricos: a remessa típica pesa 20 libras (9 kg), mas a
média passa de 6.800 libras. Por isso a seção mostra mediana e quartis, e não só a média.**

- **O que foi feito:** o script `scripts/04_secao3_descritiva.R` descreve as 5 variáveis do Check-in 1
  (origem, destino, mercadoria, modal e peso) e mais 3 (valor, distância e setor):
  - **numéricas:** quantidade, ausentes, média, desvio padrão, mínimo, quartis e máximo, na unidade
    original (libras, milhas) e na métrica (toneladas, km);
  - **categóricas:** as 10 categorias mais frequentes, com o nome de cada código, e o resto somado
    numa linha "Demais";
  - **categorias raras ou problemáticas:** SCTG escondido ou agregado em faixa, SCTG com menos de
    1.000 linhas e modal desconhecido;
  - **áreas CFS:** quantas existem, quanto as 10 maiores concentram e quanto é "restante do estado".
- **Decisões e por quê:**
  - cada medida aparece duas vezes, **sem o fator** (descreve a amostra) e **com o fator** (estima
    as remessas dos EUA). As duas contam histórias diferentes: com o fator, a remessa típica pesa
    só 3 libras, porque as encomendas pequenas são muitíssimas;
  - a descritiva usa a base completa, para mostrar também os códigos escondidos e agregados;
  - os quartis são calculados sem guardar a coluna inteira na memória, para caber em computador de
    4 GB.
- **Alguns números (base completa):**

  | | Mediana sem fator | Mediana com fator |
  |---|---|---|
  | Peso | 20 lb | 3 lb |
  | Valor | US$ 159 | US$ 37 |
  | Distância | 218 mi | 262 mi |

  Encomendas (`MODE` 21) são 44% das linhas e 59% das remessas estimadas.
- **O que mudou:** as medidas com fator usam as fórmulas aprovadas pelo Gabriel em 24/10/2026, todas
  dentro de somas.
- **Como continuar:** os resultados ficam em `obter_resultados(base, 3)$secao3`, com as tabelas
  `numericas`, `frequencias`, `maiores`, `raras` e `areas`.

#### Seção 4 — Distribuições em gráficos ✓

**Oito gráficos mostram como as variáveis se espalham. O principal recado: um terço das remessas
viaja menos de 100 km, mas outro terço passa de 1.000 km, e caminhão e encomenda somam mais de 90%
das linhas.**

- **O que foi feito:** o script `scripts/05_secao4_distribuicoes.R` calcula as tabelas no banco e
  desenha oito gráficos estáticos (ggplot), cada um respondendo a uma pergunta:
  - **peso e valor:** histogramas em escala log, porque vão de 1 a milhões;
  - **distância:** curva acumulada, com marcas em 100, 500 e 1.000 km;
  - **modal:** barras pelas 9 famílias de transporte;
  - **mercadoria:** as 15 com mais linhas, mais o grupo escondido ou agregado em faixa;
  - **setor:** barras por setor de quem envia;
  - **origem e destino:** as 20 áreas CFS com mais linhas.
- **Decisões e por quê:**
  - todo gráfico traz as duas versões lado a lado: **sem o fator** (a amostra) e **com o fator**
    (as remessas estimadas dos EUA);
  - o título de cada gráfico diz o que ele mostra, e o rodapé diz a população usada;
  - o gráfico de origem usa só as linhas com a área de origem identificada, e o de destino usa a
    população geográfica (sem exportações e sem origem escondida);
  - nada é sorteado: as faixas e os quantis vêm da base inteira, calculados no banco.
- **Alguns números:**

  | Distância em linha reta | % das linhas | % das remessas estimadas |
  |---|---|---|
  | Até 100 km | 33,4% | 26,6% |
  | De 100 a 500 km | 22,3% | 26,9% |
  | De 500 a 1.000 km | 14,2% | 15,6% |
  | Mais de 1.000 km | 30,1% | 30,9% |

  Caminhão tem 47,9% das linhas e 37,6% das remessas estimadas; encomenda tem 44,2% e 58,8%.
  Atacado envia 44,9% das linhas, mas é o varejo que soma mais remessas estimadas (50,9%).
- **O que mudou:** os gráficos ganharam um tema próprio (`R/graficos.R`), com cores testadas para
  daltonismo e títulos que quebram linha para não cortar no PDF.
- **Como continuar:** `graficos_secao4(obter_resultados(base, 4)$secao4)` devolve os oito gráficos.

#### Seção 5 — Relações entre variáveis ✓

**Cinco relações. A mais forte: 68% das toneladas estimadas não saem do estado onde a carga
nasceu, e em 48 dos 51 estados a maior fonte de carga é o próprio estado.**

- **O que foi feito:** o script `scripts/06_secao5_relacoes.R` cruza as variáveis duas a duas:
  1. **peso × distância:** mapa de calor em escala log e correlação de Pearson dos logs;
  2. **peso por família de modal:** caixas com quartis e percentis 10 e 90;
  3. **distância por família de modal:** idem;
  4. **modal × mercadoria:** quanto cada modal leva dentro de cada mercadoria, em linhas, remessas
     estimadas e toneladas estimadas;
  5. **origem × destino:** quanto das toneladas fica no próprio estado e, para cada destino, qual
     estado manda mais carga.
- **Decisões e por quê:**
  - as caixas são desenhadas a partir dos quantis calculados no banco, sem trazer as remessas para
    o R;
  - em toneladas, "sem fator" é a soma do peso das linhas da amostra, e "com fator" são as
    toneladas estimadas;
  - a correlação sai só **sem fator**: a versão com o fator é um uso novo da ponderação e espera a
    validação do Gabriel;
  - origem × destino usa a população geográfica (sem exportações e sem origem escondida), e modal ×
    mercadoria usa a população sem SCTG colapsado.
- **Alguns números:**
  - peso e distância têm correlação negativa fraca (Pearson dos logs = −0,32);
  - a remessa mediana pesa 173.872 lb por ferrovia e 3 lb por avião ou encomenda;
  - em toneladas, o caminhão lidera em todos os 16 grupos de mercadoria; em remessas estimadas, a
    encomenda lidera em 11;
  - só Distrito de Colúmbia, Connecticut e Delaware recebem mais toneladas de outro estado
    (Maryland, Nova Jersey e Nova Jersey) do que de si mesmos.
- **O que mudou:** os pedaços comuns aos gráficos (legenda sem/com fator e rodapé com a população)
  foram para `R/graficos.R`, para as Seções 4 e 5 usarem o mesmo texto.
- **Como continuar:** `graficos_secao5(obter_resultados(base, 5)$secao5)` devolve os cinco gráficos.

#### O relatório do Check-in 2 em PDF (em andamento)

**As Seções 1 a 5 já estão num PDF só, de 22 páginas, com todos os gráficos e na identidade visual
do grupo. Ele cresce a cada seção que fica pronta.**

- **O que foi feito:** o `reports/checkin2.qmd` junta as cinco seções num PDF:
  - **capa e "Em resumo":** seis números grandes, como ler "sem fator" e "com fator", e o sumário;
  - **uma página de abertura por seção**, com uma linha no rodapé que mostra em qual das 8 seções o
    leitor está;
  - **os gráficos** com o mesmo papel, fonte e cores do texto.
- **Decisões e por quê:**
  - **só PDF por enquanto:** a versão interativa vira o dashboard, num planejamento próprio;
  - **as fontes vão junto com o projeto** (`reports/fontes/`, com licença livre): o PDF sai igual em
    qualquer computador, sem instalar nada;
  - **cinza é contexto, cor é a história** (do livro *Storytelling with Data*): marinho para a
    amostra, laranja para as remessas estimadas, e nos modais só caminhão e encomenda ganham cor;
  - **o título de cada gráfico diz a conclusão**, e não só o que o gráfico mostra.
- **O que mudou:**
  - a Seção 1 tem agora um corpo só (`reports/_secao1_corpo.qmd`), usado tanto pelo relatório da
    Seção 1 quanto pelo PDF do check-in;
  - os títulos das tabelas, que o PDF da Seção 1 perdia, voltaram a aparecer.
- **Como continuar:** `quarto render reports/checkin2.qmd` gera o `reports/checkin2.pdf` em cerca de
  1,5 min. Quando uma das Seções 6 a 8 ficar pronta, ela entra no lugar da linha "Em construção" de
  "Próximos passos".

---

## 4. Instalação e uso

### 4.1 Instalar

**Em três passos: pacotes, o CSV do Census e, se for gerar relatórios, o Quarto.**

1. **Abra `commodity-flow-survey.Rproj` e rode `renv::restore()`.** No Linux, o `.Rprofile` usa os
   pacotes já compilados do Posit Package Manager, o que evita compilar cada pacote.
2. **Baixe a CFS 2022 PUMS** da página de *datasets* da CFS 2022 no site do Census e salve como
   `data/raw/cfs_2022_pums.csv`. O arquivo não vai para o Git, por causa do tamanho e da regra de
   dados brutos.
3. **Instale o [Quarto](https://quarto.org)**, só se for gerar relatórios. Confira o checksum do
   pacote baixado com o arquivo de checksums do mesmo *release*.

**Windows e macOS.** O código não depende do Linux, mas só foi medido nele. No Windows, o
`renv::restore()` baixa os pacotes já compilados do CRAN; no macOS, também, desde que o R seja o
instalador oficial do CRAN. Deixe folga no disco do projeto: a base gravada tem 1,3 GB, e a
montagem usa mais enquanto roda (o pico em disco ainda não foi medido). Na primeira vez, acompanhe a memória no Gerenciador de Tarefas (Windows) ou no Monitor de Atividade
(macOS): ela deve ficar abaixo de 1 GB para o R.

**Para rodar tudo de uma vez.** Depois de instalar, rode na pasta do projeto:

```bash
Rscript scripts/executar.R        # calcula todas as seções prontas
Rscript scripts/executar.R 2:5    # só as Seções 2 a 5
```

Ou abra `scripts/executar.R` no RStudio e clique em **Source**. O script carrega as peças na ordem
certa, guarda os resultados em `data/processed/resultados/` (na segunda vez, leva segundos) e fecha
a base no fim, mesmo se der erro.

### 4.2 `obter_base_tratada()` e `fechar_base()`

**Uma função abre a base tratada e a outra fecha. Toda abertura precisa de um fechamento.**

| Parâmetro | O que faz |
|---|---|
| `pipeline = FALSE` (padrão) | Usa `data/processed/cfs_tratada.duckdb` se estiver atual, inclusive uma cópia vinda de outro computador. Se faltar, ou se as regras ou o CSV mudaram, trata de novo e grava |
| `pipeline = TRUE` | Trata o CSV num arquivo temporário e não grava nada em `data/processed/` |

`fechar_base(base)` fecha a conexão com o banco e, no modo `TRUE`, apaga o arquivo temporário. Ela
também apaga a pasta de excedente da sessão em `data/interim/`.

### 4.3 As colunas novas da base tratada

**As 18 colunas originais ficam como estão. Somam-se 2 colunas em unidades métricas e 9 marcações
de sim/não.**

| Coluna | Regra | Linhas marcadas |
|---|---|---|
| `peso_t` | `SHIPMT_WGHT` em toneladas métricas | — |
| `dist_km` | `SHIPMT_DIST_GC` em quilômetros | — |
| `fl_sctg_colapsado` | SCTG `00` ou faixa agregada | 98.634 |
| `fl_origem_ma_suprimida` | metrópole de origem escondida, estado visível | 1.589 |
| `fl_origem_suprimida` | origem totalmente escondida (`00-00000`) | 2.407 |
| `fl_exportacao` | `EXPORT_YN = Y` | 831.876 |
| `fl_intra_area` | origem e destino na mesma área CFS | 11.587.026 |
| `fl_valor_teto` | valor ≥ US$ 30.000.000 | 79 |
| `fl_fator_teto` | `WGT_FACTOR` ≥ 3.588.000 | 469 |
| `fl_fator_menor_1` | `WGT_FACTOR` < 1 | 7.220 |
| `fl_razao_atipica` | razão valor/peso fora dos percentis 0,1% e 99,9% da própria mercadoria | 74.554 |

As colunas originais mantêm o nome do arquivo, em maiúsculas. As novas ficam em minúsculas, e as
marcações começam com `fl_`.

### 4.4 Nomes dos códigos (rótulos)

**A base guarda só códigos. Para mostrar nomes, aplique o dicionário depois de resumir.**

```r
dic <- ler_dicionario()
base |> count(SCTG) |> collect() |> rotular(dic, "SCTG")
```

`ORIG_MA`, `DEST_MA` e `SHIPMT_ID` não têm nome no dicionário. Para o nome de uma área, use
`ORIG_CFS_AREA` ou `DEST_CFS_AREA`.

### 4.5 Relatórios e testes

```bash
quarto render reports/checkin2.qmd           # o PDF do Check-in 2 (Seções 1 a 5), em cerca de 1,5 min
quarto render reports/secao1_qualidade.qmd   # só a Seção 1, em .html e .pdf, em cerca de 2,5 min
Rscript tests/testthat.R                     # 289 verificações, em cerca de 1 min
```

O `reports/.Rprofile` ativa o renv do projeto durante a geração do relatório.

### 4.6 Pacotes (renv)

| Situação | Comando |
|---|---|
| Clonou o repositório ou o `renv.lock` mudou | `renv::restore()` |
| Instalou um pacote novo | `renv::install("pacote")` e depois `renv::snapshot()` |
| Conferir se o lockfile e a biblioteca batem | `renv::status()` |

### 4.7 Banco de dados externo (opcional)

**O tratamento não precisa disso. É só para quem quiser usar um Postgres.**

O destino é escolhido pelo ambiente: `R_CONFIG_ACTIVE` no `.Renviron` e os blocos do `config.yml`.
Copie `.Renviron.example` para `.Renviron` e preencha as variáveis `CFS_DB_*`. O `.Renviron` não vai
para o Git.

---

## 5. Glossário

| Termo | O que quer dizer |
|---|---|
| **CFS** | *Commodity Flow Survey*, a pesquisa de fluxos de carga dos EUA |
| **PUMS** | A versão pública, com microdados sorteados e protegidos por sigilo |
| **Remessa** | Um envio de carga com um único destino |
| **`WGT_FACTOR` / fator de ponderação** | Quantas remessas reais uma linha representa. Não confundir com o peso da carga |
| **Área CFS** | Uma região de origem ou de destino: o estado mais a metrópole (ex.: `24-12580`, Baltimore) ou o "restante do estado" (`NN-99999`) |
| **SCTG** | O código do tipo de mercadoria (ex.: `14` = minérios metálicos) |
| **Modal (`MODE`)** | O meio de transporte (ex.: `21` = encomenda/courier, `111` = caminhão terceirizado) |
| **Colapsamento** | Troca de um detalhe que identificaria a empresa por algo mais genérico, para proteger o sigilo |
| **Teto (*topcoding*)** | Limite máximo aplicado a valores muito altos, também por sigilo |
| **FOB** | Valor da mercadoria na porta de quem vende, sem frete e sem impostos |
| **Marcação (flag, `fl_*`)** | Coluna de sim/não que identifica um caso especial sem apagar a linha |
| **DuckDB** | Banco de dados que roda dentro do R e consulta arquivos grandes direto do disco |
| **Tabela preguiçosa (*lazy*)** | Tabela que só é calculada quando o resultado é pedido com `collect()` |
| **Hash** | Uma "impressão digital" de um arquivo: muda se qualquer caractere do arquivo mudar |
| **CV** | Coeficiente de variação. Mede quão incerta é uma estimativa; quanto maior, menos confiável |
| **GVF** | Método oficial do guia para calcular o CV sem refazer o sorteio |

---

## 6. Fontes

**As regras deste projeto se apoiam nos documentos oficiais do Census, guardados em
`docs/referencias/`.**

- **2022 CFS PUMS Data Users Guide (Census Bureau):** `docs/referencias/guia_cfs_2022.pdf`.
  - Fonte das medidas de sigilo, dos tetos, do fator de ponderação e da checagem de Maryland.
- **Dicionário de dados oficial:** `data/external/dicionario.xlsx`.
  - O arquivo `docs/referencias/dicionario_original.xlsx` é guardado como referência histórica.
- **2022 CFS Methodology (Census Bureau):** o desenho da pesquisa, usado na justificativa das
  duplicações e da razão valor/peso.
- **Página da CFS 2022 no Census:** o download do PUMS, o escopo da pesquisa e as tabelas publicadas.

---

## Como este README é mantido

**A cada seção concluída e a cada check-in, este README ganha um bloco novo na
[linha do tempo](#3-linha-do-tempo-das-entregas).**

Cada bloco segue o mesmo formato:
1. o resumo em uma frase;
2. o que foi feito;
3. as decisões e por quê;
4. o que mudou em relação ao que veio antes;
5. como continuar.
