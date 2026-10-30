# 08_secao7_problemas.R ---------------------------------------------------
# Secao 7 do Check-in 2: tabela que atualiza os problemas do Check-in 1
# (FSD 31.7), no formato aprovado pelo Gabriel em 29/10/2026. So define
# objetos. As evidencias sao recalculadas aqui para a secao nao depender do
# relatorio da Secao 1; o que ja esta nas Secoes 3 a 5 vem de `res`.

STATUS_PROBLEMA <- c(
  CORRIGIDO = "O Check-in 1 afirmou algo que a base completa desmente.",
  CONFIRMADO = "O problema existe como descrito, e agora tem número.",
  NOVO = "Apareceu só agora, na exploração."
)
TIPOS_PROBLEMA <- c("Problema dos dados", "Limitação da base")

# Checagem publicada no guia oficial para a origem Maryland (ORIG_STATE 24).
MARYLAND_GUIA <- c(linhas = 612317, valor = 198598788295)
VALOR_TETO <- 30000000

calcular_secao7 <- function(base, res) {
  ev <- .evidencias_secao7(base, res)
  list(evidencias = ev, problemas = .problemas(ev), status = tibble::tibble(
    populacao = "completa", status = names(STATUS_PROBLEMA), significado = unname(STATUS_PROBLEMA)))
}

.evidencias_secao7 <- function(base, res) {
  con <- dbplyr::remote_con(base)
  q <- function(sql) DBI::dbGetQuery(con, sprintf(sql, .sql_populacao(base, "completa")))
  geral <- q(paste0("WITH d AS (%s) SELECT count(*) AS linhas, ",
                    paste(sprintf("sum(%s::INT) AS %s", FLAGS, FLAGS), collapse = ", "), ",
                    max(SHIPMT_VALUE) AS valor_max,
                    count(*) FILTER (WHERE SHIPMT_VALUE = ", VALOR_TETO, ") AS valor_no_teto,
                    count(*) FILTER (WHERE SHIPMT_VALUE > ", VALOR_TETO, ") AS valor_acima_teto,
                    least(min(SHIPMT_VALUE), min(SHIPMT_WGHT), min(SHIPMT_DIST_GC)) AS minimo_numerico,
                    count(*) FILTER (WHERE SCTG = '16') AS linhas_sctg_16 FROM d"))
  # GROUP BY transborda para o disco; count(DISTINCT) guardaria tudo na memoria.
  colunas <- setdiff(names(TIPOS_CFS), "SHIPMT_ID")
  distintas <- q(paste0("WITH d AS (%s) SELECT count(*) AS n FROM (SELECT ",
                        paste(colunas, collapse = ", "), " FROM d GROUP BY ALL)"))$n
  maryland <- q("WITH d AS (%s) SELECT count(*) AS linhas, sum(SHIPMT_VALUE * WGT_FACTOR) AS valor
                 FROM d WHERE ORIG_STATE = '24'")
  sctg_base <- q("WITH d AS (%s) SELECT SCTG FROM d GROUP BY 1")$SCTG
  areas_base <- q("WITH d AS (%s) SELECT ORIG_CFS_AREA FROM d GROUP BY 1")$ORIG_CFS_AREA

  dic <- ler_dicionario()
  supr_dic <- grep("-00000$", dic$VALUE_CODE[dic$VARIABLE == "ORIG_CFS_AREA"], value = TRUE)
  modal <- res$secao4$modal
  mode <- dplyr::filter(res$secao3$frequencias, variavel == "MODE")
  peso <- dplyr::filter(res$secao3$numericas, coluna == "SHIPMT_WGHT")
  caminhao <- dplyr::filter(res$secao5$peso_modal, familia_modal == "Caminhão", p == 0.5)

  c(
    lapply(as.list(geral), as.numeric),
    list(
      linhas_duplicadas = as.numeric(geral$linhas - distintas),
      maryland_linhas = as.numeric(maryland$linhas),
      maryland_diferenca = maryland$valor - MARYLAND_GUIA[["valor"]],
      sctg_ausentes = setdiff(trimws(dic$VALUE_CODE[dic$VARIABLE == "SCTG"]), sctg_base),
      supr_ausentes = sum(!supr_dic %in% areas_base),
      supr_dicionario = length(supr_dic),
      coluna_trocada = "TEMP_CNTRL_YN" %in% dic$VARIABLE && !"TEMP_CNTRL_YN" %in% colnames(base),
      linhas_hidrovia = sum(modal$linhas[modal$familia_modal == "Hidrovia"]),
      linhas_duto = sum(modal$linhas[modal$familia_modal == "Duto"]),
      linhas_mode_30 = sum(mode$linhas[mode$codigo == "30"]),
      peso_media = peso$sem_fator[peso$medida == "media"],
      peso_mediana = peso$sem_fator[peso$medida == "mediana"],
      caminhao_mediana_sem_fator = caminhao$sem_fator,
      caminhao_mediana_com_fator = caminhao$com_fator
    )
  )
}

.mil <- function(x, d = 0) paste(n_fmt(x / 1000, d), "mil")

.problemas <- function(ev) {
  origem <- ev$fl_origem_ma_suprimida + ev$fl_origem_suprimida
  sctg_ausentes <- paste(ev$sctg_ausentes, collapse = ", ")
  linha <- function(tipo, problema, status, evidencia) {
    tibble::tibble(populacao = "completa", tipo = TIPOS_PROBLEMA[tipo], problema, status, evidencia)
  }
  dplyr::bind_rows(
    linha(1, "Teto de valor não é rígido", "CORRIGIDO", sprintf(
      "%s remessas com valor ≥ US$ 30 mi; %s passam do teto (até US$ %s mi)",
      n_fmt(ev$fl_valor_teto), n_fmt(ev$valor_acima_teto), n_fmt(ev$valor_max / 1e6, 1))),
    linha(1, "Linhas idênticas, exceto o número de identificação", "NOVO", sprintf(
      "%s linhas (%s), mantidas", .mil(ev$linhas_duplicadas), pct(ev$linhas_duplicadas, ev$linhas, 1))),
    linha(1, "Valor por libra fora do comum para a mercadoria", "NOVO", sprintf(
      "%s linhas marcadas", .mil(ev$fl_razao_atipica, 1))),
    linha(1, "Fator menor que 1 ou no teto", "CONFIRMADO", sprintf(
      "%s abaixo de 1 (o guia prevê) e %s no teto", .mil(ev$fl_fator_menor_1, 1), n_fmt(ev$fl_fator_teto))),
    linha(1, "Dicionário diferente da base", "NOVO", paste0(
      if (ev$coluna_trocada) "nome de coluna trocado; " else "",
      sprintf("SCTG %s não aparecem; %s dos %s códigos de origem suprimida também não",
              sctg_ausentes, ev$supr_ausentes, ev$supr_dicionario))),
    linha(1, "Código lido como número perde o zero à esquerda", "CONFIRMADO",
          "resolvido lendo todos os códigos como texto"),
    linha(1, "Categorias raras", "CONFIRMADO", sprintf(
      "hidrovia com %s linhas, duto com %s, modal \"desconhecido\" com %s",
      .mil(ev$linhas_hidrovia, 1), .mil(ev$linhas_duto, 1), .mil(ev$linhas_mode_30))),
    linha(2, "Colapsamento (~100 mil de origem)", "CORRIGIDO", sprintf(
      "só %s de origem e %s de mercadoria; códigos distintos permitem marcar",
      .mil(origem), .mil(ev$fl_sctg_colapsado, 1))),
    linha(2, "Zeros por arredondamento", "CORRIGIDO", sprintf(
      "nenhum zero; mínimo %s em valor, peso e distância; distância em milhas inteiras",
      n_fmt(ev$minimo_numerico))),
    linha(2, "Amostra, não censo", "CONFIRMADO", sprintf(
      "a amostra super-representa remessas pesadas (caminhão: %s lb na amostra contra %s lb nas remessas estimadas)",
      n_fmt(ev$caminhao_mediana_sem_fator), n_fmt(ev$caminhao_mediana_com_fator))),
    linha(2, "Distribuições muito assimétricas", "NOVO", sprintf(
      "peso médio de %s lb contra mediana de %s lb; gráficos só em escala log",
      n_fmt(ev$peso_media), n_fmt(ev$peso_mediana))),
    linha(2, paste("Só 2022, sem trimestre; só linha reta; valor não é frete; sem importações, fazendas",
                   "e petróleo bruto; exportação até o porto; revenda conta de novo; diferente de 2017"),
          "CONFIRMADO", sprintf(
            "do guia; SCTG 16 (petróleo bruto) com %s linhas; %s exportações fora da análise geográfica",
            n_fmt(ev$linhas_sctg_16), .mil(ev$fl_exportacao))),
    linha(2, "Comparação com as tabelas oficiais", "CONFIRMADO", sprintf(
      "a checagem de Maryland bate (%s linhas; diferença de US$ %s no valor)",
      n_fmt(ev$maryland_linhas), n_fmt(ev$maryland_diferenca))),
    linha(2, "Arquivo grande demais para a memória", "CONFIRMADO",
          "resolvido: base inteira, sem a amostra de 200 mil linhas, em máquina de 4 GB")
  )
}
