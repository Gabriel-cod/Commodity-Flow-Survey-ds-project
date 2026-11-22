# Variaveis, niveis e referencias da modelagem do Check-in 3 (FSD 32.2 e 32.3).

# Exclusoes da ata de 15/11: SCTG colapsado e modal desconhecido.
REGRA_MODELAGEM <- "NOT fl_sctg_colapsado AND MODE <> '30'"

SETORES <- c(
  "21" = "Mineração e extração",
  "31-33" = "Indústria de transformação",
  "42" = "Comércio atacadista",
  "44-45" = "Comércio varejista",
  "48-49" = "Transporte e armazenagem",
  "51" = "Informação",
  "55" = "Gestão de empresas"
)

# Ferrovia, hidrovia, duto e outros juntos: separados teriam ~280, 15, 29 e
# 0 linhas na amostra de 100 mil (decisao do grupo, 21/11/2026).
MODAL_AGRUPADO <- "Ferrovia, hidrovia, duto e outros"
MODAIS <- c(
  "111" = "Caminhão", "112" = "Caminhão", "113" = "Retirada pelo cliente",
  "12" = MODAL_AGRUPADO, "131" = MODAL_AGRUPADO, "132" = MODAL_AGRUPADO, "133" = MODAL_AGRUPADO,
  "14" = "Aéreo", "15" = MODAL_AGRUPADO, "16" = MODAL_AGRUPADO, "21" = "Encomenda",
  "22" = "Multimodal", "23" = "Multimodal", "24" = "Multimodal", "25" = "Multimodal"
)

# Evento da Parte B: caminhao proprio ou contratado; a retirada (113) conta
# como nao, porque o veiculo e do cliente e desconhecido (ata).
MODOS_CAMINHAO <- c("111", "112")
SIM_NAO <- c("Não", "Sim")

# Referencia = categoria mais frequente (decisao de 22/11); fica no 1o nivel.
REFERENCIAS <- c(setor = "Comércio atacadista", modal = "Caminhão")

# SQL de uma linha por remessa da populacao de modelagem. O WGT_FACTOR vai
# junto so para o ajuste ponderado de comparacao (validado em 22/11).
sql_modelagem <- function(base) {
  sprintf("
    SELECT ln(SHIPMT_VALUE) AS ln_valor, ln(SHIPMT_WGHT) AS ln_peso,
           ln(dist_km) AS ln_dist, dist_km,
           SECTOR AS setor, %s AS modal,
           CASE WHEN HAZMAT IN ('H', 'P') THEN 'Sim' ELSE 'Não' END AS perigosa,
           CASE WHEN TEMP_CNTL_YN = 'Y' THEN 'Sim' ELSE 'Não' END AS temperatura,
           CASE WHEN MODE IN (%s) THEN 'Sim' ELSE 'Não' END AS caminhao,
           WGT_FACTOR
    FROM (%s) WHERE %s",
    .sql_case("MODE", MODAIS), paste0("'", MODOS_CAMINHAO, "'", collapse = ", "),
    dbplyr::sql_render(base), REGRA_MODELAGEM)
}

.sql_case <- function(coluna, mapa) {
  paste("CASE", paste(sprintf("WHEN %s = '%s' THEN '%s'", coluna, names(mapa), mapa), collapse = " "), "END")
}

# Niveis fixos com a referencia primeiro: categoria rara fora da amostra
# continua nos graficos e o lm sempre compara com o mesmo nivel.
fatorar_modelagem <- function(dados) {
  com_ref <- function(niveis, ref) c(ref, setdiff(niveis, ref))
  dplyr::mutate(dados,
    setor = factor(unname(SETORES[setor]), com_ref(SETORES, REFERENCIAS[["setor"]])),
    modal = factor(modal, com_ref(unique(MODAIS), REFERENCIAS[["modal"]])),
    perigosa = factor(perigosa, SIM_NAO),
    temperatura = factor(temperatura, SIM_NAO),
    caminhao = factor(caminhao, SIM_NAO)
  )
}

# GVIF de Fox & Monette (1992): det(R11) det(R22) / det(R), com R a matriz de
# correlacao das colunas do modelo sem intercepto. GVIF^(1/(2 gl)) e
# comparavel a raiz do VIF de um preditor numerico.
gvif <- function(modelo) {
  x <- model.matrix(modelo)[, -1, drop = FALSE]
  atrib <- attr(model.matrix(modelo), "assign")[-1]
  r <- cor(x)
  det_r <- det(r)
  termos <- attr(terms(modelo), "term.labels")
  purrr::map_dfr(seq_along(termos), function(i) {
    j <- atrib == i
    valor <- det(r[j, j, drop = FALSE]) * det(r[!j, !j, drop = FALSE]) / det_r
    tibble::tibble(preditor = termos[i], gl = sum(j), gvif = valor,
                   gvif_ajustado = valor^(1 / (2 * sum(j))))
  })
}

# Reservatorio com semente no DuckDB; uma thread so para a semente valer.
sortear_modelagem <- function(con, sql, tamanho = cfg("modelagem", "tamanho_amostra"),
                              semente = cfg("modelagem", "semente")) {
  threads <- DBI::dbGetQuery(con, "SELECT current_setting('threads') AS t")$t
  DBI::dbExecute(con, "SET threads = 1")
  on.exit(DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(threads))))
  a <- DBI::dbGetQuery(con, sprintf("SELECT * FROM (%s) USING SAMPLE reservoir(%d ROWS) REPEATABLE (%d)",
                                    sql, as.integer(tamanho), as.integer(semente)))
  fatorar_modelagem(tibble::as_tibble(a))
}
