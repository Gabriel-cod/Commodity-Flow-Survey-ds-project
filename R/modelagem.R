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
# continua nos graficos e o lm sempre compara com o mesmo nivel. Recebe o
# setor ja com o nome em portugues (perfis) ou, em fatorar_modelagem(), o codigo.
niveis_modelagem <- function(d) {
  com_ref <- function(niveis, ref) c(ref, setdiff(niveis, ref))
  d |>
    dplyr::mutate(setor = factor(setor, com_ref(SETORES, REFERENCIAS[["setor"]])),
                  modal = factor(modal, com_ref(unique(MODAIS), REFERENCIAS[["modal"]]))) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("perigosa", "temperatura", "caminhao")), ~ factor(.x, SIM_NAO)))
}

fatorar_modelagem <- function(dados) {
  niveis_modelagem(dplyr::mutate(dados, setor = unname(SETORES[setor])))
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

# Nome legivel de cada termo do broom::tidy() para as tabelas do PDF.
rotular_termo <- function(termo) {
  r <- termo
  r <- sub("^\\(Intercept\\)$", "Intercepto", r)
  r <- gsub("ln_peso", "ln(peso)", r)
  r <- gsub("ln_dist", "ln(distância)", r)
  r <- gsub("ln_valor", "ln(valor)", r)
  r <- sub("^modal(.+)$", "Modal: \\1", r)
  r <- sub("^setor(.+)$", "Setor: \\1", r)
  r <- sub(":setor", " × setor: ", r)
  r <- sub("^perigosaSim$", "Perigosa: sim", r)
  r <- sub("^temperaturaSim$", "Temperatura controlada: sim", r)
  r
}

# Escala log -> % na escala original: exp(b) - 1.
efeito_pct <- function(b) 100 * (exp(b) - 1)

# --- Graficos da modelagem ---------------------------------------------------

rodape_modelagem <- function(base_modelagem, parte = c("treino", "teste")) {
  parte <- match.arg(parte)
  m <- base_modelagem$modelagem
  sprintf("Fonte: CFS 2022 PUMS. %s: %s linhas da amostra de modelagem (%s linhas, semente %d), sem fator.",
          if (parte == "treino") "Treino" else "Teste", rotulo_num()(nrow(base_modelagem[[parte]])),
          rotulo_num()(m$tamanho_amostra), m$semente)
}

# passo = 2 para paineis estreitos (facetas): 1, 100, 10 mil, 1 mi.
eixo_peso <- function(passo = 1) {
  curto <- function(b) {
    v <- round(exp(b))
    ifelse(v >= 1e6, paste(v / 1e6, "mi"), ifelse(v >= 1e4, paste(v / 1e3, "mil"), rotulo_num()(v)))
  }
  ggplot2::scale_x_continuous(breaks = log(10^seq(0, 6, passo)), labels = if (passo > 1) curto else function(b) rotulo_num()(round(exp(b))))
}

# Afasta rotulos de fim de linha que se sobrepoem, mantendo a ordem vertical.
espalhar <- function(y, folga) {
  o <- order(y)
  z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + folga)
  z <- z - mean(z - y[o])
  y[o] <- z
  y
}

eixo_valor <- function(nome = "Valor da remessa (US$, escala log)") {
  ggplot2::scale_y_continuous(nome, breaks = log(10^(0:7)), labels = function(b) paste0("US$ ", rotulo_num()(round(exp(b)))))
}

# Media de y em 20 faixas de igual contagem de x: mostra curvas que a reta
# esconde, sem o custo de memoria de um suavizador.
medias_faixa <- function(d, x, y) {
  d |>
    dplyr::mutate(faixa = dplyr::ntile(.data[[x]], 20)) |>
    dplyr::group_by(faixa) |>
    dplyr::summarise(x = mean(.data[[x]]), y = mean(.data[[y]]), .groups = "drop")
}
