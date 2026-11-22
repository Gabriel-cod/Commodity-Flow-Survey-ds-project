# 20_checkin3_exploracao_linear.R -----------------------------------------
# Check-in 3, Parte A, opcao 1 (resposta = ln do valor): exploracao dos
# preditores antes do modelo. Mede (1) a associacao de cada preditor com a
# resposta e (2) a associacao dos preditores entre si (independencia), e
# grava um grafico de dispersao por par preditor x resposta.
#
# Tudo sem fator de ponderacao (ata de 15/11: modelos sem ponderacao).
# Associacoes na populacao inteira via DuckDB; dispersao e GVIF numa amostra.
#
# Uso:  Rscript scripts/20_checkin3_exploracao_linear.R

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "02_trata_base.R"))

# Seed e tamanho ficam aqui ate a base de modelagem ir para o config.yml.
SEMENTE_AMOSTRA <- 2026
TAMANHO_AMOSTRA <- 100000

PASTA_FIG <- here::here("outputs", "figures", "checkin3")
PASTA_TAB <- here::here("outputs", "tables", "checkin3")

# Exclusoes propostas na ata (SCTG colapsado e modal desconhecido).
REGRA_POPULACAO <- "NOT fl_sctg_colapsado AND MODE <> '30'"

SETORES <- c(
  "21" = "Mineração e extração",
  "31-33" = "Indústria de transformação",
  "42" = "Comércio atacadista",
  "44-45" = "Comércio varejista",
  "48-49" = "Transporte e armazenagem",
  "51" = "Informação",
  "55" = "Gestão de empresas"
)

# Familias de modal do Check-in 2, sem "desconhecido" (excluido acima).
# Ferrovia, hidrovia, duto e outros juntos: separados teriam ~280, 15, 29 e
# 0 linhas na amostra de 100 mil (decisao do grupo, 21/11/2026).
MODAL_AGRUPADO <- "Ferrovia, hidrovia, duto e outros"
MODAIS <- c(
  "111" = "Caminhão", "112" = "Caminhão", "113" = "Retirada pelo cliente",
  "12" = MODAL_AGRUPADO, "131" = MODAL_AGRUPADO, "132" = MODAL_AGRUPADO, "133" = MODAL_AGRUPADO,
  "14" = "Aéreo", "15" = MODAL_AGRUPADO, "16" = MODAL_AGRUPADO, "21" = "Encomenda",
  "22" = "Multimodal", "23" = "Multimodal", "24" = "Multimodal", "25" = "Multimodal"
)

PREDITORES <- c(ln_peso = "ln(peso)", ln_dist = "ln(distância)", dist_km = "Distância (km)",
                setor = "Setor", modal = "Modal", perigoso = "Periculosidade",
                temperatura = "Controle de temperatura")
NUMERICOS <- c("ln_peso", "ln_dist", "dist_km")
CATEGORICOS <- c("setor", "modal", "perigoso", "temperatura")

# SQL das colunas derivadas; HAZMAT H e P viram "perigoso" (ata, ponto 5).
.sql_variaveis <- function(base) {
  sprintf("
    SELECT ln(SHIPMT_VALUE) AS ln_valor, ln(SHIPMT_WGHT) AS ln_peso,
           ln(dist_km) AS ln_dist, dist_km,
           SECTOR AS setor, %s AS modal,
           CASE WHEN HAZMAT IN ('H', 'P') THEN 'Sim' ELSE 'Não' END AS perigoso,
           CASE WHEN TEMP_CNTL_YN = 'Y' THEN 'Sim' ELSE 'Não' END AS temperatura
    FROM (%s) WHERE %s",
    .sql_case("MODE", MODAIS), dbplyr::sql_render(base), REGRA_POPULACAO)
}

.sql_case <- function(coluna, mapa) {
  paste("CASE", paste(sprintf("WHEN %s = '%s' THEN '%s'", coluna, names(mapa), mapa), collapse = " "), "END")
}

# --- Associacoes na populacao inteira ---------------------------------------

# Pearson e assimetria das numericas; ln(dist) x dist decide a escala.
numericas_populacao <- function(con, sql) {
  DBI::dbGetQuery(con, sprintf("
    SELECT count(*) AS n,
           corr(ln_valor, ln_peso) AS r_ln_peso,
           corr(ln_valor, ln_dist) AS r_ln_dist,
           corr(ln_valor, dist_km) AS r_dist_km,
           corr(ln_peso, ln_dist) AS r_peso_ln_dist,
           corr(ln_peso, dist_km) AS r_peso_dist_km,
           skewness(ln_valor) AS assim_ln_valor, skewness(ln_peso) AS assim_ln_peso,
           skewness(dist_km) AS assim_dist_km, skewness(ln_dist) AS assim_ln_dist
    FROM (%s)", sql))
}

# eta^2 = SQ entre grupos / SQ total (R^2 da ANOVA de um fator); eta e
# comparavel a |r|. Somas por grupo no DuckDB, o resto no R.
eta2 <- function(con, sql, y, grupo) {
  g <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS g, count(*) AS n, avg(%s) AS media, var_pop(%s) AS var FROM (%s) GROUP BY 1",
    grupo, y, y, sql))
  n <- sum(g$n)
  media <- sum(g$n * g$media) / n
  sq_entre <- sum(g$n * (g$media - media)^2)
  sq_total <- sq_entre + sum(g$n * g$var)
  sq_entre / sq_total
}

# V de Cramer a partir da tabela de contingencia completa.
v_cramer <- function(con, sql, a, b) {
  t <- DBI::dbGetQuery(con, sprintf("SELECT %s AS a, %s AS b, count(*) AS n FROM (%s) GROUP BY 1, 2",
                                    a, b, sql))
  m <- xtabs(n ~ a + b, data = t)
  esperado <- outer(rowSums(m), colSums(m)) / sum(m)
  qui2 <- sum((m - esperado)^2 / esperado)
  sqrt(qui2 / (sum(m) * (min(dim(m)) - 1)))
}

# Media e quartis de ln(valor) por categoria, para a tabela e os graficos.
# Quartis aproximados (t-digest): os exatos furam o limite de memoria do
# DuckDB (pico de 2 GB) e aqui so desenham a barra do grafico.
resumo_por_categoria <- function(con, sql, grupo) {
  DBI::dbGetQuery(con, sprintf("
    SELECT %s AS categoria, count(*) AS n, avg(ln_valor) AS media,
           approx_quantile(ln_valor, 0.25) AS q1, approx_quantile(ln_valor, 0.5) AS mediana,
           approx_quantile(ln_valor, 0.75) AS q3
    FROM (%s) GROUP BY 1 ORDER BY 1", grupo, sql)) |>
    dplyr::mutate(variavel = grupo, n = as.numeric(n), .before = 1)
}

# Matriz de associacao entre preditores (sem dist_km, que sai pela escala).
matriz_associacao <- function(con, sql, num) {
  vars <- c("ln_peso", "ln_dist", CATEGORICOS)
  pares <- t(combn(vars, 2))
  medir <- function(a, b) {
    if (all(c(a, b) %in% NUMERICOS)) return(c(abs(num$r_peso_ln_dist), NA))
    if (a %in% NUMERICOS) return(c(sqrt(eta2(con, sql, a, b)), NA))
    c(NA, v_cramer(con, sql, a, b))
  }
  r <- t(mapply(medir, pares[, 1], pares[, 2]))
  tibble::tibble(a = pares[, 1], b = pares[, 2],
                 medida = ifelse(!is.na(r[, 1]) & pares[, 2] %in% NUMERICOS, "|r| de Pearson",
                                 ifelse(!is.na(r[, 1]), "eta (razão de correlação)", "V de Cramér")),
                 valor = dplyr::coalesce(r[, 1], r[, 2]))
}

# --- Amostra, GVIF e graficos ------------------------------------------------

# Reservatorio com semente; uma thread so para a semente valer.
sortear_amostra <- function(con, sql) {
  DBI::dbExecute(con, "SET threads = 1")
  on.exit(DBI::dbExecute(con, sprintf("SET threads = %d", THREADS_CONSULTA)))
  a <- DBI::dbGetQuery(con, sprintf("SELECT * FROM (%s) USING SAMPLE reservoir(%d ROWS) REPEATABLE (%d)",
                                    sql, TAMANHO_AMOSTRA, SEMENTE_AMOSTRA))
  # Niveis fixos: categoria rara que nao cai na amostra continua no grafico;
  # Caminhao (o mais comum) fica como referencia.
  a |>
    dplyr::mutate(setor = factor(SETORES[setor], SETORES),
                  modal = factor(modal, unique(MODAIS)),
                  perigoso = factor(perigoso, c("Não", "Sim")),
                  temperatura = factor(temperatura, c("Não", "Sim")))
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

.salvar <- function(grafico, nome, largura = 7, altura = 4.6) {
  ggplot2::ggsave(file.path(PASTA_FIG, paste0(nome, ".png")), grafico, width = largura, height = altura,
                  dpi = 200, device = ragg::agg_png, bg = COR[["papel"]])
}

rotulo_dolar <- function(x) paste0("US$ ", rotulo_num()(x))
EIXO_VALOR <- ggplot2::scale_y_continuous(
  "Valor da remessa (escala log)", breaks = log(10^(0:7)),
  labels = function(b) rotulo_dolar(round(exp(b))))

rodape <- function(n_pop) {
  sprintf(paste("Fonte: CFS 2022 PUMS. Pontos: amostra aleatória de %s linhas (semente %d) de %s",
                "(sem SCTG colapsado e sem modal desconhecido), sem fator."),
          rotulo_num()(TAMANHO_AMOSTRA), SEMENTE_AMOSTRA, rotulo_num()(n_pop))
}

# Media de ln(valor) em 20 faixas de igual contagem de x: mostra curvas que a
# reta esconde, sem o custo de memoria de um suavizador.
medias_por_faixa <- function(a, x) {
  a |>
    dplyr::mutate(faixa = dplyr::ntile(.data[[x]], 20)) |>
    dplyr::group_by(faixa) |>
    dplyr::summarise(x = mean(.data[[x]]), y = mean(ln_valor), .groups = "drop")
}

grafico_numerico <- function(a, x, r, titulo, eixo_x, rotulo_x, n_pop) {
  ggplot2::ggplot(a, ggplot2::aes(.data[[x]], ln_valor)) +
    ggplot2::geom_point(alpha = 0.04, size = 0.5, colour = COR[["marinho"]], shape = 16) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = COR[["laranja"]], linewidth = 0.8, se = FALSE) +
    ggplot2::geom_line(data = medias_por_faixa(a, x), ggplot2::aes(x, y), colour = COR[["secundario"]],
                       linewidth = 0.6, linetype = "22") +
    ggplot2::geom_point(data = medias_por_faixa(a, x), ggplot2::aes(x, y), colour = COR[["secundario"]], size = 1.2) +
    EIXO_VALOR + rotulo_x +
    rotulos_cfs(titulo, sprintf("Pearson na população = %s. Laranja: reta de mínimos quadrados; tracejado: média de ln(valor) em 20 faixas de x.",
                                n_fmt(r, 3)),
                rodape(n_pop), x = eixo_x) +
    tema_cfs()
}

grafico_categorico <- function(a, x, resumo, eta, titulo, eixo_x, n_pop) {
  resumo <- dplyr::filter(resumo, variavel == x) |>
    dplyr::mutate(categoria = factor(.rotular_categoria(x, categoria), levels(a[[x]])))
  ggplot2::ggplot(a, ggplot2::aes(.data[[x]], ln_valor)) +
    ggplot2::geom_jitter(alpha = 0.05, size = 0.5, width = 0.32, height = 0, colour = COR[["marinho"]],
                         shape = 16) +
    ggplot2::geom_linerange(data = resumo, ggplot2::aes(x = categoria, ymin = q1, ymax = q3), inherit.aes = FALSE,
                            colour = COR[["laranja"]], linewidth = 1.6) +
    ggplot2::geom_point(data = resumo, ggplot2::aes(x = categoria, y = media), inherit.aes = FALSE,
                        colour = COR[["laranja_texto"]], fill = COR[["cartao"]], size = 2.4, shape = 21,
                        stroke = 1.1) +
    EIXO_VALOR +
    ggplot2::scale_x_discrete(drop = FALSE, labels = function(k) {
      n <- resumo$n[match(k, resumo$categoria)]
      paste0(k, "\nn = ", rotulo_num()(n))
    }) +
    rotulos_cfs(titulo, sprintf(paste("η² na população = %s (η = %s): parcela da variância de ln(valor)",
                                      "explicada só por esta variável. Barra laranja: Q1–Q3 (aproximados); círculo: média; n: linhas na população."),
                                n_fmt(eta, 3), n_fmt(sqrt(eta), 3)),
                rodape(n_pop), x = eixo_x) +
    tema_cfs() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# Grafico de variavel adicionada: residuos de ln(valor) ~ ln(peso) contra
# residuos de ln(dist) ~ ln(peso). A inclinacao e o coeficiente que ln(dist)
# teria no modelo com ln(peso), e o r e a correlacao parcial.
grafico_distancia_parcial <- function(a, r_parcial, r_marginal, n_pop) {
  d <- tibble::tibble(x = resid(lm(ln_dist ~ ln_peso, data = a)), ln_valor = resid(lm(ln_valor ~ ln_peso, data = a)))
  ggplot2::ggplot(d, ggplot2::aes(x, ln_valor)) +
    ggplot2::geom_point(alpha = 0.04, size = 0.5, colour = COR[["marinho"]], shape = 16) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = COR[["laranja"]], linewidth = 0.8, se = FALSE) +
    ggplot2::geom_line(data = medias_por_faixa(d, "x"), ggplot2::aes(x, y), colour = COR[["secundario"]],
                       linewidth = 0.6, linetype = "22") +
    ggplot2::geom_point(data = medias_por_faixa(d, "x"), ggplot2::aes(x, y), colour = COR[["secundario"]], size = 1.2) +
    rotulos_cfs("Valor × distância (escala log), descontado o peso",
                sprintf(paste("Correlação parcial de ln(valor) e ln(distância), dado ln(peso), na população = %s",
                              "(a marginal era %s). Eixos: resíduos de cada um contra ln(peso)."),
                        n_fmt(r_parcial, 3), n_fmt(r_marginal, 3)),
                rodape(n_pop), x = "ln(distância) não explicado pelo peso", y = "ln(valor) não explicado pelo peso") +
    tema_cfs()
}

.rotular_categoria <- function(variavel, codigo) {
  if (variavel == "setor") unname(SETORES[codigo]) else codigo
}

grafico_matriz <- function(m) {
  nomes <- PREDITORES[c("ln_peso", "ln_dist", CATEGORICOS)]
  d <- dplyr::bind_rows(m, dplyr::rename(m, a = b, b = a)) |>
    dplyr::mutate(a = factor(nomes[a], nomes), b = factor(nomes[b], rev(nomes)))
  ggplot2::ggplot(d, ggplot2::aes(a, b, fill = valor)) +
    ggplot2::geom_tile(colour = COR[["papel"]], linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = n_fmt(valor, 2),
                                    colour = ifelse(valor > 0.45, "claro", "escuro")), size = 3) +
    ggplot2::scale_colour_manual(values = c(claro = COR[["cartao"]], escuro = COR[["marinho"]]), guide = "none") +
    ggplot2::scale_fill_gradientn(colours = RAMPA_SEQUENCIAL, limits = c(0, 1), breaks = c(0, 0.5, 1),
                                  labels = c("0", "0,5", "1"), name = "Associação") +
    rotulos_cfs("Associação entre os preditores",
                paste("Numérico × numérico: |r| de Pearson; numérico × categórico: η (razão de correlação);",
                      "categórico × categórico: V de Cramér. Todos de 0 (independentes) a 1."),
                "Fonte: CFS 2022 PUMS, população inteira (sem SCTG colapsado e sem modal desconhecido), sem fator.",
                x = NULL, y = NULL) +
    tema_cfs() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), panel.grid = ggplot2::element_blank())
}

# --- Execucao ----------------------------------------------------------------

executar_exploracao <- function() {
  dir.create(PASTA_FIG, recursive = TRUE, showWarnings = FALSE)
  dir.create(PASTA_TAB, recursive = TRUE, showWarnings = FALSE)
  registrar_fontes_cfs()
  base <- obter_base_tratada(pipeline = FALSE)
  on.exit(fechar_base(base))
  con <- dbplyr::remote_con(base)
  sql <- .sql_variaveis(base)

  message("Numericas na populacao...")
  num <- numericas_populacao(con, sql)
  n_pop <- as.numeric(num$n)

  message("eta2 com a resposta...")
  eta_resp <- vapply(CATEGORICOS, function(g) eta2(con, sql, "ln_valor", g), numeric(1))
  resumo <- purrr::map_dfr(CATEGORICOS, ~ resumo_por_categoria(con, sql, .x))

  message("Associacao entre preditores...")
  matriz <- matriz_associacao(con, sql, num)

  message("Amostra...")
  a <- sortear_amostra(con, sql)
  # Spearman so na amostra: o DuckDB nao tem rank de correlacao pronto.
  spearman <- c(ln_peso = cor(a$ln_valor, a$ln_peso, method = "spearman"),
                dist_km = cor(a$ln_valor, a$dist_km, method = "spearman"))

  modelo <- lm(ln_valor ~ ln_peso + ln_dist + setor + modal + perigoso + temperatura, data = droplevels(a))
  tab_gvif <- gvif(modelo)

  resposta <- tibble::tibble(
    preditor = c("ln_peso", "ln_dist", "dist_km", CATEGORICOS),
    medida = c(rep("r de Pearson", 3), rep("η²", 4)),
    valor = c(num$r_ln_peso, num$r_ln_dist, num$r_dist_km, eta_resp),
    r2_sozinho = c(num$r_ln_peso^2, num$r_ln_dist^2, num$r_dist_km^2, eta_resp)
  )

  # Correlacao parcial com ln(valor), controlando ln(peso): o que a distancia
  # acrescenta ao peso (decide a escala da distancia).
  parcial_peso <- function(r_yx, r_xz) (r_yx - num$r_ln_peso * r_xz) / sqrt((1 - num$r_ln_peso^2) * (1 - r_xz^2))
  parcial <- tibble::tibble(preditor = c("ln_dist", "dist_km"),
                            r_marginal = c(num$r_ln_dist, num$r_dist_km),
                            r_parcial_dado_ln_peso = c(parcial_peso(num$r_ln_dist, num$r_peso_ln_dist),
                                                       parcial_peso(num$r_dist_km, num$r_peso_dist_km)))
  readr::write_csv(parcial, file.path(PASTA_TAB, "distancia_parcial.csv"))

  readr::write_csv(resposta, file.path(PASTA_TAB, "associacao_resposta.csv"))
  readr::write_csv(matriz, file.path(PASTA_TAB, "associacao_preditores.csv"))
  readr::write_csv(resumo, file.path(PASTA_TAB, "ln_valor_por_categoria.csv"))
  readr::write_csv(tab_gvif, file.path(PASTA_TAB, "gvif_amostra.csv"))
  readr::write_csv(num, file.path(PASTA_TAB, "numericas_populacao.csv"))

  message("Graficos...")
  eixo_peso <- ggplot2::scale_x_continuous(breaks = log(10^(0:6)), labels = function(b) rotulo_num()(round(exp(b))))
  eixo_dist_log <- ggplot2::scale_x_continuous(breaks = log(10^(0:4)), labels = function(b) rotulo_num()(round(exp(b))))
  .salvar(grafico_numerico(a, "ln_peso", num$r_ln_peso, "Valor × peso", "Peso da remessa (lb, escala log)",
                           eixo_peso, n_pop), "01_valor_peso")
  .salvar(grafico_numerico(a, "dist_km", num$r_dist_km, "Valor × distância (escala linear)",
                           "Distância em linha reta (km)", ggplot2::scale_x_continuous(labels = rotulo_num()),
                           n_pop), "02a_valor_distancia_linear")
  .salvar(grafico_numerico(a, "ln_dist", num$r_ln_dist, "Valor × distância (escala log)",
                           "Distância em linha reta (km, escala log)", eixo_dist_log, n_pop),
          "02b_valor_distancia_log")
  .salvar(grafico_distancia_parcial(a, parcial$r_parcial_dado_ln_peso[1], num$r_ln_dist, n_pop), "02c_valor_distancia_dado_peso")
  titulos <- c(setor = "Valor × setor de quem envia", modal = "Valor × modal de transporte",
               perigoso = "Valor × periculosidade (HAZMAT)", temperatura = "Valor × controle de temperatura")
  eixos <- c(setor = "Setor (NAICS)", modal = "Família de modal",
             perigoso = "Carga perigosa (HAZMAT H ou P)", temperatura = "Temperatura controlada")
  for (i in seq_along(CATEGORICOS)) {
    g <- CATEGORICOS[i]
    p <- grafico_categorico(a, g, resumo, eta_resp[[g]], titulos[[g]], eixos[[g]], n_pop)
    if (g %in% c("setor", "modal")) p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
    .salvar(p, sprintf("%02d_valor_%s", i + 2, g))
  }
  .salvar(grafico_matriz(matriz), "07_associacao_preditores", largura = 6.4, altura = 5.4)

  print(num); print(resposta); print(spearman); print(matriz); print(tab_gvif)
  print(parcial)
  print(tibble::as_tibble(resumo), n = 40)
  invisible(list(num = num, resposta = resposta, matriz = matriz, gvif = tab_gvif, resumo = resumo))
}

if (sys.nframe() == 0) executar_exploracao()
