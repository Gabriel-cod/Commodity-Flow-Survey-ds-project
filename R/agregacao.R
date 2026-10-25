# Agregacoes sem e com o WGT_FACTOR, lado a lado (FSD 31.5 e 31.6). Tudo roda no
# DuckDB e so a tabela resumida chega ao R. Nada de median(), quantile_*() ou
# count(DISTINCT): eles guardam a coluna inteira na memoria, fora do
# memory_limit. Os quantis saem das contagens por valor distinto.

resumo_numerico <- function(dados, coluna, populacao = "completa") {
  con <- dbplyr::remote_con(dados)
  v <- DBI::dbQuoteIdentifier(con, coluna)
  r <- DBI::dbGetQuery(con, sprintf("
    WITH d AS (%s),
         g AS (SELECT %s::DOUBLE AS v, count(*) AS c, sum(WGT_FACTOR) AS f FROM d GROUP BY 1),
         m AS (SELECT sum(c) FILTER (WHERE v IS NOT NULL) AS n, sum(f) FILTER (WHERE v IS NOT NULL) AS total,
                      sum(c * v) / sum(c) FILTER (WHERE v IS NOT NULL) AS media,
                      sum(f * v) / sum(f) FILTER (WHERE v IS NOT NULL) AS media_f,
                      coalesce(sum(c) FILTER (WHERE v IS NULL), 0) AS ausentes,
                      min(v) AS minimo, max(v) AS maximo FROM g)
    SELECT any_value(m.n) AS n, any_value(m.total) AS total, any_value(m.ausentes) AS ausentes,
           any_value(m.media) AS media, any_value(m.media_f) AS media_f,
           sqrt(sum(g.c * (g.v - m.media)^2) / (any_value(m.n) - 1)) AS dp,
           sqrt(sum(g.f * (g.v - m.media_f)^2) / any_value(m.total)) AS dp_f,
           any_value(m.minimo) AS minimo, any_value(m.maximo) AS maximo
    FROM g CROSS JOIN m WHERE g.v IS NOT NULL",
    .sql_populacao(dados, populacao), v))
  q <- quantis(dados, coluna, c(0.25, 0.5, 0.75), populacao)
  tibble::tibble(
    populacao = populacao,
    variavel = coluna,
    medida = c("n", "ausentes", "media", "dp", "minimo", "q1", "mediana", "q3", "maximo"),
    sem_fator = c(r$n, r$ausentes, r$media, r$dp, r$minimo, q$sem_fator, r$maximo),
    com_fator = c(r$total, r$ausentes, r$media_f, r$dp_f, r$minimo, q$com_fator, r$maximo)
  )
}

# Sem fator: quantil tipo 7 do R (h = (n - 1) p + 1, interpolando x_floor(h) e
# o seguinte). Com fator: menor v cujo acumulado de WGT_FACTOR e >= p * total;
# a folga relativa de 1e-12 cobre o arredondamento da soma acumulada em p = 1.
quantis <- function(dados, coluna, probs, populacao = "completa", grupo = NULL) {
  con <- dbplyr::remote_con(dados)
  v <- DBI::dbQuoteIdentifier(con, coluna)
  g <- if (is.null(grupo)) "NULL" else DBI::dbQuoteIdentifier(con, grupo)
  r <- DBI::dbGetQuery(con, sprintf("
    WITH d AS (%s),
         g AS (SELECT %s AS grupo, %s AS v, count(*) AS c, sum(WGT_FACTOR) AS f
               FROM d WHERE %s IS NOT NULL GROUP BY ALL),
         o AS (SELECT *, sum(c) OVER w AS acum_c, sum(f) OVER w AS acum_f,
                      sum(c) OVER (PARTITION BY grupo) AS n, sum(f) OVER (PARTITION BY grupo) AS total
               FROM g WINDOW w AS (PARTITION BY grupo ORDER BY v)),
         p AS (SELECT unnest([%s]::DOUBLE[]) AS p),
         k AS (SELECT *, floor((n - 1) * p + 1) AS h FROM o CROSS JOIN p)
    SELECT grupo, p, any_value(n) AS linhas, any_value(total) AS remessas,
           min(v) FILTER (WHERE acum_c >= h) AS baixo,
           min(v) FILTER (WHERE acum_c >= least(h + 1, n)) AS alto,
           any_value((n - 1) * p + 1 - h) AS fracao,
           min(v) FILTER (WHERE acum_f >= p * total * (1 - 1e-12)) AS com_fator
    FROM k GROUP BY grupo, p ORDER BY grupo, p",
    .sql_populacao(dados, populacao), g, v, v, paste(probs, collapse = ", ")))
  saida <- tibble::tibble(
    populacao = populacao,
    grupo = r$grupo,
    p = r$p,
    linhas = r$linhas,
    remessas = r$remessas,
    sem_fator = r$baixo + r$fracao * (r$alto - r$baixo),
    com_fator = as.numeric(r$com_fator)
  )
  .nomear_grupo(saida, grupo)
}

# Classes de log10(v) de largura fixa; a folga de 1e-9 poe as potencias exatas
# (10, 100, ...) na classe que comeca nelas. v <= 0 fica com classe nula.
histograma_log <- function(dados, coluna, largura = 0.1, populacao = "completa", grupo = NULL) {
  con <- dbplyr::remote_con(dados)
  v <- DBI::dbQuoteIdentifier(con, coluna)
  g <- if (is.null(grupo)) "NULL" else DBI::dbQuoteIdentifier(con, grupo)
  r <- DBI::dbGetQuery(con, sprintf("
    WITH d AS (%s)
    SELECT %s AS grupo,
           CASE WHEN %s > 0 THEN floor(log10(%s) / %s + 1e-9) END AS classe,
           count(*) AS linhas, sum(WGT_FACTOR) AS remessas
    FROM d GROUP BY ALL ORDER BY grupo, classe",
    .sql_populacao(dados, populacao), g, v, v, format(largura, scientific = FALSE)))
  saida <- tibble::tibble(
    populacao = populacao,
    grupo = r$grupo,
    inicio = 10^(r$classe * largura),
    fim = 10^((r$classe + 1) * largura),
    linhas = r$linhas,
    remessas = r$remessas
  )
  .nomear_grupo(saida, grupo)
}

frequencias <- function(dados, colunas, populacao = "completa", dentro_de = NULL) {
  r <- filtrar_populacao(dados, populacao) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(colunas))) |>
    dplyr::summarise(
      linhas = dplyr::n(),
      remessas = sum(WGT_FACTOR, na.rm = TRUE),
      valor = sum(SHIPMT_VALUE * WGT_FACTOR, na.rm = TRUE),
      toneladas = sum(peso_t * WGT_FACTOR, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(linhas = as.numeric(linhas)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dentro_de))) |>
    dplyr::mutate(pct_linhas = 100 * linhas / sum(linhas),
                  pct_remessas = 100 * remessas / sum(remessas)) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(linhas))
  tibble::add_column(r, populacao = populacao, .before = 1)
}

.sql_populacao <- function(dados, populacao) {
  dbplyr::sql_render(filtrar_populacao(dados, populacao))
}

.nomear_grupo <- function(saida, grupo) {
  if (is.null(grupo)) return(dplyr::select(saida, -"grupo"))
  dplyr::rename(saida, !!grupo := "grupo")
}
