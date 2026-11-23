# 21_base_modelagem.R ------------------------------------------------------
# Check-in 3, secao 1: amostra e divisao treino/teste usadas nas Partes A e B
# (FSD 32.3). So define objetos; o cache fica em 25_resultados_checkin3.R.

# Tabela de preditores da secao 1 (FSD 32.2).
PREDITORES_MODELAGEM <- tibble::tribble(
  ~variavel,     ~descricao,                         ~tipo,        ~unidade,                         ~referencia,
  "ln_valor",    "Valor da remessa",                 "numérico",   "ln(US$)",                        "—",
  "ln_peso",     "Peso da remessa",                  "numérico",   "ln(lb)",                         "—",
  "ln_dist",     "Distância em linha reta",          "numérico",   "ln(km)",                         "—",
  "setor",       "Setor de quem envia (NAICS)",      "categórico", "7 setores",                      REFERENCIAS[["setor"]],
  "modal",       "Família de modal",                 "categórico", "6 famílias",                     REFERENCIAS[["modal"]],
  "perigosa",    "Carga perigosa (HAZMAT H ou P)",   "sim/não",    "—",                              "Não",
  "temperatura", "Temperatura controlada",           "sim/não",    "—",                              "Não",
  "caminhao",    "Despachada de caminhão (111, 112)", "sim/não",   "evento = Sim",                   "Não"
)

# Um argumento so: .obter_secao() passaria os resultados no segundo.
calcular_base_modelagem <- function(base) {
  modelagem <- cfg("modelagem")
  con <- dbplyr::remote_con(base)
  sql <- sql_modelagem(base)
  pop <- DBI::dbGetQuery(con, sprintf("
    SELECT count(*) AS linhas, sum(CASE WHEN caminhao = 'Sim' THEN 1 ELSE 0 END) AS caminhao,
           count(*) FILTER (WHERE NOT completa) AS ausentes
    FROM (SELECT *, coalesce(isfinite(ln_valor) AND isfinite(ln_peso) AND isfinite(ln_dist), false)
                    AND setor IS NOT NULL AND modal IS NOT NULL AS completa FROM (%s))", sql))
  fora <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM (%s)", dbplyr::sql_render(base)))$n - pop$linhas

  amostra <- sortear_modelagem(con, sql, modelagem$tamanho_amostra, modelagem$semente)
  partes <- dividir_modelagem(amostra, modelagem$prop_treino, modelagem$semente)

  list(
    treino = partes$treino,
    teste = partes$teste,
    modelagem = modelagem,
    populacao = tibble::tibble(linhas = as.numeric(pop$linhas), fora = as.numeric(fora),
                               pct_caminhao = 100 * as.numeric(pop$caminhao) / as.numeric(pop$linhas),
                               ausentes = as.numeric(pop$ausentes)),
    resumo = resumir_partes(partes),
    preditores = PREDITORES_MODELAGEM
  )
}

# Divisao unica, estratificada pela resposta da Parte B para o evento ter a
# mesma proporcao no treino e no teste. with_seed = set.seed sem mexer no
# gerador de quem chamou.
dividir_modelagem <- function(amostra, prop, semente) {
  divisao <- withr::with_seed(semente, rsample::initial_split(amostra, prop = prop, strata = caminhao))
  list(treino = rsample::training(divisao), teste = rsample::testing(divisao))
}

resumir_partes <- function(partes) {
  purrr::imap_dfr(partes, function(d, nome) {
    tibble::tibble(parte = nome, linhas = nrow(d), pct_caminhao = 100 * mean(d$caminhao == "Sim"),
                   media_ln_valor = mean(d$ln_valor), media_ln_peso = mean(d$ln_peso))
  })
}
