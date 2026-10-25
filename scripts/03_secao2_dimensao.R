# 03_secao2_dimensao.R ----------------------------------------------------
# Secao 2 do Check-in 2: dimensao da base e populacoes (FSD 31.7). So define
# objetos; quem calcula e obter_resultados() (scripts/10_resultados.R).

calcular_secao2 <- function(base) {
  con <- dbplyr::remote_con(base)
  linhas_base <- dplyr::pull(dplyr::count(base))
  # A base gravada guarda as linhas do CSV; a temporaria nao tem metadados, mas
  # a montagem para com erro se as contagens diferirem.
  linhas_csv <- if ("metadados" %in% DBI::dbListTables(con)) {
    DBI::dbGetQuery(con, "SELECT n_linhas FROM metadados")$n_linhas
  } else {
    linhas_base
  }
  colunas <- tibble::tibble(
    populacao = "completa",
    coluna = COLUNAS_TRATADAS,
    grupo = dplyr::case_when(coluna %in% FLAGS ~ "Flag de qualidade",
                             coluna %in% names(TIPOS_CFS) ~ "Original",
                             TRUE ~ "Derivada"),
    tipo = dplyr::coalesce(unname(TIPOS_CFS[coluna]), ifelse(coluna %in% FLAGS, "BOOLEAN", "DOUBLE"))
  )
  dimensao <- tibble::tibble(
    populacao = "completa",
    medida = c("Linhas no CSV original", "Linhas na base tratada", "Linhas removidas",
               "Colunas originais", "Colunas derivadas", "Flags de qualidade", "Colunas na base tratada"),
    valor = as.numeric(c(linhas_csv, linhas_base, linhas_csv - linhas_base,
                         sum(colunas$grupo == "Original"), sum(colunas$grupo == "Derivada"),
                         sum(colunas$grupo == "Flag de qualidade"), nrow(colunas)))
  )
  remessas_total <- dplyr::pull(dplyr::summarise(base, s = sum(WGT_FACTOR, na.rm = TRUE)))
  populacoes <- purrr::map_dfr(POPULACOES$id, function(id) {
    r <- dplyr::collect(dplyr::summarise(filtrar_populacao(base, id),
                                         linhas = dplyr::n(), remessas = sum(WGT_FACTOR, na.rm = TRUE)))
    tibble::tibble(populacao = id, linhas = as.numeric(r$linhas), remessas = r$remessas)
  }) |>
    dplyr::mutate(pct_linhas = 100 * linhas / linhas_base,
                  pct_remessas = 100 * remessas / remessas_total) |>
    dplyr::left_join(dplyr::rename(POPULACOES, populacao = "id"), by = "populacao")
  list(dimensao = dimensao, colunas = colunas, populacoes = populacoes)
}
