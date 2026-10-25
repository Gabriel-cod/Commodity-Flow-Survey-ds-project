# 04_secao3_descritiva.R --------------------------------------------------
# Secao 3 do Check-in 2: descritiva das 8 variaveis escolhidas (FSD 31.7). So
# define objetos; quem calcula e obter_resultados() (scripts/10_resultados.R).
# A descritiva usa a base completa: os codigos suprimidos e colapsados sao
# justamente o que ela precisa mostrar.

VARIAVEIS_NUMERICAS <- tibble::tribble(
  ~variavel,        ~coluna,          ~unidade,
  "SHIPMT_WGHT",    "SHIPMT_WGHT",    "lb",
  "SHIPMT_WGHT",    "peso_t",         "t",
  "SHIPMT_VALUE",   "SHIPMT_VALUE",   "US$",
  "SHIPMT_DIST_GC", "SHIPMT_DIST_GC", "mi",
  "SHIPMT_DIST_GC", "dist_km",        "km"
)

VARIAVEIS_CATEGORICAS <- c("SCTG", "MODE", "SECTOR", "ORIG_CFS_AREA", "DEST_CFS_AREA")

# SCTG com menos linhas que isto contam como raros (mesmo corte da Secao 1).
LIMITE_RARO <- 1000

calcular_secao3 <- function(base) {
  numericas <- purrr::pmap_dfr(VARIAVEIS_NUMERICAS, function(variavel, coluna, unidade) {
    dplyr::mutate(resumo_numerico(base, coluna), variavel = .env$variavel, coluna = .env$coluna,
                  unidade = .env$unidade, .after = "variavel")
  })

  dicionario <- ler_dicionario()
  frequencias_cat <- purrr::map_dfr(VARIAVEIS_CATEGORICAS, function(v) {
    frequencias(base, v) |>
      rotular(dicionario, v, sufixo = "_rotulo") |>
      dplyr::rename(codigo = dplyr::all_of(v), rotulo = dplyr::all_of(paste0(v, "_rotulo"))) |>
      dplyr::relocate(rotulo, .after = "codigo") |>
      tibble::add_column(variavel = v, .after = "populacao")
  })

  list(
    numericas = numericas,
    frequencias = frequencias_cat,
    maiores = .maiores_e_demais(frequencias_cat, 10),
    raras = .categorias_raras(frequencias_cat),
    areas = .resumo_areas(frequencias_cat)
  )
}

# As n categorias com mais linhas de cada variavel e uma linha com o resto.
.maiores_e_demais <- function(freq, n) {
  freq |>
    dplyr::group_by(populacao, variavel) |>
    dplyr::arrange(dplyr::desc(linhas), .by_group = TRUE) |>
    dplyr::mutate(posicao = dplyr::row_number(), resto = sum(posicao > n)) |>
    dplyr::mutate(codigo = ifelse(posicao > n, "Demais", codigo),
                  rotulo = ifelse(posicao > n, paste0("Demais (", resto, " categorias)"), rotulo)) |>
    dplyr::group_by(populacao, variavel, codigo, rotulo) |>
    dplyr::summarise(dplyr::across(c(linhas, remessas, valor, toneladas, pct_linhas, pct_remessas), sum),
                     posicao = min(posicao), .groups = "drop") |>
    dplyr::arrange(variavel, posicao) |>
    dplyr::select(-posicao)
}

.categorias_raras <- function(freq) {
  sctg <- dplyr::filter(freq, variavel == "SCTG")
  dplyr::bind_rows(
    dplyr::mutate(dplyr::filter(sctg, codigo == "00"), motivo = "Mercadoria suprimida"),
    dplyr::mutate(dplyr::filter(sctg, grepl("-", codigo)), motivo = "SCTG colapsado em faixa"),
    dplyr::mutate(dplyr::filter(sctg, linhas < LIMITE_RARO, codigo != "00", !grepl("-", codigo)),
                  motivo = paste("Menos de", n_fmt(LIMITE_RARO), "linhas")),
    dplyr::mutate(dplyr::filter(freq, variavel == "MODE", codigo == "30"), motivo = "Modal desconhecido")
  ) |>
    dplyr::relocate(motivo, .after = "rotulo")
}

# Concentracao das areas CFS: NN-99999 e o restante do estado fora das
# metropoles; NN-00000 e 00-00000 sao areas suprimidas.
.resumo_areas <- function(freq) {
  freq |>
    dplyr::filter(variavel %in% c("ORIG_CFS_AREA", "DEST_CFS_AREA")) |>
    dplyr::group_by(populacao, variavel) |>
    dplyr::arrange(dplyr::desc(linhas), .by_group = TRUE) |>
    dplyr::summarise(
      categorias = dplyr::n(),
      pct_linhas_10_maiores = sum(pct_linhas[seq_len(min(10, dplyr::n()))]),
      pct_remessas_10_maiores = sum(sort(pct_remessas, decreasing = TRUE)[seq_len(min(10, dplyr::n()))]),
      restante_categorias = sum(endsWith(codigo, "-99999")),
      restante_pct_linhas = sum(pct_linhas[endsWith(codigo, "-99999")]),
      suprimidas_categorias = sum(endsWith(codigo, "-00000")),
      suprimidas_linhas = sum(linhas[endsWith(codigo, "-00000")]),
      suprimidas_pct_linhas = sum(pct_linhas[endsWith(codigo, "-00000")]),
      .groups = "drop"
    )
}
