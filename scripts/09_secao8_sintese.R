# 09_secao8_sintese.R -----------------------------------------------------
# Secao 8 do Check-in 2: os numeros citados na sintese (FSD 31.7). So define
# objetos; o texto fica no qmd. Os totais saem de uma consulta so, e o modal
# dominante, da Secao 6.

calcular_secao8 <- function(base, res) {
  t <- dplyr::summarise(base, linhas = dplyr::n(), remessas = sum(WGT_FACTOR, na.rm = TRUE),
                        valor_amostra = sum(SHIPMT_VALUE, na.rm = TRUE),
                        valor = sum(SHIPMT_VALUE * WGT_FACTOR, na.rm = TRUE),
                        toneladas_amostra = sum(peso_t, na.rm = TRUE),
                        toneladas = sum(peso_t * WGT_FACTOR, na.rm = TRUE)) |>
    dplyr::collect()
  list(
    numeros = tibble::tibble(
      populacao = "completa",
      medida = c("Remessas", "Valor (US$)", "Toneladas"),
      sem_fator = as.numeric(c(t$linhas, t$valor_amostra, t$toneladas_amostra)),
      com_fator = c(t$remessas, t$valor, t$toneladas)
    ),
    modal_dominante = .modal_dominante(res$secao6$modal_carga)
  )
}

# Familia com a maior participacao em cada medida, sem e com fator.
.modal_dominante <- function(mc) {
  medidas <- c(pct_linhas = "Linhas da amostra", pct_remessas = "Remessas estimadas",
               pct_toneladas_amostra = "Peso somado da amostra", pct_toneladas = "Toneladas estimadas")
  dplyr::bind_rows(lapply(names(medidas), function(m) {
    i <- which.max(mc[[m]])
    tibble::tibble(populacao = mc$populacao[i], medida = medidas[[m]],
                   familia_modal = as.character(mc$familia_modal[i]), pct = mc[[m]][i])
  }))
}
