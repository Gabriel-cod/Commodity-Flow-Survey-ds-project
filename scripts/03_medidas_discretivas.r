source("scripts/00_setup.R")
source(here::here("scripts", "02_trata_base.R"))

base <- obter_base_tratada(pipeline = FALSE)





view(base)


# exemplo  :

#valor_mercadoria <- base |>
#  filter(!fl_exportacao) |>
#  group_by(SECTOR) |>
#  summarise(linhas = n(), valor_total = sum(SHIPMT_VALUE * WGT_FACTOR)) |>
#  collect()
#fechar_base(base)



glimpse(base)















# 1) medidas discretivas de valor
###


base |>
  pull(SHIPMT_VALUE) |>
  summary() 


###

# Por setor
base |>
  group_by(SECTOR) |>
  select(SHIPMT_VALUE) |>
  summarise(
    Media_Dos_Valores = mean(SHIPMT_VALUE),
    Primeiro_Quartil = quantile(SHIPMT_VALUE, 0.25),
    Mediana_dos_Valores = median(SHIPMT_VALUE),
    Terceiro_Quartil = quantile(SHIPMT_VALUE, 0.75)
  )  |>
  collect() |> 
  rotular(ler_dicionario(), "SECTOR")

# Por Mode

base |>
  group_by(MODE) |>
  select(SHIPMT_VALUE) |>
  summarise(
    Media_Dos_Valores = mean(SHIPMT_VALUE),
    Primeiro_Quartil = quantile(SHIPMT_VALUE, 0.25),
    Mediana_dos_Valores = median(SHIPMT_VALUE),
    Terceiro_Quartil = quantile(SHIPMT_VALUE, 0.75)
  )  |>
  collect() |> 
  rotular(ler_dicionario(), "MODE")
  
  # ----

  # 2) Medidas Discretivas de Peso

base |>
    pull(SHIPMT_WGHT) |>
  summary()


base |>
    pull(SHIPMT_WGHT) |>
  var()

## Agrupando por setor

base |> 
  group_by(SECTOR) |> 
  select(SHIPMT_WGHT) |> 
  summarise(
    Media_Dos_Pesos = mean(SHIPMT_WGHT),
    Primeiro_Quartil = quantile(SHIPMT_WGHT, 0.25),
    Mediana_dos_Pesos = median(SHIPMT_WGHT),
    Terceiro_Quartil = quantile(SHIPMT_WGHT, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "SECTOR")

## Agrupando por Transporte

base |> 
  group_by(MODE) |> 
  select(SHIPMT_WGHT) |> 
  summarise(
    Media_Dos_Pesos = mean(SHIPMT_WGHT),
    Primeiro_Quartil = quantile(SHIPMT_WGHT, 0.25),
    Mediana_dos_Pesos = median(SHIPMT_WGHT),
    Terceiro_Quartil = quantile(SHIPMT_WGHT, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "MODE")

## ---

# 3) Distancia em Km

base |>
  pull(dist_km) |>
  summary()


base |>
  pull(dist_km) |>
  var()


## Agrupando por setor

base |> 
  group_by(SECTOR) |> 
  select(dist_km) |> 
  summarise(
    Media_Distancia = mean(dist_km),
    Desvio_Padrao = sd(dist_km),
    Variancia = var(dist_km),
    Primeiro_Quartil = quantile(dist_km, 0.25),
    Mediana_Distancia = median(dist_km),
    Terceiro_Quartil = quantile(dist_km, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "SECTOR")


## Agrupando por transporte

base |> 
  group_by(MODE) |> 
  select(dist_km) |> 
  summarise(
    Media_Distancia = mean(dist_km),
    Desvio_Padrao = sd(dist_km),
    Variancia = var(dist_km),
    Primeiro_Quartil = quantile(dist_km, 0.25),
    Mediana_Distancia = median(dist_km),
    Terceiro_Quartil = quantile(dist_km, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "MODE")


## --

# 4) Medidas discretivas de WGT_FACTOR
base |>
  pull(WGT_FACTOR) |>
  summary()

base |>
  pull(WGT_FACTOR) |>
  sd()


base |>
  pull(WGT_FACTOR) |>
  var()

## Agrupando por setor

base |> 
  group_by(SECTOR) |> 
  select(WGT_FACTOR) |> 
  summarise(
    Media_Fator_Peso = mean(WGT_FACTOR),
    Desvio_Padrao = sd(WGT_FACTOR),
    Variancia = var(WGT_FACTOR),
    Primeiro_Quartil = quantile(WGT_FACTOR, 0.25),
    Mediana_Fator_Peso = median(WGT_FACTOR),
    Terceiro_Quartil = quantile(WGT_FACTOR, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "SECTOR")


## agrupando por transporte


base |> 
  group_by(MODE) |> 
  select(WGT_FACTOR) |> 
  summarise(
    Media_Fator_Peso = mean(WGT_FACTOR),
    Desvio_Padrao = sd(WGT_FACTOR),
    Variancia = var(WGT_FACTOR),
    Primeiro_Quartil = quantile(WGT_FACTOR, 0.25),
    Mediana_Fator_Peso = median(WGT_FACTOR),
    Terceiro_Quartil = quantile(WGT_FACTOR, 0.75)
  ) |> 
  collect() |> 
  rotular(ler_dicionario(), "MODE")



## -----------------------------------------------------------

## Medidas discretivas para variaveis categoricas 

dic <- ler_dicionario()


# 2. Lista com as variáveis categóricas
vars_categoricas <- c("ORIG_CFS_AREA", "DEST_CFS_AREA", "SCTG", "SECTOR", "HAZMAT", "TEMP_CNTL_YN",
"MODE", "EXPORT_YN", "EXPORT_CNTRY")


# 3. Resumo simples de frequências e percentagens da amostra
resumos_categoricos <- map(vars_categoricas, function(var) {
  
  base |> 
    group_by(.data[[var]]) |> 
    summarise(
      frequencia = n()
    ) |> 
    collect() |> 
    mutate(
      porcentagem = (frequencia / sum(frequencia)) * 100
    ) |> 
    rotular(dic, var) |> 
    arrange(desc(frequencia)) |> 
    slice_head(n = 10) # Pega o Top 10 maiores para não poluir
}) |> 
  set_names(vars_categoricas)


# Ver Áreas de Origem
resumos_categoricos$ORIG_CFS_AREA

# Ver Áreas de Destino
resumos_categoricos$DEST_CFS_AREA

# Ver Setores
resumos_categoricos$SECTOR

# Ver Modais de Transporte
resumos_categoricos$MODE

# Ver distribuição de Mercadorias (SCTG)
resumos_categoricos$SCTG

# Ver distribuição de Materiais Perigosos (HAZMAT)
resumos_categoricos$HAZMAT
