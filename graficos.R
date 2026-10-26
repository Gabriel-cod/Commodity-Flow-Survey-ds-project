install.packages('ggthemes')
install.packages('ggcorrplot')
install.packages("scales")
library(ggthemes)
library(ggcorrplot)
library(dbplot)
library(scales)
source("scripts/00_setup.R")
source(here::here("scripts", "02_trata_base.R"))

base <- obter_base_tratada(pipeline = FALSE)


# Grafico peso x valor

relacao_peso_valor <- base |> 
  filter(SHIPMT_WGHT > 0, SHIPMT_VALUE > 0) |> 
  mutate(bin_peso_log = floor(log10(SHIPMT_WGHT) * 10) / 10) |> 
  group_by(bin_peso_log) |> 
  summarise(valor_mediano = median(SHIPMT_VALUE, na.rm = TRUE)) |> 
  collect() |> 
  mutate(peso_lbs = 10^bin_peso_log)


ggplot(
  relacao_peso_valor, aes(
    x = peso_lbs,
    y = valor_mediano
  ) )+
      labs(
    title = 'Relaçao Peso X Valor',
    subtitle = "Cada ponto representa um intervalo de peso (caixa)",
    x = 'Peso (lb)',
    y = 'Valor mediano(US$)') +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()))+
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  geom_point() +
  theme_minimal() 


# GRAFICO valor x distancia


relacao_dist_valor <- base |> 
  filter(dist_km > 0, SHIPMT_VALUE > 0) |> 
  # Cria as caixas logarítmicas para a distância
  mutate(bin_dist_log = floor(log10(dist_km) * 5) / 5) |> 
  group_by(bin_dist_log) |> 
  summarise(valor_mediano = median(SHIPMT_VALUE, na.rm = TRUE)) |> 
  collect() |> 
  # Converte o logaritmo de volta para quilómetros
  mutate(distancia_km = 10^bin_dist_log)


#dispersao

ggplot(relacao_dist_valor,
  aes(x = distancia_km, y = valor_mediano)
) +
  labs(
    title = 'Relaçao Distancia X Valor',
    #subtitle = "Cada ponto representa um intervalo de peso (caixa)",
    x = 'Distancia (Km)',
    y = 'Valor mediano(US$)') +
  scale_x_log10() +
  scale_y_log10() +
  geom_point() +
  theme_minimal()



# Grafico distancia x peso 
relacao_dist_peso <- base |> 
  filter(dist_km > 0, SHIPMT_WGHT > 0) |> 
  mutate(bin_dist_log = floor(log10(dist_km) * 10) / 10) |> 
  group_by(bin_dist_log) |> 
  summarise(
    peso_mediano = median(SHIPMT_WGHT, na.rm = TRUE), 
    peso_media = mean(SHIPMT_WGHT, na.rm = TRUE)
  
  ) |> 
  collect() |> 
  mutate(distancia_km = 10^bin_dist_log)


#dispersao
ggplot(
  relacao_dist_peso, 
  aes(x = distancia_km, y = peso_mediano)
) +
  labs(
    title = 'Relaçao Distancia x Peso',
    x = 'Intervalos das distancias', 
    y = 'Mediana do Peso', 
    subtitle = 'Cada ponto esta representando um intervalo de distancia'
    ) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point() +
  theme_gdocs()



# Correlaçoes 

matriz_cor <- base |> 
  filter(SHIPMT_WGHT > 0, SHIPMT_VALUE > 0, dist_km > 0) |> 
  summarise(
    # Aplicar log10 é crucial porque as tuas variáveis têm escalas extremas
    peso_valor = cor(log10(SHIPMT_WGHT), log10(SHIPMT_VALUE)),
    dist_valor = cor(log10(dist_km), log10(SHIPMT_VALUE)),
    peso_dist  = cor(log10(SHIPMT_WGHT), log10(dist_km)),
    # A correlação de uma variável com ela mesma é sempre 1
    peso_peso = 1, valor_valor = 1, dist_dist = 1
  ) |> 
  collect()

# 2. Transforma o resultado do DuckDB numa matriz real do R
matriz_final <- matrix(
  c(matriz_cor$peso_peso, matriz_cor$peso_valor, matriz_cor$peso_dist,
    matriz_cor$peso_valor, matriz_cor$valor_valor, matriz_cor$dist_valor,
    matriz_cor$peso_dist, matriz_cor$dist_valor, matriz_cor$dist_dist),
  nrow = 3, 
  dimnames = list(
    c("Peso (log)", "Valor (log)", "Distância (log)"),
    c("Peso (log)", "Valor (log)", "Distância (log)")
  )
)

ggcorrplot(
  matriz_final, 
  hc.order = FALSE, 
  type = "lower", # Mostra apenas o triângulo inferior para evitar repetição
  lab = TRUE,     # Coloca o número exato do coeficiente dentro do quadrado
  lab_size = 4,
  colors = c("#d9534f", "white", "#2b5c8f"), # Vermelho (negativo) a Azul (positivo)
  title = "Matriz de Correlação das Variáveis Logísticas",
  legend.title = "Coeficiente (r)"
) +
  theme_par()




## 
#Box plot por setor

# Peso

base |> 
 filter(SHIPMT_WGHT > 0) |> 
  # 1. Calcula o logaritmo primeiro
  #mutate(peso_log = log10(SHIPMT_WGHT)) |> 
  # 2. Passa as variáveis diretamente sem "x=" e "y="
  dbplot_boxplot(SECTOR, SHIPMT_WGHT) +
  scale_y_log10(labels = label_number(suffix = " lbs")) +
  coord_flip() +
  labs(
    title = "Distribuição do Peso das Remessas por Setor Económico",
    x = "Setor",
    y = "Peso da Carga (escala log10)"
  ) +
  theme_par()



# Valor da mercadoria 



base |> 
 filter(SHIPMT_VALUE > 0) |> 
  # 1. Calcula o logaritmo primeiro
  #mutate(peso_log = log10(SHIPMT_WGHT)) |> 
  # 2. Passa as variáveis diretamente sem "x=" e "y="
  dbplot_boxplot(SECTOR, SHIPMT_VALUE) +
  scale_y_log10(labels = label_number(suffix = " US$")) +
  coord_flip() +
  labs(
    title = "Distribuição do Valor das Remessas por Setor Económico",
    x = "Setor",
    y = "Valor da Carga (escala log10)"
  ) +
  theme_par()



# Distancia

base |> 
 filter(dist_km > 0) |> 
  # 1. Calcula o logaritmo primeiro
  #mutate(peso_log = log10(SHIPMT_WGHT)) |> 
  # 2. Passa as variáveis diretamente sem "x=" e "y="
  dbplot_boxplot(SECTOR, dist_km) +
  scale_y_log10(labels = label_number(suffix = "KM")) +
  coord_flip() +
  labs(
    title = "Distribuição das distancias das Remessas por Setor Economico",
    x = "Setor",
    y = "Distancia da carga em KM (escala log10)"
  ) +
  theme_par()
