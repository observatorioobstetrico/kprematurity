
# cria base ---------------------------------------------------------------

library(tidyverse)

dados_km <- read.csv("cluster/d_km_clus_17_01.csv")
dados_mun <- readxl::read_excel("cluster/RELATORIO_DTB_BRASIL_DISTRITO_2022.xls")

## preparando variaveis para o join

dados_km <- dados_km |> 
  mutate(Nome_UF = nome_uf,
         Nome_Município = nome_mun)

dados_mun <- dados_mun |> 
  select(c(1, 2, 12, 13)) |> 
  unique()


### tratamentos base 1
dados_km$Nome_UF <- dados_km$Nome_UF |> 
  str_trim() %>% 
  str_to_upper() %>% 
  str_remove_all("[:punct:]") %>% 
  abjutils::rm_accent()

dados_km$Nome_Município <- dados_km$Nome_Município |> 
  str_trim() %>% 
  str_to_upper() %>% 
  str_remove_all("[:punct:]") %>% 
  abjutils::rm_accent()

### tratamentos base 2
dados_mun$Nome_UF <- dados_mun$Nome_UF |> 
  str_trim() %>% 
  str_to_upper() %>% 
  str_remove_all("[:punct:]") %>% 
  abjutils::rm_accent()

dados_mun$Nome_Município <- dados_mun$Nome_Município |> 
  str_trim() %>% 
  str_to_upper() %>% 
  str_remove_all("[:punct:]") %>% 
  abjutils::rm_accent()

# juntando bases

base_mapa <- left_join(dados_km, dados_mun, by = c("Nome_UF", "Nome_Município"))

base_mapa <- base_mapa |> 
  janitor::clean_names()

base_mapa <- base_mapa |> 
  rename(codigo_uf = uf) |> 
  select(c(-13, -14))

### baixando base 
write.csv(base_mapa, "cluster/base_mapa.csv")


# faz mapa ----------------------------------------------------------------

library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(geobr)
library(ggspatial)

d_km <- readr::read_csv("base_mapa.csv") |> 
  rename(codigo_mun = codigo_municipio_completo) |> 
  select(-`...1`)

d_mun_br <- read_municipality(code_muni = "all", year = 2020)

d_geobr <- d_km |> 
  select(
    codigo_mun, cluster, tipopremat_espontaneo, tipopremat_eletivo, 
    consprenat_menor_que_6_consultas, indice_de_gini_2010_municipio, 
    idhm_2010_municipio
  ) |> 
  left_join(d_mun_br, by = join_by(codigo_mun == code_muni)) |> 
  st_as_sf()

# mapa das distribuicoes dos clusters

d_geobr |> 
  ggplot() + 
    # geom_sf(data = d_mun_br, color = "#000000") +
    # scale_fill_continuous(na.value = "#d3d3d3") +
    geom_sf(data = d_mun_br, color = NA) +
    geom_sf(aes(fill = as.factor(cluster)), color = "#000000") +
    labs(fill = "Cluster:") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.background = element_blank(), plot.background = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.title = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    annotation_north_arrow(
      location = "br", which_north = "true",
      height = unit(1, "cm"), width = unit(1, "cm"),
      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    annotation_scale()

