
# algorithm 01 ------------------------------------------------------------

library(microdatasus)

anos <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
j <- 0

for(i in anos){
  j <- j + 1
  
  dados <- fetch_datasus(
    year_start = i, year_end = i, 
    information_system = "SINASC",
    vars = c(
      "IDADEMAE", "QTDFILVIVO", "PARTO", "CODESTAB", "APGAR1", "APGAR5", "PESO", 
      "RACACORMAE", "QTDGESTANT", "CONSPRENAT", "MESPRENAT", "SEXO", "STTRABPART", 
      "STCESPARTO", "ESTCIVMAE", "LOCNASC", "ESCMAE2010", "GRAVIDEZ", "GESTACAO", 
      "CONSULTAS", "IDANOMAL", "CODMUNNASC", "RACACOR", "CODMUNRES"
    )
  )
  
  #dados <- process_sinasc(dados)
  
  dados$ano <- i
  
  if(j == 1){
    data.table::fwrite(dados, "dados.csv.gz", row.names = FALSE)
  }
  else{
    data.table::fwrite(dados, "dados.csv.gz", row.names = FALSE, append = TRUE)
  }
}


# algorithm 02 ------------------------------------------------------------

library(dplyr)
library(httr)
library(ggplot2)
library(getPass)
library(repr)
library(questionr)

token = getPass()

url_base = "https://bigdata-api.fiocruz.br"

endpoint = paste0(url_base,"/","show_tables")

request <- POST(url = endpoint, body = list("token" = token), encode = "json")

as_tibble(content(request))

convertRequestToDF <- function(request){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  column_names <- unname(variables)
  values = content(request)$rows
  df <- as.data.frame(do.call(rbind, lapply(values, function(r) {
    row <- r
    row[sapply(row,is.null)] <- NA
    rbind(unlist(row))
  })))
  names(df) <- column_names
  return(df)
}


#########################################
# Número de nascimentos total
#########################################

estados <- c('RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN',
             'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC',
             'RS', 'MS', 'MT', 'GO', 'DF')

df_municipio <- data.frame()

endpoint <- paste0(url_base, "/", "sql_query")

for (estado in estados) {
  
  print(estado)
  
  params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query": "SELECT res_SIGLA_UF, res_MUNNOMEX, res_codigo_adotado, CODESTAB, ano_nasc, COUNT(1)',
                  ' FROM \\"datasus-sinasc\\"',
                  ' WHERE (res_SIGLA_UF = \'',estado,'\') AND (ano_nasc BETWEEN 2014 AND 2020)',
                  ' GROUP BY res_SIGLA_UF, res_MUNNOMEX, res_codigo_adotado,  CODESTAB, ano_nasc", "fetch_size": 65000}
          }
    }')
  
  request <- POST(url = endpoint, body = params, encode = "json")
  df_mun <- convertRequestToDF(request)
  names(df_mun) <- c('UF', 'Municipio', 'Codigo', 'CNES', 'Ano','Nascimentos')
  df_municipio <- rbind(df_municipio, df_mun)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
            "token": {
              "token": "',token,'"
            },
            "sql": {
              "sql": {"cursor": "',cursor,'"}
            }
          }')
    
    request <- POST(url = endpoint, body = params, encode = "json")
    
    if (length(content(request)$rows) == 0)
      break
    
    df_mun <- convertRequestToDF(request)
    names(df_mun) <- c('UF', 'Municipio', 'Codigo', 'CNES', 'Ano','Nascimentos')
    df_municipio <- rbind(df_municipio, df_mun)
    
  }
}

head(df_municipio)

#View(df_municipio)

dim(df_municipio)

######## salvar base em .xlsx
#write.xlsx(df_municipio, 'df_municipio.xlsx')





########### CNES 

convertRequestToDF <- function(request, column_names = c()){
  variables = unlist(content(request)$columns)
  variables = variables[names(variables) == "name"]
  if (!length(column_names)){
    column_names <- unname(variables)
  }
  values = content(request,)$rows
  df <- as.data.frame(do.call(rbind,lapply(values,function(r) {
    row <- r
    row[sapply(row, is.null)] <- NA
    rbind(unlist(row))
  } )))
  names(df) <- column_names
  return(df)
}

estados <- c('RO', 'AC', 'AM', 'RR', 'PA', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN',
             'PB', 'PE', 'AL', 'SE', 'BA', 'MG', 'ES', 'RJ', 'SP', 'PR', 'SC',
             'RS', 'MS', 'MT', 'GO', 'DF')

df_cnes <- data.frame()

for (estado in estados){
  
  # Partos prematuros por município
  params = paste0('{
          "token": {
            "token": "',token,'"
          },
          "sql": {
            "sql": {"query":"SELECT uf_SIGLA_UF, mun_codigo_adotado, mun_MUNNOMEX, CODUFMUN, CNES, PF_PJ, COD_IR, VINC_SUS, def_retencao, def_atividad, def_tp_prest, QTINST34, QTINST35, QTINST36, QTINST37, CENTROBS, QTLEIT32, QTLEIT34, ano_competen, mes_competen, COUNT(1)',
                  ' FROM \\"datasus-cnes\\"',
                  ' WHERE (uf_SIGLA_UF = \'',estado,'\') AND (ano_competen BETWEEN 2014 AND 2020)',
                  ' GROUP BY uf_SIGLA_UF, mun_codigo_adotado, mun_MUNNOMEX, CODUFMUN, CNES, PF_PJ, COD_IR, VINC_SUS, def_retencao, def_atividad, def_tp_prest, QTINST34, QTINST35, QTINST36, QTINST37, CENTROBS, QTLEIT32, QTLEIT34, ano_competen, mes_competen", "fetch_size": 65000 }
          }
        }')
  
  request <- POST(url = endpoint, body = params, encode = "json")
  
  df_inicial <- convertRequestToDF(request)
  names(df_inicial) <- c('UF', 'mun_adotado', 'municipio_estabelecimento', 'cod_mun','CNES', 
                         'pf_pj', 'IR', 'vinc_sus', 'def_retencao', 'def_atividad', 'def_tp_prest',
                         'salas_pre_parto', 'salas_parto_normal', 'salas_curetagem', 'salas_cirurgia',
                         'centrobs', 'leitos_recup', 'leitos_prepartos',
                         'ano_competen', 'mes_competen', 'cont')
  df_cnes <- rbind(df_cnes, df_inicial)
  
  repeat {
    
    cursor <- content(request)$cursor
    
    params = paste0('{
            "token": {
              "token": "',token,'"
            },
            "sql": {
              "sql": {"cursor": "',cursor,'"}
            }
          }')
    
    request <- POST(url = endpoint, body = params, encode = "json")
    
    if (length(content(request)$rows) == 0)
      break
    
    df_inicial <- convertRequestToDF(request, colnames(df_cnes))
    df_cnes <- rbind(df_cnes, df_inicial)
    
  }
}


df_cnes19 <- df_cnes
write.csv(df_cnes,"df_cnes.csv", row.names = FALSE)


######## salvar base em .xlsx
#write.xlsx(df_cnes, "df_cnes.xlsx")


# algorithm 03 ------------------------------------------------------------

## bibliotecas 

library(tidyverse)
library(crunch)


## dados 

dados <- readr::read_csv("dados/1_sinasc_14-20[microdatasus][BRUTO][2023-07-30].csv.gz")


## manipulacao 

dados <- dados |> 
  mutate(
    APGAR1 = as.numeric(ifelse(APGAR1 == 99, NA_character_, APGAR1)),
    APGAR5 = as.numeric(ifelse(APGAR1 == 99, NA_character_, APGAR5)),
    IDADEMAE = as.numeric(ifelse(IDADEMAE == 99, NA_character_, IDADEMAE)),
    MESPRENAT = as.numeric(ifelse(MESPRENAT > 10, NA_character_, MESPRENAT)),
    PARTO = ifelse(PARTO == 9, NA_character_, PARTO),
    PESO = as.numeric(ifelse(PESO == 9999, NA_character_, PESO)),
    QTDFILVIVO = as.numeric(ifelse(QTDFILVIVO > 10, NA_character_, QTDFILVIVO)), 
    QTDGESTANT = as.numeric(ifelse(QTDGESTANT > 15, NA_character_, QTDGESTANT))
  )


## categorizacao das variaveis, segundo sinasc 

#IDADEMAE - numérica 

#QTDFILVIVO - numérica

#PARTO

dados <- dados |> 
  mutate(
    PARTO = case_when(
      PARTO == 1 ~ "vaginal",
      PARTO == 2 ~ "cesáreo",
      TRUE ~ NA_character_
    )
  )

# CODESTAB - númerica

# APGAR1 E APGAR 5 - númericas

# PESO - númerica 

# RACACORMAE

dados <- dados |>
  mutate(
    RACACORMAE = case_when(
      RACACORMAE == 1 ~ "branca",
      RACACORMAE == 2 ~ "preta",
      RACACORMAE == 3 ~ "amarela",
      RACACORMAE == 4 ~ "parda",
      RACACORMAE == 5 ~ "indígena",
      TRUE ~ NA_character_
    )
  )

# QTDGESTANT - numérica

# CONSPRENAT - númerica

# MESPRENAT - númerica

# SEXO 

dados <- dados |> 
  mutate(
    SEXO = case_when(
      SEXO == 1 | SEXO == "M" ~ "masculino",
      SEXO == 2 | SEXO == "F" ~ "feminino",
      TRUE ~ NA_character_
    )
  )

# STTRABPART 

dados <- dados |> 
  mutate(
    STTRABPART = case_when(
      STTRABPART == 1 ~ "sim",
      STTRABPART == 2 ~ "não",
      STTRABPART == 3 ~ "não se aplica",
      TRUE ~ NA_character_
    )
  )

#STCESPARTO

dados <- dados |> 
  mutate(
    STCESPARTO = case_when(
      STCESPARTO == 1 ~ "sim",
      STCESPARTO == 2 ~ "não",
      STCESPARTO == 3 ~ "não se aplica",
      TRUE ~ NA_character_
    )
  )

# ESTCIVMAE

dados <- dados |> 
  mutate(
    ESTCIVMAE = case_when(
      ESTCIVMAE == 1 ~ "solteira", 
      ESTCIVMAE == 2 ~ "casada", 
      ESTCIVMAE == 3 ~ "viúva", 
      ESTCIVMAE == 4 ~ "separada judicialmente/divorciada", 
      ESTCIVMAE == 5 ~ "união estável",
      TRUE ~ NA_character_
    )
  )

# LOCNASC

dados <- dados |> 
  mutate(
    LOCNASC = case_when(
      LOCNASC == 1 ~ "hospital",
      LOCNASC == 2 ~ "outros estabelecimentos de saúde",
      LOCNASC == 3 ~ "domicílio",
      LOCNASC == 4 ~ "outros",
      LOCNASC == 5 ~ "aldeia indígena",
      TRUE ~ NA_character_
    )
  )

# ESCMAE2010

dados <- dados |> 
  mutate(
    ESCMAE2010 = case_when(
      ESCMAE2010 == 0 ~ "sem escolaridade",
      ESCMAE2010 == 1 ~ "fundamental 1 (1 a 4 série)",
      ESCMAE2010 == 2 ~ "fundamental 2 (5 a 8 série)",
      ESCMAE2010 == 3 ~ "médio",
      ESCMAE2010 == 4 ~ "superior incompleto",
      ESCMAE2010 == 5 ~ "superior completo",
      TRUE ~ NA_character_
    )
  )

# GRAVIDEZ

dados <- dados |> 
  mutate(
    GRAVIDEZ = case_when(
      GRAVIDEZ == 1 ~ "única",
      GRAVIDEZ == 2 ~ "dupla",
      GRAVIDEZ == 3 ~ "tripla ou mais",
      TRUE ~ NA_character_
    )
  )

# GESTACAO

dados <- dados |> 
  mutate(
    GESTACAO = case_when(
      GESTACAO == 1 ~ "menos de 22 semanas",
      GESTACAO == 2 ~ "de 22 a 27 semanas",
      GESTACAO == 3 ~ "de 28 a 31 semanas",
      GESTACAO == 4 ~ "de 32 a 36 semanas",
      GESTACAO == 5 ~ "de 37 a 41 semanas",
      GESTACAO == 6 ~ "de 42 semanas ou mais", 
      TRUE ~ NA_character_
    )
  )

# CONSULTAS

dados <- dados |> 
  mutate(
    CONSULTAS = case_when(
      CONSULTAS == 1 ~ "nenhuma",
      CONSULTAS == 2 ~ "de 1 a 3",
      CONSULTAS == 3 ~ "de 4 a 6",
      CONSULTAS == 4 ~ "7 e mais",
      TRUE ~ NA_character_
    )
  )

# IDANOMAL

dados <- dados |> 
  mutate(
    IDANOMAL = case_when(
      IDANOMAL == 1 ~ "sim",
      IDANOMAL == 2 ~ "não",
      TRUE ~ NA_character_
    )
  )

# CODMUNNASC - numérica

# RACACOR

dados <- dados |> 
  mutate(
    RACACOR = case_when(
      RACACOR == 1 ~ "branca",
      RACACOR == 2 ~ "preta",
      RACACOR == 3 ~ "amarela",
      RACACOR == 4 ~ "parda",
      RACACOR == 5 ~ "indígena",
      TRUE ~ NA_character_
    )
  )

# CODMUNRES - númerica

# ano - númerica 

## salva base em .csv.gz 

readr::write_csv(dados, paste0("dados/sinasc_14-20[CATEGORIZADOS][BRUTO][", Sys.Date(), "].csv.gz"))


# algorithm 04 ------------------------------------------------------------

## bibliotecas 

library(dplyr)
library(janitor)
library(stringr)


## dados

# sinasc 14-20

d_sinasc_20142020 <- readr::read_csv("dados/3_sinasc_14-20[MARINA][2023-08-05].csv.gz")

# codigos de municipios, segundo ibge

d_ibge <- readxl::read_excel("RELATORIO_DTB_BRASIL_DISTRITO_2022.xls", skip = 6) |> 
  clean_names() |> 
  distinct(codigo_municipio_completo, .keep_all = TRUE) |> 
  # removendo digito verificador do municipio
  mutate(cod_mun = str_sub(codigo_municipio_completo, end = 6)) |> 
  rename(nome_mun = nome_municipio) |> 
  select(cod_mun, nome_mun, nome_uf) 


## uniao sinasc + codigos de municipio ibge 

d_sinasc_ibge <- d_sinasc_20142020 |> 
  rename(cod_mun = CODMUNNASC) |> 
  mutate_if(is.numeric, as.character) |> 
  left_join(d_ibge, by = "cod_mun") |> 
  relocate(ano, nome_uf, cod_mun, nome_mun, CODESTAB)


## salva base em .csv.gz 

readr::write_csv(d_sinasc_ibge, paste0("dados/sinasc_ibge_14-20[", Sys.Date(), "].csv.gz"))


# algorithm 05 ------------------------------------------------------------

## bibliotecas 

library(dplyr)
library(janitor)
library(tidyr)
library(stringr)


## dados 

d_sinasc_ibge <- readr::read_csv("dados/4_sinasc_ibge_14-20[2023-08-05].csv.gz") #20.302.485

d_sinasc_ibge <- d_sinasc_ibge |>
  clean_names() |> 
  # removendo observacoes que nao tem codigo do estabelecimento de saude
  filter(!is.na(codestab)) |> #20.115.702
  # removendo codigo de municipio de residencia
  select(-codmunres) |> 
  mutate_if(is.numeric, as.character) |> 
  mutate(chave_id = paste0(cod_mun, "-", codestab)) |> 
  relocate(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab)


## variaveis em formato wide 

d_codestab <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |> 
  dplyr::count(codestab) |> 
  rename(n_codestab = n)

d_idademae <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |> 
  dplyr::count(idademae) |> 
  spread(key = idademae, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("idade_", .x), c(7:9)) 

d_qtdfilvivo <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |> 
  dplyr::count(qtdfilvivo) |> 
  spread(key = qtdfilvivo, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("qtdfilvivo_", .x), c(7:9))

d_parto <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |> 
  dplyr::count(parto) |> 
  spread(key = parto, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("parto_", .x), c(7:8))

d_apgar1 <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |> 
  dplyr::count(apgar1) |> 
  spread(key = apgar1, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("apgar1_", .x), c(7:9))

d_apgar5 <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(apgar5) |> 
  spread(key = apgar5, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("apgar5_", .x), c(7:9))

d_peso <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(peso) |> 
  spread(key = peso, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("peso_", .x), c(7:11))

d_racacormae <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(racacormae) |> 
  spread(key = racacormae, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("racacormae_", .x), c(7:9))

d_qtdgestant <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(qtdgestant) |> 
  spread(key = qtdgestant, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("qtdgestant_", .x), c(7:9))

d_consprenat <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(consprenat) |> 
  spread(key = consprenat, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("consprenat_", .x), c(7:9))

d_mesprenat <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(mesprenat) |> 
  spread(key = mesprenat, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("mesprenat_", .x), c(7:8))

d_sexo <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(sexo) |> 
  spread(key = sexo, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("sexo_", .x), c(7:8))

d_estcivmae <- d_sinasc_ibge|>
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(estcivmae) |> 
  spread(key = estcivmae, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("estcivmae_", .x), c(7:8))

d_locnasc <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(locnasc) |> 
  spread(key = locnasc, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("locnasc_", .x), c(7:8))

d_escmae2010 <- d_sinasc_ibge|>
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(escmae2010) |> 
  spread(key = escmae2010, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("escmae2010_", .x), c(7:12))

d_gravidez <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(gravidez) |> 
  spread(key = gravidez, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("gravidez_", .x), c(7:8))

d_gestacao <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(gestacao) |> 
  spread(key = gestacao, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("gestacao_", .x), c(7:12))

d_racacor <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(racacor) |> 
  spread(key = racacor, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("racacor_", .x), c(7:9))

d_tipopremat <- d_sinasc_ibge|> 
  group_by(chave_id, ano, nome_uf, cod_mun, nome_mun, codestab) |>  
  dplyr::count(tipo_premat) |> 
  spread(key = tipo_premat, value = n) |> 
  clean_names() |> 
  rename_with(~str_c("tipopremat_", .x), c(7:9))


## uniao das bases wide 

d_sinasc_estab <- d_codestab |> 
  left_join(d_apgar1, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |>  
  left_join(d_apgar5, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_consprenat, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_escmae2010, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_estcivmae, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_gestacao, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_gravidez, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_idademae, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_locnasc, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_mesprenat, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_parto, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_peso, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_qtdfilvivo, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_qtdgestant, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_racacor, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_racacormae, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_sexo, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  left_join(d_tipopremat, by = c("chave_id", "ano", "nome_uf", "cod_mun", "nome_mun", "codestab")) |> 
  # removendo informacoes nao validas
  select(-c(contains(c("na.x", "na.y"))))


## salva base em .csv.gz 

readr::write_csv(d_sinasc_estab, paste0("dados/sinasc_ibge_14-20[WIDE][", Sys.Date(), "].csv.gz"))


# algorithm 06 ------------------------------------------------------------

library(tidyverse)
library(crunch)

d_premat <- readr::read_csv("5_sinasc_ibge_14-20[WIDE][2023-08-05].csv.gz") 

d_cnes <- readr::read_csv("df_cnes_12.csv")

d_ibge <- readxl::read_xlsx("classificacao_ibge.xlsx")

d_premat <- d_premat |> 
  rename(CNES = codestab) |> 
  rename(municipio_estabelecimento = nome_mun) |> 
  rename(ano_competen = ano)

d_ibge <- d_ibge |> 
  rename(cod_mun = IBGE) |> 
  rename(municipio_estabelecimento = MUNICIPIO) |> 
  rename(sigla = UF)

d_premat$CNES <- as.numeric(d_premat$CNES)

d_premat <- d_premat |> 
  mutate(municipio_estabelecimento = municipio_estabelecimento |> 
           str_trim() |> 
           str_remove_all("[:punct:]") |> 
           abjutils::rm_accent() |> 
           str_trim() |>  
           str_to_upper()
  )

d_cnes <- d_cnes |> 
  mutate(municipio_estabelecimento = municipio_estabelecimento |> 
           str_trim() |> 
           str_remove_all("[:punct:]") |> 
           abjutils::rm_accent() |> 
           str_trim() |>  
           str_to_upper()
  )

d_ibge <- d_ibge |> 
  mutate(municipio_estabelecimento = municipio_estabelecimento |> 
           str_trim() |> 
           str_remove_all("[:punct:]") |> 
           abjutils::rm_accent() |> 
           str_trim() |>  
           str_to_upper()
  )

dados_sc

dados_sc <- left_join(d_premat, d_cnes, by = c("municipio_estabelecimento", "CNES", "cod_mun", "ano_competen"))

faltantes <- dados_sc |> 
  filter(is.na(mes_competen))

nrow(faltantes) #quantidade de dados que não bateram ao unir sinasc e cnes (erros entre as bases sinasc e cnes)

dados_socio <- readxl::read_xlsx("dados_socio.xlsx")

dados_socio <- dados_socio |> 
  rename(municipio_estabelecimento = municipio) |> 
  rename(nome_uf = estado)

dados_sc <- dados_sc |>
  mutate(nome_uf = nome_uf |>
           str_trim() |>
           str_remove_all("[:punct:]") |>
           abjutils::rm_accent() |>
           str_trim() |>
           str_to_upper()
  )

dados_scs <- left_join(dados_sc, dados_socio, by = c("municipio_estabelecimento", "nome_uf"))

dados_scs <- left_join(dados_scs, d_ibge, by = c("municipio_estabelecimento", "sigla"))

dados_scs <- dados_scs %>% 
  mutate(CLASS_GEO2 = case_when(
    CLASS_GEO == "Intermediário Adjacente" |  CLASS_GEO == "Intermediário Remoto" ~ "Inter.",
    CLASS_GEO == "Rural Adjacente" |  CLASS_GEO == "Rural Remoto" ~ "Rural",
    CLASS_GEO == "Urbano"  ~ "Urbano")
  )

dados_scs$class_geo2 <- factor(dados_scs$class_geo2, levels= c("Rural", "Inter.", "Urbano"))


# faltantes2 <- dados_scs |> 
#   filter(is.na(idhm_longevidade_1991_municipio)) 
# 
# nrow(faltantes2) # quantidade de dados que não bateram ao unir sinasc-cnes e dados socio (erros entre as bases sinasc-cnes e dados socio)

write.csv.gz(dados_scs, '6_sinasc_ibge_socio_14-20[WIDE][2023-10-02].csv.gz', sep = ";", dec = ".", row.names = FALSE)


# algorithm 07 ------------------------------------------------------------

library(dplyr)
library(janitor)

d_sics20142020 <- readr::read_csv("02.dados/6_sinasc_ibge_cnes_socio_14-20[WIDE][2023-10-05].csv")

d_sics <- d_sics20142020 |> 
  clean_names() |> 
  select(
    -c(uf, mun_adotado, mes_competen, cont, consprenat_na, cod_mun_y, 
       pf_pj, ir, centrobs, est_pop, qtd_prof, valor_total, class_geo,
       contains(c("def", "salas", "leitos", "1991", "2000", "renda", 
                  "educacao", "longevidade", "percent")))
  ) |> 
  rename(
    ano = ano_competen, sigla_uf = sigla, 
    cod_mun = cod_mun_x, nome_mun = municipio_estabelecimento, 
    n_cnes = n_codestab, tipo_cnes = class_geo2
  ) |> 
  relocate(chave_id, ano, nome_uf, sigla_uf, cod_mun, nome_mun, cnes, tipo_cnes)

readr::write_csv(d_sics, paste0("02.dados/7_base_final_premat_14-20[", Sys.Date(), "].csv.gz"))

