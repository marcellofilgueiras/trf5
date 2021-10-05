# Scrapper TRF-5
# Marcello Silveira Filgueiras


# Coment?rios Iniciais ----------------------------------------------------

#Olá sou Marcello Filgueiras, fa?o Direito na UFJF e me interesso por Jurimetria.
# O objetivo desse scrapper a baixar a jurisprud?ncia, o conjunto de julgados, do TRF-5, o Tribunal Regional Federal do Nordeste,
# dispon?veis nesse [buscador de julgados](https://julia-pesquisa.trf5.jus.br/julia-pesquisa/#consulta)

#Trata-se de um buscador de julgados que vai te retornar julgados a partor de 
#uma pesquisa em texto, possibilitando de filtrar por Org?o Julgador, por Relator
#que redigiu o ac?d?o ou por per?odo de tempo.
#ainda, ? poss?vel buscar diretamente o julgado com o n?mero do processo espec?fico.


# Apesar de parecer uma requisi??o POST o site tem uma API escondida que utiliza requisi??es GET.
# Os dados v?m em Json , o que possibilita muito menos trabalho no parsing.

library(tidyverse)
library(httr)
library(abjutils)
library(jsonlite)


# Acessando a API ----------------------------------------------------------------

# Vamos descobrir como a API funciona.

# Aqui est? a URL base.

url_base <- "https://julia-pesquisa.trf5.jus.br/julia-pesquisa/api/documentos:dt?"


###### Teste Super Espec?fico 

# Ao fazer uma pesquisa na Jurisprudência pelo texto de "28,86%" em refer?ncia a uma tese consolidada
# sobre direito aumento de sal?rios de professores federais no montante de 28,86%,
# do Relator Alcides Saldanha Lima, da 3? Turma recursal, num espa?o de 10 anos,
# estes s?o os par?metros que retornam do navegador:

query_teste_especifico<- list(
  "draw" = "1",
  "columns[0][data]" = "codigoDocumento",
  "columns[0][name]" = "",
  "columns[0][searchable]" = "true",
  "columns[0][orderable]" = "false",
  "columns[0][search][value]" = "",
  "columns[0][search][regex]" = "false",
  "start" = "0",
  "length" = "10",
  "search[value]" = "",
  "search[regex]" = "false",
  "pesquisaLivre" = "28,86%",
  "numeroProcesso" = "",
  "orgaoJulgador" = "3ª TURMA",
  "relator" = "ALCIDES SALDANHA LIMA",
  "dataIni" = "01/01/2010",
  "dataFim" = "03/10/2021",
  "_" = "1633297312330")

# Podemos ver ent?o que de in?cio, todos os itens de interesse para pesquisa
#estão bem definidos nos seguintes par?meteros:

#"pesquisaLivre" = "28,86%",
#"numeroProcesso" = "",
#"orgaoJulgador" = "3? TURMA",
#"relator" = "ALCIDES SALDANHA LIMA",
#"dataIni" = "01/01/2010",
#"dataFim" = "03/10/2021",

teste_especifico <- httr::GET(url_base, 
                      query =query_teste_especifico)

# Vendo o resultado, temos um Json, com dados em Listas

content(teste_especifico) %>%
  str(max=2)

# Mas o que tem dentro dessas listas?
# O que fazem as outras querys?

#Poder?amos saber, mas essa busca retornou apenas um julgado.
# Vamos para quantidades maiores com uma requisição mais genérica.


##### Teste Genérico


query_teste_generico <- list(
  "draw" = "1",
  "columns[0][data]" = "codigoDocumento",
  "columns[0][name]" = "",
  "columns[0][searchable]" = "true",
  "columns[0][orderable]" = "false",
  "columns[0][search][value]" = "",
  "columns[0][search][regex]" = "false",
  "start" = "0",
  "length" = "10",
  "search[value]" = "",
  "search[regex]" = "false",
  "pesquisaLivre" = "28,86%",
  "numeroProcesso" = "",
  "orgaoJulgador" = "",
  "relator" = "",
  "dataIni" = "",
  "dataFim" = "",
  "_" = "1633297312330")


teste_generico <- httr::GET(url_base, 
                              query =query_teste_generico)


content(teste_generico)%>%
  str(max=2)



# Baixando e Iterando -----------------------------------------------------


# Testes que deram certo para acessar a API (antes de fazer a fun??o)

# Usar o Par?metro Draw ? fundamental, sem ele n?o ? poss?vel, retorna o seguinte erro:
#"{"status":400,"mensagem":"Required long parameter 'draw' is not present"}"

httr::GET(url_base,
          query= query_teste_generico,
          httr::write_disk("data_raw/teste7.json", overwrite = TRUE))


# query start = 0
teste4 <- jsonlite::read_json("data_raw/teste4.json", simplifyDataFrame=TRUE) %>%
  pluck("data")
# query start = 10
teste5 <- jsonlite::read_json("data_raw/teste5.json", simplifyDataFrame=TRUE) %>%
  pluck("data")

# query start = 20
teste6 <- jsonlite::read_json("data_raw/teste6.json", simplifyDataFrame=TRUE) %>%
  pluck("data")



join <- full_join(teste4, teste5)%>%
  full_join(teste6)



# Vamos Criando a Fun??o!

trf5_baixar_cjsg <- function(pesquisa_livre = "", orgao_julgador = "",
                             relator = "", data_inicial = "", data_final = "",
                             diretorio = "") {
  
  url_base <- "https://julia-pesquisa.trf5.jus.br/julia-pesquisa/api/documentos:dt?"
  
  query_1 = list(
    "draw" = "1",
    "columns[0][data]" = "codigoDocumento",
    "columns[0][name]" = "",
    "columns[0][searchable]" = "true",
    "columns[0][orderable]" = "false",
    "columns[0][search][value]" = "",
    "columns[0][search][regex]" = "false",
    "start" = "0",
    "length" = "10",
    "search[value]" = "",
    "search[regex]" = "false",
    "pesquisaLivre" = pesquisa_livre,
    "numeroProcesso" = "",
    "orgaoJulgador" = orgao_julgador,
    "relator" = relator,
    "dataIni" = data_inicial,
    "dataFim" = data_inicial)
  
 n_observacoes <- httr::GET(url_base,
            query= query_1) %>% 
             content() %>%
             pluck("recordsTotal")
   

 n_starts <- seq(0,n_observacoes,10)
 
 
 purrr::map(.x= n_starts,
            ~httr::GET(url_base,
                       query= list(
                         "draw" = "1",
                         "columns[0][data]" = "codigoDocumento",
                         "columns[0][name]" = "",
                         "columns[0][searchable]" = "true",
                         "columns[0][orderable]" = "false",
                         "columns[0][search][value]" = "",
                         "columns[0][search][regex]" = "false",
                         "start" = as.character(.x),
                         "length" = "10",
                         "search[value]" = "",
                         "search[regex]" = "false",
                         "pesquisaLivre" = pesquisa_livre,
                         "numeroProcesso" = "",
                         "orgaoJulgador" = orgao_julgador,
                         "relator" = relator,
                         "dataIni" = data_inicial,
                         "dataFim" = data_final),
            write_disk(path = paste0(diretorio, 
                                    "/",
                                   "julgados_",
                                  as.character(.x),
                                 "_",
                                Sys.time()%>%
                                 str_replace_all("\\D","_"),
                              ".json"))
            ))
           
 
}


trf5_baixar_cjsg(pesquisa_livre = "28,86%",
                 diretorio = "data_raw")




# Lendo e Iterando -------------------------------------------------------------------


julgados_28<- map_df( .x= list.files("data_raw/",
                     pattern = "\\.json$", 
                      full.names = TRUE),
                  .f = ~ jsonlite::fromJSON (.x, simplifyDataFrame = TRUE )%>%
                          pluck("data"))


#FUNCIONOU CARALHOOOOOOOOOOOOOOOOOOOOOOO
