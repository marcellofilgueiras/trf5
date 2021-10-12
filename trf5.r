
# Scrapper TRF-5
# Marcello Silveira Filgueiras


library(tidyverse)
library(httr)
library(abjutils)
library(jsonlite)


# Acessando a API ----------------------------------------------------------------

# Vamos descobrir como a API funciona.

# Aqui está a URL base.

url_base <- "https://julia-pesquisa.trf5.jus.br/julia-pesquisa/api/documentos:dt?"


###### Teste Super Específico 
# Ao fazer uma pesquisa na Jurisprudência pelo texto de "28,86%" em referência a uma tese consolidada
# sobre direito aumento de salários de professores federais no montante de 28,86%,
# do Relator Alcides Saldanha Lima, da 3ª Turma recursal, num espaço de 10 anos,
# estes são os parâmetros que retornam do navegador:

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

# Podemos ver então que, de inpicio, todos os itens de interesse para pesquisa
#estão bem definidos nos seguintes parameteros:

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

#Poderíamos saber, mas essa busca retornou apenas um julgado.
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
          httr::write_disk("data_raw/teste/teste7.json", overwrite = TRUE))


# query start = 0
teste4 <- jsonlite::read_json("data_raw/teste/teste4.json", simplifyDataFrame=TRUE) %>%
  pluck("data")
# query start = 10
teste5 <- jsonlite::read_json("data_raw/teste/teste5.json", simplifyDataFrame=TRUE) %>%
  pluck("data")

# query start = 20
teste6 <- jsonlite::read_json("data_raw/teste/teste6.json", simplifyDataFrame=TRUE) %>%
  pluck("data")



join <- full_join(teste4, teste5)%>%
  full_join(teste6)



# Vamos Criando a Função!

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
 
 barra_progresso <- progress::progress_bar $ new(total = length(n_starts))
 
 purrr::map(.x= n_starts,
            ~{
              barra_progresso$tick()
              
              httr::GET(url_base,
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
            httr::write_disk(path = paste0(diretorio, 
                                    "/",
                                   "julgados_",
                                  as.character(.x),
                                 "_",
                                Sys.time()%>%
                                 stringr::str_replace_all("\\D","_"),
                              ".json"))
            )}
              )
           
 
}


trf5_baixar_cjsg(pesquisa_livre = "28,86%",
                diretorio = "data_raw/teste")




# Lendo e Iterando -------------------------------------------------------------------


trf5_ler_cjsg <- function(diretorio= ""){
  
  arquivos <- base::list.files(paste0(diretorio,"/"),
                   pattern = "\\.json$", 
                   full.names = TRUE) 
  
  barra_progresso <- progress::progress_bar $ new(total = length(arquivos))
  
  purrr::map_df( .x= arquivos,
                  .f = ~{
                    barra_progresso$tick()
                    
                    jsonlite::fromJSON (.x, simplifyDataFrame = TRUE )%>%
                   purrr::pluck("data")
                    })
}




#FUNCIONOU CARALHOOOOOOOOOOOOOOOOOOOOOOO
#Fazendo exemplo em outro código
