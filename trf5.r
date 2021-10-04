# Scrapper TRF-5
# Marcello Silveira Filgueiras


# Comentários Iniciais ----------------------------------------------------

#Olá sou Marcello Filgueiras, faço Direito na UFJF e me interesso por Jurimetria.
# O objetivo desse scrapper a baixar a jurisprudência, o conjunto de julgados, do TRF-5, o Tribunal Regional Federal do Nordeste,
# disponíveis nesse [buscador de julgados](https://julia-pesquisa.trf5.jus.br/julia-pesquisa/#consulta)

#Trata-se de um buscador de julgados que vai te retornar julgados a partor de 
#uma pesquisa em texto, possibilitando de filtrar por Orgão Julgador, por Relator
#que redigiu o acódão ou por período de tempo.
#ainda, é possível buscar diretamente o julgado com o número do processo específico.


# Apesar de parecer uma requisição POST o site tem uma API escondida que utiliza requisições GET.
# Os dados vêm em Json , o que possibilita muito menos trabalho no parsing.

library(tidyverse)
library(httr)
library(abjutils)
library(jsonlite)


# Acessando a API ----------------------------------------------------------------

# Vamos descobrir como a API funciona.

# Aqui está a URL base.

url_base <- "https://julia-pesquisa.trf5.jus.br/julia-pesquisa/api/documentos:dt?"


###### Teste Super Específico 

# Ao fazer uma pesquisa na JurisprudÃªncia pelo texto de "28,86%" em referência a uma tese consolidada
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

# Podemos ver então que de início, todos os itens de interesse para pesquisa
#estão bem definidos nos seguintes parâmeteros:

#"pesquisaLivre" = "28,86%",
#"numeroProcesso" = "",
#"orgaoJulgador" = "3ª TURMA",
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
# Vamos para quantidades maiores com uma requisiÃ§Ã£o mais genÃ©rica.


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


# Testes que deram certo para acessar a API (antes de fazer a função)

# Usar o Parâmetro Draw é fundamental, sem ele não é possível, retorna o seguinte erro:
#"{"status":400,"mensagem":"Required long parameter 'draw' is not present"}"

httr::GET(url_base,
          query= query_teste_generico,
          httr::write_disk("data_raw/teste7.json", overwrite = TRUE))

content(teste_generico)

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
 
 
 query_2 <- list(
   "draw" = "1",
   "columns[0][data]" = "codigoDocumento",
   "columns[0][name]" = "",
   "columns[0][searchable]" = "true",
   "columns[0][orderable]" = "false",
   "columns[0][search][value]" = "",
   "columns[0][search][regex]" = "false",
   "start" = as.character(n_starts),
   "length" = "10",
   "search[value]" = "",
   "search[regex]" = "false",
   "pesquisaLivre" = pesquisa_livre,
   "numeroProcesso" = "",
   "orgaoJulgador" = orgao_julgador,
   "relator" = relator,
   "dataIni" = data_inicial,
   "dataFim" = data_final)
 
 
 paths= paste0(diretorio, 
              "/",
              "pag_",
              as.character(seq(length(n_starts / 10 +1))),
              "_",
              Sys.time()%>%
                str_replace_all("\\D","_"),
              ".json")

 purrr::walk2(.x= query_2,
              .y= paths,
              httr::GET(url_base,
                   query= .x,
                   write_disk(path = .y)
                   ))
      
                          
                                
 
}


trf5_baixar_cjsg(pesquisa_livre = "28,86%",
                 diretorio = "data_raw")


#t1 <- trf5_baixar_cjsg(pesquisa_livre = "28,86%")

#httr::GET(url_base,
 #         query= query_1)

t1 %>%
  content() %>%
  pluck("recordsTotal")


n_starts_ex <- seq(0,n_observacoes,10)

paste0("data_raw", 
       "/",
       seq(length(n_starts / 10 +1)),
       "_",
        Sys.time()%>%
          str_replace_all("\\D","_"),
  ".json")





# Lendo e Iterando -------------------------------------------------------------------

#Uma função diferente


#leitora_content<-function(diretorio){
 # 
  #arquivos_json <- base::list.files(diretorio,
   #                                pattern = "\\.json$", 
    #                               full.names = TRUE)
  
#  map_df(arquivos_json, ~jsonlite::fromJSON(.x, simplifyDataFrame = TRUE) %>%
 #       purrr::pluck("data"))
#}

#doc <- leitora_content(diretorio = "data_raw/")


#docs_belinutri_clean <- tibble(
#  arquivos = base::list.files(
#    "belinutri/data_raw/documentos_processo/pdfs",
#    pattern = "\\.pdf$",
#    full.names = TRUE), 
#  conteudo = map_chr(
 #   arquivos,
#    ~ pdftools::pdf_text(.x) %>%
 #     reduce(paste) %>%
  #    str_to_lower()%>%
   #   stringr::str_c( collapse = "\n")),
#  classificacao = map_chr(conteudo, classificadora),
 # data_inicio = str_extract(conteudo, "\\d{1,2} de .+ de \\d{4,4}"))%>%
  #mutate( data_inicio=  stringr::str_replace(data_inicio, "janeiro", "01") %>%
   #         stringr::str_replace( "fevereiro",  "02") %>%
    #        stringr::str_replace( "marÃ§o",  "03") %>%
     #       stringr::str_replace( "abril",  "04") %>%
      #      stringr::str_replace( "maio",  "05") %>%
        #    stringr::str_replace( "junho", "06") %>%
       #     stringr::str_replace( "julho", "07") %>%
         #   stringr::str_replace( "agosto", "08") %>%
  #          stringr::str_replace( "setembro", "09") %>%
   #         stringr::str_replace( "outubro", "10") %>%
    #        stringr::str_replace( "novembro", "11") %>%
     #       stringr::str_replace( "dezembro", "12")) %>%
#  mutate(data_inicio = stringr::str_replace_all(data_inicio, " de ", "-"),
       #  data_inicio = lubridate::dmy(data_inicio),
      #   ano= lubridate::year(data_inicio),
      #   arquivos = stringr::str_remove(arquivos, "belinutri/data_raw/documentos_processo/pdfs/"),
     #    classificacao = case_when(
    #       !str_detect(arquivos, "Solicita") ~ "NÃ£o Ã© erro",
   #        str_detect(arquivos, "Imagem|imagem") ~ "Imagem",
  #         TRUE ~ as.character(classificacao)
 #        ))%>%
#  fill(ano, direction= "down")

