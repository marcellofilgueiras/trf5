# Buscando Metadaos dos Precatórios
# Autor: Marcello Silveira Filgueiras


library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(lubridate)


# Baixando e Iterando -----------------------------------------------------



trf5_baixar_cpopg <- function(processos = "",
                              diretorio= "") {

url_base <- "https://cp.trf5.jus.br/processo/"

barra_progresso <- progress::progress_bar $ new(total = length(processos))

map (.x= processos, 
      .f= ~ { 
        
        barra_progresso$tick()
        
        
        httr::GET (paste0(url_base,
                             .x),
                      httr::write_disk(path = paste0(diretorio, 
                                                     "/",
                                                     "julgados_",
                                                     as.character(.x),
                                                     "_",
                                                     Sys.time()%>%
                                                       stringr::str_replace_all("\\D","_"),
                                                     ".html")
                                       )
                     ) }
      )
 
 
}


#trf5_baixar_cpopg(processos = processos_precatorios_ind,
 #                diretorio = "cpopg/data_raw")

trf5_baixar_cpopg(processos = processos_precatorios_col,
                diretorio = "cpopg/data_raw/exec_coletivas")


# Lendo e Iterando: Dados do Processo -------------------------------------



trf5_cpopg_ler_processos <- function(diretorio= "") {
  
  # Faça a lista de Arquivos presente na pasta
  arquivos_lista <-   base::list.files(paste0(diretorio,"/"),
                               pattern = "\\.html$", 
                               full.names = TRUE) 
  
  #Faça o tamanho da barra de progresso
  barra_progresso <- progress::progress_bar $ new(total = 
                                                   length(arquivos_lista)
                                                  )
  
  #Leia os Arquivos
   purrr::map_dfr( .x= arquivos_lista,
               .f = ~{
               barra_progresso$tick()
  
              arquivos_raw <- xml2::read_html (.x)
               
              processo <- arquivos_raw %>%
                        xml2::xml_find_first("/html/body/p[2]") %>%
                          xml2::xml_text() %>%
                stringr::str_remove("\n      PROCESSO Nº ")
               
                prec <- arquivos_raw %>%
                 xml2::xml_find_first("/html/body/table") %>%
                 xml2::xml_find_all("./tr/td") %>%
                 xml_text()
                
              
                n_precatorio <- arquivos_raw %>%
                 xml2::xml_find_first("/html/body/table") %>%
                 xml2::xml_find_all("./tr/td") %>%
                 xml_text() %>% pluck(1) %>%
                  stringr::str_extract("PRC.+\\b")
               
                data_atuacao <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(2) %>%
                    stringr::str_remove("AUTUADO EM ")
              
                orgao <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(3) %>%
                    stringr::str_remove("ORGÃO: ")
                
                n_originario <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(4) %>%
                  stringr::str_remove("PROC. ORIGINÁRIO Nº: ")
                 
                n_requisitorio <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(5) %>%
                  stringr::str_remove("NÚMERO DO REQUISITÓRIO: ")
                
                n_execucao <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(6) %>%
                  stringr::str_remove("NÚMERO DO PROCESSO DE EXECUÇÃO: ")
                
                vara <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(7) %>%
                  stringr::str_remove("VARA: ")
                
                natureza_credito <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(8) %>%
                  stringr::str_remove("CRÉDITO: ")
                
                assunto <- arquivos_raw %>%
                  xml2::xml_find_first("/html/body/table") %>%
                  xml2::xml_find_all("./tr/td") %>%
                  xml_text() %>% pluck(9) %>%
                  stringr::str_remove("ASSUNTO: ")
                
                
               tabelas <- arquivos_raw %>%
                xml2::xml_find_all("/html/body/table") %>%
                 html_table()
               
               tabelas2 <- arquivos_raw %>%
                 xml2::xml_find_all("/html/body/table[2]") %>%
                 xml2::xml_find_all("./tr/td") %>%
                 xml_text() 
               
               
               fase_atual <- tabelas2 %>% pluck(3)
               
               data_fase_atual <- tabelas2 %>% pluck(2)
               
               complemento <- tabelas2 %>% pluck(5)
               
               ultima_localizacao <- tabelas2 %>% pluck(7)
            
               
               tibble(processo,
                      n_precatorio,
                      data_atuacao,
                      orgao,
                      n_originario,
                      n_requisitorio,
                      n_execucao,
                      vara,
                      natureza_credito,
                      assunto,
                      fase_atual,
                      data_fase_atual,
                      complemento,
                      ultima_localizacao)
               
               #list(processo,
                #    prec,
                 #   tabelas,
                  #  tabelas2)
            
  })
  
  
}




a<- trf5_cpopg_ler_processos(diretorio = "cpopg/data_raw")




  

# Lendo e Iterando: Movimentações do Processo -----------------------------


  
  trf5_cpopg_ler_movimentacoes <- function(diretorio= "") {
    
    # Faça a lista de Arquivos presente na pasta
    arquivos_lista <-   base::list.files(paste0(diretorio,"/"),
                                         pattern = "\\.html$", 
                                         full.names = TRUE) 
    
    #Faça o tamanho da barra de progresso
    barra_progresso <- progress::progress_bar $ new(total = 
                                                      length(arquivos_lista)
    )
    
    #Leia os Arquivos
    purrr::map_dfr( .x= arquivos_lista,
                .f = ~{
                  barra_progresso$tick()
                  
                  arquivos_raw <- xml2::read_html (.x)
                  
                  processo <- arquivos_raw %>%
                    xml2::xml_find_first("/html/body/p[2]") %>%
                    xml2::xml_text() %>%
                    stringr::str_remove("\n      PROCESSO Nº ")
            
                  
                  #tabelas<- arquivos_raw %>%
                   # xml2::xml_find_all("/html/body/table") %>%
                    #html_table()
                  
                  
                  data_movimentacoes <- arquivos_raw %>%
                    xml2::xml_find_all("//li[@class='negrito']") %>%
                    xml2::xml_text() %>%
                    stringr::str_extract("\\d+/\\d+/\\d+") %>%
                    lubridate::dmy()
                  
                  
                  movimentacoes <- arquivos_raw %>%
                    xml2::xml_find_all("//table[@width= '87%']")%>%
                    xml2::xml_text() 
                  
                 tibble(processo,
                         #data_movimentacoes,
                         movimentacoes 
                  ) %>%
                   dplyr::filter(str_detect(movimentacoes, "Em \\d+/\\d+/\\d+")) %>%
                   tidyr::separate(movimentacoes,
                                   into = c("data_movimentacao", "movimentacao"),
                                    sep= "\\d+:\\d+",
                                   extra = "merge") %>%
                   dplyr::mutate(data_movimentacao= stringr::str_extract(data_movimentacao,"\\d+/\\d+/\\d+") %>%
                                   lubridate::dmy()
                   )
                  
                  #list(processo,
                      # tabelas,
                   #    data_movimentacoes,
                    #   movimentacoes)
                  
                })
    
    
  }


  
# # Lendo e Iterando: Partes do Processo ----------------------------------

  
  
  
trf5_cpopg_ler_partes <- function(diretorio= "") {
    
    # Faça a lista de Arquivos presente na pasta
    arquivos_lista <-   base::list.files(paste0(diretorio,"/"),
                                         pattern = "\\.html$", 
                                         full.names = TRUE) 
    
    #Faça o tamanho da barra de progresso
    barra_progresso <- progress::progress_bar $ new(total = 
                                                      length(arquivos_lista)
    )
    
    #Leia os Arquivos
    purrr::map_dfr( .x= arquivos_lista,
                .f = ~{
                  barra_progresso$tick()
                  
                  arquivos_raw <- xml2::read_html (.x)
                  
                  processo <- arquivos_raw %>%
                    xml2::xml_find_first("/html/body/p[2]") %>%
                    xml2::xml_text() %>%
                    stringr::str_remove("\n      PROCESSO Nº ")
                  
                  
                  tipo_partes <- arquivos_raw %>%
                    xml2::xml_find_first("//table[@height= '52']") %>%
                    rvest::html_table() %>%
                    pluck(1) %>%
                    str_replace("REQDO", "Requerido")%>%
                    str_replace("REQTE", "Requerente")
                  
                  
                  partes <- arquivos_raw %>%
                    xml2::xml_find_first("//table[@height= '52']") %>%
                    rvest::html_table() %>%
                    purrr::pluck(2) %>%
                    stringr::str_remove(": (\\t)*")
                  
                   tibble(processo,
                         tipo_partes,
                         partes) 
                  
                  #list(processo,
                  #     partes)
                  
                })
    
    
  }

  
b<-  trf5_cpopg_ler_partes(diretorio = "cpopg/data_raw")  

b $ partes  

