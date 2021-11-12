# Precatórios
# Autor: Marcello Silveira Filgueiras


library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(lubridate)


# Baixando e Iterando -----------------------------------------------------



trf5_baixar_cpo_fisico <- function(processos = "",
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

# processos_precatorios_ind <- c("0331616-40.2020.4.05.0000", "0313610-82.2020.4.05.0000", "0313609-97.2020.4.05.0000", "0313608-15.2020.4.05.0000", "0313607-30.2020.4.05.0000", "0313611-67.2020.4.05.0000", "0313498-16.2020.4.05.0000", "0313496-46.2020.4.05.0000", "0313492-09.2020.4.05.0000", "0313491-24.2020.4.05.0000", "0313490-39.2020.4.05.0000", "0313497-31.2020.4.05.0000","0321743-16.2020.4.05.0000", "0321747-53.2020.4.05.0000", "0321748-38.2020.4.05.0000", "0321753-60.2020.4.05.0000", "0321777-88.2020.4.05.0000", "0321775-21.2020.4.05.0000", "0321776-06.2020.4.05.0000", "0340656-46.2020.4.05.0000" ,"0321754-45.2020.4.05.0000", "0331612-03.2020.4.05.0000", "0331916-02.2020.4.05.0000", "0331917-84.2020.4.05.0000","0331918-69.2020.4.05.0000" ,"0331919-54.2020.4.05.0000", "0331920-39.2020.4.05.0000", "0331958-51.2020.4.05.0000", "0331933-38.2020.4.05.0000", "0331934-23.2020.4.05.0000", "0331944-67.2020.4.05.0000", "0331945-52.2020.4.05.0000" ,"0331946-37.2020.4.05.0000", "0331921-24.2020.4.05.0000" ,"0342223-15.2020.4.05.0000", "0342224-97.2020.4.05.0000", "0342225-82.2020.4.05.0000", "0342226-67.2020.4.05.0000", "0342269-04.2020.4.05.0000", "0342228-37.2020.4.05.0000", "0342229-22.2020.4.05.0000", "0342267-34.2020.4.05.0000","0342268-19.2020.4.05.0000", "0342227-52.2020.4.05.0000", "0331903-03.2020.4.05.0000", "0331904-85.2020.4.05.0000", "0331922-09.2020.4.05.0000", "0331923-91.2020.4.05.0000", "0331924-76.2020.4.05.0000", "0330972-97.2020.4.05.0000" ,"0331935-08.2020.4.05.0000", "0331936-90.2020.4.05.0000", "0331947-22.2020.4.05.0000", "0331948-07.2020.4.05.0000", "0331949-89.2020.4.05.0000", "0331925-61.2020.4.05.0000", "0341656-81.2020.4.05.0000" ,"0341787-56.2020.4.05.0000" ,"0341788-41.2020.4.05.0000", "0341789-26.2020.4.05.0000", "0341790-11.2020.4.05.0000", "0341649-89.2020.4.05.0000", "0341792-78.2020.4.05.0000", "0341793-63.2020.4.05.0000", "0341794-48.2020.4.05.0000", "0341795-33.2020.4.05.0000",  "0341648-07.2020.4.05.0000", "0341791-93.2020.4.05.0000", "0331647-60.2020.4.05.0000", "0331905-70.2020.4.05.0000", "0331906-55.2020.4.05.0000", "0331907-40.2020.4.05.0000", "0331908-25.2020.4.05.0000", "0331926-46.2020.4.05.0000" "0331959-36.2020.4.05.0000", "0331937-75.2020.4.05.0000", "0331938-60.2020.4.05.0000", "0331939-45.2020.4.05.0000", "0331950-74.2020.4.05.0000", "0331951-59.2020.4.05.0000", "0331927-31.2020.4.05.0000", "0341111-11.2020.4.05.0000", "0341112-93.2020.4.05.0000", "0341113-78.2020.4.05.0000", "0341114-63.2020.4.05.0000", "0341120-70.2020.4.05.0000", "0341121-55.2020.4.05.0000", "0341145-83.2020.4.05.0000", "0341125-92.2020.4.05.0000", "0341141-46.2020.4.05.0000", "0341142-31.2020.4.05.0000", "0341143-16.2020.4.05.0000", "0341144-98.2020.4.05.0000", "0341122-40.2020.4.05.0000", "0331651-97.2020.4.05.0000", "0331655-37.2020.4.05.0000", "0331910-92.2020.4.05.0000" "0331911-77.2020.4.05.0000" "0331912-62.2020.4.05.0000" "0331928-16.2020.4.05.0000" "0331909-10.2020.4.05.0000", "0331930-83.2020.4.05.0000", "0331940-30.2020.4.05.0000", "0331952-44.2020.4.05.0000", "0331953-29.2020.4.05.0000" ,"0331954-14.2020.4.05.0000", "0331955-96.2020.4.05.0000", "0331929-98.2020.4.05.0000", "0342020-53.2020.4.05.0000", "0342021-38.2020.4.05.0000" ,"0342022-23.2020.4.05.0000", "0342023-08.2020.4.05.0000", "0342024-90.2020.4.05.0000" ,"0342027-45.2020.4.05.0000", "0342026-60.2020.4.05.0000", "0342028-30.2020.4.05.0000" ,"0342029-15.2020.4.05.0000", "0342032-67.2020.4.05.0000" ,"0342033-52.2020.4.05.0000", "0342025-75.2020.4.05.0000", "0342294-17.2020.4.05.0000", "0342295-02.2020.4.05.0000", "0342300-24.2020.4.05.0000", "0342302-91.2020.4.05.0000", "0342338-36.2020.4.05.0000", "0342327-07.2020.4.05.0000", "0342328-89.2020.4.05.0000", "0342336-66.2020.4.05.0000", "0342337-51.2020.4.05.0000", "0342323-67.2020.4.05.0000", "0230296-10.2021.4.05.0000", "0230260-65.2021.4.05.0000", "0230261-50.2021.4.05.0000", "0230262-35.2021.4.05.0000", "0230259-80.2021.4.05.0000", "0230263-20.2021.4.05.0000", "0230264-05.2021.4.05.0000" ,"0230265-87.2021.4.05.0000", "0230266-72.2021.4.05.0000", "0230267-57.2021.4.05.0000", "0230135-97.2021.4.05.0000", "0340703-20.2020.4.05.0000", "0340722-26.2020.4.05.0000", "0340721-41.2020.4.05.0000", "0340720-56.2020.4.05.0000", "0340712-79.2020.4.05.0000", "0340711-94.2020.4.05.0000", "0340710-12.2020.4.05.0000" ,"0340709-27.2020.4.05.0000" ,"0340708-42.2020.4.05.0000", "0340707-57.2020.4.05.0000", "0340655-61.2020.4.05.0000", "0340702-35.2020.4.05.0000", "0340713-64.2020.4.05.0000" ,"0340704-05.2020.4.05.0000", "0340705-87.2020.4.05.0000", "0340706-72.2020.4.05.0000", "0342274-26.2020.4.05.0000", "0340718-86.2020.4.05.0000", "0340701-50.2020.4.05.0000", "0340700-65.2020.4.05.0000", "0340699-80.2020.4.05.0000", "0340698-95.2020.4.05.0000", "0340697-13.2020.4.05.0000", "0340696-28.2020.4.05.0000", "0340695-43.2020.4.05.0000", "0340694-58.2020.4.05.0000", "0340693-73.2020.4.05.0000", "0340692-88.2020.4.05.0000", "0340777-74.2020.4.05.0000" ,"0340776-89.2020.4.05.0000", "0340719-71.2020.4.05.0000" ,"0340775-07.2020.4.05.0000" )


#Funciona para processsos físicos

#trf5_baixar_cpopg(processos = processos_precatorios_ind,
 #                diretorio = "cpopg/data_raw")

#Não funciona para processos eletrônicos
#trf5_baixar_cpopg(processos = processos_precatorios_col,
 #               diretorio = "cpopg/data_raw/exec_coletivas")


# Lendo e Iterando: Dados do Processo -------------------------------------



trf5_cpo_fisico_ler_processos <- function(diretorio= "") {
  
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




a<- trf5_cpo_fisico_ler_processos(diretorio = "cpopg/data_raw")


  
# # Lendo e Iterando: Partes do Processo ----------------------------------

  
  
  
trf5_cpo_fisico_ler_partes <- function(diretorio= "") {
    
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

  
b<-  trf5_cpo_fisico_ler_partes(diretorio = "cpopg/data_raw")  


# Lendo e Iterando: Movimentações do Processo -----------------------------


  
  trf5_cpo_fisico_ler_movimentacoes <- function(diretorio= "") {
    
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


c<-  trf5_cpo_fisico_ler_movimentacoes(diretorio = "cpopg/data_raw")  
