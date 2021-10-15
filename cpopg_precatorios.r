# Precatórios
# Autor: Marcello Silveira Filgueiras


library(tidyverse)
library(httr)
library(xml2)
library(rvest)

#Lista de Julgados

adufb_processos_raw <- readxl::read_excel("cpopg/data_raw/adufb_processos.xlsx", 
                              skip = 3)


adufb_processos_tidy <- adufb_processos_raw %>%
  janitor::clean_names() %>%
  tidyr::fill(c(qtd, processo_execucao, situacao),
              .direction = "down")%>%
  tidyr::drop_na() %>%
  mutate(across(.cols = c(qtd:precatorio_rpv),
                .fns = stringr::str_squish))

processos_precatorios_ind <- adufb_processos_tidy %>%
  pull(processo_precatorio_rpv)

processos_precatorios_exemplo <- c( "0331616-40.2020.4.05.0000" ,
                                  "0313610-82.2020.4.05.0000",
                                  "0313609-97.2020.4.05.0000")
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


trf5_ler_cpopg <- function(diretorio= "") {
  
  # Faça a lista de Arquivos presente na pasta
  arquivos_lista <-   base::list.files(paste0(diretorio,"/"),
                               pattern = "\\.html$", 
                               full.names = TRUE) 
  
  #Faça o tamanho da barra de progresso
  barra_progresso <- progress::progress_bar $ new(total = 
                                                   length(arquivos_lista)
                                                  )
  
  #Leia os Arquivos e Parseia
   purrr::map( .x= arquivos_lista,
               .f = ~{
               barra_progresso$tick()
  
              arquivos_raw <- xml2::read_html (.x)
               
              processo <- arquivos_raw %>%
                        xml2::xml_find_first("/html/body/p[2]") %>%
                          xml2::xml_text() %>%
                stringr::str_remove("\n      PROCESSO Nº ")
               
               precatorio<- arquivos_raw %>%
                 xml2::xml_find_first("/html/body/table") %>%
                 xml2::xml_find_all("./tr/td") %>% xml_text()
               
               #tibble::as_tibble(processo
                                 #, precatorio
                #                 )
               
              list(processo,
                   precatorio)

  })
  
  
}


trf5_ler_cpopg(diretorio = "cpopg/data_raw")



a<- trf5_ler_cpopg(diretorio = "cpopg/data_raw")


  map(a,pluck())

as_tibble()
