# Análise Precatórios

# Precatórios
# Autor: Marcello Silveira Filgueiras


library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(lubridate)

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


processos_precatorios_col <- adufb_processos_tidy %>%
  pull(processo_execucao) %>% unique()


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


#trf5_baixar_cpopg(processos = processos_precatorios_ind,
#                diretorio = "cpopg/data_raw")

#trf5_baixar_cpo_fisico(processos = processos_precatorios_col,
 #                 diretorio = "cpopg/data_raw/exec_coletivas")


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




# MOVIMENTAÇÕES -----------------------------------------------------------



# Lendo e Iterando:  Lendo Movimentações do Processo -----------------------------



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
                      xml2::xml_find_all("//table[@height= '28']")%>%
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



# Contanto Tempo entre Movimentações -----------------------------------------------------------



c <- trf5_cpo_fisico_ler_movimentacoes (diretorio = "cpopg/data_raw")  %>%
  JurisMiner::tempo_movimentacao(data = data_movimentacao)



#Classificando Movimentações                         

c2 <- c %>%
  dplyr::mutate(data_inicial = case_when(
    is.na(anterior) ~ data_movimentacao
  )) %>%
  tidyr::fill(data_inicial,
              .direction= "up") %>% 
  mutate(tipo_movimentacao =case_when(
    str_detect(movimentacao, "(?i)individualiza[çc][ãa]o") ~ "Cadastro do Precatório",
    str_detect(movimentacao, "(?i)arquivad") ~ "Arquivado",
    str_detect(movimentacao, "(?i)atualiza[cç][aã]o") ~ "Atualização de Valores",
    str_detect(movimentacao, "(?i)dep[oó]sito em conta|(?i)dep[oó]sito efetivado") ~ "Depósito dos Valores",
    str_detect(movimentacao, "(?i)atualiza[cç][aã]o") ~ "Atualização de Valores",
    str_detect(movimentacao, "(?i)instituição") ~ "Indicação do banco para pagamento",
    str_detect(movimentacao, "(?i)exclus[ãa]o|excluir") ~ "Exclusão da Restrição",
    str_detect(movimentacao, "(?i)expedição de ofício") ~ "Expedição de Ofício",
    str_detect(movimentacao, "(?i)pagamento em processamento") ~ "Pagamento em Processamento",
    str_detect(movimentacao, "Precatório foi inscrito para pagamento em 2021") ~ "Inscrito para Pagamento em 2021",
    str_detect(movimentacao, "(?i)precatório sera inscrito para pagto em 2022") ~ "Inscrito para Pagamento em 2022"),
    mes_inicial= lubridate::month(data_inicial),
    ano_inicial= lubridate::year(data_inicial),
    md_inicial= as.character(data_inicial)%>% 
      stringr::str_replace("\\d{2,2}$", "01") %>%
      lubridate::ymd()
  )






# Contanto por Ano --------------------------------------------------------


count_c2 <- c2%>%
  filter(tipo_movimentacao != is.na(tipo_movimentacao))%>%
  group_by(tipo_movimentacao,
           md_inicial) %>%
  summarise(media_ultima_mov_inic = mean(decorrencia_acumulada)%>%
              as.integer(),
            desv_padrao_ultima_mov_inic =  sd(decorrencia_acumulada),
            mediana_ultima_mov_inic = median(decorrencia_acumulada)%>%
              as.integer(),
            iqr_distancia_mediana =  IQR(decorrencia_acumulada)
            )%>%
  arrange(desc(media_ultima_mov_inic))%>%
  mutate(dias_desde_inicio=  days(media_ultima_mov_inic),
         data_media_ultimo_despacho = md_inicial +days(media_ultima_mov_inic))


data_06<- count_c2%>%
  filter(md_inicial == "2020-06-01" )

data_07<- count_c2 %>%
  filter(md_inicial == "2020-07-01" )

data_02<- count_c2 %>%
  filter(md_inicial == "2021-02-01" )


# Visualização

  ggplot() +
    geom_line( data = data_06,
              aes( y= dias_desde_inicio, x = data_media_ultimo_despacho, colour= "red")) +
    geom_line( data = data_07,
               aes( y= dias_desde_inicio, x = data_media_ultimo_despacho, colour= "blue")) +
    geom_line( data = data_02,
               aes( y= dias_desde_inicio, x = data_media_ultimo_despacho, colour= "green"))

  count_c2 %>%
  ggplot(aes( y= media_ultima_mov_inic, x = data_media_ultimo_despacho)) +
    geom_line( aes(colour= md_inicial))
                    
               
  
# Últimas Movimentações

c3 <- c2 %>%
  group_by(processo) %>%
  filter(decorrencia_acumulada == max(decorrencia_acumulada)) %>%
  distinct(processo, .keep_all = TRUE) %>%
  ungroup(processo) %>%
  mutate(
    md_final= as.character(data_movimentacao)%>% 
      stringr::str_replace("\\d{2,2}$", "01") %>%
      lubridate::ymd()
  )


c3%>%
  count(md_inicial)

count_c3 <- c3%>%
  group_by(tipo_movimentacao,
           md_inicial) %>%
  summarise(media_ultima_mov_inic = mean(decorrencia_acumulada),
            desv_padrao_ultima_mov_inic =  sd(decorrencia_acumulada),
            mediana_ultima_mov_inic = median(decorrencia_acumulada)%>%
              as.integer(),
            iqr_distancia_mediana =  IQR(decorrencia_acumulada))%>%
  arrange(desc(media_ultima_mov_inic))

# VisualizandoTempo------------------------------------------------------------

  #visualizando
c3 %>%
  ggplot(aes(x= decorrencia_acumulada)) +  
  geom_histogram( fill= "blue", color= "grey10") +
  facet_grid( facets = vars(md_inicial))+
  ylab(label= "Número de Processos") +
  xlab(label= "Número de Dias") +
  labs (title = "Número de Dias da Autuação do Recurso até Julgamento",
        subtitle = "9081 julgados que retornaram do TRF5 ao buscar por \"28,86%\" ")


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

b $ partes  




# Exportando --------------------------------------------------------------

writexl::write_xlsx(list(a,b,c2,c3),
                    "cpopg/exports/precatorios_2020.xlsx")


library(openxlsx)

openxlsx::write.xlsx(list(a,b,c2,c3),
                     "cpopg/exports/precatorios_2020_3.xlsx",
                     asTable = FALSE,
                     sheetName= c("Processos", "Partes", "Movimentações", "Últimas Movimentações"),
                     overwrite = TRUE)
style_excel <- openxlsx::createStyle(fontName = "Times New Roman",
                                     fontSize = 12)

style_header<- openxlsx::createStyle(fontName = "Times New Roman", fontColour = "black",
                                     fgFill = "red",
                halign = "center", valign = "center", textDecoration = "Bold",
                border = "TopBottomLeftRight")


map(list(data_06,
         data_07,
         data_02),
    ~relocate(.x, md_inicial, .before = tipo_movimentacao) %>%
      select(!dias_desde_inicio)%>%
      rename (  "Data Inicial" =1 ,
                "Tipo de Movimentação" =2 ,
                "Média de Dias desde o Início do Processo" =3,
                "Desvio Padrão da Média" =4,
                "Mediana de Dias desde o Início" =5,
                "IQR- Distância da Mediana" =6,
                "Data Média da Movimentação"=7))

openxlsx::write.xlsx(map(.x= list(data_06,
                              data_07,
                              data_02),
                         ~relocate(.x, md_inicial, .before = tipo_movimentacao) %>%
                           select(!dias_desde_inicio)%>%
                           rename (  "Data Inicial" =1 ,
                                     "Tipo de Movimentação" =2 ,
                                     "Média de Dias desde o Início do Processo" =3,
                                     "Desvio Padrão da Média" =4,
                                     "Mediana de Dias desde o Início" =5,
                                     "IQR- Distância da Mediana" =6,
                                     "Data Média da Movimentação"=7)),
                     "cpopg/exports/precatorios_2020_mes.xlsx",
                     asTable = FALSE,
                     sheetName= c("Prec. 2020 - 06/20",
                                  "Prec. 2020 - 07/20",
                                  "Prec. 2020 - 02/21"),
                     overwrite = TRUE,
                     headerStyle= style_header,
                     tableStyle= style_excel,
                     colWidhts= 20)
