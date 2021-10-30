#Tese dos "28,86%"

library(tidyverse)

# Baixando Dados ----------------------------------------------------------


#trf5_baixar_cjsg(pesquisa_livre = "28,86%",
#                diretorio = "cjsg/data_raw")


# Lendo -------------------------------------------------------------------

julgados_28_raw <- trf5_ler_cjsg(diretorio= "cjsg/data_raw") %>%
  janitor::clean_names()

julgados_28_raw%>%
  glimpse()


# Tidying -----------------------------------------------------------------

#Órgão Julgadores estão padronizados
julgados_28_raw %>%
  count(orgao_julgador)

#A Classe de cada Recurso não
julgados_28_raw %>%
  count(classe_judicial) #%>% view()

julgados_28_tidy <- julgados_28_raw %>%
  mutate(across(.cols = c(data_autuacao, data_julgamento, data_assinatura),
                .fns = lubridate::ymd
                )
         ) %>%
  mutate(classe_judicial_raw= abjutils::rm_accent(classe_judicial)%>%
                            str_to_lower(),
         classe_judicial= case_when(
           str_detect(classe_judicial_raw, "embargos de declaracao") ~ "Embargos de Declaração",
           #str_detect(classe_judicial_raw, "inominado|inonimado") ~ "Agravo Inominado",
           str_detect(classe_judicial_raw, "regimental") ~ "Agravo Regimental",
           #str_detect(classe_judicial_raw, "agravo n|agravo retido") ~ "Agravo",
           #str_detect(classe_judicial_raw, "agravo interno") ~ "Agravo Interno",
           str_detect(classe_judicial_raw, "agravo de instrumento") ~ "Agravo de Instrumento",
           str_detect(classe_judicial_raw, "remessa|reexame") ~ "Remessa Necessária",
           str_detect(classe_judicial_raw, "embargos a execucao") ~ "Embargos à Execução",
           #str_detect(classe_judicial_raw, "embargos infringentes") ~ "Embargos Infringentes",
           #str_detect(classe_judicial_raw, "divergencia") ~ "Embargos de Divergência",
           #str_detect(classe_judicial_raw, "questao de ordem") ~ "Questão de Ordem",
           #str_detect(classe_judicial_raw, "conflito") ~ "Conflito de Competência",
           str_detect(classe_judicial_raw, "^apelacao") ~ "Apelação",
           str_detect(classe_judicial_raw, "acao rescisoria") ~ "Ação Rescisória",
           TRUE ~ "Outros"
                                    ),
         ano_julgamento= lubridate::year(data_julgamento),
         autuacao_julgamento = data_julgamento - data_autuacao
         )

julgados_28_tidy %>%
  #filter(classe_judicial == "Ação Rescisória") %>%
  select(numero_processo,
         classe_judicial,
         relator,
         orgao_julgador,
        # referencia,
         data_autuacao,
         data_julgamento,
         autuacao_julgamento,
         ementa
         ) %>% view()


  glimpse()


julgados_28_tidy %>%
  count(classe_judicial)

julgados_28_tidy %>% 
  group_by(classe_judicial) %>%
  summarise(median(data_julgamento - data_autuacao, na.rm = TRUE),
            mean(data_julgamento - data_autuacao, na.rm = TRUE)) %>%
  rename("mediana julgamento - autuacao" =2,
         "média julgamento - autuacao" = 3)


julgados_28_tidy %>%
  count(ano_julgamento)%>%
  mutate(ano_julgamento= as.Date(ano_julgamento))


# Visualização ------------------------------------------------------------

# Todos Porcessos por Data de Julgamento

julgados_28_tidy%>%
  #filter(ano_julgamento>2015) %>%
  group_by(ano_julgamento)%>%
  count(classe_judicial)%>%
  ggplot(aes(x=ano_julgamento, y=n, fill=classe_judicial)) +
  geom_col(color="black") +
  labs(title = "Número de Julgados por Ano que mencinonam \"28,86%\" no TRF-5",
       subtitle = "Dividos em Classe de Acórdãos") +
  scale_fill_discrete(name= "Classe de Acórdãos") +
  #scale_x_date(name= "Ano do Julgamento", date_breaks = "5 years")+
  xlab(label= "Ano do Julgamento") +
  scale_y_continuous(name= "Nº de Julgados")


# Histogramas

# Histograma duração das Ações Rescisórias

julgados_28_tidy %>%  
  group_by(classe_judicial ) %>% 
  summarise(data_julgamento - data_autuacao) %>% 
  filter(classe_judicial == "Ação Rescisória") %>% 
  ggplot(aes(x= `data_julgamento - data_autuacao`)) + 
  geom_histogram( fill= "blue", color= "black") + 
  ylab(label= "Número de Processos")+
  xlab(label= "Dias entre a Data de Julgamento e a Data de Autuação do Recurso") +
  scale_x_continuous(breaks = c(0,  180,  seq(365,6000, 365) ) ) +
  labs (title = "Efeito da Interposição do Recurso: Número de Dias da sua Autuação até o Julgamento",
        subtitle = "Nas Ações Rescisórias, a mediana desses números é de 495 dias. A média, 600 dias.")+
  theme_update()




#hist mais desnsity
julgados_28_tidy %>% 
  group_by(classe_judicial ) %>%
  summarise(data_julgamento - data_autuacao) %>%
  filter(classe_judicial == "Ação Rescisória"
        # , `data_julgamento - data_autuacao`>= -1
         ) %>% 
  ggplot(aes(x= `data_julgamento - data_autuacao`)) +
  geom_histogram( aes(y = ..density..), color= "black") +
  geom_density(fill= "blue", alpha= 0.2)+
  ylab(label= "Número de Processos") +
  xlab(label= "Dias entre a Data de Julgamento e a Data de Autuação do Recurso") +
  #scale_x_continuous(breaks = c(0,  180,  seq(365,6000, 365) ) ) +
  labs (title = "Efeito da Interposição do Recurso: Número de Dias da sua Autuação até o Julgamento",
        subtitle = "Dentre os maiores grupos de Recursos")


# Histograma facetado por tipo de Ação

# Histograma duração das Ações Rescisórias
tempo_julg<- julgados_28_tidy %>% 
  #contando
  group_by(classe_judicial ) %>%
  summarise(data_julgamento - data_autuacao) %>%
 filter(`data_julgamento - data_autuacao` <=10000)

tempo_julg%>%
  #visualizando
  ggplot(aes(x= `data_julgamento - data_autuacao`)) +  
  geom_histogram( fill= "blue", color= "grey10") +
  facet_wrap( facets = vars(classe_judicial))+
  ylab(label= "Número de Processos") +
  xlab(label= "Efeito da Interposição do Recurso: Dias da sua Autuação até o Julgamento") +
  labs (title = "Número de Dias da Autuação do Recurso até Julgamento",
        subtitle = "9081 julgados que retornaram do TRF5 ao buscar por \"28,86%\" ")
  #scale_x_continuous(breaks = c(0,  365, 720, ))


# Histograma duração dos processos
julgados_28_tidy %>% 
  filter(classe_judicial == "Ação Rescisória") %>%
  group_by(ano_julgamento) %>%
  filter(ano_julgamento > 2013) %>%
  summarise(data_julgamento - data_autuacao) %>%
  ggplot(aes(x= `data_julgamento - data_autuacao`, fill= ano_julgamento)) +
               geom_histogram(color= "black") +
  scale_fill_discrete() +
  ylab(label= "Número de Processos") 
# Tipo de Julgado por ano




  

#julgados_28_tidy %>%
#  select(numero_processo, classe_judicial, classe_judicial_raw,
 #        data_julgamento, data_autuacao, ementa) %>%
  #DT::datatable(extensions = "Responsive",
   #             filter = "top")
