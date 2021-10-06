#Tese dos "28,86%"

library(tidyverse)

# Baixando Dados ----------------------------------------------------------


#trf5_baixar_cjsg(pesquisa_livre = "28,86%",
#                diretorio = "data_raw")


# Lendo -------------------------------------------------------------------

julgados_28_raw <- trf5_ler_cjsg(diretorio= "data_raw") %>%
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
         ano_julgamento= lubridate::year(data_julgamento)
         )

julgados_28_tidy %>%
  count(classe_judicial)

julgados_28_tidy %>%
  count(ano_julgamento)%>%
  mutate(ano_julgamento= as.Date(ano_julgamento))


# Visualização ------------------------------------------------------------

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



  

julgados_28_tidy %>%
  select(numero_processo, classe_judicial, classe_judicial_raw,
         data_julgamento, data_autuacao, ementa) %>%
  DT::datatable(extensions =  "Responsive",
                options = list(top= "Filter"))
  