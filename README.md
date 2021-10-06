# trf5
# Web Scrapper do Tribunal Regional Federal da 5ª Região (Tribunal Federal do Nordeste)

A página scrappeada é o buscador de jurisprudência do TRF5 presente no https://julia-pesquisa.trf5.jus.br/julia-pesquisa/.
Apesar de ser um formulário, que seria uma requisição POST, o site tem uma API escondida acessível com requisições GET.

O scrapper foi feito para trabalho no meu escritório, mas também vou entregar à Curso-R como TCC do curso de Web Scrapping.

no arquivo **trf5.R** está as funções para baixar os dados. Nele estão duas funções: **trf5_baixar_cjsg**, que baixa o resultado da Consulta de Jurisprudência do 2º Grau e salva no pasta que você indicar, e **trf5_ler_cjsg** que lê os dados baixados com essa função mencionada.

no arquivo **28_86.r** está o que é disponível fazer, uma parte que vou entregar ao meu escritório.

o TCC da Curso-R estará no Rmarkdown.
