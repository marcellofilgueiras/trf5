# TRF5 - Web Scrapper do Tribunal Regional Federal da 5ª Região (Tribunal Federal do Nordeste)

Neste projeto está presente um WebScrapper construído para baixar decisões do TRF5 e fazer estatísticas com as informações disponíveis online.

Foi construído tanto para razões profissionais, mas compartilho com a comunidade o trabalho realziado, especialmente por ser um trabalho para Conclusão do Curso de Web Scrapping da CursoR.

Consulte o Markdown ao ado que explico como foram criadas as funções;

No arquivo **trf5.R** estão alguns testes inicias para baixar a jurisprudência dos julgados. Desconsidere.\n
No **trf5_cjsg.r** estão as função de baixar a jurisprudência e  **28_86.r** e sua aplicação.\n
No **trf_cpo_fisico** está a consulta processual, para metadados dos processos, e **precatorios_2020.r** sua aplicação.

## **Dados de Julgados de segundo Grau: trf5_baixar_cjsg() e trf_ler_cjsg()**

> CJSG = (C)onsulta (J)urisprudencial de (S)egundo (G)rau.

Presentes no arquivo **tr5_cjsg.r**, estas duas funções fazem o Web Scrapping do buscador de jurisprudência do TRF5 presente no https://julia-pesquisa.trf5.jus.br/julia-pesquisa/.
Apesar de iniciar uma pesquisa em um formulário por a requisição POST, o site tem uma API escondida acessível com requisições GET.

A primeira função é a **trf5_baixar_cjsg**, que baixa o resultado da Consulta de Jurisprudência do 2º Grau e salva no pasta que você indicar.\n
A segunda função é a **trf5_ler_cjsg**, que lê os dados a partir da pasta selecionada com com essa função mencionada.

NO arquivo **28_86.r** está o que é disponível uma análise prévia do que é possível fazer com os dados buscados.

## **Dados de Capa de Processos Físicos: trf5_baixar_cpo_fisico() e trf_cpo_fisico_ler_XXXXX()**

> CPO = (C)onsulta (P)r(o)cessual.

Munido com os números dos processos baixados dos julgados acima, você consegue baixar os dados de processos físicos (infelizmente quebrar PJE é bem difícil) com a função **trf5_baixar_cpo_fisico()**.

Para ler os _metadados_ de cada processo pode usar as seguintes funções:

Com **trf5_cpo_fisico_ler_processo()** você lê os "dados de capa".\n
Com **trf5_cpo_fisico_ler_partes()** você lê as partes da cada do processo.\n
Com **trf5_cpo_fisico_ler_movimentacoes()** você lê as movimentações procedimentais da cada do processo.\n

Faça análises com segurança! Filtre quantos julgados você quer. Se for baixar muitos julgados faça de noite. Tenha ética com Web Scrapping!
Me mencione por favor ser fizer análises. =D
