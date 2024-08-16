#-------------------------------------------------------------------------------
# PACKAGES ---------------------------------------------------------------------
# Pacotes ou bibliotecas utilizadas.

library(devtools)      # Baixar pacote do github
library(tidytext)      # Pacote de text mining
library(tidyverse)     # Manipulação de dados
library(magrittr)      # Operador pipe
library(stringr)       # Manipulacao de texto
library(rvest)         # web scraping
library(quanteda)      # Analise Quantitativa de texto
library(quanteda.textplots)
library(qdap)          # Analise Quanlitativa de texto
library(forcats)       # manipulacao de fatores
library(ggthemes)      # Temas para o ggplot2
library(lexiconPT)     # Dicionário Lexico de palavras
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Web Scraping -----------------------------------------------------------------
#-------------------------------------------------------------------------------
rm_accent <- function(x) {
  if (.Platform$OS.type == 'unix') {
    gsub("`", "", iconv(x, to = "ASCII//TRANSLIT"))
  } else {
    gsub("`", "", iconv(x, from = 'latin1', to="ASCII//TRANSLIT"))
  }
}
extract_chapter_url <- function(wikisource_book_page, xpath_chapters){
  # First function: scrape chapters urls of a given book url
  # url example: https://pt.wikisource.org/wiki/A_escrava_Isaura
  
  book_html <- wikisource_book_page %>% read_html()
  
  chapters <- book_html %>%
    html_nodes(xpath = xpath_chapters)
  # extract chapter names and urls
  chapter_names <- chapters %>% html_text()
  chapter_urls <- chapters %>% html_attr("href")
  
  # extract chapter names and urls from table if it exists
  chapter_table <- book_html %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
    html_nodes(css = "a")
  
  if (length(chapter_table) > 0){
    chapter_names_table <- chapter_table %>% html_text()
    chapter_urls_table <- chapter_table %>% html_attr("href")
    
    chapter_names <- c(chapter_names, chapter_names_table)
    chapter_urls <- c(chapter_urls, chapter_urls_table)
  }
  
  chapter_urls <- paste0("https://pt.wikisource.org", chapter_urls)
  # extract name of book
  book_name <- book_html %>%
    html_nodes(xpath  = '//*[@id="firstHeading"]') %>%
    html_text()
  
  # return as a dataframe
  data.frame(book_name, chapter_name = chapter_names, url = chapter_urls,
             stringsAsFactors = FALSE)
}

extract_chapter_text <- function(wikisource_chapter_page, xpath_type){
  
  if (xpath_type == 'A'){
    xpath_main <- '//*[@id="mw-content-text"]/div/p'
    xpath_b <- '//*[@id="mw-content-text"]/div/div/p'
  } else if(xpath_type == "B"){
    xpath_main <- '//*[@id="mw-content-text"]/div/div/p'
    xpath_b <- '//*[@id="mw-content-text"]/div/div/p'
  } else {
    stop("Choose A or B for xpath_type")
  }
  
  chp <- wikisource_chapter_page %>%
    read_html() %>%
    # xpath to extract chapter text
    html_nodes(xpath = xpath_main)# %>% html_text()# %>% str_split("[\n]") %>% unlist()
  
  if (length(chp) == 0){
    chp <- wikisource_chapter_page %>%
      read_html() %>%
      html_nodes(xpath = xpath_b) %>%
      html_text()
    # remove header
    chp <- chp[-1]
    # if object is still empty, use another xpath
    # if (length(chp) == 0){
    #   chp <- wikisource_chapter_page %>%
    #     read_html() %>%
    #     html_nodes(xpath = xpath_c) %>%
    #     html_text()
    # }
  } else{
    chp <- chp %>%
      html_text()
  }

  # remove empty lines
  chp <- chp[chp != ""]
  paragraph_number <- seq_along(chp)
  
  if (length(chp) == 0){
    paragraph_number <- NA_integer_
    chp <- NA_character_
  }
  
  data.frame(url = wikisource_chapter_page, paragraph_number, text = chp,
             stringsAsFactors = FALSE)
}

### function to extract text of a whole book
extract_book <- function(wikisource_book_page,
                         xpath_type,
                         xpath_chapters = '//*[@id="mw-content-text"]/div/ul/li/a'){
  df_chapters_urls <- extract_chapter_url(wikisource_book_page, xpath_chapters)
  # check if dataframe
  if(!inherits(df_chapters_urls, "data.frame")) {
    stop("Output of extract_url_chapters() not a dataframe")
  }
  df_chapters_text <- df_chapters_urls$url %>%
    map_df(extract_chapter_text, xpath_type  = xpath_type)
  
  df <- left_join(df_chapters_urls, df_chapters_text, by = "url")
  return(df)
}


# A escrava Isaura #
url <- "https://pt.wikisource.org/wiki/A_escrava_Isaura"
escrava_isaura <- extract_book(url, xpath_type = "A")
escrava
use_data(escrava_isaura, overwrite = TRUE)

# O Ateneu
url <- "https://pt.wikisource.org/wiki/O_Ateneu"
ateneu <- extract_book(url, xpath_type = "A")
use_data(ateneu, overwrite = TRUE)

# Memórias Póstumas de Bras Cubas
url <- "https://pt.wikisource.org/wiki/Mem%C3%B3rias_P%C3%B3stumas_de_Br%C3%A1s_Cubas"
system.time(memorias_postumas_bras_cubas <- extract_book(url, xpath_type = "B"))
use_data(memorias_postumas_bras_cubas, overwrite = TRUE)

# O Alienista
url <- "https://pt.wikisource.org/wiki/O_Alienista"
alienista <- extract_book(url, "B")
use_data(alienista, overwrite = TRUE)

# Memorias de um Sargento de Milicias
url <- "https://pt.wikisource.org/wiki/Mem%C3%B3rias_de_um_Sargento_de_Mil%C3%ADcias"
memorias_de_um_sargento_de_milicias <- extract_book(url, "B")
use_data(memorias_de_um_sargento_de_milicias, overwrite = TRUE)

# O Cortiço #
url <- "https://pt.wikisource.org/wiki/O_Corti%C3%A7o"
cortico <- extract_book(url, "B", xpath_chapters = '//*[@id="mw-content-text"]/div/div/div/div/div[1]/div/span/a')
use_data(cortico, overwrite = TRUE)

# Noite na Taverna #
url <- "https://pt.wikisource.org/wiki/Noite_na_Taverna"
noite_na_taverna <- extract_book(url, "B")
use_data(noite_na_taverna, overwrite = TRUE)

# Dom Casmurro #
url <- "https://pt.wikisource.org/wiki/Dom_Casmurro"
dom_casmurro <- extract_book(url, "A")
use_data(dom_casmurro, overwrite = TRUE)
#-------------------------------------------------------------------------------


# DATA FRAME -------------------------------------------------------------------
# Importar os datasets e transformar em um dataset único.

data("alienista")
data("cortico")
data("dom_casmurro")
data("memorias_postumas_bras_cubas")
data("memorias_de_um_sargento_de_milicias")
data("ateneu")
data("escrava_isaura")
data("noite_na_taverna")

df <- bind_rows(alienista,
                cortico,
                dom_casmurro,
                memorias_postumas_bras_cubas,
                memorias_de_um_sargento_de_milicias,
                ateneu,
                escrava_isaura,
                noite_na_taverna)
#-------------------------------------------------------------------------------


# ESTRUTURA DATA FRAME ---------------------------------------------------------
glimpse(df)
head(df)
#-------------------------------------------------------------------------------


# CORPUS DE TEXTO --------------------------------------------------------------
# converter o dataframe dos livros em um objeto do tipo corpus.
# Datasets possuem a mesma estrutura onde: 
# cada linha corresponde a um parágrafo de um livro e contêm 5 variáveis:



df_corpus <- df %>% 
  # agrupar por livro
  group_by(book_name) %>% 
  # formatar o dataframe para que so tenha uma linha por livro
  summarise(text = paste0(text, sep = "", collapse = ". "))

dim(df_corpus)
#-------------------------------------------------------------------------------



# CONTAGEM DE PALAVRAS ---------------------------------------------------------
# Types:     N°de Palavras Diferentes
# Tokens:    N°Total de Palavras
# Sentences:  N°de Frases em cada Livro

meu_corpus <- quanteda::corpus(df_corpus$text, 
                               docnames = df_corpus$book_name)
summary(meu_corpus)
#-------------------------------------------------------------------------------


# Document-Feature Matrix ------------------------------------------------------

# Tokenizar o corpus
tokens_corpus <- tokens(meu_corpus, remove_punct = TRUE)

# Criar o dfm a partir dos tokens
corpus_dfm <- dfm(tokens_corpus)

# Agrupar o dfm por livro
corpus_dfm_grouped <- dfm_group(corpus_dfm, groups = df_corpus$book_name)

# Remover Pontuações/stopwords
corpus_dfm_grouped <- dfm_remove(corpus_dfm_grouped, quanteda::stopwords("portuguese"))

# Usar topfeatures para obter as 30 palavras mais comuns
top_words <- topfeatures(corpus_dfm_grouped, 30)
print(top_words)

# Para um livro específico (substitua "NomeDoLivro" pelo nome real do livro)
top_words_book <- topfeatures(corpus_dfm_grouped[, df_corpus$book_name == "escrava_isaura"], 30)
print(top_words_book)
#-------------------------------------------------------------------------------

# Retorna Ocorrência de cada palavra em, cada livro.

dfm_sort(corpus_dfm)[, 1:50]
#-------------------------------------------------------------------------------


# OCORRÊNCIA DE PALAVRAS -------------------------------------------------------

dfm_select(corpus_dfm, "amor")
dfm_select(corpus_dfm, "fogo")
dfm_select(corpus_dfm, "paixão")
#-------------------------------------------------------------------------------


# CONTEXTO DA PALAVRA ----------------------------------------------------------

tokens_corpus <- tokens(meu_corpus)
concordancias <- kwic(tokens_corpus, "amor") %>% head(10) 
print(concordancias)
#-------------------------------------------------------------------------------

# Palavras + Usadas
topfeatures(corpus_dfm, groups = df_corpus$book_name)



# COMPARAÇÃO ENTRE LIVROS ------------------------------------------------------
# normalizar os livros pelo seu tamanho
corpus_dfm_norm <- dfm_weight(corpus_dfm, "count")     # prop, 
corpus_simil <- textstat_simil(corpus_dfm_norm, 
                               method = "correlation", #cosine, jaccard, dice
                               margin = "documents",   #features
                               upper = TRUE,
                               diag = FALSE)
# ver os resultados individualmente para cada livro
round(corpus_simil, 3)


corpus_dist <- textstat_dist(corpus_dfm_norm, 
                             method = "euclidean", #manhattan,minkowski
                             margin = "documents")
# ver os resultados individualmente para cada livro
plot(hclust(corpus_dist))



#-------------------------------------------------------------------------------

# Criar um dataframe em que cada linha corresponda a uma unica palavra
df.token <- df %>%
  unnest_tokens(term, text)

glimpse(df.token)


# importar lexico de sentimentos
data("oplexicon_v3.0")
df.token <- df.token %>%
  inner_join(oplexicon_v3.0, by = "term")

# extrair capitulos de cada livro
df_chapter_number <- df.token %>%
  distinct(book_name, chapter_name) %>%
  group_by(book_name) %>%
  # normalizar capitulo de acordo com sua posicao no livro
  mutate(chapter_number_norm = row_number()/max(row_number()))

glimpse(df_chapter_number)
#-------------------------------------------------------------------------------

# Sentimento por Capítulo ------------------------------------------------------

df.sentiment <- df.token %>%
  # calcular sentimento por capitulo
  group_by(book_name, chapter_name) %>%
  summarise(polarity = sum(polarity, 
                           na.rm = TRUE)) %>%
  ungroup() %>%
  # retornar posicao relativa (ou normalizada) do capitulo de cada livro
  left_join(df_chapter_number) %>%
  arrange(book_name, chapter_number_norm)


# grafico
df.sentiment %>%
  ggplot(aes(x = chapter_number_norm, 
             y = polarity)) +
  geom_line() +
  facet_wrap(~ book_name, 
             ncol = 4, 
             labeller = label_wrap_gen(20)) +
  labs(x = "Posição Relativa no Livro", 
       y = "Estimativa de Sentimento") +
  theme_gray()
#-------------------------------------------------------------------------------



# COMPLEXIDADE LÉXICA DAS PALAVRAS ---------------------------------------------
# aplicando a funcao no objeto sem stopwords e pontuação
lexdiv <- textstat_lexdiv(corpus_dfm, measure = "TTR")
lexdiv

#grafico
lexdiv %>% 
  as.data.frame() %>% 
  magrittr::set_colnames("TTR") %>% 
  tibble::rownames_to_column("livro") %>% 
  mutate(livro = forcats::fct_reorder(livro, TTR)) %>% 
  ggplot(aes(x = livro, y = TTR)) + 
  geom_col(fill = "blue") +
  coord_flip() + 
  labs(x = NULL, y = "TTR") +
  theme_minimal()





#-------------------------------------------------------------------------------
# Gráfico de Dispersão Lexico

# Tokenizar o corpus
tokens_corpus <- tokens(meu_corpus)
# Encontrar as concordâncias da palavra "amor"
concordancias <- kwic(tokens_corpus, "fogo")
# Visualizar as concordâncias usando textplot_xray
textplot_xray(concordancias, scale = "relative")


#-------------------------------------------------------------------------------














