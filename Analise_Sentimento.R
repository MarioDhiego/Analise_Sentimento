
#====================================================#
#       Análise Textual de Clássicos Brasileiros    #
#====================================================#

#==============================
# 1️⃣ Pacotes
#==============================
library(literaturaBR)  # textos clássicos
library(tidytext)      # text mining
library(tidyverse)     # manipulação de dados
library(stringr)       # manipulação de texto
library(quanteda)      # análise quantitativa de texto
library(quanteda.textplots)
library(quanteda.textstats)
library(qdap)          # complementar (opcional)
library(forcats)       # manipulação de fatores
library(ggthemes)      # temas para ggplot2
library(lexiconPT)     # léxicos em português
library(devtools)
library(patchwork) 
library(reshape2)
library(viridis)   # paleta de cores
library(dendextend) # para dendrograma


#==============================
# 2️⃣ Importar os livros
#==============================
data("memorias_de_um_sargento_de_milicias")
data("memorias_postumas_bras_cubas")
data("alienista")
data("escrava_isaura")
data("ateneu")
data("cortico")
data("dom_casmurro")
data("noite_na_taverna")

# Unir todos em um único data frame
df <- bind_rows(
  memorias_de_um_sargento_de_milicias,
  memorias_postumas_bras_cubas,
  alienista,
  escrava_isaura,
  ateneu,
  cortico,
  dom_casmurro,
  noite_na_taverna
)

glimpse(df)

#==============================
# 3️⃣ Criar dataframe por livro
#==============================
df_corpus <- df %>%
  group_by(book_name) %>%
  summarise(text = paste0(text, collapse = ". "))

dim(df_corpus)

#==============================
# 4️⃣ Criar corpus
#==============================
meu_corpus <- quanteda::corpus(df_corpus$text, docnames = df_corpus$book_name)
summary(meu_corpus)

#==============================
# 5️⃣ Criar tokens e DFM (quanteda ≥3.x)
#==============================
# 5.1 Criar tokens e remover pontuação
corpus_tokens <- tokens(meu_corpus, remove_punct = TRUE)

# 5.2 Remover stopwords em português
corpus_tokens <- tokens_remove(corpus_tokens, stopwords("portuguese"))

# 5.3 Criar dfm a partir dos tokens
corpus_dfm <- dfm(corpus_tokens)

# 5.4 Agrupar por livro
corpus_dfm <- dfm_group(corpus_dfm, groups = docnames(meu_corpus))

# 5.5 Top palavras
cat("Top 20 palavras do corpus inteiro:\n")
print(topfeatures(corpus_dfm, 20))

cat("\nTop 20 palavras por livro:\n")
for (i in docnames(corpus_dfm)) {
  cat("\nLivro:", i, "\n")
  print(topfeatures(corpus_dfm[i, ], 20))
}

#==============================
# 6️⃣ Ocorrências de palavras específicas
#==============================
dfm_select(corpus_dfm, "amor")
kwic(corpus_tokens, "amor") %>% head()


# Gráficos X-Ray
kwic(corpus_tokens, "amor") %>% textplot_xray(scale = "relative")
kwic(corpus_tokens, "liberdade") %>% textplot_xray(scale = "relative")
kwic(corpus_tokens, "fogo") %>% textplot_xray(scale = "relative")


#==============================
# 7️⃣ Similaridade e dendrograma
#==============================
# Normalizar por frequência relativa
corpus_dfm_norm <- dfm_weight(corpus_dfm, "prop")

# Similaridade por correlação
corpus_simil <- textstat_simil(corpus_dfm_norm, method = "correlation")
round(corpus_simil, 3)


#========================================
# 1️⃣ Heatmap da similaridade por correlação
#========================================

# Transformar o objeto textstat_simil em matriz
sim_matrix <- as.matrix(corpus_simil)

# Transformar em data.frame longo para ggplot
sim_long <- melt(sim_matrix)
colnames(sim_long) <- c("Livro1", "Livro2", "Correlacao")

ggplot(sim_long, aes(x = Livro1, y = Livro2, fill = Correlacao)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlacao, 2)), size = 3) +
  scale_fill_viridis(option = "C", limits = c(0,1)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Heatmap de Similaridade Lexical entre Livros",
    x = "",
    y = "",
    fill = "Correlação"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



# Distância euclidiana e dendrograma
corpus_dist <- textstat_dist(corpus_dfm_norm, method = "euclidean")
hclust_res <- hclust(as.dist(corpus_dist))
plot(hclust_res)















#==============================
# 8️⃣ Tokenização e análise de sentimento
#==============================
df.token <- df %>% unnest_tokens(term, text)

# Importar léxico de sentimentos
data("oplexicon_v3.0")

# Remover duplicações do léxico (evita join many-to-many)
oplexicon_unique <- oplexicon_v3.0 %>%
  distinct(term, .keep_all = TRUE)

# Fazer o join dos termos do texto com o léxico
df.token <- df.token %>%
  inner_join(oplexicon_unique, by = "term")

# Normalizar capítulos

df_chapter_number <- df.token %>%
  distinct(book_name, chapter_name) %>%
  group_by(book_name) %>%
  mutate(chapter_number_norm = row_number() / max(row_number())) %>%
  ungroup()

# Sentimento por capítulo
df.sentiment <- df.token %>%
  group_by(book_name, chapter_name) %>%
  summarise(polarity = sum(polarity, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(df_chapter_number, by = c("book_name", "chapter_name")) %>%
  arrange(book_name, chapter_number_norm)

# Gráfico de sentimento por capítulo
ggplot(df.sentiment, aes(x = chapter_number_norm, y = polarity)) +
  geom_line(color = "black", linewidth = 0.8) +                 # linha
  #geom_point(color = "darkred", size = 1.8, alpha = 0.8) +    # marcadores
  facet_wrap(~ book_name, ncol = 4, labeller = label_wrap_gen(20)) +
  labs(
    #title = "Evolução do Sentimento ao Longo dos Capítulos",
    x = "Posição relativa no livro",
    y = "Sentimento (polaridade)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


#=========================================================#
# 5️⃣ Gráfico complementar: sentimento médio por livro
#=========================================================#

df.sentiment_book <- df.sentiment %>%
  group_by(book_name) %>%
  summarise(sentimento_medio = mean(polarity, na.rm = TRUE)) %>%
  arrange(desc(sentimento_medio))

# Gráfico de barras ordenado
ggplot(df.sentiment_book, aes(x = reorder(book_name, sentimento_medio),
                              y = sentimento_medio,
                              fill = sentimento_medio > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = round(sentimento_medio, 2)),
            hjust = ifelse(df.sentiment_book$sentimento_medio > 0, -0.2, 1.2),
            color = "black", size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  labs(
    title = "Sentimento Médio por livro",
    x = "",
    y = "Polaridade média"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  ) +
  expand_limits(y = c(min(df.sentiment_book$sentimento_medio) - 0.1,
                      max(df.sentiment_book$sentimento_medio) + 0.1))





#==============================
# 9️⃣ Diversidade lexical (TTR)
#==============================
# Calcular diversidade lexical (TTR)
lexdiv <- textstat_lexdiv(corpus_dfm, measure = "TTR")

# Gráfico TTR
lexdiv %>%
  ggplot(aes(x = fct_reorder(document, TTR), y = TTR)) +
  geom_col(fill = "cadetblue4") +
  coord_flip() +
  labs(x = NULL, y = "Tipo-Token Ratio (TTR)",
       title = "Diversidade lexical dos livros") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))












