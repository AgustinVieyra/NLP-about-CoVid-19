#Declarar las librerías
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)
library(quanteda)

#Leemos el archivo
nov_raw <- read_lines("~/Pablo Agustín/ITESO/Verano 2021/PAP/Code/Encuesta/Respuestas abiertas_afectacion_economica_jalisco.csv")

#Creamos el data frame
nov_text <- nov_raw

#Quitamos puntuación, caracteres especiales, y transformamos a minusculas
nov_text  <-  gsub("[[:punct:]]", "", nov_text)
nov_text  <-  gsub("[[:cntrl:]]", "", nov_text)
nov_text <- tolower(nov_text)

#Quitamos palabras vacias, numeros y espacios excesivos
nov_text <- removeWords(nov_text, words = stopwords("spanish"))
#nov_text$nov_raw <- removeNumbers(nov_text$nov_raw)
nov_text <- stripWhitespace(nov_text)

#Hago un "document term matrix" con la paquetería tm
docs <- Corpus(VectorSource(nov_text))
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

#Hago el grafico de wordcloud
set.seed(1234) # for reproducibility
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

top_wordscause <- topfeatures(dfm_nov_text, n = 5000) %>% 
  enframe(name = "palabra", value = "n") 

top_wordscause

#Grafica de frecuencia
top_n(top_wordscause,10) %>% 
  ggplot(aes(palabra, n)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = n)) + 
  coord_flip() + 
  labs(title = "Las diez palabras más frecuentes en la encuesta",  x = "Palabras", y = "Número de usos") +
  theme(plot.title=element_text( family='', size=25),
        axis.text = element_text(size = 20)
  )


#Observando asociaciones entre palabras

nov_corpus <- Corpus(VectorSource(nov_text))

nov_corpus
nov_text <- removeWords(nov_text, words = c("usted", "pues", "tal", "tan", "así", "dijo", "cómo", "sino", "entonces", "aunque", "don", "doña"))
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm
findAssocs(nov_tdm, terms = c("trabajo", "si", "pandemia", "negocio", "ingresos"), corlimit = .25)

#Eliminando terminos dispersos
nov_new <- removeSparseTerms(nov_tdm, sparse = .95)
nov_tdm
nov_new

#Matriz de distancia
nov_new <- nov_new %>% as.matrix()
nov_new <- nov_new / rowSums(nov_new)
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "Dendrograma de la encuesta - hclust", sub = "", xlab = "")
#Colocamos cuadros de agrupación
plot(nov_hclust, main = "Dendrograma de la encuesta - hclust", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 2, border="blue")

nov_agnes <- agnes(nov_dist, method = "average")
plot(nov_agnes, which.plots = 2, main = "Dendrograma de la encuesta - Agnes", sub = "", xlab = "")
#Colocamos cuadros de agrupación
plot(nov_agnes, which.plots = 2, main = "Dendrograma de la encuesta - Agnes", sub = "", xlab = "")
rect.hclust(nov_agnes, k = 2, border = "blue")

