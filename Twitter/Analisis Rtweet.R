#Instalar la paquetería
install.packages("rtweet")
install.packages("tidyverse")
install.packages("quanteda")
install.packages("forestmangr")
install.packages("dplyr")
install.packages("janeaustenr")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("syuzhet")

install.packages("tm")
install.packages("devtools")

#Declarar las librerías
library(rtweet)      
library(tidyverse)
library(quanteda)   #Analisitca texto
library(forestmangr)
library(dplyr)
library(janeaustenr)#Función token
library(tidytext)   #Analisitca texto
library(wordcloud)  #Nube palabras
library(syuzhet)    #Analisis sentimientos

library(tm)
library(devtools)

#CONTEO DE PALABRAS CLAVE Y LOCACION A BUSCAR EN TWITTER
#certificado de vacunación ->
#Vacunas CDMX
#Pandemia Jalisco ->660 tweets
#Retorno a clases/aulas Jalisco ->1 tweet
#Variantes Covid Jalisco ->58 tweets
#Noticias Jalisco Covid -> 73 tweets
#Salud Jalisco -> 818 tweets
#Radar Jalisco -> 80 tweets
##jaliscourge -> 10 tweets
#54 al día en Jalisco -> 12 tweets
#Jalisco fuente -> 221
#jaliscourge -> 10 tweets
#contagios Jalisco -> 907
#contagios Guadalajara -> 108
#2021vacúnate Jalisco ->10



#Al ejecutar la siguiente linea va a abrir en tu navegador Twitter para acceder a tu cuenta  y utilizar la API
tweets_PalabraClave <- search_tweets(q = "Salud Jalisco", n = 1000) #Si no quisiera retweets agregaría ", include_rts = FALSE"

#Eliminando los retweets y las respuestas a otros usuarios y solo dejar los tweets organicos
tweetsOrganicos <- tweets_PalabraClave[tweets_PalabraClave$is_retweet==FALSE, ] 
tweetsOrganicos <- subset(tweetsOrganicos, is.na(tweetsOrganicos$reply_to_status_id))

#Organizar por numero de favoritos
tweetsOrganicos <- tweetsOrganicos %>% arrange(-favorite_count)

#Guardando los retweets y las respuestas
retweets <- tweets_PalabraClave[tweets_PalabraClave$is_retweet==TRUE,]
respuestas <- subset(tweets_PalabraClave, !is.na(tweets_PalabraClave$reply_to_status_id))

#Creando un dataframe de los tweets organicos y respuestas
data <- data.frame(
  category=c("Organico", "Retweets", "Respuesta"),
  count=c(object.size(tweetsOrganicos), object.size(retweets), object.size(respuestas))
)
#Haciendo la grafica de respuestas/retweets/tweets organicos
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
#Redondeando a dos decimales
data <- round_df(data, 2)
#Poniendo titulo a los datos
Tipo_de_Tweet <- paste(data$category, data$percentage, "%")
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo_de_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

#Sacando los datos de qué dispositivo salieron los tweets
dispositivos <- tweets_PalabraClave %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())
dispositivos <- subset(dispositivos, count > 3)

#Grafica de anillo de los dispositivos
data <- data.frame(
  category=dispositivos$source,
  count=dispositivos$count
)
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
data <- round_df(data, 2)
Source <- paste(data$category, data$percentage, "%")
#Grafica de anillos
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

#Buscando las palabras más utilizadas

#Borrando puntuación y simbolos
tweetsOrganicos$text <-  gsub("https\\S*", "", tweetsOrganicos$text)
tweetsOrganicos$text <-  gsub("@\\S*", "", tweetsOrganicos$text) 
tweetsOrganicos$text  <-  gsub("amp", "", tweetsOrganicos$text) 
tweetsOrganicos$text  <-  gsub("[\r\n]", "", tweetsOrganicos$text)
tweetsOrganicos$text  <-  gsub("[[:punct:]]", "", tweetsOrganicos$text)

#Eliminar palabras vacias
    #La variable 'tweets' venía en el código original
tweets <- tweetsOrganicos %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stopwordslangs)
##Se utilizo stopwordslangs en vez de stop_words para que tomara palabras huecas en español##

#Grafico de las palabras utilizadas
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Número de repeticiones",
       x = "Palabras",
##CAMBIO-----------------------------------
       title = "Palabras más frecuentes con la frase salud Jalisco",
       subtitle = "Sin utilizar palabras vacías")


#Mostrar los hashtags más comunes
tweetsOrganicos$hashtags <- as.character(tweetsOrganicos$hashtags)
tweetsOrganicos$hashtags <- gsub("c\\(", "", tweetsOrganicos$hashtags)
set.seed(1234)
wordcloud(tweetsOrganicos$hashtags, min.freq=2, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Ver quién hace los retweets más populares
set.seed(1234)
wordcloud(retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

#Analisis de sentimientos

#Quitando caracteres extraños
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
#Quitando retweets
#tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
#Quitando menciones
#tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))

#Declaración del data frame
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)

#Grafico
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()

