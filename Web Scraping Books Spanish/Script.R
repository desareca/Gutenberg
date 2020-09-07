#-------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#
#--------------------------------------- SCRAPING A LA WEB WWW.GUTENBERG.ORG ---------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------#

# Este script selecciona y descarga los libros en español de la página http://www.gutenberg.org, luego los limpia y
# guarda nuevamente los archivos procesados.
# Fecha de descarga de datos originales: 04-09-2020


#----- Carga de librerías -----

library(rvest)
library(tidyverse)
library(tools)
library(tm)
setwd("Web Scraping Books Spanish/")

#----- Selección de libros (N° libro) en español -----

url <- "http://www.gutenberg.org/browse/languages/es"
tmp <- read_html(url)
tmp <- html_nodes(tmp, "li") %>% html_nodes("a")
gutEsp <- tmp %>% lapply(html_attr, name="href") %>% data.frame()
gutEsp <- gutEsp %>% select(matches("ebooks")) %>% t()
gutEsp <-gsub("/ebooks/", "", gutEsp) %>% as.numeric(gutEsp) %>% na.omit() %>% unique()

#----- Scraping a las base de datos de los libros encontrados (698) -----

gutenbergScrap <- function(n=c(1:10), items=NULL, verbose=TRUE, save.RDS=TRUE, titleData="Books Gutenberg"){
   if(is.null(items)){items <- c("Author",
                                 "Title", 
                                 "Language", 
                                 "LoC Class",
                                 "Subject", 
                                 "Category", 
                                 "EBook-No.",
                                 "Release Date")}
   titleData <- paste0(titleData, ".rds")
   G <- data.frame()
   if(verbose) {pb <- txtProgressBar(min = 0, max = length(n), style = 3)}
   j <- 1
   for(i in n){
      url <- paste0("http://www.gutenberg.org/ebooks/", i)
      tmp <- tryCatch(read_html(url), error = function(e) "error")
      if(tmp[1]!="error"){
         tmp <- html_nodes(tmp, "table")
         gut <- html_table(tmp[[2]]) %>% filter(X1 %in% items)
         gut2 <- data.frame(X1 = items, X2 = NA)
         for(k in c(1:length(items))){
            if(dim(gut[gut$X1==items[k],])[1]<1){gut2[k,2] <- ""}
            if(dim(gut[gut$X1==items[k],])[1]>=1){gut2[k,2] <- (gut %>% filter(X1==items[k]))[,2] %>% paste(collapse = " -- ")}
         }
         
         id <- which(html_table(tmp[[1]])[,2]=="Plain Text UTF-8")
         if(length(id)>0){g <- html_table(tmp[[1]])[id,3][1]}
         if(length(id)==0){g <- ""}
         gut2 <- rbind(gut2, g)
         
         if(i==1){G <- t(gut2)}
         if(i>1){G <- rbind(G, gut2 %>% select(2) %>% t())}
         
      }
      if(verbose){setTxtProgressBar(pb, j)}
      j <- j + 1
   }
   colnames(G) <- c(items, "url")
   G <- (G %>% as.data.frame())
   rownames(G) <- c(1:dim(G)[1])
   if(verbose){close(pb)}
   if(save.RDS){saveRDS(G,titleData)}
   return(G)
}
t1 = Sys.time()
G <- gutenbergScrap(n=gutEsp, titleData="Books Gutenberg Spanish")
print(difftime(Sys.time(), t1, units = 'min')) # toma alrededor de 8 mins en 698 libros


#----- Descargar archivos -----

gutenbergDown <- function(url, path = "books/"){
   books <- gsub("https://www.gutenberg.org/ebooks/","",url$url)
   books <- gsub("https://www.gutenberg.org/files/\\d{1,10}/","",books)
   books <- gsub(".utf-8","",books)
   if(!dir.exists(path)){dir.create(path)}
   
   c(1:length(books)) %>% 
      sapply(function(x){
         tryCatch(download.file(url = url$url[x], destfile = paste0(path, books[x])),
                  error = function(e) "error")
      })
   return("Descarga finalizada!")
}
G <- readRDS("Books Gutenberg Spanish.rds")
t1 = Sys.time()
gutenbergDown(url = G %>% select(url), path = "books Spanish/")
print(difftime(Sys.time(), t1, units = 'min')) # toma alrededor de 54 mins en 688 libros


#----- Carga y limpieza de libros -----

# Seleccionar los números de los ebooks descargados
listFiles <- list.files(path = "books Spanish")
listFiles <- gsub("-0.txt", ".txt", listFiles)
listFiles <- gsub(".txt", "", listFiles) %>% as.numeric()

# filtrar los ebooks descargados y sólo en español
G <- readRDS("Books Gutenberg Spanish.rds")
G <- G %>% filter(`EBook-No.` %in% listFiles) %>% filter(Language=="Spanish")

# nombre de los archivos solo en español
books <- gsub("https://www.gutenberg.org/ebooks/","",G$url)
books <- gsub("https://www.gutenberg.org/files/\\d{1,10}/","",books)
books <- gsub(".utf-8","",books)

# carga de libros descargados en español
books <- books[-602] #problemas al leer el corpus
G <- G[-602,] #problemas al leer el corpus

# creación corpus
corp <- VCorpus(VectorSource(c(1:length(books)) %>% 
                                sapply(function(x){read_file(file = paste0("books Spanish/", books[x]))})),
                readerControl = list(language = "es"))


# crea el transformador de contenido de toSpace
toSpace <- content_transformer(function(x, pattern, y=F) {return(gsub(pattern, " ", x, perl = y))})
# dominios web de paises
# source("domainWeb.R")

# limpieza (se dejan las stopwords en esta etapa)
t1 = Sys.time()
corp <- corp %>% 
   # tm_map(removeWords, words = domWeb) %>% 
   tm_map(toSpace, pattern = c("[[:punct:]]"), y = TRUE) %>% 
   tm_map(toSpace, pattern = c("?")) %>% 
   tm_map(toSpace, pattern = c("?")) %>% 
   tm_map(removeNumbers) %>% 
   tm_map(content_transformer(tolower)) %>% 
   tm_map(removeWords, words = c(letters[-c(1,15,21,25)], "\\n", "\\r")) %>% 
   tm_map(content_transformer(str_replace_all), pattern = "\\p{quotation mark}", replacement = " ") %>% 
   # tm_map(removeWords, stopwords("spanish")) %>% 
   tm_map(removeWords, words = c(" $", "^ ")) %>% 
   tm_map(stripWhitespace)
print(difftime(Sys.time(), t1, units = 'min')) # toma alrededor 2 mins 20 libros
# aprox deberia tomar 60 mins 662 libros// 66 min final

pathClean <- "Clean Books Spanish/"
if(!dir.exists(pathClean)) dir.create(path = pathClean)

c(1:length(books)) %>% sapply(function(x){
   corp[[x]]["content"] %>% unlist() %>% write(file = paste0(pathClean, books[x]))
})


#----- Carga de libros limpios -----

# Seleccionar los numeros de los ebooks descargados
listFiles <- list.files(path = "books Spanish")
listFiles <- gsub("-0.txt", ".txt", listFiles)
listFiles <- gsub(".txt", "", listFiles) %>% as.numeric()

# filtrar los ebooks descargados y sólo en español
G <- readRDS("Books Gutenberg Spanish.rds")
G <- G %>% filter(`EBook-No.` %in% listFiles) %>% filter(Language=="Spanish")

# nombre se los archivos solo en español
books <- gsub("https://www.gutenberg.org/ebooks/","",G$url)
books <- gsub("https://www.gutenberg.org/files/\\d{1,10}/","",books)
books <- gsub(".utf-8","",books)

# carga de libros descargados en español
books <- books[-602] # problemas al leer el corpus
G <- G[-602,] # problemas al leer el corpus


# creación corpus limpio
read_txt <- function(path){
   con <- file(path, open="rb") # Abre conexión
   txt <- readLines(con)          # Lee el contenido del archivo
   close(con) # Cierra conexión
   return(txt)
}
corpClean <- VCorpus(
   VectorSource(c(1:length(books)) %>% 
                   sapply(function(x){read_txt(path = paste0("Clean Books Spanish/", books[x]))})),
   readerControl = list(language = "es"))


#----- Procesado de LoC class -----

# Carga de diccionario
G <- readRDS("Books Gutenberg Spanish.rds")

# separación de items
locClass <- G$`LoC Class` %>% 
   sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

# diccionario
pasteSent <- function(x){
   t <- c()
   for (i in 1:length(x)) {
      j = "; "
      if(i == 1) {j = ""}
      t <- paste(t, x[i], sep = j)
   }
   return(t)
}
dictClass <- data.frame(symbol = c(1:length(locClass)) %>% 
                           sapply(function(x) {
                              t <- c()
                              for (i in 1:length(locClass[[x]])) {
                                 t <- c(t, (locClass[[x]][i] %>% strsplit(split = ": "))[[1]][1])
                              }
                              return(t)
                           }) %>% sapply(pasteSent), 
                        description = c(1:length(locClass)) %>% 
                           sapply(function(x) {
                              t <- c()
                              for (i in 1:length(locClass[[x]])) {
                                 t <- c(t, (locClass[[x]][i] %>% strsplit(split = ": "))[[1]][-1])
                              }
                              return(t)
                           }) %>% sapply(pasteSent),
                        stringsAsFactors = F) %>% 
   table() %>% as.data.frame(stringsAsFactors = F) %>% filter(Freq>0)

dictClass %>% 
   # filter(symbol!="PQ") %>% 
   arrange(-Freq) %>% head(49) %>% 
   ggplot(aes(x=reorder(description, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = description, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Frequency + 1)") +
   ggtitle("Cantidad de libros en español por categoría")


#----- Procesado de Subject -----

# Carga de diccionario
G <- readRDS("Books Gutenberg Spanish.rds")

# separación de items
Subject <- G$Subject %>% 
   sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

# diccionario
pasteSent <- function(x){
   t <- c()
   for (i in 1:length(x)) {
      j = "; "
      if(i == 1) {j = ""}
      t <- paste(t, x[i], sep = j)
   }
   return(t)
}
dictSubject <- data.frame(description = c(1:length(Subject)) %>% 
                             sapply(function(x) {
                                t <- c()
                                for (i in 1:length(Subject[[x]])) {
                                   t <- c(t, (Subject[[x]][i])[[1]])
                                   }
                                return(t)
                                }) %>% sapply(pasteSent),
                          stringsAsFactors = F) %>% 
   table() %>% as.data.frame(stringsAsFactors = F) %>% filter(Freq>0)
colnames(dictSubject)[1] <- "description"

dictSubject %>% 
   filter(nchar(description)<150) %>% 
   arrange(-Freq) %>% head(49) %>% 
   ggplot(aes(x=reorder(description, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = description, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Frequency + 1)") +
   ggtitle("Cantidad de libros en español por tema")


#----- Procesado de Author -----

# Carga de diccionario
G <- readRDS("Books Gutenberg Spanish.rds")

# separación de items
Author <- G$Author %>% 
   sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

# diccionario
pasteSent <- function(x){
   t <- c()
   for (i in 1:length(x)) {
      j = "; "
      if(i == 1) {j = ""}
      t <- paste(t, x[i], sep = j)
   }
   return(t)
}
dictAuthor <- data.frame(Author = c(1:length(Author)) %>% 
                             sapply(function(x) {
                                t <- c()
                                if(length(Author[[x]])==0){t <- "Sin Información"}
                                if(length(Author[[x]])>0){
                                   for (i in 1:length(Author[[x]])) {
                                      t <- c(t, (Author[[x]][i])[[1]])
                                   }
                                }
                                return(t)
                             }) %>% sapply(pasteSent),
                          stringsAsFactors = F) %>% 
   table() %>% as.data.frame(stringsAsFactors = F) %>% filter(Freq>0)
colnames(dictAuthor)[1] <- "Author"

dictAuthor %>% 
   # filter(nchar(Author)<150) %>% 
   arrange(-Freq) %>% head(49) %>% 
   ggplot(aes(x=reorder(Author, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = Author, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Frequency + 1)") +
   ggtitle("Cantidad de libros en español por Autor")




