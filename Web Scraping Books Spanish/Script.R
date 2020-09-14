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
library(lubridate)
library(text2vec)
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

# carga de palabras en ingles que aparecen en los libros
source("textEnglish.R")
# crea el transformador de contenido de toSpace
toSpace <- content_transformer(function(x, pattern, y=F) {return(gsub(pattern, "", x, perl = y))})
# dominios web de paises
# source("domainWeb.R")

# limpieza (se dejan las stopwords en esta etapa)
t1 = Sys.time()
corp <- corp %>% 
   tm_map(toSpace, pattern = c("[[:punct:]]"), y = TRUE) %>% 
   tm_map(toSpace, pattern = c("?")) %>% 
   tm_map(toSpace, pattern = c("!")) %>% 
   tm_map(removeNumbers) %>% 
   tm_map(content_transformer(tolower)) %>% 
   tm_map(content_transformer(str_replace_all), pattern = "\\p{quotation mark}", replacement = " ") %>% 
   tm_map(removeWords, words = c("\\n", "\\r")) %>%
   tm_map(removeWords, words = textEnglish) %>%
   tm_map(stripWhitespace)
t2 = Sys.time()
print(difftime(t2, t1, units = 'min')) # toma alrededor 1.13 mins 10 libros
# aprox deberia tomar 75 mins 662 libros// 88 min final

pathClean <- "Clean Books Spanish/"
if(!dir.exists(pathClean)) dir.create(path = pathClean)

c(1:length(books)) %>% sapply(function(x){
   corp[[x]]["content"] %>% unlist() %>% write(file = paste0(pathClean, books[x]))
})
print(difftime(t2, t1, units = 'min')) 


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


#----- Procesado de Metadatos ----- 

# LoC class
# Carga de diccionario
G <- readRDS("Books Gutenberg Spanish.rds")

# separación de items
locClass <- G$`LoC Class` %>% sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

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
dictClass <- data.frame(nEbook = G$`EBook-No.`, 
                        symbol = c(1:length(locClass)) %>% 
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
   mutate(class1 = sapply(symbol, function(x){strsplit(x, split = "; ")[[1]][1]}),
          class2 = sapply(symbol, function(x){strsplit(x, split = "; ")[[1]][2]})) %>% 
   select(-symbol) %>% 
   mutate(class2 = ifelse(is.na(class2), "", class2))

# ajuste de los datos
G <- G %>% mutate(dictClass, nBook = `EBook-No.`) %>% select(-`LoC Class`, -`EBook-No.`, -nEbook, -url) %>% select(c(10, 1:9))
rm(dictClass, locClass, pasteSent)

#Author
# Limpieza de textos
G <- G %>% mutate(Author = Author %>% 
                     gsub(pattern = "\\?", replacement = "") %>% 
                     gsub(pattern = "\\(", replacement = "") %>% 
                     gsub(pattern = "\\)", replacement = "")
                  )
# separación de items
Author <- G$Author %>% 
   sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

t <- c()
for (i in 1:length(Author)) {
   if(length(Author[i])>0){
      for (j in 1:length(Author[[i]])) {
         t <- c(t, Author[[i]][j])
      }
   }
}
t[is.na(t)] <- "Sin Información"
t <- t %>% unique()

# diccionario
asignAuthor <- function(x){
   x <- ifelse(x=="","Sin Información",x)
   return(
      x %>% 
         sapply(function(y){strsplit(y, split = " -- ") %>% unlist()}) %>% 
         sapply(function(y) {1*grepl(x=t, pattern = y, fixed = F)}) %>%
         unlist() %>% rowSums() %>% t()
      )
   }

dictAuthor <- c(1:dim(G)[1]) %>% sapply(function(x) asignAuthor(x = G$Author[x]))
dictAuthor <- dictAuthor %>% t() %>% data.frame()
colnames(dictAuthor) <- t

G <- G %>% mutate(dictAuthor) %>% select(-Author)
rm(Author, i, j, dictAuthor, t, asignAuthor)

# Release Date
G <- G %>% rename(Date = `Release Date`)
G <- G %>% mutate(Date = Date %>% 
                     gsub(pattern = ",", replacement = "") %>% 
                     gsub(pattern = " ", replacement = "-") %>% 
                     gsub(pattern = "Jan", replacement = "1") %>% 
                     gsub(pattern = "Feb", replacement = "2") %>% 
                     gsub(pattern = "Mar", replacement = "3") %>% 
                     gsub(pattern = "Apr", replacement = "4") %>% 
                     gsub(pattern = "May", replacement = "5") %>% 
                     gsub(pattern = "Jun", replacement = "6") %>% 
                     gsub(pattern = "Jul", replacement = "7") %>% 
                     gsub(pattern = "Aug", replacement = "8") %>% 
                     gsub(pattern = "Sep", replacement = "9") %>% 
                     gsub(pattern = "Oct", replacement = "10") %>% 
                     gsub(pattern = "Nov", replacement = "11") %>% 
                     gsub(pattern = "Dec", replacement = "12") %>% 
                     as.Date("%m-%d-%Y")
                  )


saveRDS(G, "Metadata Books Spanish.rds")

#----- Análisis de Metadatos -----
G <- readRDS("Metadata Books Spanish.rds")

# Language
G %>% select(Language) %>% 
   table(dnn = c("Language")) %>% as.data.frame(stringsAsFactors = F) %>% 
   filter(Freq>0) %>% arrange(-Freq) %>% #head(49) %>% 
   ggplot(aes(x=reorder(Language, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = Language, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Frequency + 1)") +
   ggtitle("Cantidad de libros por idioma")

# Subject
G %>% select(Subject) %>% 
   table(dnn = c("Subject")) %>% as.data.frame(stringsAsFactors = F) %>% 
   filter(Freq>0) %>% arrange(-Freq) %>% filter(nchar(Subject)<150) %>% head(49) %>% 
   ggplot(aes(x=reorder(Subject, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = Subject, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Subject + 1)") +
   ggtitle("Cantidad de libros por tema")

# Categoría
G %>% select(Category) %>% 
   table(dnn = c("Category")) %>% as.data.frame(stringsAsFactors = F) %>% 
   filter(Freq>0) %>% arrange(-Freq) %>% 
   ggplot(aes(x=reorder(Category, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = Category, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Category + 1)") +
   ggtitle("Cantidad de libros por categoría")


# Clasificacion
G %>% select(description) %>% 
   table(dnn = c("description")) %>% as.data.frame(stringsAsFactors = F) %>% 
   filter(Freq>0) %>% arrange(-Freq) %>% filter(nchar(description)<150) %>% head(49) %>% 
   ggplot(aes(x=reorder(description, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = description, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Clasificación + 1)") +
   ggtitle("Cantidad de libros por clasificacion")


# Autor
data.frame(Freq = G %>% select(-c(1:9)) %>% t() %>% rowSums()) %>%
   mutate(Author = colnames(G %>% select(-c(1:9)))) %>% 
   arrange(-Freq) %>% select(Author, Freq) %>% head(49) %>% 
   ggplot(aes(x=reorder(Author, Freq), y = log1p(Freq))) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = Author, label = Freq), hjust = -0.5, vjust = 0.4, size = 3)+
   coord_flip()+
   xlab("") + ylab("Log(Author + 1)") +
   ggtitle("Cantidad de libros por autor")

# Date
G %>% select(Date) %>% mutate(year = year(Date)) %>%
   select(year) %>% 
   table(dnn = c("year")) %>% as.data.frame(stringsAsFactors = F) %>% 
   filter(Freq>0) %>% arrange(-Freq) %>% 
   ggplot(aes(x=reorder(year, year), y = Freq)) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = year, label = Freq), vjust = -0.5, hjust = 0.4, size = 3)+
   xlab("") + ylab("year") +
   ggtitle("Cantidad de libros por año")


G %>% select(Date) %>% mutate(month = month(Date)) %>% select(month) %>% 
   table(dnn = c("month")) %>% as.data.frame(stringsAsFactors = F) %>% 
   mutate(month = as.integer(month)) %>% 
   filter(Freq>0) %>% arrange(month) %>% mutate(month = month(month, label = T)) %>% 
   ggplot(aes(x= month, y = Freq)) + 
   geom_col(fill = "red", alpha = 0.7) +
   geom_text(aes(x = month, label = Freq), vjust = -0.5, hjust = 0.4, size = 3)+
   xlab("") + ylab("month") +
   ggtitle("Cantidad de libros por mes")

## los autores tienen la fecha de vida y muerte en la mayoria de los casos
## se utiliza esto para establecer un periodo de tiempo de escritura del libro
G2 <- readRDS("Books Gutenberg Spanish.rds") # metadatos raw

#Author
# Limpieza de textos
G2 <- G2 %>% mutate(Author = Author %>% 
                     gsub(pattern = "\\?", replacement = "") %>% 
                     gsub(pattern = "\\(", replacement = "") %>% 
                     gsub(pattern = "\\)", replacement = "")
                    )
# separación de items
Author <- G2$Author %>% 
   sapply(function(x){strsplit(x, split = " -- ") %>% unlist()})

t <- c()
for (i in 1:length(Author)) {
   if(length(Author[i])>0){
      for (j in 1:length(Author[[i]])) {
         t <- c(t, Author[[i]][j])
      }
   }
}
t[is.na(t)] <- "Sin Información"
t <- t %>% unique()

Author <- data.frame(autor = t, stringsAsFactors = F)

str_year <- function(x, iloc = 1){
   if(is.na(str_locate(x , "\\d{4}-\\d{4}")[1])){y <- c(NA, NA)}
   if(!is.na(str_locate(x , "\\d{4}-\\d{4}")[1])){
      y <- str_sub(x, str_locate(x , "\\d{4}-\\d{4}")[1], str_locate(x , "\\d{4}-\\d{4}")[2]) %>%
         str_split("-") %>% unlist() %>% as.numeric()
   }
   return(y[iloc])
}

Author <- Author %>% mutate(year_start = autor %>% sapply(str_year, iloc = 1),
                            year_end = autor %>% sapply(str_year, iloc = 2))


Author %>% filter(is.na(year_start)) %>% View()









