textEnglish = read_file(file = "textEnglish.txt")
textEnglish = gsub("http "," ",textEnglish) # limpia paginas http
textEnglish = gsub("https "," ",textEnglish) # limpia paginas https
textEnglish = gsub("com "," ",textEnglish) # limpia direcciones
textEnglish = gsub("co "," ",textEnglish) # limpia direcciones
textEnglish = gsub("org "," ",textEnglish) # limpia direcciones
textEnglish = gsub("[[:punct:]]"," ",textEnglish) # elimina puntuacion
textEnglish = gsub("\\w*[0-9]+\\w*\\s*", " ",textEnglish) #elimina numeros
textEnglish = stringr::str_replace_all(textEnglish, "\\p{quotation mark}", "") # elimina comillas
textEnglish = gsub("\\n", " ",textEnglish) # elimina saltos de linea
textEnglish = stringr::str_replace_all(textEnglish,"[\\s]+", " ")
# elimina emojis
textEnglish = gsub("<\\w+>","",iconv(textEnglish, from = "UTF-8", to = "latin1", sub = "byte"))
textEnglish = gsub("<\\w+>","",iconv(textEnglish, from = "UTF-8", to = "latin1", sub = "byte"))
textEnglish = gsub("-", "",textEnglish) # elimina giones
textEnglish = tolower(textEnglish) # transforma a minuscula
textEnglish = stringr::str_replace_all(textEnglish," $", "") # elimina espacios finales
textEnglish = tm::stripWhitespace(textEnglish) # quita espacios en blanco repetidos
textEnglish = stringr::str_replace_all(textEnglish,"^ ", "") # elimina espacios iniciales
# filtra las palabras que se repiten más de 5 veces
textEnglish <- textEnglish %>% words() %>% table()
textEnglish <- textEnglish[order(textEnglish, decreasing = TRUE)]
textEnglish <- textEnglish %>% head(10)
textEnglish <- row.names(textEnglish)
