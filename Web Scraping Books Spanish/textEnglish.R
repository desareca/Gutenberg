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
textEnglish <- textEnglish %>% words() %>% unique()
textEnglish <- textEnglish[-c(which(textEnglish=="online"),
                              which(textEnglish=="a"),
                              which(textEnglish=="e"),
                              which(textEnglish=="archive"),
                              which(textEnglish=="individual"),
                              which(textEnglish=="as"),
                              which(textEnglish=="no"),
                              which(textEnglish=="status"),
                              which(textEnglish=="active"),
                              which(textEnglish=="use"),
                              which(textEnglish=="at"),
                              which(textEnglish=="re"),
                              which(textEnglish=="set"),
                              which(textEnglish=="version"),
                              which(textEnglish=="web"),
                              which(textEnglish=="original"),
                              which(textEnglish=="derive"),
                              which(textEnglish=="he"),
                              which(textEnglish=="has"),
                              which(textEnglish=="prepare"),
                              which(textEnglish=="considerable"),
                              which(textEnglish=="transcribe"),
                              which(textEnglish=="virus"),
                              which(textEnglish=="legal"),
                              which(textEnglish=="actual"),
                              which(textEnglish=="exclusion"),
                              which(textEnglish=="procision"),
                              which(textEnglish=="come"),
                              which(textEnglish=="u"),
                              which(textEnglish=="director"),
                              which(textEnglish=="determine"),
                              which(textEnglish=="particular"),
                              which(textEnglish=="general"),
                              which(textEnglish=="produce"),
                              which(textEnglish=="subscribe"))]