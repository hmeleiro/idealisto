#' Scrap idealista website.
#' 
#' This function scraps idealista (a spanish real estate website) and downloads all the rent ads of a given province
#' 
#' 
#' @param url character. An idealista website url that links to the province you want to scrap, e.g. 'https://www.idealista.com/alquiler-viviendas/zaragoza-provincia/'.
#' @param ruta character. A valid path in your computer where you want to create the csv file.
#' @param silent logical. If TRUE it will print a less messages. Useful if you want to schedule with cron and want a cleaner log file. The default is FALSE.
#' @return It returns a csv in the specified path
#' @export
get_provincia <- function(url, ruta = "~/idealisto_prov.csv", silent = FALSE) {
  start <- Sys.time()
  
  # Creo el csv vacío
  line <- data.frame("Titulo", "Zona", "Distrito", "Barrio", "calle", "Precio", "Precio_m2", "Superficie", "Habitaciones", "Descripcion", "Anunciante", "Agencia", "Url", "ultima_actualizacion", "fecha")
  write.table(line, file = ruta, sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "", append = FALSE)
  
  # instalo los paquetes necesarios en el caso de que no lo estén y los cargo
  list.of.packages <- c("stringr", "rvest", "httr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}
  
  library(stringr)
  library(rvest)
  library(httr)
  
  # Creo los headers
  desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                       'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')
  
  
  
  x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  url_zonas <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li ul li a") %>% html_attr(name = "href")
  zonas <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li ul li") %>% html_text()
  
  zonas <- data.frame(urls = url_zonas, zonas = zonas)
  zonas
  url_zonas_tot <- c()
  
  
  
  if (silent == TRUE) {
    print("Capturando los links a las páginas principales de cada barrio.")
  }
  
  for (d in 1:length(url_zonas)) {
    links <- paste0("https://www.idealista.com", url_zonas[d])
    url_zonas_tot <- c(url_zonas_tot, links)
  }
  
  if (silent == FALSE) {
    print(paste("Estos son los links a las páginas principales de cada zona"))
    print(url_zonas_tot)
  }
  
  url_zonas_tot_ <- paste0("https://www.idealista.com", url_zonas)
  
  
  
  
  if (silent == TRUE) {
    print("Capturando los links a todas las páginas de cada zona...")
  }
  
  repeat {
    
    p <- length(url_zonas_tot)
    
    sig_pag_tot <- c()
    
    repeat {
      x <- GET(url_zonas_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
      sig_pag <- x %>% read_html() %>% html_nodes(".icon-arrow-right-after") %>% html_attr(name = "href", default = NA)
      
      if (length(sig_pag) == 0) {
        sig_pag <- NA
      } else {
        sig_pag_tot <- c(sig_pag, sig_pag_tot)
      }
      
      p <- p - 1
      
      if (p == 0) {
        break
      }
      
    }
    
    url_zonas_tot <- paste0("https://www.idealista.com", sig_pag_tot)
    
    if (silent == FALSE) {
      print(url_zonas_tot)
    }
    
    if (isTRUE(url_zonas_tot == "https://www.idealista.com")) {
      break
    }
    
    url_zonas_tot_ <- c(url_zonas_tot_, url_zonas_tot)
    
    
    if (silent == FALSE) {
      print(url_zonas_tot_)
      print(paste("Capturando los links de todas las páginas de cada zona..."))
    }
    
  }
  
  
  
  urls_paginas <- unique(url_zonas_tot_[url_zonas_tot_ != "https://www.idealista.com"])
  
  links_anuncios_tot <- data.frame(anuncio = NA, zona = NA)
  
  tabla_zona <- data.frame(paginas = urls_paginas, urls = str_remove_all(urls_paginas, "https://www.idealista.com|pagina-..\\.htm|pagina-.\\.htm"))
  
  tabla_zona <- merge(zonas, tabla_zona, by = "urls")
  
  tabla_zona$paginas <- as.character(tabla_zona$paginas)
  
  
  for (p in 1:length(tabla_zona$paginas)) {
    x <- GET(tabla_zona$paginas[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    links <- x %>% read_html() %>% html_nodes(".item-link") %>% html_attr(name = "href", default = NA)
    
    links_anuncios <- data.frame(anuncio = paste0("https://www.idealista.com", links), zona = tabla_zona$zonas[p])
    
    links_anuncios_tot <- rbind(links_anuncios_tot, links_anuncios)
    
    if (silent == FALSE) {
      print(links_anuncios_tot)
      print("Capturando los links a todos los anuncios...")
    }
  }
  
  links_anuncios_tot$zona <- str_trim(links_anuncios_tot$zona, "both")
  links_anuncios_tot <- unique(links_anuncios_tot)
  links_anuncios_tot <- links_anuncios_tot[-1,]
  
  links_anuncios_tot <<- links_anuncios_tot
  
  n <- length(links_anuncios_tot$anuncio)
  
  links_anuncios_tot$anuncio <- as.character(links_anuncios_tot$anuncio)
  
  
  print(paste("Idealisto ha extraido las urls de", n, "anuncios."))
  print(paste("Ahora Idealisto comenzará a extraer la información de esos", n, "anuncios. Pero antes vamos a descansar durante 30 segundos"))
  
  Sys.sleep(30)
  
  
  start_2 <- Sys.time()
  
  ###### 
  
  if (silent == TRUE) {
    print("Comienza la extracción de los anuncios...")
  }
  
  for (p in 1:length(links_anuncios_tot$anuncio)) {     
    x <- GET(links_anuncios_tot$anuncio[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    
    titulo <- x %>% read_html() %>% html_nodes(".main-info__title-main") %>% html_text()
    precio <- x %>% read_html() %>% html_nodes(".h1-simulated") %>% html_text()
    ubi <- x %>% read_html() %>% html_nodes("#headerMap li") %>% html_text()
    anunciante <- x %>% read_html() %>% html_nodes(".name") %>% html_text()
    agencia <- x %>% read_html() %>% html_nodes(".about-advertiser-name") %>% html_text()
    info <- x %>% read_html() %>% html_nodes(".info-features") %>% html_text()
    descrip <- x %>% read_html() %>% html_nodes(".expandable") %>% html_text()
    alta <- x %>% read_html() %>% html_nodes("#stats > p") %>% html_text()
    
    info <- str_trim(info, "both")
    info <- str_split(info, "   ")
    info <- unlist(info)
    
    metros <- info[str_detect(info, "m²") == TRUE]
    metros <- str_replace_all(string = metros, pattern = " m²| |\\.", replacement = "")
    metros <- as.numeric(metros)
    
    habit <- info[str_detect(info, "hab\\.") == TRUE]
    habit <- str_remove(habit, "hab\\.")
    habit <- as.integer(habit)
    
    descrip <- str_replace_all(descrip, pattern = '\"', "")
    
    fecha <- Sys.Date()
    
    if (str_detect(precio, "eur") == TRUE) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " eur/mes|\\.", replacement = "")))
    } else if (str_detect(precio, "€") == TRUE) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " €/mes|\\.", replacement = "")))
    }
    
    try(precio_m2 <- precio/metros)
    
    
    
    if (length(titulo) == 0) {
      titulo <- "Sin título"
    }
    
    if (length(precio) == 0) {
      precio <- "Sin precio"
    }
    
    if (length(descrip) == 0) {
      descrip <- "Sin descripción"
    } else if (length(descrip > 1)) {
      descrip <- descrip[1]
    }
    
    if (length(agencia) == 0) {
      agencia <- NA
    }
    
    if (length(anunciante) == 0 | isTRUE(anunciante == " ")) {
      anunciante <- "Particular"
    }
    
    
    zona <- links_anuncios_tot$zona[p]
    
    calle <- ubi[1]
    barrio <- as.character(ubi[str_detect(ubi, pattern = "Barrio ") == TRUE])
    
    distrito <- as.character(ubi[str_detect(ubi, pattern = "Distrito") == TRUE])
    distrito <- str_replace_all(string = distrito, pattern = "Distrito ", replacement = "")
    
    if (length(distrito) == 0) {
      distrito <- NA
    }
    
    if (length(barrio) == 0) {
      barrio <- NA
    }
    
    if (length(calle) == 0) {
      calle <- NA
    }
    
    
    try(line <- data.frame(titulo[1], zona[1], distrito[1], barrio[1], calle[1], precio[1], precio_m2[1], metros[1], habit[1], descrip[1], anunciante[1], agencia[1], links_anuncios_tot$anuncio[p], alta[1], fecha[1]))
    
    if (silent == FALSE) {
      print(line)
    }
    
    if (silent == FALSE) {
      n <- n - 1
      process <- 100 - ((n/length(links_anuncios_tot$anuncio))*100)
      print(paste0("Idealisto lleva descargados el ", round(process, digits = 1),"% de los anuncios."))
    }
    
    
    try(write.table(line, file = ruta, sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = ""))    
    #########
    
    
    
    if (Sys.time() > start_2 + 420) {
      stop_t <- sample(x = 100:120, size = 1)
      print(paste("Para que no se nos cabree Idealista vamos a parar la máquina durante", stop_t, "segundos."))
      Sys.sleep(time = stop_t)
      start_2 <- Sys.time()
    }
    
    Sys.sleep(sample(x = 1:3, size = 1))
  }
  
  end <- Sys.time()
  diff <- end - start
  print(paste("Idealisto ha descargado los", length(links_anuncios_tot$anuncio), "anuncios"))
  print(round(diff, digits = 1))
}