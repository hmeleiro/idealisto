#' Scrap idealista website.
#' 
#' This function scraps idealista (a spanish real estate website) and downloads all the rent ads in the given province, city, disctrict or neighborhood.
#' 
#' This function is a silent version of idealisto_alter. This means that it will print a less messages. Useful if you want to schedule with cron and want a cleaner log file.
#' 
#' @param url An idealista website url that links to the area you want to scrap, e.g. 'https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/'.
#' @param area The type of area you want to scrap. It can take these values: 'Provincia', 'Ciudad', 'Distrito' or 'Barrio'.
#' @param ruta A valid path in your computer where you want to create the csv file.
#' @return It returns a csv in the specified path
#' @export
idealisto_silent <- function(url, area, ruta = "~/idealisto_alter.csv") {
  start <- Sys.time()
  
  list.of.packages <- c("stringr", "rvest", "httr")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) {install.packages(new.packages)}
  
  library(stringr)
  library(rvest)
  library(httr)
  
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
  
  
  
  
  if (area == "Distrito" | area == "distrito") {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li li a") %>% html_attr(name = "href")
    
    subarea <- "barrio"
    url_distris_tot <- c()
    
  } else if (area == "Provincia" | area == "provincia") {
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li li a") %>% html_attr(name = "href")
    
    subarea <- "zona"
    url_distris_tot <- c()
    
  } else if(area == "Ciudad" | area == "ciudad") {
    
    x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    url_distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li a") %>% html_attr(name = "href")
    distris <- x %>% read_html() %>% html_nodes(".breadcrumb-subitems li a") %>% html_text()
    
    distris <- data.frame(urls = url_distris, distritos = distris)
    
    subarea <- "distrito"
    url_distris_tot <- c()
    
  } else if (area == "Barrio" | area == "barrio") {
    url_distris <- c()
    url_distris_tot <- c()
  } else {
    url_distris <- c()
    url_distris_tot <- c()
  }
  
  
  
  d <- length(url_distris)
  
  ### CREA UN ÍNDICE DE PÁGINAS DE CADA SUBAREA
  print(paste0("Capturando los links a las principales páginas de cada ", subarea, "."))
  repeat {
    links <- paste0("https://www.idealista.com", url_distris[d])
    url_distris_tot <- c(url_distris_tot, links)
    
    d <- d - 1
    if (d <= 0) {
      break
    }
  }
  
  if (area == "Barrio" | area == "barrio") {
    url_distris_tot <- url
    url_distris_tot_ <- url
  } else {
    url_distris_tot_ <- paste0("https://www.idealista.com", url_distris)
  }
  
  
  print("Capturando los links a todas las páginas de todas las subareas.")
  
  repeat {
    
    p <- length(url_distris_tot)
    
    sig_pag_tot <- c()
    
    repeat {
      x <- GET(url_distris_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
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
    
    url_distris_tot <- paste0("https://www.idealista.com", sig_pag_tot)
    
    if (isTRUE(url_distris_tot == "https://www.idealista.com")) {
      break
    }
    
    
    url_distris_tot_ <- c(url_distris_tot_, url_distris_tot)
    
  }
  
  urls_paginas <- unique(url_distris_tot_[url_distris_tot_ != "https://www.idealista.com"])
  
  links_anuncios_tot <- data.frame(anuncio = NA, distrito = NA)
  
  tabla_distritos <- data.frame(paginas = urls_paginas, urls = str_remove_all(urls_paginas, "https://www.idealista.com|pagina-..\\.htm|pagina-.\\.htm"))
  
  if (area == "Ciudad" | area == "ciudad") {
    tabla_distritos <- merge(distris, tabla_distritos, by = "urls")
    
  }
  
  tabla_distritos$paginas <- as.character(tabla_distritos$paginas)
  
  p <- length(tabla_distritos$paginas)
  
  
  print("Capturando los links a todos los anuncios...")
  
  repeat {
    x <- GET(tabla_distritos$paginas[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    links <- x %>% read_html() %>% html_nodes(".item-link") %>% html_attr(name = "href", default = NA)
    
    if (area == "Ciudad" | area == "ciudad") {
      links_anuncios <- data.frame(anuncio = paste0("https://www.idealista.com", links), distrito = tabla_distritos$distritos[p])
    } else {
      links_anuncios <- data.frame(anuncio = paste0("https://www.idealista.com", links), distrito = NA)
    }
    
    links_anuncios_tot <- rbind(links_anuncios_tot, links_anuncios)
    
    p <- p - 1
    if (p == 0) {
      break
    }
  }
  
  links_anuncios_tot <- unique(links_anuncios_tot)
  links_anuncios_tot <- links_anuncios_tot[-1,]
  
  links_anuncios_tot <<- links_anuncios_tot
  
  line <- data.frame("Titulo", "Distrito", "Barrio", "calle", "Precio", "Precio_m2", "Superficie", "Habitaciones", "Descripcion", "Anunciante", "Agencia", "Url", "ultima_actualizacion", "fecha")
  
  write.table(line, file = ruta, sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "", append = FALSE)
  
  n <- length(links_anuncios_tot$anuncio)
  
  links_anuncios_tot$anuncio <- as.character(links_anuncios_tot$anuncio)
  
  
  print(paste("Idealisto ha extraido las urls de", n, "anuncios."))
  print(paste("Ahora Idealisto comenzará a extraer la información de esos", n, "anuncios. Pero antes vamos a descansar durante 30 segundos"))
  
  Sys.sleep(30)
  
  print("Comenzamos a extraer los anuncios...")
  start_2 <- Sys.time()
  ###### 
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
    
    
    
    if (length(titulo) == 0) {
      titulo <- "Sin título"
    }
    
    if (length(precio) == 0) {
      precio <- "Sin precio"
    }
    
    if (length(descrip) == 0) {
      descrip <- "Sin descripción"
    }
    
    if (length(agencia) == 0) {
      agencia <- NA
    }
    
    if (length(anunciante) == 0 | isTRUE(anunciante == " ")) {
      anunciante <- "Particular"
    }
    
    calle <- ubi[1]
    barrio <- as.character(ubi[str_detect(ubi, pattern = "Barrio ") == TRUE])
    
    if (area == "ciudad" | area == "Ciudad") {
      distrito <- links_anuncios_tot$distrito[p]
    } else {
      distrito <- as.character(ubi[str_detect(ubi, pattern = "Distrito") == TRUE])
      distrito <- str_replace_all(string = distrito, pattern = "Distrito ", replacement = "")
    }
    
    
    if (length(distrito) == 0) {
      distrito <- str_replace(string = links_anuncios_tot[p], pattern = "https://www.idealista.com/alquiler-viviendas/", replacement = "")
    }
    
    if (length(barrio) == 0) {
      barrio <- NA
    }
    
    if (length(calle) == 0) {
      calle <- NA
    }
    
    metros <- str_extract(pattern = "..m²|...m²|....m²|.....m²|......m²|.......m²|........m²|.........m²|..........m²", string = info)
    metros <- str_replace_all(string = metros, pattern = " m²| |\\.", replacement = "")
    metros <- as.numeric(metros)
    habit <- as.integer(str_replace_all(pattern = " hab.", replacement = "", string = str_extract(pattern = ".hab.|..hab.", string = info)))
    descrip <- str_replace_all(descrip, pattern = '\"', "")
    fecha <- Sys.Date()
    
    if (str_detect(precio, "eur")) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " eur/mes|\\.", replacement = "")))
    } else if (str_detect(precio, "€")) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " €/mes|\\.", replacement = "")))
    }
    
    try(precio_m2 <- precio/metros)
    
    
    try(line <- data.frame(titulo, distrito, barrio, calle, precio, precio_m2, metros, habit, descrip, anunciante, agencia, links_anuncios_tot$anuncio[p], alta, fecha))
    
    n <- n - 1
    
    
    try(write.table(line, file = ruta, sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = ""))    
    #########
    
    
    
    if (Sys.time() > start_2 + 420) {
      stop_t <- sample(x = 100:120, size = 1)
      #print(paste("Para que no se nos cabree Idealista vamos a parar la máquina durante", stop_t, "segundos."))
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