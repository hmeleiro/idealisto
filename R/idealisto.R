#' Scrap idealista website.
#' 
#' This function scraps idealista (a spanish real estate website) and downloads all the rent ads in the given province, city, district or neighborhood.
#' @param url An idealista website url that links to the area you want to scrap, e.g. 'https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/'.
#' @param area The type of area you want to scrap. It can take these values: 'Provincia', 'Ciudad', 'Distrito' or 'Barrio'.
#' @param ruta A valid path in your computer where you want to create the csv file.
#' @return It returns a csv in the specified path
#' @export
idealisto <- function(url, area, ruta = "~/idealisto.csv") {
  
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
    print(paste("Estos son los links a las páginas principales de cada", subarea, ":"))
    print(url_distris_tot)
    url_distris_tot_ <- paste0("https://www.idealista.com", url_distris)
  }
  
  
  
  repeat {
    
    p <- length(url_distris_tot)
    
    sig_pag_tot <- c()
    
    repeat {
      x <- GET(url_distris_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
      sig_pag <- x %>% read_html() %>% html_nodes(".icon-arrow-right-after") %>% html_attr(name = "href", default = NA)
      
      
      if (length(sig_pag) == 0) {
        sig_pag <- NA
      } else {
        sig_pag_tot <- c(sig_pag,sig_pag_tot)
      }
      
      p <- p - 1
      
      if (p == 0) {
        break
      }
      
    }
    
    url_distris_tot <- paste0("https://www.idealista.com", sig_pag_tot)
    
    if (url_distris_tot == "https://www.idealista.com") {
      break
    }
    
    
    url_distris_tot_ <- c(url_distris_tot_, url_distris_tot)
    
    
    if (area == "Barrio" | area == "barrio") {
      
    } else {
      print(url_distris_tot_)
      print(paste("Capturando los links de todas las páginas de cada", subarea, "..."))
    }
    
  }
  
  urls_paginas <- unique(url_distris_tot_[url_distris_tot_ != "https://www.idealista.com"])
  
  p <- length(urls_paginas)
  
  links_anuncios_tot <- c()
  
  repeat {
    x <- GET(urls_paginas[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    links <- x %>% read_html() %>% html_nodes(".item-link") %>% html_attr(name = "href", default = NA)
    
    links_anuncios <- paste0("https://www.idealista.com", links)
    links_anuncios_tot <- c(links_anuncios_tot, links_anuncios)
    
    print(links_anuncios_tot)
    print("Capturando los links a todos los anuncios...")
    
    p <- p - 1
    if (p == 0) {
      break
    }
  }
  
  links_anuncios_tot <- unique(links_anuncios_tot)
  
  links_anuncios_tot <<- links_anuncios_tot
  
  line <- data_frame("Titulo", "Distrito", "Barrio", "calle", "Precio", "Precio_m2", "Superficie", "Habitaciones", "Descripcion", "Anunciante", "Agencia", "Url", "fecha")
  
  write.table(line, file = ruta, sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "")
  
  p <- length(links_anuncios_tot)
  
  print(paste("Idealisto ha extraido las urls de", p, "anuncios."))
  print(paste("Ahora Idealisto comenzará a extraer la información de esos", p, "anuncios. Pero antes vamos a descansar durante 30 segundos"))
  
  Sys.sleep(30)
  
  
  start_2 <- Sys.time()
  
  repeat {
    x <- GET(links_anuncios_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    
    titulo <- x %>% read_html() %>% html_nodes(".main-info__title-main") %>% html_text()
    precio <- x %>% read_html() %>% html_nodes(".h1-simulated") %>% html_text()
    ubi <- x %>% read_html() %>% html_nodes("#headerMap li") %>% html_text()
    anunciante <- x %>% read_html() %>% html_nodes(".name") %>% html_text()
    agencia <- x %>% read_html() %>% html_nodes(".about-advertiser-name") %>% html_text()
    info <- x %>% read_html() %>% html_nodes(".info-features") %>% html_text()
    descrip <- x %>% read_html() %>% html_nodes(".expandable") %>% html_text()
    
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
    
    #calle <- as.character(ubi[str_detect(ubi, pattern = "Calle ") == TRUE])
    calle <- ubi[1]
    distrito <- as.character(ubi[str_detect(ubi, pattern = "Distrito ") == TRUE])
    barrio <- as.character(ubi[str_detect(ubi, pattern = "Barrio ") == TRUE])
    
    
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
    distrito <- str_replace_all(string = distrito, pattern = "Distrito ", replacement = "")
    descrip <- str_replace_all(descrip, pattern = '\"', "")
    fecha <- Sys.Date()
    
    if (str_detect(precio, "eur")) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " eur/mes|\\.", replacement = "")))
    } else if (str_detect(precio, "€")) {
      try(precio <- as.integer(str_replace_all(string = precio, pattern = " €/mes|\\.", replacement = "")))
    }
    
    precio_m2 <- precio/metros
    
    
    line <- data_frame(titulo, distrito, barrio, calle, precio, precio_m2, metros, habit, descrip, anunciante, agencia, links_anuncios_tot[p], fecha)
    print(line)
    
    process <- 100 - ((p/length(links_anuncios_tot))*100)
    print(paste0("Idealisto lleva descargados el ", round(process, digits = 1),"% de los anuncios."))
    
    
    write.table(line, file = ruta, sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = "")
    
    p <- p - 1
    if (p <= 0) {
      break
    }
    
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
  print(paste("Idealisto ha descargado los", length(links_anuncios_tot), "de los anuncios"))
  print(round(diff, digits = 1))
}