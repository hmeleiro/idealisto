#' Scrap Fotocasa website.
#' 
#' This function scraps Fotocasa (a spanish real estate website) and downloads all the rent ads in the given province, city, disctrict or neighborhood.
#' @param url A Fotocasa website url that links to the area you want to scrap, e.g. 'https://www.fotocasa.es/es/alquiler/casas/madrid-capital/todas-las-zonas/l'.
#' @param ruta A valid path in your computer where you want to create the csv file.
#' @return It returns a csv in the specified path
#' @export
fotocasa <- function(url, ruta = "~/fotocasa.csv") {
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
  
  
  
  x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
  paginas <- x %>% read_html() %>% html_nodes(".sui-PaginationBasic-item a") %>% html_text()
  paginas <- as.numeric(paginas)
  paginas <- seq(from = 1, to = max(paginas, na.rm = TRUE), by = 1)
  urls_pags <- paste0(url,paginas)
  urls_pags
  
  ads <- c()
  
  for(p in 1:length(urls_pags)) {
    x <- GET(urls_pags[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    ad <- x %>% read_html() %>% html_nodes(".re-CardImage-link") %>% html_attr(name = "href")
    ad <- paste0("https://www.fotocasa.es", ad)
    ads <- c(ad, ads)
    print(ad)
  }
  
  print(paste("Hemos capturado las urls de", length(ads), "anuncios."))
  
  line <- data_frame("Titulo",
                     "Distrito",
                     "Barrio",
                     "Precio",
                     "Precio_m2",
                     "Metros",
                     "Habitaciones",
                     "Baños",
                     "Planta",
                     "Descripción",
                     "anunciante",
                     "agencia",
                     "url")
  
  #line <- data_frame("Titulo", "Distrito", "Barrio", "calle", "Precio", "Precio_m2", "Superficie", "Habitaciones", "Descripcion", "Anunciante", "Agencia", "Url", "fecha")
  
  
  write.table(line, file = ruta, sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "")
  
  
  start_2 <- Sys.time()
  
  
  for (p in 1:length(ads)) {
    x <- GET(ads[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    
    titulo <- x %>% read_html() %>% html_nodes(".property-title") %>% html_text()
    titulo <- str_replace_all(titulo, "\r\n", "")
    titulo <- str_trim(titulo, "both")
    
    if(length(titulo) == 0) {
      titulo <- NA
    }
    
    precio <- x %>% read_html() %>% html_nodes("#priceContainer") %>% html_text()
    precio <- str_remove_all(precio, "\r\n")
    precio <- str_remove_all(precio, "€|mes|/")
    precio <- str_remove_all(precio, "\\.")
    precio <- str_trim(precio, "both")
    precio <- as.numeric(precio)
    
    if(length(precio) == 0) {
      precio <- NA
    }
    
    
    baños <- x %>% read_html() %>% html_nodes("#litBaths") %>% html_text()
    baños <- str_remove_all(baños, "baños")
    baños <- str_remove_all(baños, "baño")
    baños <- as.numeric(baños)
    
    if(length(baños) == 0) {
      baños <- NA
    }
    
    rooms <- x %>% read_html() %>% html_nodes("#litRooms") %>% html_text()
    rooms <- str_remove_all(rooms, "habitaciones|habitación")
    rooms <- as.numeric(rooms)
    
    if(length(rooms) == 0) {
      rooms <- NA
    }
    
    
    metros <- x %>% read_html() %>% html_nodes("#litSurface") %>% html_text()
    metros <- str_remove_all(metros, "m²")
    metros <- str_remove_all(metros, "\\.")
    metros <- as.numeric(metros)
    
    if(length(metros) == 0) {
      metros <- NA
    }
    
    planta <- x %>% read_html() %>% html_nodes("#litFloor") %>% html_text()
    #planta <- str_remove_all(planta, "ª planta")
    
    #if (planta == "Bajos") {
    # planta <- 0
    #} else if(length(planta) == 0) {
    #  planta <- NA
    #}
    #planta <- as.numeric(planta)
    
    
    descrip <- x %>% read_html() %>% html_nodes(".detail-description") %>% html_text()
    descrip <- str_remove_all(descrip, "\r\n")
    descrip <- str_trim(descrip, "both")
    descrip <- str_remove_all(descrip, '"')
    
    if(length(descrip) == 0) {
      descrip <- NA
    }
    
    agencia <- x %>% read_html() %>% html_nodes("#lnkMoreBuildings") %>% html_text()
    agencia
    
    if (length(agencia) == 0) {
      anunciante <- "Particular"
      agencia <- NA
    } else if (length(agencia) > 0) {
      anunciante <- "Profesional"
    }
    
    
    
    link <- ads[p]
    
    if(length(link) == 0) {
      link <- NA
    }
    
    barrio <- x %>% read_html() %>% html_nodes("#ctl00_content1_PaginationTop_breadcrumbclassic_lbl_LocalizationUpperLevelwithLink") %>% html_text()
    barrio
    
    dis <- x %>% read_html() %>% html_nodes("#lnk_LocalizationUpperLevel") %>% html_attr("href")
    dis <- paste0("https://www.fotocasa.es", dis)
    dis
    x <- GET(dis, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))  ##   Creo q para pasar las páginas en el GET
    distrito <- x %>% read_html() %>% html_nodes(".re-Breadcrumb-link") %>% html_text()
    distrito <- distrito[5]
    distrito
    
    if (length(barrio) == 0) {
      barrio <- NA
    }
    if (length(distrito) == 0) {
      distrito <- NA
    }
    
    
    precio_m2 <- precio/metros
    print(precio_m2)
    
    line <- data_frame(titulo,
                       distrito,
                       barrio,
                       precio,
                       precio_m2,
                       metros, 
                       rooms,
                       baños,
                       planta,
                       descrip,
                       anunciante,
                       agencia,
                       link)
    print(line)
    
    write.table(line, file = ruta, sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = "", dec = ".")
    
    if (Sys.time() > start_2 + 420) {
      stop_t <- sample(x = 90:110, size = 1)
      print(paste("Para que no se nos cabree Fotocasa vamos a parar la máquina durante", stop_t, "segundos."))
      Sys.sleep(time = stop_t)
      start_2 <- Sys.time()
    }
  }
  
  stop <- Sys.time()
  diff <- stop - start
  print(diff)
}






