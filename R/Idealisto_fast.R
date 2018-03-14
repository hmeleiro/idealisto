#' Scrap idealista website.
#' 
#' This function scraps spanish idealista (a real estate website) and downloads a maximum of 1.800 rent ads in the given province, city, disctrict or neighborhood. So if you want to scrap an area that has more than 1.800 ads use idealisto function.
#' @param url An idealisto website url that links to the area you want to scrap, e.g. 'https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/'.
#' @param area The type of area you want to scrap. It can take these values: 'Provincia', 'Ciudad', 'Distrito' or 'Barrio'.
#' @param ruta A valid path in your computer where you want to create the csv file.
#' @return It returns a csv in the specified path
#' @export
idealisto_fast <- function(url, ruta = "~/idealisto_fast.csv") {
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
  
  
  
  url_base <- url       ## Defino la url base
  
  #### CALCULO CUANTAS PÁGINAS TIENE EL AREA SOLICITADA
  x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  anuncios <- x %>% read_html() %>% html_nodes(".h1-simulated") %>% html_text()
  anuncios <- str_remove(anuncios, "\\.")
  q <- round(as.numeric(anuncios)/30)
  
  ####  CREO EL ÍNDICE DE PÁGINAS
  times <- 1:q                ## Creo un vector con los números del 1 al n para cambiar las páginas de la web
  url <- c(replicate(expr = paste0(url_base, "pagina-", times,".htm"), n = 1))  ##Construyo el string con las urls definitivas. Una para cada página a scrapear
  print(url)
  #url <- c(url, replicate(expr = paste0(url_base), n = 1))
  
  
  links_anuncios_tot <- c()
  
  #### EXTRAIGO LOS LINKS DE TODOS LOS ANUNCIOS DEL AREA
  for (p in 1:length(url)) {
    x <- GET(url[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
    links <- x %>% read_html() %>% html_nodes(".item-link") %>% html_attr(name = "href", default = NA)
    
    links_anuncios <- paste0("https://www.idealista.com", links)
    links_anuncios_tot <- c(links_anuncios_tot, links_anuncios)
    links_anuncios_tot <- unique(links_anuncios_tot)
    
    print(links_anuncios_tot)
    print("Capturando los links a todos los anuncios...")
  }
  
  
  links_anuncios_tot <<- links_anuncios_tot
  
  line <- data.frame("Titulo", "Distrito", "Barrio", "calle", "Precio", "Precio_m2", "Superficie", "Habitaciones", "Descripcion", "Anunciante", "Agencia", "Url", "fecha")
  
  write.table(line, file = ruta, sep = ",", quote = FALSE, col.names = FALSE, row.names = FALSE, na = "")
  
  n <- length(links_anuncios_tot)
  
  print(paste("Idealisto ha extraido las urls de", n, "anuncios."))
  print(paste("Ahora Idealisto comenzará a extraer la información de esos", n, "anuncios. Pero antes vamos a descansar durante 30 segundos"))
  
  Sys.sleep(30)
  
  
  start_2 <- Sys.time()
  
  
  
  ###### EMPIEZA EL SCRAPER
  for (p in 1:length(links_anuncios_tot)) {     
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
    precio <- as.integer(str_replace_all(string = precio, pattern = " eur/mes|\\.", replacement = ""))
    descrip <- str_replace_all(descrip, pattern = '\"', "")
    fecha <- Sys.Date()
    
    precio_m2 <- precio/metros
    
    
    line <- data.frame(titulo, distrito, barrio, calle, precio, precio_m2, metros, habit, descrip, anunciante, agencia, links_anuncios_tot[p], fecha)
    print(line)
    
    n <- n - 1
    p <- p + 1
    process <- 100 - ((n/length(links_anuncios_tot))*100)
    print(paste0("Idealisto lleva descargados el ", round(process, digits = 1),"% de los anuncios."))
    
    
    write.table(line, file = ruta, sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE, na = "")
    
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
  print(paste("Idealisto ha descargado los", length(links_anuncios_tot), "de los anuncios"))
  print(round(diff, digits = 1))
}