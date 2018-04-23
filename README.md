## WARNING:
idealista has blocked this scraper. For the moment idealisto does not work.

# What is idealisto?
Idealisto is a web scrapper for the spanish real estate website [Idealista.com](https://www.idealista.com/). It downloads rent ads and creates a csv with the data.


# How to install it
Type the following text in your R studio console:

```
library(devtools)
install_github("meneos/idealisto")
```

# get_ functions

Idealisto differentiates between the type of area being scraped and between rent and for sale ads. If what you want to download is a city you need to use ```get_city()```, if is a city district you'll need ```get_distrito()``` and of you want a whole province you'll need ```get_provincia()```. Whether you want rent or for sale ads you have to specify as the second argument "rent" or "sale".

If the area you need includes 1.800 or less ads you are encouraged to use a faster alternative: ```get_fast()```

# Example

If you are interested in the rent ads of particular city disctrict just type:
```
get_distrito("https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/", "rent")
```

To download the for sale ads of a whole city (this may take hours in the cases of Madrid or Barcelona) type:
```
get_city("https://www.idealista.com/alquiler-viviendas/madrid-madrid/", "sale")
```

If you want to specify a path in your computer where the csv will be created yo can add a second character argument (by default the csv will be created in the working directory):

```
get_city("https://www.idealista.com/alquiler-viviendas/madrid-madrid/", "VALID-PATH-IN-YOUR-COMPUTER")
```

# vigencia() function
The vigencia function gives useful data if you call it weeks later on the results of the idealisto function. It adds columns to the data frame of a previous idealisto search related to the current state of the ads: are the ads is still online, do they have a new price, if the ads are no longer online it will tell how many days passed until it went offline, etc...

```
vigencia("VALID-PATH-IN-YOUR-COMPUTER-TO-A-IDEALISTO-CSV")
```


# Fotocasa

The real estate web [fotocasa.com](https://www.fotocasa.es/es/) has it's own function but it may not work correctly.


# Windows

There is a problem with windows involving the "€" currency symbol. It will be solved soon.
