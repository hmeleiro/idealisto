# What is idealisto?
Idealisto is a web scrapper for the spanish real estate website [Idealista.com](https://www.idealista.com/) (soon also for [fotocasa.com](https://www.fotocasa.es/es/)). It downloads rent ads and creates a csv with the data.


# How to install it
Type the following text in your R studio console:

```
library(devtools)
install_github("meneos/idealisto")
```

# Example

If you are interested in a Madrid disctrict in particular just type:
```
idealisto("https://www.idealista.com/alquiler-viviendas/madrid/arganzuela/", "distrito", "VALID-PATH-IN-YOUR-COMPUTER")

```

To download a whole city (this may take hours in the cases of Madrid or Barcelona) type:
```
idealisto("https://www.idealista.com/alquiler-viviendas/madrid-madrid/", "ciudad", "VALID-PATH-IN-YOUR-COMPUTER")

```


# vigencia() function
The vigencia function gives useful data if you call it weeks later on the results of the idealisto function. It adds columns to the data frame of a previous idealisto search related to the current state of the ads: are the ads is still online, do they have a new price, if the ads are no longer online it will tell how many days passed until it went offline, etc...

```
Madrid <- read.csv("VALID-PATH-IN-YOUR-COMPUTER-TO-A-IDEALISTO-CSV")

vigencia(Madrid)

```
