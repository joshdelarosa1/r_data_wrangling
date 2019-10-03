---
title: "Census Geocoder"
author: "Josh DeLaRosa"
date: "2019 Oct 02"
output: 
  html_document: 
    highlight: zenburn
    theme: united
    toc: yes
    toc_float: yes
    keep_md: yes
---

# Function Creation
Create function for Census geography look-up using lat/long

```r
census_latlong<-function (street, city, state) 
{
  call_start <- "https://geocoding.geo.census.gov/geocoder/locations/address?"
  url <- paste0("street=", utils::URLencode(street), "&city=", 
                utils::URLencode(city), "&state=", state)
  call_end <- "&benchmark=4&format=json"
  url_full <- paste0(call_start, url, call_end)
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$addressMatches) == 0) {
    message(paste0("Address (", street, " ", city, " ", 
                   state, ") returned no address matches. An NA was returned."))
    return(NA_character_)
  }
  else {
    if (length(response$result$addressMatches) > 1) {
      message(paste0("Address (", street, " ", city, " ", 
                     state, ") returned more than one address match. The first match was returned."))
    }
    return(paste(response$result$addressMatches[[1]]$coordinates$y,response$result$addressMatches[[1]]$coordinates$x,sep=','))
  }
}
```

Test `census_latlong`function

```r
census_latlong("1600 PENNSYLVANIA AVE NW", "WASHINGTON", "DC")
```

```
## [1] "38.898754,-77.03535"
```

Create function for Census geography look-up using address

```r
call_geolocator2<-function (street, city, state) 
{
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/address?"
  url <- paste0("street=", utils::URLencode(street), "&city=", 
                utils::URLencode(city), "&state=", state)
  call_end <- "&benchmark=9&vintage=910&layers=14&format=json"
  url_full <- paste0(call_start, url, call_end)
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$addressMatches) == 0) {
    message(paste0("Address (", street, " ", city, " ", 
                   state, ") returned no address matches. An NA was returned."))
    return(NA_character_)
  }
  else {
    if (length(response$result$addressMatches) > 1) {
      message(paste0("Address (", street, " ", city, " ", 
                     state, ") returned more than one address match. The first match was returned."))
    }
    return(response$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID)
  }
}
```

Test `call_geolocator2`function

```r
call_geolocator2("1600 PENNSYLVANIA AVE NW", "WASHINGTON", "DC")
```

```
## Address (1600 PENNSYLVANIA AVE NW WASHINGTON DC) returned more than one address match. The first match was returned.
```

```
## [1] "110010062021036"
```


```r
call_geolocator_latlon2<-function (lat, lon) 
  {
    call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"
    url <- paste0("x=", lon, "&y=", lat)
    call_end <- "&benchmark=9&vintage=910&layers=14&format=json"
    url_full <- paste0(call_start, url, call_end)
    r <- httr::GET(url_full)
    httr::stop_for_status(r)
    response <- httr::content(r)
    if (length(response$result$geographies$`Census Blocks`) == 
        0) {
      message(paste0("Lat/lon (", lat, ", ", lon, ") returned no geocodes. An NA was returned."))
      return(NA_character_)
    }
    else {
      if (length(response$result$geographies$`Census Blocks`) > 
          1) {
        message(paste0("Lat/lon (", lat, ", ", lon, ") returned more than geocode. The first match was returned."))
      }
      return(response$result$geographies$`Census Blocks`[[1]]$GEOID)
    }
  }
```

Test `call_geolocator_latlon2`function

```r
call_geolocator_latlon2("38.898754","-77.03535")
```

```
## [1] "110010062021031"
```

Create function that uses the `call_geolocator2` and `census_latlong`

```r
append_geoid2<-function (address, geoid_type = "bl") 
{
  if ("lat" %in% colnames(address) && "lon" %in% colnames(address)) {
    geoids <- vector(mode = "character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator_latlon2(address$lat[i], 
                                          address$lon[i])
    }
  }
  else {
    geoids <- vector(mode = "character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator2(as.character(address$street[i]), 
                                   as.character(address$city[i]), as.character(address$state[i]))
    }
  }
  address <- dplyr::mutate(address, geoid = geoids)
  if (geoid_type == "co") {
    end <- 5
  }
  else if (geoid_type == "tr") {
    end <- 11
  }
  else if (geoid_type == "bg") {
    end <- 12
  }
  else {
    end <- 15
  }
  address <- dplyr::mutate(address, geoid = ifelse(is.na(geoid), 
                                                   NA_character_, substr(geoid, 1, end)))
  return(address)
}
```

# Geocode
Set library

```r
library("tidyverse")
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.2.0     ✔ purrr   0.3.2
## ✔ tibble  2.1.3     ✔ dplyr   0.8.1
## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
## ✔ readr   1.3.1     ✔ forcats 0.4.0
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

Test `census_latlong` function

```r
census_latlong("42-09 28th St"," Long Island City", "NY")
```

```
## Address (42-09 28th St  Long Island City NY) returned more than one address match. The first match was returned.
```

```
## [1] "40.749714,-73.93909"
```

Create test dataset

```r
# create dataset
street<-c('42-09 28th St','160 W 100th St','350 Linwood St')
city<-c('Long Island City','New York','Brooklyn')
state<-c('NY','NY','NY')

DOH_BSA <- data.frame(street, city, state) %>%
  mutate_if(is.factor, as.character)  # turns factor into character

glimpse(DOH_BSA)
```

```
## Observations: 3
## Variables: 3
## $ street <chr> "42-09 28th St", "160 W 100th St", "350 Linwood St"
## $ city   <chr> "Long Island City", "New York", "Brooklyn"
## $ state  <chr> "NY", "NY", "NY"
```

Geocode the data

```r
DOH_geocoded<-DOH_BSA %>%
  mutate(LL=mapply(census_latlong,street,city,state)) %>% # returns lat/long
  separate(LL, into = c("lat", "long"),sep = ",") %>% #splits lat/long response into 2 columns
  append_geoid2('bl')  %>% # get census geo for addresses
  mutate(fip_state= str_sub(geoid,1,2)) %>% # begin to parse geography data
  mutate(fip_county= str_sub(geoid,3,5)) %>%
  mutate(fip_tract= str_sub(geoid,6,11)) %>% 
  mutate(fip_block= str_sub(geoid,12,15))
```

```
## Address (42-09 28th St Long Island City NY) returned more than one address match. The first match was returned.
```

```
## Address (42-09 28th St Long Island City NY) returned more than one address match. The first match was returned.
```

```
## Address (160 W 100th St New York NY) returned more than one address match. The first match was returned.
```

```r
DOH_geocoded
```

```
##           street             city state       lat      long
## 1  42-09 28th St Long Island City    NY 40.749714 -73.93909
## 2 160 W 100th St         New York    NY 40.796417 -73.96795
## 3 350 Linwood St         Brooklyn    NY 40.677097 -73.88342
##             geoid fip_state fip_county fip_tract fip_block
## 1 360810019001050        36        081    001900      1050
## 2 360610185002000        36        061    018500      2000
## 3 360471170002000        36        047    117000      2000
```
