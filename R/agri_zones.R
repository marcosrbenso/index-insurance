library(cropCalendars)
library(geobr)
library(data.table)
library(sf)
library(tidyverse)
library(ncdf4)
library(terra)
library(lattice)
library(latticeExtra)
library(rasterVis)
library(tidyverse)

setwd("C:/Projetos/17_index_insurance/indices")
my_cv <- function(x){
  sd(x,na.rm=T)/mean(x,na.rm=T)
}

my_seasonality <- function(CVprec,
                           CVtemp,
                           Tmin_coldest){

  type <- rep(NA, length(CVprec))

  # Classify seasonality type
  type[CVprec <= 0.4  & CVtemp <= 0.1] <- 5
  type[CVprec  > 0.4  & CVtemp <= 0.1] <- 1
  type[CVprec <= 0.4  & CVtemp  > 0.1] <- 2
  type[CVprec  > 0.4  & CVtemp  > 0.1] <- 6

  # Further classification for type 4
  type[type == 6 & Tmin_coldest  > 10] <- 3
  type[type == 6 & Tmin_coldest <= 10] <- 4

  return(type)

}

dates <- unlist(lapply(str_split(list.files(pattern = "Prctot"),"_"),function(x){x[2]}))
dates <- as.Date(paste(dates,"01",sep='-'))
months <- month(dates)
head()

prec <- lapply(1:12,function(i){
  app(rast(list.files(pattern = "Prctot"))[[months==i]],mean)})

temp <- lapply(1:12,function(i){
  app(rast(list.files(pattern = "temp"))[[months==i]],mean)})

ss <- c(app(rast(prec),my_cv),
        app(rast(temp),my_cv),
        app(rast(temp),min))

season <- lapp(ss,my_seasonality)
plot(season, col = c('#398286','#9D2337','#75BEC7','#E35D61','#D5D3D3'))



