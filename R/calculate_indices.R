library(ncdf4)
library(lubridate)
library(terra)
library(tidyverse)
library(data.table)
library(sf)
library(zoo)
library(RcppRoll)

setwd("C:\\Projetos\\17_index_insurance\\Observed data")

raster_create <- function(sum_matrix,lat,lon){
  r <- rast(nrows = length(lat), ncols = length(lon),
            xmin = min(lon), xmax = max(lon),
            ymin = min(lat), ymax = max(lat),
            crs = "EPSG:4326")
  values(r) <- as.vector(sum_matrix)
  r <- terra::flip(r)
  return(r)
}


rx5day_fun <- function(x){
  if(sum(is.na(x))==length(x)){
    NA
  }else{
    max(roll_sum(x,n = 1, align = "left"),na.rm=T)
  }
}


pr_files <- list.files(pattern = "pr")
Tmin_files <- list.files(pattern = "Tmin")
Tmax_files <- list.files(pattern = "Tmax")
f <- 3
for(f in 2:3){
    nc <- nc_open(pr_files[f])
    nc_Tmax <- nc_open(Tmin_files[f])
    nc_Tmin <- nc_open(Tmax_files[f])

    time_vals <- ncvar_get(nc, "time")
    time_units <- ncatt_get(nc, "time", "units")$value
    origin_str <- sub(".*since ", "", time_units)
    dates <- as.Date(time_vals/24, origin = origin_str)
    ym <- format(dates, "%Y-%m")

    pr_data <- ncvar_get(nc)
    Tmin_data <- ncvar_get(nc_Tmin)
    Tmax_data <- ncvar_get(nc_Tmax)

    unique_ym <- unique(ym)
    # Extrair coordenadas
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")

    nc_close(nc)
    nc_close(nc_Tmin)
    nc_close(nc_Tmax)

    for(i in seq_along(unique_ym)){
      cat("Processando:", unique_ym[i],"\n")
      idx <- which(ym == unique_ym[i])

      tmed <- (Tmax_data[,,idx]+Tmin_data[,,idx])/2

      # Sum across the time axis
      txx <- apply(Tmax_data[,,idx], c(1, 2), max)
      tnx <- apply(Tmin_data[,,idx], c(1, 2), max)
      rx5day <- apply(pr_data[,,idx], c(1, 2), rx5day_fun)
      Prctot <- apply(pr_data[,,idx], c(1, 2), sum)
      temp <- apply(tmed, c(1, 2), mean)

      writeRaster(raster_create(txx, lat,lon),
                  file.path("C:/Projetos/17_index_insurance/indices",
                             paste0("txx","_",unique_ym[i],"_",".tif")),overwrite=TRUE)
      writeRaster(raster_create(tnx, lat,lon),
                  file.path("C:/Projetos/17_index_insurance/indices",
                             paste0("tnx","_",unique_ym[i],"_",".tif")),overwrite=TRUE)
      writeRaster(raster_create(rx5day, lat,lon),
                  file.path("C:/Projetos/17_index_insurance/indices",
                             paste0("rx5day","_",unique_ym[i],"_",".tif")),overwrite=TRUE)
      writeRaster(raster_create(Prctot, lat,lon),
                  file.path("C:/Projetos/17_index_insurance/indices",
                             paste0("Prctot","_",unique_ym[i],"_",".tif")),overwrite=TRUE)
      writeRaster(raster_create(temp, lat,lon),
                  file.path("C:/Projetos/17_index_insurance/indices",
                             paste0("temp","_",unique_ym[i],"_",".tif")),overwrite=TRUE)


    }
}





#### Historic climate data ####
setwd("C:/Users/marco/Downloads")
pr_files <- list.files(pattern = "-pr-")[-2]
Tmin_files <- list.files(pattern = "tasmin")
Tmax_files <- list.files(pattern = "tasmax")


my_mean <- function(x){
  if(sum(is.na(x))==length(x)){
    NA
  }
  else{
    mean(x,na.rm=T)
  }
}
my_sum <- function(x){
  if(sum(is.na(x))==length(x)){
    NA
  }
  else{
    sum(x,na.rm=T)
  }

  }
my_max <- function(x){
  if(sum(is.na(x))==length(x)){
    NA
  }
  else{
    max(x,na.rm=T)
  }

  }

for(f in 1:3){
  nc <- nc_open(pr_files[f])
  nc_Tmax <- nc_open(Tmin_files[f])
  nc_Tmin <- nc_open(Tmax_files[f])

  time_vals <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  origin_str <- sub(".*since ", "", time_units)
  dates <- as.Date(time_vals, origin = origin_str)
  ym <- format(dates, "%Y-%m")

  pr_data <- ncvar_get(nc)
  Tmin_data <- ncvar_get(nc_Tmin)
  Tmax_data <- ncvar_get(nc_Tmax)

  unique_ym <- unique(ym)
  # Extrair coordenadas
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")

  nc_close(nc)
  nc_close(nc_Tmin)
  nc_close(nc_Tmax)

  model <- str_split(Tmax_files,"-tasmax-")[f][[1]][1]

  for(i in seq_along(unique_ym)){
    cat("Processando:", unique_ym[i],"\n")
    idx <- which(ym == unique_ym[i])

    tmed <- (Tmax_data[,,idx]+Tmin_data[,,idx])/2


    # Sum across the time axis
    txx <- apply(Tmax_data[,,idx], c(1, 2), my_max)
    tnx <- apply(Tmin_data[,,idx], c(1, 2), my_max)
    rx5day <- apply(pr_data[,,idx], c(1, 2), rx5day_fun)
    Prctot <- apply(pr_data[,,idx], c(1, 2), my_sum)
    temp <- apply(tmed, c(1, 2), my_mean)

    raster_create(txx, lat,lon) %>% plot()
    raster_create(Prctot, lat,lon) %>% plot()
    raster_create(temp, lat,lon) %>% plot()

    writeRaster(raster_create(txx, lat,lon),
                file.path("E:\\",
                          paste0("txx","_",unique_ym[i],"_",model,".tif")),overwrite=TRUE)
    writeRaster(raster_create(tnx, lat,lon),
                file.path("E:\\",
                          paste0("tnx","_",unique_ym[i],"_",model,".tif")),overwrite=TRUE)
    writeRaster(raster_create(rx5day, lat,lon),
                file.path("E:\\",
                          paste0("rx5day","_",unique_ym[i],"_",model,".tif")),overwrite=TRUE)
    writeRaster(raster_create(Prctot, lat,lon),
                file.path("E:\\",
                          paste0("Prctot","_",unique_ym[i],"_",model,".tif")),overwrite=TRUE)
    writeRaster(raster_create(temp, lat,lon),
                file.path("E:\\",
                          paste0("temp","_",unique_ym[i],"_",model,".tif")),overwrite=TRUE)

  }
}

setwd("E:\\")

pr_files <- list.files(pattern = "-pr-")
Tmin_files <- list.files(pattern = "tasmin")
Tmax_files <- list.files(pattern = "tasmax")

for(f in 1:6){
  nc <- nc_open(pr_files[f])
  nc_Tmax <- nc_open(Tmin_files[f])
  nc_Tmin <- nc_open(Tmax_files[f])

  time_vals <- ncvar_get(nc, "time")
  time_units <- ncatt_get(nc, "time", "units")$value
  origin_str <- sub(".*since ", "", time_units)
  dates <- as.Date(time_vals, origin = origin_str)
  ym <- format(dates, "%Y-%m")

  pr_data <- ncvar_get(nc)
  Tmin_data <- ncvar_get(nc_Tmin)
  Tmax_data <- ncvar_get(nc_Tmax)

  unique_ym <- unique(ym)
  # Extrair coordenadas
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")

  nc_close(nc)
  nc_close(nc_Tmin)
  nc_close(nc_Tmax)

  model <- str_split(Tmax_files,"-tasmax-")[f][[1]][1]
  scenario <- substr(str_split(Tmax_files,"-tasmax-")[f][[1]][2],1,6)

  for(i in seq_along(unique_ym)){
    cat("Processando:", unique_ym[i],"\n")
    idx <- which(ym == unique_ym[i])

    tmed <- (Tmax_data[,,idx]+Tmin_data[,,idx])/2


    # Sum across the time axis
    txx <- apply(Tmax_data[,,idx], c(1, 2), my_max)
    tnx <- apply(Tmin_data[,,idx], c(1, 2), my_max)
    rx5day <- apply(pr_data[,,idx], c(1, 2), rx5day_fun)
    Prctot <- apply(pr_data[,,idx], c(1, 2), my_sum)
    temp <- apply(tmed, c(1, 2), my_mean)


    writeRaster(raster_create(txx, lat,lon),
                file.path("E:\\",
                          paste0("txx","_",unique_ym[i],"_",model,"_",scenario,".tif")),overwrite=TRUE)
    writeRaster(raster_create(tnx, lat,lon),
                file.path("E:\\",
                          paste0("tnx","_",unique_ym[i],"_",model,"_",scenario,".tif")),overwrite=TRUE)
    writeRaster(raster_create(rx5day, lat,lon),
                file.path("E:\\",
                          paste0("rx5day","_",unique_ym[i],"_",model,"_",scenario,".tif")),overwrite=TRUE)
    writeRaster(raster_create(Prctot, lat,lon),
                file.path("E:\\",
                          paste0("Prctot","_",unique_ym[i],"_",model,"_",scenario,".tif")),overwrite=TRUE)
    writeRaster(raster_create(temp, lat,lon),
                file.path("E:\\",
                          paste0("temp","_",unique_ym[i],"_",model,"_",scenario,".tif")),overwrite=TRUE)

  }
}

library(SPEI)
library(tidyverse)
library(terra)

setwd("C:/Projetos/17_index_insurance/indices")
list.files()

sel1 <- list.files(pattern = "Prctot")[grepl("ACCESS-CM2",list.files(pattern = "Prctot"))]
sel2 <- sel1[grepl("ssp245",sel1)]

prec_data <- rast(c(list.files(pattern = "Prctot_1"),
       list.files(pattern = "Prctot_2"),
       sel2))

models <- c("ACCESS-CM2","HadGEM3-GC31-LL","UKESM1-0-LL")
scenarios <- c("ssp245","ssp585")

prec_data <- rast(list.files(pattern = "Prctot"))

length(names(prec_data))/12

my_fun <- function(x){
  res <- spi(ts(x,start = c(1961,01,01), end = c(2024,03,01), freq = 12),
             scale = 3,
             start  = c(1981,01), ref.end = c(2010,12),na.rm=T)
  return(res$fitted)
}

my_fun2 <- function(x){
  x <- as.numeric(x)
  res <- spi(ts(x,start = c(1961,01), end = c(2024,03), freq = 12),
             scale = 6,
             start  = c(1981,01), ref.end = c(2010,12),na.rm=T)
  return(res$fitted)
}



spi_raster  <- terra::app(prec_data,my_fun)
spi_raster6 <- terra::app(prec_data,my_fun2)

writeRaster(spi_raster ,"spi_3.tif")
writeRaster(spi_raster6,"spi_6.tif")

#### Read Data ####
library(tidyverse)
library(terra)

dates <- seq.Date(from = as.Date("1961-01-01"),
                  to   = as.Date("2024-03-01"),
                  by = 'month')

setwd("C:/Projetos/17_index_insurance/indices")

lapply(list.files(pattern = "Prctot"),rast) -> prec_data
prec_data <- rast(prec_data)

lapply(list.files(pattern = "rx5day"),rast) -> rx5day
rx5day <- rast(rx5day)

lapply(list.files(pattern = "tnx"),rast) -> tnx
tnx <- rast(tnx)

lapply(list.files(pattern = "txx"),rast) -> txx
rx5day <- rast(txx)

lapply(list.files(pattern = "temp"),rast) -> temp
temp <- rast(temp)
spei_3 <- rast("spi_3.tif")
spei_6 <- rast("spi_6.tif")

setwd("E:\\")

lapply(list.files(pattern = "Prctot"),rast) -> prec_data
prec_data <- rast(prec_data)

library(zyp)
library(Kendall)
library(trend)
library(hydroTSM)


dates <- as.Date(paste(unlist(lapply(str_split(list.files(pattern = "Prctot"),"_"),function(x){x[2]})),"01",sep='-'))


# Define reclassification matrix: columns are (from, to, becomes)
reclass_matrix <- matrix(c(
  -Inf, -2.0, 1,
  -2.0, -1.5, 2,
  -1.5, -1.0, 3,
  -1.0,  1.0, 4,
  1.0,  1.5, 5,
  1.5,  2.0, 6,
  2.0,  Inf, 7
), ncol = 3, byrow = TRUE)

# Subset year 2019 from SPEI stack
spei_2019 <- spei_3[[year(dates) == 2019]]
names(spei_2019) <- month.abb

# Reclassify
spei_recl_2019 <- classify(spei_2019, rcl = reclass_matrix)

# Define colors
spi_colors <- c("red", "orange", "yellow", "white", "#C8A2C8", "plum", "purple")

# Define labels
spi_labels <- c("Extremely dry", "Very dry", "Moderately dry", "Near normal",
                "Moderately wet", "Very wet", "Extremely wet")


rasterVis::levelplot(spei_recl_2019,
          col.regions = spi_colors,
          at = seq(0.5, 7.5, 1),
          main = "SPEI-3 Classificação Para 2019",
          colorkey = list(
            at = 1:7,
            labels = list(labels = spi_labels, at = 1:7)
          ),
          margin = FALSE)



sttdf <- as.data.frame(spei_3,xy=T)

colnames(sttdf) <- c("x","y",
                     unlist(lapply(str_split(list.files(pattern = "Prctot"),"_"),function(x){x[2]})))

sttdf %>%
  pivot_longer(!c(x,y),names_to="date",values_to="values") -> sttdf

sttdf %>% na.omit() -> sttdf

sttdf6 <- as.data.frame(spei_6,xy=T)
names(sttdf6) <- c("x","y",as.character(dates))
sttdf6 %>%
  pivot_longer(!c(x,y),names_to="date",values_to="values") -> sttdf6

sttdf %>%
  mutate(values = case_when(
    values <= -2.0                     ~ "Extremamente seco",
    values > -2.0 & values <= -1.5     ~ "Severamente seco",
    values > -1.5 & values <= -1.0     ~ "Moderadamente seco",
    values > -1.0 & values < 1.0       ~ "Normal",
    values >= 1.0 & values < 1.5       ~ "Normal",
    values >= 1.5 & values < 2.0       ~ "Normal",
    values >= 2.0                      ~ "Normal",
    TRUE                               ~ NA_character_
  )) %>%
  mutate(values = factor(values,
                         levels = c("Extremamente seco",
                                    "Severamente seco",
                                    "Moderadamente seco",
                                    "Normal"))) %>%
  group_by(date) %>%
  mutate(total = n()) %>%
  group_by(date,values) %>%
  summarise(n = n()/max(total)) -> drought_monitor


sttdf6  %>%
  pivot_longer(!c(x,y),names_to="date",values_to="values") %>%
  mutate(values = case_when(
    values <= -2.0                     ~ "Extremamente seco",
    values > -2.0 & values <= -1.5     ~ "Severamente seco",
    values > -1.5 & values <= -1.0     ~ "Moderadamente seco",
    values > -1.0 & values < 1.0       ~ "Normal",
    values >= 1.0 & values < 1.5       ~ "Normal",
    values >= 1.5 & values < 2.0       ~ "Normal",
    values >= 2.0                      ~ "Normal",
    TRUE                               ~ NA_character_
  )) %>%
  mutate(values = factor(values,
                         levels = c("Extremamente seco",
                                    "Severamente seco",
                                    "Moderadamente seco",
                                    "Normal"))) %>%
  group_by(date) %>%
  mutate(total = n()) %>%
  group_by(date,values) %>%
  summarise(n = n()/max(total)) -> drought_monitor6

drought_monitor %>%
  mutate(date = as.Date(paste(date,"01",sep='-'))) -> drought_monitor

drought_monitor6 %>%
  mutate(date = as.Date(paste(date,"01",sep='-'))) -> drought_monitor6


drought_monitor %>%
  mutate(decade = case_when(
    year(date) >= 1961 & year(date) <= 1980 ~ "1961-1980",
    year(date) >= 1981 & year(date) <= 2000 ~ "1981-2000",
    year(date) >= 2001 & year(date) <= 2024 ~ "2001-2024"
  )) %>%
  filter(values %in% c("Extremamente seco", "Severamente seco", "Moderadamente seco")) %>%
  group_by(date) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = n, fill = values)) +
  geom_area() +
  scale_fill_manual(
    values = c(
      "Extremamente seco" = "#d73027",
      "Severamente seco" = "#fc8d59",
      "Moderadamente seco" = "#fee08b"
    ),
    name = "Categoria"
  ) +
  labs(
    title = "Monitoramento da Seca com Classificação SPI-3 meses",
    x = "Data",
    y = "Porcentagem de Ocorrências (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~decade, ncol = 1, scales = "free_x")

ggsave("C:\\Projetos\\17_index_insurance\\spi_3_monitor.png",height = 5,width = 8)


drought_monitor6 %>%
  mutate(decade = case_when(
    year(date) >= 1961 & year(date) <= 1980 ~ "1961-1980",
    year(date) >= 1981 & year(date) <= 2000 ~ "1981-2000",
    year(date) >= 2001 & year(date) <= 2024 ~ "2001-2024"
  )) %>%
  filter(values %in% c("Extremamente seco", "Severamente seco", "Moderadamente seco")) %>%
  group_by(date) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = n, fill = values)) +
  geom_area() +
  scale_fill_manual(
    values = c(
      "Extremamente seco" = "#d73027",
      "Severamente seco" = "#fc8d59",
      "Moderadamente seco" = "#fee08b"
    ),
    name = "Categoria"
  ) +
  labs(
    title = "Monitoramento da Seca com Classificação SPI-6 meses",
    x = "Data",
    y = "Porcentagem de Ocorrências (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~decade, ncol = 1, scales = "free_x")

ggsave("C:\\Projetos\\17_index_insurance\\spi_6_monitor.png",height = 5,width = 8)

my_prob <- function(x){
  x <- na.omit(x)
  p_empirical <- mean(x < -1.5)
  return(p_empirical)
}

prob_d1 <- app(spei_3[[year(dates)>=1961 & year(dates) <= 1980]],my_prob)
prob_d2 <- app(spei_3[[year(dates)>=1981 & year(dates) <= 2010]],my_prob)
prob_d3 <- app(spei_3[[year(dates)>=1991 & year(dates) <= 2024]],my_prob)

dd <- c(prob_d1,prob_d2,prob_d3)
dd6 <- c(app(spei_6[[year(dates)>=1961 & year(dates) <= 1980]],my_prob),
         app(spei_6[[year(dates)>=1981 & year(dates) <= 2010]],my_prob),
         app(spei_6[[year(dates)>=1991 & year(dates) <= 2024]],my_prob))
names(dd) <- c("dec_1961_1980", "dec_1981_2010", "dec_1991_2024")
names(dd6) <- c("dec_1961_1980", "dec_1981_2010", "dec_1991_2024")

library(rasterVis)
library(imputeTS)
my_palette <- colorRampPalette(c("#FFFFFF", "#FFFFCC", "#FF6600", "#CC0000", "black"))

rasterVis::levelplot(dd,
                     names.attr = c("1961–1980", "1981–2010", "1991–2024"),
                     main = "Probabilidade de seca Severa",
                     col.regions = my_palette(100),
                     margin = FALSE)

rasterVis::levelplot(dd6,
                     names.attr = c("1961–1980", "1981–2010", "1991–2024"),
                     main = "Probabilidade de seca Severa",
                     col.regions = my_palette(100),
                     margin = FALSE)

my_s_trend <- function(x) {
  ts_data <- ts(as.numeric(x), start = c(1961,3), frequency= 12)
  test <- trend::smk.test(ts_data)

  list(
    p.value = test$p.value,
    trend = ifelse(test$statistic < -1.96,"Decreasing",
                   ifelse(test$statistic > 1.96,"Increasing", "No Trend"))
  )
}

library(drought)


my_RunDS <- function(DI, thre){
  if(min(DI) > thre){
    return(NA)
  }
  else{
    DI = as.numeric(DI)
    DI = as.matrix(DI,ncol=1)
    return(RunDS(DI,thre))
  }
}



sttdf %>%
  mutate(date = as.Date(paste(date,"01",sep='-'))) %>%
  mutate(decade = case_when(
    year(date) >= 1961 & year(date) <= 1990 ~ "1961-1990",
    year(date) >= 1981 & year(date) <= 2010 ~ "1981-2010",
    year(date) >= 1991 & year(date) <= 2024 ~ "2001-2024"
  )) -> sttdf

sttdf %>%
  group_by(x,y,decade) %>%
  nest() %>%
  mutate(drought = map(data,~my_RunDS(.x$values,-1.5))) -> spi_3_droughts

spi_3_droughts %>%
  unnest(drought) -> spi_3_droughts


spi_3_droughts %>%
  group_by(Duration, decade) %>%
  summarise(Severity = median(Severity, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Duration, y = Severity, color = decade)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6, size = 2) +
  geom_smooth(method = 'gam', se = FALSE, size = 1.2) +
  scale_color_manual(values = c("#fe9929", "#d95f0e", "#993404")) +
  labs(
    x = "Duração da Seca (meses)",
    y = "Severidade",
    color = "Década"
  ) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

spi_3_droughts %>%
  ggplot(aes(Severity,col=decade))+
  geom_density()+
  scale_x_continuous(trans='log2')


sttdf  %>%
  mutate(date = as.Date(paste(date,"01",sep='-'))) %>%
  mutate(decade = case_when(
    year(date) >= 1961 & year(date) <= 1980 ~ "1961-1980",
    year(date) >= 1981 & year(date) <= 2000 ~ "1981-2000",
    year(date) >= 2001 & year(date) <= 2024 ~ "2001-2024"
  )) %>%
  mutate(values = case_when(
  values <= -2.0                     ~ "Extremamente seco",
  values > -2.0 & values <= -1.5     ~ "Severamente seco",
  values > -1.5 & values <= -1.0     ~ "Moderadamente seco",
  values > -1.0                      ~ "Normal")) %>%
  group_by(values,year = year(date)) %>%
  summarise(
    freq = n(),
    min = min(values)
  ) -> historic_spi_3



as.data.frame(spei_3[[10]],cell=T,xy=T) -> cells
results <- merge(cells,results,by=c("x","y"))

spei_3_trend <- spei_3[[10]]
spei_3_trend[results$cell] <- ifelse(results$trend=="Increasing",1,
                                     ifelse(results$trend=="Decreasing",0,
                                            ifelse(results$trend=="No Trend",3,NA)))
plot(spei_3_trend)

spei_3_trend <- app(spei_3, fun = trend_alert)

summary(spei_3_trend)

spei_6_trend <- app(spei_6, fun = analyze_trend_monthly)

tnx_trend <- app(tnx, fun = analyze_trend_fast,max)
tnx_trend2 <- app(tnx, fun = analyze_trend_monthly)

plot(values(tnx_trend2),
     values(tnx_trend))

cor(data.frame(values(tnx_trend2),
               values(tnx_trend)) %>% na.omit())

plot(tnx_trend)
plot(tnx_trend2)

plot(spei_3_trend)
plot(tnx_trend)

d <- zyp.trend.vector(runif(100),
                      conf.intervals=FALSE)

setwd("C:\\Projetos\\17_index_insurance\\indices")

library(geobr)
library(terra)
library(tidyverse)
library(sf)

cities <- read_municipality()
cities <- st_transform(cities,"EPSG:4326")
v_cities <- vect(cities)
cities_dataset <- st_drop_geometry(cities)[,c("code_muni","name_muni")]


spei_3 <- rast("spi_3.tif")
spei_6 <- rast("spi_6.tif")

dates <- seq.Date(from = as.Date("1961-01-01"),
                  to   = as.Date("2024-03-01"),
                  by = 'month')

names(spei_3) <- dates
names(spei_6) <- dates


data.frame(files = list.files(),
           years = unlist(lapply(list.files(),function(x){str_split(x,"_")[[1]][2]})) ) %>%
  mutate(years = as.Date(paste(years,"01",sep='-'))) %>%
  subset(years >= "1988-01-01") -> files

st_drop_geometry(cities)[,c("code_muni","name_muni")] -> dataset
v_cities <- vect(cities)

r <- rast(files$files)
names(r) <- files$files

extracted_values <- terra::extract(r, v_cities, fun = mean, na.rm = TRUE)
extracted_spei_3 <- terra::extract(spei_3, v_cities, fun = mean, na.rm = TRUE)
extracted_spei_6 <- terra::extract(spei_6, v_cities, fun = mean, na.rm = TRUE)


dataset <- cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_values)


write.csv(dataset,"C:\\Projetos\\17_index_insurance\\climate_indices.csv",row.names=F)

write.csv(cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_spei_3),"C:\\Projetos\\17_index_insurance\\spi_3_indices.csv",row.names=F)
write.csv(cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_spei_6),"C:\\Projetos\\17_index_insurance\\spi_6_indices.csv",row.names=F)


#### Scenarios ####
library(geobr)
library(terra)
library(tidyverse)
library(sf)
library(exactextractr)


cities <- read_municipality()
cities <- st_transform(cities,"EPSG:4326")

setwd("E:\\")

files <- list.files(pattern = ".tif")

index <- rep(NA,length(files))
for(i in seq_along(files)){
  if(grepl("spi",files[i])){
    parts <- str_split(files[i],'_',simplify = T)
    index[i] <- paste(parts[1],parts[2],sep='_')}
  else{
    index[i] <- str_extract(files[i], "^[^_]+")}
}


scenario  <- str_extract(files, "ssp\\d+")
model <- str_split(files,"_",simplify = T)[,3] %>% substr(1,nchar(.)-4)
model <- ifelse(model=="UKESM1-","UKESM1-0-LL",model)
model <- ifelse(model=="ACCESS","ACCESS-CM2",model)
model <- ifelse(model=="HadGEM3-GC3","HadGEM3-GC31-LL",model)
scenario <- ifelse(is.na(scenario),"historic",scenario)

comparison_dataset_corrected <- read.csv("C:\\Projetos\\17_index_insurance\\soybean_dataset.csv")

cities <- cities %>% subset(code_muni %in% unique(comparison_dataset_corrected$CD_MUN))
v_cities <- vect(cities)

M_models <- unique(model)
S_scenario <- unique(scenario)
I_index <- unique(index)
I_index <- I_index[c(3,4)]

for(m in seq_along(M_models)){
  for(s in seq_along(S_scenario)){
    for(i in seq_along(I_index)){
      file <- files[model == M_models[m] & index == I_index[i] & scenario == S_scenario[s]]
      cat(M_models[m],I_index[i],S_scenario[s],"\n")
      r <- raster::stack(file)
      extracted_values <- exact_extract(r, cities,fun =  'mean')
      n <- length(colnames(extracted_values))

      colnames(extracted_values) <- paste(I_index[i],paste(rep(1980:(1980+n/12-1),each = 12),rep(1:12,n/12),sep='-'),sep="_")
      dataset <- cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_values)
      write.csv(dataset,paste0(M_models[m],"_",I_index[i],"_",S_scenario[s],".csv"),row.names = F)
    }
  }
}


lapply(list.files(pattern = ".tif"), function(file){
  cat(file,"\n")
  r <- raster::raster(file)
  extracted_values <- exact_extract(r, cities, fun =  'mean')
  colnames(extracted_values) <- c("ID",substr(file,1,nchar(file)-4))
  dataset <- cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_values)
  write.csv(dataset,paste0(substr(file,1,nchar(file)-4),".csv"),row.names = F)

})

library(SPEI)

my_fun <- function(x){
  end_year <- 1980+length(x)/12
  res <- spi(ts(x,start = c(1980,01), end = c(end_year,12), freq = 12),
             scale = 3,
             start  = c(1981,01), ref.end = c(2013,12),na.rm=T)
  return(res$fitted)
}

my_fun2 <- function(x){
  x <- as.numeric(x)
  end_year <- 1980+length(x)/12
  res <- spi(ts(x,start = c(1980,01), end = c(end_year,12), freq = 12),
             scale = 6,
             start  = c(1980,01), ref.end = c(2013,12),na.rm=T)
  return(res$fitted)
}


setwd("E:\\")

dates <- seq.Date(from = as.Date("1980-01-01"),
                  to   = as.Date("2099-12-01"),
                  by = 'month')

prec_files <- list.files(pattern = "Prctot")
prec_files <- prec_files[grepl(".tif",prec_files)]


scenario  <- str_extract(prec_files, "ssp\\d+")                       # ssp245, ssp585, etc.
model     <- str_extract(prec_files, "(?<=_)[^_]+(?=_ssp\\d+)")       # between last "_" and "_ssp..."
model <- ifelse(
  is.na(scenario),
  str_extract(prec_files, "(?<=_[0-9]{4}-[0-9]{2}_).+(?=\\.tif)"),
  str_extract(prec_files, "(?<=_[0-9]{4}-[0-9]{2}_).+(?=_ssp\\d+)")
)
scenario <- ifelse(is.na(scenario),"historic",scenario)

comparison_dataset_corrected <- read.csv("C:\\Projetos\\17_index_insurance\\soybean_dataset.csv")

cities <- cities %>% subset(code_muni %in% unique(comparison_dataset_corrected$CD_MUN))
v_cities <- vect(cities)

M_models <- unique(model)
S_scenario <- unique(scenario)
I_index <- unique(index)

for(m in seq_along(M_models)){
  for(s in seq_along(S_scenario)){
    for(i in seq_along(I_index)){
      file <- prec_files[model == M_models[m] & scenario == "historic"] %>% na.omit()
      cat(M_models[m],S_scenario[s],"\n")
      r <- rast(file)
      spi_raster  <- terra::app(r,my_fun)
      spi_raster6 <- terra::app(r,my_fun2)

      writeRaster(spi_raster,paste0("spi_3_",M_models[m],"_",S_scenario[s],".tif"),overwrite=T)
      writeRaster(spi_raster6,paste0("spi_6_",M_models[m],"_",S_scenario[s],".tif"),overwrite=T)

    }
  }
}

files_spi <- list.files(pattern = "spi")
dates
for(spi in seq_along(files_spi)){
      cat(files_spi[spi],"\n")
      r <- raster::stack(files_spi[spi])
      names(r) <- dates
      extracted_values <- exact_extract(r, cities,fun =  'mean')
      colnames(extracted_values) <- c(substr(file,1,nchar(file)-4))
      dataset <- cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_values)
      write.csv(dataset,paste0(M_models[m],"_",substr(files_spi[spi],1,5),"_",S_scenario[s],".csv"),row.names = F)
}

