library(terra)
library(tidyverse)
library(rasterVis)
library(RColorBrewer)
library(gridExtra)
library(raster)
library(geobr)
library(trend)
library(latticeExtra)
library(sf)

biomes <- read_biomes()

x <- seq(1.01,1.1,0.001)
y <- 1/log(x)
y2 <- 1/log(x+0.01)-10
# Plot the base curve
# Plot the base curve without axis labels or ticks
plot(x, y, type = 'l', ylim = c(min(y2), max(y)),
     xaxt = 'n', yaxt = 'n',  # remove axis ticks
     xlab = "Exceedance Probability", ylab = "Damages",    # remove axis labels
     col = "black", lty = 1, lwd = 2)

# Fill the area between curves
polygon(c(x, rev(x)), c(y, rev(y2)),
        col = rgb(0.1, 0.4, 0.9, 0.3), border = NA)

# Add dashed line for risk reduction
lines(x, y2, col = "red", lty = 2, lwd = 2)

# Add custom legend with smaller text and less spacing
legend("topleft",
       legend = c("Original Loss Frequency Curve - Without Risk Reduction",
                  "Loss Frequency Curve - With Risk Reduction",
                  "Benefits of Risk Reduction"),
       col = c("black", "red", rgb(0.1, 0.4, 0.9, 0.3)),
       lty = c(1, 2, NA),
       lwd = c(2, 2, NA),
       pch = c(NA, NA, 15),
       pt.cex = 2,
       bty = "n",
       cex = 0.8,           # smaller font
       x.intersp = 0.8,     # reduce horizontal spacing
       y.intersp = 0.8      # reduce vertical spacing
)


setwd("E:\\")

dates <- seq.Date(from = as.Date("1980-01-01"),
                  to   = as.Date("2099-12-01"),
                  by = 'month')


dates <- as.Date(dates)  # ensure dates are Date class
decade <- ifelse(dates <= as.Date("2013-12-01"), "Historic",
                 ifelse(dates <= as.Date("2039-12-01") & dates >= as.Date("2014-01-01"), "Short-Term",
                        ifelse(dates <= as.Date("2069-12-01") & dates >= as.Date("2040-01-01"), "Mid-Term", "Long-Term")))
decade <- factor(decade, levels = c("Historic", "Short-Term", "Mid-Term", "Long-Term"))

# Indices per group (once only)
idx_hist <- which(decade == "Historic")
idx_short <- which(decade == "Short-Term")
idx_mid <- which(decade == "Mid-Term")
idx_long <- which(decade == "Long-Term")


prob_ratio <- function(x, thres = 1.5, drought = T, term = "Long-Term") {
  idx_term <- switch(term,
                     "Short-Term" = idx_short,
                     "Mid-Term" = idx_mid,
                     "Long-Term" = idx_long,
                     "Historic" = idx_hist)

  # Compute probabilities
  hist_vals <- x[idx_hist] %>% na.omit()
  term_vals <- x[idx_term] %>% na.omit()


  if(drought){
    prob_0 <- mean(hist_vals <= -1.5)
    prob_1 <- mean(term_vals <= -1.5)
  }
  else{
    prob_0 <- mean(hist_vals >= 1.5)
    prob_1 <- mean(term_vals >= 1.5)
  }

  prob_1 / prob_0
}



calc_prob <- function(spi){
  c(app(spi,prob_ratio,drought=T,term = "Short-Term"),
    app(spi,prob_ratio,drought=T,term = "Mid-Term"),
    app(spi,prob_ratio,drought=T,term = "Long-Term")) -> spi_prob_res
  names(spi_prob_res) <- c("Short-Term", "Mid-Term", "Long-Term")
  spi_prob_res <- raster::stack(spi_prob_res)

  c(app(spi,prob_ratio,drought=F,term = "Short-Term"),
    app(spi,prob_ratio,drought=F,term = "Mid-Term"),
    app(spi,prob_ratio,drought=F,term = "Long-Term")) -> flood_prob_res
  names(flood_prob_res) <- c("Short-Term", "Mid-Term", "Long-Term")
  flood_prob_res <- raster::stack(flood_prob_res)

  return(list(drought = spi_prob_res,flood = flood_prob_res))
}

lapply(list.files(pattern = "spi")[1:12],function(file){
  spi <- rast(file)
  calc_prob(spi)
}) -> spi_probs


setwd("C:\\Projetos\\17_index_insurance\\indices")
files <- list.files(pattern = ".tif")
index <- str_extract(files, "^[^_]+")
I_index <- "spi"

spi <- rast("spi_6.tif")

dates <- seq.Date(from = as.Date("1961-01-01"),
                  to   = as.Date("2024-03-01"),
                  by = 'month')

names(spi) <- dates

decade <- ifelse(dates <= as.Date("1980-12-01"), "1960-1980",
                 ifelse(dates <= as.Date("2009-12-01") & dates >= as.Date("1980-01-01"), "1980-2010",
                        ifelse(dates <= as.Date("2024-12-01") & dates >= as.Date("1990-01-01"), "1990-2020", "Other")))
decade <- factor(decade, levels = c("1960-1980", "1980-2010", "1990-2020"))

# Indices per group (once only)
idx_1960 <- which(decade == "1960-1980")
idx_1980 <- which(decade == "1980-2010")
idx_1990 <- which(decade == "1990-2020")

prob_ratio_historic <- function(x, thres = 1.5, drought = T, term = "1990-2020") {
  idx_term <- switch(term,
                     "1960-1980" = idx_1960,
                     "1980-2010" = idx_1980,
                     "1990-2020" = idx_1990)

  # Compute probabilities
  hist_vals <- x[idx_1960] %>% na.omit()
  term_vals <- x[idx_term] %>% na.omit()


  if(drought){
    prob_0 <- pnorm(-1.5,mean(hist_vals),sd(hist_vals))
    prob_1 <- pnorm(-1.5,mean(term_vals),sd(term_vals))
  }
  else{
    prob_0 <- pnorm(1.5,mean(hist_vals),sd(hist_vals),lower.tail=F)
    prob_1 <- pnorm(1.5,mean(term_vals),sd(term_vals),lower.tail=F)
  }

  prob_1 / prob_0
}




drought_stack <- stack(raster::stackApply(stack(c(spi_probs[[1]]$drought,
                                 spi_probs[[3]]$drought,
                                 spi_probs[[5]]$drought)),
                         indices=1:3,
                         fun=mean),
      raster::stackApply(stack(c(spi_probs[[2]]$drought,
                                 spi_probs[[4]]$drought,
                                 spi_probs[[6]]$drought)),
                         indices=1:3,
                         fun=mean))

names(drought_stack) <- c("Short-Term SSP2-4.5","Mid-Term SSP2-4.5","Long-Term SSP2-4.5",
                          "Short-Term SSP5-8.5","Mid-Term SSP5-8.5","Long-Term SSP5-8.5"
                          )
# Define o máximo da escala, limitando a 5
max_val <- min(5, max(values(drought_stack), na.rm = TRUE))

# Define a escala com intervalo + Inf para >5
at <- c(-Inf,seq(0.5,0.9,length.out = 5),seq(1, 5, length.out = 10), Inf)
neutral_index <- which.min(abs(at - 1))


# Paletas azul (valores < 1), vermelha (valores > 1)
pal_blue <- colorRampPalette(c("#08306B", "#DEEBF7"))(neutral_index - 2)
pal_red  <- colorRampPalette(c("#FEE0D2", "#99000D"))(length(at) - neutral_index - 1)

# Cor adicional para valores >5
pal_extra <- "darkred"  # vermelho escuro
pal_custom1 <- c(pal_blue, "gray", pal_red, "darkred")

biomes <- biomes %>% na.omit()
biomes <- st_transform(biomes, crs(raster::crs(drought_stack)))
biomes_sp <- as(biomes, "Spatial")
centroids <- st_centroid(biomes)
centroids_sp <- as(centroids, "Spatial")
coords <- coordinates(centroids_sp)
uf <- biomes$code_biome %>% na.omit()

levelplot(
  stack(drought_stack),
  margin = FALSE,
  #main = paste0("3 month-SPI-based Probability Ratio of Drought","\nModel ensemble "),
  col.regions = pal_custom1,
  at = at,
  names.attr = c("Short-Term SSP2-4.5","Mid-Term SSP2-4.5","Long-Term SSP2-4.5",
                 "Short-Term SSP5-8.5","Mid-Term SSP5-8.5","Long-Term SSP5-8.5"
  ),
  par.settings = list(strip.background = list(col = "lightgrey")),
  frame=T,
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),
  colorkey = list(
    space = "right",
    at = at,
    labels = list(
      at = c(0.5,0.75,1,2.5,3.75,5),
      labels = c("<0.5","0.75",
                 "1", "2.5", "3.75", ">5")
    )
  )
)+
  layer(sp.polygons(biomes_sp, col = "black", lwd = 1.2))+
  layer(sp.text(coords, txt = uf, col= 'black',cex = 0.7, font = 2))


flood_stack <- stack(raster::stackApply(stack(c(spi_probs[[1]]$flood,
                                                spi_probs[[3]]$flood,
                                                spi_probs[[5]]$flood)),
                                        indices=1:3,
                                        fun=mean),
                     raster::stackApply(stack(c(spi_probs[[2]]$flood,
                                                spi_probs[[4]]$flood,
                                                spi_probs[[6]]$flood)),
                                        indices=1:3,
                                        fun=mean))

names(flood_stack) <- c("Short-Term SSP245","Mid-Term SSP245","Long-Term SSP245",
                          "Short-Term SSP585","Mid-Term SSP585","Long-Term SSP585"
)

at <- c(-Inf,seq(0.5,0.9,length.out = 10),seq(1, 3, length.out = 10), Inf)
colmax = "#003c30"
# Paletas azul (valores < 1), vermelha (valores > 1)
pal_blue <- colorRampPalette(c("#8c510a","#f6e8c3"))(neutral_index - 2)
pal_red  <- colorRampPalette(c("#80cdc1","#01665e"))(length(at) - neutral_index - 1)

# Cor adicional para valores >5
pal_extra <- colmax  # vermelho escuro
pal_custom <- c(pal_blue, "gray", pal_red, pal_extra)
names(flood_stack) <- c("Short-term SSP2-4.5","Mid-term SSP2-4.5","Long-term SSP2-4.5",
                        "Short-term SSP5-8.5","Mid-term SSP5-8.5","Long-term SSP5-8.5")

labels <- as.character(seq(0.5,3,by=0.5))
labels[1] <- "<0.5"
labels[length(labels)] <- ">3"
levelplot(
  stack(flood_stack),
  margin = FALSE,
  #main = paste0("3 month-SPI-based Probability Ratio of Excessive Rainfall","\nModel ensemble "),
  col.regions = pal_custom,
  at = at,
  names.attr = c("Short-term SSP2-4.5","Mid-term SSP2-4.5","Long-term SSP2-4.5",
                 "Short-term SSP5-8.5","Mid-term SSP5-8.5","Long-term SSP5-8.5"),
  par.settings = list(strip.background = list(col = "lightgrey")),
  frame=T,
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),
  colorkey = list(
    space = "right",
    at = at,
    labels = list(
      at = seq(0.5,3,by=0.5),
      labels = labels
    )
  )
)+
  layer(sp.polygons(biomes_sp, col = "black", lwd = 1.2))+
  layer(sp.text(coords, txt = uf, col= 'black',cex = 0.7, font = 2))

spi_resample <- terra::resample(spi,rast(spi_probs[[1]][[1]][[1]]),
                         method="average")

drought_historic <- stack(c(app(spi_resample,prob_ratio_historic,drought=T,term = "1980-2010"),
                            app(spi_resample,prob_ratio_historic,drought=T,term = "1990-2020")))
names(drought_historic) <- c("Historic 1980-2010","Historic 1990-2020")

flood_historic <- stack(c(app(spi_resample,prob_ratio_historic,drought=F,term = "1980-2010"),
                            app(spi_resample,prob_ratio_historic,drought=F,term = "1990-2020")))
names(flood_historic) <- c("Historic 1980-2010","Historic 1990-2020")

levelplot(
  drought_historic,
  margin = FALSE,
  main = paste0("3 month-SPI-based Probability Ratio of Drought","\nObserved Data"),
  col.regions = pal_custom1,
  at = at,
  names.attr = names(drought_historic),
  colorkey = list(
    at = at,
    labels = list(
      at = c(0.5,1,1.25,2.5,3.75,5),
      labels = c("<0.5",
                 "1", "1.25", "2.5", "3.75", ">5")
    )
  )
)


levelplot(
  flood_historic,
  margin = FALSE,
  main = paste0("3 month-SPI-based Probability Ratio of Excessive Rainfall","\nObserved Data"),
  col.regions = pal_custom,
  at = at,
  names.attr = names(flood_historic),
  colorkey = list(
    at = at,
    labels = list(
      at = c(0.5,1,1.25,2.5,3.75,5),
      labels = c("<0.5",
                 "1", "1.25", "2.5", "3.75", ">5")
    )
  )
)


trend_test <- function(x){

}

res <- smk.test(nottem)
## print method
res



trend_strength <- function(x) {

  if (all_na(x)) return(NA)
  my_ts <- ts(x,c(1980,01,01),frequency = 12)
  mk <- smk.test(my_ts)
  mk$p.value
}


slope_fun <- function(x){
  if (all_na(x)) return(NA)
  my_ts <- ts(x,c(1980,01,01),frequency = 12)
  sea.sens.slope(my_ts)
}

setwd("E:\\")

files <- list.files(pattern = ".tif")

index     <- str_extract(files, "^[^_]+")                        # before first "_"
scenario  <- str_extract(files, "ssp\\d+")                       # ssp245, ssp585, etc.
model     <- str_extract(files, "(?<=_)[^_]+(?=_ssp\\d+)")
model <- ifelse(
  is.na(scenario),
  str_extract(files, "(?<=_[0-9]{4}-[0-9]{2}_).+(?=\\.tif)"),
  str_extract(files, "(?<=_[0-9]{4}-[0-9]{2}_).+(?=_ssp\\d+)")
)
scenario <- ifelse(is.na(scenario),"historic",scenario)

M_models <- na.omit(unique(model))
S_scenario <- unique(scenario)
I_index <- unique(index)[c(1,2,4,5,6)]

indices <- c()
idx <- 1
for(m in seq_along(M_models)){
  for(s in seq_along(S_scenario)){
    for(i in seq_along(I_index)){
      file <- files[model == M_models[m] & index == I_index[i] & scenario == S_scenario[s]]
      cat(M_models[m],I_index[i],S_scenario[s],"\n")
      r <- rast(file)
      r1 <- app(r,trend_strength)
      r2 <- app(r,slope_fun)
      indices[[idx]] <- list(r1,r2)
      idx <- idx+1
    }
  }
}


indices_names <- c()
idx <- 1
for(m in seq_along(M_models)){
  for(s in seq_along(S_scenario)){
    for(i in seq_along(I_index)){
      cat(M_models[m],I_index[i],S_scenario[s],"\n")
      indices_names[idx] <- paste(M_models[m],I_index[i],S_scenario[s],sep='_')
      idx <- idx+1
    }
  }
}

plot(indices[[1]][[1]])

idx <- 1
indices_trend <- c()
for(s in seq_along(S_scenario)){
  for(i in seq_along(I_index)){
    cat(M_models[m],I_index[i],S_scenario[s],"\n")
    streng <- lapply(indices[grepl(paste(I_index[i],S_scenario[s],sep='_'),indices_names)],function(x){x[1]}) %>% unlist() %>% rast() %>% app(mean)
    slope <- lapply(indices[grepl(paste(I_index[i],S_scenario[s],sep='_'),indices_names)],function(x){x[2]}) %>% unlist() %>% rast() %>% app(mean)
    r3 <- xapp(streng,slope,function(x,y){ifelse(x>0.05,0,y)})
    names(r3) <- paste(I_index[i],S_scenario[s],sep='_')
    indices_trend[[idx]] <- r3
    idx <- idx+1
  }
}



setwd("C:\\Projetos\\17_index_insurance\\indices")
files <- list.files(pattern = ".tif")
index <- str_extract(files, "^[^_]+")

dates <- seq.Date(from = as.Date("1961-01-01"),
                  to   = as.Date("2024-03-01"),
                  by = 'month')

indices_historic <- c()
I_index <- unique(index)[c(1,2,4,5,6)]
idx <- 1
for(i in seq_along(I_index)){
  file <- files[index == I_index[i]]
  cat(I_index[i],"\n")
  r <- rast(file[dates >= "1981-01-01" & dates <= "2009-12-01"])
  r <- resample(r,indices_trend[[1]],method="average")
  r1 <- app(r,trend_strength)
  r2 <- app(r,slope_fun)
  indices_historic[[idx]] <- list(r1,r2)
  idx <- idx+1
}

indices_historic <- lapply(indices_historic,function(x){xapp(x[[1]],x[[2]],function(x,y){ifelse(x>0.05,0,y)})})
indices_historic <- rast(indices_historic)
names(indices_historic) <- paste0(I_index,'_',"Observed")


names(rast(indices_trend))
names(rast(indices_historic))
prec_sel <- c(resample(c(indices_historic[[1]]),indices_trend[[1]]),
             rast(indices_trend[c(1,6,11)]),
             resample(indices_historic[[2]],indices_trend[[1]]),
             rast(indices_trend[c(2,7,12)]))
prec_sel <- prec_sel[[c(1,3,4,5,7,8)]]

names(rast(indices_trend))

stack_sel <- c(c(resample(indices_historic[[3]],indices_trend[[1]]),
                 rast(indices_trend[c(3,8,13)])),
               c(resample(indices_historic[[4]],indices_trend[[1]]),
                 rast(indices_trend[c(4,9,14)])),
               c(resample(indices_historic[[5]],indices_trend[[1]]),
                 rast(indices_trend[c(5,10,15)])))


labels_prec <- as.character(seq(-5,5,2.5))
labels_prec[1] <- "<5"
labels_prec[length(labels_prec)] <- ">5"
at_prec <- c(-Inf,seq(-5,5,1), Inf)

# Índice do valor zero
zero_index <- which.min(abs(seq(-5, 5, 1)))

# Paleta: vermelho para <0, branco no 0, azul para >0
pal_red <- colorRampPalette(c("#67001f", "#fddbc7"))(zero_index - 1)
pal_blue <- colorRampPalette(c("#d1e5f0", "#053061"))(length(seq(-5, 5, 1)) - zero_index)
pal_custom <- c(pal_red, "lightgrey", pal_blue)
pal_custom <- c(pal_custom,"#053061")


names(prec_sel) <- c("Prctot Observed","Prctot SSP2-4.5","Prctot SSP5-8.5",
                     "Rx5day Observed","Rx5day SSP2-4.5","Rx5day SSP5-8.5")

levelplot(
  stack(prec_sel)*10,
  margin = FALSE,
  layout = c(3, 2),
  #main = "Trend Analysis of Climate Extreme Indices mm/decade",
  at = at_prec,
  col.regions = pal_custom,
  names.attr = names(prec_sel),
  par.settings = list(strip.background = list(col = "lightgrey")),
  frame=T,
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),
  colorkey = list(
    space = "right",
    labels = list(
      at = seq(-5, 5, 2.5),
      labels = paste0(labels_prec, " mm")
    )
  )
)+
  layer(sp.polygons(biomes_sp, col = "black", lwd = 1.2))+
  layer(sp.text(coords, txt = uf, col= 'black',cex = 0.7, font = 2))


labels <- as.character(seq(-0.5, 1.5, 0.5))
labels[1] <- "<-0.5"
labels[length(labels)] <- ">1.5"
at <- c(-Inf,seq(-0.5, 1.5, 0.25), Inf)

# Índice do valor zero
zero_index <- which.min(abs(seq(-0.5, 1.5, 0.5)))

pal_red <- colorRampPalette(c("#053061","#d1e5f0" ))(zero_index - 1)
pal_blue <- colorRampPalette(c("#fddbc7", "#67001f"))(length(seq(-1, 1, 0.5)) - zero_index)
pal_custom <- c(pal_red, "lightgrey", pal_blue)
pal_custom <- c("#d1e5f0",pal_custom,"#67001f")

names(stack_sel) <- c("Temp Observed","Temp Historic","Temp SSP2-4.5","Temp SSP5-8.5",
                      "TNX Observed","TNX Historic","TNX SSP2-4.5","TNX SSP5-8.5",
                      "TXX Observed","TXX Historic","TXX SSP2-4.5","TXX SSP5-8.5"
                      )
colors <- c(colorRampPalette(c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'))(length(seq(-0.5, 1.5, 0.1)) - 1))
colors[1:3] <- c("#08519c","#2171b5","#c6dbef")

levelplot(
  stack(stack_sel)*10,
  margin = FALSE,
  layout = c(4, 3),  # 3 colunas, 3 linhas
  #main = "Trend Analysis of Climate Extreme Indices per decade",
  at = at,
  par.settings = list(strip.background = list(col = "lightgrey")),
  col.regions = colors,
  names.attr = names(stack_sel),
  scales = list(
    draw = TRUE,
    alternating = FALSE,
    tck = c(1, 0),
    cex = 0.6
  ),
  frame = TRUE,
  colorkey = list(
    space = "right",
    labels = list(
      at = seq(-0.5, 1.5, 0.5),
      labels = paste0(labels, " ºC")
    )
  )
)+
  layer(sp.polygons(biomes_sp, col = "black", lwd = 1.2))+
  layer(sp.text(coords, txt = uf, col= 'black',cex = 0.7, font = 2))



breaks <- c(0, 0.000001, 0.2, 0.4, 0.6, 0.8, 1.0)
labels <- c("no trend", "very weak", "weak", "moderate", "strong", "very strong")

r_cat <- classify(r1, rcl = cbind(breaks[-length(breaks)], breaks[-1], 1:6))
levels(r_cat) <- data.frame(ID=1:6, category=labels)
plot(r_cat)

plot(r2)

