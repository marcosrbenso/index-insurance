library(data.table)
library(tidyverse)

setwd("E:\\")

load(file="C:\\Projetos\\17_index_insurance\\models_phase2.RData")

loss <- function(x,period){
  pmax(0,mean(x[period=="historic"])-x)/mean(x[period=="historic"])
}

files <- "E:/HadGEM3-GC31-LL_Prctot_historic.csv"

create_dataset <- function(files){

  prec_list <- lapply(files, fread)

  prec_list <- lapply(prec_list, function(dt) {

  dt_long <- melt(dt, id.vars = c("code_muni", "name_muni"),
                  variable.name = "var", value.name = "values")

  dt_long[, c("variable", "date", "Model") := tstrsplit(var, "_", keep = 1:3)]

  dt_long[, c("year", "month") := tstrsplit(date, "-", fixed=TRUE)]
  dt_long[, year := as.integer(year)]
  dt_long[, month := as.integer(month)]

  # Filter and adjust year
  dt_long <- dt_long[month %in% c(1:4, 8:12)]
  dt_long[month %in% 8:12, year := year + 1]

  # Create var_name and reshape
  dt_long[, var_name := paste0(variable, "_", month)]
  dt_wide <- dcast(dt_long, code_muni + name_muni + year ~ var_name,
                   value.var = "values")
  return(dt_wide)
    }) -> dt_wide

  idx <- dt_wide[[1]][,1:3]
  dt_wide <- lapply(dt_wide,function(x){x[,-c(1:3)]})
  dt_wide <- do.call("cbind",dt_wide)
  dt_wide <- cbind(idx,dt_wide)

  soil_indices <- fread("C:\\Projetos\\17_index_insurance\\soil_indices.csv")
  soil_indices[, c("ID", "name_muni") := NULL]

  dataset2 <- merge(dt_wide, soil_indices, by = "code_muni", all.x = TRUE)

  return(dataset2)
}

files <- list(
  "E:/UKESM1-0-LL_Prctot_historic.csv",
  "E:/UKESM1-0-LL_temp_historic.csv"
)

acess <- create_dataset(files)
acess_ssp2 <- create_dataset(list("E:/UKESM1-0-LL_Prctot_ssp245.csv",
                                  "E:/UKESM1-0-LL_temp_ssp245.csv"))

hadgem_ssp2 <- create_dataset("E:/HadGEM3-GC31-LL_Prctot_ssp585.csv")
observed <- read.csv("C:\\Projetos\\17_index_insurance\\dataset.csv")


acess %>%
  na.omit() %>%
  mutate(yield = predict(models_phase2$model3,acess)) %>%
  rbind(acess_ssp2 %>%
          na.omit() %>%
          mutate(yield = predict(models_phase2$model3,acess_ssp2))) -> dt

dt[, period := fifelse(year >= 1980 & year <= 2013, "historic",
                       fifelse(year >= 2015 & year <= 2039, "short_term",
                               fifelse(year >= 2040 & year <= 2069, "mid_term",
                                       fifelse(year >= 2070 & year <= 2100, "long_term", NA_character_))))]

dt %>%
  group_by(code_muni) %>%
  mutate(loss = loss(yield,period)) %>%
  group_by(period,year) %>%
  summarise(loss = mean(loss)) %>%
  ggplot(aes(loss,col=period))+
  geom_density()

dt %>%
  group_by(code_muni) %>%
  mutate(loss = loss(yield,period)) %>%
  group_by(period) %>%
  summarise(loss = mean(loss))

data.frame(
  method="hadgem_ssp2",
  value = predict(models_phase2$model1,acess_ssp2)
  ) %>%
  rbind(
    data.frame(
      method="Hadgem",
      value = predict(models_phase2$model1,hadgem)
    )) %>%


data.frame(
  method = "Obs",
  value = observed$yield_corrected
) %>%
  rbind(
    data.frame(
      method="Access",
      value = predict(models_phase2$model1,acess)
    )
  ) %>%
  rbind(
    data.frame(
      method="Hadgem",
      value = predict(models_phase2$model1,hadgem)
    )) %>%
  rbind(
    data.frame(
      method="ukesm",
      value = predict(models_phase2$model1,ukesm)
    )) %>%
  group_by(method) %>%
  mutate(loss = loss(value)) %>%
  summarise(mean(loss))
