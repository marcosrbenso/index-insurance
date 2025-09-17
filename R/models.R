library(tidyverse)
library(caret)



comparison_dataset_corrected <- read.csv("C:\\Projetos\\17_index_insurance\\soybean_dataset.csv")
nrow(comparison_dataset_corrected)

climate_dataset <- read.csv("C:\\Projetos\\17_index_insurance\\climate_indices.csv")
soil_indices <- read.csv("C:\\Projetos\\17_index_insurance\\soil_indices.csv")

spi_3 <- read.csv("C:\\Projetos\\17_index_insurance\\spi_3_indices.csv")
spi_6 <- read.csv("C:\\Projetos\\17_index_insurance\\spi_6_indices.csv")


spi_3 %>%
  pivot_longer(!c(code_muni,name_muni,ID),names_to="dates",values_to="spi") %>%
  mutate(ano = as.numeric(substr(dates,2,5))) %>%
  mutate(month = as.numeric(substr(dates,7,8))) %>%
  dplyr::select(-c(ID,dates)) %>%
  mutate(spi = ifelse(spi == -Inf,NA,
                      ifelse(spi == Inf,NA,spi))) %>%
  mutate(spi = imputeTS::na.kalman(spi)) %>%
  subset(month %in% c(1:4,8:12)) %>%
  mutate(ano = ifelse(month %in% c(8:12), ano+1,ano)) %>%
  mutate(month = paste("spi3",month,sep="_")) %>%
  pivot_wider(names_from="month",values_from='spi') -> spi_3

spi_6 %>%
  pivot_longer(!c(code_muni,name_muni,ID),names_to="dates",values_to="spi") %>%
  mutate(ano = as.numeric(substr(dates,2,5))) %>%
  mutate(month = as.numeric(substr(dates,7,8))) %>%
  dplyr::select(-c(ID,dates)) %>%
  mutate(spi = ifelse(spi == -Inf,NA,
                      ifelse(spi == Inf,NA,spi))) %>%
  mutate(spi = imputeTS::na.kalman(spi)) %>%
  subset(month %in% c(1:4,8:12)) %>%
  mutate(ano = ifelse(month %in% c(8:12), ano+1,ano)) %>%
  mutate(month = paste("spi6",month,sep="_")) %>%
  pivot_wider(names_from="month",values_from='spi') ->spi_6


climate_dataset %>%
  pivot_longer(!c(code_muni,name_muni,ID),names_to="var",values_to="values") %>%
  group_by(code_muni,ID,var) %>%
  mutate(variable = str_split(var,"_")[[1]][1],
         date = str_split(var,"_")[[1]][2]) %>%
  mutate(month = as.numeric(str_split(date,'\\.')[[1]][2]),
         year  = as.numeric(str_split(date,'\\.')[[1]][1]) ) %>%
  subset(month %in% c(1:4,8:12)) %>%
  mutate(year = ifelse(month %in% c(8:12), year+1,year)) %>%
  mutate(var_name = paste0(variable,"_",month)) %>%
  ungroup() %>%
  dplyr::select(c(code_muni,name_muni,year,var_name,values)) %>%
  pivot_wider(names_from="var_name",values_from="values") -> climate_dataset2

dataset <- merge(comparison_dataset_corrected,
                 climate_dataset2,by.x=c("CD_MUN","ano"),
                 by.y = c("code_muni","year"))

dataset2 <- merge(dataset,
                  soil_indices %>% dplyr::select(-name_muni),
                  by.x = "CD_MUN",
                  by.y = "code_muni")

dataset2 <- merge(dataset2,
                  spi_3 %>% dplyr::select(-name_muni),
                  by.x = c("CD_MUN","ano"),
                  by.y = c("code_muni","ano"))

dataset2 <- merge(dataset2,
                  spi_6 %>% dplyr:::select(-name_muni),
                  by.x = c("CD_MUN","ano"),
                  by.y = c("code_muni","ano"))

dataset2 <- na.omit(dataset2)

write.csv(dataset2,"C:\\Projetos\\17_index_insurance\\dataset.csv",row.names = F)

train <- dataset2 %>% subset(ano <= 2018)
test  <- dataset2 %>% subset(ano  > 2018)

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

summary(train$yield_corrected)
summary(test$yield_corrected)

train %>% select(c(name_muni,ano,
                   yield_mapbiomas,
                   yield_ibge,
                   AREA_KM2,
                   prod,
                   yield_corrected)) %>%
  arrange(prod) %>% head()

library(CAST)

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

indices <- CreateSpacetimeFolds(
  train,
  spacevar = NA,
  timevar = 'ano',
  k = 10,
  class = NA,
  seed = sample(1:1000, 1)
)


ctrl <- trainControl(method = "cv",
                     index = indices$indx_train,
                     savePredictions = "final")
colnames(train)
train_x <- train[,c(17,19:63,65:94)]
colnames(train_x)
summary(train_x)

fit.lm <- train(
  yield_corrected ~ .,
  data = train_x,
  method = 'lm',
  trControl = ctrl
)

fit.rf <- train(
  yield_corrected ~ .,
  data = train_x,
  method = 'rf',
  trControl = ctrl,
  tuneLength = 5
)

fit.glmnet <- train(
  yield_corrected ~ .,
  data = train_x,
  method = 'glmnet',
  trControl = ctrl,
  tuneLength = 5
)

fit.gbm <- train(
  yield_corrected ~ .,
  data = train_x,
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5,
  verbose = FALSE
)

fit.xgb <- train(
  yield_corrected ~ .,
  data = train_x,
  method = "xgbTree",
  trControl = ctrl,
  tuneLength = 5
)

fit.svm <- train(
  yield_corrected ~ .,
  data = train_x,
  method = "svmRadial",
  trControl = ctrl,
  tuneLength = 5
)

fit.nnet <- train(
  yield_corrected ~ .,
  data = train_x,
  method = "nnet",
  trControl = ctrl,
  tuneLength = 5,
  linout = TRUE, trace = FALSE
)

models <- list(
  lm = fit.lm,
  rf = fit.rf,
  glmnet = fit.glmnet,
  gbm = fit.gbm,
  xgb = fit.xgb,
  svm = fit.svm,
  nnet = fit.nnet
)

save(models,file="C:/Projetos/12_Insurance/Model/models.RData")

load(file="C:/Projetos/17_index_insurance/models_phase2.RData")

results <- resamples(models)
summary(results)

bwplot(results)
bwplot(results)

global_validation(fit.lm)
global_validation(fit.rf)
global_validation(fit.xgb)

lapply(models,global_validation) %>% do.call("rbind",.) %>%
  as.data.frame() %>%
  mutate(models = rownames(.),
         set = "train") -> train.r

lapply(models,function(x){
  postResample(predict(x,test),test$yield_corrected)
})  %>% do.call("rbind",.) %>%
  as.data.frame() %>%
  mutate(models = rownames(.),
         set = "test") -> test.r

model_selection <- rbind(train.r,test.r) %>%
  as.data.frame() %>%
  mutate(RMSE = round(RMSE,digits = 2),
         Rsquared = round(Rsquared,digits = 2),
         MAE = round(MAE,digits = 2)) %>%
  dplyr::select(c(models,set,RMSE,Rsquared,MAE))

knitr::kable(model_selection,'latex')

dataset2 <- read.csv("C:\\Projetos\\17_index_insurance\\dataset.csv")
train <- dataset2 %>% subset(ano <= 2018)
test  <- dataset2 %>% subset(ano  > 2018)

library(ggplot2)
library(dplyr)
library(Metrics)

results <- data.frame(
  pred = predict(models$lm, test),
  obs = test$yield_corrected
)


plot_predictions <- function(results,model){
  metrics <- postResample(results$pred,results$obs)
  rmse_val <- metrics[1]
  mae_val <- metrics[3]
  r2_val <- metrics[2]

  # Criar rótulo com as métricas
  metrics_label <- paste0("RMSE = ", round(rmse_val, 2),
                          "\nMAE = ", round(mae_val, 2),
                          "\nR² = ", round(r2_val, 2))

  # Plot
  results %>%
    ggplot(aes(obs, pred)) +
    geom_jitter(col = 'blue', size = 0.2) +
    geom_smooth(method = 'lm', col = 'red', size = 0.5) +
    geom_abline() +
    xlim(c(0, 8)) +
    ylim(c(0, 8)) +
    xlab("Observed Yield (ton/ha)") +
    ylab("Predicted Yield (ton/ha)") +
    ggtitle(model)+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    annotate("text", x = 1, y = 6.5, label = metrics_label, hjust = 0, size = 3.5)
}

library(ggpubr)

ggarrange(
  plot_predictions(data.frame(
    pred = predict(models$lm, test),
    obs = test$yield_corrected),
    "Linear Model"),
  plot_predictions(data.frame(
    pred = predict(models$rf, test),
    obs = test$yield_corrected),"Random Forest"),
  plot_predictions(data.frame(
    pred = predict(models$glmnet, test),
    obs = test$yield_corrected),"Regularized Generalized Linear Models"),
  plot_predictions(data.frame(
    pred = predict(models$gbm, test),
    obs = test$yield_corrected),"Gradient Boosting Machine"),
  plot_predictions(data.frame(
    pred = predict(models$xgb, test),
    obs = test$yield_corrected),"eXtreme Gradient Boosting Tree"),
  plot_predictions(data.frame(
    pred = predict(models$svm, test),
    obs = test$yield_corrected),"Support Vector Machine"),
  plot_predictions(data.frame(
    pred = predict(models$nnet, test),
    obs = test$yield_corrected),"Neural Network with single-hidden-layer"),
  labels = c("A","B","C","D","E","G")
)

ggsave("C:\\Projetos\\17_index_insurance\\performance.png",height = 10,width = 12)

library(doParallel)

dataset2 <- read.csv("C:\\Projetos\\17_index_insurance\\dataset.csv")
train <- dataset2 %>% subset(ano <= 2018)
test  <- dataset2 %>% subset(ano  > 2018)

indices <- CreateSpacetimeFolds(
  train,
  spacevar = NA,
  timevar = 'ano',
  k = 10,
  class = NA,
  seed = sample(1:1000, 1)
)


ctrl <- trainControl(method = "cv",
                     index = indices$indx_train,
                     savePredictions = "final")

cl <- makePSOCKcluster(20)
registerDoParallel(cl)

model1 <- train(
  yield_corrected ~ .,
  data = train[,c("yield_corrected",
                  names(train)[grepl("Prctot",names(train))],
                  names(train)[65:76])],
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5
)

model2 <- train(
  yield_corrected ~ .,
  data = train[,c("yield_corrected",
                  names(train)[grepl("temp",names(train))],
                  names(train)[65:76])],
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5
)

model3 <- train(
  yield_corrected ~ .,
  data = train[,c("yield_corrected",
                  names(train)[grepl("Prctot",names(train))],
                  names(train)[grepl("temp",names(train))],
                  names(train)[65:76])],
  method = "gbm",
  trControl = ctrl,
  tuneLength = 5
)

list(
  model1 = model1,
  model2 = model2,
  model3 = model3,
  model4 = models$xgb
) -> models_phase2

biomes <- read_biomes()

lapply(models_phase2,global_validation)

lapply(models_phase2,function(x){
  postResample(predict(x,test),test$yield_corrected)
})

save(models_phase2,file="models_phase2.RData")


library(geobr)
library(tidyverse)
library(sf)
library(ggpubr)
library(CAST)
library(caret)

biomes <- read_biomes() %>% na.omit()
cities <- read_municipality()
br <- read_country()

# List of models and labels

load(file = "models_phase2.RData")

labels <- c("A", "B", "C", "D")

models_phase2

rmse_breaks <- c(0, 0.25, 0.5, 1, 1.25, 1.5, 2.5)
rmse_labels <- c("0–0.25", "0.25–0.5", "0.5–1", "1–1.25", "1.25–1.5", "1.5–2.5")
rmse_colors <- c("#006837", "#66bd63", "#d9ef8b", "#fee08b", "#f46d43", "#a50026")

# Função auxiliar para calcular RMSE por município
compute_rmse_map <- function(model, test_data, cities_data, use_categorical = FALSE) {
  df <- test_data %>%
    mutate(pred = predict(model, test_data)) %>%
    group_by(CD_MUN) %>%
    summarise(RMSE = sqrt(mean((pred - yield_corrected)^2))) %>%
    merge(cities_data, by.x = "CD_MUN", by.y = "code_muni")

  if (use_categorical) {
    df <- df %>%
      mutate(RMSE_cat = cut(RMSE, breaks = rmse_breaks, labels = rmse_labels, include.lowest = TRUE))
    df <- df %>% na.omit()
  }

  return(st_as_sf(df))
}


# Função para plotar o mapa
plot_rmse_map <- function(data_sf, fill_var, title, fill_type = "continuous") {
  ggplot() +
    geom_sf(data = data_sf, aes(fill = .data[[fill_var]]), col = NA) +
    geom_sf(data = br, fill = NA) +
    geom_sf(data = biomes, fill = NA) +
    {
      if (fill_type == "categorical") {
        scale_fill_manual(values = rmse_colors, name = "RMSE (ton/ha)")
      } else {
        scale_fill_gradient2(low = "#006837", mid = "#ffffbf", high = "#a50026", midpoint = 1)
      }
    } +
    ggtitle(title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Gerar mapas
p1 <- plot_rmse_map(
  data_sf = compute_rmse_map(models_phase2$model1, test, cities, use_categorical = TRUE),
  fill_var = "RMSE_cat",
  title = "Prec",
  fill_type = "categorical"
)

p2 <- plot_rmse_map(
  data_sf = compute_rmse_map(models_phase2$model2, test, cities, use_categorical = TRUE),
  fill_var = "RMSE_cat",
  title = "Temp",
  fill_type = "categorical"
)

p3 <- plot_rmse_map(
  data_sf = compute_rmse_map(models_phase2$model3, test, cities, use_categorical = TRUE),
  fill_var = "RMSE_cat",
  title = "Prec-Temp",
  fill_type = "categorical"
)

p4 <- plot_rmse_map(
  data_sf = compute_rmse_map(models_phase2$model4, test, cities, use_categorical = TRUE),
  fill_var = "RMSE_cat",
  title = "Complex",
  fill_type = "categorical"
)


lapply(1:length(models_phase2),function(i){
  data <- st_intersection(compute_rmse_map(models_phase2[[i]], test, cities, use_categorical = F),
                          biomes)
  data$model <- names(models_phase2)[i]
  data
}) -> x

lapply(x,st_drop_geometry) %>%
  do.call("rbind",.) %>%
  group_by(name_biome,model) %>%
  summarise(RMSE = mean(RMSE)) %>%
  pivot_wider(names_from="model",values_from="RMSE") %>%
  knitr::kable("latex")

x %>%
  st_drop_geometry() %>%
  group_by(name_biome) %>%
  summarise(RMSE = median(RMSE))

ggarrange(plotlist = list(p1,p2,p3,p4), labels = labels,
          common.legend = TRUE,
          legend = "left")

ggsave("C:\\Projetos\\17_index_insurance\\model_errors.png",width = 6,height = 5)




