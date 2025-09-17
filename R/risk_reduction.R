library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(scales)
library(boot)
library(tidyverse)
library(data.table)

cities <- geobr::read_municipality()
br <- geobr::read_country()
biomes <- geobr::read_biomes()

cities_biomes <- st_intersection(st_make_valid(cities),biomes)

load(file="C:\\Projetos\\17_index_insurance\\models_phase2.RData")

dataset <- fread("C:\\Projetos\\17_index_insurance\\dataset.csv")

hist(dataset$yield_corrected)

loss <- function(x){
  pmax(0,mean(x)-x)/mean(x)
}


loss_sc <- function(x){
  pmax(0,mean(x)-x)/0.06
}

extract_risk_reduction <- function(loss, risk_reduction) {

  # Initialize area counters
  total_gain <- 0  # area where risk < loss
  total_loss <- 0  # area where risk > loss

  # Calculate trapezoidal areas between curves
  for (i in 1:(length(x) - 1)) {
    dx <- x[i + 1] - x[i]

    # Average height difference in interval
    h1 <- loss_sorted[i] - risk_sorted[i]
    h2 <- loss_sorted[i + 1] - risk_sorted[i + 1]

    # Area of trapezoid between curves
    area <- 0.5 * dx * (h1 + h2)

    if (area > 0) {
      total_gain <- total_gain + area  # beneficial reduction
    } else {
      total_loss <- total_loss + abs(area)  # negative effect
    }
  }

  # Return a named list of results
  return(list(
    net_risk_reduction = total_gain - total_loss,
    gain_area = total_gain,
    loss_area = total_loss,
    relative_reduction = (total_gain - total_loss) / sum(abs(loss_sorted))
  ))
}

mean_function <- function(data, indices) {
  d <- data[indices,]
  d <- loss(d)
  return(mean(d))
}

reps <- boot(data=data.frame(x = runif(45)),
             statistic=mean_function, R=3000)

mean(reps$t)
reps$t0

dataset %>%
  group_by(CD_MUN) %>%
  mutate(loss = loss(yield_corrected)) %>%
  summarise(risk_premium = mean(loss),
            E_y = mean(yield_corrected)) %>%
  mutate(risk_premium = risk_premium*E_y) -> risk_premium



breaks <- as.numeric(summary(risk_premium$risk_premium))

risk_premium <- risk_premium %>%
  mutate(risk_cat = cut(risk_premium,
                        breaks = breaks,
                        include.lowest = TRUE
                        #,labels =
                        )
         )

risk_premium %>%
  group_by(risk_cat) %>%
  summarise(risk_premium =mean(risk_premium)) -> labels


risk_premium <- risk_premium %>%
  mutate(risk_premium_sc = risk_premium/.06) %>%
  mutate(risk_cat = cut(risk_premium,
                        breaks = breaks,
                        include.lowest = TRUE,
                        labels = paste(round(unique(labels$risk_premium)/0.06,digits = 2),"sc/ha")
  )
  )

cities_risk <- cities %>%
  merge(risk_premium,
        by.x = "code_muni", by.y= "CD_MUN")


ggplot() +
  geom_sf(data = cities_risk, aes(fill = risk_cat), color = NA) +
  geom_sf(data = br, fill = NA, color = "gray30", size = 0.3) +
  scale_fill_manual(values = c('#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571'))+
  labs(
    title = "Spatial distribution of Risk Premium",
    subtitle = expression("Values in %"~of~E(yield)~"\nCost of production is ~58.3 sc/ha"),
    x = NULL, y = NULL,
    fill="Risk premium"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 9)
  )

dataset %>%
  group_by(CD_MUN) %>%
  mutate(loss = loss_sc(yield_corrected)) %>%
  merge(st_drop_geometry(cities_risk)[,c("code_muni","risk_premium_sc")],by.x = "CD_MUN",by.y = "code_muni") -> dataset2

glimpse(dataset2)

library(caret)
library(CAST)
library(doParallel)


run_model <- function(dataset,horizon,initialWindow,feature_names){

    minYear <- min(dataset$ano)
    maxYear <- max(dataset$ano)

    folds <- (maxYear-minYear+initialWindow)+1

    cashflow <- c()

  for(i in 1:folds){

    train <- dataset %>% subset(ano < minYear+initialWindow)
    test  <- dataset %>% subset(ano > minYear+initialWindow & ano <= minYear+initialWindow+i)

    foldsx <- CreateSpacetimeFolds(train,
                                  spacevar="ano",k=10)

    ctrl <- trainControl(method = "cv",
                         index = foldsx$index,
                         indexOut = foldsx$indexOut,
                         savePredictions = "final")

    model1 <- train(
      yield_corrected ~ .,
      data = train %>% dplyr::select(feature_names),
      method = "gbm",
      trControl = ctrl,
      tuneLength = 5
    )

    test$pred <- predict(model1,test)
    test %>%
      group_by(CD_MUN) %>%
      mutate(
        predicted_loss = pmax(pred-yield_corrected,0)/0.06
      ) %>%
      mutate(loss = loss*-1,
             risk_reductions = (loss-risk_premium_sc)+predicted_loss) -> test2

    cashflow[[i]] <- test2

    # Ensure the two vectors are sorted for plotting
    loss_sorted <- sort(test2$loss)
    risk_sorted <- sort(test2$risk_reductions)

    # Create index
    x <- seq_along(loss_sorted)

    # Start the base plot
    plot(x, loss_sorted, type = "l", col = "black", lwd = 2,
         ylim = range(c(loss_sorted, risk_sorted)),
         ylab = "Gain/Loss [sc/ha]",
         xlab = "Exceedance Probability",
         main = "Loss vs. Risk Reduction")

    # Add the risk reduction line
    lines(x, risk_sorted, col = "blue", lwd = 2)

    # Fill the area between the two lines
    for (i in 1:(length(x) - 1)) {
      x_poly <- c(x[i], x[i + 1], x[i + 1], x[i])
      y_loss <- c(loss_sorted[i], loss_sorted[i + 1])
      y_risk <- c(risk_sorted[i], risk_sorted[i + 1])
      y_poly <- c(y_loss[1], y_loss[2], y_risk[2], y_risk[1])

      if (mean(y_risk) > mean(y_loss)) {
        polygon(x_poly, y_poly, col = rgb(0, 0, 1, 0.3), border = NA)  # Blue
      } else {
        polygon(x_poly, y_poly, col = rgb(1, 0, 0, 0.3), border = NA)  # Red
      }
    }

bri <- function(l1,l2){
  cm <- confusionMatrix(data = l2, reference = l1)

  (cm$table[1,2]-cm$table[2,1])/
    (cm$table[2,2]+cm$table[1,1])
}

  }

    cashflow <- do.call("rbind",cashflow)

    cashflow %>%
      mutate(
        l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
        l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
      ) %>%
      group_by(CD_MUN) %>%
      summarise(
        acc = confusionMatrix(data = l2, reference = l1)$overall[1],
        bri = bri(l1,l2),
        net_risk_reduction = mean(risk_reductions),
        relative_reduction = mean(risk_reductions)/(mean(abs(loss)))
             ) -> cashflow_summary

  return(cashflow)

}

train_x <- dataset2[,c(17,19:63,65:94)]
train <- dataset2

train %>% dplyr::select(c("yield_corrected",
                          names(train)[grepl("Prctot",names(train))],
                          names(train)[65:76]))

cl <- makePSOCKcluster(20)
registerDoParallel(cl)


feature_names <- c("yield_corrected",
                   names(train)[grepl("Prctot",names(train))],
                   names(train)[65:76])

model1 <- run_model(train,
                    initialWindow = 10,
                    feature_names = feature_names)


model2 <- run_model(train,
                    initialWindow = 10,
                    feature_names = c("yield_corrected",
                                      names(train)[grepl("temp",names(train))],
                                      names(train)[65:76]))

model3 <- run_model(train,
                    initialWindow = 10,
                    feature_names = c("yield_corrected",
                                      names(train)[grepl("Prctot",names(train))],
                                      names(train)[grepl("temp",names(train))],
                                      names(train)[65:76]))

model4 <- run_model(train,
                    initialWindow = 10,
                    feature_names = names(train_x))



stopCluster(cl)


bri <- function(l1,l2){
  cm <- confusionMatrix(data = l2, reference = l1)

  (cm$table[1,2]-cm$table[2,1])/
    (cm$table[2,2]+cm$table[1,1])
}

model1 %>%
  mutate(
    l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
    l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
  ) %>%
  group_by(CD_MUN) %>%
  summarise(
    acc = confusionMatrix(data = l2, reference = l1)$overall[1],
    bri = bri(l1,l2),
    net_risk_reduction = mean(risk_reductions),
    relative_reduction = mean(risk_reductions)/(mean(abs(loss)))
  ) -> rr_model1

model2 %>%
  mutate(
    l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
    l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
  ) %>%
  group_by(CD_MUN) %>%
  summarise(
    acc = confusionMatrix(data = l2, reference = l1)$overall[1],
    bri = bri(l1,l2),
    net_risk_reduction = mean(risk_reductions),
    relative_reduction = mean(risk_reductions)/(mean(abs(loss)))
  ) -> rr_model2

model3 %>%
  mutate(
    l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
    l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
  ) %>%
  group_by(CD_MUN) %>%
  summarise(
    acc = confusionMatrix(data = l2, reference = l1)$overall[1],
    bri = bri(l1,l2),
    net_risk_reduction = mean(risk_reductions),
    relative_reduction = mean(risk_reductions)/(mean(abs(loss)))
  ) -> rr_model3

model4 %>%
  mutate(
    l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
    l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
  ) %>%
  group_by(CD_MUN) %>%
  summarise(
    acc = confusionMatrix(data = l2, reference = l1)$overall[1],
    bri = bri(l1,l2),
    net_risk_reduction = mean(risk_reductions),
    relative_reduction = mean(risk_reductions)/(mean(abs(loss)))
  ) -> rr_model4

rr_model4 %>%
    summarise(mean(bri))

library(ggExtra)
plot_rrr_bri <- function(model, title) {
  # Calculate statistics
  median_rr <- median(model$net_risk_reduction, na.rm = TRUE)
  mean_acc <- median(model$bri, na.rm = TRUE)

  # Create base plot
  p <- model %>%
    ggplot(aes(bri, relative_reduction)) +
    geom_jitter(width = 0.5, height = 0.5, alpha = 0.7) +
    xlab("Basis Risk Index (%)") +
    ylab("Relative Risk Reduction") +
    theme_minimal() +
    ylim(-10, 5) +
    ggtitle(title) +
    annotate("text",
             x = Inf, y = -9,
             label = paste0("Median NRR: ", round(median_rr, 2)),
             hjust = 1.05, vjust = 0, size = 3.5, fontface = "italic") +
    annotate("text",
             x = Inf, y = -10,
             label = paste0("Median BRI: ", round(mean_acc, 2)),
             hjust = 1.05, vjust = 0, size = 3.5, fontface = "italic")

  # Add marginal boxplots
  ggMarginal(p, type = "histogram", margins = "both", size = 15, alpha = 0.5)
}


plot_rrr_bri(rr_model1,"Precipitation")
plot_rrr_bri(rr_model2, "Temperature")
plot_rrr_bri(rr_model3, "Precipitation and Temperature")
plot_rrr_bri(rr_model4, "Complex")

ggpubr::ggarrange(
  plot_rrr_bri(rr_model1,"Precipitation"),
  plot_rrr_bri(rr_model2, "Temperature"),
  plot_rrr_bri(rr_model3, "Precipitation and Temperature"),
  plot_rrr_bri(rr_model4, "Complex"),
  labels = c("A","B","C","D")
)

make_summary <- function(model){
  model %>%
    merge(st_drop_geometry(cities_biomes)[,c("code_muni","name_biome")],
          by.x="CD_MUN",by.y="code_muni") %>%
    mutate(
      l1 = ifelse(loss < 0 ,1,0) %>% factor(levels = c(0,1)),
      l2 = ifelse(predicted_loss*-1 < 0,1,0) %>% factor(levels = c(0,1))
    ) %>%
    group_by(CD_MUN,name_biome) %>%
    summarise(
      acc = confusionMatrix(data = l2, reference = l1)$overall[1],
      bri = bri(l1,l2),
      net_risk_reduction = sum(loss-risk_reductions)/45,
      relative_reduction = (sum(loss)-sum(risk_reductions))/(sum(abs(loss))),
      premium = sum(risk_premium_sc),
      payment = sum(predicted_loss)
    ) %>%
    mutate(LR = payment/premium) %>%
    group_by(name_biome) %>%
    summarise(
      acc = median(acc) %>% round(digits = 2),
      bri = median(bri) %>% round(digits = 2),
      net_risk_reduction = median(net_risk_reduction) %>% round(digits = 2),
      relative_reduction = median(relative_reduction) %>% round(digits = 2),
      LR = median(LR) %>% round(digits = 2)
    )
}

make_summary(model1) %>%
  rbind(make_summary(model2)) %>%
  rbind(make_summary(model3)) %>%
  rbind(make_summary(model4)) -> summary_table

summary_table %>% knitr::kable("latex")


ggsave("basis_risk_risk_reduction.png",width = 8, height = 8)
