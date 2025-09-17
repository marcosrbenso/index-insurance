library(data.table)
library(tidyverse)
library(caret)
library(imputeTS)
library(data.table)
library(sf)

br <- geobr::read_municipality()
biomes <- geobr::read_biomes()

br_biomes <- st_intersection(st_make_valid(br),biomes)
br_biomes <- st_drop_geometry(br_biomes)[,c('code_muni','name_biome')]
br_biomes %>%
  subset(name_biome != "Sistema Costeiro") -> br_biomes


input_dir <- "E:\\"

soil_indices <- read.csv("C:\\Projetos\\17_index_insurance\\soil_indices.csv")

files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

cmip <- rbindlist(lapply(files, function(file) {
  cmip <- fread(file)

  # Converte para long format de maneira eficiente
  long <- melt(cmip,
               id.vars = c("code_muni", "name_muni"),
               variable.name = "names",
               value.name = "values")

  # Extrai as partes usando str_match (vetorizado e rápido)
  parts <- str_split(file,"_")[[1]]
  long$model <- substr(parts[1],4,nchar(parts[1]))
  long$index <- if(grepl("spi",file)){paste(parts[2],parts[3],sep='_')}else{str_split(long$names,"_",simplify = T)[,1]}
  long$scenario <- if(grepl("spi",file)){substr(parts[4],1,nchar(parts[4])-4)}else{substr(parts[3],1,nchar(parts[3])-4)}

  return(long)
}))

models <- cmip$model %>% unique()
scenarios <- cmip$scenario %>% unique()

prepare_data <- function(cmip) {
  require(dplyr)
  require(tidyr)
  require(stringr)

  # Extrair nome da variável e data
  cmip <- cmip %>%
    mutate(names = as.character(names)) %>%
    mutate(data = ifelse(grepl("spi", names),
                         str_split(names, "_", simplify = TRUE)[,3],
                         str_split(names, "_", simplify = TRUE)[,2])) %>%
    mutate(
      ano = str_split(data, "-", simplify = TRUE)[,1],
      month = str_split(data, "-", simplify = TRUE)[,2]
    )

  # Tratar valores extremos
  cmip <- cmip %>%
    mutate(values = case_when(
      is.infinite(values) & values == Inf ~ 4,
      is.infinite(values) & values == -Inf ~ -4,
      TRUE ~ values
    ))

  # Agrupar e calcular a média dos valores por município, ano e índice
  cmip_summary <- cmip %>%
    group_by(code_muni, ano, index, month, model,scenario) %>%
    summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
    mutate(names = paste(index, as.numeric(month), sep = "_")) %>%
    select(code_muni, ano, names,model,scenario, values) %>%
    pivot_wider(names_from = names, values_from = values)

  # Adicionar colunas do solo
  dataset2 <- merge(cmip_summary, soil_indices, by = "code_muni")

  # Corrigir nomes das colunas
  colnames(dataset2) <- sapply(colnames(dataset2), function(name) {
    aux <- str_split(name, "_", simplify = TRUE)
    if (length(aux) == 2 && !is.na(as.numeric(aux[2])) && as.numeric(aux[2]) < 10) {
      return(paste(aux[1], as.numeric(aux[2]), sep = "_"))
    } else if (length(aux) == 3) {
      return(paste0(aux[1], aux[2], "_", aux[3]))
    } else {
      return(name)
    }
  })

  return(dataset2)
}


# Models
models <- cmip$model %>% unique()
scenarios <- cmip$scenario %>% unique()

load(file="C:\\Projetos\\17_index_insurance\\models_phase2.RData")

data_prepared <- prepare_data(cmip)


data_prepared_clean <- data_prepared %>%
  mutate(across(where(is.list), ~ map_dbl(., ~ mean(unlist(.), na.rm = TRUE))))

results <- c()
for(m in seq_along(models)){
  for(s in seq_along(scenarios)){
    dat <- data_prepared %>%
      subset(model == models[m] & scenario == scenarios[s])
    dat2 <- dat %>% na.omit()
    if(is_empty(dat2$code_muni)){
      next
    }
    else{
      for(p in seq_along(models_phase2)){
        cat(paste("modelo:", models[m], "cenário:", scenarios[s], "policy:", p, "\n"))
        res1 <- data.frame(
          code_muni = dat2$code_muni,
          pred = predict(models_phase2[[p]],dat2),
          model = models[m],
          scenario = scenarios[s],
          policy = p,
          year = dat2$ano
        )
        print(mean(res1$pred))
        results <- rbind(results,res1)
      }
    }
  }
}

write.csv(results,"climate_simulations.csv",row.names = F)

results %>%
  group_by(model,scenario,policy,code_muni) %>%
  summarise(mean = mean(pred),
            sd = sd(pred)) -> results_sim

dataset <- fread("C:\\Projetos\\17_index_insurance\\dataset.csv")

dataset %>%
  group_by(CD_MUN) %>%
  summarise(obs = mean(yield_corrected)) -> results_obs

library(ggplot2)
library(dplyr)
library(Metrics)   # para rmse()
library(purrr)
library(broom)     # para glance()

# Juntar, filtrar e rotular
plot_data <- merge(results_obs,
                   results_sim,
                   by.x = "CD_MUN",
                   by.y = "code_muni") %>%
  subset(scenarios == "historic") %>%
  mutate(policy = case_when(
    policy == 1 ~ "S1 Model (Prec only)",
    policy == 2 ~ "S2 Model (Temp only)",
    policy == 3 ~ "S3 Model (Prec and Temp)",
    policy == 4 ~ "S4 Model (Complex)",
    TRUE ~ as.character(policy)
  ))

# Calcular R² e RMSE por grupo
metrics <- plot_data %>%
  group_by(model, policy) %>%
  summarise(
    rmse = Metrics::rmse(obs, mean),
    rsq = hydroGOF::NSE(mean,obs),
    .groups = "drop"
  )

# Criar gráfico com métricas
ggplot(plot_data, aes(x = obs, y = mean)) +
  geom_jitter(alpha = 0.5, width = 0.1, height = 0.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 0.5) +
  facet_wrap(~model + policy) +
  xlab("Observed yields (ton/ha)") +
  ylab("Simulated yields (ton/ha)") +
  geom_text(
    data = metrics,
    aes(
      x = Inf, y = -Inf,
      label = paste0("R² = ", round(rsq, 2), "\nRMSE = ", round(rmse, 2))
    ),
    hjust = 1.1, vjust = -0.1,
    inherit.aes = FALSE,
    size = 2
  )

ggsave("C:\\Projetos\\17_index_insurance\\obs_sim_climate_change.png",width = 10,height = 8)


results %>%
  merge(br_biomes,by='code_muni') %>%
  group_by(model,scenario,policy,name_biome) %>%
  summarise(mean = mean(pred),
            sd = sd(pred)) -> results_summary2

library(ggplot2)
data_avg_policy <- results_summary2 %>%
  mutate(
    policy = case_when(
      policy == 1 ~ "S1 Model (Prec only)",
      policy == 2 ~ "S2 Model (Temp only)",
      policy == 3 ~ "S3 Model (Prec and Temp)",
      policy == 4 ~ "S4 Model (Complex)",
      TRUE ~ as.character(policy)
    )
  ) %>%
  group_by(policy, scenario,name_biome) %>%
  summarise(mean_yield = mean(mean, na.rm = TRUE), .groups = "drop") %>%
  # Calculate percent change relative to historic
  group_by(policy,name_biome) %>%
  mutate(
    historic_mean = mean_yield[scenario == "historic"],
    pct_change = 100 * (mean_yield - historic_mean) / historic_mean,
    label = ifelse(scenario %in% c("ssp245", "ssp585"),
                   paste0(sprintf("%+0.1f", pct_change), "%"),
                   NA)
  ) %>%
  ungroup()




data_plot <- results_summary2 %>%
  mutate(
    policy = case_when(
      policy == 1 ~ "S1 Model (Prec only)",
      policy == 2 ~ "S2 Model (Temp only)",
      policy == 3 ~ "S3 Model (Prec and Temp)",
      policy == 4 ~ "S4 Model (Complex)",
      TRUE ~ as.character(policy)
    )
  )

lapply(
  unique(br_biomes$name_biome),
  function(name){
    ggplot(data_plot %>%
             ungroup() %>%
             subset(name_biome == name),
           aes(x = scenario, y = mean, color = model, group = model)) +
      geom_line() +
      geom_point() +
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = model), alpha = 0.2, color = NA) +
      # Add average percent change per policy facet
      geom_text(
        data = data_avg_policy %>% filter(!is.na(label)) %>%
          subset(name_biome == name),
        aes(x = scenario, y = mean_yield, label = label),
        inherit.aes = FALSE,
        size = 3,
        vjust = -0.8,
        color = "black"
      )+
      labs(
        y = expression("Average Crop Yield (ton" %.% ha^{-1} * ")"),
        x = "Climate Change Scenario"
      ) +
      theme(
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )+facet_wrap(~policy,ncol=2)
  }

) -> plots

plots[[1]]

ggsave("C:\\Projetos\\17_index_insurance\\impacts_on_crop_yield.png",width = 8,height = 4)

cities <- geobr::read_municipality()
br <- geobr::read_country()
biomes <- geobr::read_biomes()

loss <- function(x,x_hat){
  pmax(0,x_hat-x)/x_hat
}

results %>%
  group_by(model,policy,code_muni) %>%
  mutate(x_hat = mean(pred,na.rm=T)) %>%
  group_by(model,scenario,policy,code_muni) %>%
  summarise(
    risk_premium = mean(loss(pred,x_hat)*x_hat*30.1416129032258)
  ) %>%
  group_by(code_muni,scenario,policy) %>%
  summarise(risk_premium = mean(risk_premium)) -> results_risk_premium

results_risk_premium

library(ggplot2)
library(sf)
library(dplyr)
library(viridis)

summary(results_risk_premium$risk_premium)


# Pré-processamento: junção e renomeação dos cenários de política
map_data <- results_risk_premium %>%
  mutate(
    policy = case_when(
      policy == 1 ~ "S1_Model",
      policy == 2 ~ "S2_Model",
      policy == 3 ~ "S3_Model",
      policy == 4 ~ "S4_Model",
      TRUE ~ paste0("S", policy, "_Model")
    ),
    risk_bin = cut(
      risk_premium,
      breaks = c(0,0.05, 0.075, 0.1, 0.2, 0.25,0.45),
      labels = c("0.000–0.050", "0.050–0.750",
                 "0.075–0.100", "0.100–0.200",
                 "0.200–0.250", "0.250–0.400"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  left_join(cities, by = "code_muni") %>%
  st_as_sf()


results_risk_premium %>%
  pivot_wider(names_from="scenario",values_from="risk_premium") %>%
  mutate(change1 = (ssp245-historic)/historic*100) %>%
  mutate(change2 = (ssp585-historic)/historic*100) %>%
  subset(policy == 4) %>%
  left_join(cities, by = "code_muni") %>%
  st_as_sf() %>%
  st_as_sf() %>%
  st_write("complex_premiums.shp")

results_risk_premium %>%
  pivot_wider(names_from="scenario",values_from="risk_premium") %>%
  mutate(change1 = (ssp245-historic)/historic*100) %>%
  mutate(change2 = (ssp585-historic)/historic*100) %>%
  subset(policy == 2) %>%
  left_join(cities, by = "code_muni") %>%
  st_as_sf() %>%
  st_as_sf() %>%
  st_write("temp_premiums.shp")


st_write(biomes,"biomes.shp")
st_write(br,"br.shp")

ggplot() +
  geom_sf(data = br, fill = "gray80", color = "gray30") +
  geom_sf(data = map_data,
          aes(fill = risk_bin),
          color = NA) +
  scale_fill_brewer(palette = "YlOrRd", name = "Risk Premium\n [ton/ha]", na.value = "gray90") +
  facet_wrap(~scenario + policy) +
  geom_sf(data = na.omit(biomes), fill = NA, color = "black", linetype = "dashed") +
  theme(
    strip.text = element_text(size = 6),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

ggsave("C:\\Projetos\\17_index_insurance\\risk_premiums.png",width = 6,height = 4)

map_data %>%
  st_intersection(biomes %>% na.omit()) %>%
  group_by(scenario,policy,name_biome) %>%
  st_drop_geometry() %>%
  summarise(mean = mean(risk_premium) ) -> summary_scenarios


results_risk_premium %>%
  merge(br_biomes,by='code_muni') %>%
  group_by(name_biome,policy,scenario) %>%
  summarise(risk_premium = mean(risk_premium)) %>%
  ungroup() %>%
  mutate(risk_premium = round(risk_premium,digits = 2)) %>%
  pivot_wider(names_from="scenario",values_from = "risk_premium") %>%
  knitr::kable("latex")




summary_scenarios %>%
  ungroup() %>%
  mutate(mean = mean %>% round(digits = 3)) %>%
  pivot_wider(names_from="scenario",values_from = "mean") %>%
  mutate(change1 = (ssp245-historic)/historic*100) %>%
  mutate(change2 = (ssp585-historic)/historic*100) %>% head()
  knitr::kable('latex')

