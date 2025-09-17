# Install and load required packages
if (!require("sidrar")) install.packages("sidrar")
if (!require("tidyverse")) install.packages("tidyverse")
library(sidrar)
library(tidyverse)
library(scales)

info_sidra(1612)

soy_prod <- lapply(1974:2023,function(year){
  get_sidra(
    x = 1612,
    variable = 214,
    period = as.character(year),
    geo = "City",
    classific = "c81",
    category = list("2713")
  )
})



soy_area <- lapply(1974:2023,function(year){
  get_sidra(
    x = 1612,
    variable = 109,
    period = as.character(year),
    geo = "City",
    classific = "c81",
    category = list("2713")
  )
})

soy_prod2 <- do.call("rbind",soy_prod)
soy_area2 <- do.call("rbind",soy_area)

soy_prod2[,c("Valor","Município (Código)","Município","Ano")] %>%
  rename(prod = Valor) %>%
  merge(soy_area2[,c("Valor","Município (Código)","Município","Ano")] %>%
          rename(area = Valor),
        by = c("Município (Código)","Município","Ano")) -> soy_data

cities_list <- c(5107925,4127700, 4308508, 4314407, 4314100 ,4115200)

soy_data %>%
  mutate(Ano = as.numeric(Ano)) %>%
  rename(city_code = `Município (Código)`) %>%
  subset(city_code %in% cities_list) %>%
  ggplot(aes(Ano,prod))+
  geom_line()+
  geom_jitter()+
  facet_wrap(~Município,scales = "free")

soy_data %>%
  mutate(Ano = as.numeric(Ano)) %>%
  rename(city_code = `Município (Código)`) %>%
  subset(city_code %in% cities_list) %>%
  ggplot(aes(Ano,area))+
  geom_line()+
  geom_jitter()+
  facet_wrap(~Município,scales = "free")

soy_data %>% tail()

library(trend)

a_diff <- function(x){
  max_a <- max(x)
  y <- rep(1,length(x))
  for(i in 1:(length(x)-1)){
    y[i] <- x[i+1]-x[i]
  }
  return(y)
}

soy_data %>%
  mutate(Ano = as.numeric(Ano)) %>%
  rename(code_muni = `Município (Código)`) -> soy_data

soy_data %>%
  group_by(code_muni) %>%
  mutate(prod = ifelse(prod < 10,NA,prod)) %>%
  mutate(prod = ifelse(prod/area < 0.08,NA,prod)) %>%
  subset(is.na(prod)==F) %>%
  mutate(Ano_diff = a_diff(Ano)) %>%
  subset(Ano_diff == 1) %>%
  mutate(n = n()) %>%
  subset(n > 5) %>%
  summarise(p = mk.test(prod, continuity = TRUE)$p.value,
            tau = mk.test(prod, continuity = TRUE)$estimates[3]
            ) -> trend_soy

library(geobr)
library(sf)
library(tidyverse)

cities <- read_municipality()
br <- read_country()

trend_soy <- merge(cities,trend_soy,by = "code_muni") %>% st_as_sf()

ggplot() +
  # Tau map
  geom_sf(data = trend_soy, aes(fill = tau), color = NA) +

  # State borders
  geom_sf(data = br, fill = NA, color = "grey30", size = 0.3) +

  # Color scale
  scale_fill_gradient2(
    name = "Kendall's Tau",
    low = "#a50026",     # red
    mid = "#fee08b",
    high = "#006837",    # blue
    midpoint = 0,
    na.value = "grey90",
    limits = c(-1, 1),   # adjust if needed
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("-1", "-0.5", "0", "0.5", "1")
  ) +

  # Theme and layout
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +

  labs(
    title = "Trend in Soybean Production"
  )

ggsave('C:\\Projetos\\17_index_insurance\\prod_trend.png',height = 5, width = 8)

mapbiomas_soja <- read.csv("C:\\Projetos\\17_index_insurance\\mapbiomas_soja_municipios_1985_2023-001.csv")

mapbiomas_soja <- mapbiomas_soja[,2:7]

merge(mapbiomas_soja,
      soy_data,
      by.x = c("CD_MUN","ano"),
      by.y = c("code_muni","Ano")
      ) -> comparison_dataset

cor(data.frame(area_mp = comparison_dataset$area_soja_ha,
               area = comparison_dataset$area,
               prod = comparison_dataset$prod),
    use="complete.obs")

cities_list <- c(5107925,
                 4127700,
                 3147006,
                 5218805,
                 2928901,
                 4314100
                 #,4115200
                 )


# Reshape to long format
comparison_long <- comparison_dataset %>%
  subset(CD_MUN %in% cities_list) %>%
  pivot_longer(
    cols = c(area_soja_ha, area),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(source = recode(source,
                         area_soja_ha = "MapBiomas",
                         area = "IBGE"))

# Plot with legend
ggplot(comparison_long, aes(x = ano, y = value, color = source)) +
  geom_line(size = 1) +
  facet_wrap(~Município, scales = "free") +
  labs(
    x = "Year",
    y = "Soybean Area (x 10³ ha)",
    color = "Data Source"
  ) +
  scale_color_manual(values = c("MapBiomas" = "#1b9e77", "IBGE" = "#d95f02")) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3))+
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  ) +
  guides(color = guide_legend(ncol = 1))

ggsave('C:\\Projetos\\17_index_insurance\\Soybean_area.png',height = 5, width = 8)

comparison_dataset %>%

  mutate(n = n()) %>%
  subset(n > 5) -> comparison_dataset

library(metrica)

comparison_dataset %>%
  #mutate(area_soja_ha = ifelse(area_soja_ha == 0,NA,area_soja_ha)) %>%
  group_by(CD_MUN) %>%
  mutate(prod = ifelse(prod < 10,NA,prod)) %>%
  mutate(prod = ifelse(prod/area < 0.1,NA,prod)) %>%
  subset(is.na(prod)==F) %>%
  mutate(Ano_diff = a_diff(ano)) %>%
  subset(Ano_diff == 1) %>%
  mutate(yield_mapbiomas = prod/area_soja_ha,
         yield_ibge = prod/area) %>%
  mutate(prod_flag = rep(rle(prod)$lengths,rle(prod)$lengths)) %>%
  mutate(yield_flag = rep(rle(yield_ibge)$lengths,rle(yield_ibge)$lengths)) %>%
  mutate(yield_flag2 = ifelse(yield_ibge > 10,0,1)) %>%
  subset(
    prod_flag == 1 & yield_flag == 1
  ) %>%
  subset(yield_flag2 == 1) %>%
  na.omit() %>%
  mutate(
    n = length(ano)
  ) %>%
  subset(n > 10) -> comparison_dataset_clean

remove_trend <- function(yield,year){
  model <- lm(yield~year)
  yield <- yield+(max(year)-year)*model$coefficients[2]
  return(yield)
}

comparison_dataset_clean %>%
  mutate(yield_corrected = remove_trend(yield_ibge,ano)) -> comparison_dataset_corrected

comparison_dataset_corrected %>%
  subset(CD_MUN == 4309506) %>%
  #subset(CD_MUN %in% cities_list) %>%
  ggplot()+
  geom_jitter(aes(ano,yield_ibge),col='blue')+
  geom_jitter(aes(ano,yield_corrected),col='red')+
  facet_wrap(~Município)

write.csv(comparison_dataset_corrected,"C:\\Projetos\\17_index_insurance\\soybean_dataset.csv",row.names = F)



