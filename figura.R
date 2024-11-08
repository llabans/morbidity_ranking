library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)

# Cargar datos del archivo Excel
data_2019 <- read_excel("2019.2023.xlsx", sheet = "año2019")
data_2023 <- read_excel("2019.2023.xlsx", sheet = "año2023")
# Rellenar valores faltantes en 'Etapa'
data_2019 <- data_2019 %>%
  fill(Etapa, .direction = "down")

data_2023 <- data_2023 %>%
  fill(Etapa, .direction = "down")

# Filtrar datos para eliminar Totales
#data_2019 <- data_2019 %>%
#  filter(!str_detect(Etapa, "Total"))

#data_2023 <- data_2023 %>%
 # filter(!str_detect(Etapa, "Total"))


# Generar categorías de edad
data_2019 <- data_2019 %>%
  mutate(Etapa_ = case_when(
    Etapa %in% c("< 01m", "01-11m", "01-05a","06-11a") ~ "Niños",
    Etapa %in% c("12-17a") ~ "Adolescente",
    Etapa %in% c("18-29a") ~ "Joven",
    Etapa %in% c("30-59a") ~ "Adulto",
    Etapa %in% c("60a >") ~ "Adulto mayor",
    TRUE ~ "Otros"
  ))
writexl::write_xlsx(data_2019, "C:/Users/luisl/iCloudDrive/r/reporting_excel_ppt/morbilidad/national/2019.xlsx")

data_2023 <- data_2023 %>%
  mutate(Etapa_ = case_when(
    Etapa %in% c("< 01m", "01-11m", "01-05a","06-11a") ~ "Niños",
    Etapa %in% c("12-17a") ~ "Adolescente",
    Etapa %in% c("18-29a") ~ "Joven",
    Etapa %in% c("30-59a") ~ "Adulto",
    Etapa %in% c("60a >") ~ "Adulto mayor",
    TRUE ~ "Otros"
  ))
writexl::write_xlsx(data_2023, 
                    "C:/Users/luisl/iCloudDrive/r/reporting_excel_ppt/morbilidad/national/2023_2.xlsx")

# Calcular porcentajes por grupo de edad
# Calcular porcentajes por grupo de edad
data_2019 <- data_2019 %>%
  group_by(Etapa_) %>%
  mutate(porcentaje = Total / sum(Total) * 100) %>%
  ungroup()

data_2023 <- data_2023 %>%
  group_by(Etapa_) %>%
  mutate(porcentaje = Total / sum(Total) * 100) %>%
  ungroup()



# Seleccionar las 10 principales causas de cada año por etapa
top_10_2019 <- data_2019 %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>%
  mutate(rank_2019 = row_number()) %>%
  ungroup()

top_10_2023 <- data_2023 %>%
  group_by(Etapa_) %>%
  arrange(desc(Total)) %>%
  slice(1:10) %>%
  mutate(rank_2023 = row_number()) %>%
  ungroup()

# Función para crear colores personalizados
get_colors <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Preparar los datos para el gráfico
prepare_plot_data <- function(top_10_2019, top_10_2023) {
  combined_data <- full_join(top_10_2019, top_10_2023, by = c("Categoria", "Etapa_"), suffix = c("_2019", "_2023"))
  
  combined_data %>%
    mutate(Enfermedad = Categoria) %>%
    gather(key = "Year", value = "Ranking", rank_2019, rank_2023) %>%
    mutate(Year = ifelse(Year == "rank_2019", "2019", "2023")) %>%
    select(Year, Enfermedad, Ranking, Etapa_, Total_2019, Total_2023, porcentaje_2019, porcentaje_2023) %>%
    gather(key = "Total_Year", value = "Total", Total_2019, Total_2023) %>%
    gather(key = "Porcentaje_Year", value = "Porcentaje", porcentaje_2019, porcentaje_2023) %>%
    filter(!is.na(Ranking))
}

age_groups <- unique(data_2019$Etapa_)

for (age_group in age_groups) {
  top_10_2019_age <- top_10_2019 %>% filter(Etapa_ == age_group)
  top_10_2023_age <- top_10_2023 %>% filter(Etapa_ == age_group)
  
  plot_data <- prepare_plot_data(top_10_2019_age, top_10_2023_age)
  
  # Crear las tablas para 2019 y 2023
  table_2019 <- plot_data %>%
    filter(Year == "2019") %>%
    select(Enfermedad, Total, Porcentaje, Ranking) %>%
    arrange(Ranking)
  
  table_2023 <- plot_data %>%
    filter(Year == "2023") %>%
    select(Enfermedad, Total, Porcentaje, Ranking) %>%
    arrange(Ranking)
  
  # Crear el gráfico
  p <- ggplot() +
    geom_segment(data = plot_data, aes(x = 1, xend = 2, y = Ranking, yend = Ranking, color = Enfermedad),
                 size = 1.5, alpha = 0.5) +
    geom_text(data = table_2019, aes(x = 1, y = Ranking, label = paste(Enfermedad, "(", round(Porcentaje, 2), "%)")), 
              hjust = 1, size = 4) +
    geom_text(data = table_2023, aes(x = 2, y = Ranking, label = paste(Enfermedad, "(", round(Porcentaje, 2), "%)")), 
              hjust = 0, size = 4) +
    scale_x_continuous(breaks = c(1, 2), labels = c("2019", "2023"), expand = c(.2, .2)) +
    scale_y_reverse(breaks = 1:10) +
    scale_color_manual(values = get_colors(length(unique(plot_data$Enfermedad)))) +
    theme_minimal() +
    labs(title = paste("Ranking de Causas de Morbilidad -", age_group),
         subtitle = "Comparación de los años 2019 y 2023",
         x = "Año", y = "Ranking") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.ticks.x = element_blank())
  
  guardar = "C:/Users/luisl/iCloudDrive/r/reporting_excel_ppt/morbilidad/national/"
  # Guardar la gráfica
  ggsave(filename = paste0(guardar,"Top10_Morbilidad_", age_group, ".png"), plot = p, width = 14, height = 10)
}
