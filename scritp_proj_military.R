# ================================
# SIPRI - Gastos Militares do Brasil (2000-2024)
# Deflacionamento com base no IPCA
# Autor: Jeff Delavusca
# ================================

# -------------------------------
# 1. Carregando pacotes
# -------------------------------

if (!require("sidrar")) install.packages("sidrar")
library(sidrar)
library(tidyverse)

# -------------------------------
# 2. Carregando dataset do GitHub (Brasil)
# -------------------------------

url_data <- "https://raw.githubusercontent.com/Jeff-Delavusca/sipri-brasil-gastos-militares/refs/heads/main/dados-gastos-nominais.csv"

db_expenditure <- read.csv2(url_data)

# Tratando os dados
db_expenditure <- db_expenditure[, -3]  # Remove coluna desnecessária

# -------------------------------
# 3. IPCA anual (2000–2024)
# -------------------------------

ipca <- data.frame(
  year = 2000:2024,
  ipca = c(5.97, 7.67, 12.53, 9.3, 7.6, 5.69, 3.14, 4.46, 5.9, 4.31,
           5.91, 6.5, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31,
           4.52, 10.06, 5.79, 4.62, 4.83)
)

# -------------------------------
# 4. Juntando bases e deflacionando os dados
# -------------------------------

year_base <- 2024
bilions <- 1e9

db_complete <- db_expenditure %>%
  left_join(ipca, by = "year") %>%
  mutate(
    fator_inflacao = (ipca / 100) + 1,
    fator_acumulado = cumprod(fator_inflacao),
    deflator = fator_acumulado / fator_acumulado[year == year_base],
    gastos_reais = brazil_military_expenditure * deflator,
    gastos_reais_reduzido = gastos_reais / bilions
  )

View(db_complete)

# -------------------------------
# 5. Cálculo da variação acumulada entre 2000 e 2023
# -------------------------------

start_year <- 2000
end_year <- 2023

start_value <- db_complete %>%
  filter(year == start_year) %>%
  pull(gastos_reais)

end_value <- db_complete %>%
  filter(year == end_year) %>%
  pull(gastos_reais)

percentage_chage <- ((end_value - start_value) / start_value) * 100

cat(paste0("Variação real dos gastos militares entre ",
           start_value, " e ", end_value, ": ",
           round(percentage_chage, 2), "%\n"))

# -------------------------------
# 6. Gráfico dos gastos reais com anotações
# -------------------------------

ggplot2::ggplot() +
  geom_line(data = db_complete,
            aes(x = year, y = gastos_reais_reduzido),
            size = 1,
            color = "firebrick") +
  theme_bw() +
  geom_text(aes(x = 2002, y = 19, label = "Gastos em 2000:\nR$ 4,92 Bi"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  geom_text(aes(x = 2022, y = 60, label = "Gastos em 2023:\nR$ 103,81 Bi"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  geom_text(aes(x = 2012, y = 57, label = "Crescimento de\n+ de 2000% entre\n2000 e 2023"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Ano",
    y = "Gastos Militares (R$ bilhões)",
    title = "Gastos Militares do Brasil (valores reais, base 2024)",
    caption = "Fonte: SIPRI e IPCA/IBGE"
  )

ggsave("grafico_gastos.png", width = 7, height = 4, dpi = 300)

# -------------------------------
# 7. Carregando base regional para análises comparativas
# -------------------------------

url_data2 <- "https://raw.githubusercontent.com/Jeff-Delavusca/sipri-brasil-gastos-militares/refs/heads/main/military_expenditure_by_region.csv"

expenditure_by_region <- read.csv2(url_data2)

# Exemplo: Crescimento acumulado dos gastos da África
start_year <- 2000

base_value <- expenditure_by_region %>%
  filter(Year == start_year) %>%
  select(-Year)

acm_by_region <- expenditure_by_region %>%
  mutate(
    África = ((Africa / base_value$Africa) - 1) * 100,
    `Ásia e Oceania` = ((Asia...Oceania / base_value$Asia...Oceania) - 1) * 100,
    Europa = ((Europe / base_value$Europe) - 1) * 100,
    `Oriente Médio` = ((Middle.East / base_value$Middle.East) - 1) * 100,
    Américas = ((Americas / base_value$Americas) - 1) * 100,
  ) %>% 
  select(Year,África, `Ásia e Oceania`, Europa, `Oriente Médio`, Américas) 

# Convertendo para formato longo
acm_by_region_long <- acm_by_region %>%
  pivot_longer(-Year, names_to = "Região", values_to = "Valor")


# Ordenar regiões por valor final (último ano), para melhor sobreposição visual
ordem_regioes <- acm_by_region_long %>%
  filter(Year == max(Year)) %>%
  arrange(Valor) %>%
  pull(Região)


# Transformar Região em fator ordenado
acm_by_region_long <- acm_by_region_long %>%
  mutate(Região = factor(Região, levels = ordem_regioes))


# Plot elegante
ggplot(acm_by_region_long, aes(x = Year, y = Valor, fill = Região, color = Região)) +
  geom_line(size = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_family = "Arial") +
  geom_text(aes(x = 2024.5, y = 232, label = "Ásia e Oceania: 229,4%"),
            nudge_y = 3, color = "#228B22", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2024.5, y = 169, label = "Oriente Médio: 164,9%"),
            nudge_y = 3, color = "#c71585", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2024.95, y = 77, label = "Américas: 72,2%"),
            color = "#006400", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025, y = 116, label = "Europa: 113,3%"),
            nudge_y = 3, color = "#800080", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025, y = 106, label = "África: 107,6%"),
            nudge_y = 3, color = "#C76E00", size = 5, hjust = 0.5, vjust = 0.5) +
  scale_x_continuous(breaks = 2000:2024) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = " ",
    x = " ",
    y = "Variação acumulada dos gastos militares (%)",
    caption = "Fonte: SIPRI Military Expenditure Database"
  )


expenditure_by_region_long <- expenditure_by_region %>% 
    rename(
      `Ásia e Oceania` = Asia...Oceania,
      `Oriente Média` = Middle.East) %>% 
  pivot_longer(
    -Year,
    names_to = "Região",
    values_to = "Valor"
    )


ordem_regioes <- expenditure_by_region_long %>%
  filter(Year == max(Year)) %>%
  arrange(Valor) %>%
  pull(Região)


expenditure_by_region_long <- expenditure_by_region_long  %>%
  mutate(Região = factor(Região, levels = ordem_regioes))


ggplot(expenditure_by_region_long, aes(x = Year, y = Valor, fill = Região, color = Região)) +
  geom_line(size = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_family = "Arial") +
  geom_text(aes(x = 2025.2, y = 615, label = "Ásia e Oceania:\nU$ 632,3 bi"),
            nudge_y = 1, color = "#5959AB", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2000.5, y = 240, label = "Ásia e Oceania:\nU$ 191,9 bi"),
            nudge_y = 1, color = "#5959AB", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025.2, y = 243.5, label = "Oriente Médio:\nU$ 243,5 bi"),
            nudge_y = 1, color = "#C76E00", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2000.5, y = 139, label = "Oriente Médio:\nU$ 91,9 bi"),
            nudge_y = 1, color = "#C76E00", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025.2, y = 1068.4, label = "Américas:\nU$ 1068,4 tri"),
            color = "#006400", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2000.5, y = 680, label = "Américas:\nU$ 620,3 bi"),
            color = "#006400", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025.2, y = 715, label = "Europa:\nU$ 680,7 bi"),
            nudge_y = 1, color = "#CC3299", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2000.5, y = 360, label = "Europa:\nU$ 319,1 bi"),
            nudge_y = 1, color = "#CC3299", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2025, y = 51.7, label = "África:\nU$ 51,7 bi"),
            nudge_y = 1, color = "#228B22", size = 5, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(x = 2000.5, y = 57, label = "África:\nU$ 24,9 bi"),
            nudge_y = 1, color = "#228B22", size = 5, hjust = 0.5, vjust = 0.5) +
  scale_x_continuous(breaks = 2000:2024) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = " ",
    x = " ",
    y = "Gastos militares (US$ bilhões)",
    caption = "Fonte: SIPRI Military Expenditure Database"
  )
