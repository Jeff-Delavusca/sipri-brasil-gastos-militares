# ================================
# SIPRI - Gastos Militares do Brasil (2000-2024)
# Deflacionamento com base no IPCA
# Autor: Jeff Delavusca
# ================================

# -------------------------------
# 1. Carregando pacotes
# -------------------------------

# Instalação condicional do pacote 'sidrar'
if (!require("sidrar")) install.packages("sidrar")
library(sidrar)

library(tidyverse)

# -------------------------------
# 2. Carregando dataset do GitHub
# -------------------------------

url_dados <- "https://raw.githubusercontent.com/Jeff-Delavusca/sipri-brasil-gastos-militares/refs/heads/main/dados-gastos-nominais.csv"

db_military_expenditure <- read.csv2(url_dados)

# Tratando dados
db_military_expenditure <- db_military_expenditure[, -3]                       # Remove coluna desnecessária
db_military_expenditure <- rename(db_military_expenditure, ano = year)        # Renomeia coluna "year" para "ano"

# -------------------------------
# 3. IPCA anual (2000–2024)
# -------------------------------

# Obs: SIDRA só tem dados a partir de 2020, por isso inserido manualmente
ipca <- data.frame(
  ano = 2000:2024,
  ipca = c(5.97, 7.67, 12.53, 9.3, 7.6, 5.69, 3.14, 4.46, 5.9, 4.31,
           5.91, 6.5, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31,
           4.52, 10.06, 5.79, 4.62, 4.83)
)

# -------------------------------
# 4. Juntando bases e deflacionando
# -------------------------------

ano_base <- 2024  # Ano base para valores reais
bilhoes <- 1000000000

db_completo <- db_military_expenditure %>% 
  left_join(ipca, by = "ano") %>% 
  mutate(
    fator_inflacao = (ipca / 100) + 1,
    fator_acumulado = cumprod(fator_inflacao),
    deflator = fator_acumulado / fator_acumulado[ano == ano_base],
    gastos_reais = brazil_military_expenditure * deflator,
    gastos_reais_reduzido = gastos_reais/bilhoes
  )

# Visualizar resultado
View(db_completo)


# Cálculo da variação real dos gastos entre dois anos
ano_inicial <- 2000
ano_final <- 2023

valor_inicial <- db_completo %>% 
  filter(ano == ano_inicial) %>% 
  pull(gastos_reais)

valor_final <- db_completo %>% 
  filter(ano == ano_final) %>% 
  pull(gastos_reais)

variacao_percentual <- ((valor_final - valor_inicial) / valor_inicial) * 100


# Exibindo resultado
cat(
  paste0("Variação real dos gastos militares entre ",
         ano_inicial, " e ", ano_final, ": ",
         round(variacao_percentual, 2), "%\n")
)


# Criando gráfico dos gastos militares reais com anotações e tema limpo sem grades
ggplot2::ggplot() +
  geom_line(data = db_completo, 
            aes(x = ano, y = gastos_reais_reduzido), 
            size = 1,
            color = "firebrick") + 
  theme_bw() +
  geom_text(aes(x = 2002, y = 19, label = "Gastos em 2000:\nR$ 4,92 Bi"),
            size = 3.5,
            hjust = 0.5,
            vjust = 0.5,
            fontface = "bold") +
  geom_text(aes(x = 2022, y = 60, label = "Gastos em 2023:\nR$ 103,81 Bi"),
            size = 3.5,
            hjust = 0.5,
            vjust = 0.5,
            fontface = "bold") +
  geom_text(aes(x = 2012, y = 57, label = "Crescimento de\n+ de 2000% entre\n2000 e 2023"),
            size = 3.5,
            hjust = 0.5,
            vjust = 0.5,
            fontface = "bold") +
  theme (
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),        # Remove linhas dos eixos
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "Year",
    y = "Military Expenditure",
    title = "Military Expenditure Brazil (in Billions of BRL)",
    caption = "Source: SIPRI and IPCA/IBGE"
    
  )

ggsave("grafico_gastos.png", width = 7, height = 4, dpi = 300)








