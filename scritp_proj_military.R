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

db_completo <- db_military_expenditure %>% 
  left_join(ipca, by = "ano") %>% 
  mutate(
    fator_inflacao = (ipca / 100) + 1,
    fator_acumulado = cumprod(fator_inflacao),
    deflator = fator_acumulado / fator_acumulado[ano == ano_base],
    gastos_reais = brazil_military_expenditure * deflator
  )

# Visualizar resultado
View(db_completo)


