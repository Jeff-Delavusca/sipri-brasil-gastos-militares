# ================================
# SIPRI - Gastos Militares do Brasil (2000-2024)
# Deflacionamento com base no IPCA
# Autor: Jeff Delavusca
# ================================


# -------------------------------
# 1. Carregando pacotes
# -------------------------------

# Verifica se o pacote 'abaixo'sidrar' está instalado, caso não esteja, instala automaticamente
if (!require("sidrar")) install.packages("sidrar")

# Carrega os pacotes necessários
library(sidrar)
library(tidyverse)


# -------------------------------
# 2. Carregando dataset do GitHub (Brasil)
# -------------------------------

# Define a URL com os dados de gastos militares nominais
url_data <- "https://raw.githubusercontent.com/Jeff-Delavusca/sipri-brasil-gastos-militares/refs/heads/main/dados-gastos-nominais.csv"

# Lê o arquivo CSV da URL e armazena no dataframe 'db_expenditure'
db_expenditure <- read.csv2(url_data)

# Remove a terceira coluna (em branco) do dataframe
db_expenditure <- db_expenditure[, -3]  


# -------------------------------
# 3. IPCA anual (2000–2024)
# -------------------------------

# Cria um dataframe com os dados anuais do IPCA (inflação) de 2000 a 2024
ipca <- data.frame(
  year = 2000:2024,
  ipca = c(5.97, 7.67, 12.53, 9.3, 7.6, 5.69, 3.14, 4.46, 5.9, 4.31,
           5.91, 6.5, 5.84, 5.91, 6.41, 10.67, 6.29, 2.95, 3.75, 4.31,
           4.52, 10.06, 5.79, 4.62, 4.83)
)


# -------------------------------
# 4. Juntando bases e deflacionando os dados
# -------------------------------

# Define automaticamente o maior ano disponível na base da inflação como ano base
year_base <- max(ipca$year, na.rm = TRUE)

# Define constande para conversão de valores em bilhões
bilions <- 1e9

# Junta as bases de gastos nominais e inflação, calculando os valores deflacionados (reais)
db_complete <- db_expenditure %>%
  left_join(ipca, by = "year") %>%                                     # Une as bases "db_expenditure' e 'ipca' pela coluna 'year'
  mutate(
    fator_inflacao = (ipca / 100) + 1,                                 # Calcula o fator de inflação anual
    fator_acumulado = cumprod(fator_inflacao),                         # Calcula o fator acumulado de inflação ao longo dos anos
    deflator = fator_acumulado / fator_acumulado[year == year_base],   # Calcula o deflator com base no ano-base
    gastos_reais = brazil_military_expenditure * deflator,             # Aplica o deflator aos gastos nominais
    gastos_reais_reduzido = gastos_reais / bilions                     # Converte valores para bilhões de reais 
  )

View(db_complete)


# -------------------------------
# 5. Cálculo da variação acumulada entre 2000 e 2023
# -------------------------------

# Define automaticamente o menor ano disponível na base de gastos militares
start_year <- min(db_complete$year, na.rm = TRUE)

# Define manualmente o ano de 2023 como ano final, pois é o último com dados deflacionados
end_year <- 2023

# Filtra o primeiro ano da base e coleta o gasto militar real do respectivo ano
start_value <- db_complete %>%
  filter(year == start_year) %>%
  pull(gastos_reais)

# Filtra o ano final da base e coleta o gasto militar real do respectivo ano
end_value <- db_complete %>%
  filter(year == end_year) %>%
  pull(gastos_reais)

# Calcula a variação percentual acumulada entre os dois anos selecionados
percentage_chage <- ((end_value - start_value) / start_value) * 100

# IExibe no console os valores inicial e final dos gastos reais, além da variação percentual 
cat(paste0("Variação real dos gastos militares entre ",
           start_value, " e ", end_value, ": ",
           round(percentage_chage, 2), "%\n"))


# -------------------------------
# 6. Gráfico dos gastos reais com anotações
# -------------------------------

# Cria gráfico de linha com os gastos militares reais ao longo do tempo
ggplot2::ggplot() +
  geom_line(data = db_complete,
            aes(x = year, y = gastos_reais_reduzido),
            size = 1,
            color = "firebrick") +  # Linha vermelha principal
  theme_bw() +  # Aplica tema com fundo branco
  
  
  # Adiciona anotações de texto para destacar valores selecionados
  geom_text(aes(x = 2002, y = 19, label = "Gastos em 2000:\nR$ 4,92 Bi"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  
  
  geom_text(aes(x = 2022, y = 60, label = "Gastos em 2023:\nR$ 103,81 Bi"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  
  
  geom_text(aes(x = 2012, y = 57, label = "Crescimento de\n+ de 2000% entre\n2000 e 2023"),
            size = 3.5, hjust = 0.5, vjust = 0.5, fontface = "bold") +
  
  # Remove grades e bordas do gráfico para o visual mais limpo
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  
  # Define rótulos dos eixos, títulos e fonte dos dados
  labs(
    x = "Ano",
    y = "Gastos Militares (R$ bilhões)",
    title = "Gastos Militares do Brasil (valores reais, base 2024)",
    caption = "Fonte: SIPRI e IPCA/IBGE"
  )

# Salva o gráfico como imagem PNG
ggsave("grafico_gastos.png", width = 7, height = 4, dpi = 300)


# -------------------------------
# 7. Carregando base regional para análises comparativas
# -------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

url_data2 <- "https://raw.githubusercontent.com/Jeff-Delavusca/sipri-brasil-gastos-militares/refs/heads/main/military_expenditure_by_region.csv"

# Usar o read.csv2 pois os dados estão separados por ';' (padrão europeu)
expenditure_by_region <- read.csv2(url_data2)

# Definir ano base para cálculo do crescimento acumulado 
start_year <- 2000

# Valores base para cada região no ano base
base_value <- expenditure_by_region %>%
  filter(Year == start_year) %>%
  select(-Year)

# Calcular variação acumulada (%) para cada região, comparando com o ano base
acm_by_region <- expenditure_by_region %>%
  mutate(
    África = ((Africa / base_value$Africa) - 1) * 100,
    `Ásia e Oceania` = ((Asia...Oceania / base_value$Asia...Oceania) - 1) * 100,
    Europa = ((Europe / base_value$Europe) - 1) * 100,
    `Oriente Médio` = ((Middle.East / base_value$Middle.East) - 1) * 100,
    Américas = ((Americas / base_value$Americas) - 1) * 100,
  ) %>% 
  select(Year,África, `Ásia e Oceania`, Europa, `Oriente Médio`, Américas) 

# Converter para formato longo para facilitar o gráfico
acm_by_region_long <- acm_by_region %>%
  pivot_longer(-Year, names_to = "Região", values_to = "Valor")


# Ordenar fatores da variável Região baseado no valor do último ano para o gráfico
ordem_regioes <- acm_by_region_long %>%
  filter(Year == max(Year)) %>%
  arrange(Valor) %>%
  pull(Região)

acm_by_region_long <- acm_by_region_long %>%
  mutate(Região = factor(Região, levels = ordem_regioes))


# Gráfico da variação acumulada dos gastos militares (%)
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


# Gráfico dos gastos militares reais por região

# Renomear colunas para português para facilitar leitura
expenditure_by_region_long <- expenditure_by_region %>% 
    rename(
      `Ásia e Oceania` = Asia...Oceania,
      `Oriente Média` = Middle.East) %>% 
  pivot_longer(
    -Year,
    names_to = "Região",
    values_to = "Valor"
    )


# Ordenar fatores para legenda e cores baseando-se no último ano
ordem_regioes <- expenditure_by_region_long %>%
  filter(Year == max(Year)) %>%
  arrange(Valor) %>%
  pull(Região)


expenditure_by_region_long <- expenditure_by_region_long  %>%
  mutate(Região = factor(Região, levels = ordem_regioes))


# Gráfico dos gastos nominais (US$ bilhões)
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
