# SIPRI - Gastos Militares do Brasil (2000–2024)

Este repositório contém uma base de dados com os **gastos militares do Brasil**, extraída do SIPRI (Stockholm International Peace Research Institute), com foco em **transparência metodológica** e **reprodutibilidade**.

## 📁 Arquivos

- `dados-gastos-nominais.csv` — Valores anuais nominais (em R$) dos gastos militares no Brasil.
- `dados-deflacionados.csv` — Série ajustada para reais constantes de 2024.
- `metodologia-deflacao.docx` — Documento explicando a metodologia de deflação com IPCA.

## 📊 Fontes dos dados

- **Gastos militares**: SIPRI Military Expenditure Database – [https://www.sipri.org/databases](https://www.sipri.org/databases)
- **Inflação (IPCA)**: Instituto Brasileiro de Geografia e Estatística (IBGE) [https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?=&t=series-historicas](https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?=&t=series-historicas)

## ⚙️ Metodologia

A deflação foi realizada com base na série histórica anual do IPCA (Índice de Preços ao Consumidor Amplo).  
O ano-base adotado foi **2024**, com os seguintes passos:

1. Conversão do IPCA anual em fator de inflação (1 + IPCA / 100)
2. Cáluclo do fator acumulado
3. Cálculo do deflator
4. Aplicação do deflator aos valores nominais

Ver mais detalhes em `metodologia-deflacao.docx`.

## 🧠 Observações

- Apenas uma fração da base original do SIPRI foi utilizada, em conformidade com a política de uso não comercial (≤10% do total).

## 📄 Licença

Este repositório é público e os dados podem ser usados **para fins não comerciais**, com devida citação às fontes. Consulte a política do SIPRI para mais detalhes:  
**“SIPRI Military Expenditure Database 2025, https://www.sipri.org/databases/milex/sources-and-methods”**


