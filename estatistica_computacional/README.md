# Projeto de Imputação de Dados em R

Os procedimentos citados neste tópico foram aplicados utilizando a linguagem de programação R com os dados obtidos pelos sites oficiais do Ministério da Saúde - DATASUS, 2023.

## Descrição
Este diretório contém códigos em R para análise e imputação de dados em um conjunto de dados sobre nascidos vivos. O projeto explora diferentes métodos de imputação e avalia seu desempenho utilizando validação cruzada, simulação de Monte Carlo e métricas estatísticas.

## Objetivo
O objetivo principal é comparar a eficácia de vários métodos de imputação de dados em um conjunto de dados com valores ausentes. Os métodos abordados incluem:
- Imputação pela média
- Imputação pela mediana
- Imputação pela moda
- Imputação com K-Nearest Neighbors (KNN)
- Imputação com Bagging
- Imputação com MICE (Multiple Imputation by Chained Equations)

## Métodos de Imputação
1. **Imputação pela Média**: Substitui os valores ausentes pela média dos valores presentes.
2. **Imputação pela Mediana**: Substitui os valores ausentes pela mediana dos valores presentes.
3. **Imputação pela Moda**: Substitui os valores ausentes pelo valor mais frequente.
4. **Imputação com KNN**: Utiliza o algoritmo K-Nearest Neighbors para prever valores ausentes com base em vizinhos semelhantes.
5. **Imputação com Bagging**: Usa um modelo de Random Forest para prever e preencher valores ausentes.
6. **Imputação com MICE**: Aplica um método de imputação por cadeias múltiplas para preencher os valores ausentes.

## Resultados
Os resultados são salvos em três arquivos CSV:
- `coeficientes_resultados.csv`: Coeficientes dos modelos ajustados por método de imputação.
- `mse_resultados.csv`: Erro Quadrático Médio (MSE) dos modelos por método de imputação.
- `r2_resultados.csv`: Coeficiente de Determinação (R²) dos modelos por método de imputação.

### Melhor Método Encontrado
Entre os métodos avaliados, o **KNN (K-Nearest Neighbors)** apresentou o melhor desempenho, com o maior valor de R² e o menor valor de MSE. Isso indica que o método KNN conseguiu fornecer previsões mais precisas e com menor erro comparado aos outros métodos de imputação.

## Gráficos
Os gráficos incluídos mostram a distribuição dos coeficientes dos modelos, MSE e R² por método de imputação. Os gráficos são gerados com `ggplot2` e incluem:
- Boxplots dos coeficientes
- Boxplots do MSE
- Boxplots do R²
- Gráficos de barras com a média do MSE e do R²
