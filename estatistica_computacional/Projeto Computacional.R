library(missForest)
library(randomForest)
library(caret)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(mice) 

# Dados
dados <- data.frame(
  "Grupo_etario" = c("00 a 14", "15 a 19", "20 a 24", "25 a 29", "30 a 34", "35 a 39", "40 a +", "Branco/Ignorado"),
  "Nascidos_Vivos" = c(14242, 247748, 391051, 300533, 196747, 111648, 33708, 8, 138, 18909, 119832, 218164, 247681, 169242, 46471, 3, 2, 137, 578, 910, 1317, 1317, 732, 0, 7, 394, 3525, 8747, 11620, 10777, 4540, 0, 2799, 75534, 135747, 118035, 85996, 51157, 15676, 37, 269, 4557, 8280, 7755, 5746, 3698, 1074, 13),
  "Status_Civil" = c("Solteira", "Solteira", "Solteira", "Solteira", "Solteira", "Solteira", "Solteira", "Solteira", "Casada", "Casada", "Casada", "Casada", "Casada", "Casada", "Casada", "Casada", "Viúva", "Viúva", "Viúva", "Viúva", "Viúva", "Viúva", "Viúva", "Viúva", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "Separada_judicialmente/Divorciada", "União_estável", "União_estável", "União_estável", "União_estável", "União_estável", "União_estável", "União_estável", "União_estável", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado", "Branco/Ignorado")
)

# Número de simulações
n_sim <- 100

# Inicialize listas para armazenar os resultados
coeficientes_df <- data.frame()
mse_df <- data.frame()
r2_df <- data.frame()

# Função para calcular MSE
mse <- function(predicoes, verdade) {
  return(mean((verdade - predicoes)^2, na.rm = TRUE))
}

# Realize as simulações
for (i in 1:n_sim) {
  set.seed(i)
  
  # Gere os dados artificiais e introduza valores ausentes
  tamanho_banco_artificial <- 100
  linhas_amostradas <- sample(nrow(dados), tamanho_banco_artificial, replace = TRUE)
  dados_artificiais <- dados[linhas_amostradas,]
  
  frac_valores_ausentes <- 0.1
  num_valores_ausentes <- round(nrow(dados_artificiais) * frac_valores_ausentes)
  indices_ausentes <- sample(1:nrow(dados_artificiais), num_valores_ausentes)
  dados_artificiais$Nascidos_Vivos[indices_ausentes] <- NA
  
  # Aplique os métodos de imputação
  dados_artificiais_media <- dados_artificiais
  media <- mean(dados_artificiais$Nascidos_Vivos, na.rm = TRUE)
  dados_artificiais_media$Nascidos_Vivos[is.na(dados_artificiais_media$Nascidos_Vivos)] <- media
  
  dados_artificiais_mediana <- dados_artificiais
  mediana <- median(dados_artificiais$Nascidos_Vivos, na.rm = TRUE)
  dados_artificiais_mediana$Nascidos_Vivos[is.na(dados_artificiais_mediana$Nascidos_Vivos)] <- mediana
  
  dados_artificiais_moda <- dados_artificiais
  moda <- as.numeric(names(sort(table(dados_artificiais$Nascidos_Vivos), decreasing = TRUE)[1]))
  dados_artificiais_moda$Nascidos_Vivos[is.na(dados_artificiais_moda$Nascidos_Vivos)] <- moda
  
  dados_artificiais_knn <- dados_artificiais
  dados_artificiais_knn$Nascidos_Vivos <- as.numeric(dados_artificiais_knn$Nascidos_Vivos)
  dados_artificiais_knn$Grupo_etario <- as.factor(dados_artificiais_knn$Grupo_etario)
  dados_artificiais_knn$Status_Civil <- as.factor(dados_artificiais_knn$Status_Civil)
  imputed_data <- missForest(dados_artificiais_knn)
  dados_artificiais_knn <- imputed_data$ximp
  
  dados_artificiais_mice <- dados_artificiais
  dados_artificiais_mice$Grupo_etario <- as.factor(dados_artificiais_mice$Grupo_etario)
  dados_artificiais_mice$Status_Civil <- as.factor(dados_artificiais_mice$Status_Civil)
  imputed_data_mice <- mice(dados_artificiais_mice, m = 1, method = 'pmm', seed = i)
  dados_artificiais_mice <- complete(imputed_data_mice, 1)
  
  # Treinar o modelo bagging com os dados completos
  dados_completos <- na.omit(dados_artificiais)
  rf_modelo <- randomForest(Nascidos_Vivos ~ ., data = dados_completos, ntree = 500, mtry = 2)
  
  # Imputar os valores ausentes com o modelo bagging
  dados_artificiais_bagging <- dados_artificiais
  dados_artificiais_bagging$Nascidos_Vivos[is.na(dados_artificiais_bagging$Nascidos_Vivos)] <- predict(rf_modelo, newdata = dados_artificiais_bagging[is.na(dados_artificiais_bagging$Nascidos_Vivos),])
  
  # Definir o controle da validação cruzada
  controle_cv <- trainControl(method = "cv", number = 10) 
  
  # Ajustar os modelos aos dados imputados com validação cruzada
  modelo_media_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_media, method = "lm", trControl = controle_cv)
  modelo_mediana_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_mediana, method = "lm", trControl = controle_cv)
  modelo_moda_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_moda, method = "lm", trControl = controle_cv)
  modelo_knn_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_knn, method = "lm", trControl = controle_cv)
  modelo_bagging_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_bagging, method = "lm", trControl = controle_cv)
  modelo_mice_cv <- train(Nascidos_Vivos ~ Grupo_etario + Status_Civil, data = dados_artificiais_mice, method = "lm", trControl = controle_cv)
  
  # Calcular os coeficientes
  coeficientes <- data.frame(
    Simulacao = rep(i, 6), 
    Metodo = c("Media", "Mediana", "Moda", "KNN", "Bagging", "MICE"),
    Intercepto = c(coef(modelo)[1], coef(modelo_media_cv$finalModel)[1], coef(modelo_mediana_cv$finalModel)[1], coef(modelo_moda_cv$finalModel)[1], coef(modelo_knn_cv$finalModel)[1], coef(modelo_bagging_cv$finalModel)[1], coef(modelo_mice_cv$finalModel)[1]),
    Grupo_etario = c(coef(modelo)[2], coef(modelo_media_cv$finalModel)[2], coef(modelo_mediana_cv$finalModel)[2], coef(modelo_moda_cv$finalModel)[2], coef(modelo_knn_cv$finalModel)[2], coef(modelo_bagging_cv$finalModel)[2], coef(modelo_mice_cv$finalModel)[2]),
    Status_Civil = c(coef(modelo)[3], coef(modelo_media_cv$finalModel)[3], coef(modelo_mediana_cv$finalModel)[3], coef(modelo_moda_cv$finalModel)[3], coef(modelo_knn_cv$finalModel)[3], coef(modelo_bagging_cv$finalModel)[3], coef(modelo_mice_cv$finalModel)[3])
  )
  coeficientes_df <- rbind(coeficientes_df, coeficientes)
  
  # Calculo do MSE
  mse_modelos <- c(
    media = modelo_media_cv$results$RMSE[1]^2,
    mediana = modelo_mediana_cv$results$RMSE[1]^2,
    moda = modelo_moda_cv$results$RMSE[1]^2,
    knn = modelo_knn_cv$results$RMSE[1]^2,
    bagging = modelo_bagging_cv$results$RMSE[1]^2,
    mice = modelo_mice_cv$results$RMSE[1]^2
  )
  
  mse_df <- rbind(mse_df, data.frame(Simulacao = i, Metodo = names(mse_modelos), MSE = mse_modelos))
  
  # Calculo do R²
  r2_valores <- c(
    media = modelo_media_cv$results$Rsquared[1],
    mediana = modelo_mediana_cv$results$Rsquared[1],
    moda = modelo_moda_cv$results$Rsquared[1],
    knn = modelo_knn_cv$results$Rsquared[1],
    bagging = modelo_bagging_cv$results$Rsquared[1],
    mice = modelo_mice_cv$results$Rsquared[1]
  )
  
  r2_df <- rbind(r2_df, data.frame(Simulacao = i, Metodo = names(r2_valores), R2 = r2_valores))
}

# Exporta os data frames para visualização
write.csv(coeficientes_df, "coeficientes_resultados.csv", row.names = FALSE)
write.csv(mse_df, "mse_resultados.csv", row.names = FALSE)
write.csv(r2_df, "r2_resultados.csv", row.names = FALSE)

#----------------GRÁFICOS--------------------

# Gráfico dos coeficientes
coeficientes_df_long <- melt(coeficientes_df, id.vars = c("Simulacao", "Metodo"), 
                             measure.vars = c("Intercepto", "Grupo_etario", "Status_Civil"))

ggplot(coeficientes_df_long, aes(x = Metodo, y = value, color = variable)) +
  geom_boxplot() +
  labs(title = "Coeficientes dos Modelos por Método de Imputação",
       x = "Método de Imputação",
       y = "Coeficiente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico do MSE
ggplot(mse_df, aes(x = Metodo, y = MSE)) +
  geom_boxplot() +
  labs(title = "Erro Quadrático Médio (MSE) por Método de Imputação",
       x = "Método de Imputação",
       y = "MSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico do R²
ggplot(r2_df, aes(x = Metodo, y = R2)) +
  geom_boxplot() +
  labs(title = "R² dos Modelos por Método de Imputação",
       x = "Método de Imputação",
       y = "R²") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de Barras para MSE
ggplot(mse_df, aes(x = Metodo, y = MSE, fill = Metodo)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Média do Erro Quadrático Médio (MSE) por Método de Imputação",
       x = "Método de Imputação",
       y = "Média do MSE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de Barras para R²
ggplot(r2_df, aes(x = Metodo, y = R2, fill = Metodo)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Média do R² dos Modelos por Método de Imputação",
       x = "Método de Imputação",
       y = "Média do R²") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

# Calculando as métricas resumidas para MSE
mse_summary <- mse_df %>% 
  group_by(Metodo) %>% 
  summarize(
    MSE_medio = mean(MSE),
    Desvio_padrao_MSE = sd(MSE)
  )

# Calculando as métricas resumidas para R²
r2_summary <- r2_df %>% 
  group_by(Metodo) %>% 
  summarize(
    R2_medio = mean(R2),
    Desvio_padrao_R2 = sd(R2)
  )

# Combinando as tabelas de resumo
tabela_resumo <- left_join(mse_summary, r2_summary, by = "Metodo")

# Exibindo a tabela
print(tabela_resumo)

library(knitr)
kable(tabela_resumo, caption = "Resumo das Métricas por Método de Imputação",
      digits = 3, align = 'c')
