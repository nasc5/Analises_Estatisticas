library(Stat2Data)
library(GGally)
library(ggcorrplot)
library(factoextra)
library(cluster)
library(dendextend)
library(mclust)
library(CCA)
library(CCP)
library(yacca)
library(MASS)
library(klaR)
library(psych)
library(ggplot2)

library(Stat2Data)
data("NFL2007Standings")

#-------PRIMEIRA UNIDADE-----------------------------------

library(Stat2Data)
data("NFL2007Standings")

#-------------------para todas as variáveis quantitativas--------------------

# Criar matrix de dados com nome nas dimencoes
x <- (NFL2007Standings[,c(4:10)]) # Exceto as variáveis referentes a Equipe, Conferencia e Divisão
dimnames(x) <- list(NFL2007Standings$Team,colnames(NFL2007Standings[,c(4:10)]))
dimnames(x) <- list(NFL2007Standings$Team,colnames(NFL2007Standings[,c(4:10)]))
colnames(x) <- c( "Vitorias", "Derrotas", "PercentualVitorias", "PontosFeitos", 
                  "PontosSofridos", "DiferencaPontos", "Touchdowns")

library(GGally)

# Criar a matriz de gráficos de dispersão
ggpairs(x)

# Padronizar as variáveis 
x <- scale(x)

correlacao <- cor(x)

library(ggcorrplot) 
ggcorrplot(correlacao, colors = c("#002951", "#eeeeee","#a0000a"), type = "lower") 


#------------------Para as variáveis da análise dos componentes principais----------------

x <- (NFL2007Standings[,c(5,7,8,10)]) # Exceto as variáveis referentes a Equipe, Conferencia e Divisão
dimnames(x) <- list(NFL2007Standings$Team,colnames(NFL2007Standings[,c(5,7,8,10)]))
dimnames(x) <- list(NFL2007Standings$Team,colnames(NFL2007Standings[,c(5,7,8,10)]))
colnames(x) <- c( "Derrotas", "PontosFeitos", "PontosSofridos", "Touchdowns")

library(GGally)

# Criar a matriz de gráficos de dispersão
ggpairs(x)

# Padronizar as variáveis 
x <- scale(x)

correlacao <- cor(x)

library(ggcorrplot) 
ggcorrplot(correlacao, colors = c("#002951", "#eeeeee","#a0000a"), type = "lower") 


# componentes principais 
comp.p <- prcomp(x, scale. = TRUE, center = TRUE, tol=0) 

print(comp.p) # Imprimir os resultados
summary(comp.p) # Mostrar um resumo dos resultados

# gráficos da análise de componentes principais

library(factoextra) 
fviz_eig(comp.p, addlabels = TRUE, barfill="#002951") 

fviz_pca_var(comp.p, col.var = "#002951",repel=TRUE) 

fviz_cos2(comp.p, choice = "var", axes = 1:2, fill="#002951")

fviz_pca_var(comp.p, col.var = "cos2", title = "Biplot - Componentes principais",
             gradient.cols = c("#002951", "#eeeeee","#a0000a"),
             repel = TRUE) 




#-------------------Para as variáveis da análise fatorial----------------------
data("NFL2007Standings")
x <- (NFL2007Standings[,c(5 ,6 ,7 ,8 ,10)]) 
dimnames(x) <- list(NFL2007Standings$Team,colnames(NFL2007Standings[,c(5 ,6 ,7 ,8 ,10)]))
colnames(x) <- c( "Derrotas", "PercentualVitorias", "PontosFeitos", 
                  "PontosSofridos", "Touchdowns")
colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", "Vitorias", 
                                "Derrotas", "PercentualVitorias", "PontosFeitos", 
                                "PontosSofridos", "DiferencaPontos", "Touchdowns")

library(GGally)

# Criar a matriz de gráficos de dispersão
ggpairs(x)

# Padronizar as variáveis 
x <- scale(x)

correlacao <- cor(x)

library(ggcorrplot) 
ggcorrplot(correlacao, colors = c("#002951", "#eeeeee","#a0000a"), type = "lower") 




#  ANÁLISE FATORIAL



a.fatorial <- factanal(NFL2007Standings[,c(5,6,7,8,10)],factors = 1);print(a.fatorial)
a.fatorial2 <- factanal(NFL2007Standings[,c(5,6,7,8,10)],factors = 2);print(a.fatorial2)

af.reg <-  factanal(NFL2007Standings[,c(5,6,7,8,10)],factors = 2, scores="regression")
# Extraindo escores e cargas fatoriais
scores <- as.data.frame(af.reg$scores)
loadings <- as.data.frame(af.reg$loadings[,1:2])

# Nomeando as colunas dos escores para corresponder aos fatores
colnames(scores) <- c("Factor1", "Factor2")
scores$Equipe <- NFL2007Standings$Equipe

# Criação de um data frame para as cargas fatoriais com nomes das variáveis
loadings$Variable <- rownames(loadings)

# Biplot com ggplot2
ggplot(scores, aes(x = Factor1, y = Factor2, color = Equipe)) +
  geom_point() +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = Factor1, yend = Factor2),
               arrow = arrow(length = unit(0.2, "cm")), color = '#444444') +
  geom_text(data = loadings, aes(x = Factor1, y = Factor2, label = Variable),
            color = "#a0000a", hjust = -0.2, size = 4) +
  theme_minimal() +
  labs(title = "Biplot - Análise Fatorial",
       x = "Fator 1", y = "Fator 2")




data("NFL2007Standings")
NFL2007Standings$Classificacao <- 1:nrow(NFL2007Standings)
colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", "Vitorias", 
                                "Derrotas", "PercentualVitorias", "PontosFeitos", 
                                "PontosSofridos", "DiferencaPontos", "Touchdowns", "Classificacao")

# Ajustando a análise fatorial com escores de regressão
af.reg <- factanal(NFL2007Standings[,c(5,6,7,8,10)], factors = 2, scores = "regression")

# Extraindo escores e cargas fatoriais
scores <- as.data.frame(af.reg$scores)
loadings <- as.data.frame(af.reg$loadings[, 1:2])

# Nomeando as colunas dos escores para corresponder aos fatores
colnames(scores) <- c("Factor1", "Factor2")
scores$Classificacao <- NFL2007Standings$Classificacao

# Criando um data frame para as cargas fatoriais com nomes das variáveis
loadings$Variable <- rownames(loadings)

# Criando o biplot com ggplot2
ggplot(scores, aes(x = Factor1, y = Factor2)) +
  geom_point(aes(color = Classificacao)) +  # Adiciona uma cor baseada na classificação
  geom_text(aes(label = Classificacao), color = "#002951", size = 4, nudge_x = 0.03, nudge_y = 0.03) +  # Ajusta o tamanho e o posicionamento dos labels
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = Factor1, yend = Factor2),
               arrow = arrow(length = unit(0.2, "cm")), color = '#444444') +
  geom_text(data = loadings, aes(x = Factor1, y = Factor2, label = Variable),
            color = "#a0000a", hjust = -0.2, size = 4) +  # Ajusta o tamanho dos labels das cargas fatoriais
  scale_color_gradient(low = "green", high = "red") +  # Define o gradiente de cores
  theme_minimal() +
  labs(title = "Biplot - Análise Fatorial",
       x = "Fator 1", y = "Fator 2", color = "Classificação")


#-------SEGUNDA UNIDADE------------------------------------

library(Stat2Data)
data("NFL2007Standings")
colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", "Vitorias", 
                                "Derrotas", "PercentualVitorias", "PontosFeitos", 
                                "PontosSofridos", "DiferencaPontos", "Touchdowns")
# Renomeando as observações
NFL2007Standings$Equipe <- paste0(seq_along(NFL2007Standings$Equipe))


#padronizando
xi <-data.frame(scale(NFL2007Standings[,c(4 ,7 ,8 ,10)]), Equipe = NFL2007Standings[,1])

#-------------------Classificação Hierárquica----------------
library(cluster)    
library(factoextra) 
library(dendextend)


# MATRIZ DE DISSIMILARIDADE
Mdist <- dist(xi[,1:4], method = "euclidean")

# CLASSIFICACAO USANDO hclust
CH1 <- hclust(Mdist)
plot(CH1, main=" ",labels = substr(xi$Equipe, 1,2),ylab = NULL, ann =
       FALSE)
rect.hclust(CH1, k = 4, border = c("#002951","#005129","#a0000a"))

corte = cutree(CH1 , k = 4)
mdados <- as.matrix(xi[,1:4])
fviz_cluster(list(data = mdados, cluster = corte ,show_labels = FALSE), main = "")   






# SELECAO DE METODOS
m <- c( "average", "single", "complete", "ward")
ac.m <- data.frame(metodo=m,Criterio=rep(0,length(m)))
for(i in 1:length(m)){
  ac.m[i,2] <- agnes(xi, method = m[i])$ac
}
print(ac.m)


# ANALISE DE CLUSTER PELO METODO WARD

CHward <- agnes(xi, method = "ward")
CHward <- as.hclust(CHward)
plot(CHward, main=" ",labels = substr(xi$Equipe,1,2),ylab = NULL, ann
     = FALSE)
rect.hclust(CHward, k = 4, border = c("#002951","#005129","#a0000a"))
corte4 <- cutree(CHward , k = 4)

# Número de equipes por grupo
table(corte4)

# Tabela de contigencia
table(data.frame(Equipe=xi$Equipe,Classes=corte4))


# Biplot
mdados <- as.matrix(xi[,1:4])
fviz_cluster(list(data = mdados, cluster = corte4,show_labels = FALSE))

#-------------------Não-Hierárquica (K-means)------------------------------------

(km <- kmeans(xi[,-5],centers = 4))

table(data.frame(Equipe=xi$Equipe,Classes=km$cluster))


fviz_cluster(list(data = xi[,1:4], cluster = km$cluster,show_labels = FALSE))+
  scale_colour_manual(values = c("#002951","#512900","#005129","#a0000a"))+
  scale_fill_manual(values = c("#002951","#512900","#005129","#a0000a"))


#-------------------Agrupamento Paramétrico--------------------------

library(mclust)

BIC <- mclustBIC(xi[,-5])
plot(BIC)
summary(BIC)

#mod1
mod1 <- Mclust(xi[,-5], x = BIC)

gg <- mod1$classification
table(data.frame(grupos=gg,equipe=xi$Equipe))
plot(mod1, what = "classification",col=c("#002951"))
mdados <- as.matrix(xi[,1:4])
fviz_cluster(list(data = mdados, cluster = gg,show_labels = F), main = "")


#mod2
BIC2 <- mclustBIC(xi[,-5], G = 4)
summary(BIC2)

mod2 <- Mclust(xi[,-5], x = BIC2)
summary(mod2)

gg <- mod2$classification
table(data.frame(grupos=gg,equipe=xi$Equipe))
plot(mod2, what = "classification",col=c("#002951"))
mdados <- as.matrix(xi[,1:4])
fviz_cluster(list(data = mdados, cluster = gg,show_labels = F), main = "")

#mod3
mod3 <- Mclust(xi[,-5], G=4, modelNames = "VEE")
summary(mod3)
gg <- mod3$classification
plot(mod3, what = "classification", col=c("#002951","#005129", "#a0000a"), main = "")
mdados <- as.matrix(xi[,1:4])
fviz_cluster(list(data = mdados, cluster = gg,show_labels = FALSE), main = "")




#-------TERCEIRA UNIDADE-----------------------------------------

library(Stat2Data)
data("NFL2007Standings")

colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", 
                                "Vitorias", "Derrotas", "PercentualVitorias", 
                                "PontosFeitos", "PontosSofridos", 
                                "DiferencaPontos", "Touchdowns")

X = (NFL2007Standings[,c(7,8,10)]) # PontosFeitos, Touchdowns, PontosSofridos
Y = (NFL2007Standings[,c(5,6,9)]) #  Derrotas, PercentualVitorias, DiferencaPontos


x1 = NFL2007Standings[,c(5:10)]

library(CCA)
library(CCP)
library(yacca) 



# Calcular correlacao
correlacao <- cor(x1)

# Grafico Correlacao
library(ggcorrplot)
ggcorrplot(correlacao,
           colors = c("#002951", "#eeeeee","#a0000a"),
           type = "lower",
           lab = TRUE)
library(GGally)
ggpairs(x1)


#-------------------Análise Correlação Canônica---------------

library(Stat2Data)
data("NFL2007Standings")

colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", 
                                "Vitorias", "Derrotas", "PercentualVitorias", 
                                "PontosFeitos", "PontosSofridos", 
                                "DiferencaPontos", "Touchdowns")

X = (NFL2007Standings[,c(7,8,10)]) # PontosFeitos, Touchdowns, PontosSofridos
Y = (NFL2007Standings[,c(5,6,9)]) #  Derrotas, PercentualVitorias, DiferencaPontos


library(CCA)
library(CCP)
library(yacca)
cca.fit <- cca(X,Y)
summary(cca.fit)
plot(cca.fit)
 
conflicted::conflicts_prefer(CCA::cc)
res.cc=cc(X,Y)
plt.cc(res.cc,type = "v", var.label = TRUE)
plt.cc(res.cc,type = "i", var.label = TRUE)
rho <- res.cc$cor
nn <- dim(X)[1]; pp <- dim(X)[2]; qq <- dim(Y)[2]
# "Wilks" (default), "Hotelling", "Pillai", or "Roy".
assintotico <- p.asym(rho, N=nn, p=pp, q=qq)
round(assintotico$p.value,4)

plt.asym(assintotico,rhostart=1)
plt.asym(assintotico,rhostart=2)
plt.asym(assintotico,rhostart=3)

permutacao <- p.perm(X, Y, type = "Hotelling")
plt.perm(permutacao)


#-------------------Análise discriminante------------------------

library(MASS)
library(klaR)
library(psych)

library(Stat2Data)
data("NFL2007Standings")
colnames(NFL2007Standings) <- c("Equipe", "Conferencia", "Divisao", "Vitorias", 
                                "Derrotas", "PercentualVitorias", "PontosFeitos", 
                                "PontosSofridos", "DiferencaPontos", "Touchdowns")

# Adicionando uma nova coluna para a classificação
NFL2007Standings$Classificacao <- c(rep("Superior", each = 10),
                                    rep("Mediano", each = 11),
                                    rep("Pior", each = 11))

x <- (NFL2007Standings[,c(5,7,8,10,11)]) 




pairs.panels(x[1:4],hist.col = "#a0000a",
             gap = 0,
             bg = ifelse(x$Classificacao=="Superior","#1122aa", "#808000"),
             pch = ifelse(x$Classificacao=="Pior",21,22))

# Amostras de Treino
set.seed(123)
treinar <- sample(1:32, 25)
table(x$Classificacao[treinar])
Tdados <- x[treinar,]
Tdados$Classificacao <- as.factor(Tdados$Classificacao)

# Analise Discriminante Linear 
linear <- lda(Classificacao ~ ., x, prior = c(1,1,1)/3, subset = treinar,CV=F)
tab.lda <- predict(linear, x[-treinar, ])$class
table(x$Classificacao[-treinar],tab.lda)
linear


linear <- lda(Classificacao ~ ., x, prior = c(1,1,1)/3, subset = treinar,CV=T)
pairs.panels(Tdados[1:4],hist.col = "#a0000a",
             gap = 0,
             bg = ifelse(linear$class=="Superior","#1199ff", "#cccccc"),
             pch = ifelse(linear$class=="Pior",21,22))
partimat(Classificacao~., data = Tdados, method = "lda",
         image.colors=c("#1199ff", "#cccccc", "#808000"))


quadratico <- qda(Classificacao ~ ., x, prior = c(1,1,1)/3, subset = treinar,CV=F)
tab.qda <- predict(quadratico, x[-treinar, ])$class
table(x$Classificacao[-treinar],tab.qda)
quadratico

quadratico <- qda(Classificacao ~ ., x, prior = c(1,1,1)/3, subset = treinar,CV=T)
pairs.panels(Tdados[1:4],hist.col = "#a0000a",
             gap = 0,
             bg = ifelse(quadratico$class=="Superior","#1199ff", "#cccccc"),
             pch = ifelse(quadratico$class=="Pior",21,22))
partimat(Classificacao~., data = Tdados, method = "qda",
         image.colors=c("#1199ff", "#cccccc", "#808000"))









