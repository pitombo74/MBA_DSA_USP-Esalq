################################################################################
##                TCC MBA DATA SCIENCE & ANALYTICS USP ESALQ                  ##
##                        SCRIPTS DE APOIO R                                  ##
##                        By: Marcelo Pitombo                                 ##
################################################################################


####################### INSTALACAO DE PACOTES ##################################



install.packages('tensorflow')
library(tensorflow)
install_tensorflow()
library(tensorflow)
tf$constant("Hello")
install.packages('keras')
library('keras')
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

pacotes <- c('titanic',
             'tidyverse',
             'rpart',
             'rpart.plot',
             'gtools',
             'Rmisc',
             'scales',
             'caret',
             'dplyr',
             'DMwR',
             'readxl',
             'knitr',
             'gtools',
             'skimr',
             'kableExtra',
             'flextable',
             'gtsummary',
             'neuralnet',
             'gamlss',
             'gamlss.add',
             'randomForest')      

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library('DMwR')
library('readxl')
library('gtools')
library('caret')
library('skimr')
library('kableExtra')
library('knitr')
library('dplyr')
library ('flextable')
library ('neuralnet')
library ('gamlss')
library ('gamlss.add')
library ('gtsummary')
library('fastDummies')
library('randomForest')



################################################################################
##    CARREGAMENTO DA BASE DE DADOS - PEQUISA OD METRO 2017                   ##
################################################################################

#Importa Base Greal
OD2017 <- read_excel("C:/Users/Marcelo/OneDrive/MBA_DataScience/TCC/Pesq OD Metro 2017/OD-2017/Banco de Dados-OD2017/OD_2017_v1.xlsx")

#Importa Base de Zonas
ZONAS <- read_excel("C:/Users/Marcelo/OneDrive/MBA_DataScience/TCC/Pesq OD Metro 2017/OD-2017/Banco de Dados-OD2017/ZONAS.xlsx")

#Seleciona vari?veis de interesse para novo dataframe

ODbase <-  OD2017[, c(1,42,49,50,52,53,83,103,118,119,127)]

# 01  ZONA DOMICILIO
# 42  RENDA_FA
# 49  IDADE
# 50  SEXO
# 52  GRAU_INS
# 53  CD_ATIVI
# 83  ZONA_O
# 103 MOTIVO_O
# 118 DURACAO
# 119 MODOPRIN
# 127 DISTANCIA

#Reclassifica a vari?vel dependente (MODOPRIN)

ODbase <- ODbase %>%
  mutate(ModoNovo = if_else(MODOPRIN %in% 01:08, "Coletivo motorizado",if_else(MODOPRIN %in% 09:14, "Individual motorizado","N?o motorizado")))

# 01  Coletivo motorizado
# 02  Individual motorizado
# 03  N?o motorizado

#Reclassifica a vari?vel ZonaNovo segundo o arquivo ZONA.xls

ODbase <- merge(ODbase, ZONAS, by="ZONA")

#    1	GRANDE S?O PAULO
#    2	S?O PAULO - CENTRAL
#    3	S?O PAULO - LESTE
#    4	S?O PAULO - NORDESTE
#    5	S?O PAULO - NOROESTE
#    6	S?O PAULO - NORTE
#    7	S?O PAULO - OESTE
#    8	S?O PAULO - SUDESTE
#    9	S?O PAULO - SUDOESTE
#   10	S?O PAULO - SUL

#Transforma vari?veis para tipo categ?rico/fatorial

ODbase$ModoNovo <- as.factor(ODbase$ModoNovo)
ODbase$ZONA_O <- as.factor(ODbase$ZONA_O)
ODbase$MOTIVO_O <- as.factor(ODbase$MOTIVO_O)
ODbase$MODOPRIN <- as.factor(ODbase$MODOPRIN)
ODbase$SEXO <- as.factor(ODbase$SEXO)
ODbase$CD_ATIVI <- as.factor(ODbase$CD_ATIVI)
ODbase$GRAU_INS <- as.factor(ODbase$GRAU_INS)
ODbase$ZonaNovo <- as.factor(ODbase$ZonaNovo)

#Verifica Balanceamento da Variavel Modo Principal

prop.table(table(ODbase$ModoNovo))

#Categoriazando IDADE, RENDA_FA, DISTANCIA e DURACAO em uma nova base ODBase2

#Transfere o conte?do da base original para a nova base ODbase2
ODbase2 <- ODbase
#Remove casos ausentes (NA)
ODbase2 <- na.omit(ODbase2)

ODbase2 <- ODbase2 %>%
  mutate(RENDA_FA = quantcut (ODbase2$RENDA_FA,q=4,na.rm = TRUE, labels = FALSE))

ODbase2 <- ODbase2 %>%
  mutate(IDADE = if_else(IDADE %in% 01:17, "1",if_else(IDADE %in% 18:25, "2", if_else(IDADE %in% 26:40, "3","4"))))

ODbase2 <- ODbase2 %>%
  mutate(DISTANCIA = quantcut (ODbase2$DISTANCIA,q=4,na.rm = TRUE, labels = FALSE))

ODbase2 <- ODbase2 %>%
  mutate(DURACAO = quantcut (ODbase2$DURACAO,q=4,na.rm = TRUE, labels = FALSE))

#IDADE	
#1	01-17 anos
#2	18-25 anos
#3	25-39 anos
#4	>40 anos

#RENDA_FA
#1	< R$2400
#2	R$ 2400 - R$ 3778
#3	R$ 3778 - R$ 6270
#4	> R$ 6270

#DISTANCIA	
#1	< 664 m
#2	664 m - 2152 m
#3	2152 m - 6539 m
#4	> 6539 m

#DURACAO	
#1	< 10 Min
#2	10 - 20 Min
#3	20 - 40 Min
#4	> 40 Min

#Converte vari?veis rec?m categorizadas para FACTOR
ODbase2$IDADE <- as.factor(ODbase2$IDADE)
ODbase2$RENDA_FA <- as.factor(ODbase2$RENDA_FA)
ODbase2$DISTANCIA <- as.factor(ODbase2$DISTANCIA)
ODbase2$DURACAO <- as.factor(ODbase2$DURACAO)

################################################################################
##  SEGMENTA A BASE DE DADOS EM TREINO E VALIDACAO MANTENDO PROPORCIONALIADE  ##          
################################################################################                        

prop.table(table(ODbase2$ModoNovo)) #propor??o base de trabalho (modificada/categorizada)

set.seed(200)
trainIndex <- createDataPartition(ODbase2$ModoNovo, p = 0.8,
                                  list = FALSE,
                                  times = 1)

Train <- ODbase2[ trainIndex,]
Valid <- ODbase2[-trainIndex,]

prop.table(table(Train$ModoNovo)) #propor??o treinamento
prop.table(table(Valid$ModoNovo)) #propor??o teste


################################################################################
##                            ARVORES DE DECISAO                              ##
################################################################################

#Arvore Trainamento

arvore_train <- rpart(ModoNovo ~ IDADE + SEXO + RENDA_FA + CD_ATIVI + GRAU_INS + MOTIVO_O + DISTANCIA + DURACAO + ZonaNovo,
                      data=Train,
                      parms = list(split = 'information'),
                      method='class',
                      na.action = na.omit)

rpart.plot::rpart.plot(arvore_train)

#Predizendo a base de teste com a arvore de treinamento e avalaindo acur?cia do Modelo na base de teste

pred_arvore_teste <- predict(arvore_train, Valid, type = "class")

confusionMatrix(table(pred_arvore_teste, Valid$ModoNovo))

#Podando a ?rvore de treinamento

printcp(arvore_train)
plotcp(arvore_train)

rpart.plot::rpart.plot(prune(arvore_train,cp=0.068))
arvore_train_pod <- prune(arvore_train,cp=0.068)
arvore_train_pod

#Predizendo a base de teste com a arvore de treinamento podada e avalaindo acur?cia do Modelo na base de teste

pred_arvore_pod <- predict(arvore_train_pod, Valid, type = "class")

confusionMatrix(table(pred_arvore_pod, Valid$ModoNovo))

################################################################################
##                                RANDOM FOREST                               ##
################################################################################

# Treinar a Random Forest

# Semente aleat?ria para buscar a reprodutibilidade
set.seed(2360873)

#RF modelo com Zona

# Rodar o algoritmo rf treino

arvore_rf_treino <- randomForest::randomForest(
  ModoNovo ~ IDADE + SEXO + RENDA_FA + CD_ATIVI + GRAU_INS + MOTIVO_O + DISTANCIA + DURACAO + ZonaNovo, 
  data = Train, 
  ntree = 50,
  mtry = 3, 
  importance = T,
  na.action = na.omit)

#Predizendo RF

pred_rf_teste <- predict(arvore_rf_treino, Valid)

confusionMatrix(table(pred_rf_teste, Valid$ModoNovo))

# Analisando as vari?veis importantes do modelo

varImp(arvore_rf_treino)

plot(varImp(arvore_rf_treino))

varImpPlot(arvore_rf_treino)

################################################################################
##                       REDES NEURAIS (KERAS/TENSORFLOW)                     ##
################################################################################

Train_Keras <-  dummy_cols(Train, select_columns = c("ModoNovo","IDADE","SEXO","RENDA_FA", "CD_ATIVI","GRAU_INS","MOTIVO_O","DISTANCIA","DURACAO","ZonaNovo"))
Valid_Keras <-  dummy_cols(Valid, select_columns = c("ModoNovo","IDADE","SEXO","RENDA_FA", "CD_ATIVI","GRAU_INS","MOTIVO_O","DISTANCIA","DURACAO","ZonaNovo"))

names(Train_Keras)[14] <- "ModoNovo_CM"
names(Train_Keras)[15] <- "ModoNovo_IM"
names(Train_Keras)[16] <- "ModoNovo_NM"
names(Valid_Keras)[14] <- "ModoNovo_CM"
names(Valid_Keras)[15] <- "ModoNovo_IM"
names(Valid_Keras)[16] <- "ModoNovo_NM"

### Base de Treino Keras

entrada <- data.frame (Train_Keras$DURACAO_1, Train_Keras$DURACAO_2, Train_Keras$DURACAO_3, Train_Keras$DURACAO_4, Train_Keras$DISTANCIA_1, Train_Keras$DISTANCIA_2, Train_Keras$DISTANCIA_3, Train_Keras$DISTANCIA_4)
matriz_entrada <- as.matrix(entrada)

saida <- data.frame (Train_Keras$ModoNovo_CM,Train_Keras$ModoNovo_IM,Train_Keras$ModoNovo_NM)
matriz_saida <- as.matrix(saida)

### Base de Teste Keras

teste_entrada <- data.frame(Valid_Keras$DURACAO_1,Valid_Keras$DURACAO_2,Valid_Keras$DURACAO_3,Valid_Keras$DURACAO_4,Valid_Keras$DISTANCIA_1,Valid_Keras$DISTANCIA_2,Valid_Keras$DISTANCIA_3, Valid_Keras$DISTANCIA_4)
matriz_teste_entrada <- as.matrix (teste_entrada)

teste_saida <- data.frame (Valid_Keras$ModoNovo_CM,Valid_Keras$ModoNovo_IM, Valid_Keras$ModoNovo_NM) 
matriz_teste_saida <- as.matrix(teste_saida)

######################  Modelo 1 #####################################

modelo1 <- keras_model_sequential()

modelo1 %>%
  layer_dense(units = 1, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo1 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  epochs = 20,
  batch_size = 32,
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
)

# Matriz de Confusao para o Keras (modelo1)

predicoes <- modelo1 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 2 #####################################

modelo2 <- keras_model_sequential()

modelo2 %>%
  layer_dense(units = 2, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo2 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo2 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  epochs = 20,
  batch_size = 32,
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
)

# Matriz de Confusao para o Keras (modelo2)

predicoes <- modelo2 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 3 #####################################

modelo3 <- keras_model_sequential()

modelo3 %>%
  layer_dense(units = 3, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo3 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo3 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  epochs = 20,
  batch_size = 32,
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
)

# Matriz de Confusao para o Keras (modelo3)

predicoes <- modelo3 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 4 #####################################

modelo4 <- keras_model_sequential()

modelo4 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo4 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo4 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  epochs = 20,
  batch_size = 32,
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
)

# Matriz de Confusao para o Keras (modelo4)

predicoes <- modelo4 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 5 #####################################

modelo5 <- keras_model_sequential()

modelo5 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  layer_dense(units = 2, activation = "relu") %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo5 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo5 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  #epochs = 100,
  epochs = 20,
  batch_size = 32,
  #validation_split = 0.2
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
  
)

#### Matriz de Confusao para o Keras (modelo 5)

predicoes <- modelo5 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 6 #####################################

modelo6 <- keras_model_sequential()

modelo6 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  layer_dense(units = 3, activation = "relu") %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo6 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo6 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  #epochs = 100,
  epochs = 20,
  batch_size = 32,
  #validation_split = 0.2
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
  
)

#### Matriz de Confusao para o Keras (modelo 6)

predicoes <- modelo6 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))


###################### Modelo 7 #####################################

modelo7 <- keras_model_sequential()

modelo7 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  layer_dense(units = 4, activation = "relu") %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo7 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo7 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  #epochs = 100,
  epochs = 20,
  batch_size = 32,
  #validation_split = 0.2
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
  
)

#### Matriz de Confusao para o Keras (modelo 7)

predicoes <- modelo7 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))


###################### Modelo 8 #####################################

modelo8 <- keras_model_sequential()

modelo8 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo8 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo8 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  #epochs = 100,
  epochs = 20,
  batch_size = 32,
  #validation_split = 0.2
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
  
)

#### Matriz de Confusao para o Keras (modelo 8)

predicoes <- modelo8 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))

###################### Modelo 9 #####################################

modelo9 <- keras_model_sequential()

modelo9 %>%
  layer_dense(units = 4, activation = "relu", input_shape = ncol(matriz_entrada)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 5, activation = "relu") %>%
  # Camada de saida com ativacao "Softmax" para problemas de classificacao Multiclasse
  layer_dense(units = ncol(matriz_saida), activation = "softmax")

# Compilar o modelo
modelo9 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  #metrics = c("accuracy")
  metrics = c("accuracy", "categorical_accuracy")
)

# Treino do modelo
historico_treino <- modelo9 %>% fit(
  x = matriz_entrada,
  y = matriz_saida,
  epochs = 20,
  batch_size = 32,
  validation_data = list(matriz_teste_entrada, matriz_teste_saida)
  
)


# Matriz de Confusao para o Keras (modelo 9)

predicoes <- modelo9 %>% predict(matriz_teste_entrada)
colnames(predicoes) <- c("Coletivo motorizado","Individual motorizado", "N?o motorizado")
binary_predictions <- colnames(predicoes)[max.col(predicoes,ties.method="first")]
binary_reais <- colnames(predicoes)[max.col(matriz_teste_saida,ties.method="first")]
confusionMatrix(table(binary_predictions, binary_reais))


################################################################################
##                       TABELAS DESCRITIVAS - QUI SQUARE                    ##
################################################################################

# Seleciona vari?veis para sumariza??o
ODbase3 <- ODbase2 %>% dplyr::select(IDADE,SEXO,RENDA_FA,CD_ATIVI,GRAU_INS,MOTIVO_O,DISTANCIA,DURACAO,ZonaNovo,ModoNovo)

ODbase3 <- na.omit(ODbase3)

ODbase3 <- 
  tbl_summary(
    ODbase3,
    by = ModoNovo, # divide a tabela por grupos
    missing = "no" # n?o exibe os casos ausentes separadamente
  ) %>%
  add_n() %>% # adiciona coluna com total de ausentes
  add_p() %>% # adiciona p-value para diferen?a entre grupos *qui-quadrado"
  modify_header(label = "**Vari?vel**") %>% # Altera o cabe?alho da tabela
  bold_labels() 


ODbase3

################################################################################
##                            ANALISE CORRESPONDENCIA MCA                     ##
################################################################################

DBMCA1 <- ODbase2[,c(2,3,4,5,6,8,9,11,12,13)]
DBMCA2 <- ODbase2[,c(9,11,12)]

res.mca1 <- MCA(DBMCA1, graph = FALSE)
res.mca2 <- MCA(DBMCA2, graph = FALSE)

print(res.mca1)
print(res.mca2)

fviz_screeplot(res.mca1, addlabels = TRUE, ylim = c(0, 10))
fviz_screeplot(res.mca2, addlabels = TRUE, ylim = c(0, 35))

var1 <- get_mca_var(res.mca1)
var2 <- get_mca_var(res.mca2)
var1
var2

fviz_mca_var(res.mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca2, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca2, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


fviz_mca_var(res.mca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())


####################
DBMCA3 <- ODbase2[,c(2,5,12)]
res.mca3 <- MCA(DBMCA3, graph = FALSE)

fviz_mca_var(res.mca3, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca3, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_ind(res.mca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal(),
             addEllipses = TRUE, ellipse.type = "confidence",
             habillage = "ModoNovo", # color by groups
)