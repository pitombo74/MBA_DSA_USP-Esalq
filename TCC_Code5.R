################################################################################
##                TCC MBA DATA SCIENCE & ANALYTICS USP ESALQ                  ##
##                        SCRIPTS DE APOIO R                                  ##
##                        By: Marcelo Pitombo                                 ##
################################################################################


####################### INSTALAÇÃO DE PACOTES ##################################

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

#Seleciona variáveis de interesse para novo dataframe

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

#Reclassifica a variável dependente (MODOPRIN)

ODbase <- ODbase %>%
  mutate(ModoNovo = if_else(MODOPRIN %in% 01:08, "Coletivo motorizado",if_else(MODOPRIN %in% 09:14, "Individual motorizado","Não motorizado")))

# 01  Coletivo motorizado
# 02  Individual motorizado
# 03  Não motorizado

#Reclassifica a variável ZonaNovo segundo o arquivo ZONA.xls

ODbase <- merge(ODbase, ZONAS, by="ZONA")

#    1	GRANDE SÃO PAULO
#    2	SÃO PAULO - CENTRAL
#    3	SÃO PAULO - LESTE
#    4	SÃO PAULO - NORDESTE
#    5	SÃO PAULO - NOROESTE
#    6	SÃO PAULO - NORTE
#    7	SÃO PAULO - OESTE
#    8	SÃO PAULO - SUDESTE
#    9	SÃO PAULO - SUDOESTE
#   10	SÃO PAULO - SUL

#Transforma variáveis para tipo categórico/fatorial

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

#Transfere o conteúdo da base original para a nova base ODbase2
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

#Converte variáveis recém categorizadas para FACTOR
ODbase2$IDADE <- as.factor(ODbase2$IDADE)
ODbase2$RENDA_FA <- as.factor(ODbase2$RENDA_FA)
ODbase2$DISTANCIA <- as.factor(ODbase2$DISTANCIA)
ODbase2$DURACAO <- as.factor(ODbase2$DURACAO)

################################################################################
##  SEGMENTA A BASE DE DADOS EM TREINO E VALIDAÇÃO MANTENDO PROPORCIONALIADE  ##          
################################################################################                        

prop.table(table(ODbase2$ModoNovo)) #proporção base de trabalho (modificada/categorizada)

set.seed(200)
trainIndex <- createDataPartition(ODbase2$ModoNovo, p = 0.8,
                                  list = FALSE,
                                  times = 1)

Train <- ODbase2[ trainIndex,]
Valid <- ODbase2[-trainIndex,]

prop.table(table(Train$ModoNovo)) #proporção treinamento
prop.table(table(Valid$ModoNovo)) #proporção teste


################################################################################
##                            ÁRVORES DE DECISÃO                              ##
################################################################################

#Arvore Trainamento

arvore_train <- rpart(ModoNovo ~ IDADE + SEXO + RENDA_FA + CD_ATIVI + GRAU_INS + MOTIVO_O + DISTANCIA + DURACAO + ZonaNovo,
                      data=Train,
                      parms = list(split = 'information'),
                      method='class',
                      na.action = na.omit)

rpart.plot::rpart.plot(arvore_train)

#Predizendo a base de teste com a arvore de treinamento e avalaindo acurácia do Modelo na base de teste

pred_arvore_teste <- predict(arvore_train, Valid, type = "class")

confusionMatrix(table(pred_arvore_teste, Valid$ModoNovo))

#Podando a árvore de treinamento

printcp(arvore_train)
plotcp(arvore_train)

rpart.plot::rpart.plot(prune(arvore_train,cp=0.068))
arvore_train_pod <- prune(arvore_train,cp=0.068)
arvore_train_pod

#Predizendo a base de teste com a arvore de treinamento podada e avalaindo acurácia do Modelo na base de teste

pred_arvore_pod <- predict(arvore_train_pod, Valid, type = "class")

confusionMatrix(table(pred_arvore_pod, Valid$ModoNovo))

################################################################################
##                                RANDOM FOREST                               ##
################################################################################

# Treinar a Random Forest

# Semente aleatória para buscar a reprodutibilidade
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

# Analisando as variáveis importantes do modelo

varImp(arvore_rf_treino)

plot(varImp(arvore_rf_treino))

varImpPlot(arvore_rf_treino)

################################################################################
##                               REDES NEURAIS                                ##
################################################################################

# one hot encoding

Train_RNA <-  dummy_cols(Train, select_columns = c("IDADE","SEXO","RENDA_FA", "CD_ATIVI","GRAU_INS","MOTIVO_O","DISTANCIA","DURACAO","ZonaNovo"))
Valid_RNA <-  dummy_cols(Valid, select_columns = c("IDADE","SEXO","RENDA_FA", "CD_ATIVI","GRAU_INS","MOTIVO_O","DISTANCIA","DURACAO","ZonaNovo"))

# Modelo resumido com Distancia e Duração com 1 camada escondida e 1 neuronio

nn1 <- neuralnet(ModoNovo ~ DISTANCIA_1 + DISTANCIA_2 + DISTANCIA_3 + DISTANCIA_4 + DURACAO_1 + DURACAO_2 + DURACAO_3 + DURACAO_4,
                data=Train_RNA,
                hidden = c(1),
                linear.output = TRUE)
            
plot(nn1,rep = "best")

nn1$result.matrix

# Modelo resumido com Distancia e Duração com 1 camada escondida e 2 neuronio


nn2 <- neuralnet(ModoNovo ~ DISTANCIA_1 + DISTANCIA_2 + DISTANCIA_3 + DISTANCIA_4 + DURACAO_1 + DURACAO_2 + DURACAO_3 + DURACAO_4,
                 data=Train_RNA,
                 hidden = c(2),
                 linear.output = TRUE)

plot(nn2,rep = "best")

nn2$result.matrix

# Calcula predições da RNA (nn)

pred1 <- predict(nn1, Valid_RNA)
colnames(pred1) <- c("Coletivo motorizado","Individual motorizado", "Não motorizado")
newp1 <- colnames(pred1)[max.col(pred1,ties.method="first")]

confusionMatrix(table(newp1, Valid_RNA$ModoNovo))

pred2 <- predict(nn2, Valid_RNA)
colnames(pred2) <- c("Coletivo motorizado","Individual motorizado", "Não motorizado")
newp2 <- colnames(pred2)[max.col(pred2,ties.method="first")]

confusionMatrix(table(newp2, Valid_RNA$ModoNovo))

################################################################################
##                       TABELAS DESCRITIVAS - QUI SQUARE                    ##
################################################################################

# Seleciona variáveis para sumarização
ODbase3 <- ODbase2 %>% dplyr::select(IDADE,SEXO,RENDA_FA,CD_ATIVI,GRAU_INS,MOTIVO_O,DISTANCIA,DURACAO,ZonaNovo,ModoNovo)

ODbase3 <- na.omit(ODbase3)

ODbase3 <- 
  tbl_summary(
    ODbase3,
    by = ModoNovo, # divide a tabela por grupos
    missing = "no" # não exibe os casos ausentes separadamente
  ) %>%
  add_n() %>% # adiciona coluna com total de ausentes
  add_p() %>% # adiciona p-value para diferença entre grupos *qui-quadrado"
  modify_header(label = "**Variável**") %>% # Altera o cabeçalho da tabela
  bold_labels() 


ODbase3

