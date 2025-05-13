# Carregar bibliotecas necessárias
library(caret)
library(ggvis)
library(randomForest)
library(e1071)
library(dplyr)
library(varImp)
library(e1071)
library(fastDummies)


# Carregar e preparar os dados
dataset <- read.table("random.txt", header = TRUE)

# Filtrar colunas para usar no dataset
variaveis_necessarias <- c(
  "NO_EVENTO",    #SITUATION                
  "IN_SEXO_ALUNO", #IN_SEX_ALUNO
  "NU_IDADE_ALUNO", #NU_STUDENT_AGE
  "IN_ALUNO_DEFICIENCIA", #IN_STUDENT_DISABILITY
  "IN_ATIVIDADE_COMPLEMENTAR", #IN_STUDENT_DISABILITY
  "IN_RESERVA_VAGAS", #IN_AFFIRMATIVE_POLICIES
  "IN_ING_CONVENIO_PECG", #IN_PARTNERSHIP_PROGRAM
  "IN_APOIO_SOCIAL", #IN_SOCIAL_SUPPORT
  "TEMPO", #TIME_LENGTH
  "CO_TURNO_NOTURNO", #CO_NIGHT_SHIFT
  "CO_TURNO_MATUTINO", #CO_MORNING_SHIFT
  "CO_TURNO_INTEGRAL", #CO_FULLTIME_SHIFT
  "CO_TURNO_VESPERTINO", #CO_AFTERNOON_SHIFT
  "CO_TURNO_N_INF" #CO_SHIFT_N_INF
)

# Filtra as variáveis, renomeia TEMPO e cria CO_ALUNO_EVASAO
dataset <- dataset[, variaveis_necessarias] %>% 
  rename(T = "TEMPO") %>% 
  mutate(CO_ALUNO_EVASAO = if_else(NO_EVENTO == "Evadido", 1, 0))

# Transformar em factor
dataset <- dataset %>%
  mutate(NO_EVENTO = as.factor(NO_EVENTO))

# Transformar em dummies
dataset <- dataset %>% 
  dummy_cols(select_columns = c("T"), 
           remove_selected_columns = TRUE)

# Dividir os dados em conjunto de treino (70%) e validação (30%)
set.seed(1234)

validation_index <- createDataPartition(dataset$NO_EVENTO, p = 0.70, list = FALSE)
validation <- dataset[-validation_index, ]
training <- dataset[validation_index, ]

# Numero de observações em cada conjunto
cat("Número de observações no conjunto de TREINAMENTO:", nrow(dataset), "\n")
cat("Número de observações no conjunto de VALIDAÇÃO:", nrow(validation), "\n")


# Análise exploratória dos dados
dim(training)  # Dimensões do conjunto de dados
sapply(training, class)  # Tipo de cada atributo
head(training)  # Primeiras 5 linhas


# Distribuição da variável de classe
percentage <- prop.table(table(training$NO_EVENTO)) * 100
cbind(freq = table(training$NO_EVENTO), percentage = percentage)

# Resumo estatístico dos dados
summary(training)

# Separar variáveis preditoras (x) e alvo (y)
x <- training %>% 
  select(-NO_EVENTO,-CO_ALUNO_EVASAO)
y <- training[, 1]

# Visualização da distribuição da classe
plot(y)

# Controle de validação cruzada
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# -----------------------------
# Modelo Random Forest com caret
# -----------------------------

training_rf <- training %>% 
  select(-CO_ALUNO_EVASAO)

validation_rf<- validation %>% 
  select(-CO_ALUNO_EVASAO)

fit.rf <- train(
  NO_EVENTO ~ IN_ATIVIDADE_COMPLEMENTAR + CO_TURNO_NOTURNO + NU_IDADE_ALUNO + IN_ALUNO_DEFICIENCIA
  + T_1 + T_2 + T_3 + T_4 + T_5 + T_6 + T_7, 
  data = training_rf, 
  method = "rf", 
  metric = metric, 
  trControl = control, 
  importance = TRUE,
  tuneGrid = data.frame(mtry = 4),
  ntree = 500
)

# Avaliar modelo no conjunto de validação
predictions.rf <- predict(fit.rf, validation_rf)
confmat.rf <- confusionMatrix(predictions, validation_rf$NO_EVENTO)

# Importância das variáveis
# Obter importância das variáveis
importance.rf <- caret::varImp(fit.rf, scale = TRUE)
# Visualizar graficamente
plot(importance.rf, top = 11, main = "Top 20 Variáveis Mais Importantes")

# -----------------------------
# Modelo Reg Log
# -----------------------------
#### INICIAL ####
training_rl <- training %>% 
  select(-NO_EVENTO)

validation_rl <- validation %>% 
  select(-NO_EVENTO)


attach(training_rl)
# Reg Log com todas as variáveis 
fit0.rl = glm(CO_ALUNO_EVASAO~.,family=binomial(link = "logit"),data = training_rl)
summary(fit0.rl)

# Reg Log com as variáveis selecionadas
fit1.rl = glm(CO_ALUNO_EVASAO ~ IN_ATIVIDADE_COMPLEMENTAR + CO_TURNO_NOTURNO + IN_SEXO_ALUNO + IN_APOIO_SOCIAL
           + NU_IDADE_ALUNO + T_1 + T_2 + T_3 + T_4 + T_5 + T_6,family=binomial(link = "logit"))
summary(fit1.rl)



# Avaliar modelo no conjunto de validação
predictions.rl <- predict(fit1.rl, validation_rl, type = "response")

# Converter predições contínuas em classes (0 ou 1)
predicted_class <- ifelse(predictions.rl >= 0.5, 1, 0)

# Converter para factor
validation_rl$CO_ALUNO_EVASAO <- factor(validation_rl$CO_ALUNO_EVASAO, levels = c(0, 1))


# Transformar para fator com os mesmos níveis da variável resposta
predicted_class <- factor(predicted_class, levels = levels(validation_rl$CO_ALUNO_EVASAO))

# Gerar a matriz de confusão
confmat.rl <- confusionMatrix(predicted_class, validation_rl$CO_ALUNO_EVASAO)

# Importância das variáveis
# Obter importância das variáveis
importance.rf <- caret::varImp(fit.rf, scale = TRUE)
# Visualizar graficamente
plot(importance.rf, top = 11, main = "Top 20 Variáveis Mais Importantes")


# -----------------------------
# Modelo SVM
# -----------------------------
sapply(training_rf, class)

normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}
denormalize <- function(x, original_min, original_max) {
  return (x * (original_max - original_min) + original_min)
}

# Lista das variáveis a normalizar
vars_to_normalize <- "NU_IDADE_ALUNO"

# Copiar os dados
training_norm <- training_rf
validation_norm <- validation_rf

# Normalizar apenas NU_IDADE_ALUNO
training_norm$NU_IDADE_ALUNO <- normalize(training_rf$NU_IDADE_ALUNO)
validation_norm$NU_IDADE_ALUNO <- normalize(validation_rf$NU_IDADE_ALUNO)

# Controle de treino com validação cruzada de 10 folds
ctrl <- trainControl(
  method = "cv",        # cross-validation
  number = 10,          # 10 folds
  savePredictions = "final", # salva predições para análise posterior
  classProbs = TRUE,         # para modelos de classificação
  summaryFunction = twoClassSummary  # usa métricas como ROC, Sens, Spec
)

# Modelo SVM com kernel radial
set.seed(1234)  # para reprodutibilidade
fit.svm.cv <- train(
  NO_EVENTO ~ .,
  data = training_norm,
  method = "svmRadial",      # ou "svmLinear", "svmPoly"
  metric = "ROC",            # otimizar AUC, pode trocar por "Accuracy"
  trControl = ctrl,
  preProcess = NULL,
  tuneLength = 5             # número de combinações de hiperparâmetros a tentar
)

# Resultados da cross-validation
print(fit.svm.cv)
plot(fit.svm.cv)

# Predição
pred.svm <- predict(fit.svm.cv, validation_norm)

# Matriz de confusão
confusionMatrix(pred.svm, validation_norm$NO_EVENTO)

# Importância das variáveis
importance.svm <- caret::varImp(fit.svm.caret, scale = TRUE)

# Mostrar tabela de ranking
importance_table <- importance.svm$importance %>%
  tibble::rownames_to_column("Variavel") %>%
  rename(Importancia = 1 + 1) %>%  # ou especifique o nome correto se `value` ou `NO_EVENTO`
  arrange(desc(Importancia))

print(importance_table)

# Gráfico de importância
plot(importance.svm, top = 11, main = "Top 11 Variáveis Mais Importantes")

