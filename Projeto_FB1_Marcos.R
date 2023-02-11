# Segunda Versão do Projeto.

# Projeto com Feedback 1: 
# Machine Learning em Logística Prevendo o Consumo de Energia de Carros Elétricos


#### Problema de Negócio ####

# Uma empresa da área de transporte e logística deseja migrar sua frota para 
# carros elétricos com o objetivo de reduzir os custos. Antes de tomar a decisão, 
# a empresa gostaria de prever o consumo de energia de carros elétricos com base 
# em diversos fatores de utilização e características dos veículos.


# Definindo Diretório de Trabalho
setwd("C:/Users/marcos/Documents/Cientista_de_Dados/BigDataRAzure/Projetos_com_Feedback/Arquivos/Projeto_1")
getwd()


# Etapa 1: Carregando Pacotes
library(tidyverse)
library(ggplot2)
library(readxl)
library(corrplot)
library(caTools)
library(rpart)
library(e1071)
library(randomForest)


# Etapa 2: Carregando Dados
df <- read_excel("FEV-data-Excel.xlsx")
class(df)


# Etapa 3: #### Análise Exploratória dos Dados - Limpeza dos Dados #####

# Informações sobre o Dataframe
dim(df)
str(df)
head(df)

# Visualizando os Dados
View(df)


# Etapa 3.1: Verificando se existe valores ausentes (NA)
verifica_na <- function(x){
  colSums(is.na(x))
}

# Etapa 3.1.1: Chamando a Função "verifica_na"
verifica_na(df)


# Etapa 3.2: Verificando quantas observações possuem casos completos
casos_completos <- sum(complete.cases(df))
casos_completos

# Etapa 3.2.1: Verificando quantas observações possuem casos incompletos
casos_incompletos <- sum(!complete.cases(df))
casos_incompletos

# Etapa 3.3.2: Percentual de casos incompletos no Dataset
percentual_incompletos = round((casos_incompletos/53) * 100, 2)
print(percentual_incompletos) # 20,75%

percentual_completos = round((100 - percentual_incompletos), 2)
print(percentual_completos) # 79,25%


# Etapa 3.3.3: Removendo o objeto anterior para liberar memória RAM
rm(percentual)


#### Conclusão preliminar da Análise de Dados acima: ####

# O Dataset contém uma quantidade considerável de valores NA, 20.75% do total de 
# Observações, concentrando a maioria na variável target. Para a criação do
# do modelo preditivo, as observações com valores NA serão omitidas.


# Etapa 4: Removendo a primeira coluna ("Car full name") do Dataset. 
# Já existe variáveis com a mesma informação dentro do próprio conjunto de dados
df[1] <- NULL
View(df)
dim(df)

# Etapa 5: Alterando os nomes das colunas, e gravando em um vetor
new_names <- colnames(df)
new_names

# Etapa 5.1: Nomeando as colunas do vetor
new_names[1] <- "Fabricante"
new_names[2] <- "Modelo"
new_names[3] <- "Preco_Minimo(PLN)"
new_names[4] <- "Potencia_Motor(KM)"
new_names[5] <- "Torque(Nm)"
new_names[6] <- "Tipo_Freio"
new_names[7] <- "Transmissao"
new_names[8] <- "Capacidade_Bateria(kwh)"
new_names[9] <- "Autonomia(WLTP)(km)"
new_names[10] <- "Distancia_Eixos(cm)"
new_names[11] <- "Comprimento(cm)"
new_names[12] <- "Largura(cm)"
new_names[13] <- "Altura(cm)"
new_names[14] <- "Peso_Vazio(kg)"
new_names[15] <- "Peso_Admissivel(kg)"
new_names[16] <- "Carga_Max(kg)"
new_names[17] <- "N_Assentos"
new_names[18] <- "N_Portas"
new_names[19] <- "Raio_Pneu(in)"
new_names[20] <- "Vel_Maxima(kph)"
new_names[21] <- "Mala(VDA)(L)"
new_names[22] <- "Aceleracao_0_100(kph)(s)"
new_names[23] <- "Potencia_Max_Carregamento(kW)"
new_names[24] <- "Consumo_Energia(KWh/100km)"

# Visualizando o resultado
new_names

# Etapa 5.2: Atribuindo os nomes do vetor ao Dataset
colnames(df) <- new_names

# Visualizando o Dataset com as colunas renomeadas
View(df)

# Etapa 5.3: Removendo os objetos anteriores para liberar memória RAM
rm(new_names)


# Etapa 6: Criando uma nova variável, relação "peso/potencia" para determinar 
# o desempenho do carro

relacao_peso_potencia = df%>%
  select(`Potencia_Motor(KM)`, `Peso_Vazio(kg)`) %>%
  ggplot(aes(x = `Peso_Vazio(kg)`, y = `Potencia_Motor(KM)`)) +
  xlab("Peso Vazio (kg)") +
  ylab("Potência do Motor (KM)") +
  ggtitle("Relação entre peso vazio e potência do motor") +
  geom_point() + geom_smooth()

relacao_peso_potencia


# Etapa 6.1: Copiando os dados da variável "Potencia_Motor(KM)" para uma
# variável temporária
potencia <- df$`Potencia_Motor(KM)`
potencia 

# Etapa 6.1.1: Conventerndo a unidade de medida da variável "Potencia_Motor(KM)"
# para cavalo de força (hp)
potencia <- potencia * 1.341
potencia

# Etapa 6.2: Copiando os dados da variável "`Peso_Vazio(kg)`)" para uma
# variável temporária
peso <- df$`Peso_Vazio(kg)`
peso

# Etapa 6.3: Criando a variável "desempenho" que será incorporada no Dataset
desempenho <- peso/potencia
desempenho


# Etapa 6.4: Adicionando a nova coluna ao Dataframe
df$`Desempenho(kg/hp)` <- c(desempenho)
View(df)
dim(df)


# Etapa 7: Removendo os objetos anteriores para liberar memória RAM
rm(peso)
rm(potencia)
rm(desempenho)


# Etapa 8: Omitindo valores ausêntes do Dataset, e criando uma nova variável "df2"
df2 <- na.omit(df)

# Visualizando o Novo Dataset com os valores ausêntes removidos
View(df2)
dim(df2)


# Etapa 9: Visualizando as colunas removidas
removed_col <- setdiff(df,df2)
removed_col


# Etapa 10: Extraindo as variáveis numéricas do Dataset "df2"
variaveis_numericas <- sapply(df2, is.numeric)
dados_numericos <- df2[variaveis_numericas]

View(dados_numericos)
dim(dados_numericos)

# Etapa 10.1: Removendo o objeto anterior para liberar memória RAM
rm(variaveis_numericas)


##### Plots e Estatísticas ##### 

#### Plots ####

# Etapa 11: Criando uma Matrix de Correlação para compreender o relacionamento 
# entre as variáveis numéricas

# Etapa 11.1: Salavando a Matrix de Correlção em uma variável
interacao <- cor(dados_numericos)
interacao

# Etapa 11.2: Criando o Mapa de Correlação com a Matrix acima
corrplot(interacao, method = "color")

#### Conclusão da Análise do Mapa de Correlação: ####

# A maioria das variáveis possuem uma forte correlação tanto positiva quanto 
# negativa em relação a variável target (Consumo_Energia(kwh/100km)), no entanto
# há 4 variáveis que apresentaram os menores valores (próximo a zero) de interação.


# Etapa 12: Removendo as variáveis com nível de significância mais baixo.
df2$`Autonomia(WLTP)(km)` <- NULL
df2$`Altura(cm)` <- NULL
df2$N_Assentos <- NULL
df2$N_Portas <- NULL

# Visualizando o Dataset
View(df2)
dim(df2)


# Etapa 13: Análise por Fabricante

# Etapa 13.1: Obtendo a Média de Consumo de Energia por Fabricante
group_1 <- df2 %>%
  group_by(Fabricante) %>%
  summarise(Media_Consumo = mean(`Consumo_Energia(KWh/100km)`)) %>%
  arrange(desc(Media_Consumo))

View(group_1)
head(group_1)

#### Conclusão da análise acima: ####

# Os carros da fabricante Citroën possuem a média de consumo de combustível em
# 25.2(KWh/100km), enquanto o menor consumo é da fabricante Mazda, 15.5(KWh/100km).


# Etapa 13.2: Relação Consumo x Desempenho
group_2 <- df2 %>%
  group_by(Fabricante) %>%
  select(Fabricante, Modelo, `Preco_Minimo(PLN)`, `Consumo_Energia(KWh/100km)`, `Desempenho(kg/hp)`) %>%
  arrange(desc(`Consumo_Energia(KWh/100km)`))

View(group_2)
head(group_2)
tail(group_2)

#### Conclusão da análise acima: ####

# Os carros da Audi lidera as duas primeiras posições do ranking com o maior
# consumo de energia e com um desempenho igual a 4.00(kg/hp) para os seus carros.
# Enquanto a BMW i3 é a última do ranking, mas com um equilíbrio maior entre 
# o consumo de energia e o desempenho do carro. Olhando apenas para esses dados, 
# e comparando as duas marcas, podemos afirmar que o melhor custo benefício entre 
# os carros Audi e-tron 55 quattro, Audi e-tron 55 Sportback S quattro e a BMW i3
# fica com a BMW.


# Etapa 13.3: Gráfico 1 - Consumo de Energia x Desempenho
ggplot(group_2, aes(x = `Desempenho(kg/hp)`, y = `Consumo_Energia(KWh/100km)` , 
                    colour = (Fabricante))) + geom_point(size = 1) +
  scale_y_continuous(limits = c(12,28), breaks = seq(12,29,2)) +
  geom_smooth(method = lm , color = "red", se = FALSE) +
  ggtitle("Consumo de Energia x Desempenho") +
  theme(legend.position = "right") +
  xlab ("Desempenho(kg/hp)")  + 
  ylab ("Consumo de Energia (KWh/100km)")

#### Conclusão do Gráfico acima: ####

# O gráfico confirma a informação do Mapa de Correlação, existe uma forte
# correlação negativa entre as duas variáveis, a medida que o desempenho do 
# carro aumenta o consumo de energia do veículo diminui. 


# Etapa 13.4: Gráfico 2 - Consumo de Energia x Bateria
summary(df2$`Capacidade_Bateria(kwh)`)

ggplot(df2, aes(x = `Consumo_Energia(KWh/100km)`, y = `Capacidade_Bateria(kwh)`, 
                colour = (Fabricante))) + geom_point(size = 1) +
  scale_y_continuous(limits = c(16,100), breaks = seq(16,96,10)) +
  geom_smooth(method = lm , color = "red", se = FALSE) +
  ggtitle("Consumo de Energia x Bateria") +
  theme(legend.position = "right") +
  xlab ("Consumo de Energia (KWh/100km)") + 
  ylab ("Capacidade da Bateria")

#### Conclusão Gráfico 2: ####

# O gráfico mostra que há uma forte correlação positiva, o que confirma que 
# para um consumo maior de energia o carro tem que ter uma bateria com maior
# capacidade.


# Etapa 13.5: Gráfico 3 - Consumo de Energia x Aceleração
summary(df2$`Aceleracao_0_100(kph)(s)`)

ggplot(df2, aes(x = `Consumo_Energia(KWh/100km)`, y = `Aceleracao_0_100(kph)(s)`, 
                colour = (Fabricante))) + geom_point(size = 1) +
  scale_y_continuous(limits = c(0,14), breaks = seq(0,14,2)) +
  geom_smooth(method = lm , color = "red", se = FALSE) +
  ggtitle("Consumo de Energia x Aceleração") +
  theme(legend.position = "right") +
  xlab ("Consumo de Energia (KWh/100km)") + 
  ylab ("Aceleração(kph/s)")

#### Conclusão Gráfico 3: ####

# O gráfico mostra que quanto maior a acelerção do veículo menor é o consumo de
# energia. Quanto maior a massa, menor é a aceleração e consequentemente a 
# velocidade.


# Etapa 13.5: Gráficos Auxiliares para ajudar a entender a distribuição dos dados
# na variável target.
summary(df2$`Consumo_Energia(KWh/100km)`)


# Etapa 13.5.1: Gráfico 4 - Histograma de Distribuição
ggplot(df2, aes(x = `Consumo_Energia(KWh/100km)`, fill = Fabricante)) +
  geom_histogram(bins = 8) + 
  geom_vline(aes(xintercept=mean(`Consumo_Energia(KWh/100km)`)), color="blue", linetype="solid", size=1) +
  ggtitle("Distribuição dos Dados sobre Consumo") + 
  xlab("Consumo de Energia (kwh/100km)") +  ylab("Frequência")


# Etapa 13.5.2: Gráfico 5 - BoxPlot do Consumo de Energia dos Carros
ggplot(df2, aes(y = `Consumo_Energia(KWh/100km)`, fill = Fabricante)) +
  geom_boxplot(show.legend = T, alpha = .9) +
  scale_y_continuous(limits = c(12,29), breaks = seq(12,29,2)) +
  ggtitle("Histograma sobre Consumo") +
  theme_classic(base_size = 18) +
  ylab("Consumo de Energia (kwh/100km)")


#### Estatística ##### 

# Verificando se os dados da variável target segue uma distribuição normal
test_shapiro <- shapiro.test(df2$`Consumo_Energia(KWh/100km)`)
test_shapiro # p-value = 0.0001665


#### Conclusão da análise acima:
# p-value < 0.05, rejeita-se H0 (hipótese nula) e conclui que a amostra não vem de uma 
# distribuição normal.


##### Análise Preditiva ##### 

# Etapa 14: Divisão dos dados em Treino e Teste

# Observação: Para criação do Modelo,  será utilizado o conjunto de dados criado 
# anteriormente com apenas valores numéricos, "dados_numericos".
View(dados_numericos)

# Etapa 14.1: Removendo as variáveis com valor de significância mais próximo de zero 
dados_numericos$`Autonomia(WLTP)(km)` <- NULL
dados_numericos$`Altura(cm)` <- NULL
dados_numericos$N_Assentos <- NULL
dados_numericos$N_Portas <- NULL

View(dados_numericos)


# Etapa 14.2: Divisão do dataset em dados de Treino e Teste
set.seed(480)
split = sample.split(dados_numericos$`Consumo_Energia(KWh/100km)`, SplitRatio = 0.70)

# Dados de Treino
dados_treino = subset(dados_numericos, split == TRUE)
View(dados_treino)
dim(dados_treino)

# Dados de Teste
dados_teste = subset(dados_numericos, split == FALSE)
View(dados_teste)
dim(dados_teste)


#### Importante ####
# Alguns modelos, como a regressão linear, são baseados na suposição de que os dados sejam 
# normalmente distribuídos.

# Etapa 15: Criando o Modelo Preditivo 1, Arvóre de Decisão versão ANOVA:
modelo_v1 <- rpart(`Consumo_Energia(KWh/100km)` ~ ., data = dados_treino, method = "anova")

# Etapa 15.1: Visualizando os coeficientes
modelo_v1
summary(modelo_v1)
plot(modelo_v1, branch = 0.8, margin = 0.1)
text(modelo_v1)

# Etapa 15.2: Prevendo o Consumo com os dados de teste:
previsao_v1 <- predict(modelo_v1, dados_teste)

# Etapa 15.3: Cálculo das métricas de avaliação
mse_modelo_v1 <- mean((previsao_v1 - dados_teste$`Consumo_Energia(KWh/100km)`)^2)
mae_modelo_v1 <- mean(abs(previsao_v1 - dados_teste$`Consumo_Energia(KWh/100km)`))
acuracia_modelo_v1 <- 1 - mean(abs(previsao_v1 - dados_teste$`Consumo_Energia(KWh/100km)`)/dados_teste$`Consumo_Energia(KWh/100km)`)

# Etapa 15.4: Visualizando os valores previstos e observados para "previsao_v1"
print(paste("MSE:", mse_modelo_v1)) # 2.57
print(paste("MAE:", mae_modelo_v1)) # 1.33
print(paste("Acurácia:", acuracia_modelo_v1)) # 0.91

#### Conclusão do modelo_v1: ####
# O modelo apresentou um nível de precisão alto de 91%, assim como o MSE que representa o erro 
# quadrático médio entre os valores previstos e os valores reais, 2.57 (quanto menor melhor) e 
# o MAE que representa o erro absoluto médio entre os valores previstos e os valores reais, 1.33
# (quanto menor melhor o modelo).


# Criando o Modelo Preditivo 2, Arvóre de Decisão versão POISSON:
modelo_v1_poisson <- rpart(`Consumo_Energia(KWh/100km)` ~ ., data = dados_treino, method = "poisson")

# Prevendo o Consumo com os dados de teste:
previsao_v1_poisson <- predict(modelo_v1_poisson, dados_teste)

# Cálculo das métricas de avaliação
mse_modelo_v2_poisson <- mean((previsao_v1_poisson - dados_teste$`Consumo_Energia(KWh/100km)`)^2)
mae_modelo_v2_poisson <- mean(abs(previsao_v1_poisson - dados_teste$`Consumo_Energia(KWh/100km)`))
acuracia_modelo_v2_poisson <- 1 - mean(abs(previsao_v1_poisson - dados_teste$`Consumo_Energia(KWh/100km)`)/dados_teste$`Consumo_Energia(KWh/100km)`)

# Visualizando os valores previstos e observados para "previsao_v1_poisson"
print(paste("MSE:", mse_modelo_v2_poisson)) # 2.57
print(paste("MAE:", mae_modelo_v2_poisson)) # 1.34
print(paste("Acurácia:", acuracia_modelo_v2_poisson)) # 0.91

#### Conclusão do modelo_v1_poisson: ####
# O modelo apresentou as mesmas medidas do primeio modelo usando method = anova, ouve apenas uma 
# pequena diferença no MAE foi 0.01 pontos maior que o primeiro modelo, com essas configuração não
# diferença significativa entre os modelos.


# Etapa 16: Criando o Modelo Preditivo 2, SVM:
modelo_v2 <- svm(`Consumo_Energia(KWh/100km)` ~ ., data = dados_treino, type = "nu-regression")

# Etapa 16.1: Visualizando o modelo acima
summary(modelo_v2)

# Etapa 16.2: Prevendo o Consumo com os dados de teste:
previsao_v2 <- predict(modelo_v2, dados_teste)
previsao_v2

# Etapa 16.3: Calculo das métricas de avaliação
mse <- mean((previsao_v2 - dados_teste$`Consumo_Energia(KWh/100km)`)^2)
mae <- mean(abs(previsao_v2 - dados_teste$`Consumo_Energia(KWh/100km)`))
acuracia <- 1 - mean(abs(previsao_v2 - dados_teste$`Consumo_Energia(KWh/100km)`)/dados_teste$`Consumo_Energia(KWh/100km)`)

# Etapa 16.4: Visualizando os valores previstos e observados para "previsao_v1"
print(paste("MSE:", mse)) # 2.60
print(paste("MAE:", mae)) # 1.21
print(paste("Acurácia:", acuracia)) # 0.92


#### Conclusão do modelo_v2: ####
# O modelo apresentou um nível de precisão de 92% maior que o modelo_v1, mas o MSE foi um pouco maior, 2.60
# (ainda um bom valor) e o MAE de 1.21 também menor comparando com o modelo_v1.


#### Observação ####

# Para o modelo com RandomForest é necessario trocar os nomes das colunas, por conta da sintaxe dos 
# nomes o Modelo apresentou erro.

# Salvando o dados_treino em uma nova variável
dados_treino_rf <- dados_treino

# Alterando os nomes das colunas do conjunto de dados_treino
dados_treino_rf1 <- colnames(dados_treino_rf)

dados_treino_rf1 [1] <- "col1"
dados_treino_rf1 [2] <- "col2"
dados_treino_rf1 [3] <- "col3"
dados_treino_rf1 [4] <- "col4"
dados_treino_rf1 [5] <- "col5"
dados_treino_rf1 [6] <- "col6"
dados_treino_rf1 [7] <- "col7"
dados_treino_rf1 [8] <- "col8"
dados_treino_rf1 [9] <- "col9"
dados_treino_rf1 [10] <- "col10"
dados_treino_rf1 [11] <- "col11"
dados_treino_rf1 [12] <- "col12"
dados_treino_rf1 [13] <- "col13"
dados_treino_rf1 [14] <- "col14"
dados_treino_rf1 [15] <- "col15"
dados_treino_rf1 [16] <- "alvo"
dados_treino_rf1 [17] <- "col17"

# Atribuindo os nomes do vetor ao dados_treino_rf
colnames(dados_treino_rf) <- dados_treino_rf1

# Etapa 17: Criando Modelo Preditivo 3, Random Forest:

# Segundo modelo sem determinar um numero minímo de nós
set.seed(123)
modelo_v3_1 <- randomForest(alvo ~ ., data = dados_treino_rf, ntree = 100, nodesize = 10)
modelo_v3_1 # Mean of squared residuals: 3.34 


# Segundo modelo sem determinar um numero minímo de nós
modelo_v3_2 <- randomForest(alvo ~ ., data = dados_treino_rf, ntree = 100)
modelo_v3_2 # Mean of squared residuals: 2.98


# Análise dos Modelos criados acima: 
# Quanto menor o valor dessa métrica, melhor o modelo se ajustou aos dados de treinamento, 
# o que geralmente indica que o modelo está capturando bem as tendências nos dados e fazendo 
# previsões mais precisas, dito isso dos modelos criado vou seguir com o segundo modelo criado
# modelo_v3_2


# Salvando o dados_teste em uma nova variável
dados_teste_rf <- dados_teste

# Alterando os nomes das colunas do conjunto de dados_teste
dados_teste_rf1 <- colnames(dados_teste_rf)

dados_teste_rf1 [1] <- "col1"
dados_teste_rf1 [2] <- "col2"
dados_teste_rf1 [3] <- "col3"
dados_teste_rf1 [4] <- "col4"
dados_teste_rf1 [5] <- "col5"
dados_teste_rf1 [6] <- "col6"
dados_teste_rf1 [7] <- "col7"
dados_teste_rf1 [8] <- "col8"
dados_teste_rf1 [9] <- "col9"
dados_teste_rf1 [10] <- "col10"
dados_teste_rf1 [11] <- "col11"
dados_teste_rf1 [12] <- "col12"
dados_teste_rf1 [13] <- "col13"
dados_teste_rf1 [14] <- "col14"
dados_teste_rf1 [15] <- "col15"
dados_teste_rf1 [16] <- "alvo"
dados_teste_rf1 [17] <- "col17"

# Atribuindo os nomes do vetor ao dados_teste_rf
colnames(dados_teste_rf) <- dados_teste_rf1
View(dados_teste_rf)

# Etapa 18: Prevendo o Consumo(alvo) com os dados de teste:
previsao_v3 <- predict(modelo_v3_2, dados_teste_rf)
previsao_v3

# Etapa 18.2: Cálculo das métricas de avaliação
erro_quadratico_medio_rf <- mean((dados_teste_rf$alvo - previsao_v3)^2)
erro_absoluto_medio_rf <- mean(abs(dados_teste_rf$alvo - previsao_v3))
acuracia_rf <- mean(dados_teste_rf$alvo == previsao_v3)

# Etapa 18.3: Visualizando os valores previstos e observados para "previsao_v1"
print(paste("MSE:", erro_quadratico_medio_rf)) # 1.67
print(paste("MAE:", erro_absoluto_medio_rf)) # 1.01
print(paste("Acurácia:", acuracia_rf)) # 0


#### Conclusão do modelo_v3: ####
# O modelo apresentou valores para MSE e MAE menores que os dos dois modelos criados anteriormente
# mas em contra-partida uma acurácia igual a 0.




# Etapa 20: Conclusão Final
# Essa foi a segunda versão do programa, as alterações foram significantes nessa nova versão,
# sendo as prirncipais:
# Previsão com Modelo Arvoré de Decisão (rpart)
# Previsão com Modelo SVM
# Previsão com Modelo RandomForest
# Na primeira versçao do modelo, uns dos erros foi usar regressao linear em dados que não seguem
# uma distribuição normal, corrigindo esse problema, foi removido o modelo criado anteriomente com
# Linear Regression, e no lugar foram criados 5 modelos que analisam dados independente da normalidade
# dos dados. Nos 5 modelos criados, foram explorados os hiperparâmetros afim de alcançar a máxima 
# precisão. Assim o modelo que se destacou foi o modelo criado com o Support Vector Machine (SVM),
# sendo assim a escolha para realizar as previsões deste estudo de caso.


#### Fim ####



