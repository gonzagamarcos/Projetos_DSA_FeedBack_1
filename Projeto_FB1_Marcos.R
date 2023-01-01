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
percentual = (casos_incompletos/53) * 100
percentual

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

# Etapa 14.2.1: Criando o Modelo Preditivo, versão 1:
modelo_v1 <- lm(`Consumo_Energia(KWh/100km)` ~ ., data = dados_treino)

# Etapa 14.2.2: Visualizando os coeficientes
modelo_v1
summary(modelo_v1) # R-squared: 0.97

#### Conclusão do modelo_v1: ####
# O modelo apresentou um nível de precisão alto, alguns outros fatores que ajudam
# a explicar isso, além de todo o processo de Limpeza e Transformação executado
# durante a Análise Exploratória, são que o conjunto de dados é relativamente 
# pequeno, e muitos parâmetros com uma alta correlação com a variável target foram 
# usados para treinar o modelo. Devido a esses fatores vou permanecer somente com
# essa versão de modelo de análise de regressão.


# Etapa 15: Prevendo o Consumo com os dados de teste:
previsao_v1 <- predict(modelo_v1, dados_teste)
previsao_v1

# Etapa 15.1: Visualizando os valores previstos e observados para "previsao_v1"
resultados_v1 <- cbind(previsao_v1, dados_teste$`Consumo_Energia(KWh/100km)`) 
colnames(resultados_v1) <- c('Previsto','Real')
resultados_v1 <- as.data.frame(resultados_v1)
View(resultados_v1)

# Etapa 16: Para fins de estudo e entender mais como o modelo criado acima se
# comporta, abaixo vamos utilizar os dados que foram removidos do Dataset original
# na Etapa 9, como um exemplo de validação para o modelo_v1.

View(removed_col)

# Etapa 16.1: Criando uma nova variável
validacao <- removed_col

# Etapa 16.2: Removendo o objeto anterior para liberar memória RAM
rm(removed_col)

# Etapa 16.3: Selecionando apenas as colunas numéricas
variaveis_numericas_2 <- sapply(validacao, is.numeric)
validacao_numericos <- validacao[variaveis_numericas_2]
View(validacao_numericos)


# Etapa 16.2: Removendo o objeto anterior para liberar memória RAM
rm(validacao)


# Etapa 16.4: Excluindo a variável Target, a mesma será prevista pelo modelo
validacao_numericos$`Consumo_Energia(KWh/100km)` <- NULL
View(validacao_numericos)


# Etapa 16.5: Função que realiza a Imputação do valor da média, de cada coluna 
# com seus respectivos valores ausêntes
imputacao <- function(x){
  for (i in 1:ncol(x)) {
    medias <- mean(as.numeric(unlist(x[,i])), na.rm = TRUE)
    for (j in 1:nrow(x)){
      if (is.na(x[j,i]))
        x[j,i] <- medias
    }
  }
  return(x)
}


# Etapa 16.6: Fazendo a chamada da função "Imputação" no Dataset
validacao_numericos <- imputacao(validacao_numericos)

class(validacao_numericos)
View(validacao_numericos)          


# Etapa 16.7: Fazendo a Previsão no Dataset de "Validação"
previsao_v2 <- predict(modelo_v1, validacao_numericos)
previsao_v2


# Etapa 16.8: Incluindo a Previsão no Dataset
validacao_final <- cbind(validacao_numericos, previsao_v2) 
colnames(validacao_final)[21] <- "Consumo_Previsto"

View(validacao_final)


#### Exemplo ####

# Abaixo uma tentativa de ilustrar a minha sujestão de como o dado final sobre a
# Previsão do Consumo de Energia, poderia ser apresentado forma visual aos
# tomadores de decisão.

# Etapa 17: Data Frame dados de Teste + Consumo Previsto
teste_final <- cbind(dados_teste, previsao_v1)
colnames(teste_final)[18] <- "Consumo_Previsto"

View(teste_final)

# Etapa 18: Obtendo os fabricantes dos carros com os valores previsto na 
# previsao_v1

previsao_fabricante <- left_join(df, teste_final)
previsao_fabricante <- na.omit(previsao_fabricante)

View(previsao_fabricante)
head(previsao_fabricante)

# Etapa 19: Gráfico 6 - Consumo_Previsto x Fabricante, para os dados de Teste
summary(previsao_fabricante$Consumo_Previsto)

ggplot(previsao_fabricante, aes(y = Consumo_Previsto, x = Fabricante, fill = Fabricante)) +
  geom_bar(stat = "identity") + ggtitle("Consumo_Previsto por Fabricante - Dados de Teste") + 
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,5)) +
  theme_classic(base_size = 14) + xlab("Fabricantes") + 
  ylab("Previsao do Consumo (kWh/100km)") 

# Etapa 20: Conclusão
# Como produto final, fiz o Gráfico 6,  no intuito de ilustrar todo o trabalho 
# realizado. O Gráfico acima mostra a relação de Fabricantes dos carros que 
# foram selecionados randomicamente na Etapa 14.2, a ilustração é específica das
# previsões realizadas pelo modelo nos dados de Teste. O objetivo era criar um
# modelo que fizesse as previsões do Consumo de Energia dos carros foi criando com
# sucesso. Todo processo que foi desenvolvido da carga de dados a previsão final,
# foi seguindo a metodologia aprendida, e aplicando os conceitos ensinados nas 
# aulas.

#### Fim ####


