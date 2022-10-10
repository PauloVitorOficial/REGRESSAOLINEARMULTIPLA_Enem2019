#Paulo Vitor Silva

# Limpa a memória do software R
rm(list = ls())

# Utiliza virgula para demarcar casas decimais (padrao portugues)
options(OutDec = ",")

# Verifica se os pacotes do software R que serao utilizados estao instalados. Em caso negativo, realiza instalacao 
if(!require("ggplot2")){install.packages("ggplot2"); dependencies=TRUE}
if(!require("MASS")){install.packages("MASS"); dependencies=TRUE}
if(!require("car")){install.packages("car"); dependencies=TRUE}
if(!require("gridExtra")){install.packages("gridExtra"); dependencies=TRUE}

# Carrega os pacotes que serao utilizados
require(ggplot2)
require(MASS)
require(car)
require('gridExtra')

# ------------------------------------------------------------------------------
# Primeiro Passo: Realizar a Leitura dos Dados

# Leitura dos dados

dadosBrutos <- read.table("ENEM_2019_MG1.csv", sep = ";", dec = ",", header = T)
head(dadosBrutos)
dim(dadosBrutos)
tail(dadosBrutos)
summary(dadosBrutos)

# Excluindo variaveis que nao serão usadas

dados=dadosBrutos[,-c(1,2,3,4,9,10,12,13,14,15,16,17,18,19,20,22,23,31)] # CO_UF_ESC = 16  NO_MUNICIPIO_PROVA =21, Q003  =31
# ou  dados=dadosBrutos[,c(5,6,7,8,11,21,24,25,26,27,28,29,30,32,33,34,35,36,37,38)] #NU_NOTA_REDACAO =28 ; Q022=35
dim(dados)

dados=na.omit(dados) # retirando linhas que possuem NA

dados$media=(dados[,7]+dados[,8]+dados[,9]+dados[,10]+dados[,11])/5 # criando uma variavel resposta
head(dados,20)
summary(dados)

dados=dados[dados$NO_MUNICIPIO_PROVA =='Ouro Preto',] # filtrando o municipio de Ouro Preto
dim(dados)
# ------------------------------------------------------------------------------
## AnÃ¡lise descritiva dos Dados
colnames(dados)
# 1) Se X for contÃ?nua, utilize um grÃ¡fico de DispersÃ£o

# GrÃ¡fico de DispersÃ£o entre X e Y, utilizando o pacote GGPLOT 
g1 = ggplot(dados, aes(y=media, x=NU_IDADE)) + 
  geom_point(size = 1) + 
  # O comando abaixo permite incluir a reta de regressao no grafico de dispersao
  geom_smooth(method=lm, se=F)+
  theme_bw()+
  labs(y="Nota no ENEM", x="Idade",  title = "(a)")+
  theme(axis.text.x = element_text(hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title=element_text(size=12))

# 2) Se X for uma variÃ¡vel categÃ³rica, uma opÃ§Ã£o Ã© utilizar Boxplot

# Variavel Sexo
g2 = ggplot(dados, aes(x=factor(`TP_SEXO`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Sexo",  title = "(b)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

colnames(dados)
g3 = ggplot(dados, aes(x=factor(`TP_ESTADO_CIVIL`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Estado Civi",  title = "(a)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g4 = ggplot(dados, aes(x=factor(`TP_COR_RACA`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Cor/raça",  title = "(c)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g5 = ggplot(dados, aes(x=factor(`TP_ESCOLA`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Tipo de Escola/Ensino Médio",  title = "(d)")+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

colnames(dados)
g6= ggplot(dados, aes(x=factor(`Q001`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Escolaridade do Pai",  title = "(a)")+
  theme_bw(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 90))


#c("A","B","C","D","E" ,"F","G","H")

g7= ggplot(dados, aes(x=factor(`Q002`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Escolaridade da Mãe",  title = "(b)", )+
  theme_bw(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g8 = ggplot(dados, aes(y=media, x=Q005)) + 
  geom_point(size = 1) + 
  # O comando abaixo permite incluir a reta de regressao no grafico de dispersao
  geom_smooth(method=lm, se=F)+
  theme_bw()+
  labs(y="Nota no ENEM", x="Número de residentes",  title = "(c)")+
  theme(axis.text.x = element_text(hjust = 1, size=12),
        axis.text.y = element_text(hjust = 1, size=12),
        plot.title = element_text(hjust = 0.5),
        axis.title=element_text(size=12))

g9= ggplot(dados, aes(x=factor(`Q006`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Renda Familiar",  title = "(b)", )+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g10= ggplot(dados, aes(x=factor(`Q022`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Celulares na residência ",  title = "(b)", )+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g11= ggplot(dados, aes(x=factor(`Q023`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Tem telefone fixo na residência",  title = "(a)", )+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

g12= ggplot(dados, aes(x=factor(`Q024`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Tem computador na residência",  title = "(c)", )+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

g13= ggplot(dados, aes(x=factor(`Q025`), y=`media`)) + 
  geom_boxplot(fill = "lightblue")+
  labs(y="Nota no ENEM", x="Tem acesso á internet na residência",  title = "(d)", )+
  theme_bw(base_size = 12)+
  theme(plot.title = element_text(hjust = 0.5))

###Graficos das variaveis
windows();grid.arrange(arrangeGrob(g1, g2,g4,g5,ncol = 2, nrow =2))
windows();grid.arrange(arrangeGrob(g3,g9,g8,ncol =3, nrow =1))
windows();grid.arrange(arrangeGrob(g6,g7,ncol = 2, nrow =1))
windows();grid.arrange(arrangeGrob(g11,g10,g12,g13,ncol = 2, nrow =2))


##------------------------------------------------------------------------------------------

## Ajuste do Modelo

colnames(dados)

# O comando "lm" ajusta o modelo de regressÃ£o linear no software R
modelo = lm( media  ~ NU_IDADE+TP_SEXO+TP_ESTADO_CIVIL+TP_COR_RACA+TP_ESCOLA+      
            Q001+Q002+Q005+Q006+Q022+Q023+Q024+Q025, data = dados)

# Utilize o comando summary para visualizar os resultados
summary(modelo)

# Coeficientes do Modelo
round(summary(modelo)$coef,4)

#O valor do Coeficiente de Determinação Ajustado
summary(modelo)$adj.r.squared
#-----------------------------------------------------------------------------------
# ANOVA

#Para calcular a anova no R Ã© necessÃ¡rio ajustar o modelo nulo
modelo.null = lm(media ~1, data = dados)
summary(modelo.null)
round(anova(modelo.null,modelo),4)


#-----------------------------------------------------------------------------------
# Verificando se existe multicolinearidade
vif(modelo)

#-----------------------------------------------------------------------------------
# SeleÃ§Ã£o de variÃ¡veis

# Utilizando o comando step (critÃ©rio AIC)
step(modelo)
 
#-----------------------------------------------------------------------------------
# Modelo final
modelo.final = lm(media ~ NU_IDADE + TP_COR_RACA + TP_ESCOLA + Q001 + 
                    Q002 +Q005+ Q006 + Q024, data = dados)
summary(modelo.final)

# Coeficientes do Modelo
round(summary(modelo.final)$coef,4)

# Coeficiente de DeterminaÃ§Ã£o R^2
summary(modelo.final)$adj.r.squared

# Estimativa de sigma
summary(modelo.final)$sigma

#-----------------------------------------------------------------------------------
## AnÃ¡lise de resÃ?duos

#Obtendo os resÃ?duos
residuos = residuals(modelo.final) 

# Teste de Normalidade para os resÃ?duos
shapiro.test(residuos)$p.value

## AnÃ¡lise de resÃ?duos padronizados ------------------------------------------------

X = model.matrix(modelo.final)
H= X %*% solve(t(X)%*%X)%*%t(X)
resid.padrao = residuals(modelo.final)
s  = summary(modelo.final)$sigma
resid.h = resid.padrao /(s *sqrt(1-diag(H)))

windows()
par(mfrow = c(1,3))

qqnorm(resid.h, main = "(a)", xlab = "Quantis Teóricos", ylab = "Quantis Empíricos")
qqline(resid.h)

plot(fitted(modelo.final), resid.h,main = "(b)", ylab = "Resíduos padronizados"
     , xlab = "Valores ajustados")
abline(h = 0, col = "red", lty = 2)

plot(resid.h,main = "(c)", ylab= "Resíduos padronizados", xlab = "Ordem")
abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1,1))
#------------------------------------------------------------------------------------
# Pontos Influentes
#------------------------------------------------------------------------------------
# CÃ¡lculo da DistÃ¢ncia de Cook
max(cooks.distance(modelo.final)) #O maior valor observado para a Distância de Cook
windows()
cooksd <- cooks.distance(modelo.final)
sample_size <- nrow(dados)

maximo = max(cooksd, 1.1)
plot(cooksd, pch="*",  ylab = "Distância de Cook", 
     ylim = c(0,maximo))  # GrÃ¡fico da DistÃ¢ncia de Cook
abline(h = 1, col="orange")  # CritÃ©rio 1
abline(h = 4/sample_size, col="red")  # CritÃ©rio 2

#------------------------------------------------------------------------------------
