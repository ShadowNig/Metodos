library(tidyverse)
library(readxl)
library(DescTools)
library(gridExtra)

## funcao para calcular as proporcoes de uma linha ou coluna

prop <- function(x){return(x/sum(x))}
prop2 <- function(tabela, l, c){return(sum(tabela[l,])*sum(tabela[,c])/(sum(tabela)^2))}

### 1

## Vamos realizar um teste de aderencia para verificar se o numero de acidentes em
## cada dia da semana muda ou nao.

# H0: p1 = p2 = p3 = ... = p7 VS H1: pelo menos um deles diferente dos demais

## Analise descritiva

Dia <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
Numeros <- c(20,10,10,15,30,20,35)

base1 <- tibble(Dia, Numeros)

# sob H0, como pi = pj, para i,j = {1,2,...,7}, pi = 1/7
base1 %>% mutate(`Frequencia Esperada` = (1/7)*sum(Numeros))

# Observando a tabela, vemos que alguns valores estao longe da frequencia esperada,
# o que pode ser indicativo de que os dias da semana interferem no numero de acidentes

# como as frequencias sao todas maiores que cinco, podemos realizar o teste de
# aderencia sem problemas

chisq.test(Numeros, p = rep(1/7, 7), correct = F)

# Adotando um nivel de significancia de 1%, rejeitamos H0, pois obtivemos p-valor
# muito menor. Ou seja, temos evidencias de que o numero de acidentes muda de 
# acordo com o dia da semana.


### 2

## Queremos verificar a relacao entre o tipo de cancer com a reacao a quimioterapia

## Pela forma como foram coletadas as amostras, vamos realizar um teste de homoge-
## neidade, para verificar sua relacao

## Vamos comparar como um paciente de um tipo reage a quimioterapia

# H0: p11 = p21 = p31 = p41, ... , p13 = p23 = p33 = p43 VS
# H1: pelo menos uma delas diferentes

## Analise descritiva

tabela2 <- matrix(c(51,58,48,26,33,29,42,38,16,13,30,16), nrow = 4, ncol = 3)
colnames(tabela2) <- c('Pouca','Media','Alta')
(rownames(tabela2) <- c("tipo 1", "tipo 2", "tipo 3", "tipo 4"))

## Tabela com as proporcoes 

(tabela.prop <- apply(tabela2, 1, prop))

barplot(tabela.prop, col = c(2,7,4))

# Pelo que vemos na tabela e no grafico, as proporcoes das pessoas com o cancer do
# tipo 4 sao diferentes das demais

chisq.test(tabela2, correct = F)
 
# Adotando um nivel de significancia de 1%, rejeitamos H0, pois obtivemos p-valor
# menor. Ou seja, temos evidencias de que o tipo de cancer afeta a reacao a
# quimioterapia

### 3
base3 <- read_excel("Banco Escalas Psicologia.xls", na = "999")
base3 <- base3[-(215:219),] # retirando as ultimas linhas
base3$Grupo <- ordered(base3$Grupo, labels = c("controle","Trauma","TEPT"))
base3$Sexo <- factor(base3$Sexo, labels = c("M","F"))

## (a)

# Vamos realizar um teste de hipotese para media

## Analise descritiva

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(Idade, na.rm = T), sd = sd(Idade, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = Idade), fill = c(3,4,5))

## Pelo que podemos observar na tabela e no boxplot, as medias parecem proximas

## Teste ANOVA

## Vamos supor independencia
## Vamos verificar normalidade

ggplot(base3, aes(sample = Idade)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)

# Parece ser razoavel supor normalidade para os 3 grupos

# teste

aux = base3$Idade[base3$Grupo=="controle"]
ks.test(aux, "pnorm", mean(aux, na.rm=T), sd(aux, na.rm=T))
aux = base3$Idade[base3$Grupo=="Trauma"]
ks.test(aux, "pnorm", mean(aux, na.rm=T), sd(aux, na.rm=T))
aux = base3$Idade[base3$Grupo=="TEPT"]
ks.test(aux, "pnorm", mean(aux, na.rm=T), sd(aux, na.rm=T))

## Adotando um nivel de significancia de 3%, nao rejeitamos nenhuma das hipoteses
## nulas, portanto, e razoavel supor normalidade para  idade de cada grupo

## Testando variancias iguais

LeveneTest(base3$Idade, base3$Grupo, center = "mean")

## Adotando um nivel de significancia de 3%, nao rejeitamos H0, entao considerare
## mos as variancias iguais.

## Teste ANOVA

teste3a <- aov(Idade~Grupo, data = base3)
summary(teste3a)

## Adotando um nivel de significancia de 3%, rejeitamos H0, ou seja, ha
## evidencias de que pelo menos uma das medias sao diferentes

## Analise de Acompanhamento

PostHocTest(teste3a, method = "bonferroni")

## No teste de Bonferroni temos que dividir o nivel de significancia pelo numero 
## de comparacoes que vamos fazer. Tomamos entao um nivel de significancia de 1%.

## Entramos em contradicao, pois concluimos que as medias comparadas 2 a 2 sao
## todas iguais

## Vamos testar pelo metodo de Duncan

PostHocTest(teste3a, method = "duncan")

## Concluimos entao, com nivel de significancia de 1%, que a idade media dos 
## pacientes controle se difere da idade media dos pacientes com trauma.

## (b)

## Pela forma de coleta, vamos fazer um teste de independencia

# H0: genero e transtorno sao independentes (Pij = Pi.*P.j) vs 
# H1: genero e transtorno nao sao independentes

## Analise descritiva

## tabela com as frequencias

base3 %>% group_by(Sexo, Grupo) %>% filter(Grupo != "controle") %>%
  summarise(freq = n())
(tabela <- matrix(c(23, 26, 50, 57), nrow = 2, ncol = 2))

## tabela com as proporcoes observadas e proporcoes esperadas, sob H0 verdadeiro

tabela3 = base3 %>% group_by(Sexo, Grupo) %>% filter(Grupo != "controle") %>%
  summarise(prop = n()/sum(tabela))
tabela3$prop.esp= c(prop2(tabela,1,1),prop2(tabela,2,1),prop2(tabela,1,2),
                    prop2(tabela, 2,2))

tabela3

## Comparando as proporcoes esperadas e observadas, parece bastante razoavel supor que 
## elas sao independentes

## teste chi-quadrada (Independencia)


chisq.test(tabela, correct = F)

## Adotando um nivel de significancia de 5%, nao rejeitamos H0, entao considerare
## que as variaveis genero e transtorno sao independentes

## (c)

## Analise Descritiva

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(Anos_escolaridade, na.rm = T),
            sd = sd(Anos_escolaridade, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = Anos_escolaridade), fill = c(3,4,5))

# As medias estao proximas, pode ser um indicio de que elas sao iguais

## Precisamos fazer ANOVA 

# Verificando normalidade

ggplot(base3, aes(sample = Anos_escolaridade)) + stat_qq() + stat_qq_line() +
  facet_wrap(~Grupo)

base3$Anos_escolaridade[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Anos_escolaridade[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Anos_escolaridade[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando um nivel de significancia de 5%, rejeitamos H0, ou seja, rejeitamos
# a hipotese de que as variaveis sao provenientes de uma distribuicao normal,
# portanto nao podemos realizar ANOVA.

## (d)

## Analise Descritiva

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(QI, na.rm = T), sd = sd(QI, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = QI), fill = c(3,4,5))

# as medias estao proximas mesmo com uma varibilidade razoavelmente alta, pode ser um in
# dicio de que as medias sao iguais

## Precisamos fazer ANOVA 

## Vamos supor independencia

## Verificando normalidade

ggplot(base3, aes(sample = QI)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)

## Observando o grafico, os dados parecem seguir uma distribuicao normal

base3$QI[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$QI[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$QI[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando um nivel de significancia de 5%, nao rejeitamos H0, ou seja, e razoa-
# vel suporque as variaveis sao provenientes de uma distribuicao normal

## Testando homocedasticidade (variancias iguais)

LeveneTest(base3$QI, base3$Grupo, center = "mean")

## Adotando um nivel de significancia de 5%, nao rejeitamos H0, entao considerare
## mos as variancias iguais.

teste3d = aov(QI~Grupo, data =base3)
summary(teste3d)

# Adotando um nivel de significancia de 5%, rejeitamos H0, ou seja, ha pelo menos
# um par de medias diferente

PostHocTest(teste3d, method = "bonferroni")

# Adotando nivel de significancia 1.66% (correcao), concluimos que a media do
# QI do grupo TEPT e diferente do grupo que nao tem nenhum transtorno, as outras
# sao iguais

## (e)

## Analise Descritiva

# Para RAVLT

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(RAVLT, na.rm = T), sd = sd(RAVLT, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = RAVLT), fill = c(3,4,5))

# Uma das medias (controle) esta um pouco distante das demais, porem temos uma variabili
# consideravel, nao podemos afirmar nada ainda

# Para WCST

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(WCST, na.rm = T), sd = sd(WCST, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = WCST), fill = c(3,4,5))

# Temos medias proximas mesmo com variancia razoavelmente alta, temos indicios de que as
# medias podem ser iguais

# Para Stroop

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(Stroop, na.rm = T), sd = sd(Stroop, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = Stroop), fill = c(3,4,5))

# As medias nao estao muito proximas, mas temos uma certa variabilidade

# Para Digitos

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(Digitos, na.rm = T), sd = sd(Digitos, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = Digitos), fill = c(3,4,5))

# As medias estao proximas, indicio de que elas podem ser iguais

# Para Reproducao visual

base3 %>% group_by(Grupo) %>% 
  summarise(media = mean(`Reproduçao Visual`, na.rm = T),
            sd = sd(`Reproduçao Visual`, na.rm = T))

ggplot(base3, aes(x = Grupo)) + geom_boxplot(aes(y = `Reproduçao Visual`), fill = c(3,4,5))

# As medias nao estao muito proximas, mas temos uma certa variabilidade

## Precisamos fazer ANOVA para as 5 variaveis

## Vamos supor independencia de todas elas

## Verificando normalidade de cada variavel

ggplot(base3, aes(sample = RAVLT)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)
ggplot(base3, aes(sample = WCST)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)
ggplot(base3, aes(sample = Stroop)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)
ggplot(base3, aes(sample = Digitos)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)
ggplot(base3, aes(sample = `Reproduçao Visual`)) + stat_qq() + stat_qq_line() + facet_wrap(~Grupo)

## RAVLT , WCST e reproducao visual parecem estar proximos de uma normal, Stroop e digitos
## nem tanto, mas vamos fazer teste de normalidade para verificar mais precisamente

## Para RAVLT
base3$RAVLT[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$RAVLT[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$RAVLT[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, nao rejeitamos H0 em nenhum dos tres casos, assim
# consideraremos normalidade para os dados de RAVLT para cada grupo

## Para WCST
base3$WCST[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$WCST[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$WCST[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, rejeitamos H0 para o grupo controle, assim nao
# podemos realizar ANOVA para essa variavel, mesmo que as outras sejam normais

## Para Stroop
base3$Stroop[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Stroop[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Stroop[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, nao rejeitamos H0 em nenhum dos tres casos, assim
# consideraremos normalidade para os dados de Stroop para cada grupo

## Para Digitos
base3$Digitos[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Digitos[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$Digitos[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, nao rejeitamos H0 em nenhum dos tres casos, assim
# consideraremos normalidade para os dados de Digitos para cada grupo

## Para Reproducao visual
base3$`Reproduçao Visual`[base3$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$`Reproduçao Visual`[base3$Grupo=="Trauma"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base3$`Reproduçao Visual`[base3$Grupo=="TEPT"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T),sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, nao rejeitamos H0 em nenhum dos tres casos, assim
# consideraremos normalidade para os dados de reproducao visual para cada grupo

# Agora, vamos testar se as variancias sao iguais para RAVLT, Stroop, Digitos e reproducao 
# visual

LeveneTest(base3$RAVLT, base3$Grupo, center = "mean")
LeveneTest(base3$Stroop, base3$Grupo, center = "mean")
LeveneTest(base3$Digitos, base3$Grupo, center = "mean")
LeveneTest(base3$`Reproduçao Visual`, base3$Grupo, center = "mean")

# Adotando nivel de significancia igual a 3%, rejeitamos H0 para Stroop e Digitos, ou seja
# ha evidencias de que as variancias dessas variaveis nao sao iguais quando divididas em
# relacao a grupo. Por outro lado, nao rejeitamos H0 para RAVLT e reproducao visual, 
# assim, consideraremos que suas variancias sao iguais, portanto podemos fazer ANOVA para
# estas duas variaveis.

## ANOVA

teste3e <- aov(RAVLT~Grupo, data = base3)
teste3e2 <- aov(`Reproduçao Visual`~Grupo, data = base3)
summary(teste3e)
summary(teste3e2)

# Adotando nivel de significancia igual a 3%, rejeitamos H0 para os dois testes, ou seja
# temos evidencias de que ha pelo menos um par de medias diferentes em cada uma das varia
# veis

## Vamos verificar quais medias sao diferentes

PostHocTest(teste3e, method = "bonferroni")

# Para RAVLT, adotando nivel de significancia de 1% (correcao), concluimos que somente
# TEPT e trauma possuem medias iguais

PostHocTest(teste3e2, method = "bonferroni")

# Para reproducao visual, nao rejeitamos H0 em nenhum caso, ou seja, o teste indica que 
# as medias sao iguais, entrando em cotradicao com o teste ANOVA

### 4

# criando a base4 que agrupa os pacientes com trauma e tept numa categoria so

base4 <- read_excel("Banco Escalas Psicologia.xls", na = "999")
base4 <- base4[-(215:219),] # retirando as ultimas linhas
base4$Grupo <- cut(base4$Grupo, breaks = c(-Inf,0,Inf), 
                                labels = c("controle","novo grupo"))
base4$Sexo <- factor(base4$Sexo, labels = c("M","F"))

## (a)

# Vamos realizar um teste de hipotese para diferenca de duas medias 

## Analise descritiva

base4 %>% group_by(Grupo) %>% 
  summarise(media = mean(Idade, na.rm = T), sd = sd(Idade, na.rm = T))

ggplot(base4, aes(x = Grupo)) + geom_boxplot(aes(y = Idade), fill = c(3,4))

## Pelo que podemos observar na tabela e no boxplot, as medias parecem nao tao 
## proximas

## Vamos supor independencia
## Vamos verificar normalidade

ggplot(base4, aes(sample = Idade)) + stat_qq() + stat_qq_line() + 
       facet_wrap(~Grupo)

# Parece ser razoavel supor normalidade para os dois grupos
View(base4)
aux1 = base4$Idade[base4$Grupo=="controle"]
ks.test(aux1, "pnorm", mean(aux1, na.rm=T), sd(aux1, na.rm=T), alternative = "two.sided")
aux2 = base4$Idade[base4$Grupo=="novo grupo"]
ks.test(aux2, "pnorm", mean(aux2, na.rm=T), sd(aux2, na.rm=T))

## Adotando um nivel de significancia de 3%, nao rejeitamos nenhuma das hipoteses
## nulas, portanto, e razoavel supor normalidade para idade de cada grupo

## Testando variancias iguais

VarTest(aux1, aux2, alternative = "two.sided")

## Adotando um nivel de significancia de 3%, nao rejeitamos H0, entao considerare-
## mos as variancias iguais.

## Teste de diferenca de medias 

t.test(aux1, aux2, alternative = "two.sided", mu = 0, var.equal = TRUE)

## Adotando um nivel de significancia de 3%, rejeitamos H0, ou seja, ha
## evidencias de que as medias sao diferentes.

## (b)

## Pela forma de coleta, vamos fazer um teste de independencia

# H0: genero e transtorno sao independentes  vs 
# H1: genero e transtorno nao sao independentes

## Analise descritiva

## tabela com as frequencias

base4 %>% group_by(Sexo, Grupo) %>% summarise(freq = n())
(tabela <- matrix(c(21, 49, 37, 107), nrow = 2, ncol = 2, byrow = T))

## tabela com as proporcoes observadas e proporcoes esperadas, sob H0 verdadeiro

tabela4 = base4 %>% group_by(Sexo, Grupo) %>% summarise(prop = n()/sum(tabela))
tabela4$prop.esp= c(prop2(tabela,1,1),prop2(tabela,2,1),prop2(tabela,1,2),
                    prop2(tabela, 2,2))
tabela4

## Comparando as proporcoes esperadas e observadas, parece bastante razoavel supor que 
## elas sao independentes

## teste chi-quadrado (Independencia)

chisq.test(tabela, correct = F)

## Adotando um nivel de significancia de 5%, nao rejeitamos H0, entao considerare
## que as variaveis genero e transtorno sao independentes

## (c)

# Queremos saber se pra cada grupo temos a mesma proporcao de escolaridade

base4$Escolaridade = cut(base4$Anos_escolaridade, breaks = c(0,9,12,20), 
                         labels = c("fundamental", "medio", "superior"))

# Analise descritiva

## tabela com as frequencias

base4 %>% group_by(Grupo, Escolaridade) %>% summarise(freq = n())
tab = table(base4$Grupo, base4$Escolaridade)


## tabela com as proporcoes observadas e proporcoes esperadas, sob H0 verdadeiro

tabela4 = base4 %>% group_by(Escolaridade, Grupo) %>% 
  summarise(prop = n()/sum(tabela))
tabela4$prop.esp= c(prop2(tab,1,1),prop2(tab,1,2),prop2(tab,1,3),
                    prop2(tab,2,1),prop2(tab,2,2),prop2(tab,2,3), 0)
tabela4

## Fazendo o teste de Homogeneidade

# H0: proporcao de Escolaridade e a mesma em cada grupo

chisq.test(tab, correct = F)

## Adotando um nivel de significancia de 5%, rejeitamos H0, entao consideramos 
## que as proporcoes de escolaridade para cada grupo e diferente

## (d)

# Vamos realizar um teste de hipotese para diferenca de duas medias 

## Analise Descritiva

base4 %>% group_by(Grupo) %>% 
  summarise(media = mean(QI, na.rm = T), sd = sd(QI, na.rm = T))

ggplot(base4, aes(x = Grupo)) + geom_boxplot(aes(y = QI), fill = c(3,4))

# as medias estao proximas mesmo com uma varibilidade razoavelmente alta, pode 
# ser um indicio de que as medias sao iguais

## Vamos supor independencia

## Vamos verificar normalidade

ggplot(base4, aes(sample = QI)) + stat_qq() + stat_qq_line() + facet_wrap(
  ~Grupo)

# Parece ser razoavel supor normalidade para os dois grupos

aux1 = base4$QI[base4$Grupo=="controle"]
ks.test(aux1, "pnorm", mean(aux1, na.rm=T), sd(aux1, na.rm=T))
aux2 = base4$QI[base4$Grupo=="novo grupo"]
ks.test(aux2, "pnorm", mean(aux2, na.rm=T), sd(aux2, na.rm=T))

## Adotando um nivel de significancia de 3%, nao rejeitamos nenhuma das hipoteses
## nulas, portanto, e razoavel supor normalidade para idade de cada grupo

## Testando variancias iguais

VarTest(aux1, aux2, alternative = "two.sided")

## Adotando um nivel de significancia de 3%, nao rejeitamos H0, entao considerare-
## mos as variancias iguais.

## Teste de diferenca de medias 

t.test(aux1, aux2, alternative = "two.sided", mu = 0, var.equal = TRUE)

## Adotando um nivel de significancia de 3%, rejeitamos H0, ou seja, ha
## evidencias de que as medias sao diferentes

## (e)

# Vamos analisar se ha evidencias de que as pontuacoes medias dos testes sao 
# iguais pra cada grupo

## Analise descritiva

medias = base4 %>% group_by(Grupo) %>% summarise(ravlt = mean(RAVLT, na.rm=T),
    wcst = mean(WCST, na.rm=T), stroop = mean(Stroop, na.rm=T),
    digitos = mean(Digitos, na.rm=T), repr.vis = mean(`Reproduçao Visual`, na.rm=T))
medias

g1 = ggplot(base4, aes(x = Grupo)) + 
     geom_boxplot(aes(y = RAVLT), fill = "aquamarine3") 
g2 = ggplot(base4, aes(x = Grupo)) + 
     geom_boxplot(aes(y = WCST), fill = "deeppink") 
g3 = ggplot(base4, aes(x = Grupo)) + 
     geom_boxplot(aes(y = Stroop),fill = "chocolate")  
g4 = ggplot(base4, aes(x = Grupo)) + 
     geom_boxplot(aes(y = Digitos),fill = "coral2") 
g5 = ggplot(base4, aes(x = Grupo)) + 
     geom_boxplot(aes(y = `Reproduçao Visual`), fill = "darkseagreen")
grid.arrange(g1, g2, g3, g4, g5)

## As medias parecem razoavelmente proximas, exceto talvez o teste RAVLT e Reprod. vis.
## Vamos analisar melhor

## Precisamos fazer ANOVA para as 5 variaveis

## Vamos supor independencia de todas elas

## Verificando normalidade de cada variavel

g1 = ggplot(base4, aes(sample = RAVLT)) + stat_qq() + stat_qq_line() + 
     facet_wrap(~Grupo)
g2 = ggplot(base4, aes(sample = WCST)) + stat_qq() + stat_qq_line() + 
     facet_wrap(~Grupo)
g3 = ggplot(base4, aes(sample = Stroop)) + stat_qq() + stat_qq_line() + 
     facet_wrap(~Grupo)
g4 = ggplot(base4, aes(sample = Digitos)) + stat_qq() + stat_qq_line() + 
     facet_wrap(~Grupo)
g5 = ggplot(base4, aes(sample = `Reproduçao Visual`)) + stat_qq() + stat_qq_line() +
     facet_wrap(~Grupo)
grid.arrange(g1, g2, g3, g4, g5)

## Para RAVLT
base4$RAVLT[base4$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base4$RAVLT[base4$Grupo=="novo grupo"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, nao rejeitamos H0 em nenhum dos dois casos,
# assim consideraremos normalidade para os dados de RAVLT para cada grupo

## Para WCST
base4$WCST[base4$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base4$WCST[base4$Grupo=="novo grupo"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, rejeitamos H0 para o grupo controle, 
# assim nao podemos realizar ANOVA para essa variavel, mesmo a outra sendo normal

## Para Stroop
base4$Stroop[base4$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base4$Stroop[base4$Grupo=="novo grupo"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, rejeitamos H0 para o novo grupo, 
# assim nao podemos realizar ANOVA para essa variavel, mesmo a outra sendo normal

## Para Digitos
base4$Digitos[base4$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base4$Digitos[base4$Grupo=="novo grupo"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, rejeitamos H0 para o novo grupo, 
# assim nao podemos realizar ANOVA para essa variavel, mesmo a outra sendo normal

## Para Reproducao visual
base4$`Reproduçao Visual`[base4$Grupo=="controle"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")
base4$`Reproduçao Visual`[base4$Grupo=="novo grupo"] %>%
  ks.test(.,"pnorm",mean(.,na.rm=T) ,sd(.,na.rm=T),alternative = "two.sided")

# Adotando nivel de significancia de 3%, rejeitamos H0 para o novo grupo, 
# assim nao podemos realizar ANOVA para essa variavel, mesmo a outra sendo normal

# Agora, vamos testar se as variancias sao iguais para RAVLT

VarTest(base4$RAVLT[base4$Grupo=="controle"], 
        base4$RAVLT[base4$Grupo=="novo grupo"], 
        alternative = "two.sided")

# Adotando nivel de significancia igual a 3%, nao rejeitamos H0 para RAVLT  
# assim, consideraremos que suas variancias sao iguais

## Teste de diferenca de medias 

t.test(base4$RAVLT[base4$Grupo=="controle"], 
       base4$RAVLT[base4$Grupo=="novo grupo"], 
       alternative = "two.sided", mu = 0, var.equal = TRUE)

## Adotando um nivel de significancia de 3%, rejeitamos H0, ou seja, ha
## evidencias de que as medias sao diferentes.

### 5

base5 <- read_excel("EMBRAPA.xls")
base5$Tratamento <- ordered(base5$Tratamento, labels = c("1","2","3","4"))

## (a)

# Vamos fazer ANOVA para comparar a media dos pesos em cada tratamento

## Analise Descritiva

base5 %>% group_by(Tratamento) %>% 
  summarise(media = mean(Peso26, na.rm = T), sd = sd(Peso26, na.rm = T))

ggplot(base5, aes(x = Tratamento)) + geom_boxplot(aes(y = Peso26), fill = c(3,4,5,6))

# Podemos observar que as medias nao estao muito proximas, porem ha uma certa variabilida
# como podemos observar pelo desvios padroes

## Vamos supor independencia

## Verificando normalidade

ggplot(base5, aes(sample = Peso26)) + stat_qq() + stat_qq_line() + facet_wrap(~Tratamento)

## Os pontos nos graficos nao estao muito distantes da reta, vamos fazer o teste de norma
## lidade para tomarmos uma decisao

base5$Peso26[base5$Tratamento == 1] %>%
  ks.test(., "pnorm", mean(., na.rm = T) ,sd(., na.rm = T), alternative = "two.sided")
base5$Peso26[base5$Tratamento == 2] %>%
  ks.test(., "pnorm", mean(., na.rm = T), sd(., na.rm = T), alternative = "two.sided")
base5$Peso26[base5$Tratamento == 3] %>%
  ks.test(., "pnorm", mean(., na.rm = T), sd(., na.rm = T), alternative = "two.sided")
base5$Peso26[base5$Tratamento == 4] %>%
  ks.test(., "pnorm", mean(., na.rm = T), sd(., na.rm = T), alternative = "two.sided")

# Com base num nivel de significancia de 5%, nao rejeitamos H0 em nenhum dos testes, 
# portanto e razoavel supor que os dados sao provenientes de uma normal

## verificando homocedasticidade

LeveneTest(base5$Peso26, base5$Tratamento, center = "mean")

# Com base num nivel de significancia de 5%, nao rejeitamos H0, consideraremos, portanto,
# variancias iguais

# ANOVA

teste5a <- aov(Peso26~Tratamento, data = base5)
summary(teste5a)

# Com base num nivel de significancia de 5%, nao rejeitamos H0, ou seja, o teste indica
# que as medias do peso dos bezerros apos 26 semanas sao iguais.

## (b)

# Precisamos realizar um teste de diferenca de medias para populacoes normais dependentes,
# uma vez que a amostra e pareada.

# Hipoteses  H0: mu26 - mu0 = 88 VS H1: mu26 - mu0 > 88 

# retirando segunda linha que possui NA, pois com ela nao conseguiremos comparar as medias

base5.new <- na.omit(base5)

## Analise descritiva

mean(base5.new$Peso26 - base5.new$Peso0) ; sd(base5.new$Peso26 - base5.new$Peso0)

ggplot(base5.new, aes(x = "diferença de pesos")) +
  geom_boxplot(aes(y = Peso26 - Peso0))

## Pelo que podemos observar pela media amostral e pelo boxplot, a diferenca das medias
## parece ser superior a 88, que e o que vamos testar

## verificando normalidade

ggplot(base5, aes(sample = Peso0)) + stat_qq() + stat_qq_line()
ggplot(base5, aes(sample = Peso26)) + stat_qq() + stat_qq_line()

# Os pontos estao proximos da reta, indicando que as amostras podem vir de uma distribui-
# cao normal

ks.test(base5$Peso0, "pnorm", mean(base5$Peso0, na.rm = T), sd(base5$Peso0, na.rm = T),
        alternative = "two.sided")
ks.test(base5$Peso26, "pnorm", mean(base5$Peso26, na.rm = T), sd(base5$Peso26, na.rm = T),
        alternative = "two.sided")

# Adotando um nivel de significancia de 4%, nao rejeitamos H0 em ambos os testes, ou seja,
# e razoavel supor normalidade para as variaveis de peso do bezerro inicialmente e apos
# 26 semanas

## Teste

t.test(x = base5.new$Peso26, y = base5.new$Peso0, mu = 88, alternative = "greater",
       paired = T, correct = F)

# Com base num nivel de significancia de 4%, rejeitamos H0, ou seja, o teste indica que
# a diferenca das medias do peso dos bezerros cresceu mais de 88 kg apos 26 semanas

### 6

base6 <- read_csv2("Basevulnerabilidade.csv")
base6$situacao <- factor(base6$situacao, labels = c("alugado", "proprio"))
base6$escmae <- ordered(base6$escmae, labels = c("baixa", "media", "alta"))

## (a)

## Teste de independencia

# Para situacao de moradia e vulnerabilidade

# H0: situacao de moradia e vulnerabilidade sao independentes
# H1: situacao de moradia e vulnerabilidade nao sao independentes

## Analise descritiva

# Vamos comparar as proporcoes totais observadas com as proporcoes esperadas,
# supondo H0 verdadeiro

(aux <- table(base6$vulnerabilidade, base6$situacao))

tabela6 <- base6 %>% group_by(vulnerabilidade, situacao) %>%
  summarise(freq.obs <- n()/nrow(base6))

tabela6$prop.esp <- c(prop2(aux,1,1), prop2(aux,1,2), prop2(aux,2,1),prop2(aux,2,2))
tabela6

# Pelo que observamos na tabela, as proporcoes nao estao mt proximas, o que me leva
# a pensar que elas nao serao independentes, ou seja, que elas terao alguma relacao

# Teste

chisq.test(aux, correct = F)

# Adotando nivel de significancia de 5%, rejeitamos H0, ou seja, o teste indica 
# que as variaveis vulnerabilidade e situacao de moradia possuem relacao

# Para situacao de moradia e vulnerabilidade

# H0: escolaridade da mae e vulnerabilidade sao independentes
# H1: escolaridade da mae e vulnerabilidade nao sao independentes

## Analise descritiva

# Vamos comparar as proporcoes totais observadas com as proporcoes esperadas,
# supondo H0 verdadeiro

(aux <- table(base6$escmae, base6$vulnerabilidade))

(tabela6 <- base6 %>% group_by(vulnerabilidade, escmae) %>%
  summarise(freq.obs = n()))

tabela6$freq.esp <- c(prop2(aux, 1, 1), prop2(aux, 2, 1), prop2(aux, 3, 1),
                      prop2(aux, 1, 2), prop2(aux, 2, 2), prop2(aux, 3, 2))*40

tabela6

# Pelo que observamos na tabela, as proporcoes estaoproximas, o que me leva a 
# pensar que elas serao independentes, ou seja, que elas terao alguma relacao

# porem dessa forma, temos frequencias esperadas menores que 5, assim nao consegui
# remos ter uma boa aproximacao da distribuicao para realizar o teste de hipotese
# Para resolver isso, vamos juntar o grupo de escolaridade da mae media e alta

base6.new <- read_csv2("Basevulnerabilidade.csv")
base6.new$escmae <- cut(base6.new$escmae, breaks = c(-Inf, 1, Inf),
                  labels = c("baixa", "media/alta"))

(aux <- table(base6.new$escmae, base6.new$vulnerabilidade))

(tabela6 <- base6.new %>% group_by(vulnerabilidade, escmae) %>%
    summarise(freq.obs = n()))

tabela6$freq.esp <- c(prop2(aux, 1, 1), prop2(aux, 2, 1), prop2(aux, 1, 2),
                      prop2(aux, 2, 2))*40

tabela6

## Agora sim !!

# Teste

chisq.test(aux, correct = F)

# Adotando nivel de significancia de 5%, nao rejeitamos H0, ou seja, o teste 
# indica  que as variaveis vulnerabilidade e situacao de moradia nao possuem
# relacao

## (b)
# Vamos comparar as medias de anos de escolaridade entre pessoas com e sem 
# vulnerabilidade social.

# Analise descritiva

(tabela = base6 %>% group_by(vulnerabilidade) %>% summarise(media = mean(escolaridade, na.rm=T), sd = sd(escolaridade, na.rm=T)))

ggplot(base6, aes(x=vulnerabilidade, y=escolaridade)) + geom_boxplot()

# Testando normalidade pra cada grupo

base6$escolaridade[base6$vulnerabilidade == "Sim"] %>%
  ks.test(., "pnorm", mean = mean(., na.rm = T), sd = sd(.,na.rm = T), alternative = "two.sided")

base6$escolaridade[base6$vulnerabilidade == "Nao"] %>%
  ks.test(., "pnorm", mean = mean(., na.rm = T), sd = sd(.,na.rm = T), alternative = "two.sided")

# Conclusao: com base num ns de 5%, concluimos q os dois grupo seguem distribuicao normal

# Verificando se variancias sao iguais

VarTest(base6$escolaridade[base6$vulnerabilidade == "Sim"],
        base6$escolaridade[base6$vulnerabilidade == "Nao"],
        alternative = "two.sided", correct = F)
var.test(base6$escolaridade[base6$vulnerabilidade == "Sim"],
         base6$escolaridade[base6$vulnerabilidade == "Nao"],
         alternative = "two.sided", correct = F)

# Conclusao: com base num ns de 5% , temos que as variancias nao sao iguais.

# Teste

# Hipoteses: H0: mu1 = mu2

t.test(base6$escolaridade[base6$vulnerabilidade == "Sim"],
       base6$escolaridade[base6$vulnerabilidade == "Nao"],
       var.equal=F, correct=F, alternative="two.sided")

# Conclusao: com base num ns de 5%, concluimos que as medias nao sao iguais.

## Estudo I
library(readxl)
pappagu = read_excel("Banco Cancer Mama.xls", na="NA")
pappagu$cancer = factor(pappagu$cancer, labels = c("Nao", "Sim"))
pappagu$menstruacao = factor(pappagu$menstruacao, labels = c("Nao", "Sim"))
pappagu$grauparentesco = factor(pappagu$grauparentesco, labels = c("3o Grau", "2o Graus", "1o Grau"))

## 7

# Teste de homogeneidade

# Analise descritiva

(tab <- table(pappagu$cancer, pappagu$menstruacao))
(tab.analise <- pappagu %>% group_by(cancer, menstruacao) %>%
    summarise(freq.obs = n()))

tab.analise$freq.esp = c(sum(tab[1,])*sum(tab[,1])/sum(tab),
                         sum(tab[1,])*sum(tab[,2])/sum(tab),
                         sum(tab[2,])*sum(tab[,1])/sum(tab),
                         sum(tab[2,])*sum(tab[,2])/sum(tab))
tab.analise

# Teste chi - quadrado

chisq.test(tab, correct = F)
 
# Conclusao: com base num ns de 5%, temos que as proporcoes de pessoas que 
# tiveram ou nao menstruacao antes dos 12 anos comparada com ter ou nao cancer 
# nao sao homogeneas, ou seja, nao sao distribuidas de forma igual. 

## (8)
pappagu.can = pappagu %>% filter(cancer=="Sim")

# Analise descritiva
tabelas.p = tibble(nome = c("antes", "depois"),
                   media = c(mean(pappagu.can$antes), mean(pappagu.can$depois)),
                   sd = c(sd(pappagu.can$antes), sd(pappagu.can$depois)))
tabelas.p

pappagu.can$difer = pappagu.can$antes - pappagu.can$depois
ggplot(pappagu.can, aes(x = "diferenca")) + geom_boxplot(aes(y = difer))

# Testando normalidade
ks.test(pappagu.can$depois, "pnorm", mean(pappagu.can$depois), 
        sd(pappagu.can$depois), alternative = "two.sided")
ks.test(pappagu.can$antes, "pnorm", mean(pappagu.can$antes), 
        sd(pappagu.can$antes), alternative = "two.sided")
# as duas amostras sao normais. 

t.test(pappagu.can$antes, pappagu.can$depois, alternative = "greater", paired = T)
 
# Concluimos entao que o tratamento nao foi eficiente.

## 9
# H0: mupcc = mupsc x H1: mupcc > mupsc
# Analise descritiva
pappagu %>% group_by(cancer) %>% summarise(media = mean(peso), sd = sd(peso))
ggplot(pappagu, aes(x=cancer, y=peso)) + geom_boxplot()
# Parece que as medias estao proximas uma da outra

# normalidade
pappagu$peso[pappagu$cancer=="Sim"] %>% 
  ks.test(., "pnorm", mean(., na.rm=T), sd(., na.rm=T), alternative = "two.sided")
pappagu$peso[pappagu$cancer=="Nao"] %>% 
  ks.test(., "pnorm", mean(., na.rm=T), sd(., na.rm=T), alternative = "two.sided")
# Parece que os dois grupos seguem distribuicao normal

# homocedasticidade
VarTest(pappagu$peso[pappagu$cancer=="Sim"], pappagu$peso[pappagu$cancer=="Nao"], 
        alternative = "two.sided")
# Os grupos possuem variancias iguais

# Teste para diferenca de medias
t.test(pappagu$peso[pappagu$cancer=="Sim"], pappagu$peso[pappagu$cancer=="Nao"],
       alternative = "greater", var.equal = T)
# com base num ns de 5%, concluimos que as medias dos dois grupos sao iguais

## 10
# Queremos comparar duas variaveis categoricas. Pelo jeito como foi coletado os dados, 
# vamos fazer um teste de homogeneidade.

# Analise Descritiva
tabela = table(pappagu$cancer, pappagu$grauparentesco)
tabela

tab.prop = matrix(c(prop(tabela[1,]), prop(tabela[2,])), nrow = 2, byrow = T)
rownames(tab.prop) = c("Nao", "Sim")
colnames(tab.prop) = c("3o Grau", "2o Graus" , "1o Grau")
tab.prop

ggplot(pappagu, aes(x=cancer)) + geom_bar(aes(fill=grauparentesco), position = "fill")

# H0: as proporcoes de pacientes com parentesco de 3o 2o e 1o grau se distribuem de 
# maneira igual entre pacientes com cancer e sem.
chisq.test(tabela, correct = F)
# com base num ns de 5%, rejeitamos H0, ou seja, acreditamos que as proporcoes de 
# pacientes com parentesco de 3o 2o e 1o grau nao se distribuem de maneira igual
# entre pacientes com cancer e sem.


