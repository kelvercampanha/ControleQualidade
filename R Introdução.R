### Introdução a Linguagem R ###

# R é uma linguagem de programação multi-paradigma orientada a objetos, 
# funcional, dinâmica, fracamente tipada, de alto nível e voltada à manipulação, 
# análise e visualização de dados.

### Compiladores ###
# R é open source e pode ser obtido na página oficial do Projeto R.
# Outra opção popular é a IDE do R Studio, que é um ambiente integrado de 
# desenvolvimento para o R, mais amigável e mais funcional, 
# além de possuir uma versão free.

### Primeiros códigos ###
# É possível trabalhar escrevendo códigos diretamente no prompt do R,
# ou utilizando scripts com blocos de código como esse material.

### Pacotes ###
# O R possui algumas funções preexistentes em seus pacotes default (base e stats, por exemplo),
# Mas eventualmente as análises desejadas requerem a instalação de pacotes adicionais. Ex: install.packages(ggplot2);library(ggplot)
# Ao carregar um pacote R, todas as suas funções são importadas diretamente
# Para obter ajuda sobre alguma função, pasta digitar ?+função que se deseja ajuda.
# Ex: ?lm (lm é função para ajuste de regressão linear)

### Criando objetos no R ###
# Basicamente, tudo que instanciamos será atribuído para um objeto.
# Obs: Valores podem ser atribuídos com os caracteres "<-" e "=", R é case sensitive, 
# os objetos podem ser consultados com a função print ou chamados diretamente,
# e os principais tipos de variáveis são character, numeric, integer e logic.
# Os principais tipos de objetos são os vetores, matrizes, dataframe, listas e funções.

#Objetos e atribuiçõrd
x=10
x
print(x)

y<-10
y
print(y)

#Tipos variáveis
class(x)
y<-as.character(y);class(y)
x==10

#Tipos objetos
v=rep(1,5);v
m=matrix(seq(1,6,1),3,2,T);m

library(datasets)
data(iris)
summary(iris)

lista=list(vetor=v,matriz=m,df=iris);lista
lista$vetor
lista[1]

#dados na memória
rm(x)
x
y
rm(list=ls(all=T));y

#operações
#calculadora
5+8
sqrt(121)
x <- 1:5; x
x <- 1/5; x

#matricial
x1=c(4,8,0)
x2=c(9,1,5)
x1+x2
x1*x2
x1%*%x2
sort(x1)

#features
data("iris")
head(iris)
iris$sum=rowSums(iris[,c(1:4)])
iris$mean=iris$sum/4
head(iris)

as.data.frame(rbind(x1,x2))
rm(list=ls(all=T))

#gráficos
data("iris")

boxplot(iris$Petal.Length~iris$Species)
boxplot(iris$Petal.Length~iris$Species,xlab="Espécies",ylab="Tamanho da pétala",
        main="Tamanho da Pétala por Espécie",col=c("royalblue4",'darkgreen','gold'))

plot(iris$Petal.Length, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width,col=c("royalblue4",'darkgreen','gold')[iris$Species],
     pch=19,xlab="Comprimento da Pétala",ylab="Largura da pétala",main="Gráfico de Dispersão")
legend("topleft", legend = levels(iris$Species),lwd = 3, lty = c(1, 1, 1), 
       col=c("royalblue4",'darkgreen','gold'),bty="n")

data("mtcars")
barplot(mtcars$hp)
mtcars=mtcars[order(-mtcars$hp),]
par(mar=c(7,5,3,1))
barplot(mtcars$hp,names=rownames(mtcars),las=2,cex.names = 0.7,ylab="HP",
        ylim=c(0,400),col="royalblue4",main="Carros com mais cavalos de potência")
abline(h=200,col="red",lty=2)
par(mar=c(5.1,4.1,4.1,2.1))

#Há uma grande possibilidade de outros tipos de gráfico, tal como outras bibliotecas
#que permitem customizações diferenciadas. 

rm(list=ls(all=T))

#Estruturas condicionais

nota=7
if (nota>=7){
  print("Aprovado")
}else{
    print("Reprovado")
}


nota=6
ifelse(nota>=7,"Aprovado","Reprovado")

#Loops
for (i in seq(1,10,1))
{
  if(i%%2==0)
  {
    print(i)
  }
}


soma=1
j=0
while (soma<100)
{
  soma=soma+2^j
  print(soma)
  j=j+1
  print(j)
  print("-----")
}
  

#Funções
sena=function(n)
{
  num=seq(1,60,1)
  amostra=sample(num,n,replace=F)
  amostra
}
sena(6)

histograma=function(x)
{
  hist(x,col="royalblue4",xlab="x",ylab="Frequência",main="",breaks = 10)
}


#Testes de hipóteses
par(mfrow=c(1,2))
set.seed(123)
x1=rnorm(1000,0,1)
shapiro.test(x1)
histograma(x1)
x2=rexp(1000)
shapiro.test(x2)
histograma(x2)

t.test(x1,x2)
t.test(x1,mu=1)
