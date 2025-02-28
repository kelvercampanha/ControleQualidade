### Introdu��o a Linguagem R ###

# R � uma linguagem de programa��o multi-paradigma orientada a objetos, 
# funcional, din�mica, fracamente tipada, de alto n�vel e voltada � manipula��o, 
# an�lise e visualiza��o de dados.

### Compiladores ###
# R � open source e pode ser obtido na p�gina oficial do Projeto R.
# Outra op��o popular � a IDE do R Studio, que � um ambiente integrado de 
# desenvolvimento para o R, mais amig�vel e mais funcional, 
# al�m de possuir uma vers�o free.

### Primeiros c�digos ###
# � poss�vel trabalhar escrevendo c�digos diretamente no prompt do R,
# ou utilizando scripts com blocos de c�digo como esse material.

### Pacotes ###
# O R possui algumas fun��es preexistentes em seus pacotes default (base e stats, por exemplo),
# Mas eventualmente as an�lises desejadas requerem a instala��o de pacotes adicionais. Ex: install.packages(ggplot2);library(ggplot)
# Ao carregar um pacote R, todas as suas fun��es s�o importadas diretamente
# Para obter ajuda sobre alguma fun��o, pasta digitar ?+fun��o que se deseja ajuda.
# Ex: ?lm (lm � fun��o para ajuste de regress�o linear)

### Criando objetos no R ###
# Basicamente, tudo que instanciamos ser� atribu�do para um objeto.
# Obs: Valores podem ser atribu�dos com os caracteres "<-" e "=", R � case sensitive, 
# os objetos podem ser consultados com a fun��o print ou chamados diretamente,
# e os principais tipos de vari�veis s�o character, numeric, integer e logic.
# Os principais tipos de objetos s�o os vetores, matrizes, dataframe, listas e fun��es.

#Objetos e atribui��rd
x=10
x
print(x)

y<-10
y
print(y)

#Tipos vari�veis
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

#dados na mem�ria
rm(x)
x
y
rm(list=ls(all=T));y

#opera��es
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

#gr�ficos
data("iris")

boxplot(iris$Petal.Length~iris$Species)
boxplot(iris$Petal.Length~iris$Species,xlab="Esp�cies",ylab="Tamanho da p�tala",
        main="Tamanho da P�tala por Esp�cie",col=c("royalblue4",'darkgreen','gold'))

plot(iris$Petal.Length, iris$Petal.Width)
plot(iris$Petal.Length, iris$Petal.Width,col=c("royalblue4",'darkgreen','gold')[iris$Species],
     pch=19,xlab="Comprimento da P�tala",ylab="Largura da p�tala",main="Gr�fico de Dispers�o")
legend("topleft", legend = levels(iris$Species),lwd = 3, lty = c(1, 1, 1), 
       col=c("royalblue4",'darkgreen','gold'),bty="n")

data("mtcars")
barplot(mtcars$hp)
mtcars=mtcars[order(-mtcars$hp),]
par(mar=c(7,5,3,1))
barplot(mtcars$hp,names=rownames(mtcars),las=2,cex.names = 0.7,ylab="HP",
        ylim=c(0,400),col="royalblue4",main="Carros com mais cavalos de pot�ncia")
abline(h=200,col="red",lty=2)
par(mar=c(5.1,4.1,4.1,2.1))

#H� uma grande possibilidade de outros tipos de gr�fico, tal como outras bibliotecas
#que permitem customiza��es diferenciadas. 

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
  

#Fun��es
sena=function(n)
{
  num=seq(1,60,1)
  amostra=sample(num,n,replace=F)
  amostra
}
sena(6)

histograma=function(x)
{
  hist(x,col="royalblue4",xlab="x",ylab="Frequ�ncia",main="",breaks = 10)
}


#Testes de hip�teses
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
