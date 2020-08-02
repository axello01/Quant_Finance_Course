###############
### CLASE 2 ###
###############

BirdData <- c(Wingcrd, Tarsus, Head, Wt)
Id <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4)
Id

Id <- rep(c(1, 2, 3, 4), each = 8)
Id <- rep(1 : 4, each = 8)
Id

a <- seq(from = 1, to = 4, by = 1)
a

rep(a, each = 8)

VarNames <- c("Wingcrd", "Tarsus", "Head", "Wt")
VarNames

Id2 <- rep(VarNames, each = 8)
Id2

# Combinaci??n de objetos por filas o columnas
Z <- cbind(Wingcrd, Tarsus, Head, Wt)
Z
str(Z)
class(Z)

# Para acceder a elementos
Z[, 1]        # Primera columna
Z[1 : 8, 1]   # Primera columna
Z[2, ]	      # Segunda fila
Z[2, 1:4]     # Segunda fila

# the following commands are all valid:
Z[1, 1]
Z[, 2 : 3]    # Segunda y tercer fila
X <- Z[4, 4]
Y <- Z[, 4]
W <- Z[, -3]
D <- Z[, c(1, 2)]
E <- Z[, c(-1, -3)]

# Dimensi??n
dim(Z)
dim(Z)[1]
nrow <- dim(Z)[1]
nrow

# Funci??n rbind(). Combina las variables de tal manera que 
# la salida se estructura en forma de filas
Z2 <- rbind(Wingcrd, Tarsus, Head, Wt)
Z2



##############################
# Funciones
rm(list = ls())

die <- 1:6
round(3.1415) 
factorial(3) 
mean(1:6) 
mean(die) 
round(mean(die)) 
sample(x = 1:6, size = 1)
sample(x = 1:6, size = 2)
sample(x = die, size = 9, replace = T)

# Argumentos
args(sample)
help(sample)

sample(x = 1:6, size = 3) 
args(sample)

# Sin replacement
sample(die, size = 2, replace = TRUE) 

# Finalmente, nuestra primer simulaci??n en R
two_dice <- sample(die, size = 2, replace = TRUE) 
two_dice
str(two_dice)
sum(two_dice)

roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

die <- 1000:2000
roll()  # Se puede pensar en los par??ntesis como el "disparador" 
# que hace que R ejecute la funci??n
roll    # Sin los par??ntesis R muestra el c??digo de la funci??n

# Argumentos
roll2 <- function() {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll2()
bones <- 1:10

roll3 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll3()

# Valores por defecto
roll4 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
roll4(1:20)

# Script
## Extract function
sample(die, size = 2, replace = TRUE) 

# Paquetes y gr??ficos
install.packages("ggplot2")
library("ggplot2")

x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
y<-x^3
qplot(x, y)

x<-c(1,2,2,2,3,3)
qplot(x, binwidth = 1)
x2<-c(1,1,1,1,1,2,2,2,2,3,3,4)
qplot(x2, binwidth = 1)

# Simulaci??n dados
replicate(3, 1 + 1)
replicate(10, roll())
rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)
mean(rolls)
sd(rolls)
hist(rolls,freq=FALSE,ylim=c(0,0.5),col="lightsalmon",main="Histograma",sub="Datos simulados de una N(170,12)")
curve(dnorm(x,mean(rolls),1),xlim=c(2,12),ylim=c(0,0.5), col="blue",lwd=2,add=TRUE)

# Hemos notado que los resultados nunca se repiten, como podemos hacer esto?
?sample

roll3 <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE,
                 prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}
rolls <- replicate(10000, roll3())
qplot(rolls, binwidth = 1)

###############
### CLASE 3 ###
###############

die<-c(1,2,3,4,5,6)
die
is.vector(die)
length(die)
typeof(die) 

# Integer
int <- c(-1L, 2L, 4L) 
int
typeof(int) 

# Texto
text <- c("Hello", "World")
text
typeof(text)
typeof("Hello")

# Logical
3>4 
logic <- c(TRUE, FALSE, TRUE)
logic
typeof(logic)
typeof(F)

#Atributos
attributes(die) 
names(die) <- c("one", "two", "three", "four", "five", "six") 
names(die)
attributes(die)
die+1
names(die) <- NULL 

#Dimensi??n
dim(die) <- c(2, 3) # Matriz 2 ?? 3 (2 filas y 3 columnas)
dim(die) <- c(3, 2) 
dim(die) <- c(1, 2, 3) 

#Matrix
m <- matrix(die, nrow = 2)
m
m <- matrix(die, nrow = 2, byrow = TRUE)
m

# Array
ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3)) 
ar

#Clase
die <- 1:6 
typeof(die)
class(die) 

dim(die) <- c(2, 3)
typeof(die)
class(die) 
class("Hello")
class(5)

# Time
now <- Sys.time()
now
typeof(now) 
class(now)
unclass(now) 

mil <- 1000000
mil
class(mil) <- c("POSIXct", "POSIXt")
mil

#Factores
gender <- factor(c("male", "female", "female", "male"))
typeof(gender)
attributes(gender)
unclass(gender)
as.character(gender)

# Listas
list1 <- list(100:130, "R", list(TRUE, FALSE))
list1
str(list1)
list1[1]
list1[[1]]
list1[[1]][2]

# Data frame
df <- data.frame(face = c("ace", "two", "six"), suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df 
typeof(df) 
class(df)
str(df)
df <- data.frame(face = c("ace", "two", "six"),
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3), stringsAsFactors = FALSE) 

deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",  
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",  
           "nine", "eight", "seven", "six", "five", "four", "three", "two", 
           "ace", "king", "queen", "jack", "ten", "nine", "eight", "seven", 
           "six", "five", "four", "three", "two", "ace", "king", "queen", "jack", 
           "ten", "nine", "eight", "seven", "six", "five", "four", "three", 
           "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
           "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts"),
  value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13
          ,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1) 
)

#Importar datos
getwd() 

head(deck) 
View(deck)

#Guardar datos
write.csv(deck, file = "cards.csv", row.names = FALSE) 

#Datos
deck[ , ] 	# mazo
deck[1, 1] 

# para traer la primer fila:
deck[1, c(1, 2, 3)] 
deck[1, 1:3] 

#podemos guardar estos valores:
new <- deck[1, c(1, 2, 3)] 

# Si el n??mero en nuestro ??ndice, R trae m??s de una vez el resultado:
deck[c(1, 1), c(1, 2, 3)] 

# La indexaci??n sirve para cualquier tipo de datos:
vec<-c(6,1,3,6,10,5)
vec[1:3]
str(deck)
str(deck[1:2, 1:2])
str(deck[1:2, 1])
str(deck[1:2, 1, drop = FALSE])

#Negativos
deck[-1, 1:3] 

# Valores l??gicos
deck[1, c(TRUE, TRUE, FALSE)] 
rows<-c(TRUE,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F, F,F,F,F,F,F,F,F,F,F,F,F,F,F) 

# nombres
deck[1, c("face", "suit", "value")] 

# Ejercicio 2 ??? Repartir una carta
deal <- function(cards) {
  cards[1, ] 
} 

deal(deck)

# Ejercicio 3 ??? Mezclar el mazo
deck2 <- deck[1:52, ]
head(deck2) 

deck3 <- deck[c(2, 1, 3:52), ]
head(deck3) 

random <- sample(1:52, size = 52)

deck4 <- deck[random, ]
head(deck4)

# Ejercicio 4 ??? Nueva funci??n ???shuffled???
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52) 
  cards[random, ] 
} 

deal(deck) # ahora Podemos mezclar las cartas antes de repartir
deck2 <- shuffle(deck)
deal(deck2)

# Modificando valores
deck2 <- deck
# ejemplo:
vec<-c(0,0,0,0,0,0)
vec
vec[1] 
vec[1] <- 1000
vec

# tambi??n podemos cambiar multiples valores:
vec<-c(0,0,0,0,0,0)
vec[c(1, 3, 5)] <- c(1, 1, 1)
vec
vec[4:6] <- vec[4:6] + 1 
vec

# finalmente tambi??n podemos crear nuevos valores:
vec[7] <- 0
vec

deck2$new <- 1:52
head(deck2) 

# obviamente si queremos podemos borrar la nueva columna:
deck2$new <- NULL 
head(deck2) 

deck2[c(13, 26, 39, 52), 3]
deck2$value[c(13, 26, 39, 52)]

deck2$value[c(13, 26, 39, 52)] <- c(14, 14, 14, 14)
deck2$value[c(13, 26, 39, 52)] <- 14

head(deck2, 13)

###############
### CLASE 4 ###
###############

# Mezclando el mazo
# Test l??gicos

1>2 
1>c(0,1,2)
c(1,2,3)==c(3,2,1) 

1 %in% c(3, 4, 5)
c(1, 2) %in% c(3, 4, 5)
c(1, 2, 3) %in% c(3, 4, 5)
c(1, 2, 3, 4) %in% c(3, 4, 5)

# Ejercicio 5 ??? Encontrando Ases
deck2$face
deck2$face == "ace"
sum(deck2$face == "ace") 
deck3$face == "ace"
deck3$value[deck3$face == "ace"] 
deck3$value[deck3$face == "ace"] <- 14 
head(deck3)

# Pongamos subconjuntos l??gicos para usar con un nuevo juego: hearts.
# En corazones, cada carta tiene un valor de cero:
deck4 <- deck
deck4$value <- 0
head(deck4, 13)

# Encontrando la carta
deck4$suit == "hearts"

# Seleccionamos las cartas de corazones:
deck4$value[deck4$suit == "hearts"] 

# Finalmente asignamos los valores:
deck4$value[deck4$suit == "hearts"] <- 1 

# podemos ver que los valores se actualizaron:
deck4$value[deck4$suit == "hearts"] 

# En este juego, la reina de espadas ???queen of spades??? tiene el 
# valor m??s inusual de todos: vale 13 puntos. 
# C??mo encontramos todas las reinas?:
deck4[deck4$face == "queen", ] 

# Pero son demasiadas. Por otro lado, puedes encontrar todas las 
# cartas en espadas:
deck4[deck4$suit == "spades", ]

# Operador Booleano
W <- c(-1,0,1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday") 

W>0
10<x&x<20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

a<-c(1,2,3)
b<-c(1,2,3)
c<-c(1,2,4) 
a==b
b==c
a==b&b==c

# En nuestro juego de naipes:
deck4$face == "queen" & deck4$suit == "spades" 

# Creamos un nuevo objeto
queenOfSpades <- deck4$face == "queen" & deck4$suit == "spades"

deck4[queenOfSpades, ] 

deck4$value[queenOfSpades] 

deck4$value[queenOfSpades] <- 13
deck4[queenOfSpades, ] 

# Consideremos un ??ltimo juego, el blackjack
deck5 <- deck 
head(deck5, 13)

# Puede cambiar el valor de las cartas de la cara en un solo 
# golpe con %in%:
facecard <- deck5$face %in% c("king", "queen", "jack") 
deck5[facecard, ]

deck5$value[facecard] <- 10
head(deck5, 13)

# Informaci??n perdida
1+NA
c(NA, 1:50)
mean(c(NA, 1:50)) 
mean(c(NA, 1:50), na.rm = TRUE) 

# Problemas con NAs
NA==NA 
c(1,2,3,NA)==NA 

# Para hacer esto tenemos la funci??n especial is.NA: 
is.na(NA) 
vec <- c(1, 2, 3, NA) 
is.na(vec)

################
################
####Entornos####
################
################

deal <- function(cards) {
  cards[1, ] 
} 

deal(cards = deck)

shuffle <- function(cards) { 
  random <- sample(1:52, size = 52) 
  cards[random, ] 
} 

head(shuffle(deck))
head(deck)

library(pryr)
parenvs(all = TRUE)

as.environment("package:stats") 

# 3 entornos con acceso directo:
globalenv()
baseenv()
emptyenv()
parent.env(globalenv())
parent.env(emptyenv())

# Viendo objetos dentro de cada entorno
ls(emptyenv())
ls()
ls(globalenv())

head(deck,3)

# el base tiene muchos !
ls(baseenv())

# podemos buscar
head(globalenv()$deck, 3)

# Y podemos usar la funci??n assign para guardar un objeto en un ambiente particular:
new <- "Hello Global"
assign("new", "Hello Global", envir = globalenv())

# Entorno Activo
environment()

# Asignaci??n
new <- "Hello Global"
new <- "Hello Active"

# Reglas de B??squeda
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE) 
  sum(dice)
}

roll
roll()
roll(dados)

die <- 20:80
rm(die)

roll2 <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE) 
  list(suma.2d = sum(dice),
       ra.in = environment())
}

roll2()



# Creamos una funci??n que nos muestra los ???entornos de ejecuci??n???:
show_env <- function(){ 
  list(entorno.ejecucion = environment(),
       entorno.padre = parent.env(environment()),
       objetos.entorno = ls.str(environment()))
  }

# Crear?? un nuevo entorno cada vez que ejecutemos la funci??n
show_env()

# Entorno de origen
environment(show_env)
environment(roll)
# Recordemos el paquete que cargamos para utilizar la funci??n "parenvs"
environment(parenvs)

# Objetos y entorno
show_env <- function(){
  a<-1
  b<-2
  c<-3
  list(entorno.ejecucion = environment(),
       entorno.padre = parent.env(environment()),
       objetos.entorno = ls.str(environment()))
}
show_env()

# Argumentos
foo <- "take me to your runtime"
foo

show_env <- function(x = foo){ 
  list(entorno.ejecucion = environment(),
       entorno.padre = parent.env(environment()),
       objetos.entorno = ls.str(environment()))
}

show_env()
foo <- "take me to your runtime again"

show_env()


# Nuestra funci??n
# si no tengo cargado deck lo busca en el ent global y no lo encuentra
deal <- function() { 
  deck <- data.frame(
    face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",  
             "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",  
             "nine", "eight", "seven", "six", "five", "four", "three", "two", 
             "ace", "king", "queen", "jack", "ten", "nine", "eight", "seven", 
             "six", "five", "four", "three", "two", "ace", "king", "queen", "jack", 
             "ten", "nine", "eight", "seven", "six", "five", "four", "three", 
             "two", "ace"),
    suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
             "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
             "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
             "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", 
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
             "diamonds", "diamonds", "hearts", "hearts", "hearts", "hearts", 
             "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
             "hearts", "hearts"),
    value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13
            ,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1) 
  )
  deck
}

rm(deck)
head(deal())
environment(deal)

# borrar la 1era carta
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",  
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",  
           "nine", "eight", "seven", "six", "five", "four", "three", "two", 
           "ace", "king", "queen", "jack", "ten", "nine", "eight", "seven", 
           "six", "five", "four", "three", "two", "ace", "king", "queen", "jack", 
           "ten", "nine", "eight", "seven", "six", "five", "four", "three", 
           "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
           "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
           "diamonds", "diamonds", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
           "hearts", "hearts"),
  value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13
          ,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1) 
)

deck<-DECK
DECK <- deck
head(DECK)
deck <- deck[-1, ]
head(deck, 3)

rm(deck)

# nuevo codigo
deal <- function() { 
  card <- deck[1, ] 
  deck <- deck[-1, ]
  #deck_pepe <- deck[-1, ]
  card 
} 

deal()

# finalmente
deal <- function(){
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card 
}

deal()

head(DECK)

head(deck)
head(deck)
head(deck)

# la funci??n shuffle
shuffle <- function(cards) { 
  random <- sample(1:52, size = 52) 
  cards[random, ] 
} 

head(deck,3)
a <- shuffle(deck)
head(deck, 3)
head(a, 3)

# Finalmente
shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}

shuffle()
deal()
deal()

# Guardando la informaci??n en el "entorno de ejecuci??n"
ls(globalenv())

setup <- function(deck){
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card 
  }  
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
}
setup()
setup
ls(globalenv())
environment(setup)
parent.env(environment(setup))
ls(environment(setup))
ls.str(environment(setup))

#########################
##### PARA VER EL ENTORNO
#########################
setup2 <- function(){
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card 
  }  

  SHUFFLE <- function(){
      random <- sample(1:52, size = 52)
      assign("deck", DECK[random, ], envir = globalenv())
  }
  list(ran.in = environment(), 
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
setup2()

# Vamos a pedirle que devuelva DEAL y SHUFFLE
setup <- function(deck){
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = globalenv())
    card 
  }  
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = globalenv())
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
cards

cards$deal
cards$shuffle
deal <- cards$deal
shuffle <- cards$shuffle
deal
shuffle

# Entornos
environment(deal)
environment(shuffle)

### Finalmente entorno cerrado
# Vamos a pedirle que devuelva DEAL y SHUFFLE
setup3 <- function(deck){
  DECK <- deck
  
  DEAL <- function(){
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card 
  }  
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup3(deck)
deal <- cards$deal
shuffle <- cards$shuffle

rm(deck)
shuffle()
deal()

##############
##############
#TRAGAMONEDAS#
##############
##############

get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

get_symbols()
get_symbols()
get_symbols()

# IF 
num <- -2
if(num<0){
  num<-num*-1
}
num

num<-4
if(num<0){
  num<-num*-1
}
num

## ej 2
num <- -1 

if(num<0){
  print("num is negative.")
  print("Don't worry, I'll fix it.") 
  num <- num * -1 
  print("Now num is positive.")
  }
## "num is negative."
## "Don't worry, I'll fix it."
## "Now num is positive."
num 

# Quiz A
x<-1 

if(3==3){ 
  x<-2
} 
x 

# Quiz B
x<-1

if (TRUE) {
  x<-2
}
x

# Quiz C
x<-1 

if(x==1){
  x<-2
  if(x==1){
    x<-3
  }
  } 
x 

# ELSE
a <- 3.14
trunc(a)

dec <- a - trunc(a) 
dec

if (dec >= 0.5) {
  a <- trunc(a) + 1 
}else{
  a <- trunc(a) 
}

a 
# ejemplo 2
a<-1 
b<-1

if (a>b) {
  print("a wins!")
} else if (a<b) {
  print("b wins!")
} else { print("Tie!")
}

#### Verificando s??mbolos ####
symbols <- c("7", "7", "7")
symbols[1] == symbols[2] & symbols[2] == symbols[3] 
symbols[1] == symbols[2] & symbols[1] == symbols[3]
all(symbols == symbols[1])
length(unique(symbols) == 1)

# Segundo caso de simbolos - barras#
symbols <- c("B", "BBB", "BB")
symbols[1] == "B" | symbols[1] == "BB" | symbols[1] == "BBB" &
  symbols[2] == "B" | symbols[2] == "BB" | symbols[2] == "BBB" &
  symbols[3] == "B" | symbols[3] == "BB" | symbols[3] == "BBB"

all(symbols %in% c("B", "BB", "BBB"))

# Contempla tambien el caso 1
symbols <- c("B", "B", "B")
all(symbols %in% c("B", "BB", "BBB")) 


# Premios - Lookup Tables
payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, "B"=10,"C"=10,"0"=0)
payouts

payouts["DD"]
payouts["B"]

unname(payouts["DD"])

### que pasa si 

symbols <- c("7", "7", "7")
symbols[1]

payouts[symbols[1]] 

symbols <- c("C", "C", "C") 
payouts[symbols[1]]

# Caso 3
symbols <- c("C", "DD", "C")
symbols == "C"
sum(symbols == "C")
sum(symbols == "DD")

# Contar cherries
c(0, 2, 5)[1] 

################
## PLAY !!!!! ##
################

# recopilando
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  # get prize
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B"=10,"C"=10,"0"=0)
    prize <- unname(payouts[symbols[1]]) 
  } else if (all(bars)) {
    prize <- 5 
  }  else {
    cherries <- sum(symbols == "C") 
    prize <- c(0, 2, 5)[cherries + 1] 
  } 
  
  # adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds 
}

play <- function() {
  symbols <- get_symbols()
  print(symbols)
  score(symbols) 
} 

play()
