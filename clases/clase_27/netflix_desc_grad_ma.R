options(digits=4)
library(ggplot2)
library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)


load('../clase_26/datos/dat_muestra_nflix.Rdata')
dim(dat.muestra)
head(dat.muestra)

pelis.nombres <- read.csv('../clase_26/datos/movies_title_fix.csv', stringsAsFactors = FALSE, header=FALSE)
names(pelis.nombres) <- c('peli_id','release','nombre')
head(pelis.nombres)

set.seed(28882)
valida_usuarios <- sample(unique(dat.muestra$usuario_id), 20000 )
valida_pelis <- sample(unique(dat.muestra$peli_id), 2000 )
dat.2 <- dat.muestra %>%
  mutate(valida_usu = usuario_id %in% valida_usuarios) %>%
  mutate(valida_peli = peli_id %in% valida_pelis)

dat.entrena <- filter(dat.2, !valida_usu | !valida_peli)
dat.valida <- filter(dat.2, valida_usu & valida_peli)
nrow(dat.entrena) + nrow(dat.valida)
nrow(dat.2)

peli.media <- dat.entrena %>% group_by(peli_id) %>% summarise(media.peli = mean(calif))
usuario.media <- dat.entrena %>% group_by(usuario_id) %>% summarise(media.usu = mean(calif))
media.gral <- mean(dat.entrena$calif)


dat.entrena.2 <- dat.entrena %>% 
  left_join(peli.media) %>%
  left_join(usuario.media) %>%
  mutate(media.tot=media.peli+media.usu-media.gral)

dat.entrena.2 <- mutate(dat.entrena.2, calif.aj = calif-media.tot)

## reescribimos ids:
dat.entrena.2$id_2 <- as.numeric(factor(dat.entrena.2$usuario_id))

dat.valida.2 <- dat.valida %>%
  left_join(unique(dat.entrena.2[,c('usuario_id', 'id_2')])) %>%
  left_join(peli.media) %>%
  left_join(usuario.media) %>%
  mutate(media.tot=media.peli+media.usu-media.gral) %>%
  mutate(calif.aj = calif - media.tot)

## construimos matriz rala
library(Matrix)
i <- dat.entrena.2$id_2
j <- dat.entrena.2$peli_id
y <- dat.entrena.2$calif.aj
X <- sparseMatrix(i, j, x = y, dims=c(100000, 17770))
dim(X)

dat.valida.3 <- filter(dat.valida.2, !is.na(id_2) & !is.na(peli_id))

###
i.v <- dat.valida.3$id_2
j.v <- dat.valida.3$peli_id
y.v <- dat.valida.3$calif.aj
X.v <- sparseMatrix(i.v, j.v, x = y.v, dims=c(100000, 17770))
dim(X.v)


### ahora compliamos las funciones
library(Rcpp)
Rcpp::sourceCpp('netflix_gradiente.cpp')
Rcpp::sourceCpp('calc_error_bias.cpp')


#inicializar parámetros
set.seed(2805)
P <- matrix(rnorm(5*dim(X)[1],0,0.01), ncol=5, nrow=dim(X)[1])
Q <- matrix(rnorm(5*dim(X)[2],0,0.01), ncol=5, nrow=dim(X)[2])
a <- rep(0, dim(X)[1])
b <- rep(0, dim(X)[2])

## raíz de ecm:
sqrt(calc_error(i,j,y, P, Q, a,b))
sqrt(calc_error(i.v,j.v,y.v, P, Q,a,b))



for(k in 1:10){
  print(k)
  out <- netflix_gradiente(i, j, y, P, Q, a, b, 0.004, 0.01)
  P <- out[[1]]
  Q <- out[[2]]
  a <- out[[3]]
  b <- out[[4]]
  print(sqrt(calc_error(i, j, y, P, Q, a, b)))
  print(sqrt(calc_error(i.v, j.v, y.v, P, Q, a, b)))
}


pelis.nombres <- read.csv('../clase_26/datos/movies_title_fix.csv', header=FALSE, stringsAsFactors=FALSE)
head(pelis.nombres)
names(pelis.nombres) <- c('peli_id','a','nombre')
num_pelis <- dat.entrena.2 %>% group_by(peli_id) %>% summarise(num = length(calif))
xx <- data.frame(pelis.nombres %>% arrange(peli_id), Q)

# por ejemplo, factor 1:
arrange(xx, desc(X1)) %>% head(20)
arrange(xx, desc(X1)) %>% tail(20)
# factor 2
arrange(xx, desc(X3)) %>% head(20)
arrange(xx, desc(X3)) %>% tail(20)

### Ahora chequemos algunos usuarios

usuario <- 5100
califs.1 <- dat.entrena.2 %>% filter(id_2==usuario) %>%
  arrange(desc(calif))
califs.2 <- left_join(califs.1, pelis.nombres)
head(califs.2,50)

pred.1 <- data.frame(peli.media, pred.x =  Q%*%P[usuario, ] ) %>%
  left_join(pelis.nombres) %>%
  arrange(desc(pred.x))
head(pred.1,20)


usuario <- 2600
## Aquí vemos películas que le gustaron por encima del promedio. Nótese
# que falta sumar el nivel de películas - en este caso
# deberíamos restringir a películas con muchas calificaciones,
# o regularizar también la media de películas

califs.1 <- dat.entrena.2 %>% filter(id_2==usuario) %>%
  arrange(desc(calif))
califs.2 <- left_join(califs.1, pelis.nombres)
head(califs.2,100)

pred.1 <- data.frame(peli.media, pred.x =   Q%*%P[usuario, ] ) %>%
  mutate(pred = pred.x) %>%
  left_join(pelis.nombres) %>%
  arrange(desc(pred))
head(pred.1,20)


