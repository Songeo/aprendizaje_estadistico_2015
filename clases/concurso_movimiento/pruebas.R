library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(randomForest)

entrena_s

rf_1 <- randomForest(factor(cocina_pir) ~ cocina, 
                   data=entrena_s, 
                   ntree=500, importance = T)
rf_2 <- randomForest(factor(sala_pir) ~ sala, 
                     data=entrena_s,
                     ntree=500, importance = T)
rf_3 <- randomForest(factor(estudiof_pir) ~ estudiof, 
                     data=entrena_s,
                     ntree=500, importance = T)

pred_cocina <- predict(rf_1, eval, type ='prob')[,2]
pred_sala <- predict(rf_2, eval, type ='prob')[,2]
pred_estudiof <- predict(rf_2, eval, type ='prob')[,2]

sub <- example_sub
sub$cocina_pir <- pred_cocina
sub$sala_pir <- pred_sala
sub$estudiof_pir <- pred_estudiof
write_csv(sub, 'data/bench_1.csv')
plot(rf)
importance(rf, scale= F)

table(pp <- predict(rf, publicos, type='prob')[,1] > 0.5)
publicos_2 <- publicos
publicos$pred <- predict(rf, publicos, type='prob')[,1]
#ggplot(publicos, aes(x=estudiof, y=pred, colour=puerta_m)) + geom_point() +
#  facet_wrap(~puerta_m) + geom_smooth()

table(pp, publicos$puerta_m)
prop.table(table(pp, publicos$puerta_m),2)
table(pp, publicos$puerta_m, cut(publicos$estudiof, seq(0,1,0.25)))


privados <- filter(datos_b, muestra=='privado')
table(pp <- predict(rf, privados))
table(pp, privados$puerta_m)
prop.table(table(pp, privados$puerta_m),2)



partialPlot(rf, datos_t %>% data.frame, consumo)
