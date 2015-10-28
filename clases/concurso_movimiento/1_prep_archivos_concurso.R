library(purrr)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(randomForest)

datos <- readRDS('datos/datos_completos_cons.rds')
datos_f <- filter(datos, !is.na(consumo))
nrow(datos_f)

datos_f$hora <- hour(datos_f$datetime)
datos_f$dia_sem <- factor(weekdays(datos_f$datetime))
datos_b <- filter(datos_f, estudiof < 0.5)
table(datos_b$estudiof_pir)
datos_b$puerta_m <- 'Activado'
datos_b$puerta_m[!datos_b$estudiof_pir] <- 'Desactivado'
datos_b$puerta_m <- factor(datos_b$puerta_m)
table(datos_b$puerta_m)
datos_b$ind <- 1:nrow(datos_b)
quantile(datos_b$ind)
datos_b$muestra <- 'entrena'
datos_b$muestra[datos_b$ind > 70000] <- 'publico'
datos_b$muestra[datos_b$ind > 90000] <- 'privado'
entrena  <- filter(datos_b, muestra == 'entrena')
publicos <- filter(datos_b, muestra == 'publico')
privados <- filter(datos_b, muestra == 'privado')
nrow(entrena)
nrow(publicos)
nrow(privados)
table(entrena$puerta_m)
table(publicos$puerta_m)
table(privados$puerta_m)

set.seed(391)
datos_1 <- entrena %>% filter(estudiof_pir==1)
datos_2 <- entrena %>% filter(estudiof_pir==0) %>% sample_n(nrow(datos_1))
datos_t <- bind_rows(datos_1, datos_2)
#datos_t <- datos_b
nrow(datos_t)
prop.table(table(datos_t$estudiof_pir))
table(datos_t$puerta_m)


entrena_subir <- entrena %>% dplyr::select(datetime:vestidor, consumo,  cocina_pir, sala_pir, estudiof_pir)
publico_f <- entrena %>% dplyr::select(datetime:vestidor, consumo, cocina_pir, sala_pir, estudiof_pir)
privado_f <- entrena %>% dplyr::select(datetime:vestidor, consumo, cocina_pir, sala_pir, estudiof_pir)
eval <- bind_rows(publico_f, privado_f)

head(eval)
solucion <- eval %>% dplyr::select(cocina_pir, sala_pir, estudiof_pir) %>%
  mutate(cocina_pir = as.numeric(cocina_pir), 
         sala_pir=as.numeric(sala_pir),
         estudiof_pir = as.numeric(estudiof_pir))
solucion$id <- 1:nrow(solucion)
solucion <- solucion %>% dplyr::select(id, cocina_pir, sala_pir, estudiof_pir) 
write_csv(solucion, './datos/solucion.csv')

example_sub <- solucion
example_sub$cocina_pir <- 0
example_sub$sala_pir <- 0
example_sub$estudiof_pir <- 0
write_csv(example_sub, './datos/sample_sub.csv')

entrena_s <- entrena_subir %>% 
  mutate(cocina_pir = as.numeric(cocina_pir), 
         sala_pir=as.numeric(sala_pir),
         estudiof_pir = as.numeric(estudiof_pir))
prueba_s <- eval %>%
  mutate(cocina_pir = as.numeric(cocina_pir), 
         sala_pir=as.numeric(sala_pir),
         estudiof_pir = as.numeric(estudiof_pir)) %>% 
  dplyr::select(-cocina_pir, -sala_pir, -estudiof_pir)

write_csv(prueba_s, './datos/test.csv')


write_csv(entrena_s, './datos/train.csv')

write_csv(prueba_s, './datos/test.csv')
