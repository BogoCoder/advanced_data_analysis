library(ggplot2)

datas<-read.csv("Saber_11__2019-2.csv", stringsAsFactors=FALSE)

hist(datas$PUNT_INGLES)
hist(datas$PUNT_MATEMATICAS)
hist(datas$PUNT_C_NATURALES)

p_den_ingles <- ggplot(datas, aes(x = datas$PUNT_INGLES)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.2, fill="#FF6666") + scale_x_continuous(name = "Puntaje en inglés") + scale_y_continuous(name = "Densidad")
p_den_ingles

p_den_mat <- ggplot(datas, aes(x = datas$PUNT_MATEMATICAS)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.2, fill="#23809C") + scale_x_continuous(name = "Puntaje en matemáticas") + scale_y_continuous(name = "Densidad")
p_den_mat

p_den_c_naturales <- ggplot(datas, aes(x = datas$PUNT_C_NATURALES)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.2, fill="#202143") + scale_x_continuous(name = "Puntaje en ciencias naturales") + scale_y_continuous(name = "Densidad")
p_den_c_naturales

## Segregación de variables

### Puntaje de inglés segregado por género
p_den_ingles_genre <- ggplot(datas, aes(x = datas$PUNT_INGLES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#485A74") + scale_x_continuous(name = "Puntaje en inglés") + scale_y_continuous(name = "Densidad") +facet_grid(datas$ESTU_GENERO ~ .)
p_den_ingles_genre

### Puntaje de matemáticas segregado por género
p_den_mat_genre <- ggplot(datas, aes(x = datas$PUNT_MATEMATICAS)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#28B8C8") + scale_x_continuous(name = "Puntaje en matemáticas") + scale_y_continuous(name = "Densidad") +facet_grid(datas$ESTU_GENERO ~ .)
p_den_mat_genre

### Puntaje de ciencias naturales segregado por género
p_den_c_naturales_genre <- ggplot(datas, aes(x = datas$PUNT_C_NATURALES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#772792") + scale_x_continuous(name = "Puntaje en ciencias naturales") + scale_y_continuous(name = "Densidad") +facet_grid(datas$ESTU_GENERO ~ .)
p_den_c_naturales_genre

### Puntaje de inglés segregado por colegio oficial/no oficial
p_den_ingles_cole_naturaleza <- ggplot(datas, aes(x = datas$PUNT_INGLES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#485A74") + scale_x_continuous(name = "Puntaje en inglés") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_NATURALEZA ~ .)
p_den_ingles_cole_naturaleza

### Puntaje de matemáticas segregado por colegio oficial/no oficial
p_den_mat_cole_naturaleza <- ggplot(datas, aes(x = datas$PUNT_MATEMATICAS)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#28B8C8") + scale_x_continuous(name = "Puntaje en matemáticas") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_NATURALEZA ~ .)
p_den_mat_cole_naturaleza

### Puntaje de ciencias naturales segregado por colegio oficial/no oficial
p_den_c_naturales_cole_naturaleza <- ggplot(datas, aes(x = datas$PUNT_C_NATURALES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#772792") + scale_x_continuous(name = "Puntaje en ciencias naturales") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_NATURALEZA ~ .)
p_den_c_naturales_cole_naturaleza

### Puntaje de inglés segregado por jornada
p_den_ingles_cole_jornada <- ggplot(datas, aes(x = datas$PUNT_INGLES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#485A74") + scale_x_continuous(name = "Puntaje en inglés") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_JORNADA ~ .)
p_den_ingles_cole_jornada

### Puntaje de matemáticas segregado por jornada
p_den_mat_cole_jornada <- ggplot(datas, aes(x = datas$PUNT_MATEMATICAS)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#28B8C8") + scale_x_continuous(name = "Puntaje en matemáticas") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_JORNADA ~ .)
p_den_mat_cole_jornada

### Puntaje de ciencias naturales segregado por jornada
p_den_c_naturales_cole_jornada <- ggplot(datas, aes(x = datas$PUNT_C_NATURALES)) + geom_histogram(aes(y=..density..),position="identity", colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.5, fill = "#772792") + scale_x_continuous(name = "Puntaje en ciencias naturales") + scale_y_continuous(name = "Densidad") +facet_grid(datas$COLE_JORNADA ~ .)
p_den_c_naturales_cole_jornada

## Inglés vs matemáticas

matrix_datas = cbind(as.numeric(datas$PUNT_MATEMATICAS),na.omit(as.numeric(datas$PUNT_INGLES)))
covar = cov(matrix_datas)
covar

corre = cor(matrix_datas)
corre

## Ciencias naturales vs matemáticas

matrix_datas = cbind(as.numeric(datas$PUNT_MATEMATICAS),na.omit(as.numeric(datas$PUNT_C_NATURALES)))
covar = cov(matrix_datas)
covar

corre = cor(matrix_datas)
corre


## Puntos

hola = ggplot(datas, aes(x = datas$PUNT_INGLES[c(0, 100)], y = datas$PUNT_MATEMATICAS[c(0, 100)])) + 
  geom_point()+
  geom_smooth(method=lm) + scale_x_continuous(name = "Puntaje en inglés") + scale_y_continuous(name = "Puntaje en matemáticas")
hola
