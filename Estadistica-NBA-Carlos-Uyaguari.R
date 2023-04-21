install.packages("lessR")
install.packages("dplyr")
install.packages("FSelector")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("rjava")
install.packages("writexl")
install.packages("cowplot")
install.packages("scales")
                                  #Librerias

library(readr)
library(rlang)
library(ggplot2)
library(dplyr)
library(tidyverse) 
library(agricolae) 
library(modeest) 
library(rio) 
library(caret) 
library(DescTools)
library(missForest)
library(lessR)
library(plotrix)
library(data.table)
library(microbenchmark)
library(PerformanceAnalytics)
library(FSelector)
library(descr)
library(pROC)
library(ggrepel)
library(rJava)
library(writexl)
library(cowplot)
library(cowplot)

                    ### Para Conocer la Ruta del Archivo ###
                        
                      ### Data del 2003-2023 ###
file.choose()# Determina la Ruta del Archivo
play_2003_2023 <- read_delim("V:\\DOCUMENTOS\\Carlos\\Maestria\\Caso-Practico-Final\\Caso-Practico-Final\\Tesis-Final\\Base-2003-2022.csv")
View(play_2003_2023)
                              ### Limpieza de Datos Data 2003-2023 ###                          
                                      ### Renombrar###

colnames(play_2003_2023)<-c("Año","Jugadores", "Posicion","Edad" ,
                            "Equipo","Juego", "Juego_Inic", 
                            "Min_Jugados", "Canasta","Intent_de_Canasta", 
                            "%_de_Canasta", "Canasta_de_3_puntos",
                            "Intent_de_Canasta_de_3_punt", 
                            "%_de_intent_de_Canasta_de_3_punt",
                            "Canasta_de_2_punt",
                            "Intent_de_Canasta_de_2_punt", 
                            "%_de_intent_de_Canasta_de_2_punt", 
                            "Canasta_de_3_punt_vale_un_punt_más_q_un_gol_de_2_punt ",
                            "Tiros_libres", "Intent_de_tiros_libres", 
                            "%_Intent_de_tiros_libres","Rebotes_ofensivos",
                            "Rebotes_defensivos","Total_de_rebotes", 
                            "Asistencias", "Robos", "Bloqueos", 
                            "Perdidas_de_balón", "Faltas_person", "Puntos"
                            )
View(play_2003_2023)
# Verifica los datos validos y se lo excluye del analisis (0) de cada fila
play_2003_2023<- subset(play_2003_2023,play_2003_2023$Canasta !="0")

# Selecciono la variable a trabajar #
selct_var_2003_2023 <- play_2003_2023[,c("Año","Jugadores", "Posicion","Edad" ,
                                         "Equipo","Juego","Min_Jugados", 
                                         "Canasta","%_de_Canasta", 
                                         "Canasta_de_3_puntos",
                                         "%_de_intent_de_Canasta_de_3_punt",
                                         "Canasta_de_2_punt",
                                         "%_de_intent_de_Canasta_de_2_punt",
                                         "Tiros_libres", 
                                         "%_Intent_de_tiros_libres","Rebotes_ofensivos",
                                         "Rebotes_defensivos", "Asistencias",
                                         "Robos", "Bloqueos", 
                                         "Faltas_person", "Puntos"
                                          )]
View(selct_var_2003_2023)

        ### Verifica los datos validos y se lo excluye del analisis (0) de cada fila ###

play_2003_2023<- subset(play_2003_2023,play_2003_2023$Canasta !="0")
View(play_2003_2023)


                        ### Graficos Punto Vs Posiciones 2003-2023 ###

ggplot(selct_var_2003_2023, aes(x = Puntos,color = Posicion, fill=Posicion)) +
  geom_histogram(binwidth = 150) + facet_wrap(~Año, nrow = 5)+
  labs( 
    title = "Puntos por año Vs Posiciones de Jugadores de la NBA", 
    subtitle = "NBA Stats 2003-2023", 
    x = "Puntos de Jugadores de la NBA", 
    y = "Total de Puntos 2003-2023" 
  )

### Extraer data del año 2016-2023, los Agrupa por Año y muestra las primeros 10 Filas###

player_punt<-selct_var_2003_2023 %>% group_by(Año) %>% slice_max(order_by=Puntos, n=10) %>% filter(Año >2015)
player_fin <- player_punt[,c("Año","Jugadores", "Edad","Posicion", "Puntos", 
                             "Tiros_libres", "Canasta_de_3_puntos","Canasta_de_2_punt",
                             "Bloqueos")]
View(player_fin)

                    ### Graficos Punto Vs Posiciones 2016-2023 ###

ggplot(player_fin, aes(x = Puntos, color = Posicion, fill=Posicion)) +
  geom_histogram(binwidth = 150) + facet_wrap(~Año, nrow = 5)+
  labs( 
    title = "Puntos por año Vs Posiciones de Jugadores de la NBA", 
    subtitle = "NBA Stats 2016-2023", 
    x = "Puntos de Jugadores de la NBA", 
    y = "Total de Puntos 2016-2023" 
  )

                      ### Tiros de 2 Puntos de cada jugador ###

summary(player_fin$Canasta_de_2_punt)
boxplot(player_fin$Canasta_de_2_punt)

ggplot(player_fin, aes(x=Jugadores, y= Canasta_de_2_punt, label=Canasta_de_2_punt
                       ,group= Año))+
  geom_point(size=5, aes(colour=Año))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(size=3)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1))+
  labs( 
    title = " Top 10 mejores Jugadores NBA Vs Canasta de 2 Puntos",
    subtitle = "NBA 2016-2023", 
    x = "Jugadores de la NBA", 
    y = "Total Canasta de 2 Puntos")


                        ### Tiros de 3 Puntos de cada jugador ###

summary(player_fin$Canasta_de_3_puntos)
boxplot(player_fin$Canasta_de_3_puntos)

ggplot(player_fin, aes(x=Jugadores, y= Canasta_de_3_puntos, label=Canasta_de_3_puntos,
                       group= Año))+
  geom_point(size=5, aes(colour=Año))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(size=4)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1))+
  labs( 
    title = " Top 10 mejores Jugadores NBA Vs Canasta de 3 Puntos",
    subtitle = "NBA 2016-2023", 
    x = "Jugadores de la NBA", 
    y = "Total Canasta de 3 Puntos")

              ### Tiros libres por juegos 2016-2023 ###

summary(player_fin$Tiros_libres)
boxplot(player_fin$Tiros_libres)

ggplot(player_fin, aes(x=Jugadores, y=Tiros_libres, label=Tiros_libres, Group= Año))+
  geom_point(size=5, size=5, aes(colour=Año))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(size=4)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1))+
  labs( 
    title = "Top 10 (Puntos de Tiros Libres por Jugadores de la NBA)", 
    subtitle = "NBA Stats 2016-2023", 
    x = "Top 10 de Jugadores de la NBA", 
    y = "Total Puntos de Tiros Libres")

                  ### Estadisticas de experiencias por Año ###

summary(player_fin$Edad)
summary(player_fin$Puntos)

ggplot(player_fin, aes(x=Edad, y= Puntos, label=Puntos, group=Año))+
  geom_point(size=3, aes(color=Año))+
  scale_color_continuous(low= "black", high= "red")+
  geom_text_repel(color="purple")+
  labs( 
    title = "Top 10 de Jugadores que llegan a su maxima experiencia a traves de los Años", 
    subtitle = "NBA Stats 2016-2023", 
    x = "Edad de Jugadores de la NBA", 
    y = "Total Puntos por Jugador")
  

                    ### Histograma de Frecuencia de Edad ###

summary(player_fin$Edad)
boxplot(player_fin$Edad)
his_fr_Ed<-table(player_fin$Edad)
his_fr_Ed

mean(his_fr_Ed)
median(his_fr_Ed)
quantile(his_fr_Ed)

mean(his_fr_Ed)
median(his_fr_Ed)
quantile(his_fr_Ed)
sd(his_fr_Ed)# Desviacion Estandar
density(his_fr_Ed)

hist(his_fr_Ed, prob=TRUE ,main = "HISTOGRAMA DE FRECUENCIA DE EDAD",
     ylab = "FRECUENCIA", col = "Sky blue", border = "Red", lwd=3)

x <- seq(min(his_fr_Ed), max(his_fr_Ed), length = 90)
f <- dnorm(x, mean = mean(his_fr_Ed), sd = sd(his_fr_Ed))
lines(x, f, col = "red", lwd = 2)
lines(density(his_fr_Ed), lwd = 2, col = 'Blue')
legend("topright", col= c("red", "Blue"), 
       legend = c("HISTOGRAMA CON CURVA NORMAL", "HISTOGRAMA CON CURVA DE DENSIDAD"), lwd = 2)

                    ### Histograma de Frecuencia de Puntos ###
summary(player_fin$Puntos)
boxplot(player_fin$Puntos)
his_fr_ptos<-table(player_fin$Puntos)

his_fr_ptos

mean(his_fr_ptos)
median(his_fr_ptos)
quantile(his_fr_ptos)

sd(his_fr_ptos)# Desviacion Estandar
density(his_fr_ptos)


hist(his_fr_ptos, prob=TRUE ,main = "HISTOGRAMA DE FRECUENCIA DE PUNTOS",
     ylab = "FRECUENCIA", col = "Sky blue", border = "Red", lwd=3)
x <- seq(min(his_fr_ptos), max(his_fr_ptos), length = 90)
f <- dnorm(x, mean = mean(his_fr_ptos), sd = sd(his_fr_ptos))
lines(x, f, col = "red", lwd = 2)
lines(density(his_fr_ptos), lwd = 2, col = 'Blue')
legend("topright", col= c("red", "Blue"), 
       legend = c("HISTOGRAMA CON CURVA NORMAL", "HISTOGRAMA CON CURVA DE DENSIDAD"), lwd = 3 )

                          ### Regresion lineal ###

rl_ed_Pto<-lm(Edad~Jugadores+Puntos,data=player_fin)
summary(rl_ed_Pto)


                        ###Retirar ordenada al Origen##

rl_ed_Pto_2<-lm(Edad~-1+Jugadores+Puntos,data=player_fin)
summary(rl_ed_Pto_2)

                ### Analisi Anova entre Edad y Puntos ###
ANOVA<-aov(rl_ed_Pto_2)
summary(ANOVA)
plot(ANOVA)

              ### Grafico Top 10 Mejores Jugadores por año 2016-2023 ###

ggplot(player_fin, aes(x= Año, y= Puntos, group= Jugadores, fill=Jugadores, label=Jugadores,
                       color=Jugadores))+
  geom_point(size=3)+
  geom_text_repel()+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1))+
  labs( 
    title = "Puntos vs Jugadores NBA", 
    subtitle = "NBA Stats 2016-2023", 
    x = "Año ", 
    y = "Puntos Jugadores de la NBA")

                    ### Data Salarios 2016-2023 ###

file.choose()# Determina la Ruta del Archivo
dat_sal_2016_2022 <- read_delim("V:\\DOCUMENTOS\\Carlos\\Maestria\\Caso-Practico-Final\\Caso-Practico-Final\\Tesis-Final\\Salaries-NBA-2016-2023.csv")
colnames(dat_sal_2016_2022)<-c("Año","Jugadores", "Posicion",
                            "Equipo", "Salario")
View(dat_sal_2016_2022) 

            ### Graficos de Jugadores Mejores Pagados vs Puntos ###

          ## Unimos las dataFrame de Jugadores con Datframe de salarios ##

un_play_Sal<- merge(dat_sal_2016_2022, player_fin, all.y =TRUE)
un_play_Sal[is.na(un_play_Sal)]<-0
View (un_play_Sal)
#write.csv(un_play_Sal, "V:\\DOCUMENTOS\\Carlos\\Maestria\\Caso-Practico-Final\\Caso-Practico-Final\\Salario.csv")

                ### Regresion lineal de la Union de dos Tablas ###

rl_jug_Pto<-lm(Puntos~Jugadores+Salario,data=un_play_Sal)
summary(rl_jug_Pto)


                  ###Retirar ordenada al Origen##

rl_jug_Pto_2<-lm(Puntos~-1+Jugadores+Salario,data=un_play_Sal)
summary(rl_jug_Pto_2)

                  ### Analisi Anova entre Puntos-Jugadores y Salario ###
ANOVA<-aov(rl_jug_Pto_2)
summary(ANOVA)
plot(ANOVA)


                  ### Analisis pot Año Salarios de Jugadores ###

div_dat_sal<- split(un_play_Sal, un_play_Sal$Año)
View(div_dat_sal)

                  ### Salario VS Top 10 mejores Jugadores 2016 ###

player_2016<- div_dat_sal[[1]]
player_2016 %>% filter(Salario >0)
View(player_2016)

  ggplot(player_2016, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = " Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2016", 
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")
                          
            ### Salario VS Top 10 mejores Jugadores 2017 ###

player_2017<- div_dat_sal[[2]]
View(player_2017)

ggplot(player_2017, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2017", 
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")



            ### Salario VS Top 10 mejores Jugadores 2018 ###

player_2018<- div_dat_sal[[3]]
View(player_2018)

ggplot(player_2018, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2018",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")

            ### Salario VS Top 10 mejores Jugadores 2019 ###

player_2019<- div_dat_sal[[4]]
View(player_2019)

ggplot(player_2019, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2019",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")

              ### Salario VS Top 10 mejores Jugadores 2020 ###

player_2020<- div_dat_sal[[5]]
View(player_2020)

ggplot(player_2020, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2020",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")
                
          ### Salario VS Top 10 mejores Jugadores 2021 ###

player_2021<- div_dat_sal[[6]]
View(player_2021)

ggplot(player_2021, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 5 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2021",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")

              ### Salario VS Top 10 mejores Jugadores 2022 ###

player_2022<- div_dat_sal[[7]]
View(player_2022)

ggplot(player_2022, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2022",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")

              ### Salario VS Top 10 mejores Jugadores 2023 ###

player_2023<- div_dat_sal[[8]]
View(player_2023)

ggplot(player_2023, aes(x=Salario, y= Puntos, label=Puntos))+
  geom_point(size=3, aes(color=Posicion))+
  geom_text_repel()+
  facet_grid(.~Jugadores)+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="Blue"))+
  labs( 
    title = "Top 10 mejores Jugadores NBA Vs Salarios",
    subtitle = "NBA 2023",
    x = "Salario por cada Jugador", 
    y = "Puntos de los Jugadores de la NBA")

                        ### Data Draft 2022 ###
file.choose()# Determina la Ruta del Archivo
draft_22 <- read_delim("V:\\DOCUMENTOS\\Carlos\\Maestria\\Caso-Practico-Final\\Caso-Practico-Final\\Tesis-Final\\draft2022.csv")
View(draft_22)
colnames(draft_22)<-c("Año","Jugadores", "Edad", "Posicion", 
                            "Equipo","Colegio","Año_Juegos", "Juego_Inic", 
                            "Canasta","Puntos", "Rebotes","Asistencia", 
                            "Canasta_de_3_punt","Canasta_de_2_punt"
                            )
View(draft_22) 
player_draf <- draft_22[,c("Año","Jugadores", "Edad","Posicion", "Puntos", "Canasta", 
                                "Canasta_de_3_punt","Canasta_de_2_punt", "Asistencia",
                                "Rebotes")]
View(player_draf)

        ### Comparaciones de Jugadores, Posiciones y canasta de 3 Puntos ###


ggplot(player_draf, aes(x=Jugadores, y=Puntos, label=Puntos))+
  geom_point(size=5,aes(color=Canasta_de_3_punt, shape=Posicion))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(color="blue")+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="red"))+
  labs( 
    title = "Top 10 mejores Jugadores de la Temporada por Posicion y Canasta de 3 Puntos",
    subtitle = "Periodo 2022",
    x = "Mejores Jugadores de la Temporada", 
    y = "Total Puntos de la Temporada")

          ### Comparaciones de Jugadores, Posiciones y Canasta de 2 punt ###

ggplot(player_draf, aes(x=Jugadores, y=Puntos, label=Puntos))+
  geom_point(size=5,aes(color=Canasta_de_2_punt, shape=Posicion))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(color="blue")+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="red"))+
  labs( 
    title = "Top 10 mejores Jugadores de la Temporada por Posiciones y Canasta de 2 Puntos",
    subtitle = "Periodo 2022",
    x = "Mejores Jugadores de la Temporada", 
    y = "Total Puntos de la Temporada")
            
          ### Comparaciones de Jugadores, Posiciones y  Asistencias ###

ggplot(player_draf, aes(x=Jugadores, y=Puntos, label=Puntos))+
  geom_point(size=5,aes(color=Asistencia, shape=Posicion))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(color="blue")+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="red"))+
  labs( 
    title = "Top 10 mejores Jugadores de la  Temporada por Asistencia",
    subtitle = "Periodo 2022",
    x = "Mejores Jugadores de la Temporada", 
    y = "Total Puntos de la Temporada")

          ### Comparaciones de Jugadores, Edad ,Posiciones y Rebotes ###

ggplot(player_draf, aes(x=Jugadores, y=Rebotes, label=Rebotes))+
  geom_point(size=5,aes(color=Edad, shape=Posicion))+
  scale_color_continuous(low= "blue", high= "red")+
  geom_text_repel(color="blue")+
  theme(axis.text.x = element_text(angle=47, vjust = 1, hjust = 1, color="red"))+
  labs( 
    title = "Top 10 mejores Jugadores de la Temporada por Rebotes",
    subtitle = "Periodo 2022",
    x = "Mejores Jugadores de la Temporada", 
    y = "Total de Rebotes 2022")

