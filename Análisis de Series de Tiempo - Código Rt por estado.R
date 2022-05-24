#Download packages
library("tidyverse")
library("beepr")
library("tictoc")
library("tibbletime")
library("ggplot2")
library("magrittr")

datosSinave<- read.csv("D:/Usuarios/Judith/Documentos/Pablo Agustín/ITESO/Verano 2021/PAP/Code/Rt corregido/210703COVID19MEXICO.csv")

datosConferencia_rho<- read.csv("D:/Usuarios/Judith/Documentos/Pablo Agustín/ITESO/Verano 2021/PAP/Datos/datosConferencia.csv")

rolling_mean <- rollify(mean, window=7)
rolling_mean14 <- rollify(mean, window=14)

##DATOS CD DE MÉXICO - 9

#Acumulados Sinave

datosSinave_9 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 9) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_9 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 9) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))
  
datosSinave_9 <- merge(datosSinave_9,datosConferencia_rho_9)
head(datosSinave_9)

#Graficar
ggplot(data= datosSinave_9, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_9, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del rho
datosConferencia_rho_9 <- datosSinave_9 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_9 <- 9.209944*1000000/100000
rho_9 <- as.data.frame(datosConferencia_rho_9) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_9) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_9)

ggplot(data=rho_9, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_9, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_9, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo en Cd. de México', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Ciudad de México, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Ciudad de México')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS EDO. MÉX. - 15

#Acumulados Sinave

datosSinave_15 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 15) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_15 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 15) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_15 <- merge(datosSinave_15,datosConferencia_rho_15)
head(datosSinave_15)

#Graficar
ggplot(data= datosSinave_15, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_15, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_15 <- datosSinave_15 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_15 <- 16.992418*1000000/100000
rho_15 <- as.data.frame(datosConferencia_rho_15) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_15) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_15)

ggplot(data=rho_15, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_15, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_15, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo en Estado de México', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Estado de México, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_15, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Edo. Méx.')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS GUANAJUATO - 11

#Acumulados Sinave

datosSinave_11 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 11) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_11 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 11) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_11 <- merge(datosSinave_11,datosConferencia_rho_11)
head(datosSinave_11)

#Graficar
ggplot(data= datosSinave_11, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_11, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_11 <- datosSinave_11 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_11 <- 6.166934*1000000/100000
rho_11 <- as.data.frame(datosConferencia_rho_11) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_11) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_11)

ggplot(data=rho_11, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_11, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_11, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo Guanajuato', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Guanajuato, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Guanajuato')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS NUEVO LEÓN - 19 

#Acumulados Sinave

datosSinave_19 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 19) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_19 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 19) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_19 <- merge(datosSinave_19,datosConferencia_rho_19)
head(datosSinave_19)

#Graficar
ggplot(data= datosSinave_19, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_19, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_19 <- datosSinave_19 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_19 <- 5.784442*1000000/100000
rho_19 <- as.data.frame(datosConferencia_rho_19) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_19) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_19)

ggplot(data=rho_19, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_19, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_19, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo Nuevo León', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Nuevo León, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Nuevo León')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS JALISCO - 14

#Acumulados Sinave

datosSinave_14 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 14) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_14 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 14) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_14 <- merge(datosSinave_14,datosConferencia_rho_14)
head(datosSinave_14)

#Graficar
ggplot(data= datosSinave_14, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_14, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_14 <- datosSinave_14 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_14 <- 8.348151*1000000/100000
rho_14 <- as.data.frame(datosConferencia_rho_14) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_14) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_14)

ggplot(data=rho_14, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_14, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_14, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo Jalisco', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Jalisco, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Jalisco')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS PUEBLA - 21

#Acumulados Sinave

datosSinave_21 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 21) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_21 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 21) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_21 <- merge(datosSinave_21,datosConferencia_rho_21)
head(datosSinave_21)

#Graficar
ggplot(data= datosSinave_21, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_21, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_21 <- datosSinave_21 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_21 <- 6.583278*1000000/100000
rho_21 <- as.data.frame(datosConferencia_rho_21) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_21) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_21)

ggplot(data=rho_21, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_21, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_21, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo Puebla', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Puebla, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Puebla')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2


##DATOS SONORA - 26

#Acumulados Sinave

datosSinave_26 <- datosSinave %>%
  filter(CLASIFICACION_FINAL <=3,
         ENTIDAD_RES == 26) %>%
  mutate(casos = 1,
         fecha = as.Date(FECHA_SINTOMAS)) %>%
  group_by(fecha) %>%
  summarise(Confirmados = sum(casos)) %>%
  mutate(Confirmados_cum = cumsum(Confirmados))

datosConferencia_rho_26 <- datosConferencia_rho %>%
  filter(Clave_Entidad == 26) %>% select(fecha, Activos) %>%
  mutate(fecha = as.Date(fecha)) %>%
  group_by(fecha) %>% summarise(Activos = sum(Activos))

datosSinave_26 <- merge(datosSinave_26,datosConferencia_rho_26)
head(datosSinave_26)

#Graficar
ggplot(data= datosSinave_26, aes(x=fecha, y=Confirmados_cum)) + geom_line()
ggplot(data= datosConferencia_rho_26, aes(x=fecha, y=Activos)) + geom_line()

#Calculo del Rt
datosConferencia_rho_26 <- datosSinave_26 %>%
  mutate('confirmados_t-1'=lag(Confirmados_cum, n=1, default=0),
         'confirmados_t-2'=lag(Confirmados_cum, n=2, default=0),
         'confirmados_t-5'=lag(Confirmados_cum, n=5, default=0),
         'confirmados_t-6'=lag(Confirmados_cum, n=6, default = 0),
         'confirmados_t-7'=lag(Confirmados_cum, n=7, default = 0),
         'confirmados_t'=lead(Confirmados_cum, n=1, default = 0)) %>%
  filter(fecha>='2020-04-01') %>% rowwise() %>%
  mutate('rho_t-1'=(confirmados_t+`confirmados_t-1`+`confirmados_t-2`)/(`confirmados_t-5`+`confirmados_t-6`+`confirmados_t-7`))

Pob_26 <- 2.94484*1000000/100000
rho_26 <- as.data.frame(datosConferencia_rho_26) %>% mutate(rho_mav=rolling_mean(`rho_t-1`)) %>%
  mutate(activos_100hab = Activos/Pob_26) %>% mutate(activo_mav=rolling_mean14(activos_100hab)) %>%
  filter(fecha>=as.Date('2020-05-01'))
colnames(rho_26)

ggplot(data=rho_26, aes(x=fecha, y=rho_mav))+geom_line()

#Diagrama del huevo
u <- seq(0,3,length.out=100) 
v <- seq(0,40,length.out=100) 
z = outer(u,v,function(a,b) 10*((a*b)^(-.1)))
#z = outer(u,v,function(a,b) 25*((a*b)^(-.002)))
rownames(z)=u
colnames(z)=v

df <- as.data.frame(z) %>%
  rownames_to_column(var="row") %>%
  gather(col, value, -row) %>%
  mutate(row=as.numeric(row), col=as.numeric(col))

fecha_corte = as.Date("2021-07-01")
#
p <- ggplot()+
  geom_contour(data = df, aes(col, row, z=value), bins=20) +
  geom_contour_filled(data=df, aes(col, row, z=value), bins=10) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values=c('#D73027','#FC8D59','#FEE08B','#FFFFBF','#91CF60','#1A9850','#1A9850','#1A9850','#1A9850','#1A9850')) +
  geom_path(data=rho_26, aes(x=activo_mav, y=rho_mav), lineend = "round", arrow=arrow(angle=20, length = unit(2.5, "mm"),ends = "last", type = "closed")) +
  geom_point(data=rho_26, aes(x=activo_mav, y=rho_mav), size=0.5) +
  geom_hline(yintercept = 1, linetype='dashed') +
  xlab('Positivos diarios por cada 100,000 habitantes, media movil 14 días')+
  ylab('Número de reproducción efectiva') +
  ggtitle('Diagrama de Riesgo Sonora', subtitle = paste0(sprintf('Datos: SINAVE. Fechas de corte: 01-05-2020 a %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n',
                                                         'La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Sonora, comparando el número de reproducción efectiva con número de positivos diarios por 100,000 habitantes')) +
  theme(plot.title=element_text(family='', size=15),
        plot.subtitle=element_text(size=8),
        legend.position="none", legend.direction="horizontal", legend.title = element_text(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.line.x=element_line(colour = "#666666"),
        axis.line.y=element_line(colour = "#666666"),
        axis.ticks = element_line(colour = "#666666"),
        axis.text.x = element_text(margin = margin(t=9, r=0, b=0, l=0)),
        axis.text.y = element_text(margin = margin(t=0, r=9, b=0, l=0)),
        plot.margin=unit(c(1,1,1,1),"cm"))
p

#Grafica del Rt
p2 <- ggplot(rho_9, aes(x=fecha,y = rho_mav, colour = "Rt"))+
  geom_hline(yintercept = 1, linetype='dashed')+
  xlab('Fecha de inicio de sintoma') +
  ylab('Número de reproducción efectiva') +
  geom_line()+
  scale_color_manual(values = c(
    'Rt' = 'darkblue')) +
  labs(color = 'Rt')+
  ggtitle('Evolucion Rt en el tiempo', subtitle = paste0(sprintf('Datos: SINAVE. Fechas: 01-05-2020 al %s', format(as.Date(fecha_corte), "%d-%m-%Y")), '\n','La gráfica representa la trayectoria que ha seguido los contagios a lo largo del tiempo en Sonora')) 

theme(plot.title=element_text( family='', size=15),
      plot.subtitle=element_text(size=8),
      legend.position="none", legend.direction="horizontal", legend.title = element_text(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank(),
      axis.line.x=element_line(colour = "#666666"),
      axis.line.y=element_line(colour = "#666666"),
      axis.ticks = element_line(colour = "#666666"),
      axis.text.x = element_text(margin = margin(t = 9, r = 0, b = 0, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
      plot.margin=unit(c(1,1,1,1),"cm"))

p2

