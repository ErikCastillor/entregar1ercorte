library(nycflights13)
library(tidyverse)



nycflights13_al <- nycflights13::airlines #aerolineas
nycflights13_ap <- nycflights13::airports #aeropuertos
nycflights13_vu <- nycflights13::flights #vuelos
nycflights13_av <- nycflights13::planes #aviones
nycflights13_cl <- nycflights13::weather #clima

opcion <- 1
retrieve_answer <- function(x){
  if (x == 1)
  {
    #Ejercicio 5.2.4
    '5.2.4-1.2'<-filter (nycflights13_vu, arr_delay > '2' )%>%#vuelos retrasados mas de dos horas
    A
    '5.2.4-1.2'<-filter (nycflights13_vu, dest == "IAH"| dest =="HOU")#vuelos con destino a houston (IAHo HOU)
    '5.2.4-1.3'<-filter (nycflights13_vu, carrier == "AA"| dest =="UA" | dest =="DL") #Fueron operados por United, American o Delta (CARRIER)primero se consulta la tabla de aerolineas para sacar las siglas de aerolinea
    '5.2.4-1.4'<-filter (nycflights13_vu, month==7| month==8 | month==9) #Salida en verano (julio, agosto y septiembre)
    '5.2.4-1.5'<-filter (nycflights13_vu, arr_delay > 2 & dep_delay <= 0)#Llegó más de dos horas tarde, pero no se fue tarde
    '5.2.4-1.6'<-filter (nycflights13_vu, dep_delay >= 1 & arr_delay <= 0.5)#Se retrasaron al menos una hora, pero recuperaron más de 30 minutos en vuelo
    '5.2.4-1.7'<-filter (nycflights13_vu, dep_time <= 600)#Salida entre la medianoche y las 6 a. m. (incluidas)
    print (A
           )
    #con operador between
    filter (nycflights13_vu,between( arr_delay , '2' ,'200'))## para vuelos retrasados entre 2 y 200 hrs
    filter (nycflights13_vu,between(dest , "IAH" , "HOU" ))# en este caso por el tipo de dato arrojara un error de coercion
    #filter (nycflights13_vu,between(carrier , "AA","UA","DL"))# al trabajar con cadenas de caracteres se presenta el inconveniente de normalización
    filter (nycflights13_vu,between(month,7,9))#para el tema de meses años o dias, es perfecto el uso por que between representa un rango
    filter (nycflights13_vu,between(arr_delay, '2','200') & dep_delay<='0')#podemos usarlo como combinacion entre un valor exacto o con parametros y una comparacion con rangos

    filter (nycflights13_vu, between( dep_delay, '1','100') & between (arr_delay,0, 0.5)) # mismas combinaciones para rangos de enteros
    filter (nycflights13_vu,between( dep_time , '0' , '600'))#podemos asumir que las 12 de la media noche son 00:00 y al ser enteros lo manejamos como 0
  }
  else if (x == 2){
    #5.3.1
    colnames(nycflights13_vu)
    '5.3.1.1'<- nycflights13_vu%>% #¿Cómo podría utilizar arrange()para ordenar todos los valores que faltan al principio? (Sugerencia: use is.na()).
      arrange(desc(is.na(dep_time))) # en esta ocasion se organiza por tiempos de salida en NA

    '5.3.1.2'<- nycflights13_vu%>% #Ordenar flightspara encontrar los vuelos más retrasados. Encuentra los vuelos que salieron antes.
      arrange (dep_delay)# implementamos el la funcion para ver que vuelos salieron mas horas de antelacion, y al final encontraremos los retrasados
    '5.3.1.3'<- nycflights13_vu%>%#Ordenar flightspara encontrar los vuelos más rápidos (velocidad más alta).
      arrange (air_time*distance)
    '5.3.1.4'<- nycflights13_vu%>%#¿Qué vuelos viajaron más lejos? ¿Cuál viajó menos?
      arrange ((distance/air_time)*60)
    '5.3.1.4-2 menos' <- nycflights13_vu%>%
      group_by(distance)%>%
      transmute(carrier,flight,tailnum,velocidad_promedio = (distance/air_time)*60)%>%
      ungroup()

  }
  else if (x == 3){
    #5.4.1

    '5.4.1.2'<- nycflights13_vu%>%#¿Qué sucede si incluye el nombre de una variable varias veces en una select()llamada?
      select(carrier,carrier,carrier,flight,tailnum)%>%#en realidad solo trae una unica fila a pesar de ser llamada 3 veces
      arrange(carrier)

    #5.4.1.3 #¿Qué hace la any_of()función? ¿Por qué podría ser útil junto con este vector?
    vars <- c("year", "month", "day", "dep_delay", "arr_delay")%>%
      SELECT ("dep_delay",any_of(na))
    #5.4.1.4#¿Te sorprende el resultado de ejecutar el siguiente código? ¿Cómo tratan los ayudantes selectos el caso de forma predeterminada? ¿Cómo se puede cambiar ese valor predeterminado?
    select(flights, contains("TIME"))# en realidad lo que hace es generar un filtro en las columnas que tienen la palabra time como palabra clave
    select(flights, contains("dep"))#en este caso se usa el filtro para buscar la palabra dep en los encabezados de columnas


  }
 else if (x == 4){
    #5.5.2
    '5.5.2.1' <- nycflights13_vu%>% #Actualmente dep_time y sched_dep_time son convenientes a la vista, pero difíciles de calcular porque en realidad no son números continuos. Conviértalos a una representación más conveniente de la cantidad de minutos desde la medianoche.
      transmute(dep_time,sched_dep_time,dep_horas =(dep_time %/% 100), dep_minutos = (dep_time %% 100 ),sched_horas=(sched_dep_time%/% 100),sched_minutos = (sched_dep_time %% 100 ))#lo mas sencillo en este caso seria fraccionarlo en dos columnas para que sea legible

    '5.5.2.2' <- nycflights13_vu%>% # Comparar air_timecon arr_time - dep_time. Que esperas ver? ¿Que ves? ¿Qué necesitas hacer para arreglarlo?
      select(air_time,arr_time,dep_time)%>%# en primera medida se ve que el air time esta en minutos, debo convertirlo a horas.
      group_by(arr_time)%>%#procuro dejar todo en horas para que sea mass facil la lectura y el calculo correspondiente
      transmute(hora_llegada=(arr_time/100),hora_salida=(dep_time/100),tiempo_vuelo_horas=(air_time/60),arr_time)%>%
      ungroup%>%
      arrange(arr_time)

  }
  else if (x == 5){
    #5.6.7
    #Haga una lluvia de ideas sobre al menos 5 formas diferentes de evaluar las características típicas de retraso de un grupo de vuelos. Considere los siguientes escenarios:
    #Un vuelo llega 15 minutos antes el 50% del tiempo y 15 minutos tarde el 50% del tiempo.
    Tiempo_adicional<-15
    not_cancelled %>%
      group_by(year, month, day) %>%
      summarise(n_early = sum(dep_time)+Tiempo_adicional)#teniendo en cuenta que la hora de llegada de los vuelos sera aumentada
    #Un vuelo siempre llega 10 minutos tarde.
    '10_TARDE'<- not_cancelled %>%
      group_by(year, month, day)%>%
      transmute(flight,tailnum,origin,dest,sched_arr_time,hora_con_retraso=(sched_arr_time + 10))%>%
      ungroup
    #Un vuelo llega 30 minutos antes el 50% del tiempo y 30 minutos tarde el 50% del tiempo.
    '30_mminutos'<- not_cancelled %>%
      group_by(year, month, day)%>%
      transmute(flight,tailnum,origin,dest,sched_arr_time,hora_adelanto=sched_arr_time-30,hora_atraso=sched_arr_time+30)%>%
      transmute(Tiempo_desviacion=(mean(hora_atraso-hora_adelanto)))%>%
      ungroup
    #El 99% de las veces un vuelo es puntual. El 1% de las veces llega 2 horas tarde.

    #¿Qué es más importante: el retraso en la llegada o el retraso en la salida?
    #En esta ocasion podemos determinar que genera mas inconvenientes el retraso en la llegada, por que a partir de las tablas
    #podiamos determinar que muchos vuelos recuperaban tiempo en el aire, aumentando su velocidad, por ende al llegar tarde significa que desaprovecha la posibilidad de viajar a mas velocidad
  }
  else if (num == 6){
    #5.7.1 Vuelva a consultar las listas de funciones útiles de mutación y filtrado. Describe cómo cambia cada operación cuando la combinas con la agrupación.
    avion_retraso <- nycflights13_vu%>%
      arrange (flights,desc(dep_delay))%>%
      group_by(tailnum)
  }
}

while (opcion==1){
  num <- readline(prompt = "ingrese un numero de punto")
  num <- as.numeric (num)
  retrieve_answer (x = num)
  opcion <- readline ("continue?")
  if (opcion==2){
    print ('Gracias')
  }
}











