#la siguiente funcion tomara las siguientes variables:
#n el numero de individuos activos en la generacion 0
#m el numero de semillas en la generacion 0
#c_1 la tasa con la que los individuos activos pasan a inactivos
#c_2 la tasa con la que los individuos inactivos pasan a estar activos

#-------------------------------------------------------------------------------
#--------------------------------------------------Aqui se declara dicha funcion
#-------------------------------------------------------------------------------
Coalescente.Seedbank<-function(n , m , c_1 , c_2){
  i<-n                                #Empezamos con los tamaños ya establecidos
  j<-m                                           #para nuestra poblacion inicial
  N_T_K<-rep(NA , n)            #Tomaremos el modelo como el proceso estocastico
  M_T_K<-rep(NA , n)                                            #{N_T_k , M_T_K}
  
  Tiempos<-rep(NA,n)           #Se interpretara que los eventos de coalescencia, 
                          #desactivacion y activacion como una competencia donde
                       #el ganador lo determinara el menor tiempo y se guardaran 
                        #los tiempos que tardaron en suceder desde paso anterior
  
  Etiquetas<-rep(NA,n) #Se guardaran las etiquetas de que evento resulto ganador
  
  k<-rep(NA,n)      #Se genera un vector que marque los pasos que se 
                         #realizan
  
  for (l in 1:n){          #En este ciclo se generaran competencias durante cada
                        #iteracion para esto se definen las siguientes variables
    k[l]<-l                                        #Aqui guardaremos el indice k

                          #Los siguientes tiempos estaran determinados siguiendo 
                                 #una funcion exponencial con radio de ocurencia 
                                                                #correspondiente
    Tiempo_Coalescencia<-ifelse(i == 0 |i ==1, 
                                max(c_1*i , c_2*j )  , 
                                rexp(1,choose(i,2)))
    Tiempo_Desactivacion<-ifelse(i == 0 , 
                                 c_1*i , 
                                 rexp(1,c_1*i))
    Tiempo_Activacion<-ifelse(j == 0 , 
                              max(c_1*i , choose(i,2) ) , 
                              rexp(1,c_2*j)) 
#--------------------------ESTO SE EXPLICARA MEJOR EN LA TESIS------------------
#---------------------------PORQUE REQUIERE UNA DEMOSTRACION--------------------
    competencia<-c(Tiempo_Coalescencia , Tiempo_Desactivacion , 
                   Tiempo_Activacion) 
    ganador<-min(competencia)  
    Tiempos[l]<-ganador       #En un vector guardaremos las marcas de tiempo del 
                                                                 #tiempo ganador 
    
    #El siguiente ciclo sera determinar el estado al que saltara nuestro proceso
   #Para esto se deberan comparar cada uno de los competidores contra el ganador
    if(ganador == Tiempo_Coalescencia){
      Etiquetas[l]<-1            #Esto indicara que en el paso l se registro una 
                                                                   #coalescencia
      i<-i-1                                   #Los activos pierden un individuo
    }else{if(ganador == Tiempo_Desactivacion){
      Etiquetas[l]<-2              #Esto indica que en el paso l se registro una 
                                                                  #desactivacion
      i<-i-1                                   #Los activos pierden un individuo
      j<-j+1                                     #Las semillas ganan una semilla
    }else{
      Etiquetas[l]<-3              #Esto indica que en el paso l se registro una 
                                                                     #activacion
      i<-i+1                                 #gana un individuo que se despierta
      j<-j-1                                  #pierde un individuo que despierta
    }
    }
    N_T_K[l]<-i        #Por ultimo guardamos i y j para Registrar en que estado se 
    M_T_K[l]<-j                                           #se encuentra el proceso
  }
  Seed.Bank.C<-data.frame( k , Etiquetas , Tiempos , N_T_K ,
                           M_T_K )           #Se guardan los datos en un dataframe

}


#-------------------------------------------------------------------------------
#--------------------------------------------------------Aqui termina la funcion
#-------------------------------------------------------------------------------
#---------------------------Se obtiene un ejemplo para visualizar los resultados

#>>>>>>>>>>>>>>>>>>>>>>>>>>Solo ejemplo, no necesita comprobarse

          ejemplo<-Coalescente.Seedbank(10, 0 , 1 , 1)
          

