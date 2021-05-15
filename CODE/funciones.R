
verificar_estado<- function(main_name,dir_dato,dir_resultados){
  
  #TRUE if main_name,dir_dato,dir_resultdos are ok
  
  resumen_ok<-file.exists(paste(dir_resultados,paste(main_name,".csv",sep=""),sep = "/"))
  datos_ok<-length(list.dirs(dir_dato))==6
  
  return(resumen_ok & datos_ok)
}

fecha_analisis<-function (datos){
  
  #INPUT: lista con las direcciones de los .txt a analizar
  #OUTPUT: fecha (character) de la carpeta en analisis
  
  return (datos[1] %>% path_dir() %>% path_dir() %>% path_file())
}

crear_tabla_por_fecha<- function (datos){
  
  # INPUT: ruta a dir de archivos .txt (tienen que haber 5 archivos 100pre con los mismos nombres
  #        y mismo orden estados humedad, temp, variedad, voltaj
  # OUTPUT: lista con df crudos [ THV,TEMEPRATURA, HUMEDAD, VOLTAJE,DOSIFICADOR] de una o mas corridas
  #         THV es la tabla conjunta final
  
  ESTADOS<-read.table(datos[1], fileEncoding = "UTF-16LE",fill = TRUE,header = TRUE) %>%
    select(-c(4,5)) %>%
    filter(VarName %in% c("ES TORRE", "ES MARCO","ES DOSIFICADOR")) 

  HUM<-read.table(datos[2], fileEncoding = "UTF-16LE",fill = TRUE,header = TRUE) %>%
    select(-c(4,5)) %>%
    filter(VarName %in% c("HUMEDAD GRANO", "TEMPERATURA GRANO")) %>% 
    spread(key=VarName,VarValue)
  
  VOL<-read.table(datos[5], fileEncoding = "UTF-16LE",fill = TRUE,header = TRUE) %>%
    select(-c(4,5)) %>%
    filter(VarName %in% c("Vol_h_estatico", "Vol_t_estatico")) %>% 
    spread(key=VarName,VarValue)
  
  TEMP<-read.table(datos[3], fileEncoding = "UTF-16LE",fill = TRUE,header = TRUE) %>%
    select(-c(4,5)) %>% 
    filter(VarName %in% c("Temp_sec_estatico", "Temp_hum_estatico")) %>% 
    spread(key=VarName,VarValue)
  
  
  aux<-function(fecha,CODIGO){
    
    aux<-TRUE
    fila<- 1
    valor<-0
    fecha<-fecha %>% as.POSIXct()
    lim<-nrow(CODIGO)
    
     while (aux & (fila<lim)) {
      intervalo<-interval(as.POSIXct(CODIGO[fila,2]),as.POSIXct(CODIGO[fila+1,2])-1)
      if (fecha %within% intervalo){
        valor<-CODIGO[fila,3]
        aux=FALSE
      }
      fila = fila+1
    }
    return(valor)
  } # agregar estados por fecha
  
  torre<-map_dbl(TEMP[[1]], aux, CODIGO=(ESTADOS %>% filter(VarName=="ES TORRE")))
  marco<-map_dbl(TEMP[[1]], aux, CODIGO=(ESTADOS %>% filter(VarName=="ES MARCO")))
  dosi<-map_dbl(TEMP[[1]], aux, CODIGO=(ESTADOS %>% filter(VarName=="ES DOSIFICADOR")))
  
  THV<-TEMP %>% 
    drop_na() %>%
    mutate(ES_TORRE = torre,ES_MARCO = marco,ES_DOSIFICADOR = dosi) %>% 
    inner_join(HUM,by=c("TimeString" = "TimeString")) %>%
    inner_join(VOL,by=c("TimeString" = "TimeString")) %>%
    mutate(Fecha = as.POSIXct(TimeString,tz="UTC")) %>% relocate(Fecha) %>%
    filter(ES_TORRE==-1 & ES_MARCO==-1) %>%
    select(-c(2,5,6)) %>% relocate(ES_DOSIFICADOR,.after = last_col()) %>% 
    add_column(PARADA='-')
  
  return(list(THV=THV,TEMP=TEMP,HUM=HUM,VOL=VOL,ES=ESTADOS))
}

crear_tabla_corridas<-function (THV) {
  
  # INPUT: tabla [temperatura, humedad, voltaje] con varias corridas
  # OUTPUT: tabla con las corridas identificadas y hora acumulada
  
  #para detectar las filas donde se da un salto de corrida, salto de 80 min y 2pto de humedad
  n=nrow(THV)
  filas<-c()
  paradas<-c()
  for (i in 3:n) {
    
    previo=THV[i-1,1]
    actual=THV[i,1]
    
    H_previo<-THV[(i-3):(i-1),4]
    H_actual<-THV[i:(i+2),4]
    
    H_previo<-mean(H_previo[H_previo>0])
    H_actual<-mean(H_actual[H_actual>0])
    
    if (is.nan(H_previo)){H_previo=0}
    if (is.nan(H_actual)){H_actual=0}
    if ((difftime(actual,previo,units="mins")>80) & (abs(H_actual-H_previo)>2) ) {
      filas<-filas %>% append(i-1)
    } else if ((difftime(actual,previo,units="mins")>80) & (H_actual==0 | H_previo==0)){
      filas<-filas %>% append(i-1)
    } else if (difftime(actual,previo,units="mins")>15) {
      paradas<-paradas %>% append(c(i,i-1)) 
    }
  }
  filas<-filas %>% append(n)
  
  # crear columna con numero de corrida
  Corrida<-c()
  n=1
  for(i in 1:length(filas)) {
    if (i==1){
      Corrida<-Corrida %>% append(rep(n,filas[i]))
      n=n+1
    } else {
      Corrida<-Corrida %>% append(rep(n,filas[i]-filas[i-1]))
      n=n+1
    }
  }
  
  THV<-THV %>% bind_cols(as.data.frame(Corrida)) %>% relocate(Corrida)
  THV_Corrida<-THV %>% split(Corrida)
  
  THV_fin<-map_dfr(THV_Corrida,agregar_t_acum)
  if (length(paradas)!=0){THV_fin[paradas,10]<-"STOP"}
  
  return(THV_fin)
}

agregar_t_acum<-function(tb_corr){
  
  #INPUT tabla THV con datos de una sola corrida
  #OUTPUT misma tabla THV pero con una columna de hora acumulada
  
  hora_acum<-c(0)
  
  for (i in 2:nrow(tb_corr)){
    dif<-difftime(tb_corr[i,2],tb_corr[i-1,2],units="mins")
    hora_acum<-hora_acum %>% append(dif+last(hora_acum))
  }
  
  hora_acum<-hora_acum %>% round()
  tb_corr<-tb_corr %>% bind_cols(as.data.frame(hora_acum)) %>%
    relocate(hora_acum,.after=Corrida)
  
  return(tb_corr)
}

actualizar_resumen<-function (dir_resultados,df,main_name){
  
  #INPUT: direccion de resumen, THV a agregar, nrow para agregar el THV
  #OUTPUUT: guarda el archivo resumen actualizado con THV
  
  original<-paste(dir_resultados,paste(main_name,".csv",sep=""),sep = "/")
  write.csv2(df, file=original, row.names = FALSE,na="")
  
}

actualizar_dir<-function (df_T,df_H,df_V,df_E,dir_resultados,fecha){
  
  #INPUT tablas crudas de T H y V E direccion donde ubicar las tablas y carpeta en donde guardarlas
  #OUTPUT guardar tablas T H y V E .csv en carpeta destino
 
  carpeta_final= paste(dir_resultados,paste('csv-SENSOR',fecha,sep = '/'),sep = "/")
  dir.create(carpeta_final)
  
  ubi<-map2_chr(carpeta_final,c("TEMPERATURA.csv","HUMEDAD.csv","VOLTAJE.csv","ESTADOS.csv"),paste,sep="/")
  tab<-list(df_T,df_H,df_V,df_E)

  map2(tab,ubi,write.csv2,row.names = FALSE)
}

respaldo<-function (df,direccion,fecha){
  
  #INPUT tablas crudas de T H y V direccion donde ubicar las tablas y carpeta en donde guardarlas
  #OUTPUT guardar tablas T H y V .xlsx en carpeta destino
  
  ubi<-paste(direccion,'RESPALDO',sep="/")
  nombre=paste('RESUMEN_MASTER',fecha,sep = '_')
  nombre_csv=paste(nombre,'.csv',sep = '')
  ubi_respaldo<-paste(ubi,nombre_csv,sep = "/")
  
  write.csv2(df,ubi_respaldo, row.names = FALSE)
  
}