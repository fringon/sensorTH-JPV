
datos<-list.files(dir_dato, pattern = "*.txt",full.names = TRUE, recursive = TRUE)
fecha<-fecha_analisis(datos) 
fechas_analizadas<-list.dirs(paste(dir_resultados,'csv-SENSOR',sep = '/')) %>% path_file()

if (fecha %in% fechas_analizadas){

  "Carpeta ya analizada"

  } else {

  # crear tabla THV completa y despues creartablas por corridas
  tablas<-crear_tabla_por_fecha(datos)

  df_new<-crear_tabla_corridas(tablas[["THV"]]) %>% clean_names() %>% mutate(fecha=as.character(fecha))
  df_old<-read.csv2(file=paste(dir_resultados,paste(main_name,".csv",sep=""),sep = "/")) %>% clean_names()

  ult_corr<-ifelse(nrow(df_old)==0, 0, tail(df_new[[1]],n=1))
  df_new<-df_new %>% mutate(corrida= corrida + ult_corr)

  if (ult_corr==0){
    df<-df_new %>% mutate_if(is.numeric,signif,3)
  }else{
    df<-df_old %>% bind_rows(df_new) %>% mutate_if(is.numeric,signif,5)
  }

  actualizar_resumen(dir_resultados,df,main_name)
  actualizar_dir(tablas[['TEMP']],tablas[['HUM']],tablas[['VOL']],tablas[['ES']],dir_resultados,fecha)
  respaldo(df,dir_resultados,fecha)
}


