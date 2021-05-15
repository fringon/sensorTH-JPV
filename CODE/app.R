ui <- fluidPage(
  
  h1(strong("LATITUD - FUNDACION LATU")),
  h2("RESUMEN DATOS DE MUESTREADOR"),
  
  sidebarLayout(
    sidebarPanel (
      textInput(inputId = "dir_dato",
                label = "UBICACION DATOS",
                value = "C:/Users/mauro/Desktop/PROYECTOS/LATITUD/SENSOR-CASARONE/SHINY-APP/txt-SENSOR/22-4-21"),
      textInput(inputId = "dir_resultados",
                label = "UBICACION RESUMEN",
                value = "C:/Users/mauro/Desktop/PROYECTOS/LATITUD/SENSOR-CASARONE/SHINY-APP"),
      textInput(inputId = "main_name",
                label = "NOMBRE ARCHIVO PRINCIPAL",
                value = "RESUMEN_MASTER"),
      textInput(inputId = "dir_code",
                label = "CODIGO",
                value = "C:/Users/mauro/Desktop/PROYECTOS/LATITUD/SENSOR-CASARONE/CODE"),
      actionButton(inputId = "Cargar",label = "Analizar"), 
      textOutput("msj_1"),
    ),
    mainPanel( 
      tabsetPanel(
        tabPanel("TODAS LAS CORRIDAS",plotOutput("df")),
        tabPanel("CORRIDAS NUEVAS",plotOutput("df_new")),
        tabPanel("CORRIDAS ANTERIORES",plotOutput("df_old"))
      )
    )
  )
)


server <- function(input, output) {
  
  respuesta<-eventReactive(input$Cargar, {

    dir_resultados<-input$dir_resultados
    dir_dato<-input$dir_dato
    main_name<-input$main_name
    dir_code<-input$dir_code
  
    source(paste(dir_code,"funciones.R",sep = "/"))

    if (!verificar_estado(main_name,dir_dato,dir_resultados)) {
      "pone bien las direcciones!!!"
    } else {
      
      # datos<-list.files(dir_dato, pattern = "*.txt",full.names = TRUE, recursive = TRUE)
      # fecha<-fecha_analisis(datos) 
      # fechas_analizadas<-list.dirs(paste(dir_resultados,'csv-SENSOR',sep = '/')) %>% path_file()
      source(paste(dir_code,"main.R",sep = "/"),local = TRUE)
      
      
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
        
        if ( nrow(df_old)!=0 ) {
          
          g_df<-ggplot(df) +
            aes(x = hora_acum, y = humedad_grano, color = as.factor(corrida)) +
            labs(color = "# Corrida") +
            geom_point(size = 1L) +
            scale_color_hue() +
            theme_minimal()
          
          g_df_new<-ggplot(df_new) +
            aes(x = hora_acum, y = humedad_grano, color = as.factor(corrida)) +
            labs(color = "# Corrida") +
            geom_point(size = 1L) +
            scale_color_hue() +
            theme_minimal()
          
          g_df_old<-ggplot(df_old) +
            aes(x = hora_acum, y = humedad_grano, color = as.factor(corrida)) +
            labs(color = "# Corrida") +
            geom_point(size = 1L) +
            scale_color_hue() +
            theme_minimal()
          
          output$df<-renderPlot({g_df})
          output$df_new<-renderPlot({g_df_new})
          output$df_old<-renderPlot({g_df_old})
          
        } #GRAFICOS
        
        df_new
      }
    }

  })

  output$msj_1<-renderText({

    if (is.data.frame( respuesta() )) {
      "Analisis completo"
    } else {
      respuesta()
    }

  })
  
}

shinyApp(ui, server)

