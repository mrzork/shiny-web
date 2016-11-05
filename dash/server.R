mLoad <- function(...) {
  sapply(sapply(match.call(), as.character)[-1], require, character.only = TRUE)
}
Sys.setlocale("LC_TIME", "english")
suppressMessages(mLoad(shiny,shinydashboard,dplyr,xts,tidyr,DT,rpivotTable,googleVis,dygraphs,lubridate,ggplot2,ggvis))


options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session){
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$actual<-renderPrint({ finf <- file.info(dir(), extra_cols = FALSE)
  cat("Ultima Actualizacíon:\n")
  print(sub("2016-","",finf$mtime[row.names(finf)=="Actualizada.csv"])) })
  
  DataU<-observeEvent(input$do, {
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
    if(input$passwd=="holamundo"){
      a<-read.csv(inFile$datapath, sep=";", stringsAsFactors=FALSE,fileEncoding="latin1")
      write.csv2(a,file="./Actualizada.csv",row.names =FALSE,na = "",fileEncoding="latin1")
    }else{
    }

  })
  
  Pcliente<-reactive({
    D<-read.csv("./JMG/Pcliente.csv", sep=";", stringsAsFactors=FALSE,fileEncoding="latin1")
  })
  
  Prob<-reactive({
    file<-paste("./Probabilidad_Cump.csv",sep="")
    w <- read.csv(file, sep=";", stringsAsFactors=FALSE,fileEncoding="latin1")
    w2<-w %>% filter(Dia==1) %>% mutate(Captotal=P0) %>% select(AREA,Captotal)
    Dat0 <- Data() %>% filter(Fecha.real.de.solución!="")
    Dat <- Data() %>% filter(Fecha.real.de.solución=="")
    w<-w[w$Dia==as.numeric(format(Sys.time(), "%d")),]
    Dat<-Dat %>% group_by(AREA) %>% summarise(Total=n(),Vencen_en_Mes=sum(Se.vence.en.el.mes+Viene.vencido.de.meses.anteriores))
    Dat0<-Dat0 %>% group_by(AREA) %>% summarise(TotalSol=n())
    if(dim(Dat)[1]==0){
      w<-data.frame(AREA=c("Gestión del Servicio","Infraestructura Informatica","Sistemas de Informacion","Telecomunicaciones","GT"),
                    Total=0,Vencen_en_Mes=0,P0=1,Probabilidad=100,CapacidadRMes=0,Captotal=0,TotalSol=0,CuotaMes=100)
    }else{
    suppressMessages(w<-inner_join(Dat,w) %>% mutate(Probabilidad=100*(P0/Vencen_en_Mes))%>% select(AREA,Total,Vencen_en_Mes,P0,Probabilidad))
    w$Probabilidad[w$Probabilidad>100]<-100
    w$CapacidadRMes<-w$P0-w$Vencen_en_Mes
    suppressMessages(w<-inner_join(w,w2))
    suppressMessages(w<-inner_join(w,Dat0))
    w$CuotaMes<-(w$TotalSol/w$Captotal)*100
    w<-rbind(w,c("GT",colSums(w[,2:4]),mean(w$Probabilidad),colSums(w[,6:8]),mean(w$CuotaMes)))
    w<-data.frame(w)
    }
    w
  }) 
  
  Data<- reactive({
    file2<-paste("./Actualizada.csv",sep="")
    Data <- read.csv(file2, sep=";", stringsAsFactors=FALSE,fileEncoding="latin1")

    Datah <<- Data
    inFile <- Data
    if (is.null(inFile))
      return(NULL)
    
    Data$Incumplidos<-(Data$Cumplió.SLA*-1)+1
    # Data<-Data[Data$Razón != "Solución implementada al problema reportado",]
    # Data<-Data[Data$Razón != "Requiere cambio planificado",]

    #filtra casos fallidos
    CasosJUL<-c(127093,127128,127409,122953,122973,122974,122976,122977,122978,122979,122980,122983,122984,122988,122990,122991,122992,122995,122996,122997,122998,122999,123003,123004,123006,123007,123009,123010,123011,123012,123013,123015,123018,123019,123020,123021,123022,123023,123025,123026,123027,123029,123030,123031,123033,123034,123043,123044,123046,123047,123051,123052,123055,123056,123057,123059,123060,123061,123083,123088,128129,123579,129281)
    Casow<-c(129190,126530,126719,126904,127195,127200,128220,128290,128639,127861,127895,128133,CasosJUL)
    Data<-Data[!(Data$Caso %in% Casow),]
    ##filtros adds

    
    Data<-Data[Data$GRUPO != "Mesas de ayuda",]
    Data<-Data[Data$SUBGRUPO != "Centro de Servicios - SAU No Operativo",]
    Data<-Data[Data$SUBGRUPO != "Centro de Servicios - MAS No Operativo",]
    Data<-Data[Data$SUBGRUPO != "Proyectos",]
   
     if(input$B1=="Usuarios Finales"){
      Data<-Data %>% filter(Tipo.de.Servicio=="Servicios Usuario Final")
     }
    
    if(input$B1=="Usuarios Internos"){
      Data<-Data %>% filter(Tipo.de.Servicio=="Servicios Internos")
    }
    
    if(input$B2=="Incidentes"){
      Data<-Data %>% filter(Tipo.de.caso=="Incidente")
    }
    if(input$B2=="Requerimientos"){
      Data<-Data %>% filter(Tipo.de.caso=="Requerimiento")
    }
    
    #### identidad proteccion
    q<-data.frame(Especialista=as.character(unique(Data$Especialista)))
    q$esp<-paste("User",1:length(q$Especialista),sep="")
    Da<-data.frame(Especialista=Data$Especialista)
    suppressMessages(q2<-inner_join(Da,q))
    Data$Especialista<-q2$esp
    rm(q,q2,Da)
    #####
    
    Data
  })
  
  Tiemp<-reactive({
    Data<-Data()
    Serie<-ymd_hms(as.POSIXct(seq(ISOdate(as.numeric(format(Sys.time(), "%Y")),as.numeric(format(Sys.time(), "%m")),1)-60*60*12, ISOdate(as.numeric(format(Sys.time(), "%Y")),as.numeric(format(Sys.time(),"%m")),as.numeric(format(Sys.time(),"%d")))+60*60*12,"hours")),tz= "") #America/New_York
   
    ####tratar festivos y fines de semana en 2016. 
    Serie<-Serie[!weekdays.Date(Serie)%in%c("sábado","domingo")]
    festivos<-as.POSIXct(strptime(c("01/01/2016","04/01/2016","21/03/2016","20/03/2016","24/03/2016",
    "25/03/2016","27/03/2016","01/05/2016","09/05/2016","30/05/2016",
    "06/06/2016","04/07/2016","20/07/2016","07/08/2016","15/08/2016",
    "17/10/2016","07/11/2016","14/11/2016","08/12/2016","25/12/2016"),"%d/%m/%Y")) 
    Serie<-Serie[!as.POSIXct(strftime(Serie,"%Y-%m-%d")) %in% festivos]
    #####
    
    maxim<-Serie[length(Serie)]
    Data$Registro<-dmy_hms(Data$Fecha.de.registro,tz="America/New_York")
    Data$Vence<-dmy_hms(Data$Fecha.de.vencimiento,tz="America/New_York")
    Data$Solucion<-dmy_hms(Data$Fecha.real.de.solución,tz="America/New_York")
    Data<-Data %>% filter(Vence<maxim)
    
    a<-b<-d<-0
    
    for(i in 1:length(Serie)){
      a[i]<-sum(Data$Registro<Serie[i],na.rm = T)
      b[i]<-sum(Data$Solucion<Serie[i],na.rm = T)
      d[i]<-sum(Data$Vence<Serie[i],na.rm = T)
    }
    e<-a-b
    w<-((d/b)-1)*100
    razon<-(b/a)*100
    w[is.na(w)]<-0
    razon[is.na(razon)]<-0
    
    q<-list(a,b,d,e,w,razon,Serie)
  })
  
  output$GT <- DT::renderDataTable({ 
    data<-datatable(Data(),
                    extensions = 'Responsive')
    data
  })
  
  output$GDS <- DT::renderDataTable({ 
    dat<-filter(Data(),AREA=="Gestión del Servicio")
    data<-datatable(dat,extensions = 'Responsive')
    data
  })
  
  output$SSII <- DT::renderDataTable({ 
    dat<-filter(Data(),AREA=="Sistemas de Informacion")
    data<-datatable(dat,extensions = 'Responsive')
    data
  })
  
  output$Telco <- DT::renderDataTable({ 
    dat<-filter(Data(),AREA=="Telecomunicaciones")
    data<-datatable(dat,extensions = 'Responsive')
    data
  })
  
  output$II <- DT::renderDataTable({ 
    dat<-filter(Data(),AREA=="Infraestructura Informatica")
    data<-datatable(dat,extensions = 'Responsive')
    data
  })
  
  output$Gestion<-DT::renderDataTable({
    Dat <- Data() %>% filter(Fecha.real.de.solución=="")
    Dat<-Dat %>% group_by(AREA,GRUPO,SUBGRUPO,Especialista)  %>%mutate(incumplidos=1-Cumplió.SLA) %>% summarise(Casos_Activos_Hoy=n(),Vencen_en_Mes=sum(Se.vence.en.el.mes),Casos_Vencidos=sum(incumplidos))
    Dat<-datatable(Dat,rownames=FALSE, extensions = 'Buttons', options = list(pageLength = 1000,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'print')
    ))
    Dat
  })
  
  output$Probabi<-DT::renderDataTable({
    Dat<-Prob()
    Dat<-datatable(Dat,rownames=FALSE, extensions = 'Buttons', options = list(pageLength = 1000,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'print')
    ))
    Dat
  })
  
  
  output$JMGDT<-DT::renderDataTable({
    Pcliente<-Pcliente()
    if(input$AREAP=="Todas"){
      Pcliente<-Pcliente
    }
    if(input$AREAP2=="II"){
      Pcliente<-Pcliente %>% filter(Area=="Infraestructura Informatica")  
    }
    if(input$AREAP2=="SSII"){
      Pcliente<-Pcliente %>% filter(Area=="Sistemas de Informacion")  
    }
    if(input$AREAP2=="TELCO"){
      Pcliente<-Pcliente %>% filter(Area=="Telecomunicaciones")  
    }
    if(input$AREAP2=="GDS"){
      Pcliente<-Pcliente %>% filter(Area=="Gestión del Servicio")  
    } 
    Dat<-Pcliente %>% group_by(Area,especialista_resp,numero_de_caso) %>% summarise(total=n()) %>% filter(total>1)
    Dat<-datatable(Dat,rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'print')
    ))
    Dat
  })
  #### Pivotes
  
  output$pivote<- rpivotTable::renderRpivotTable({
    suppressWarnings(da<-Data() %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
    
    names(da)[c(11:14,17:22,32:35,37:42)]<-c("Fecha_R","Fecha_V","Fecha_PS","Fecha_SR","Sol_SLA",
                                       "Inclu_Ind","Cerrado_Per","Cerrado_Anti","Cerrado_Adel",
                                       "Vence_Mes","Cumplio_Sla","Nom_Sla","Cambio_Sla","Reapertura",
                                       "Jerarquia1","Jerarquia2","Jerarquia3","Jerarquia4","Cliente","CI")
    da<-da[,c(1:22,32:35,37:45)]
    rpivotTable(da, row=c("AREA","GRUPO","SUBGRUPO"), aggregatorName="Sum", 
                              vals="Incumplidos", rendererName="Heatmap",width="100%", height="1000px")
  })
  
  output$pivoteGDS<- rpivotTable::renderRpivotTable({
    da<-Data() %>% filter(AREA=="Gestión del Servicio")
    rpivotTable(da, row=c("AREA","GRUPO","SUBGRUPO"), aggregatorName="Sum", 
                              vals="Incumplidos", rendererName="Heatmap",width="100%", height="1000px")
  })
  
  output$pivoteCI<- rpivotTable::renderRpivotTable({
    w<-Data() %>% filter(Tipo.de.caso=="Incidente") 
    w$Registro<-format(dmy_hms(w$Fecha.de.registro,tz="America/New_York"),format = "%d-%m-%Y")
    w<-w%>% group_by(AREA,GRUPO,CI,Servicio,Registro) %>% summarise(total=n()) 
    mini<-round(sum(w$total)*0.002)
    w<-w %>%filter(total>mini)  %>% arrange(desc(total),AREA,GRUPO,CI,Servicio,Registro) 
    w$CI[w$CI==""]<-"CI No_Registrado"
    w$GRUPO[w$GRUPO==""]<-"No_Grupo"

    w %>% rpivotTable(row=c("AREA","CI","Servicio","total","Registro"), aggregatorName="Sum",
                              vals="total", rendererName="Treemap",width="100%", height="1000px")

    })

  ##### valueboxes de ICS
  
  output$progressICGT <- renderValueBox({
    MetaIC<-0.9398
    dif<-0.005
    Nam<- "Cumplimiento de Gerencia"
    # indicador<-(length(da$Caso)-sum(da$Incumplidos))/length(da$Caso)
    da<-Data()
    da<-filter(da,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC")
    indicador<-sum((da$Cumplió.SLA)/sum(da$Cerrado.en.el..periodo))
    
    if(indicador<(MetaIC-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIC-dif) & indicador <= (MetaIC)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIC)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressICII <- renderValueBox({
    MetaIC<-0.9362
    dif<-0.0339
    Nam<- "Cumplimiento de Infraestructura"
    # da<-Data() %>% filter(AREA=="Infraestructura Informatica")
    # indicador<-(length(da$Caso)-sum(da$Incumplidos))/length(da$Caso)
    da<-Data()
    da<-filter(da,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC",AREA=="Infraestructura Informatica")
    indicador<-sum((da$Cumplió.SLA)/sum(da$Cerrado.en.el..periodo))
    
    if(indicador<(MetaIC-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIC-dif) & indicador <= (MetaIC)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIC)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressICSSII <- renderValueBox({
    MetaIC<-0.9506
    dif<-0.0378
    Nam<- "Cumplimiento de Sistemas de Información"
    da<-Data()
    da<-filter(da,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC",AREA=="Sistemas de Informacion")
    indicador<-sum((da$Cumplió.SLA)/sum(da$Cerrado.en.el..periodo))
    # da<-Data()
    # indicador<-(length(da$Caso)-sum(da$Incumplidos))/length(da$Caso)
    
    if(indicador<(MetaIC-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIC-dif) & indicador <= (MetaIC)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIC)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressICSTELCO <- renderValueBox({
    MetaIC<-0.9568
    dif<-0.0488
    Nam<- "Cumplimiento de Telecomunicaciones"
    # da<-Data() %>% filter(AREA=="Telecomunicaciones")
    # indicador<-(length(da$Caso)-sum(da$Incumplidos))/length(da$Caso)
    da<-Data()
    da<-filter(da,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC",AREA=="Telecomunicaciones")
    indicador<-sum((da$Cumplió.SLA)/sum(da$Cerrado.en.el..periodo))
    
    if(indicador<(MetaIC-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIC-dif) & indicador <= (MetaIC)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIC)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressICSGDS <- renderValueBox({
    MetaIC<-0.8835
    dif<-0.005
    Nam<- "Cumplimiento de Gestión del Servicio"
    # da<-Data() %>% filter(AREA=="Gestión del Servicio")
    # indicador<-(length(da$Caso)-sum(da$Incumplidos))/length(da$Caso)
    da<-Data()
    da<-filter(da,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC",AREA=="Gestión del Servicio")
    indicador<-sum((da$Cumplió.SLA)/sum(da$Cerrado.en.el..periodo))
    
    if(indicador<(MetaIC-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIC-dif) & indicador <= (MetaIC)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIC)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  ##### valueboxes de IVS
  
  output$progressIVSGT <- renderValueBox({
    Nam<- "Velocidad de Gerencia"
    da<-Data()
    indicador<-(sum(da$Tiempo.Total.Solución..minutos.)/sum(da$Tiempo.requerido.SLA..minutos.))
    if(indicador<0.4 | indicador>1){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (0.4) & indicador <= (0.5)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>0.5 & indicador<1){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIVSII <- renderValueBox({
    Nam<- "Velocidad de Infraestructura"
    da<-Data() %>% filter(AREA=="Infraestructura Informatica")
    indicador<-(sum(da$Tiempo.Total.Solución..minutos.)/sum(da$Tiempo.requerido.SLA..minutos.))
    if(indicador<0.4 | indicador>1){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (0.4) & indicador <= (0.5)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>0.5 & indicador<1){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIVSSSII <- renderValueBox({
    Nam<- "Velocidad de Sistemas de Informacion"
    da<-Data() %>% filter(AREA=="Sistemas de Informacion")
    indicador<-(sum(da$Tiempo.Total.Solución..minutos.)/sum(da$Tiempo.requerido.SLA..minutos.))
    if(indicador<0.4 | indicador>1){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (0.4) & indicador <= (0.5)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>0.5 & indicador<1){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIVSTELCO <- renderValueBox({
    Nam<- "Velocidad de Telecomunicaciones"
    da<-Data() %>% filter(AREA=="Telecomunicaciones")
    indicador<-(sum(da$Tiempo.Total.Solución..minutos.)/sum(da$Tiempo.requerido.SLA..minutos.))
    if(indicador<0.4 | indicador>1){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (0.4) & indicador <= (0.5)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>0.5 & indicador<1){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIVSGDS <- renderValueBox({
    Nam<- "Velocidad de Gestión del Servicio"
    da<-Data() %>% filter(AREA=="Gestión del Servicio")
    indicador<-(sum(da$Tiempo.Total.Solución..minutos.)/sum(da$Tiempo.requerido.SLA..minutos.))
    if(indicador<0.4 | indicador>1){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (0.4) & indicador <= (0.5)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>0.5 & indicador<1){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  ##### valueboxes de IES
  
  output$progressIESGT <- renderValueBox({
    MetaIES<-0.9590
    dif<-0.0125
    Nam<- "Eficacia de Gerencia"
    da<-Data()
      indicador<-((sum(da$Cerrado.en.el..periodo) + sum(da$Solucionado.pendiente.cierre))/(sum(da$Cerrado.x.anticipado.del.mes.siguiente)+sum(da$Se.vence.en.el.mes)+sum(da$Solucionado.anticipado.mes.siguiente..pendiente.cierre) + sum(da$Viene.vencido.de.meses.anteriores) - sum(da$Cerrado.x.anticipado.en.el.mes.anterior)))
    
    if(indicador<(MetaIES-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIES-dif) & indicador <= (MetaIES)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIES)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIESII <- renderValueBox({
    MetaIES<-0.9790
    dif<-0.0097
    Nam<- "Eficacia de Infraestructura"
    da<-Data() %>% filter(AREA=="Infraestructura Informatica")
       indicador<-((sum(da$Cerrado.en.el..periodo) + sum(da$Solucionado.pendiente.cierre))/(sum(da$Cerrado.x.anticipado.del.mes.siguiente)+sum(da$Se.vence.en.el.mes)+sum(da$Solucionado.anticipado.mes.siguiente..pendiente.cierre) + sum(da$Viene.vencido.de.meses.anteriores) - sum(da$Cerrado.x.anticipado.en.el.mes.anterior)))
    
    if(indicador<(MetaIES-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIES-dif) & indicador <= (MetaIES)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIES)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIESSSII <- renderValueBox({
    MetaIES<-0.9070
    dif<-0.0281
    Nam<- "Eficacia de Sistemas de Información"
    da<-Data() %>% filter(AREA=="Sistemas de Informacion")
    indicador<-((sum(da$Cerrado.en.el..periodo) + sum(da$Solucionado.pendiente.cierre))/(sum(da$Cerrado.x.anticipado.del.mes.siguiente)+sum(da$Se.vence.en.el.mes)+sum(da$Solucionado.anticipado.mes.siguiente..pendiente.cierre) + sum(da$Viene.vencido.de.meses.anteriores) - sum(da$Cerrado.x.anticipado.en.el.mes.anterior)))
    
    if(indicador<(MetaIES-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIES-dif) & indicador <= (MetaIES)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIES)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIESTELCO <- renderValueBox({
    MetaIES<-0.9495
    dif<-0.0364
    Nam<- "Eficacia de Telecomunicaciones"
    da<-Data() %>% filter(AREA=="Telecomunicaciones")
    indicador<-((sum(da$Cerrado.en.el..periodo) + sum(da$Solucionado.pendiente.cierre))/((sum(da$Cerrado.x.anticipado.del.mes.siguiente)+sum(da$Se.vence.en.el.mes)+sum(da$Solucionado.anticipado.mes.siguiente..pendiente.cierre) + sum(da$Viene.vencido.de.meses.anteriores)) - sum(da$Cerrado.x.anticipado.en.el.mes.anterior)))
   
    if(indicador<(MetaIES-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIES-dif) & indicador <= (MetaIES)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIES)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$progressIESGDS <- renderValueBox({
    MetaIES<-0.9160
    dif<-0.0882
    Nam<- "Eficacia de Gestión del Servicio"
    da<-Data() %>% filter(AREA=="Gestión del Servicio")
    indicador<-((sum(da$Cerrado.en.el..periodo) + sum(da$Solucionado.pendiente.cierre))/(sum(da$Cerrado.x.anticipado.del.mes.siguiente)+sum(da$Se.vence.en.el.mes)+sum(da$Solucionado.anticipado.mes.siguiente..pendiente.cierre) + sum(da$Viene.vencido.de.meses.anteriores) - sum(da$Cerrado.x.anticipado.en.el.mes.anterior)))
    
    if(indicador<(MetaIES-dif)){
      valueBox(
        paste0(round(indicador*100,2), "%"),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (MetaIES-dif) & indicador <= (MetaIES)){
        valueBox(
          paste0(round(indicador*100,2), "%"), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador>(MetaIES)){
          valueBox(
            paste0(round(indicador*100,2), "%"), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  ###### Casos Cerrados en Mes
  
  output$CerradosGT<-renderValueBox({
    all<-Prob() %>% filter(AREA=="GT")
    Maxim<-100
    dif<-5
    Nam<- "Casos Gestionados"
    indicador2<-as.numeric(all$CuotaMes)
    indicador<-as.numeric(all$TotalSol)
    
    if((indicador2+dif)>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador2 > (Maxim-dif) & indicador2 <(Maxim+dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador2<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$CerradosGDS<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Gestión del Servicio")
    Maxim<-100
    dif<-5
    Nam<- "Casos Gestionados"
    indicador2<-as.numeric(all$CuotaMes)
    indicador<-as.numeric(all$TotalSol)
    
    if((indicador2+dif)>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador2 > (Maxim-dif) & indicador2 <(Maxim+dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador2<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$CerradosII<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Infraestructura Informatica")
    Maxim<-100
    dif<-5
    Nam<- "Casos Gestionados"
    indicador2<-as.numeric(all$CuotaMes)
    indicador<-as.numeric(all$TotalSol)
    
    if((indicador2+dif)>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador2 > (Maxim-dif) & indicador2 <(Maxim+dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador2<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$CerradosSSII<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Sistemas de Informacion")
    Maxim<-100
    dif<-5
    Nam<- "Casos Gestionados"
    indicador2<-as.numeric(all$CuotaMes)
    indicador<-as.numeric(all$TotalSol)
    
    if((indicador2+dif)>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador2 > (Maxim-dif) & indicador2 <(Maxim+dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador2<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  output$CerradosTELCO<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Telecomunicaciones")
    Maxim<-100
    dif<-5
    Nam<- "Casos Gestionados"
    indicador2<-as.numeric(all$CuotaMes)
    indicador<-as.numeric(all$TotalSol)
    
    if((indicador2+dif)>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador2 > (Maxim-dif) & indicador2 <(Maxim+dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador2<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("check"),
            color = "green"
          )
        }}}
  })
  
  ###### Casos Incumplidos en el Mes
  
  output$IncumplidosGT<-renderValueBox({
    all<- all2<-Data() 
    indicador2<-round(length(all2$Incumplidos)*0.05,0)
    all <- all %>% filter(Incumplidos==1)
    indicador<-sum(all$Incumplidos)
    base<-length(all2$Incumplidos)
    porc<-round((indicador/base)*100,2)
    Nam<- paste("Casos Incumplidos: ",porc,"%",sep="")
    if(indicador>=indicador2){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("thumbs-down"),
        color = "red"
      )
    }else{
      if(indicador < indicador2){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("check"),
          color = "green"
        )
      }}
  })
  
  output$IncumplidosGDS<-renderValueBox({
    all<- all2<-Data() %>% filter(AREA=="Gestión del Servicio")
    indicador2<-round(length(all2$Incumplidos)*0.05,0)
    all <- all %>% filter(Incumplidos==1)
    indicador<-sum(all$Incumplidos)
    base<-length(all2$Incumplidos)
    porc<-round((indicador/base)*100,2)
    Nam<- paste("Casos Incumplidos: ",porc,"%",sep="")
    
    if(indicador>=indicador2){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("thumbs-down"),
        color = "red"
      )
    }else{
      if(indicador < indicador2){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heart"),
          color = "green"
        )
      }}
  })
  
  output$IncumplidosII<-renderValueBox({
    all<- all2<-Data() %>% filter(AREA=="Infraestructura Informatica")
    indicador2<-round(length(all2$Incumplidos)*0.05,0)
    all <- all %>% filter(Incumplidos==1)
    indicador<-sum(all$Incumplidos)
    base<-length(all2$Incumplidos)
    porc<-round((indicador/base)*100,2)
    Nam<- paste("Casos Incumplidos: ",porc,"%",sep="")
    
    if(indicador>=indicador2){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("thumbs-down"),
        color = "red"
      )
    }else{
      if(indicador<indicador2){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heart"),
          color = "green"
        )
      }}
  })
  
  output$IncumplidosSSII<-renderValueBox({
    all<- all2<-Data() %>% filter(AREA=="Sistemas de Informacion")
    indicador2<-round(length(all2$Incumplidos)*0.05,0)
    all <- all %>% filter(Incumplidos==1)
    indicador<-sum(all$Incumplidos)
    base<-length(all2$Incumplidos)
    porc<-round((indicador/base)*100,2)
    Nam<- paste("Casos Incumplidos: ",porc,"%",sep="")
    
    if(indicador>=indicador2){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("thumbs-down"),
        color = "red"
      )
    }else{
      if(indicador < indicador2){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heart"),
          color = "green"
        )
      }}
  })
  
  output$IncumplidosTELCO<-renderValueBox({
    all<- all2<-Data() %>% filter(AREA=="Telecomunicaciones")
    indicador2<-round(length(all2$Incumplidos)*0.05,0)
    all <- all %>% filter(Incumplidos==1)
    indicador<-sum(all$Incumplidos)
    base<-length(all2$Incumplidos)
    porc<-round((indicador/base)*100,2)
    Nam<- paste("Casos Incumplidos: ",porc,"%",sep="")
    
    if(indicador>=indicador2){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("thumbs-down"),
        color = "red"
      )
    }else{
      if(indicador < indicador2){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heart"),
          color = "green"
        )
      }}
  })
  
  ###### Casos Activos
  
  output$ActivosGT<-renderValueBox({
    all<-Prob() %>% filter(AREA=="GT")
    Maxim<-as.numeric(all$P0)
    dif<-(as.numeric(all$P0)*0.1)
    Nam<- "Casos a Cerrar en el Mes"
    
    indicador<-as.numeric(all$Vencen_en_Mes)
    
    if(indicador>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (Maxim-dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("eye"),
            color = "green"
          )
        }}}
  })
  
  output$ActivosGDS<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Gestión del Servicio")
    Maxim<-as.numeric(all$P0)
    dif<-(as.numeric(all$P0)*0.1)
    Nam<- "Casos a Cerrar en el Mes"
    
    indicador<-as.numeric(all$Vencen_en_Mes)
    
    if(indicador>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (Maxim-dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("eye"),
            color = "green"
          )
        }}}
  })
  
  output$ActivosII<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Infraestructura Informatica")
    Maxim<-as.numeric(all$P0)
    dif<-(as.numeric(all$P0)*0.1)
    Nam<- "Casos a Cerrar en el Mes"
    
    indicador<-as.numeric(all$Vencen_en_Mes)
    
    if(indicador>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (Maxim-dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("eye"),
            color = "green"
          )
        }}}
  })
  
  output$ActivosSSII<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Sistemas de Informacion")
    Maxim<-as.numeric(all$P0)
    dif<-(as.numeric(all$P0)*0.1)
    Nam<- "Casos a Cerrar en el Mes"
    
    indicador<-as.numeric(all$Vencen_en_Mes)
    
    if(indicador>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (Maxim-dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("eye"),
            color = "green"
          )
        }}}
  })
  
  output$ActivosTELCO<-renderValueBox({
    all<-Prob() %>% filter(AREA=="Telecomunicaciones")
    Maxim<-as.numeric(all$P0)
    dif<-(as.numeric(all$P0)*0.1)
    Nam<- "Casos a Cerrar en el Mes"
    
    indicador<-as.numeric(all$Vencen_en_Mes)
    
    if(indicador>(Maxim)){
      valueBox(
        paste0(round(indicador,2), ""),Nam, icon = icon("warning"),
        color = "red"
      )
    }else{
      if(indicador > (Maxim-dif)){
        valueBox(
          paste0(round(indicador,2), ""), Nam, icon = icon("heartbeat"),
          color = "orange"
        )
      }else{
        if(indicador<(Maxim-dif)){
          valueBox(
            paste0(round(indicador,2), ""), Nam, icon = icon("eye"),
            color = "green"
          )
        }}}
  })
  
  #### graph
  
  output$view <- renderGvis({
    if(input$B2=="Todos"){
    suppressWarnings(w<-Data() %>% filter(Tipo.de.caso=="Incidente") %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
    w$GRUPO[w$GRUPO==""]<-"NoGrupo"
    w$GRUPO<-chartr('áéíóúñ?','aeioun.',w$GRUPO)
    w$AREA<-chartr('áéíóúñ?','aeioun.',w$AREA)
    w$A<-chartr('áéíóúñ?','aeioun.',w$A)
    w$B<-chartr('áéíóúñ?','aeioun.',w$B)
    w$D<-chartr('áéíóúñ?','aeioun.',w$D)
    w$E<-chartr('áéíóúñ?','aeioun.',w$E)
    p1<-w %>% group_by(AREA,GRUPO) %>% summarise(n())
    p2<-w %>% group_by(GRUPO,A) %>% summarise(n());p2<-p2[complete.cases(p2),]
    p3<-w %>% group_by(A,B) %>% summarise(n());p3<-p3[complete.cases(p3),]
    p4<-w %>% group_by(B,D) %>% summarise(n());p4<-p4[complete.cases(p4),]
    p5<-w %>% group_by(D,E) %>% summarise(n());p5<-p5[complete.cases(p5),]
    names(p1)<-names(p2)<-names(p3)<-names(p4)<-names(p5)<-c("A","B","C")
    p1<-data.frame(p1);p2<-data.frame(p2);p3<-data.frame(p3)
    p4<-data.frame(p4);p5<-data.frame(p5)
    flujo<-data.frame(rbind(p1,p2,p3,p4,p5))
    
    workingdata=flujo
    colnames(workingdata)=c('source','target','value')
    
    ####################
    
    
      graphw<-gvisSankey(workingdata, from="source", 
                 to="target", weight="value",
                 options=list(
                   height=1700,width=1100,
                   sankey="{link:{color:{fill:'lightblue'}}}"
                 ))
      return(graphw)}else{return(NULL)}

  })
  
  output$view2 <- renderGvis({
    if(input$B2=="Todos"){
    suppressWarnings(w<-Data() %>% filter(Tipo.de.caso=="Requerimiento") %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
    w$GRUPO[w$GRUPO==""]<-"NoGrupo"
    w$GRUPO<-chartr('áéíóúñ?','aeioun.',w$GRUPO)
    w$AREA<-chartr('áéíóúñ?','aeioun.',w$AREA)
    w$A<-chartr('áéíóúñ?','aeioun.',w$A)
    w$B<-chartr('áéíóúñ?','aeioun.',w$B)
    w$D<-chartr('áéíóúñ?','aeioun.',w$D)
    w$E<-chartr('áéíóúñ?','aeioun.',w$E)
    p1<-w %>% group_by(AREA,GRUPO) %>% summarise(n())
    p2<-w %>% group_by(GRUPO,A) %>% summarise(n());p2<-p2[complete.cases(p2),]
    p3<-w %>% group_by(A,B) %>% summarise(n());p3<-p3[complete.cases(p3),]
    p4<-w %>% group_by(B,D) %>% summarise(n());p4<-p4[complete.cases(p4),]
    p5<-w %>% group_by(D,E) %>% summarise(n());p5<-p5[complete.cases(p5),]
    names(p1)<-names(p2)<-names(p3)<-names(p4)<-names(p5)<-c("A","B","C")
    p1<-data.frame(p1);p2<-data.frame(p2);p3<-data.frame(p3)
    p4<-data.frame(p4);p5<-data.frame(p5)
    
    # p1[,2]<-paste(p1[,2],"1")
    # p2[,1]<-paste(p2[,1],"1")
    # p2[,2]<-paste(p2[,2],"")
    # p3[,1]<-paste(p3[,1],"")
    p3[,2]<-paste(p3[,2],"")
    p4[,1]<-paste(p4[,1],"")
    # p4[,2]<-paste(p4[,2],"4")
    # p5[,1]<-paste(p5[,1],"4")
    
    flujo<-data.frame(rbind(p1,p2,p3,p4,p5))
    workingdata=flujo
    colnames(workingdata)=c('source','target','value')
    
    ####################
    
    
    graphw2<-gvisSankey(workingdata, from="source", 
                       to="target", weight="value",
                       options=list(
                         height=1700,width=1100,
                         sankey="{link:{color:{fill:'lightblue'}}}"
                       ))
    return(graphw2) }else{return(NULL)}
    
  })
  
  output$view3 <- renderGvis({
    if(input$B2=="Todos"){
    suppressWarnings(w<-Data() %>% filter(Incumplidos==1,Tipo.de.caso=="Incidente") %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
    w$GRUPO[w$GRUPO==""]<-"NoGrupo"
    w$GRUPO<-chartr('áéíóúñ?','aeioun.',w$GRUPO)
    w$AREA<-chartr('áéíóúñ?','aeioun.',w$AREA)
    w$A<-chartr('áéíóúñ?','aeioun.',w$A)
    w$B<-chartr('áéíóúñ?','aeioun.',w$B)
    w$D<-chartr('áéíóúñ?','aeioun.',w$D)
    w$E<-chartr('áéíóúñ?','aeioun.',w$E)
    p1<-w %>% group_by(AREA,GRUPO) %>% summarise(n())
    p2<-w %>% group_by(GRUPO,A) %>% summarise(n());p2<-p2[complete.cases(p2),]
    p3<-w %>% group_by(A,B) %>% summarise(n());p3<-p3[complete.cases(p3),]
    p4<-w %>% group_by(B,D) %>% summarise(n());p4<-p4[complete.cases(p4),]
    p5<-w %>% group_by(D,E) %>% summarise(n());p5<-p5[complete.cases(p5),]
    names(p1)<-names(p2)<-names(p3)<-names(p4)<-names(p5)<-c("A","B","C")
    

    
    p1<-data.frame(p1);p2<-data.frame(p2);p3<-data.frame(p3)
    p4<-data.frame(p4);p5<-data.frame(p5)
    
    p3[,2]<-paste(p3[,2],"")
    p4[,1]<-paste(p4[,1],"")
    
    flujo<-data.frame(rbind(p1,p2,p3,p4,p5))
    
    workingdata=flujo
    colnames(workingdata)=c('source','target','value')
    
    ####################
    
    
    graphq<-gvisSankey(workingdata, from="source", 
                       to="target", weight="value",
                       options=list(
                         height=1200,width=1100,
                         sankey="{link:{color:{fill:'lightblue'}}}"
                       ))
    return(graphq) }else{return(NULL)}
    
  })
  
  output$view4 <- renderGvis({
    if(input$B2=="Todos"){
    suppressWarnings(w<-Data() %>% filter(Incumplidos==1,Tipo.de.caso=="Requerimiento") %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
    w$GRUPO[w$GRUPO==""]<-"NoGrupo"
    w$GRUPO<-chartr('áéíóúñ?','aeioun.',w$GRUPO)
    w$AREA<-chartr('áéíóúñ?','aeioun.',w$AREA)
    w$A<-chartr('áéíóúñ?','aeioun.',w$A)
    w$B<-chartr('áéíóúñ?','aeioun.',w$B)
    w$D<-chartr('áéíóúñ?','aeioun.',w$D)
    w$E<-chartr('áéíóúñ?','aeioun.',w$E)
    p1<-w %>% group_by(AREA,GRUPO) %>% summarise(n())
    p2<-w %>% group_by(GRUPO,A) %>% summarise(n());p2<-p2[complete.cases(p2),]
    p3<-w %>% group_by(A,B) %>% summarise(n());p3<-p3[complete.cases(p3),]
    p4<-w %>% group_by(B,D) %>% summarise(n());p4<-p4[complete.cases(p4),]
    p5<-w %>% group_by(D,E) %>% summarise(n());p5<-p5[complete.cases(p5),]
    names(p1)<-names(p2)<-names(p3)<-names(p4)<-names(p5)<-c("A","B","C")
    
    
    
    p1<-data.frame(p1);p2<-data.frame(p2);p3<-data.frame(p3)
    p4<-data.frame(p4);p5<-data.frame(p5)
    
    p3[,2]<-paste(p3[,2],"")
    p4[,1]<-paste(p4[,1],"")
    
    flujo<-data.frame(rbind(p1,p2,p3,p4,p5))
    
    workingdata=flujo
    colnames(workingdata)=c('source','target','value')
    
    ####################
    
    
    graphq2<-gvisSankey(workingdata, from="source", 
                       to="target", weight="value",
                       options=list(
                         height=1200,width=1100,
                         sankey="{link:{color:{fill:'lightblue'}}}"
                       ))
    return(graphq2)}else{return(NULL)}
    
  })
  
  output$view5 <- renderGvis({
    if(input$B2=="Todos"){
      suppressWarnings(w<-Data() %>% filter(!Estado%in% c("Cerrado","Solucionado")) %>% separate(Jerarquía.de.Categorías,c("A","B","D","E"),sep="[.]"))
      w$CI<-chartr('áéíóúñ?','aeioun.',w$CI)
      w$AREA<-chartr('áéíóúñ?','aeioun.',w$AREA)
      w$Especialista<-chartr('áéíóúñ?','aeioun.',w$Especialista)
      w$Estado<-chartr('áéíóúñ?','aeioun.',w$Estado)
      q1<-w %>% group_by(AREA,Especialista) %>% summarise(n());q1<-q1[complete.cases(q1),]
      q2<-w %>% group_by(Especialista,Estado) %>% summarise(n());q2<-q2[complete.cases(q2),]
      q3<-w %>% group_by(CI,Estado) %>% summarise(n())
      names(q3)<-names(q2)<-names(q1)<-c("A","B","C")
      q3<-data.frame(q3);q2<-data.frame(q2);q1<-data.frame(q1)
      q3[,1][q3[,1]==""]<-"No_CI"
      q3[,2][q3[,2]==""]<-"NA1c"
      q2[,1][q2[,1]==""]<-"NA1a"
      q2[,2][q2[,2]==""]<-"No_CI"
      flujo<-data.frame(rbind(q1,q2))
      # ,q3
      
      workingdata=flujo
      colnames(workingdata)=c('source','target','value')

      ####################
      
      
      graphq2<-gvisSankey(workingdata, from="source", 
                          to="target", weight="value",
                          options=list(
                            height=1200,width=1000,
                            sankey="{link:{color:{fill:'lightblue'}}}"
                          ))
      return(graphq2)}else{return(NULL)}
    
  })
  
  #########JMG
  
  output$viewJMG <- renderGvis({
    Pcliente <- Pcliente()
    
    if(input$AREAP=="Todas"){
      Pcliente<-Pcliente
    }
    if(input$AREAP=="II"){
      Pcliente<-Pcliente %>% filter(Area=="Infraestructura Informatica")  
    }
    if(input$AREAP=="SSII"){
      Pcliente<-Pcliente %>% filter(Area=="Sistemas de Informacion")  
    }
    if(input$AREAP=="TELCO"){
      Pcliente<-Pcliente %>% filter(Area=="Telecomunicaciones")  
    }
    if(input$AREAP=="GDS"){
      Pcliente<-Pcliente %>% filter(Area=="Gestión del Servicio")  
    } 
    
    Pcliente$categoria<-chartr('áéíóúñ?','aeioun.',Pcliente$categoria)
    Pcliente$categoria<-paste(Pcliente$categoria,".",sep="")
    w<-Pcliente
    w$Area<-chartr('áéíóúñ?','aeioun.',w$Area)
    w$especialista_resp<-chartr('áéíóúñ?','aeioun.',w$especialista_resp)
    w$servicio<-chartr('áéíóúñ?','aeioun.',w$servicio)
    p1<-w %>% group_by(Area,servicio) %>% summarise(n())
    p2<-w %>% group_by(servicio,categoria) %>% summarise(n());p2<-p2[complete.cases(p2),]
    p3<-w %>% group_by(categoria,especialista_resp) %>% summarise(n());p3<-p3[complete.cases(p3),]
    names(p1)<-names(p2)<-names(p3)<-c("A","B","C")
    p1<-data.frame(p1);p2<-data.frame(p2);p3<-data.frame(p3)
    flujo<-data.frame(rbind(p1,p2,p3))
    workingdata=flujo
    colnames(workingdata)=c('source','target','value')
    
    ####################
    
    
    graphq9<-gvisSankey(workingdata, from="source", 
                        to="target", weight="value",
                        options=list(
                          height=800,width=1100,
                          sankey="{link:{color:{fill:'LightSkyBlue'}}}"
                        ))
      return(graphq9)
  })
  
  
  #####TS
  
  output$tsGT<-renderDygraph({
    if(input$B2=="Todos"){
    Dat<-Data()
    Dat<-Dat[!duplicated(Dat$Caso),]
    Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
    Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
    Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,Requerimiento,na.rm=T)))
    Dat <- cbind(xts(Dat$Incidente, Dat$Fechas),Dat$Requerimiento,Dat$Total)
    colnames(Dat)<-c("Incidentes","Requerimientos","Total")
    dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
      dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
      dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
      dyAxis("y", label = "# Casos Registrados") %>% 
      dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
        if(input$B2=="Incidentes"){
          Dat<-Data()
          Dat<-Dat[!duplicated(Dat$Caso),]
          Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
          Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
          Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,na.rm=T)))
          Dat <- cbind(xts(Dat$Incidente, Dat$Fechas))
          colnames(Dat)<-c("Incidentes")
          dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
            dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
            dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
            dyAxis("y", label = "# Casos Registrados") %>% 
            dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
              Dat<-Data()
              Dat<-Dat[!duplicated(Dat$Caso),]
              Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
              Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
              Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Requerimiento,na.rm=T)))
              Dat <- cbind(xts(Dat$Requerimiento, Dat$Fechas))
              colnames(Dat)<-c("Requerimientos")
              dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
                dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
                dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
                dyAxis("y", label = "# Casos Registrados") %>% 
                dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}
      }
  })
  
  output$tsII<-renderDygraph({
    Dat<-Data() %>% filter(AREA=="Infraestructura Informatica")
    if(input$B2=="Todos"){
    Dat<-Dat[!duplicated(Dat$Caso),]
    Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
    Dat<-Dat %>% group_by(Fechas,Tipo.de.caso)%>% summarise(Total=n()) %>% select(Fechas,Total,Tipo.de.caso)
    Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,Requerimiento,na.rm=T)))
    Dat <- cbind(xts(Dat$Incidente, Dat$Fechas),Dat$Requerimiento,Dat$Total)
    colnames(Dat)<-c("Incidentes","Requerimientos","Total")
    dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
      dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
      dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
      dyAxis("y", label = "# Casos Registrados") %>% 
      dyRoller(rollPeriod = 1)}else
        if(input$B2=="Incidentes"){
          Dat<-Dat[!duplicated(Dat$Caso),]
          Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
          Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
          Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,na.rm=T)))
          Dat <- cbind(xts(Dat$Incidente, Dat$Fechas))
          colnames(Dat)<-c("Incidentes")
          dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
            dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
            dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
            dyAxis("y", label = "# Casos Registrados") %>% 
            dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
              Dat<-Dat[!duplicated(Dat$Caso),]
              Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
              Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
              Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Requerimiento,na.rm=T)))
              Dat <- cbind(xts(Dat$Requerimiento, Dat$Fechas))
              colnames(Dat)<-c("Requerimientos")
              dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
                dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
                dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
                dyAxis("y", label = "# Casos Registrados") %>% 
                dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}
    })
  
  output$tsSSII<-renderDygraph({
    Dat<-Data() %>% filter(AREA=="Sistemas de Informacion")
    if(input$B2=="Todos"){
      Dat<-Dat[!duplicated(Dat$Caso),]
      Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
      Dat<-Dat %>% group_by(Fechas,Tipo.de.caso)%>% summarise(Total=n()) %>% select(Fechas,Total,Tipo.de.caso)
      Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,Requerimiento,na.rm=T)))
      Dat <- cbind(xts(Dat$Incidente, Dat$Fechas),Dat$Requerimiento,Dat$Total)
      colnames(Dat)<-c("Incidentes","Requerimientos","Total")
      dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
        dyAxis("y", label = "# Casos Registrados") %>% 
        dyRoller(rollPeriod = 1)}else
          if(input$B2=="Incidentes"){
            Dat<-Dat[!duplicated(Dat$Caso),]
            Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
            Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
            Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,na.rm=T)))
            Dat <- cbind(xts(Dat$Incidente, Dat$Fechas))
            colnames(Dat)<-c("Incidentes")
            dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
              dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
              dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
              dyAxis("y", label = "# Casos Registrados") %>% 
              dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
                Dat<-Dat[!duplicated(Dat$Caso),]
                Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
                Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
                Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Requerimiento,na.rm=T)))
                Dat <- cbind(xts(Dat$Requerimiento, Dat$Fechas))
                colnames(Dat)<-c("Requerimientos")
                dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
                  dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
                  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
                  dyAxis("y", label = "# Casos Registrados") %>% 
                  dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}
    })
  
  output$tsTELCO<-renderDygraph({
    Dat<-Data() %>% filter(AREA=="Telecomunicaciones")
    if(input$B2=="Todos"){
      Dat<-Dat[!duplicated(Dat$Caso),]
      Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
      Dat<-Dat %>% group_by(Fechas,Tipo.de.caso)%>% summarise(Total=n()) %>% select(Fechas,Total,Tipo.de.caso)
      Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,Requerimiento,na.rm=T)))
      Dat <- cbind(xts(Dat$Incidente, Dat$Fechas),Dat$Requerimiento,Dat$Total)
      colnames(Dat)<-c("Incidentes","Requerimientos","Total")
      dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
        dyAxis("y", label = "# Casos Registrados") %>% 
        dyRoller(rollPeriod = 1)}else
          if(input$B2=="Incidentes"){
            Dat<-Dat[!duplicated(Dat$Caso),]
            Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
            Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
            Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,na.rm=T)))
            Dat <- cbind(xts(Dat$Incidente, Dat$Fechas))
            colnames(Dat)<-c("Incidentes")
            dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
              dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
              dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
              dyAxis("y", label = "# Casos Registrados") %>% 
              dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
                Dat<-Dat[!duplicated(Dat$Caso),]
                Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
                Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
                Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Requerimiento,na.rm=T)))
                Dat <- cbind(xts(Dat$Requerimiento, Dat$Fechas))
                colnames(Dat)<-c("Requerimientos")
                dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
                  dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
                  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
                  dyAxis("y", label = "# Casos Registrados") %>% 
                  dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}
    
    })
  
  output$tsGDS<-renderDygraph({
    Dat<-Data() %>% filter(AREA=="Gestión del Servicio")
    
    if(input$B2=="Todos"){
      Dat<-Dat[!duplicated(Dat$Caso),]
      Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
      Dat<-Dat %>% group_by(Fechas,Tipo.de.caso)%>% summarise(Total=n()) %>% select(Fechas,Total,Tipo.de.caso)
      Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,Requerimiento,na.rm=T)))
      Dat <- cbind(xts(Dat$Incidente, Dat$Fechas),Dat$Requerimiento,Dat$Total)
      colnames(Dat)<-c("Incidentes","Requerimientos","Total")
      dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
        dyAxis("y", label = "# Casos Registrados") %>% 
        dyRoller(rollPeriod = 1)}else
          if(input$B2=="Incidentes"){
            Dat<-Dat[!duplicated(Dat$Caso),]
            Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
            Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
            Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Incidente,na.rm=T)))
            Dat <- cbind(xts(Dat$Incidente, Dat$Fechas))
            colnames(Dat)<-c("Incidentes")
            dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
              dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
              dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
              dyAxis("y", label = "# Casos Registrados") %>% 
              dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}else{
                Dat<-Dat[!duplicated(Dat$Caso),]
                Dat$Fechas<-as.POSIXct(as.character(as.POSIXct(Dat$Fecha.de.registro,format = "%d/%m/%Y %H")),format = "%Y-%m-%d %H:%M:%S")
                Dat<-Dat %>% group_by(Fechas,Tipo.de.caso) %>% summarise(Total=n())%>% select(Fechas,Total,Tipo.de.caso)
                Dat<-spread(Dat,Tipo.de.caso,Total) %>% mutate(Total=(sum(Requerimiento,na.rm=T)))
                Dat <- cbind(xts(Dat$Requerimiento, Dat$Fechas))
                colnames(Dat)<-c("Requerimientos")
                dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
                  dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
                  dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
                  dyAxis("y", label = "# Casos Registrados") %>% 
                  dyRoller(rollPeriod = 1)%>% dyLegend(width = 600)}
    
    })
  
  output$trendGT<-renderDygraph({
    # if(input$B2=="Todos"){
      ###
      q<-Tiemp()
      a<-q[[1]]
      b<-q[[2]]
      d<-q[[3]]
      e<-q[[4]]
      w<-q[[5]]
      razon<-q[[6]]
      Serie<-q[[7]]
      ###
 
      Dat <- cbind(xts(a, Serie),b,d,e)
      colnames(Dat)<-c("#Casos a Gestionar","#Casos Solucionados","#Casos Con Vencimiento Original","# Casos Abiertos")
      dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
        dyAxis("y", label = "# Casos") %>% 
        dyRoller(rollPeriod = 1) %>% dyLegend(width = 900)
      # }else{return(NULL)
          # }
  })
  
  output$trend2GT<-renderDygraph({
    # if(input$B2=="Todos"){
      ###
      q<-Tiemp()
      a<-q[[1]]
      b<-q[[2]]
      d<-q[[3]]
      e<-q[[4]]
      w<-q[[5]]
      razon<-q[[6]]
      Serie<-q[[7]]
      ###
        Dat <- cbind(xts(w, Serie),razon)
      colnames(Dat)<-c("% Casos LAG","% Casos Gestionados del Total")
      dygraph(Dat,group = "g1") %>% dyRangeSelector(dateWindow = c(paste(format(Sys.time(), "%Y"),"-",format(Sys.time(), "%m"),"-01",sep=""), paste(format(Sys.time()+60*24*60*30, "%Y"),"-",format(Sys.time()+60*24*60*30, "%m"),"-01",sep=""))) %>% 
        dyOptions(fillGraph = TRUE, fillAlpha = 0.2)%>%  
        dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2,hideOnMouseOut = FALSE)%>%
        dyAxis("y", label = "% Casos") %>% 
        dyRoller(rollPeriod = 1) %>% dyLegend(width = 900)
      # }else{return(NULL)}
  })

  outputOptions(output, "view", suspendWhenHidden = FALSE)
  
  outputOptions(output, "view2", suspendWhenHidden = FALSE)
  
  outputOptions(output, "view3", suspendWhenHidden = FALSE)
  
  output$messageMenu <- renderMenu({
    top<-Data() %>% group_by(Especialista) %>% summarise(Incumplidos=sum(Incumplidos))%>% arrange(desc(Incumplidos))
    top<-data.frame(top[1:20,])
    w<-unique(data.frame(Especialista=as.character(Data()$Especialista),AREA=as.character(Data()$AREA)))
    w$Especialista<-as.character(w$Especialista)
    w$AREA<-as.character(w$AREA)
    top2<-inner_join(top,w,by="Especialista")
    top<-data.frame(top2$Especialista,paste(top$Incumplidos," Casos Vencidos - ",top2$AREA ,sep=""))
    names(top)<-c("from","message")
    # top$message<-paste("Se le vencieron ",top$message," Casos",sep="")
    messageData<-top
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    dropdownMenu(type = "notifications", .list = msgs)
  })
  
  output$messageMenu2 <- renderMenu({
    top<-data.frame(Data() %>% filter(Tipo.de.Servicio=="Servicios Usuario Final",Tipo.de.caso=="Incidente") %>% group_by(Sede.Cliente,Categoria) %>% summarise(total=n()))
    top<-top %>% arrange(desc(total))
    top<-data.frame(top[1:20,])
    top<-data.frame(top$Sede.Cliente,paste(top$total," : Incidentes : ",top$Categoria,sep=""))
    names(top)<-c("from","message")
    messageData<-top
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]],icon = shiny::icon("map-marker"))
    })
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  #exploradata
  
  output$scatter1 <- renderPlot({
    Data<-Data() %>% filter(!Estado %in% c("Cerrado","Solucionado"))
    Data$Tipo.de.caso<-as.factor(Data$Tipo.de.caso)
    Data$Progreso.Individual<-as.numeric(sub(",",".",Data$Progreso.Individual))
    Data$Detalle<-paste(Data$GRUPO," - ",Data$Servicio)
    ggplot(Data, aes(x=Progreso.Individual,y=Tiempo.Total.Solución..minutos.,fill=Tipo.de.caso,colour=Tipo.de.caso))+ scale_x_log10() + scale_y_log10() + 
      xlab("Progreso") +
      ylab("Tiempo Total de Casos") + facet_grid(.~ AREA) +geom_point(alpha=0.2) +
      theme_bw(base_size = 16)+
      theme(legend.position="bottom")+ geom_vline(xintercept = 100,col="green")
  })
  
  output$scatter2 <- renderPlot({
    Data<-Data() %>% filter(!Estado %in% c("Cerrado","Solucionado"))
    Data$Tipo.de.caso<-as.factor(Data$Tipo.de.caso)
    Data$Progreso.Individual<-as.numeric(sub(",",".",Data$Progreso.Individual))
    Data$Detalle<-paste(Data$GRUPO," - ",Data$Servicio)
    brushed <- brushedPoints(Data, input$brush)
    if(length(input$brush)>0){
      if(length(brushed$AREA)>0){
          ggplot(brushed, aes(Progreso.Individual,Detalle)) + facet_grid(AREA~ .) +
          geom_point(size = 3, shape = 21, fill = "white", colour = "black") +
          geom_point(data = brushed, colour = "#4488ee", size = 3) +
          theme_bw(base_size = 16) + geom_vline(xintercept = 100,col="green")
      }
    }else{
      return(NULL)
    }
    
  })
  
  output$bruselect<- renderDataTable({
    Data<-Data() %>% filter(!Estado %in% c("Cerrado","Solucionado"))
    Data$Tipo.de.caso<-as.factor(Data$Tipo.de.caso)
    Data$Progreso.Individual<-as.numeric(sub(",",".",Data$Progreso.Individual))
    Data$Detalle<-paste(Data$GRUPO," - ",Data$Servicio)
    # Data$Trestante<-(Data$Tiempo.requerido.SLA..minutos. - Data$Tiempo.Total.Solución..minutos.)-35
    res <- brushedPoints(Data, input$brush)
    if (nrow(res) == 0)
      return()
    select(res,c(2,4:7,36,38)) # %>% mutate(Trestante=())
  },rownames=FALSE, extensions = 'Buttons', options = list(pageLength = 1000,
                                                           dom = 'Bfrtip',
                                                           buttons = c('copy', 'csv', 'excel', 'print')
  ))
  
  ###HeatM Especialistas
  
  output$HeatE <- renderPlot({
    if(input$AREA=="Todas"){
      Dat<-Data() %>% filter(!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="II"){
      Dat<-Data() %>% filter(AREA=="Infraestructura Informatica",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="SSII"){
      Dat<-Data() %>% filter(AREA=="Sistemas de Informacion",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="TELCO"){
      Dat<-Data() %>% filter(AREA=="Telecomunicaciones",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="GDS"){
      Dat<-Data() %>% filter(AREA=="Gestión del Servicio",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }    
    
    Dat<-Dat %>% group_by(Estado,Especialista) %>% summarise(total=n())
    
    p <- ggplot(Dat, aes(Estado, Especialista)) +
      geom_tile(aes(fill = total),colour = "white") + 
      scale_fill_gradient(low = "green",high = "red")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
      geom_text(aes(Estado, Especialista, label = paste(total,"Casos")), color = "black", size = 3)
    
    p
  })
  
  output$contents <- renderDataTable({
    
    if(input$AreaPS=="GT"){
      obs<-reactive({
        Pr_CI_GT<-Data() %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(Incumplidos/Casos),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo)),IES=((sum(Cerrado.en.el.mes.cumpliendo.SLA)+sum(Solucionado.pendiente.cierre.y.cumple.SLA)+sum(Sigue.pendiente.en.el.mes.y.cumple.SLA))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))),IES2=((sum(Cerrado.en.el..periodo)+sum(Solucionado.pendiente.cierre)+sum(Sigue.pendiente.en.el.mes))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))))
        Pr_CI_GT$ICS<-((Pr_CI_GT$Casos-Pr_CI_GT$Incumplidos)/Pr_CI_GT$Casos)
        Pr_CI_GT<-select(Pr_CI_GT,Incumplidos,Casos,Prop_Casos,ICS,ICS_Est,IES,IES2) 
        names(Pr_CI_GT)<-c("Incumplidos","Casos","Pro_Casos","ICS","ICS_Est","IES_Est","IES2")
        Pr_CI_GT
      })
    }
    
    if(input$AreaPS=="Pr_CI_A"){
      obs<-reactive({
        Pr_CI_A<-Data() %>%
          group_by(AREA) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo)),IES=((sum(Cerrado.en.el.mes.cumpliendo.SLA)+sum(Solucionado.pendiente.cierre.y.cumple.SLA)+sum(Sigue.pendiente.en.el.mes.y.cumple.SLA))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))),IES2=((sum(Cerrado.en.el..periodo)+sum(Solucionado.pendiente.cierre)+sum(Sigue.pendiente.en.el.mes))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))))
        Pr_CI_A$Prop_Gerencia<-(Pr_CI_A$Incumplidos/sum(Pr_CI_A$Incumplidos))
        Pr_CI_A$ICS<-((Pr_CI_A$Casos-Pr_CI_A$Incumplidos)/Pr_CI_A$Casos)
        Pr_CI_A<-select(Pr_CI_A,AREA,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est,IES,IES2) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_A)<-c("AREA","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est","IES_Est","IES2")
        Pr_CI_A
      })
    }
    ##Proporcion de Casos Incumplidos X Area y Grupo
    if(input$AreaPS=="Pr_CI_AG"){
      obs<-reactive({
        Pr_CI_AG<-Data() %>%
          group_by(AREA,GRUPO) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo)),IES=((sum(Cerrado.en.el.mes.cumpliendo.SLA)+sum(Solucionado.pendiente.cierre.y.cumple.SLA)+sum(Sigue.pendiente.en.el.mes.y.cumple.SLA))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))),IES2=((sum(Cerrado.en.el..periodo)+sum(Solucionado.pendiente.cierre)+sum(Sigue.pendiente.en.el.mes))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))))
        Pr_CI_AG$Prop_Gerencia<-(Pr_CI_AG$Incumplidos/sum(Pr_CI_AG$Incumplidos))
        Pr_CI_AG$ICS<-((Pr_CI_AG$Casos-Pr_CI_AG$Incumplidos)/Pr_CI_AG$Casos)
        Pr_CI_AG<-select(Pr_CI_AG,AREA,GRUPO,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est,IES,IES2) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_AG)<-c("AREA","GRUPO","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est","IES_est","IES2")
        Pr_CI_AG
      })
    }
    # Proporcion de Casos Incumplidos X Area, Grupo y Jerarquia
    if(input$AreaPS=="Pr_CI_AG_SG"){
      obs<-reactive({
        Pr_CI_AG_SG<-Data() %>%
          group_by(AREA,GRUPO,SUBGRUPO) %>%
          summarise(Incumplidos=sum(Incumplidos),
                    Casos=length(Caso),
                    Prop_Casos=(sum(Incumplidos)/length(Caso)),
                    ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo)),IES=((sum(Cerrado.en.el.mes.cumpliendo.SLA)+sum(Solucionado.pendiente.cierre.y.cumple.SLA)+sum(Sigue.pendiente.en.el.mes.y.cumple.SLA))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))),IES2=((sum(Cerrado.en.el..periodo)+sum(Solucionado.pendiente.cierre)+sum(Sigue.pendiente.en.el.mes))/(sum(Viene.vencido.de.meses.anteriores)+sum(Se.vence.en.el.mes)+(sum(Solucionado.anticipado.mes.siguiente..pendiente.cierre)))))
        Pr_CI_AG_SG$Prop_Gerencia<-(Pr_CI_AG_SG$Incumplidos/sum(Pr_CI_AG_SG$Incumplidos))
        Pr_CI_AG_SG$ICS<-((Pr_CI_AG_SG$Casos-Pr_CI_AG_SG$Incumplidos)/Pr_CI_AG_SG$Casos)
        Pr_CI_AG_SG<-select(Pr_CI_AG_SG,AREA,GRUPO,SUBGRUPO,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est,IES,IES2) 
        Pr_CI_AG_SG<-arrange(Pr_CI_AG_SG,desc(Incumplidos))
        names(Pr_CI_AG_SG)<-c("AREA","GRUPO","SUBGRUPO","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est","IES_Est","IES2")
        Pr_CI_AG_SG
      })
    }
    # Proporcion de Casos Incumplidos X Area, Grupo y Jerarquia
    if(input$AreaPS=="Pr_CI_AGJ"){
      obs<-reactive({
        Data_1<-Data()
        if(input$B1 == "Usuarios Finales"){
          Data_1<-filter(Data_1,Tipo.de.Servicio=="Servicios Usuario Final")
        }
        if(input$B1 == "Usuarios Internos"){
          Data_1<-filter(Data_1,Tipo.de.Servicio=="Servicios Internos")
        }
        if(input$B2 == "Requerimientos"){
          Data_1<-filter(Data_1,Tipo.de.caso=="Requerimiento")
        }
        if(input$B2 == "Incidentes"){
          Data_1<-filter(Data_1,Tipo.de.caso=="Incidente")
        }
        Pr_CI_AGJ<-Data_1 %>%
          group_by(AREA,GRUPO,Jerarquía.de.Categorías) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo))) %>%
          filter(Prop_Casos>0)
        
        Pr_CI_AGJ$Prop_Gerencia<-(Pr_CI_AGJ$Incumplidos/sum(Pr_CI_AGJ$Incumplidos))
        Pr_CI_AGJ$ICS<-((Pr_CI_AGJ$Casos-Pr_CI_AGJ$Incumplidos)/Pr_CI_AGJ$Casos)
        Pr_CI_AGJ<-select(Pr_CI_AGJ,AREA,GRUPO,Jerarquía.de.Categorías,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_AGJ)<-c("AREA","GRUPO","Categoria","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est")
        Pr_CI_AGJ
      })
    }
    # Proporcion de Casos Incumplidos X Area, Grupo y Jerarquia
    if(input$AreaPS=="Pr_CI_AJ"){
      obs<-reactive({
        Data_1<-Data()
        if(input$B1 == "Usuarios Finales"){
          Data_1<-filter(Data_1,Tipo.de.Servicio=="Servicios Usuario Final")
        }
        if(input$B1 == "Usuarios Internos"){
          Data_1<-filter(Data_1,Tipo.de.Servicio=="Servicios Internos")
        }
        if(input$B2 == "Requerimientos"){
          Data_1<-filter(Data_1,Tipo.de.caso=="Requerimiento")
        }
        if(input$B2 == "Incidentes"){
          Data_1<-filter(Data_1,Tipo.de.caso=="Incidente")
        }
        Pr_CI_AJ<-Data_1 %>%
          group_by(AREA,Jerarquía.de.Categorías) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo))) %>%
          filter(Prop_Casos>0)
        
        Pr_CI_AJ$Prop_Gerencia<-(Pr_CI_AJ$Incumplidos/sum(Pr_CI_AJ$Incumplidos))
        Pr_CI_AJ$ICS<-((Pr_CI_AJ$Casos-Pr_CI_AJ$Incumplidos)/Pr_CI_AJ$Casos)
        Pr_CI_AJ<-select(Pr_CI_AJ,AREA,Jerarquía.de.Categorías,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_AJ)<-c("AREA","Categoria","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est")
        Pr_CI_AJ
      })
    }
    
    ################# Especialista
    if(input$AreaPS=="Pr_CI_E"){
      obs<-reactive({
        Pr_CI_E<-Data() %>%
          group_by(AREA,GRUPO,Especialista) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo))) %>%
          arrange(Incumplidos) 
        
        Pr_CI_E$Prop_Gerencia<-(Pr_CI_E$Incumplidos/sum(Pr_CI_E$Incumplidos))
        Pr_CI_E$ICS<-((Pr_CI_E$Casos-Pr_CI_E$Incumplidos)/Pr_CI_E$Casos)
        Pr_CI_E<-select(Pr_CI_E,AREA,GRUPO,Especialista,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_E)<-c("AREA","GRUPO","Especialista","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est")
        Pr_CI_E
      })
    }
    
    
    ################# Especialista X Jerarquia
    if(input$AreaPS=="Pr_CI_EJ"){
      obs<-reactive({
        Pr_CI_EJ<-Data() %>%
          group_by(AREA,GRUPO,Especialista,Jerarquía.de.Categorías) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo))) %>%
          arrange(Incumplidos)
        
        Pr_CI_EJ$Prop_Gerencia<-(Pr_CI_EJ$Incumplidos/sum(Pr_CI_EJ$Incumplidos))
        Pr_CI_EJ$ICS<-((Pr_CI_EJ$Casos-Pr_CI_EJ$Incumplidos)/Pr_CI_EJ$Casos)
        Pr_CI_EJ<-select(Pr_CI_EJ,AREA,GRUPO,Especialista,Jerarquía.de.Categorías,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est) %>%
          arrange(desc(Incumplidos))
        names(Pr_CI_EJ)<-c("AREA","GRUPO","Especialista","Categoria","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est")
        Pr_CI_EJ
      })
    }
    
    
    ######## Servicio
    if(input$AreaPS=="Pr_CI_SJ"){
      obs<-reactive({
        Pr_CI_SJ<-Data() %>%
          group_by(AREA,Servicio,Jerarquía.de.Categorías) %>%
          summarise(Incumplidos=sum(Incumplidos),Casos=length(Caso),Prop_Casos=(sum(Incumplidos)/length(Caso)),ICS_Est= sum((Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cumplió.SLA)/sum(Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC" & Cerrado.en.el..periodo))) %>%
          filter(Prop_Casos>0) %>% arrange(desc(Incumplidos))
        Pr_CI_SJ$Prop_Gerencia<-(Pr_CI_SJ$Incumplidos/sum(Pr_CI_SJ$Incumplidos))
        Pr_CI_SJ$ICS<-((Pr_CI_SJ$Casos-Pr_CI_SJ$Incumplidos)/Pr_CI_SJ$Casos)
        Pr_CI_SJ<-select(Pr_CI_SJ,Servicio,Jerarquía.de.Categorías,Incumplidos,Casos,Prop_Casos,Prop_Gerencia,ICS,ICS_Est)
        names(Pr_CI_SJ)<-c("AREA","Servicio","Categoria","Incumplidos","Casos","Pro_Casos","Prop_Gerencia","ICS","ICS_Est")
        Pr_CI_SJ
      })
    }

    if (is.null(obs()))
      return(NULL)
    
    if(input$AreaPS %in% c("Urgentes","Incumplidos","Gestion","razon")){
      if(input$Detalle2!="Gerencia Total"){
        if(input$AreaPS=="GT"){return(NULL)}
        da<-datatable(filter(obs(),AREA %in% input$Detalle2),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'print')
        )) 
      }else da<<-datatable(obs(),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print')
      ))
      
    }else if(input$Detalle2!="Gerencia Total"){
      if(input$AreaPS=="GT"){return(NULL)}
      if("Prop_Gerencia" %in% names(obs())){
        da<-datatable(filter(obs(),AREA %in% input$Detalle2),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
                                                                                   dom = 'Bfrtip',
                                                                                   buttons = c('copy', 'csv', 'excel', 'print')
        ))%>% formatPercentage((length(obs())-3):length(obs()), 2) %>%  formatPercentage('Pro_Casos', 2) %>%  formatPercentage('Prop_Gerencia', 2)
        
      }else{
        da<-datatable(filter(obs(),AREA %in% input$Detalle2),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
                                                                                   dom = 'Bfrtip',
                                                                                   buttons = c('copy', 'csv', 'excel', 'print')
        ))%>% formatPercentage((length(obs())-3):length(obs()), 2) %>%  formatPercentage('Pro_Casos', 2)
        
      }
      }else if("Prop_Gerencia" %in% names(obs())){
      da<-datatable(obs(),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
                                                                                 dom = 'Bfrtip',
                                                                                 buttons = c('copy', 'csv', 'excel', 'print')
      ))%>% formatPercentage((length(obs())-3):length(obs()), 2) %>%  formatPercentage('Pro_Casos', 2) %>%  formatPercentage('Prop_Gerencia', 2)
      
    }else{
      da<-datatable(obs(),rownames=FALSE,extensions = 'Buttons', options = list(pageLength = 1000,
                                                                                 dom = 'Bfrtip',
                                                                                 buttons = c('copy', 'csv', 'excel', 'print')
      ))%>% formatPercentage((length(obs())-3):length(obs()), 2) %>%  formatPercentage('Pro_Casos', 2)
      
    }
    
  })
  
  
  ##########aqui termina content
  
  
  output$Especi <- renderDataTable({
    if(input$AREA=="Todas"){
      Dat<-Data() %>% filter(!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="II"){
      Dat<-Data() %>% filter(AREA=="Infraestructura Informatica",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="SSII"){
      Dat<-Data() %>% filter(AREA=="Sistemas de Informacion",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="TELCO"){
      Dat<-Data() %>% filter(AREA=="Telecomunicaciones",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="GDS"){
      Dat<-Data() %>% filter(AREA=="Gestión del Servicio",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }    
    
    # Dat<-Dat %>% group_by(Estado,Especialista) %>% summarise(total=n())
    Dat$Progreso.Individual<-as.numeric(sub(",",".",Dat$Progreso.Individual))
    Dat %>% select(c(1:7,10:12,30,31,35,36,38,39))
  },rownames=FALSE, extensions = 'Buttons', options = list(pageLength = 1000,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'print')
  ))
  
  
  plotInput = function() {
    if(input$AREA=="Todas"){
      Dat<-Datah %>% filter(!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="II"){
      Dat<-Datah %>% filter(AREA=="Infraestructura Informatica",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="SSII"){
      Dat<-Datah %>% filter(AREA=="Sistemas de Informacion",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="TELCO"){
      Dat<-Datah %>% filter(AREA=="Telecomunicaciones",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }
    if(input$AREA=="GDS"){
      Dat<-Datah %>% filter(AREA=="Gestión del Servicio",!Estado %in% c("Cerrado","Solucionado","Denegado"))  
    }    
    
    Dat<-Dat %>% group_by(Estado,Especialista) %>% summarise(total=n())
    
    p <- ggplot(Dat, aes(Estado, Especialista)) +
      geom_tile(aes(fill = total),colour = "white") + 
      scale_fill_gradient(low = "green",high = "red")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
      geom_text(aes(Estado, Especialista, label = paste(total,"Casos")), color = "black", size = 3)
    
    p
  }
  
  output$foo <- downloadHandler(
    filename = 'Estado_Casos.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 9, height = 10,
                       res = 600, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  
  reactive({
    Data2<-Data()
    Data2$Fecha<-as.POSIXct(Data2$Fecha.real.de.solución,format = "%d/%m/%Y")
    DataF<-filter(Data2,!is.na(Fecha))
    DataF<-filter(DataF,Incluido.en.los.Indicadores.=="Incluido IES,IVS,IC")
    DataF<-arrange(DataF,Fecha)
    fechas<-unique(DataF$Fecha)
    w<-data.frame()
    for(i in 1:length(fechas)){
      Data2<-filter(DataF,Fecha %in% fechas[1:i])
      Pr<-Data2 %>%
        summarise(Incumplidos=sum(Incumplidos),ICS_Est= 100*sum((Cumplió.SLA)/sum(Cerrado.en.el..periodo)))

      Pr2<-Data2 %>%
        group_by(AREA) %>%
        summarise(Incumplidos=sum(Incumplidos),ICS_Est= 100*sum((Cumplió.SLA)/sum(Cerrado.en.el..periodo)))
      ######
      Pr2<-cbind(data.frame(Pr2),Fecha=fechas[i])
      Pr<-cbind(data.frame(Pr),Fecha=fechas[i])
      Pr$AREA<-"Gerencia AO"
      Pr<-select(Pr,AREA,Incumplidos,ICS_Est,Fecha)
      w<-rbind(w,Pr2,Pr)
    }

    w$Fecha<-as.character(w$Fecha)
    w<-arrange(w,AREA,Fecha)
    dat<-w
    if(input$Detalle != "Gerencia Total"){
      if(input$Detalle=="Arquitectura Organizacional"){
        dat<-filter(dat,AREA=="Gerencia AO")
      }else{
        dat<-filter(dat,AREA %in% input$Detalle)
      }
    }
    dat$Fecha<-as.factor(dat$Fecha)
    input_Evo<- reactive(input$sliderEvo)
    dat %>%
      ggvis(~Fecha, ~ICS_Est) %>%
      layer_points(shape = ~AREA, fill = ~AREA) %>%
      layer_text(text:=~round(ICS_Est,2)) %>%
      group_by(AREA) %>%
      layer_lines() %>%
      scale_numeric("y", domain = input_Evo, clamp = TRUE)  %>%
      set_options(width = 1080, height = 500) %>%
    add_axis("x", properties = axis_props(labels = list(angle =270, align = "right"))) }) %>%
    bind_shiny('evoluc')
})
