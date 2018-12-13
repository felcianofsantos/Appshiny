## app.R ##
library(shinydashboard)
library(ggplot2)
library(dplyr)



ui <- dashboardPage(
  dashboardHeader(title = "SHINYAPP1"),
  
  
  ## Sidebar content
  sidebar <-  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Geral", tabName = "ger", icon = icon("bar-chart-o")),
      menuItem("Dados SIA", tabName = "lin", icon = icon("bar-chart-o")),
      menuItem("Ouvidoria", tabName = "ouv", icon = icon("bar-chart-o")),
      menuItem("Top", tabName = "top", icon = icon("bar-chart-o"))
    )
  ),
  
  
  
  
  
  ## Body content
  
dash <-  dashboardBody(
    tabItems(
      
      tabItem(tabName = "ger",
              
              fluidRow(
                
                sidebarPanel(
                  selectInput("Emp","Empresa", choices=(dados4$Empresa), selected= names(dados4)[1]))
                
                
               
              ),
              valueBoxOutput("value17")
              ,valueBoxOutput("value18")
              ,valueBoxOutput("value15")
              ,valueBoxOutput("value14")
              ,valueBoxOutput("value5")
              ,valueBoxOutput("value6")
              ,valueBoxOutput("value7")
              ,valueBoxOutput("value8")
              ,valueBoxOutput("value9")
              ,valueBoxOutput("value10")
              ,valueBoxOutput("value11")
              ,valueBoxOutput("value12")
              ,valueBoxOutput("value13")
              ,valueBoxOutput("value16")
              
              ),
            
              
            
      
      
      # First tab content
      tabItem(tabName = "lin",
              fluidRow(
                
                #Seleciona Linha
                sidebarPanel(
                #selectInput("lin","Linhas", choices=(dados4$Linha_MOD),selected= names(dados4)[1] )),
                selectInput("Linha","Linha", choices="",selected="" )),
                
                #Seleciona indicador
                sidebarPanel(
                   selectInput("Ind","Indicadores", choices=colnames(dados4[,c("ICPO","IPHS","ICVI","IMFU","IOQ","IDI","IO1","IO2",
                                                                              "IO3","IOAP")]))),
         
           
                
                #Cria o boxplot
                tabsetPanel(type="tab",        
                            tabPanel("Grafico Boxplot",plotOutput(outputId = "box1")),
                            tabPanel("Evolucao temporal",plotOutput(outputId = "line")))
              )
      ),
      

      
      # Segunda Tabela Ouv
      tabItem(tabName = "ouv",
              #Seleciona Linha
              
              tabsetPanel(
                type = "tab",
                tabPanel("Grafico Ouvidoria",plotOutput(outputId = "boxouv"))
              ),
              
              #sidebarPanel(
               # selectInput("Empe","Empresa", choices=(transportes1$Concessionaria))
             # ),
              sidebarPanel(
                selectInput("Area","Area", choices=(transportes1$Area))
              )             
      ),
      tabItem(tabName = "top",
              fluidRow(
                
                sidebarPanel(
                  selectInput("Ind2","Indicadores", choices=colnames(dados4[,c("ICPO","IPHS","ICVI","IMFU","IOQ","IDI","IO1","IO2",
                                                                              "IO3","IOAP")])))
                
              ),
              #Exibe Tabela Maior
              fluidRow(
                tabPanel("INDICAORES", DT::dataTableOutput("mytable1")
                )
              )
              
              )
      
      
      
      
      
      

    )
  )

  
  
  
  
  
  
  





  
)

server<- function(session, input, output){
  
#shinyServer(function(input, output){
  

  
observeEvent(
    input$Emp,
    updateSelectInput(session, "Linha", "Linha", 
                      choices = dados4$Linha_MOD[dados4$Empresa==input$Emp]))
  
  
  
  df <-  reactive({
    
    df=filter(dados4,Empresa==input$Emp)
    df=filter(df,Linha_MOD==input$Linha)
    
    
  })
  
 

  output$value17 <- renderValueBox({
    NEMP.LC=nrow(table(dados4$Empresa))
    
    valueBox(
      formatC(NEMP.LC, format="d", big.mark=',')
      ,'Num Total de Empresas no sistema '
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
  }) 
  
  output$value18 <- renderValueBox({
    NDATA.LC=nrow(table(dados4$data))
    
    valueBox(
      formatC(NDATA.LC, format="d", big.mark=',')
      ,'Num Total de meses enviados'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
  }) 
  
  
  output$value15 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    NMES.LC=nrow(table(dados4$data))
    
    valueBox(
      formatC(NMES.LC, format="d", big.mark=',')
      ,'Meses enviados por empresa '
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
  }) 
  
  
  output$value14 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    NLINHA.LC=nrow(table(dados4$Linha_MOD))
    
    valueBox(
      formatC(NLINHA.LC, format="d", big.mark=',')
      ,' Num Linhas da empresa '
      ,icon = icon("stats",lib='glyphicon')
      ,color = "black")
    
  }) 
  
  
  output$value5 <- renderValueBox({
    
    dados4<-subset(dados4,Empresa ==input$Emp)
    ICPO.LC=(sum(dados4$VR,na.rm =TRUE))/(sum(dados4$VP,na.rm =TRUE))
    
    valueBox(
      formatC(ICPO.LC, format="f", big.mark=',')
      ,'ICPO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "navy")
    
  }) 
  
  output$value6 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IPHS.LC=(sum(dados4$VR,na.rm =TRUE)-sum(dados4$VIMP,na.rm =TRUE))/(sum(dados4$VR,na.rm =TRUE))
    
    valueBox(
      formatC(IPHS.LC, format="f", big.mark=',')
      ,'IPHS GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
    
  }) 
  
  output$value7 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    
    ICVI.LC=(sum(dados4$VR,na.rm =TRUE)-sum(dados4$VINT,na.rm =TRUE))/(sum(dados4$VR,na.rm =TRUE))
    
    valueBox(
      formatC(ICVI.LC, format="f", big.mark=',')
      ,'ICVI GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "aqua")
    
  }) 
  output$value8 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IMFU.LC=mean(dados4$IMFU,na.rm =TRUE)
    
    valueBox(
      formatC(IMFU.LC, format="f", big.mark=',')
      ,'IMFU MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "olive")
    
  }) 
  
  output$value9 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IOQ.LC=mean(dados4$IOQ,na.rm =TRUE)
    valueBox(
      formatC(IOQ.LC, format="f", big.mark=',')
      ,'IOQ MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "maroon")
    
  }) 
  
  output$value10 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IDI.LC=mean(dados4$IDI,na.rm =TRUE)
    valueBox(
      formatC(IDI.LC, format="f", big.mark=',')
      ,'IDI MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
  }) 
  
  output$value11 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IO1.LC=mean(dados4$IO1,na.rm =TRUE)
    valueBox(
      formatC(IO1.LC, format="f", big.mark=',')
      ,'IO1  MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "teal")
    
  }) 
  
  
  output$value12 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IO2.LC=mean(dados4$IO2,na.rm =TRUE)
    valueBox(
      formatC(IO2.LC, format="f", big.mark=',')
      ,'IO2  MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  }) 
  
  
  output$value13 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IO3.LC=mean(dados4$IO3,na.rm =TRUE)
    valueBox(
      formatC(IO3.LC, format="f", big.mark=',')
      ,'IO3  MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")
    
  }) 
  
  
 
  output$value16 <- renderValueBox({
    dados4<-subset(dados4,Empresa == input$Emp)
    IOAP.LC=mean(dados4$IOAP,na.rm =TRUE)
    valueBox(
      formatC(IOAP.LC, format="f", big.mark=',')
      ,'IOAP  MEDIO GLOBAL'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  }) 

  
  
  
  
 
  
  
  
  
  
  
  
  output$box1<-renderPlot({
    if (input$Ind=="ICPO") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=ICPO)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
    } else if(input$Ind=="IPHS") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IPHS)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
    } else if (input$Ind=="ICVI") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=ICVI)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    } else if (input$Ind=="IMFU") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IMFU)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    } else if (input$Ind=="IOQ") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IOQ)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    }  else if (input$Ind=="IDI") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IDI)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    } else if (input$Ind=="IO1") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IO1)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    } else if (input$Ind=="IO2") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IO2)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
    } else if (input$Ind=="IO3") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IO3)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
      
      
    } else if (input$Ind=="IOAP") {
      
      g=ggplot(df(), aes(x=Linha_MOD, y=IOAP)) + 
        geom_boxplot() +
        theme_bw() + 
        theme_classic()
      g
    } 
    
   
    
      
  })
  
  
  
  
  
  ##
  
  
  
  
  
  
  
  
  
  
  
  output$line<-renderPlot({
    
    if (input$Ind=="ICPO") {
      
      g=ggplot(df(),aes(ICPO, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=ICPO), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic()
      g
      
    } else if(input$Ind=="IPHS") {
      
      g=ggplot(df(),aes(IPHS, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IPHS), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      
      g
    } else if (input$Ind=="ICVI") {
      
      g=ggplot(df(),aes(ICVI, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=ICVI), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
    } else if (input$Ind=="IMFU") {
      
      g=ggplot(df(),aes(IMFU, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IMFU), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
    } else if (input$Ind=="IOQ") {
      
      g=ggplot(df(),aes(IOQ, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IOQ), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
    }  else if (input$Ind=="IDI") {
      
      g=ggplot(df(),aes(IDI, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IDI), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
    } else if (input$Ind=="IO1") {
      
      g=ggplot(df(),aes(IO1, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IO1), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
      
    } else if (input$Ind=="IO2") {
      
      g=ggplot(df(),aes(IO2, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IO2), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
    } else if (input$Ind=="IO3") {
      
      g=ggplot(df(),aes(IO3, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IO3), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
      
      
    } else if (input$Ind=="IOAP") {
      
      g=ggplot(df(),aes(IOAP, x=data)) +  geom_point()+geom_line()+
      geom_text(aes(label=IOAP), vjust=-0.26, size=3 ) +
        theme_bw() + 
        theme_classic() 
      g
    } 
    
    
    
  })
##
##Ouvidoria
  
 

  
  output$boxouv<-renderPlot({
    
    empresa =filter(transportes1,Concessionaria==input$Emp )
    empresa=filter(empresa, Area==input$Area)
  
    dadosouv= empresa%>%
      group_by(Subtipo) %>%
      summarise(Solicitacoes=n())%>%
      arrange(desc(Solicitacoes))
    dadosouv
    
    ggplot(data=dadosouv, aes(x=Subtipo, y=Solicitacoes, fill=Subtipo)) + 
      geom_bar(position = 'dodge', stat='identity') +
      geom_text(aes(label=Solicitacoes),vjust=-0.25,size=3.0)+
      theme_bw() + 
      theme_classic() +
      theme(legend.position = "none",axis.text.x = element_text(angle=90, vjust=0.4)) 
  })
  

  

##
  #Lista Tops
  output$mytable1 <- DT::renderDataTable({
    if (input$Ind2=="ICPO") {
      DT::datatable(tabela[, c(1,2)])
    }else if(input$Ind2=="IPHS"){
      DT::datatable(tabela[, c(1,3)])
    }else if(input$Ind2=="ICVI"){
      DT::datatable(tabela[, c(1,4)])
    }else if(input$Ind2=="IMFU"){
      DT::datatable(tabela[, c(1,5)])
    }else if(input$Ind2=="IOQ"){
      DT::datatable(tabela[, c(1,6)])
    }else if(input$Ind2=="IDI"){
      DT::datatable(tabela[, c(1,7)])
    }else if(input$Ind2=="IO1"){
      DT::datatable(tabela[, c(1,8)])
    }else if(input$Ind2=="IO2"){
      DT::datatable(tabela[, c(1,9)])
    }else if(input$Ind2=="IO3"){
      DT::datatable(tabela[, c(1,10)])
    }else if(input$Ind2=="IOB"){
      DT::datatable(tabela[, c(1,11)])
    }else if(input$Ind2=="IOAP"){
      DT::datatable(tabela[, c(1,12)])
    }
  })
  
  
  
  
  
  
  
  

}






shinyApp(ui, server)