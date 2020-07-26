#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressWarnings(library(forecast))
suppressWarnings(library(shiny))
suppressWarnings(library(ggplot2))


suppressWarnings(library(wbstats))
suppressWarnings(library(corrplot))
suppressWarnings(library(dynlm))

suppressWarnings(library(astsa))
suppressWarnings(library(xts))

# Define list of countries
countries= list("Australia","Argentina","Brazil","Canada","China","European_Union","France","Germany","India","Italy","Indonesia","Japan","Mexico","Russia","Saudi_Arabia","South_Africa","South_Korea","Turkey","Great_Britain","US")
symbols = list(Australia='AU',Russia='RU',India='IN',China='CN',US='US',Canada='CA',European_Union="EU",Great_Britain='GB',France='FR',Germany='DE',Japan='JP',Argentina='AR',Brazil='BR',Indonesia='ID',Italy='IT',Mexico='MX',South_Africa='ZA',Saudi_Arabia='SA'
               ,Turkey='TR',South_Korea='KR')
# Define UI for application that draws a histogram
ui=
  navbarPage("Shiny App",
             
             tabPanel("Countries",
                      # Application title
                      titlePanel("Statistical G-20 Analysis"),
                      
                      # Sidebar with a slider input for number of bins 
                      
                      sidebarPanel(
                        selectInput("state", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy")),
                        sliderInput("bins",
                                    "Choose Start Years:",
                                    min = 1960,
                                    max = 2016,
                                    value = 1960),
                        sliderInput("bin1",
                                    "Choose End Years:",
                                    min = 1960,
                                    max = 2016,
                                    value = 2016), 
                        
                        
                        selectInput("state1", "Choose a Country:",
                                    countries
                                    
                        ),
                        
                        
                        radioButtons("var1",label="Select File Type",choices=list("png","pdf")),
                        
                        downloadButton(
                          outputId = "down",label="Download This Plot")
                      ),
                      mainPanel(
                        plotOutput("plot2")
                        
                        
                      )
             ),
             tabPanel("Comaprison",
                      sidebarPanel(
                        selectInput("state5", "Choose a Country:",
                                  countries
                        ),selectInput("state6", "Choose a Country:",
                                      countries
                        ),
                        selectInput("state7", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state10", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state8", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state9", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy")),
                        downloadButton(
                          outputId = "down4",label="Download This Plot"),
                        radioButtons("var4",label="Select File Type",choices=list("png","pdf")),
                        sliderInput("bin2",
                                    "Choose Start Years:",
                                    min = 1960,
                                    max = 2016,
                                    value = 1960),
                        sliderInput("bin3",
                                    "Choose End Years:",
                                    min = 1960,
                                    max = 2016,
                                    value = 2016)),
                      mainPanel(
                        plotOutput("plot3")
                      )
                      
             ),                         
             tabPanel("Corelation",
                      titlePanel(
                        "Please Select 5 different countries"
                      ),
                      sidebarPanel(
                        selectInput("state31", "Choose a Country:",
                                    countries
                        ),selectInput("state32", "Choose a Country:",
                                      countries
                        ),
                        selectInput("state33", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state34", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state36", "Choose a Country:",
                                    countries
                        ),
                        selectInput("state35", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy"))
                        
                      ),
                      mainPanel(
                        plotOutput("plot4")
                      )
                      
             ),
             tabPanel(" Future Forecast",
                      titlePanel("Forecasted Data"),
                      
                      # Sidebar with a slider input for number of bins 
                      
                      sidebarPanel(
                        selectInput("stateF", "Choose a Parameter:",list("GDP","GDPgrowth","perCapita","lifeExpectancy")),
                        selectInput("state100", "Choose a Country:",
                                    countries
                        ),
                        
                        
                        radioButtons("var2",label="Select File Type",choices=list("png","pdf")),
                        
                        downloadButton(
                          outputId = "down2",label="Download This Plot")
                      ),
                      mainPanel(
                        plotOutput("plot6")
                        
                        
                      )
             )
             
  )
server = function(input, output) {
  
  
  
  data<-reactive({    
    parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
    country=symbols
    
    p=data.frame(wb(indicator=parametres[input$state],country=country[input$state1],startdate=input$bins,enddate = input$bin1))
    p
  })
  
  data4<-reactive({    
    parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
    country=symbols
    p=data.frame(wb(indicator=parametres[input$stateF],country=country[input$state100],startdate=1960,enddate = 2016))
    p})
  
  
  data1<-reactive({                             
    parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN") 
    country=symbols
    
    p=data.frame(wb(indicator=parametres[input$state9],country=c(country[input$state5],country[input$state6],country[input$state7],country[input$state8],country[input$state9],country[input$state10]),startdate=input$bin2,enddate = input$bin3))
    p
  })
  output$plot3<-renderPlot({
    
    
    ggplot(data=data1(),aes(x=as.numeric(date),y=value,color=country))+geom_line()+xlab('Date')+geom_point()},height = 600,width = 1000)
  
  output$plot2<-renderPlot({
    
    colorSamp=sample(c("blue","green","red","yellow","magenta","purple"),1)
    
    ggplot(data=data(),aes(x=as.numeric(date),y=value,color=country))+geom_line()+geom_smooth(fill=colorSamp)+xlab('Date')+geom_point()
    
  })
  output$plot6=renderPlot({
    data_univariate=data4()
    data_univariate=data_univariate[,1:3]
    dates=as.Date(data_univariate$date,"%Y")
    xs=xts(data_univariate$value,order.by=dates)
    x.ts = ts(xs, freq=1, start=1960)
    time_s=ets(x.ts,damped = FALSE)
    fcast=forecast(time_s,h=10) 
    plot(fcast,xlab="date",ylab=as.character(parametres[input$stateF]),main=as.character(country[input$state100]))
  })
  parametres=list(GDP='NY.GDP.MKTP.CD',GDPgrowth="NY.GDP.MKTP.KD.ZG",perCapita="NY.GDP.PCAP.CD",lifeExpectancy="SP.DYN.LE00.IN")
  country=symbols
  data3<-reactive({    
    w=wb(indicator=parametres[input$state35],country=c(country[input$state31],country[input$state32],country[input$state33],country[input$state34],country[input$state36]),startdate=input$bin2,enddate =input$bin3)
    w
  })
  output$plot4<-renderPlot({
    
    w=data3()
    e=w
    p1=w[1:57,]
    p2=w[58:114,]
    p3=w[115:171,]
    p4=w[172:228,]
    p5=w[229:286,]          
    
    names(p1)[names(p1)=="value"]=e[1,6]
    
    cor_tab=merge(p1,p2,by="date")
    names(cor_tab)[names(cor_tab)=="value.y"]=e[58,6]
    
    cor_tab=merge(cor_tab,p3,by="date")
    names(cor_tab)[names(cor_tab)=="value.y"]=e[115,6]
    names(cor_tab)[names(cor_tab)=="value.x"]=e[58,6]
    
    cor_tab=merge(cor_tab,p4,by="date")
    names(cor_tab)[names(cor_tab)=="value.y"]=e[172,6]
    
    cor_tab=merge(cor_tab,p5,by="date")
    names(cor_tab)[names(cor_tab)=="value.y"]=e[229,6]
    names(cor_tab)[names(cor_tab)=="value.x"]=e[172,6]
    
    num <- sapply(cor_tab, is.numeric)
    cor_tab=cor_tab[,num]
    corr=cor(cor_tab)
    corrplot(corr,addCoef.col = "green",order = "AOE",height = 600,width = 1000)
    
  })
  
  output$down <- downloadHandler(
    filename =  function() {
      paste("Data", input$var1, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var1 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      # draw the plot
      colorSamp=sample(c("blue","green","red","yellow","magenta","purple"),1)
      print(ggplot(data=data(),aes(x=as.numeric(date),y=value,color=country))+geom_line()+xlab('Date')+geom_point(),height = 600,width = 1000)
      
      dev.off()  # turn the device off
      
    }) 
  output$down2 <- downloadHandler(
    filename =  function() {
      paste("Data", input$var2, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$var2 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      # draw the plot
      data_univariate=data4()
      data_univariate=data_univariate[,1:3]
      dates=as.Date(data_univariate$date,"%Y")
      xs=xts(data_univariate$value,order.by=dates)
      x.ts = ts(xs, freq=1, start=1960)
      time_s=ets(x.ts,damped = FALSE)
      fcast=forecast(time_s,h=5) 
      plot(fcast,xlab="date",ylab=as.character(parametres[input$stateF]),main=as.character(country[input$state100]))
      dev.off()  # turn the device off
      
    }) 
  output$down4 <- downloadHandler(
    filename =  function() {
      paste("Data", input$var4, sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    
    content = function(file) {
      if(input$var4 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device  
      
      print(ggplot(data=data1(),aes(x=as.numeric(date),y=value,color=country))+geom_line()+xlab('Date')+geom_point())
      dev.off()
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

