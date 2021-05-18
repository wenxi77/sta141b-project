library(ggplot2)
library(tidyverse)
library(plotly)
library(hrbrthemes)
library(lubridate)
library(patchwork)
library(bslib)
library(zoo)
#https://wenxizhang.shinyapps.io/sta141b-final-project/
#define function for api
get_ecndata<-function(indicator,country){
    r<-GET(str_glue("http://api.worldbank.org/v2/country/{COUNTRY}/indicator/{INDICATOR}?per_page=500&format=json",COUNTRY=country,INDICATOR=indicator))
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    if(is.null(nrow(fromJSON(json,flatten = TRUE)[[2]])))return()
    test<-fromJSON(json,flatten = TRUE)[[2]]%>%select(date,value)
    test$date=year(as.Date(test$date,format = "%Y"))
    test= test %>% drop_na()
    return(test)
}
Timeget_ecndata<-function(indicator,country,year){
    r<-GET(str_glue("http://api.worldbank.org/v2/country/{COUNTRY}/indicator/{INDICATOR}?date={date}:2021&per_page=500&format=json",COUNTRY=country,INDICATOR=indicator,date=year))
    stop_for_status(r)
    json <- content(r, as = "text", encoding = "UTF-8")
    if(is.null(nrow(fromJSON(json,flatten = TRUE)[[2]]))) return()
    test<-fromJSON(json,flatten = TRUE)[[2]]%>%select(date,value)
    test$date=year(as.Date(test$date,format = "%Y"))
    test= test %>% drop_na()
    return(test)
}
exc_ecndata<-function(country,year){
  r<-GET(str_glue("http://api.worldbank.org/v2/country/{COUNTRY}/indicator/DPANUSSPB?date={date}M01:2021M01&per_page=500&format=json",COUNTRY=country,date=year))
  stop_for_status(r)
  json <- content(r, as = "text", encoding = "UTF-8")
  if(is.null(nrow(fromJSON(json,flatten = TRUE)[[2]])))return()
  currexchange<-fromJSON(json,flatten = TRUE)[[2]]%>%select(date,value)
  currexchange$date<-currexchange$date %>% str_replace("M","-") 
  currexchange$date=as.Date(as.yearmon(currexchange$date,"%Y-%m"))
  currexchange%>% drop_na()
  return(currexchange)
}

get_covid<-function(Country){
    endpoint <- str_glue("https://disease.sh/v3/covid-19/historical/{country}?lastdays=all",country=Country)
    c<-GET(endpoint)
    stop_for_status(c)
    cases<-content(c, as = "text", encoding = "UTF-8")
    tibblecase<-fromJSON(cases)$timeline$cases
    time<-lst(tibblecase) %>% map(names)
    time<-parse_date_time(time$tibblecase, orders = c("ymd", "dmy", "mdy"))
    casedf<-tibble(time=ymd(time),cases=as.numeric(tibblecase))
    return(casedf)
}
get_coviddeath<-function(Country){
  endpoint <- str_glue("https://disease.sh/v3/covid-19/historical/{country}?lastdays=all",country=Country)
  c<-GET(endpoint)
  stop_for_status(c)
  cases<-content(c, as = "text", encoding = "UTF-8")
  tibblecase<-fromJSON(cases)$timeline$deaths
  time<-lst(tibblecase) %>% map(names)
  time<-parse_date_time(time$tibblecase, orders = c("ymd", "dmy", "mdy"))
  casedf<-tibble(time=ymd(time),cases=as.numeric(tibblecase))
  return(casedf)
}
get_covidrecover<-function(Country){
  endpoint <- str_glue("https://disease.sh/v3/covid-19/historical/{country}?lastdays=all",country=Country)
  c<-GET(endpoint)
  stop_for_status(c)
  cases<-content(c, as = "text", encoding = "UTF-8")
  tibblecase<-fromJSON(cases)$timeline$recovered
  time<-lst(tibblecase) %>% map(names)
  time<-parse_date_time(time$tibblecase, orders = c("ymd", "dmy", "mdy"))
  casedf<-tibble(time=ymd(time),cases=as.numeric(tibblecase))
  return(casedf)
}
get_table<-function(data){
  data$time=quarter(data$time,with_year = TRUE)
  df<-data%>% mutate(increase=cases-lag(cases)) %>%na.fill(0)%>%as_tibble()%>%group_by(time)%>%summarise(average_increasement=mean(increase),.groups = 'drop') %>% mutate(quarter=c("2020Q1","2020Q2","2020Q3","2020Q4","2021Q1"))%>%select(quarter,average_increasement)
  return(df)
}
#define countrylist
library(rvest)
library(httr)
library(jsonlite)
#WBcountry
allcountry<-GET("http://api.worldbank.org/v2/country?per_page=297&format=json")
stop_for_status(allcountry)
countrylist <- content(allcountry, as = "text", encoding = "UTF-8")
wb_country<-fromJSON(countrylist,flatten = TRUE)[[2]] %>% select(id,name)
#covid country list
country<-GET("https://disease.sh/v3/covid-19/historical?lastdays=all")
stop_for_status(country)
cases <- content(country, as = "text", encoding = "UTF-8")
covid_country<-distinct(tibble(fromJSON(cases)$country))
filtered<-inner_join(wb_country,covid_country,by = c("name"="fromJSON(cases)$country"))%>%arrange(name)

library(shiny)

ui <- navbarPage("My application",
                 theme = bs_theme(version = 4, bootswatch = "minty"),
             #   navlistPanel(
    # Application title of covid
    tabPanel("Covid vs Macroeconomic Statistics",
    sidebarLayout(
        sidebarPanel(
            selectInput("Country","Country",filtered %>% pull(name)),
            hr(),
            'Covid information',
            radioButtons("type","Types",
                         list("comfirned cases"="comfirned","deaths"="death","recovered"="recovered"),selected = "comfirned"),
            actionButton("Covid_table","Covid Increasement Table"),
            actionButton("reset", "Clear"),
            hr(),
            'Macroeconomic information',
            selectInput("gdp_start","GDP Start Date","-"),
            selectInput("trade_start","Trade Strart Date","-"),
            selectInput("excrate_start","Exchange Rate Strart Date","-"),
         actionButton("umemployment", "all time Umemployment plot"),
         actionButton("reset2", "Clear")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("covidPlot",width = "100%",
                         height = "400px",
                         inline = FALSE,
                         reportTheme = TRUE),
            textOutput("label"),
            tableOutput('tbl'),
            verbatimTextOutput("about"),
            plotlyOutput("gdpPlot",width = "100%",
                         height = "400px",
                         inline = FALSE,
                         reportTheme = TRUE),
            plotOutput("tradePlot"),
            verbatimTextOutput("about_trade"),
            plotlyOutput("exratePlot",width = "100%",
                         height = "400px",
                         inline = FALSE,
                         reportTheme = TRUE),
           plotlyOutput("unemployment"),
           verbatimTextOutput("summary_unem")
            
        )
    )))

 
    #about
   # tabPanel("about",
          #  fluidRow(
          #       column(12,
           # verbatimTextOutput("summary")))
                 



server <- function(input, output, session) {

    covid<- reactive({
      get_covid(input$Country)
    })

    #gdp data
    gdp<- reactive({
        req(input$gdp_start!='-')
        Timeget_ecndata("NY.GDP.MKTP.CD",filtered%>%filter(name==input$Country)%>% pull(id),input$gdp_start)
    })
    #trade data
    import<-reactive({
        req(input$trade_start!='-')
      Timeget_ecndata("NE.IMP.GNFS.ZS",filtered%>%filter(name==input$Country)%>% pull(id),input$trade_start)
    })
    export<-reactive({
        req(input$trade_start!='-')
        Timeget_ecndata("NE.EXP.GNFS.ZS",filtered%>%filter(name==input$Country)%>% pull(id),input$trade_start)
    })
    #exchange rate data
    exchange<-reactive({
      req(input$excrate_start!='-')
      exc_ecndata(filtered%>%filter(name==input$Country)%>% pull(id),input$excrate_start)
    })
    #get gdp year
    observeEvent(input$Country,{
        gdpstart1<-get_ecndata("NY.GDP.MKTP.CD",filtered%>%filter(name==input$Country)%>% pull(id)) 
        if(is.null(gdpstart1)){updateSelectInput(session,"gdp_start",choices = c("-"))}
        if (is.null(gdpstart1)==FALSE){
        gdpstart<-gdpstart1%>% pull(date)
        updateSelectInput(session,"gdp_start",choices = c("-",gdpstart))}
    })
    #get trade year use import to start
    observeEvent(input$Country,{
        importstart1<-get_ecndata("NE.IMP.GNFS.ZS",filtered%>%filter(name==input$Country)%>% pull(id))
        if (is.null(importstart1)){updateSelectInput(session,"trade_start",choices = c("-"))}
        if (is.null(importstart1)==FALSE){
        importstart<-importstart1%>% pull(date)
        updateSelectInput(session,"trade_start",choices = c("-",importstart))}
    })
    #get exchange rate year 
    observeEvent(input$Country,{
      ratestart1<-get_ecndata("DPANUSSPB",filtered%>%filter(name==input$Country)%>% pull(id))
      if (is.null(ratestart1)){updateSelectInput(session,"excrate_start",choices = c("-"))}
      if (is.null(ratestart1)==FALSE){ 
        ratestart<-ratestart1%>% pull(date)
      #if(length(ratestart)>1){ilist<-ratestart[2:length(ratestart)]}
      #if(length(ratestart)==1){ilist<-ratestart}
      updateSelectInput(session,"excrate_start",choices = c("-",ratestart))}
    })
    
    #get employnment data

    umemploy<-reactive({
        get_ecndata("SL.UEM.TOTL.NE.ZS",filtered%>%filter(name==input$Country)%>% pull(id))
      
    })
    
    
    

        output$covidPlot <- renderPlotly({
          if(input$type=="comfirned"){
            x=get_covid(input$Country)
          }
          if(input$type=="death"){
            x=get_coviddeath(input$Country)
          }
          if(input$type=="recovered"){
            x=get_covidrecover(input$Country)
          }
          
          
          Title = sprintf("COVID %s cases vs Time in %s",input$type,input$Country)
          req(nrow(x)>0)
            p<-x%>%
                ggplot( aes(x=time, y=cases)) +
                geom_area(fill="bisque", alpha=0.5) +
                geom_line(color="bisque")+labs(title=Title)
            p
            
            
        })

    #plot table
    v <- reactiveValues(data = NULL)
    observeEvent(input$Covid_table, {
        v$data <- covid()
    })
    observeEvent(input$reset, {
        v$data <- NULL
    }) #observeEvent(input$Covid_table,{
     observe({
       input$type
       
       if(input$type=="comfirned"){
         y=get_covid(input$Country)
       }
       if(input$type=="death"){
         y=get_coviddeath(input$Country)
       }
       if(input$type=="recovered"){
         y=get_covidrecover(input$Country)
       }
         output$label<-renderPrint({
           if (is.null(v$data)) return(cat("please click to update table!"))
             cat("Covid",input$type,"Increasement rate Table")
         })
     output$tbl<-renderTable({
        if (is.null(v$data)) return()
       req(nrow(y)>0)
        df<-get_table(y)
         df1<-data.frame(t(df))
         colnames(df1) <- NULL
         df1
         })
     #about
     output$about <- renderText({
       req(v$data)
       paste("Covid",input$type,"Increasement rate Table gives average daily increasement of covid",input$type,"cases across each quarter")
     })
     })


    #plot gdp
    output$gdpPlot<-renderPlotly({
        req(input$gdp_start!='-')
        Title = sprintf("Yearly Country GDP (current US$) in %s",input$Country)
        gdp()%>%
            ggplot( aes(x=date, y=value)) +
            geom_area(fill="#69b3a2", alpha=0.5) +geom_point(color="#69b3a2")+
            geom_line(color="#69b3a2") +labs(title=Title,x="Time in year",y="Country GDP in current US$")
        
        
    })
    #plot trade
    output$tradePlot<-renderPlot({
      req(input$trade_start!='-')
        data<-inner_join(export(),import(),by = c("date"))
        data=data %>% mutate(dif=(value.x-value.y))
        Title = sprintf("Yearly percentage change in GDP of Exports vs Import in %s",input$Country)
        ggplot(data, aes(x=date)) +
            geom_line( aes(y=value.x), size=1, color='cornflowerblue')+ geom_point(aes(y=value.x),color='cornflowerblue')+ 
            geom_line( aes(y=value.y ), size=1, color='coral2') + geom_point(aes(y=value.y), color='coral2')+
            geom_line(mapping=aes(y=dif),size=1)+ geom_point(aes(y=dif))+labs(caption = "trade balance")+
            scale_y_continuous(
                # Features of the first axis
                name = "Exports of Goods and services, GDP % change",#value.x is export
                # Add a second axis and specify its features
                sec.axis = sec_axis(~.,name="Imports of Goods and services, GDP % change")
            ) +
          #  theme_ipsum() +
            theme(
                axis.title.y = element_text(color = 'cornflowerblue', size=13),
                axis.title.y.right = element_text(color = 'coral2', size=13)
            ) +
            labs(title=Title)
    })
    #print about for trade
    output$about_trade<-renderText({
        req(input$trade_start!='-')
        "black line give trade balance, which is the difference of exports and imports of goods and services measured as a share of gross domestic product.\n"
    })
    #plot exchange rate
    output$exratePlot<-renderPlotly({
      req(input$excrate_start!='-')
      Title = sprintf("Exchange Rate in %s",input$Country)
      exchange()%>%
        ggplot( aes(x=date, y=value)) +
        geom_area(fill="thistle", alpha=0.5) +
        geom_line(color="thistle") +labs(title=Title,x="Time in year",y="Local currency units (LCU) per U.S. dollar")
      
      
    })
 #plot unemploynment
    v2 <- reactiveValues(data = NULL)
    observeEvent(input$umemployment, {
        v2$data <- get_ecndata("SL.UEM.TOTL.NE.ZS",filtered%>%filter(name==input$Country)%>% pull(id))
    })
    observeEvent(input$reset2, {
        v2$data <- NULL
    }) 
    output$unemployment<-renderPlotly({
        if (is.null(v2$data)) return()
      if (nrow(umemploy())==0)return()
        Title = sprintf("percentage of Unemployment in total labor force in %s",input$Country)
        umemploy()%>%
            ggplot( aes(x=date, y=value)) +
            geom_point(color="deepskyblue", alpha=0.5) +
            geom_line(color="deepskyblue") +labs(title=Title,x="Time in year",y="Unemployment, total (% of total labor force)")
        
    })
    #summary unemploynment
    output$summary_unem<-renderPrint({
        req(v2$data)
      if (nrow(umemploy())==0)return(cat("No avaliable Unemployment data!"))
        cat("Summary statistic for unemploynment rate\n")
        summary(umemploy()$value)
    })
    
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
