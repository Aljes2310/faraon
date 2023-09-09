#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(readr)

setwd("C:/Users/Alfredo/Desktop/freelancer/faraon/")
#setwd("~/brc20/faraon/datos")

df <- read_csv("name22.csv")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$coronas <- renderDT({
  
    #setwd("~/brc20")
    cartera <- read_csv("cartera2.txt")
    
  
    cartera$token <- substr(cartera$token, 11,11)
    
   
    
#     aver <- subset(cartera, cartera2$token %in% dos$character)
#     
#     
#     aver2 <- subset(dos, dos$character %in% cartera2$token)
# aver2    
#     
    # corona1 <- "????"
    # corona2 <- "????"
    # 
    # coronas <- datatable(cbind(corona1, corona2))
    # 
    # 
    # coronas
    
    cartera <- cartera[c(139, 140,142, 143, 150, 156, 161, 165, 166, 167, 181),1]
    
    dd <- t(cartera)                                 
                                                  # , "????", "????", "????",
                                                  #    "????", "????", "????", "????",
                                                  #    "????", "????",	"????"))
     

    dd %>%
      datatable(options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"), dom = 't'), rownames = FALSE) %>%
      formatStyle(columns = c(1,11), fontSize = '150%') %>%
      formatStyle(columns = c(2,10), fontSize = '180%') %>%
      formatStyle(columns = c(3,9), fontSize = '210%')%>%
      formatStyle(columns = c(4,8), fontSize = '240%') %>%
      formatStyle(columns = c(5,7), fontSize = '270%')%>%
      formatStyle(columns = c(6), fontSize = '300%')%>% 
      formatStyle(2,target='row',backgroundColor = "white")
    
 
    

})
  
  
  
  output$elefante <- renderDT({
    
    
    
    elephant <- subset(df, df$deploy == 3288908)
   
    elephant <- as.data.frame(t(as.data.frame(rep(elephant$character[1],25))))
    elephant[2,1:25] <- ""
    
    donde <- round(7)
    
    
    elephant %>%
      datatable(options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"), dom = 't', columnDefs = list(
            list(width = "1%", class = "dt-right", targets = 1)
          )), rownames = FALSE) %>%
      formatStyle(columns = c(1:100), fontSize = '175%',lineHeight='80%')%>% 
      formatStyle(2,target='row',backgroundColor = "white")
    
  })
    
  
  
  
  output$tabla <- renderDT({
    
    
    df <- df[,c(2,4,7:12)]
    
    #setwd("~/brc20/faraon/datos")
    df2 <- read_csv("name30.csv")
    df2 <- df2[,c(2,11,12)]
    colnames(df2) <- c("BRC-20", "Sales (BTC)", "Holders")
    
    
    df3 <- merge(df2, df, by.x = "BRC-20", by.y="character", all.x=TRUE, all.y=TRUE)
    df3$`Change in Holders` <- df3$Holders - df3$holders
    df3$`Change in Sales` <- round((df3$Sales - df3$sales)/100000000,5)
    
    df3$Minted <- ((df3$supply - df3$remaining)/df3$supply)*100
    
    df3 <- df3[,c(8, 1, 4, 2, 12,  3, 11, 13)]
    df3$`Sales (BTC)` <- round(df3$`Sales (BTC)`/100000000,5)
    colnames(df3)[1] <- "Deploy Number"
    colnames(df3)[2] <- "BRC-20"
    
    df3$`BRC20 Position` <- as.numeric(df3$`Deploy Number`) - 348020 
    names <- df3$name
    df3$Name <- df3$name
    df3$name <- NULL
    
    df3 <- df3[order(-df3$Sales), ]
    
    #df3 <- subset(df3, df3$`Sales (BTC)` >= input$sales[1] & df3$`Sales (BTC)` <= input$sales[2])
    
    
    #df3$Token <- paste(df3$`BRC-20`, bquote([names]))
    
    
    #colnames(df3) <- c("")
    #colnames(df) <- c("BRC-20", "Colloquial Name", "Supply", ) 
    
    df3 <- df3[,c(1,2,9,3,4,5,6,8,7)]
    
    
    datatable(df3, rownames = FALSE, options = list(
      columnDefs = list(list(className = 'dt-right', targets = c(1)), list(className = 'dt-center', targets = c(4:8))))) %>%
 
      formatStyle(columns = "BRC-20", fontSize = "250%") %>%
      formatStyle(columns = "Name", fontSize = "65%")%>%
    formatStyle(columns = "Change in Sales", color = styleInterval(0, c("black", "#00CD00")))%>%
      formatStyle(columns = "Change in Holders", color = styleInterval(0, c("black", "#00CD00")))

    
    
  })
  
  
  observeEvent(input$Analyze,{
  
  output$wallet <- renderDT({
    
    library(stringr)
    
    wallet <-input$adresse
      
    #wallet <- "bc1qsecl9297rz0zclr5ctjwpnfjphcvfhegfrctft"
    
    url <- paste0("https://brc20api.bestinslot.xyz/v1/get_brc20_balance/", wallet)
    
    
    download.file(url, "cartera.csv", fileEncoding = "UTF-8" )
    
    
    cartera <- as.data.frame(colnames(read_csv("cartera.csv" )))
    
    colnames(cartera) <- "emoti"
    
    
    
    
    emoti_detect <- str_detect(cartera$emoti, "tick")
    emoti <- subset(cartera, emoti_detect == TRUE)
    
    filtro <- ifelse(nchar(emoti$emoti) > 12 , 1, 0)
    emoti <- subset(emoti, filtro == 0)
    
    emoti$emoti2[2:length(emoti$emoti)] <- substr(emoti$emoti[2:length(emoti$emoti)], 10,10)
    emoti$emoti2[1] <- substr(emoti$emoti[1], 11, 11)
    
    
    
    balance_detect <- str_detect(cartera$emoti, "ava")
    balance <- subset(cartera, balance_detect == TRUE)
    balance$Balance33 <-str_remove(balance$emoti, "\\.[^.]*$")
    balance$Balance <- as.numeric(unlist(regmatches(balance$Balance33,
                                                    gregexpr("[[:digit:]]+\\.*[[:digit:]]*",balance$Balance33))))
    
    
    all <- cbind(emoti[,2], balance[,3])
    colnames(all) <- c("BRC20", "Balance")
    
    all
    
    
  })
  
  })
  
  output$collection1 <- renderDT({
    
    #setwd("~/brc20")
    cartera <- read_csv("cartera2.txt")
    
    
    cartera$token <- substr(cartera$token, 11,11)
   
    cartera <- cartera[c(139, 140,142, 143, 150, 156, 161, 165, 166, 167, 181),1]
    
    dd <- t(cartera)                                 
   
    
    dd %>%
      datatable(options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"), dom = 't'), rownames = FALSE) %>%
      formatStyle(columns = c(1,11), fontSize = '300%') %>%
      formatStyle(columns = c(2,10), fontSize = '300%') %>%
      formatStyle(columns = c(3,9), fontSize = '300%')%>%
      formatStyle(columns = c(4,8), fontSize = '300%') %>%
      formatStyle(columns = c(5,7), fontSize = '300%')%>%
      formatStyle(columns = c(6), fontSize = '300%')%>% 
      formatStyle(2,target='row',backgroundColor = "white")
    
  })
  
  
  output$collection2 <- renderDT({
    
    #setwd("~/brc20")
    cartera <- df
    
    cartera$MONKEY <- str_detect(cartera$name, "MONKEY")
    
    cartera <- subset(cartera, cartera$MONKEY == TRUE)
    
    dd <- t(cartera[,2])                                 
    dd2 <- t(cartera[,11])
    
    dd <- rbind(dd, dd2)
    
    dd %>%
      datatable(options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $(thead).remove();",
          "}"), dom = 't'), rownames = FALSE) %>%
      formatStyle(columns = c(1,11), fontSize = '300%') %>%
      formatStyle(columns = c(2,10), fontSize = '300%') %>%
      formatStyle(columns = c(3,9), fontSize = '300%')%>%
      formatStyle(columns = c(4,8), fontSize = '300%') %>%
      formatStyle(columns = c(5,7), fontSize = '300%')%>%
      formatStyle(columns = c(6), fontSize = '300%')%>% 
      formatStyle(2,target='row',backgroundColor = "white")
    
  })
  
  

})
