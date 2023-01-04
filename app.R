library(shiny)
library(shinycssloaders)
library(rtweet)
library(tidyverse)
library(shinyauthr)

user_base <- tibble::tibble(
  user = c("cristiambustos", "admin"),
  password = sapply(c("somosforma123.*", "admin"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  shinyauthr::loginUI(id = "login"),
  uiOutput("app_login"),
  # Application title
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  data <- eventReactive(input$action, {
    if(is.null(input$keyword) || input$keyword == ''){
      return()
    }else{
      token <- create_token(
        app = "Aprendizaje en R",
        consumer_key = "GJruHW9VDQChK0BdffxyQonQp",
        consumer_secret = "lvR1gReV8mMC7dCmNpqyzx8Vm5Is1bKgjcA6Q950aAExrqMym1",
        access_token = "1208931207955800064-QpIttxKTfBz6ioe2JSLODWpG24Ojiw",
        access_secret = "gzENcNTPa5FRO1k9fStulRAMuAeHu6mZicMkLfx1bD7b6")
      df <- search_tweets(input$keyword, n = input$lenght, include_rts = T)%>%select(created_at, id_str, text, favorite_count, retweet_count)
      df
    }
  })
  
  output$app_login <- renderUI({
    req(credentials()$user_auth)
    sidebarLayout(
      sidebarPanel(
        textInput("keyword", h4("Escribe una palabra")),
        numericInput("lenght", h4("Cantidad de Tweets"), value = 200),
        actionButton("action", label = "Buscar", icon = shiny::icon("search"), class = "btn-info"),
        downloadButton("downloadData",label = "Descargar",icon = shiny::icon("download"), class="btn-danger"),
        width = 3
      ),
      mainPanel(
        dataTableOutput("data_show")%>%withSpinner(type = 8, color="#103778")
      )
    )
  })
  
  output$data_show <- renderDataTable(
    data()
  )
  
  print(observe(data))
  
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste(input$keyword, " ",Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(data(),row.names = F, file)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
