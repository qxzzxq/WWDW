# Tirage au sort !
# @author: Qin

library(shiny)
library(futile.logger)


USER_DATABASE_PATH <- "data.csv"
ACTION_DATABASE_PATH <- "action.csv"

initialize_user_data <- function(db) {
  name <-  c("1", "2")
  count <-  c(0, 0)
  data <- data.frame(name = name, count = count)
  
  return(data)
}

load_user_data <- function(path) {
  if (!file.exists(path)) {
    flog.info("Initialize user database...")
    data <- initialize_user_data()
    write.csv(data, path, row.names = FALSE)
    
  } else {
    flog.info("Found user database, loading...")
    data <- read.csv(path)
  }
  return(data)
}

load_action_data <- function(path) {
  if (!file.exists(path)) {
    flog.info("Initialize action database...")
    data <- data.frame(action = "WHO will do WHAT?")
    write.csv(data, path, row.names = FALSE)
    
  } else {
    flog.info("Found action database, loading...")
    data <- as.character(rev(tail(read.csv(path)$action, 10)))
  }
  return(data)
}

save_user_data <- function(data, path) {
  if (file.exists(path)) {
    flog.info("Saving user data")
    write.csv(data, path, row.names = FALSE)
  } else {
    flog.info(paste("No file name", path))
  }
}

save_new_action <- function(data, path) {
  if (file.exists(path)) {
    flog.info("Saving action data")
    write(paste0('"', data, '"'), path, append = TRUE)
  } else {
    flog.info(paste("No file name", path))
  }
}

update_user_data <- function(reactive_data, name) {
  data <- reactive_data()
  count <- data[data$name == name,]$count
  data[data$name == name,]$count <- count + 1
  flog.info(paste("Update data of player", name))
  data
}

add_name <- function(name, data, count = 0) {
  to_add <- data.frame(name = name, count = count)
  output <- rbind(data, to_add)
  flog.info(paste("Add new player: ", name))
  return(na.omit(output))
}

select_player <- function(player_data, number_of_player) {
  return("test")
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("who", "Who?", choices = NULL, selected = NULL, multiple = TRUE, options = NULL),
      textInput("what", "What", value = "bring breakfast"),
      sliderInput("number_of_players","Number of lucky guys:", min = 1, max = 50, value = 1),
      textInput("new_player", "Add new player", placeholder = "John Handsome"),
      actionButton("add_user", "Add player", icon = icon("plus", lib = "font-awesome")),
      actionButton("go", "Good luck!", icon = icon("paper-plane", lib = "font-awesome"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("oh_my_god"),
      plotOutput("distPlot")
    )
  ),
  
  fillRow(
    # titlePanel("What happened recently..."),
    mainPanel(htmlOutput("what_happened_recently"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  player_data <- reactiveVal(load_user_data(USER_DATABASE_PATH))
  action_data <- reactiveVal(load_action_data(ACTION_DATABASE_PATH))
  result <- reactiveVal("WHO will do WHAT?")
  
  observe({
    updateSelectizeInput(session, 'who', choices = player_data()$name, selected = player_data()$name, server = TRUE)
  })
  
  observe({
    updateSliderInput(session, "number_of_players", value = 1, min = 1, max = length(input$who), step = 1)
  })
  
  observeEvent(input$add_user, {
    if (input$new_player %in% player_data()$name) {
      flog.info(paste("Player >>", input$new_player, "<< already exists, do nothing."))
      
    } else if (input$new_player != "") {
      new_player_data <- add_name(input$new_player, player_data())
      new_player_data <- unique(new_player_data)
      save_user_data(new_player_data, USER_DATABASE_PATH)
      player_data(new_player_data)
    }
  })
  
  observeEvent(input$go, {
    lucky_guys <- sample(input$who, size = input$number_of_players)
    action_present <- paste0(paste(lucky_guys, collapse = " and "), " will ", input$what, "!")
    action_past <- paste0(paste(lucky_guys, collapse = " and "), ": ", input$what)
    
    for (guy in lucky_guys) {
      player_data(update_user_data(reactive_data = player_data, name = guy))
    }
    save_user_data(player_data(), USER_DATABASE_PATH)
    save_new_action(action_past, ACTION_DATABASE_PATH)
    action_data(load_action_data(ACTION_DATABASE_PATH))
    result(action_present)
  })
  
  output$oh_my_god <- renderUI({
    h1(result())
  })
  
  output$distPlot <- renderPlot({
    barplot(player_data()$count,
            names.arg = player_data()$name,
            main = "Index of luckiness")
  })
  
  output$what_happened_recently <- renderUI({
    text <- action_data()
    HTML("<h3>The 10 last lucky guys</h3>", paste(text, collapse = '<br/>'))
  })
}


# Run the application
shinyApp(ui = ui, server = server)
