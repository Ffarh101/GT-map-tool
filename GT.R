
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyalert)

# Dataset
dt <- read.csv("TG_data2.csv")
dt$Gesture <- factor (dt$Gesture, levels = unique(dt$Gesture[order(dt$OrderX)]))
dt$Task <- factor (dt$Task, levels = unique(dt$Task[order(dt$OrderY)]))
dt$Priority <- as.factor(dt$Priority)
all_interact <- dt

# ====
# ui.R
# ====
ui <- shinyUI(fluidPage(
  titlePanel("Gesture-Task Analytic Database"),
  useShinyalert(),  # Set up shinyalert
  fluidRow(
    column(4,
      wellPanel(
        sliderInput("priorities", "Maximum number of priorities",
          0, 11, 11, step = 1)
      )
    ),
    column(4,
      wellPanel(
        sliderInput("scores", "Range of scores",
          1, 7, c(1, 7), step = 1)
      )
    ),
    column(4,
      wellPanel(
        selectInput("tasks", "Task (a scenario can have multiple tasks)",
           c("All", "1- Power on", "2- Power off", "3- Mute", "4- Unmute", "5- Back",
             "6- Activate menu", "7- Home", "8- Navigate", "9- Run/Select", "10- Go to first", "11- Go to last",
             "12- Confirm (Yes)", "13- Reject (No)", "14- Play", "15- Pause", "16- Move forward", "17- Move backward",
             "18- Zoom in", "19- Zoom out", "20- Next", "21- Previous", "22- Volume up", "23- Volume down")
        )
      )
    )
  ),
  fluidRow(
    column(12,
      wellPanel(
        withSpinner(plotlyOutput("hoverplot")),
        imageOutput("myImage")
      ),
      br(),
      wellPanel(
        span("Click on a point in the scatterplot to display the info/image", br()),
        br(),
        verbatimTextOutput("n_interactions"),
        br(),
        textOutput("info"),
        br(),
        DT::dataTableOutput('mytable'),
        br(),
        downloadButton('download',"Download the data")
      )
    )
  )
))

# ========
# server.R
# ========
server <- function(input, output, session_store) {
  session_store <- reactiveValues(s=NULL)
  
  # Filter the interactions, returning a data frame
  interactions <- reactive({
    priorities <- input$priorities
    minscore <- input$scores[1]
    maxscore <- input$scores[2]
    tasks <- input$tasks
    
    # Apply filters
    all_interact <- dt %>%
      filter(
        as.numeric(Priority) <= priorities,
        Score >= minscore,
        Score <= maxscore
      ) %>%
      arrange(Score)

    # Filter by task
    if (input$tasks != "All") {
      ndx = grep(input$tasks, dt$Task, perl=T)
      all_interact <- dt[ndx,]
    }
    
    all_interact <- as.data.frame(all_interact)
    all_interact <- all_interact %>%
      # Prepare text for tooltip
      mutate(text = paste("Score: ", all_interact$Score, "\nTask: ", all_interact$Task, "\nGesture: ", 
                          all_interact$Gesture, "\nPriority: ", all_interact$Priority, "<br>", 
                          sep=""))

    return(all_interact)
  })

  output$hoverplot <- renderPlotly({
    Sys.sleep(2) # Adding intentional delay in the execution of plot to demo the loading spinner
    x <- interactions()$Gesture
    gifs <- interactions()$Gesture
    # base64 encoded string of each image
    uris <- purrr::map_chr(
      gifs, ~ base64enc::dataURI(file = sprintf("www/%s.gif", .x))
    )
    
    # Classic ggplot
    p2 <- ggplot(interactions(),  aes(x=Gesture, y=Task, size = Score, color = as.factor(Priority), 
            customdata = uris, text=text, key = Interaction)) +
      geom_point(alpha=0.7) +
      scale_size(range = c(1, 5), name="Priority") +
      viridis::scale_color_viridis(discrete=TRUE, guide=FALSE) +
      hrbrthemes::theme_ipsum() +
      ylab("Task") +
      xlab("Gesture") +
      theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(axis.text.y = element_text(angle = 0, vjust = 1)) 

    session_store$hoverplot <- ggplotly(p2, tooltip="text", source = "subset", height = 750) %>%
    htmlwidgets::onRender(readLines("js/tooltip-image.js", warn=FALSE)) 
  })

  output$n_interactions <- renderPrint({
    cat("Number of interactions plotted: ", nrow(interactions()))
  })
  
  # plotly click event
  log <- NULL
  log1 <- NULL
  observe({
    session_store$s <- event_data("plotly_click", source = "subset")
    s <- session_store$s

    if(length(s) == 0) return(NULL)
    
    temp <- dplyr::filter(all_interact, Interaction == s$key)
    log <<- rbind(log, temp)

    output$info <- renderText({ "You selected: " })

    log1 <- subset(log, select = c("Task","Gesture","Score","Priority", "Image"))
    log1$Priority <- as.numeric(log1$Priority)

    listVar = "Navigate"
    showModal(modalDialog(
      title = "Instruction:",
      paste0("1) Please select the task \"", listVar, "\" from dropdown menu,
              2) then hover on the points to observe the gestures (at the top left corner) representing this task,
              3) then select the most prefered gesture from the end pose of the image at the bottom left corner."),
      # easyClose = TRUE,
      footer = modalButton("Dismiss"),
      fade = TRUE
    ))
    
    # showModal(modalDialog(
    #   title = "A new selection has been added into the table below.",
    #   paste0("The interaction is: \"", temp$Task, "\" AND \"", temp$Gesture, '\".'), 
    #   # easyClose = TRUE,
    #   footer = modalButton("Dismiss"),
    #   fade = TRUE
    # ))
    
    # shinyalert("A new selection has been added into the table below.",
    #            paste0("The interaction is: \"", temp$Task, "\" AND \"", temp$Gesture, '\".'),
    #            type = "info")
    
    showNotification("A new selection has been added into the table below.",
                     paste0("The interaction is: \"", temp$Task, "\" AND \"", temp$Gesture, '\".'),
                     duration = 5)
    
    # Report table
    # output$mytable <- DT::renderDataTable({
    #   DT::datatable(log1, escape = FALSE)})
    output$mytable <- DT::renderDataTable(
      DT::datatable(data = log1, escape = FALSE,
        extensions = 'Buttons',
        options = list(
          # scrollY = '500px', 
          # autoWidth = TRUE,
          # columnDefs = list(list(width = '200px', targets = "_all")),
          # processing = TRUE,
          dom = 'lfrtBip',
          buttons = list('copy', 'csv', 'excel', 'pdf', 'print')
        )
      )
    )
    
    output$download <- downloadHandler(
      filename = function(){"report.csv"}, 
      content = function(fname){
        write.csv(log1, fname)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
# runApp(list(ui=ui,server=server), launch.browser=TRUE) # Now runs by default in the external browser.
