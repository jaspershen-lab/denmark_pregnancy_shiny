# Required Libraries
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "RColorBrewer", "plotly"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)

# Simulated dataset
set.seed(123)
participants <- c("All participants", paste0("Participant_", 1:40))
data <- expand.grid(Weeks = 1:40, Participant = participants[-1])
data$Progesterone <- rnorm(nrow(data), 50, 10)
data$Estrogen <- rnorm(nrow(data), 100, 20)
data$HCG <- rnorm(nrow(data), 2000, 500)

# Add correlation and recovery score to the simulated dataset
data$Correlation <- runif(nrow(data), -1, 1) # Random values between -1 and 1
data$Recovery_Score <- runif(nrow(data), 0, 100) # Random values between 0 and 100

# Define molecule types and associated molecules outside the server
molecule_choices <- list(Proteins = c("Progesterone", "Estrogen"),
                         Metabolites = c("HCG"))

# Server
server <- function(input, output, session) {
  output$aboutContent <- renderUI({
    # Convert the RMarkdown to HTML
    tmp_about <- tempfile(fileext = ".html")
    rmarkdown::render("about.Rmd", output_file = tmp_about)
    # Include the HTML content in the Shiny app
    includeHTML(tmp_about)
  })
  
  output$authorContent <- renderUI({
    # Convert the RMarkdown to HTML
    tmp_authors <- tempfile(fileext = ".html")
    rmarkdown::render("authors.Rmd", output_file = tmp_authors)
    # Include the HTML content in the Shiny app
    includeHTML(tmp_authors)
  })
  
  # Dynamic dropdown for molecules based on molecule type
  observe({
    updateSelectInput(session,
                      "molecule",
                      choices = molecule_choices[[input$moleculeType]],
                      selected = molecule_choices[[input$moleculeType]][1])
  })
  
  makePlot <- reactive({
    req(input$molecule, input$participant)
    
    filtered_data <- if ("All participants" %in% input$participant || length(input$participant) == 0) {
      data
    } else {
      data %>% filter(Participant %in% input$participant)
    }
    
    color_palette <- scales::hue_pal()(length(unique(filtered_data$Participant)))
    
    p <- ggplot(filtered_data, aes(x = Weeks, y = get(input$molecule), group = Participant, color = Participant))
    
    if (input$points) {
      p <- p + geom_point()
    }
    
    if (input$smooth) {
      p <- p + geom_smooth(se = FALSE)
    } else {
      p <- p + geom_line()
    }
    
    # Add correlation and recovery score annotations
    corr_value <- round(mean(filtered_data$Correlation), 2)
    recovery_value <- round(mean(filtered_data$Recovery_Score), 2)
    
    p <- p + annotate("text", x = max(filtered_data$Weeks) * 0.1, y = max(get(input$molecule, data)) * 0.9, label = paste("Correlation:", corr_value), hjust = 0)
    p <- p + annotate("text", x = max(filtered_data$Weeks) * 0.1, y = max(get(input$molecule, data)) * 0.85, label = paste("Recovery Score:", recovery_value), hjust = 0)
    
    p <- p + ggtitle(paste(input$moleculeType, "-", input$molecule))
    p <- p + scale_color_manual(values = color_palette) +
      theme_bw()
    
    return(p)
  })
  
  output$lineChart <- renderPlotly({
    p <- makePlot()
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".", tolower(input$filetype), sep = "")
    },
    content = function(file) {
      if (input$filetype == "PNG") {
        png(file,
            width = input$width * 96,
            height = input$height * 96)
        print(makePlot())
        dev.off()
      } else if (input$filetype == "PDF") {
        pdf(file, width = input$width, height = input$height)
        print(makePlot())
        dev.off()
      }
    }
  )
}
