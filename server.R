
# load data
# set.seed(123)
# participants <- c("All participants", paste0("Participant_", 1:40))
# data <- expand.grid(Weeks = 1:40, Participant = participants[-1])
# data$Progesterone <- rnorm(nrow(data), 50, 10)
# data$Estrogen <- rnorm(nrow(data), 100, 20)
# data$HCG <- rnorm(nrow(data), 2000, 500)
#
# # Add correlation and recovery score to the simulated dataset
# data$Correlation <-
#   runif(nrow(data), -1, 1) # Random values between -1 and 1
# data$Recovery_Score <-
#   runif(nrow(data), 0, 100) # Random values between 0 and 100
#
# data$PValue <- rep(0.000001, nrow(data))

load("data/denmark_data2")

denmark_data <-
  denmark_data2

participants <- c("All participants", unique(denmark_data$subject_id2))

# Define molecule types and associated molecules outside the server
molecule_choices <- list(RNA = unique(denmark_data$Molecular_name[denmark_data$class == "RNA"]),
                         Protein = unique(denmark_data$Molecular_name[denmark_data$class == "Protein"]),
                         Metabolite = unique(denmark_data$Molecular_name[denmark_data$class == "Metabolite"]),
                         Cytokine = unique(denmark_data$Molecular_name[denmark_data$class == "Cytokine"]))


subject_color <-
  colorRampPalette(colors = RColorBrewer::brewer.pal(11, name = "BrBG"))(n = length(unique(denmark_data$subject_id2)))

names(subject_color) <-
  stringr::str_sort(unique(denmark_data$subject_id2), numeric = TRUE)

# Server
server <-
  function(input, output, session) {
    output$aboutContent <- renderUI({
      # Convert the RMarkdown to HTML
      tmp_about <- tempfile(fileext = ".html")
      rmarkdown::render("markdown/about.Rmd", output_file = tmp_about)
      # Include the HTML content in the Shiny app
      includeHTML(tmp_about)
    })
    
    output$authorContent <- renderUI({
      # Convert the RMarkdown to HTML
      tmp_authors <- tempfile(fileext = ".html")
      rmarkdown::render("markdown/authors.Rmd", output_file = tmp_authors)
      # Include the HTML content in the Shiny app
      includeHTML(tmp_authors)
    })
    
    # Dynamic dropdown for molecules based on molecule type
    observe({
      updateSelectInput(
        session = session,
        inputId = "molecule",
        choices = molecule_choices[[input$moleculeType]],
        selected = molecule_choices[[input$moleculeType]][1]
      )
    })
    
    makePlot <- reactive({
      req(input$molecule, input$participant)
      
      filtered_data <-
        if ("All participants" %in% input$participant ||
            length(input$participant) == 0) {
          denmark_data %>% 
            filter(Molecular_name %in% input$molecule)
        } else {
          denmark_data %>% 
            filter(subject_id2 %in% input$participant) %>% 
            filter(Molecular_name %in% input$molecule) %>% 
            dplyr::filter(!is.na(value))
        }
      
      # color_palette <-
      #   scales::hue_pal()(length(unique(filtered_data$Participant)))
      
      p <-
        ggplot(filtered_data,
               aes(
                 x = g_stage,
                 y = value
               ))
      
      if (input$points) {
        p <- p + geom_point(aes(
          group = subject_id2,
          color = subject_id2
        )) 
      }
      
      if (input$smooth) {
        if(input$smooth_one){
          p <-
            p + 
            geom_smooth(se = FALSE,
                        color = "black") 
        }else{
          p <- p + geom_smooth(se = FALSE,
                               aes(
                                 group = subject_id2,
                                 color = subject_id2
                               ))  
        }
      } else {
        p <- p + geom_line(
          aes(
            group = subject_id2,
            color = subject_id2
          )
        ) 
      }
      
      p <-
        p + ggtitle(paste(input$moleculeType, "-", input$molecule))
      p <- p + 
        scale_color_manual(values = subject_color[names(subject_color) %in% filtered_data$subject_id2]) +
        theme_bw() +
        theme(panel.grid.minor = element_blank()) +
        labs(x = "Gestational age (weeks)",
             y = "Scaled intensity")
      
      return(p)
    })
    
    output$lineChart <- renderPlotly({
      p <- makePlot()
      ggplotly(p) %>% layout(autosize = TRUE)
    })
    
    make_table <- reactive({
      req(input$molecule, input$participant)
      
      filtered_data <-
        if ("All participants" %in% input$participant ||
            length(input$participant) == 0) {
          denmark_data %>% 
            filter(Molecular_name %in% input$molecule)
        } else {
          denmark_data %>% 
            filter(subject_id2 %in% input$participant) %>% 
            filter(Molecular_name %in% input$molecule) %>% 
            dplyr::filter(!is.na(value))
        }
      
      molecule_info <-
      filtered_data %>% 
        dplyr::distinct(variable_id, .keep_all = TRUE) %>% 
        select(SAM_score, SAM_FDR, Correlation, Correlation_FDR,
               ENSEMBL, UNIPROT, SYMBOL, ENTREZID, recover_score,
               HMDB_ID, KEGG_ID)
      
      return(molecule_info)
    })
    
    
    output$moleculeInfoTable <- renderDT({
      molecule_info <- make_table()
      datatable(molecule_info)
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
