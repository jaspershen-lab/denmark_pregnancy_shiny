
# Required Libraries
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "RColorBrewer", "plotly"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(DT)

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


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Pregnancy Molecules Project"),
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem(
      "Data Visualization",
      tabName = "viz",
      icon = icon("line-chart")
    ),
    menuItem("Authors", tabName = "authors", icon = icon("users"))
  )),
  dashboardBody(
    tags$head(tags$style(
      HTML(
        "
                /* Change the background color of the header */
                .skin-blue .main-header .navbar,
                .skin-blue .main-header .logo {
                    background-color: #00458A;  /* NTU Blue */
                    border-bottom-color: #8E0C3A;  /* NTU Red */
                }

                /* Change the hover color of the sidebar menu */
                .skin-blue .sidebar-menu > li:hover > a,
                .skin-blue .sidebar-menu > li.active > a {
                    border-left-color: #8E0C3A;  /* NTU Red */
                }

                /* Other custom styles can be added as needed */
            "
      )
    )),
    tabItems(
      # About tab
      tabItem(tabName = "about",
              uiOutput("aboutContent")),
      # Data Visualization tab
      tabItem(tabName = "viz",
              fluidRow(column(
                width = 12,
                box(plotlyOutput("lineChart"), width = 12),
                box(DTOutput("moleculeInfoTable"), width = 12),
                box(
                  title = "",
                  fluidRow(
                    column(
                      4,
                      selectInput(
                        "participant",
                        "Participants:",
                        choices = participants,
                        selected = "All participants",
                        multiple = TRUE
                      )
                    ),
                    column(
                      4,
                      selectInput(
                        inputId = "moleculeType",
                        label = "Molecule class:",
                        choices = names(molecule_choices),
                        selected = "Protein"
                      ),
                    ),
                    column(
                      4,
                      selectInput(
                        inputId = "molecule",
                        label = "Molecule:",
                        choices = molecule_choices$Protein,
                        selected = molecule_choices$Protein[1]
                      )
                    )
                  ),
                  fluidRow(
                    column(4, checkboxInput("smooth", "Smooth lines?", TRUE)),
                    column(4, checkboxInput("smooth_one", "Only one smoothed line?", TRUE)),
                    column(4, checkboxInput("points", "Show points?", TRUE))
                  ),
                  fluidRow(
                    column(3, downloadButton("downloadPlot", "Download Plot")),
                    column(
                      3,
                      numericInput(
                        "width",
                        "Download Width (in):",
                        10,
                        min = 1,
                        max = 20
                      )
                    ),
                    column(
                      3,
                      numericInput(
                        "height",
                        "Download Height (in):",
                        6,
                        min = 1,
                        max = 20
                      )
                    ),
                    column(3, selectInput(
                      "filetype", "File Type:", choices = c("PNG", "PDF")
                    ))
                  ),
                  width = 12
                )
              ))),
      # Authors tab
      tabItem(tabName = "authors",
              uiOutput("authorContent"))
    ),
    tags$head(tags$style(
      HTML(
        "
        .author-img {
          border-radius: 50%;
          width: 120px;
          height: 150px;
          object-fit: cover;
        }
      "
      )
    ))
  )
)
