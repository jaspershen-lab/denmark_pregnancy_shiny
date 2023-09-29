# Required Libraries
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "RColorBrewer", "plotly"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)


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
    tags$head(
      tags$style(HTML("
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
            "))
    ),
    tabItems(
      # About tab
      tabItem(
        tabName = "about",
        uiOutput("aboutContent")
      ),
      # Data Visualization tab
      tabItem(tabName = "viz",
              fluidRow(column(
                width = 12,
                box(plotlyOutput("lineChart"), width = 12),
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
                      selectInput("moleculeType", "Molecule Type:", choices = names(molecule_choices))
                    ),
                    column(
                      4,
                      selectInput(
                        "molecule",
                        "Molecule:",
                        choices = molecule_choices$Proteins,
                        selected = molecule_choices$Proteins[1]
                      )
                    )
                  ),
                  fluidRow(column(
                    3, checkboxInput("smooth", "Smooth lines?", FALSE)
                  ),
                  column(
                    3, checkboxInput("points", "Show points?", FALSE)
                  )),
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
              uiOutput("authorContent")
      )
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
