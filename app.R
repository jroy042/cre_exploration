#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
#Supporting Libraries for analysis
library(mgcv)
library(visreg)

# UI Consists of three parts
# User Interface is what the web user see on their screen. 
# icons are defined in two places:
# https://fontawesome.com/icons?from=io
# 
ui <- dashboardPage(
   dashboardHeader(title="DO Support"),
   dashboardSidebar(sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("archway")),
      menuItem("Data", tabName = "data",icon=icon("envelope-square")),
      menuItem("Social Variables", tabName = "social", icon = icon("user-friends")),     
      menuItem("Linguistic Variables", tabName = "lx", icon = icon("comments")),
      menuItem("Constant Rate Effect", tabName = "cre", icon = icon("calculator"))
      
   )),
   dashboardBody(tabItems(
       tabItem("cre", 
              fluidRow( box(plotOutput("creTest", height = 250))
               ))
                        
      
      
   ))
)



# The server consists of the visualizations created and data wrangling carried out.
# 
server <- function(input, output) {
   fits = readRDS("fitmodels.rds")
   negs = readRDS("negs.rds")
   output$creTest <- renderPlot({
        ggplot() +
         geom_ribbon(
            data = fits, 
            aes(date, ymin=visregLwr, ymax=visregUpr, group=Model), fill="gray90"
         ) +
         geom_line(data = fits, aes(date, visregFit, group=Model, color=Model)) +
         ylab("P(DO vs Inversion)")+
         xlab("Date") + 
         theme_bw()
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

