
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Warfarin Pharmagenomic Dose Predictor"),
    sidebarPanel(selectInput("age", "Age in Decades:", choices = c("10-19","20-29","30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")),
                 radioButtons("race", "Race:", choices = c( "Other or Unknown", "Asian", "African American", "White")),
                 radioButtons("vkorc1", "VKORC1 Genotype:", choices = c("Unknown", "G/G", "G/A", "A/A")),
                 radioButtons("cyp2c9", "CYP2C9 Genotype:", choices = c("Unknown", "*1/*2", "*1/*3", "*2/*2", "*2/*3", "*3/*3","Other")), 
                 checkboxInput('amiodarone', "Taking Amiodarone", FALSE),
                 checkboxInput("enzyme_inducers", "Taking an Enzyme Inducer (rifampin, carbamazepine, phenytoin or rifampicin)", FALSE),
                 actionButton("calc","Calculate")
    ),
    mainPanel(
      strong(em("THIS IS A PROGRAMMING EXAMPLE ONLY - DO NOT USE FOR PATIENT CARE!")),
      br(),br(),
      strong(em("IF YOU HAVE QUESTIONS ABOUT YOUR WARFARIN DOSE, PLEASE CONTACT YOUR DOCTOR.")),
      br(),br(),
      p("You Selected: "),
      tableOutput("selectedvalues"),
      br(),br(),
      textOutput("warfarindose")

    )
  )
)
