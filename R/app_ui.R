#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import shinythemes
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import leaflet
#' @import data.table
#' @import bsplus
#' @import sf
#' @import shinyBS
#' @import DT
#' @import dplyr
#' @import purrr
#' @import shinyjs

app_ui <- function(request) {
  attr_choices<-list("abund","n_fish","p_occ", "hab_den", "hab_mov", "hab_rus", "hab_cwd", "hab_cav", "thlb", "ogma", "defer")
  names(attr_choices)<-c("Estimated Abundance (N)", "Potential Abundance(N)", "Rel. Probability Occupancy", "Denning", "Movement", "Resting Rust", "Resting CWD", "Resting Cavity", "THLB", "OGMA", "Old Growth Deferral")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    fluidPage(
      shinyjs::useShinyjs(), 
      navbarPage(collapsible = T, id="nav",
              title = div(img(src ='www/img/gov3_bc_logo.png', class = "padding"),'Fisher Equivalent Territory Area (FETA)'),
             
              tabPanel("Map",
                       sidebarLayout(
                         
                         sidebarPanel(
                           h4("Query"),
                           selectizeInput("tsa", "Select by TSA", choices = c("Clear All", tsaBnds),
                             selected = NULL, multiple = TRUE, 
                             options = list('plugins' = list('remove_button'), placeholder = 'Select a Timber Supply Area', 'persist' = F)
                           ),
                           selectInput("colorFilt", "Select by Attribute", choices = attr_choices
                           ),
                           conditionalPanel("input.colorFilt == 'hab_den'",
                            sliderInput("threshold_hab_den", "Denning Habitat (%)", 0, 20, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_cav'",
                                           sliderInput("threshold_hab_cav", "Cavity Habitat (%)", 0,25, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_mov'",
                                           sliderInput("threshold_hab_mov", "Movement Habitat (%)", 0, 50, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_rus'",
                                           sliderInput("threshold_hab_rus", "Rust Habitat (%)", 0, 25, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_cwd'",
                                           sliderInput("threshold_hab_cwd", "CWD Habitat (%)", 0, 25, 0, step= 0.1)
                          ),
                          conditionalPanel("input.colorFilt == 'ogma'",
                                           sliderInput("threshold_ogma", "OGMA (ha)", 0, 1000, 0, step= 10)
                          ),
                          conditionalPanel("input.colorFilt == 'defer'",
                                           sliderInput("threshold_defer", "Old Growth Deferral (ha)", 0, 1000, 0, step= 10)
                          ),
                          h4("FETA Summary"),
                          tableOutput("fetaSummary"),
                          
                          h4("Old Growth Summary"),
                          tableOutput("oldGrowthSummary"),
                         ),
                         mainPanel(
                            leafletOutput("map", height = 600),
                            absolutePanel(id = "fetaInfoPanel", class = "panel panel-default", draggable = T, 
                                          top = 350, left = "auto", right = 42, bottom = "auto",
                                          fixed = T, width = 180, height = "auto",
                                          tableOutput("fetaInfoTable")
                                          )
                         )
                       )
                      
              ),
              tabPanel("Data",
                       sidebarLayout(
                         sidebarPanel(
                           h4("Result Set"),
                           h5("TSA:"),
                           htmlOutput("tsaSelected"),
                           h5("FETAs with Habitat (%):"),
                           tableOutput("rsHabitat"),
                           h5("FETAs with Old Growth (ha):"),
                           tableOutput("rsOldGrowth"),
                           h4("Export"),
                           checkboxInput("terms", value = FALSE,
                                         label = actionLink("termsMod","I Agree to Terms and Conditions")),
                           downloadButton("downloadData", "Download")
                         ),
                         mainPanel(
                           
                           dataTableOutput("fetaData")
                         )
                       ),
                       tags$footer(actionLink("contactUs", "Contact Us ", onclick = "window.open('https://www.bcfisherhabitat.ca/contact/')" ), 
                                   align = "center",
                                   style = "
                bottom:0;
                width:100%;
                height:40px;   /* Height of the footer */
                color: white;
                padding:10px;
                background-color: #036;
                z-index: 2000;
                font-family:sans-serif;"),
                       tags$footer(
                         tags$style(HTML('#contactUs{color:white}'))
                       ),
                       tags$head(tags$script(HTML('
                           Shiny.addCustomMessageHandler("jsCode",
                           function(message) {
                           eval(message.value);
                           });'
                       )))
              )
              )
      )
      
    )
      
    
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(path = app_sys('app/www'),
                            app_title = 'FETA Mapper'),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    golem::activate_js(),
    golem::favicon()
  )
}

