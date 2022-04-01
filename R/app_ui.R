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
  
  attr_choices<-list("abund","abnd_pt", "nfish","p_occ", "hab_den", "hab_mov", "hab_rus", "hab_cwd", "hab_cav", "thlb", "ogma", "defer")
  names(attr_choices)<-c("Estimated Abundance (N)", "Potential Abundance (N)", "Density", "Relative Probability Occupancy", "Denning", "Movement", "Resting Rust", "Resting CWD", "Resting Cavity", "THLB", "OGMA", "Old Growth Deferral")
  
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
              title = div(img(src ='www/img/gov3_bc_logo.png', class = "padding"),'Fisher Equivalent Territory Area (FETA) Mapper'),
              tabPanel("Methods",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             HTML("<h4>Navigation</h4><br><br>
                                  <li>Use the METHODS tab to get information about the fisher attributes</li>
                                  <li>Use the MAP tab to interactively explore the fisher attributes</li>
                                  <li>Use the DATA tab to download the fisher attributes you were looking at in the MAP</li>
                                  <br><br><h4>Attribute Definitions</h4><br>
                                  <h5>Use the drop down list below to get information about the attributes used in FETA Mapper.</h5>")
                           ),
                           fluidRow(
                             selectizeInput(inputId ="attribute_def", label="Select an attribute", choices = c("Overview", attr_choices),
                                            selected = NULL, multiple = FALSE, 
                                            options = list('plugins' = list('remove_button'), placeholder = 'Overview', 'persist' = F)
                             )
                           )
                         ), 
                         mainPanel(
                           uiOutput("ui_overview")
                         )
                       )
              ),
              tabPanel("Map",
                       sidebarLayout(
                         sidebarPanel(
                           div(h4("Query"),
                             icon('info-circle') %>%
                               bsplus::bs_embed_tooltip(
                                 "Select a population, area of interest and attribute to map",
                                 "right"
                               )),
                           radioButtons(inputId = "pop", label="Select a fisher population", choices =c("Boreal", "Columbia"), inline =T, selected = character(0)),
                           selectizeInput(inputId ="aoi", label="Select an area of interest", choices = "All",
                             selected = NULL, multiple = TRUE, 
                             options = list('plugins' = list('remove_button'), placeholder = 'All', 'persist' = F)
                           ),
                           selectInput("colorFilt", "Select an attribute to map", choices = attr_choices
                           ),
                           conditionalPanel("input.colorFilt == 'p_occ'",
                                            sliderInput("threshold_p_occ", "Rel. Prob. Occupancy", 0, 1, 0, step= 0.05)
                           ),
                           conditionalPanel("input.colorFilt == 'hab_den'",
                            sliderInput("threshold_hab_den", "Denning habitat (%)", 0, 20, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_cav'",
                                           sliderInput("threshold_hab_cav", "Cavity Habitat (%)", 0,25, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_mov'",
                                           sliderInput("threshold_hab_mov", "Movement habitat (%)", 0, 50, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_rus'",
                                           sliderInput("threshold_hab_rus", "Rust habitat (%)", 0, 25, 0, step= 0.1)
                          ),
                           conditionalPanel("input.colorFilt == 'hab_cwd'",
                                           sliderInput("threshold_hab_cwd", "CWD habitat (%)", 0, 25, 0, step= 0.1)
                          ),
                          conditionalPanel("input.colorFilt == 'ogma'",
                                           sliderInput("threshold_ogma", "OGMA (ha)", 0, 1000, 0, step= 10)
                          ),
                          conditionalPanel("input.colorFilt == 'defer'",
                                           sliderInput("threshold_defer", "Old growth deferral (ha)", 0, 1000, 0, step= 10)
                          ),
                          div(textOutput("selectedFisherHabitatThresholds") %>%
                                bsplus::bs_embed_tooltip(
                                  "Selected thresholds of fisher habitat"
                                ),
                              style = "margin-bottom: 20px;"),
                          
                          div(h4("FETA Summary"),
                              icon('info-circle') %>%
                                bsplus::bs_embed_tooltip(
                                  "Total = count of fetas selected; Est. N = estimated abundance; Pot. N = Potential abundance; THLB = timber harvesting landbase",
                                  "right"
                                )),
                          tableOutput("fetaSummary"),
                          
                          div(h4("Old Growth Summary"),
                              icon('info-circle') %>%
                                bsplus::bs_embed_tooltip(
                                  "OGMA = Old Growth Management Area (ha); OGMA Overlap Den + Cav = the amount of OGMA area (ha) overlaping denning and cavity habitat; Deferral = Old Growth Deferral area (ha); Deferral Overlap Den + Cav = the amount of Deferral area (ha) overlaping denning and cavity habitat  ",
                                  "right"
                                )),
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
                      
              ),
              
              tabPanel("Data",
                       sidebarLayout(
                         sidebarPanel(
                           #img(src="www/img/fisher_logo.png",height=72,width=72),
                           h4("Results from query"),
                           tags$h5("Area of interest:"),
                           htmlOutput("aoiSelected", ),
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
                           
                           DT::dataTableOutput("fetaData")
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
  
  add_resource_path(
    "sbs", system.file("www", package = "shinyBS")
  )
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

