#' The application server-side
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
#' @import config
#' @import ggplot2
#' @import leaflet.extras
#' @import leaflet.extras2
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  #Reactive to store click on feta information
  fetaInfo <- reactiveVal()
  fetaInfo(data.table::data.table(Attribute =c("FID", "Denning", "Movement", "Cavity", "Rust", "CWD", "THLB", "OGMA", "Deferral"),
                                  Value = c(0,0,0,0,0,0,0,0,0)))
  fetaPolyTSA <-reactive({
    if(is.null(input$tsa)){
      fetaPolySf
    }else{
      fetaPolySf[fetaTSA[fetaTSA$tsa %in% input$tsa,]$fid,]
    }
  })
  
  #Filter based on habitat selection
  fetaPolyRender <-reactive({
      fetaPolyTSA() %>% filter(hab_den >= (input$threshold_hab_den/100)*3000 & hab_mov >= (input$threshold_hab_mov/100)*3000 & hab_cav >= (input$threshold_hab_cav/100)*3000 & hab_cwd >= (input$threshold_hab_cwd/100)*3000 & hab_rus >= (input$threshold_hab_rus/100)*3000 & ogma >= input$threshold_ogma & defer >= input$threshold_defer)
  })
  
  ## render the summary table
  output$fetaSummary<-renderTable({
    summaryTable<-fetaPolyRender() %>% 
                    as.data.frame %>% 
                    summarise(Total =n(), abund=sum(abund), n_fish= sum(n_fish), thlb = sum(thlb)) %>%
                    mutate(abund=as.integer(abund), n_fish = as.integer(n_fish), thlb = as.integer(thlb))
    
    colnames(summaryTable) <- c("Total", "Estimated N", "Potential N", "THLB (ha)")
    summaryTable<-sapply(summaryTable[1:4,], FUN=function(x) prettyNum(x, big.mark=","))
    head(summaryTable, 1)
  })
  
  ## render the old growth table
  
  output$oldGrowthSummary<-renderTable({
    summaryTable<-fetaPolyRender() %>% 
      as.data.frame %>% 
      summarise(ogma = as.integer(sum(ogma)), og_dc = as.integer(sum(dc_ogma)), defer = sum(defer), defer_dc = sum(dc_defer))
    
    colnames(summaryTable) <- c("OGMA (ha)", "OGMA Overlap Den+Cav (ha)", "Deferral (ha)", "Deferral Overlap Den+Cav (ha)")
    summaryTable<-sapply(summaryTable[1:4,], FUN=function(x) prettyNum(x, big.mark=","))
    head(summaryTable, 1)
  })
    
  ## render the base leaflet map  
  output$map = leaflet::renderLeaflet({ 
    leaflet(options = leafletOptions(doubleClickZoom= TRUE, minZoom = 5)) %>% 
      setView(-121.7476, 53.7267, 6) %>%
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group ="WorldImagery" ) %>% 
      
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
                  layers = c("pub:WHSE_ADMIN_BOUNDARIES.FADM_TSA"),
                  group = c('TSA'),
                  options = leaflet::WMSTileOptions(
                    transparent = TRUE,
                    format = "image/png",
                    info_format = "text/html",
                    tiled = FALSE
                  ))%>%
      addWMSTiles(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
                  layers = c("pub:WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW"),
                  group = 'Parks',
                  options = leaflet::WMSTileOptions(
                    transparent = T,
                    format = "image/png",
                    info_format = "text/html",
                    tiled = FALSE
                  ))%>%
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
                  layers = c("pub:WHSE_ADMIN_BOUNDARIES.FADM_DESIGNATED_AREAS"),
                  group = 'FADM',
                  options = leaflet::WMSTileOptions(
                    transparent = TRUE,
                    format = "image/png",
                    info_format = "text/html",
                    tiled = FALSE
                  ))%>%
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
                  layers = c("pub:WHSE_WILDLIFE_MANAGEMENT.WCP_WILDLIFE_HABITAT_AREA_POLY"),
                  group = "WHA",
                  options = leaflet::WMSTileOptions(
                    transparent = TRUE,
                    format = "image/png",
                    info_format = "text/html",
                    tiled = FALSE
                  ))%>%
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
                  layers = c("pub:WHSE_WILDLIFE_MANAGEMENT.WCP_UNGULATE_WINTER_RANGE_SP"),
                  group = "UWR",
                  options = leaflet::WMSTileOptions(
                    transparent = TRUE,
                    format = "image/png",
                    info_format = "text/html",
                    tiled = T
                  ))%>%
      addLayersControl(
        baseGroups = "WorldImagery",
        overlayGroups = c("FETA","TSA","Parks","FADM", "WHA", "UWR" ),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("TSA", "FADM", "WHA", "UWR") ) %>%
      addScaleBar(position = "topleft")
      
  })
 
  output$fetaData<-DT::renderDataTable({
    a<-st_drop_geometry(fetaPolyRender()) %>%
      select(fid, abund, n_fish, hab_den, hab_mov, hab_cav, hab_rus, hab_cwd, ogma, defer) %>%
      mutate(abund =round(abund, 4))
    DT::datatable(a, 
                  caption = 'FETAs to be exported (omitting some of columns)',
                  extensions = "Buttons",
                  options = list("pageLength" = 10, dom = 'Bfrtip',
                                 buttons = list(list(extend = 'colvis', columns = c(1, 2, 3,4,5,6,7)))
                  )
    )
  })
  
  output$fetaInfoTable<-renderTable({
    fetaInfo()
  })
  
  output$selectedFisherHabitatThresholds<-renderText({
    paste0("Denning >",input$threshold_hab_den, "%, Movement >", input$threshold_hab_mov,
           "%, Cavity >", input$threshold_hab_cav, "%, Rust >", input$threshold_hab_rus, "% CWD >", input$threshold_hab_cwd, "%")
  })
  
  output$tsaSelected<-renderText({
    if(is.null(input$tsa)){
      paste("<B>All</B>")
      }else{
        paste("<B>", paste(input$tsa, sep = "", collapse = ", "), "</B>")}
    })
  
  output$rsHabitat<-renderTable({
    data.table(Denning = input$threshold_hab_den, Movement = input$threshold_hab_mov,
               Cavity = input$threshold_hab_cav, Rust= input$threshold_hab_rus, CWD = input$threshold_hab_cwd)
  })
  
  output$rsOldGrowth<-renderTable({
    data.table(OGMA= input$threshold_ogma, Deferral = input$threshold_defer)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("feta", Sys.Date(),".zip", sep = "")
    },
    content = function(file) {
      # create a temp folder for shp files
      temp_shp <- tempdir()
      st_write(fetaPolyRender(), paste0(temp_shp,"/feta.shp"), row.names = FALSE)
      
      zip_file <- file.path(temp_shp, "feta_shp.zip")
      shp_files <- list.files(temp_shp, "feta",
                              full.names = TRUE)
      # zip
      zip_command <- paste("zip -j", 
                           zip_file, 
                           paste(shp_files, collapse = " "))
      system(zip_command)
      # copy the zip file to the file argument
      file.copy(zip_file, file)
      # remove all the files created
      file.remove(zip_file, shp_files)
    }
  )
  
  # ... OBSERVE EVENTS ----
 #click the check box for data download 
  observeEvent(input$terms, {
    if(input$terms){
      shinyjs::enable("downloadData")
    }else{
      shinyjs::disable("downloadData")
    }
    
  })
  
  observeEvent(input$termsMod, {
    showModal(termsAndCondsModal)
  })
  
  # store the click
  observeEvent(input$map_geojson_click, {
   
    fetaInfo( data.table::data.table(Attribute =c("fid",  "denning", "movement", "cavity", "rust", "cwd","thlb", "ogma", "defer"),
                                     Value = c(as.integer(input$map_geojson_click$properties$fid),input$map_geojson_click$properties$hab_den,
                                               input$map_geojson_click$properties$hab_mov, input$map_geojson_click$properties$hab_cav, 
                                               input$map_geojson_click$properties$hab_rus, input$map_geojson_click$properties$hab_cwd,
                                               input$map_geojson_click$properties$thlb,input$map_geojson_click$properties$ogma, 
                                               input$map_geojson_click$properties$defer))
    )
  })
  
  # Change the choropleth
  observe( { 
    req(input$colorFilt)
    
    leafletProxy('map')  %>% 
      clearGroup('FETA') %>%
      addGeoJSONChoropleth(
        geojsonsf::sf_geojson(fetaPolyRender()), group = 'FETA', layerId = "fid",
        valueProperty = input$colorFilt,
        scale = c("white", "blue"),
        color = "#ffffff", weight = 1, fillOpacity = 0.5,
        highlightOptions = highlightOptions(
          weight = 2, color = "#000000",
          fillOpacity = 0.1, opacity = 1,
          bringToFront = T, sendToBack = T)
        #legendOptions =legendOptions (position = 'bottomleft')
      )  
      
  })
  
  
  # ... OBSERVE ---- 
  observe({
    if ("Clear All" %in% input$tsa) {
      # choose all the choices _except_ "Select All"
      selected_choices <- ""
      updateSelectInput(session, "tsa", selected = selected_choices)
    }
  })
  
  # ... MODALS ----
  # Terms and Cond
  termsAndCondsModal = modalDialog(
    title = HTML("<h2><b>Terms and Conditions</b></h2>"),
    easyClose = TRUE,
    fade = FALSE,
    HTML("<h4><b>Conditions for the Release of Fisher Equivalent Territory Areas Data
B.C. Ministry of Forests, Lands and Natural Resource Operations
Forest Analysis and Inventory Branch
</b></h4>
          The release of Fisher Equivalent Territory Areas Data requires that your agency agrees
          to the following terms and conditions prior to completing this transaction.
          <ol>
          <li>  The data cannot be used for purposes other than those negotiated between the Forest Analysis and Inventory Branch (Branch) and the agency.</li>
          <li>  The data cannot be distributed or sold to a third party or retained by the agency as a proprietary asset.</li>
          <li>  Any models/analyses developed from the use of the data may be requested for review by the Branch and may be put into the public domain.</li>
          <li>  The data will be returned to the Branch at the completion of the project, if requested.</li>
          <li>  Although these data have been carefully validated, some data quality/completion issues may still exist. The Branch cannot be held liable for the state of the data.</li>
          <li>  The Branch is not obliged to act on, or make a standard, the results from the agency's use/interpretation of the data.</li>
          <li>  The Branch is not held liable from the agency's use/interpretation of the data.</li>
          <li>  The agency is responsible for these terms and conditions for all its staff, associates and sub-contractors.</li>
          </ol>")
  )
  
  # disable the downdload button on page load
  shinyjs::disable("downloadData")
  
}