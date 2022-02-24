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

  fetaPolyAOI <-reactive({
    req(input$pop)
    if(!is.null(input$pop)){
      switch(input$pop,
        Boreal = {
          fetaPolySf <- fish.constraints.boreal 
          fetaLU <- aoi.boreal
        },
        Columbia = {
          fetaPolySf <- fish.constraints.columbia
          fetaLU <- aoi.columbia
        }
      )
      
      if(!is.null(input$aoi)){
        fetaPolySf <- fetaPolySf[fetaLU[fetaLU$aoi %in% input$aoi,]$fid,]
      }else{
        fetaPolySf
      }
    }else{
      NULL
    }
    
  })
  
  #Filter based on habitat selection
  fetaPolyRender <-reactive({
    fetaPolyAOI() %>% filter(hab_den >= (input$threshold_hab_den/100)*3000 & hab_mov >= (input$threshold_hab_mov/100)*3000 & hab_cav >= (input$threshold_hab_cav/100)*3000 & hab_cwd >= (input$threshold_hab_cwd/100)*3000 & hab_rus >= (input$threshold_hab_rus/100)*3000 & ogma >= input$threshold_ogma & defer >= input$threshold_defer)
  })
  
  ## render the summary table
  output$fetaSummary<-renderTable({
    summaryTable<-fetaPolyRender() %>% 
                    as.data.frame %>% 
                    summarise(Total =n(), abund=sum(abund), n_fish= sum(nfish), thlb = sum(thlb)) %>%
                    mutate(abund=as.integer(abund), n_fish = as.integer(n_fish), thlb = as.integer(thlb))
    
    colnames(summaryTable) <- c("Total", "Estimated N", "Potential N", "THLB (ha)")
    summaryTable<-sapply(summaryTable[1:4,], FUN=function(x) prettyNum(x, big.mark=","))
    head(summaryTable, 1)
  })
  
  ## render the old growth table
  
  output$oldGrowthSummary<-renderTable({
    summaryTable<-fetaPolyRender() %>% 
      as.data.frame %>% 
      summarise(ogma = as.integer(sum(ogma)), og_dc = as.integer(sum(dc_ogma)), defer = sum(defer), defer_dc = sum(dc_defr))
    
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
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
             layers = c("pub:WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW"),
             group = "OGMA-legal",
             options = leaflet::WMSTileOptions(
               transparent = TRUE,
               format = "image/png",
               info_format = "text/html",
               tiled = T
             ))%>%
      addWMS(baseUrl = "https://openmaps.gov.bc.ca/geo/ows/",
             layers = c("pub:WHSE_LAND_USE_PLANNING.RMP_OGMA_NON_LEGAL_CURRENT_SVW"),
             group = "OGMA-nonlegal",
             options = leaflet::WMSTileOptions(
               transparent = TRUE,
               format = "image/png",
               info_format = "text/html",
               tiled = T
             ))%>%
      addLayersControl(
        baseGroups = "WorldImagery",
        overlayGroups = c("FETA","TSA","Parks","FADM", "WHA", "UWR", "OGMA-legal", "OGMA-nonlegal"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(c("TSA", "FADM", "WHA", "UWR", "OGMA-legal", "OGMA-nonlegal") ) %>%
      addScaleBar(position = "topleft")
      
  })
 
  output$fetaData<-DT::renderDataTable({
    a<-st_drop_geometry(fetaPolyRender()) %>%
      select(fid, abund, nfish, hab_den, hab_mov, hab_cav, hab_rus, hab_cwd, ogma, defer) %>%
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
  
  output$aoiSelected<-renderText({
    if(is.null(input$aoi)){
      paste("<B>All</B>")
      }else{
        paste("<B>", paste(input$aoi, sep = "", collapse = ", "), "</B>")}
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
    req(input$pop)
    req(input$colorFilt)
    
    bbox<-st_bbox(fetaPolyRender())
    leafletProxy('map')  %>% 
      clearGroup('FETA') %>%
      fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>% 
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
  
  observeEvent(input$pop, {
    if(!is.null(input$pop)){
      switch(input$pop,
             Boreal = {
               fetaLU <- aoi.boreal
             },
             Columbia = {
               fetaLU <- aoi.columbia
             }
      )
    }
    updateSelectizeInput(session, "aoi",
                         choices = as.character(unique(fetaLU$aoi)),
                         server = TRUE)
  })
  
  output$ui_overview <- renderUI({
    req(input$attribute_def)
    switch(input$attribute_def,
           Overview = {
             tagList(HTML("<h3>Overview</h3> Fishers are territorial animals that require specific forest structures to meet their life history needs. While each individual fisher will have a unique territory shape and location (i.e., home range), we cannot identify the location of each fisher territory across BC. Therefore, we used a spatial grid of hexagons with an area of 30 km2 to represent a fisher equivalent territory area (FETA). We used a 30 km2 size because it approximates the measured average home range size of fisher in BC (Rich Weir, pers. comm.). These hexagons represent a territory area required by fisher, within which we can estimate habitat characteristics and assess whether they meet fisher needs.<br>")
                     )
            },
           abund = {
             tagList(HTML("<h3>Estimated Abundance</h3> Fisher abundance is the estimated number of fisher occupying an area of interest. Two sources of information were used to estimate fisher abundance within a FETA: the relative probability of territory occupancy using the model developed by Weir and Corbould (2010) and the fisher habitat capability rating (a 2004 spatial file provided by Rich Weir).<br>"),
                     HTML("<br><h4>Technical</h4> <br> The estimate of fisher abundance in a FETA then follows as: <br>"),
                     withMathJax("$$\\text{abund=prob_}occ_FETA*∑_r^R▒〖(〖AreaFisherCapability〗_r*〖Density〗_r)〗_FETA$$"),
                     HTML("<br> Where, N is the abundance of fisher in a FETA, rel.prob.occupancy is the relative probability of occupancy (Weir and Corbould 2010) , AreaFisherCapability is the area (in 1000 km2) of the rth fisher capability rating and Density is the density estimate for the rth fisher capability rating (R is the number of fisher capability ratings in a FETA).<br>"),
                     HTML("<br><h4>Data Reference</h4><br>abund")
             )
             },
           abund_pot = {
             tagList(
               HTML("<h3>Potential Abundance</h3>"),
               HTML("<br><h4>Technical</h4><br> <br>"),
               HTML("<br><h4>Data Table<h4><br>abund_pot")
             )
           },
           nfish = {
             tagList(
               HTML("<h3>Density</h3> The density of fisher or number of fisher per FETA (30 km2). We used a fisher capability rating to adjust a maximum fisher density estimate of 16.3 fisher per 1000 km2 (taken from the Williston area of the B.C). “A habitat capability rating is defined as the ability of the habitat, under the optimal natural conditions for a species to provide its life requisites, irrespective of the current condition of the habitat” (https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/wildlife/wildlife-habitats/wildlife-habitat-mapping)."),
               HTML("<br><h4>Technical</h4> <br> Fisher density was calculated by habitat capability rating based on the capability rating adjustment provided in Table 1. The proportion of area of a FETA within each fisher capability rating was then multiplied by the respective fisher density estimate found in Table 1 and then summed across the number of fisher capability ratings to estimate fisher density within a FETA <br>"),
               HTML("<br><h4>Data Table<h4><br>nfish")
             )
             },
           p_occ={
             tagList( 
               HTML("<h3>Relative Probability of Occupancy</h3>"),
               HTML("<br><h4>Technical</h4> <br> The relative probability of occupancy model (Weir and Corbould 2010) was estimated using: <br>"),
               withMathJax("$$\\text{prob_occ}=(exp(-0.219*openess)/(1+exp(-0.219*openess)))/0.5$$"), 	#rel.prob.occupancy= (exp(-0.219*openess)/(1+exp(-0.219*openess)))/0.5
               HTML("<br> Where, openness is the percentage of a FETA that is open, which includes permanently open areas (i.e., wetlands, lakes, non-vegetated, etc) and forest less than or equal to 12 years old (cutblocks and fire origin stands). Permanently open areas and forest age were queried from the Vegetation Resource Inventory projected to the year 2020."),
               HTML("<br><h4>Data Reference<h4><br>p_occ")
             )
           },
           hab_den = {
             tagList(
               HTML("<h3>Denning</h3>"),
               HTML("<br><h4>Technical</h4> <br>"),
               HTML("<br><h4>Data Table<h4><br>hab_den")
             )
           },
           hab_mov={
             tagList(
               HTML("<h3>Movement</h3> Required to safely travel between important habitats within and between territories <br>"),
               HTML("<br><h4>Technical</h4><br>"),
               HTML("<br><h4>Data Table<h4><br>hab_mov")
             )
           },
           hab_rus={
             tagList(
               HTML("<h3>Rust</h3>"),
               HTML("<br><h4>Technical</h4> <br>"),
               HTML("<br><h4>Data Table<h4><br>hab_rus")
             )
           },
           hab_cwd={
             tagList(
               HTML("<h3>CWD</h3>"),
               HTML("<br><h4>Technical</h4> <br>"),
               HTML("<br><h4>Data Table<h4><br>hab_cwd")
             )
           },
           hab_cav={
             tagList(
               HTML("<h3>Cavity</h3> The amount of area in hectares within a FETA classified as Resting Habitat: Cavity sites as per Habitat Category Descriptions (at stand level). These include secure locations required during daily activity bouts. In general, tall and large diameter Populus Spp forest structure characterizes cavity sites."),
               HTML("<br><h4>Technical</h4><br>The provincial forest inventory ("), 
               HTML('<a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>'),
               HTML(") was used to estimate this forest structure. The following queries are used to select cavity habitat within the VRI:<br>
             <li> SBS wet: ((SPECIES_CD_1 LIKE 'A%')  or (SPECIES_CD_2 LIKE 'A%') or ( SPECIES_CD_3 LIKE 'A%')) and (CROWN_CLOSURE>=25) and (QUAD_DIAM_125>=30) and (BASAL_AREA>=32) and (PROJ_HEIGHT_1>=35) and bec_zone_code = 'SBS' and bec_subzone in('wk','mk','mm','mw') <br>
             <li> SBS dry: (((SPECIES_CD_1 LIKE 'A%')  or (SPECIES_CD_2 LIKE 'A%') or (SPECIES_CD_3 LIKE 'A%')) and PROJ_HEIGHT_1>=35 and BASAL_AREA>=32) and bec_zone_code = 'SBS' and bec_subzone in ('dw','dh','dk') <br>"),
               HTML("<br><h4>Data Table<h4><br>hab_cav")
             )
           },
           thlb = {
             tagList(
               HTML("<h3>Timber Harvesting Land base</h3>"),
               HTML("<br><h4>Data Table<h4><br>thlb")
             )
           },
           ogma={
             tagList(
               HTML("<h3>Old Growth Management Areas</h3>"),
               HTML("<br><h4>Technical</h4> <br>"),
               HTML("<br><h4>Data Table<h4><br>ogma")
             )
           },
           defer ={
             tagList(
               HTML("<h3>Old Growth Deferrals</h3>"),
               HTML("<br><h4>Technical</h4><br>"),
               HTML("<br><h4>Data Table<h4><br>defer")
             )
           }
           )
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