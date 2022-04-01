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
        Columbian = {
          fetaPolySf <- fish.constraints.columbia
          fetaLU <- aoi.columbia
        }
      )
      
      if(!is.null(input$aoi)){
        fetaPolySf <- fetaPolySf[unique(fetaLU[fetaLU$aoi %in% input$aoi,]$fid),]
      }else{
        fetaPolySf
      }
    }else{
      NULL
    }
    
  })
  
  #Filter based on habitat selection
  fetaPolyRender <-reactive({
    fetaPolyAOI() %>% filter(p_occ >= input$threshold_p_occ & hab_den >= (input$threshold_hab_den/100)*3000 & hab_mov >= (input$threshold_hab_mov/100)*3000 & hab_cav >= (input$threshold_hab_cav/100)*3000 & hab_cwd >= (input$threshold_hab_cwd/100)*3000 & hab_rus >= (input$threshold_hab_rus/100)*3000 & ogma >= input$threshold_ogma & defer >= input$threshold_defer)
  })
  
  ## render the summary table
  output$fetaSummary<-renderTable({
    summaryTable<-fetaPolyRender() %>% 
                    as.data.frame %>% 
                    summarise(Total =n(), abund=sum(abund, na.rm = T), n_fish= sum(nfish, na.rm = T), thlb = sum(thlb, na.rm = T)) %>%
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
    paste0("Rel. Prob. Occupancy >",input$threshold_p_occ, ", Denning >",input$threshold_hab_den, "%, Movement >", input$threshold_hab_mov,
           "%, Cavity >", input$threshold_hab_cav, "%, Rust >", input$threshold_hab_rus, "% CWD >", input$threshold_hab_cwd, "%")
  })
  
  output$aoiSelected<-renderText({
    if(is.null(input$aoi)){
      paste("<B>All</B>")
      }else{
        paste("<B>", paste(input$aoi, sep = "", collapse = ", "), "</B>")}
    })
  
  output$rsHabitat<-renderTable({
    data.table(p_occ = input$threshold_p_occ, Denning = input$threshold_hab_den, Movement = input$threshold_hab_mov,
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
      shp_files <- list.files(temp_shp, "^[feta]",
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
             Columbian = {
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
             tagList(
               h1(
                div(style="display:inline-block;",img(src="www/img/FCP.png", height = 150, width = 150,style="left;"),
                    h1("FETA Mapper"), h4("A tool to visualize and download information about fisher territories")),
                
               ),
               #HTML('<h2>Welcome to the FETA Mapper!</h2>'),
                #     img(src = "www/img/FCP.png"),
              HTML("<h3>Overview</h3><p>Fishers are territorial animals that require specific forest structures in their territories to meet their life history needs. While each individual fisher will have a unique territory shape and location (i.e., home range), we cannot identify the location of each fisher territory across British Columbia (BC). Therefore, we used a spatial grid of hexagons with an area of 30 km<sup>2</sup> to represent a fisher equivalent territory area (FETA). A 30 km<sup>2</sup> size was used because it approximates the measured average home range size of fisher in BC (Rich Weir, pers. comm.). These hexagons represent a territory area required by fisher, within which we can estimate habitat characteristics and assess whether they meet fisher needs.</p><br>")
            )
            },
           abund = {
             tagList(
               HTML("<h3>Estimated Abundance</h3><p>Estimated abundance is the number of fishers occupying a FETA after an adjustment using the relative probability of occupancy. Two sources of information were used to estimate fisher abundance within a FETA: the relative probability of territory occupancy (Weir and Corbould 2010) and the 2004 estimate of fisher habitat capability rating (provided by the Fisher team).</p><br>"),
               HTML("<h4>Attribute Name</h4> <p><i>abund</i></p>"),
               HTML("<h4>Technical</h4><p>The estimate of fisher abundance in a FETA then follows as:</p>"),
               withMathJax("$$\\text{abund = p_occ (nfish)}$$"),
               HTML("<p>Where, abund is the abundance of fisher in a FETA, p_occ is the relative probability of occupancy (Weir and Corbould 2010) , nfish is the density or number of fisher per FETA (30 km<sup>2</sup>)</p>"),
               HTML("<h4>References</h4><p><li>Weir, R.D., Corbould, F.B. 2010. Factors Affecting Landscape Occupancy by Fishers in North-Central British Columbia. Journal of Wildlife Management 74(3):405–410; 2010; DOI: 10.2193/2008-579</li></p>")
             )
             },
           abund_pot = {
             tagList(
               HTML("<h3>Potential Abundance</h3><p>Potential abundance is the number of fishers occupying a FETA that is adjusted using a maximally estimated relative probability of occupancy. Two sources of information were used to estimate fisher abundance within a FETA: the relative probability of territory occupancy (Weir and Corbould 2010) and the 2004 estimate of fisher habitat capability rating (provided by the Fisher team).</p>"),
               HTML("<h4>Attribute Name</h4><p><i>abund_pot</i></p>"),
               HTML("<h4>Technical</h4><p>Potential abundance is estimated as follows:</p>"),
               withMathJax("$$\\text{abund_pot = p_occ_max (nfish)}$$"),
               HTML("<p>Where, abund_pot is the abundance of fisher in a FETA, p_occ_max is the relative probability of occupancy calculated with the assumption that there are no hectares less than 12 years of age (Weir and Corbould 2010) , nfish is the density or number of fisher per FETA (30 km<sup>2</sup>)</p>"),
               HTML("<h4>References</h4><p><li>Weir, R.D., Corbould, F.B. 2010. Factors Affecting Landscape Occupancy by Fishers in North-Central British Columbia. Journal of Wildlife Management 74(3):405–410; 2010; DOI: 10.2193/2008-579</li></p>")
            )
           },
           nfish = {
             tagList(
               HTML("<h3>Density</h3><p>The density of fisher or the number of fisher per FETA (30 km<sup>2</sup>). A fisher habitat capability rating was used to adjust a fisher density estimates.</p>"),
               HTML('<blockquote cite="https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/wildlife/wildlife-habitats/wildlife-habitat-mapping"> A habitat capability rating is defined as the ability of the habitat, under the optimal natural conditions for a species to provide its life requisites, irrespective of the current condition of the habitat.</blockquote>'),
               HTML("<h4>Attribute Name</h4><p><i>nfish</i></p>"),
               HTML("<h4>Technical</h4><p>Fisher density within a FETA was calculated by habitat capability rating based on the adjustment provided below. The area within a FETA by each of the fisher habitat capability rating was then multiplied by the respective fisher density estimate and then summed across the number of fisher capability ratings to estimate the FETA level fisher density.</p>"),
               HTML('<table><thead><tr class="header"><th align="left">Fisher habitat capability rating</th><th align="left">Habitat Capability Adjustment (%)</th><th align="left">Fisher Density (N per 1000 km<sup>2</sup>)</th><th align="right">Fisher Density (N per FETA 30 km<sup>2</sup>)</th></tr></thead>
                     <tbody>
<tr class="odd"><td align="left">Very High</td><td align="right">100.0</td><td align="left">18.10</td><td align="right">0.5430</td></tr>
<tr class="even"><td align="left">High</td><td align="right">71.3</td><td align="left">12.90</td><td align="right">0.3870</td></tr>
<tr class="odd"><td align="left">Medium</td><td align="right">42.8</td><td align="left">7.75</td><td align="right">0.2325</td></tr>
<tr class="even"><td align="left">Low</td><td align="right">17.4</td><td align="left">3.15</td><td align="right">0.0945</td></tr>
</tbody></table>')
               
             )
             },
           p_occ={
             tagList( 
               HTML("<h3>Relative Probability of Occupancy</h3><p>The relative probability that a FETA would be occupied by a fisher.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>p_occ</i></p>"),
               HTML("<h4>Technical</h4><p>The relative probability of occupancy model (Weir and Corbould 2010) was estimated using:</p>"),
               withMathJax("$$\\text{p_occ}=(exp(-0.219*openess)/(1+exp(-0.219*openess)))/0.5$$"), 	#rel.prob.occupancy= (exp(-0.219*openess)/(1+exp(-0.219*openess)))/0.5
               HTML("<br><p>Where, openness is the percentage of a FETA that is open, which includes permanently open areas (i.e., wetlands, lakes, non-vegetated, etc) and forest less than or equal to 12 years old (cutblocks and fire origin stands). Permanently open areas and forest age were queried from the Vegetation Resource Inventory projected to the year 2020.</p>"),
               HTML("<h4>References</h4><li>Weir, R.D., and  Corbould, F.B. 2010. Factors Affecting Landscape Occupancy by Fishers in North-Central British Columbia. Journal of Wildlife Management 74(3):405–410; 2010; DOI: 10.2193/2008-579</li>")
             )
           },
           hab_den = {
             tagList(
               HTML("<h3>Denning</h3><p>The number of hectares within a FETA classified as reproductive denning habitat as per habitat category descriptions (at stand level) found here. In general, denning habitat is characterized by forest structure with large diameter, older <i>Populus</i> spp.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>hab_den</i></p>"),
               HTML(paste0('<h4>Technical</h4><p>The <a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>',
                           " was used to estimate this forest structure. The various queries are presented below by habitat zone.</p>
<li><b>SBS-wet:</b> (((SPECIES_CD_1 LIKE 'AC%')  or (SPECIES_CD_2 LIKE 'AC%') or ( SPECIES_CD_3 LIKE 'AC%')) or ((SPECIES_CD_1 LIKE 'S%') and (SPECIES_CD_2 IS NULL))) and (PROJ_AGE_1>=125) and (CROWN_CLOSURE>=30) and (QUAD_DIAM_125>=28.5) and (BASAL_AREA>=29.75) and (bec_zone_code = 'SBS') and (bec_subzone in('wk','mk','mm','mw'))</li>
<li><b>SBS-dry:</b> (((SPECIES_CD_1 LIKE 'AC%')  or (SPECIES_CD_2 LIKE 'AC%') or ( SPECIES_CD_3 LIKE 'AC%')) or ((SPECIES_CD_1 LIKE 'S%') and (SPECIES_CD_2 IS NULL))) and (PROJ_AGE_1>=125) and (CROWN_CLOSURE>=20) and (QUAD_DIAM_125>=28) and (BASAL_AREA>=28) and (bec_zone_code = 'SBS') and (bec_subzone in ('dw','dh','dk'))</li>
<li><b>Dry Forest:</b> ((SPECIES_CD_1 LIKE 'A%')  or (SPECIES_CD_2 LIKE 'A%')) and PROJ_AGE_1>=135) or (((SPECIES_CD_1 LIKE 'F%') and (SPECIES_CD_2  IS NULL)) and PROJ_AGE_1>=207 and CROWN_CLOSURE>=20 and QUAD_DIAM_125>=34.3) and ((bec_zone_code = 'SBPS' and bec_subzone in('xc','mc','dc','mk')) or (bec_zone_code = 'IDF' and bec_subzone in('dk','dc','mw','dw','ww')) or (bec_zone_code = 'MS' and bec_subzone in('xc','xk','dv','dm', 'dk', 'dc')))</li>
<li><b>Boreal:</b> (((SPECIES_CD_1 LIKE 'AC%')  or (SPECIES_CD_2 LIKE 'AC%') or ( SPECIES_CD_3 LIKE 'AC%')) and (PROJ_AGE_1>=88) and (QUAD_DIAM_125>=19.5) and (PROJ_HEIGHT_1>=19)) or (((SPECIES_CD_1 LIKE 'AT%')  or (SPECIES_CD_2 LIKE 'AT%') or ( SPECIES_CD_3 LIKE 'AT%')) and (PROJ_AGE_1>=98) and (QUAD_DIAM_125>=21.3) and (PROJ_HEIGHT_1>=22.8)) and ((bec_zone_code = 'BWBS' and bec_subzone in ('dk', 'mw', 'wk')) or (bec_zone_code ='SBS' and bec_subzone = 'wk' and bec_variant ='2'))</li>"))
             )
           },
           hab_mov={
             tagList(
               HTML("<h3>Movement</h3><p>The number of hectares within a FETA classified as movement as per habitat category descriptions (at stand level) found here. Movement habitat is required to safely travel between important habitats within and between territories. In general, movement habitat is linked to forest structure characterized by high crown cover.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>hab_mov</i></p>"),
               HTML(paste0('<h4>Technical</h4><p>The <a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>',
                           " was used to estimate this forest structure. The various queries are presented below by habitat zone.</p>
                    <li><b>All:</b>((crown_closure + shrub_crown_closure >= 50 and crown_closure > 30) or crown_closure >= 50)</li>"))
             )
           },
           hab_rus={
             tagList(
               HTML("<h3>Rust</h3><p>The number of hectares within a FETA classified as Resting Habitat: Rust broom sites as per habitat category descriptions (at stand level) found here. In general, mature spruce (<i>Picea</i> spp.) forest structure characterizes the presence of rust broom.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>hab_rus</i></p>"),
               HTML(paste0('<h4>Technical</h4><p>The <a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>',
                           " was used to estimate this forest structure. The various queries are presented below by habitat zone.</p>
                    <li><b>SBS-wet:</b> ((SPECIES_CD_1 LIKE 'S%')  or (SPECIES_CD_2 LIKE 'S%') or ( SPECIES_CD_3 LIKE 'S%')) and (CROWN_CLOSURE>=30) and (QUAD_DIAM_125>=22.7) and (BASAL_AREA>=35) and (PROJ_HEIGHT_1>=23.7) and bec_zone_code = 'SBS' and bec_subzone in('wk','mk','mm','mw')</li>
<li><b>SBS-dry:</b> ((SPECIES_CD_1 LIKE 'S%')  or (SPECIES_CD_2 LIKE 'S%') or ( SPECIES_CD_3 LIKE 'S%')) and (PROJ_AGE_1>=72) and (CROWN_CLOSURE>=25) and (QUAD_DIAM_125>=19.6) and (BASAL_AREA>=32) and bec_zone_code = 'SBS' and bec_subzone in ('dw','dh','dk')</li>
<li><b>Dry Forest:</b> (((SPECIES_CD_1 LIKE 'S%')  or (SPECIES_CD_2 LIKE 'S%') or ( SPECIES_CD_3 LIKE 'S%')) and (PROJ_AGE_1>=83) and (CROWN_CLOSURE>=40) and (QUAD_DIAM_125>=20.1)) and ((bec_zone_code = 'SBPS' and bec_subzone in('xc','mc','dc','mk')) or (bec_zone_code = 'IDF' and bec_subzone in('dk','dc','mw','dw','ww')) or (bec_zone_code = 'MS' and bec_subzone in('xc','xk','dv','dm', 'dk', 'dc')))</li>
<li><b>Boreal:</b> ((((SPECIES_CD_1 LIKE 'SW%')  or (SPECIES_CD_2 LIKE 'SW%') or (SPECIES_CD_3 LIKE 'SW%')) and (PROJ_AGE_1>=78) and (CROWN_CLOSURE>=50) and (QUAD_DIAM_125>=18.5) and (PROJ_HEIGHT_1>=19) and (BASAL_AREA>=31.4 )) or (((SPECIES_CD_1 LIKE 'SB%')  or (SPECIES_CD_2 LIKE 'SB%') or ( SPECIES_CD_3 LIKE 'SB%')) and (PROJ_AGE_1>=68) and (CROWN_CLOSURE>=35) and (QUAD_DIAM_125>=17) and (PROJ_HEIGHT_1>=14.8))) and ((bec_zone_code = 'BWBS' and bec_subzone in ('dk', 'mw', 'wk')) or (bec_zone_code ='SBS' and bec_subzone = 'wk' and bec_variant ='2'))</li>"))
               
             )
           },
           hab_cwd={
             tagList(
               HTML("<h3>CWD</h3><p>The number of hectares within a FETA classified as resting habitat: coarse woody habitat as per habitat category descriptions (at stand level) found here. In general, older- large diameter forest structure characterizes the presence of coarse woody habitat.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>hab_cwd</i></p>"),
               HTML(paste0('<h4>Technical</h4><p>The <a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>',
                           " was used to estimate this forest structure. The various queries are presented below by habitat zone.</p>
                    <li><b>SBS-wet:</b> (PROJ_AGE_1>=135) and (QUAD_DIAM_125>=22.7) and (PROJ_HEIGHT_1>=23.7) and bec_zone_code = 'SBS' and bec_subzone in('wk','mk','mm','mw')</li>
<li><b>SBS-dry:</b> (PROJ_AGE_1>=135) and (CROWN_CLOSURE>=25) and (QUAD_DIAM_125>=22.7) and (PROJ_HEIGHT_1>=23.7) and bec_zone_code = 'SBS' and bec_subzone in ('dw','dh','dk')</li>
<li><b>Dry Forest:</b> ((((SPECIES_CD_1 LIKE 'S%' and SPECIES_PCT_1>=25) or (SPECIES_CD_2 LIKE 'S%' and SPECIES_PCT_2>=25) or (SPECIES_CD_3 LIKE 'S%' and SPECIES_PCT_3>=25)) or ((SPECIES_CD_1 LIKE 'AT%' and SPECIES_PCT_1>=25) or (SPECIES_CD_2 LIKE 'AT%' and SPECIES_PCT_2>=25) or (SPECIES_CD_3 LIKE 'AT%' and SPECIES_PCT_3>=25))) and (PROJ_AGE_1>=100)) and ((bec_zone_code = 'SBPS' and bec_subzone in('xc','mc','dc','mk')) or (bec_zone_code = 'IDF' and bec_subzone in('dk','dc','mw','dw','ww')) or (bec_zone_code = 'MS' and bec_subzone in('xc','xk','dv','dm', 'dk', 'dc')))</li>
<li><b>Boreal:</b> ((PROJ_AGE_1>=78) and (QUAD_DIAM_125>=18.1) and (PROJ_HEIGHT_1>=19) and (CROWN_CLOSURE>=60)) and ((bec_zone_code = 'BWBS' and bec_subzone in ('dk', 'mw', 'wk')) or (bec_zone_code ='SBS' and bec_subzone = 'wk' and bec_variant ='2'))</li>"))
               
             )
           },
           hab_cav={
             tagList(
               HTML("<h3>Cavity</h3><p>The number of hectares within a FETA classified as resting habitat: cavity sites as per habitat category descriptions (at stand level) found here. These include secure locations required during daily activity bouts. In general, tall and large diameter <i>Populus</i> spp. forest structure characterizes cavity sites.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>hab_cav</i></p>"),
               HTML(paste0('<h4>Technical</h4><p>The <a href="https://catalogue.data.gov.bc.ca/dataset/vri-2020-forest-vegetation-composite-rank-1-layer-r1-" target="_blank">Vegetation Resource Inventory</a>',
               " was used to estimate this forest structure. The following queries are used to select cavity habitat within the VRI:<br>
             <li><b>SBS wet:</b> ((SPECIES_CD_1 LIKE 'A%')  or (SPECIES_CD_2 LIKE 'A%') or ( SPECIES_CD_3 LIKE 'A%')) and (CROWN_CLOSURE>=25) and (QUAD_DIAM_125>=30) and (BASAL_AREA>=32) and (PROJ_HEIGHT_1>=35) and bec_zone_code = 'SBS' and bec_subzone in('wk','mk','mm','mw') <br>
             <li><b>SBS dry:</b> (((SPECIES_CD_1 LIKE 'A%')  or (SPECIES_CD_2 LIKE 'A%') or (SPECIES_CD_3 LIKE 'A%')) and PROJ_HEIGHT_1>=35 and BASAL_AREA>=32) and bec_zone_code = 'SBS' and bec_subzone in ('dw','dh','dk') <br>")),
               
             )
           },
           thlb = {
             tagList(
               HTML("<h3>Timber Harvesting Land base</h3><p>An estimate of the number of hectares available for timber harvesting after netting out various harvesting constraints.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>thlb</i></p>")
             )
           },
           ogma={
             tagList(
               HTML("<h3>Old Growth Management Areas</h3><p>The number of hectares in old growth management areas (OGMA) that are legally binding and are identified during landscape unit planning or an operational planning process. OGMAs in combination with other areas where forestry development is prevented or constrained, are used to achieve biodiversity targets.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>ogma</i></p>"),
               HTML(paste0("<h4>Technical</h4><p>A spatial later of current OGMAs can found on ", 
                    '<a href="https://catalogue.data.gov.bc.ca/dataset/old-growth-management-areas-legal-current" target="_blank">DataBC</a></p>'))
               
             )
           },
           defer ={
             tagList(
               HTML("<h3>Old Growth Deferrals</h3><p>The number of hectares that are priority areas for deferrals of harvesting in areas of old growth. It combines the Prioritised Big-treed Old Growth (Map 3), Remnant Old Ecosystems (Map 4) and Ancient Forests (Map 5) maps.</p>"),
               HTML("<h4>Attribute Name</h4><p><i>defer</i></p>"),
               HTML(paste0("<h4>Technical</h4><p>The data are provided by the Old Growth Technical Advisory Panel and definitions are outlined in the panel’s report. For more information ",
               '<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/old-growth-forests/old-growth-maps" target="_blank">go here</a></p>'))
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
    HTML("<h4><b>Conditions for the Release of Fisher Equivalent Territory Areas Data</b></h4>
<h5>B.C. Ministry of Forests, Lands and Natural Resource Operations
Forest Analysis and Inventory Branch</h5>
          <p>The release of Fisher Equivalent Territory Areas Data requires that your agency agrees
          to the following terms and conditions prior to completing this transaction.</p>
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