# tribal data app


# 1 - load packages -----------------------------------------------------------
library(shiny)

# Mapping and GIS operations
library(sf)
library(leaflet)
library(htmlwidgets)
# library(geojsonsf)
# library(rmapshaper)
# library(htmltools)
# library(tmap)

# shiny stuff
library(shinycssloaders)
library(DT)
library(shinyWidgets)
library(shinyjs)

library(glue)



# 2 - set defaults ------------------------------------------------------------
## Define coordinate systems to use for transformations
projected_crs <- 3310 # see: https://epsg.io/3310 
# other options: 26910 see: https://epsg.io/26910
# resources: 
# https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline

geographic_crs <- 4269 # see: https://epsg.io/4269
# see: https://epsg.io/4326

## choices
list_tribal_data <- list('Bureau of Indian Affairs' = 'BIA_National_LAR.gpkg',
                         'Census Bureau' = 'native_areas_census.gpkg')

unlist(unname(list_tribal_data))



# 3 - define UI ---------------------------------------------------------------
ui <- navbarPage(title = "California Tribal Data", # theme = shinythemes::shinytheme('flatly'),
                 
                 ## A - Maps / Tabular Data Tab ----
                 tabPanel('Map', icon = icon('map'),
                          
                          ### 1 - setup ----
                          useShinyjs(),
                          tags$head(
                              # Code to resize main panel to 100% width after show/hide sidebar button clicked
                              # see: https://stackoverflow.com/a/33424463
                              tags$script(
                                  HTML("
                                $(document).ready(function(){
                                  // Mark columns we want to toggle
                                  $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
                                  $('body').find('div [class=col-sm-8]').addClass('mainPanel');
                                })
                    
                    
                                Shiny.addCustomMessageHandler ('resize',function (message) {
                                  $('.sidebarPanel').toggle();
                                  $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
                                  $(window).trigger('resize')
                                });
                    
                               ")
                              ),
                              # define button style (background color and font color)
                              tags$style(".buttonstyle{background-color:#f2f2f2;} .buttonstyle{color: black;}")
                          ), 
                          
                          
                          # Sidebar layout with input and output definitions
                          sidebarLayout(
                              ### 2 - Sidebar ----
                              # div(id ="Sidebar", sidebarPanel(
                              sidebarPanel(
                                  # Inputs:
                                  selectInput(inputId = 'tribal_boundaries_selected',
                                              label = 'Select Tribal Boundaries:',
                                              choices = names(list_tribal_data)#,
                                              # selected = initial_selected_city
                                  ), # ifelse (initial_zoom_level == 'State', 'Statewide', initial_selected_city)),
                                  hr(style = "border: 1px solid darkgrey"),
                                  h4('Mobile Home Parks:')
                              ), # end of sidebarPanel
                              
                              ### 3 - Main panel ----
                              # Main panel for displaying outputs 
                              mainPanel(
                                  fluidRow(
                                      div(style = "display:inline-block;vertical-align:top;",
                                          # actionButton("toggleSidebar", "Toggle sidebar"),
                                          actionButton("showpanel", "Show/Hide Sidebar", 
                                                       class = "buttonstyle", icon = icon('bars'), 
                                                       style = 'padding:4px; font-size:80%')#, # 'display:inline;'), 
                                          
                                      ),
                                      fluidRow(
                                          leafletOutput(outputId = 'map1', height = 400) %>% withSpinner(color="#0dc5c1")
                                      ),
                                  ),
                              ) # end of mainPanel
                          ) # end of sidebarLayout
                 ), ## end of tabPanel (for Map)
                 
                 
                 ## B - Analysis Tab ----
                 tabPanel('Analysis', 
                          icon = icon('chart-bar'),
                          fluidRow(
                              
                          )
                 ), ## end of tabPanel (for Analysis)
                 
                 
                 ## C - Background Info Tab ---- 
                 tabPanel('Background Information', 
                          icon = icon('info-circle'), # icon = icon('book-reader')
                          fluidRow(
                              
                          )
                 ) ## end of tabPanel (for Info)
                 
) # end of UI




# 4 - define server logic -----------------------------------------------------
server <- function(input, output, session) {
    
    ## toggle sidebar panel ----
    observeEvent(input$showpanel,{
        session$sendCustomMessage(type = 'resize', message = 1)
    })
    
    ## read CA boundary ----
    sf_ca_boundary <- st_read('data_processed/ca_boundary.gpkg')
    
    ## read tribal boundaries ----
    sf_tribal_boundaries <- reactive(
        st_read(paste0('data_processed/',
                       list_tribal_data[[input$tribal_boundaries_selected]]))
    ) %>%
        debounce(1000) %>%
        {.}
    
    ## create map ----
    output$map1 <- renderLeaflet({
        # withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
        
        # bounds_selected <- attributes(st_geometry(rb_boundary %>% 
        #                                               filter(rb == input$regional_board)) %>% 
        #                                   st_transform(crs = geographic_crs))$bbox
        
        # bounds_selected <- attributes(st_geometry(sites_data()) %>% 
        #                                   st_transform(crs = geographic_crs))$bbox
        
        # create the new (empty) map
        l_map1 <- leaflet()
        
        # set drawing order
        l_map1 <- l_map1 %>% 
            addMapPane('ca_boundary', zIndex = 410) %>% 
            addMapPane("tribal_boundaries", zIndex = 420) %>%
            # addMapPane("303d_lines", zIndex = 430) %>% 
            # addMapPane("303d_polygons", zIndex = 440) %>% 
            # addMapPane("region_polygon", zIndex = 450) %>%
            # addMapPane('zip_polygon', zIndex = 460) %>%
            # addMapPane('ww_facilities', zIndex = 470) %>% 
            {.}
        
        # Basemap Options
        basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
        for (provider in basemap_options) {
            l_map1 <- l_map1 %>% 
                addProviderTiles(provider, group = provider)
        }
        
        # add the min-map window
        l_map1 <- l_map1 %>% 
            addMiniMap(tiles = basemap_options[[1]], 
                       toggleDisplay = TRUE, 
                       position = "bottomleft")
        
        # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
        l_map1 <- l_map1 %>% onRender(
            "function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                    function (e) {
                    myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                    })
                    }"
        )
        
        # add ca boundary
        l_map1 <- l_map1 %>%
            addPolygons(data = sf_ca_boundary %>%
                            st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
                        options = pathOptions(pane = "ca_boundary"),
                        color = 'black', # "#444444",
                        weight = 0,
                        smoothFactor = 1.0,
                        opacity = 0, #1.0,
                        fill = FALSE,
                        # fillOpacity = 0.5,
                        # fillColor = 'lightblue',
                        # highlightOptions = highlightOptions(color = "white", weight = 2),
                        # popup = ~paste0('<b>', '<u>','California Boundary', '</u>','</b>'#,'<br/>'#,
                        #                 # '<b>', 'Region Number: ', '</b>', rb,'<br/>',
                        #                 # '<b>', 'Region Number: ', '</b>',  RB,'<br/>',
                        #                 # '<b>', 'Region Name: ', '</b>', office_name
                        # ), 
                        group = 'California Boundary',
                        label = 'California Boundary'#,
                        # eval(as.symbol(ces_field_names %>%
                        #                    filter(name == input$ces_parameter) %>%
                        #                    pull(ces_variable))),
                        # ')')
            )
        
        # Add controls to select the basemap and layers
        l_map1 <- l_map1 %>% addLayersControl(baseGroups = basemap_options,
                                              overlayGroups = c('California Boundary'#,
                                                                # 'Regional Board Boundary',
                                                                # 'Treatment Facilities & Collection Systems'
                                              ), 
                                              # 'HOLC Polygons', 
                                              # '303d Listed Waterbodies', 
                                              # 'CalEPA Regulated Sites',
                                              # 'Drinking Water Provider Service Areas',
                                              # 'State Water Board Region Boundaries',
                                              # 'Legend'),
                                              options = layersControlOptions(collapsed = TRUE,
                                                                             autoZIndex = TRUE))
        # Hide some groups by default (can be turned on with the layers control box on the map)
        # l_map1 <- l_map1 %>% hideGroup(c('Regional Board Boundary')) #, 'HOLC Polygons'))
        
        # Add legend
        # l_map1 <- l_map1 %>% 
        #     addLegend(position = 'bottomright',
        #               pal = ces_leaflet_pal,
        #               values = ces_pal_domain, # map_data$fill_variable,
        #               opacity = 1,
        #               layerId = 'ces_legend',
        #               bins = 4,
        #               group = 'Legend',
        #               # title = paste0('CalEnviroScreen title')
        #               title = input$ces_parameter
        #               # title = ces_field_names %>% 
        #               #     filter(name == input$ces_parameter) %>%
        #               #     pull(name)
        #     )
        
        # output the map object
        l_map1
        # })
    })
    
    
    observe({
        # input$regional_board # this line just here so this code block gets activated when the region is changed
        
        withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
            # add polygons
            leafletProxy('map1') %>%
                clearGroup('Tribal Boundaries') %>%
                addPolygons(data = sf_tribal_boundaries() %>% 
                                st_transform(crs = geographic_crs), 
                            options = pathOptions(pane = "tribal_boundaries"),
                            color = 'black', # "#444444",
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 1.0,
                            fill = FALSE,
                            # fillOpacity = 0.5,
                            # fillColor = 'lightblue',
                            highlightOptions = highlightOptions(color = "white", weight = 2),
                            # popup = ~paste0('<b>', '<u>', 'Wastewater Treatment Facility / Collection System', '</u>','</b>','<br/>',
                            #                 # '<b>', '<u>', 'Site Information:', '</u>', '</b>','<br/>',
                            #                 '<b>', 'Facility Name: ', '</b>', facility_name,'<br/>',
                            #                 '<b>', 'Facility ID: ', '</b>', facility_id,'<br/>',
                            #                 '<b>', 'WDID: ', '</b>', wdid,'<br/>',
                            #                 '<b>', 'Agency Name: ', '</b>', agency_name,'<br/>',
                            #                 '<b>', 'Agency Type: ', '</b>', agency_type,'<br/>',
                            #                 '<b>', 'Address: ', '</b>', combined_address, '<br/>',
                            #                 # '<b>', 'City: ', '</b>', site_city, '<br/>',
                            #                 '<b>', 'County: ', '</b>', place_county,'<br/>',
                            #                 # '<b>', 'Zip Code: ', '</b>', site_zip, '<br/>',
                            #                 '<b>', 'Region: ', '</b>', region, '<br/>'#,
                            #                 # '<b>', 'SIC Descriptor: ', '</b>', sic_descriptor, '<br/>',
                            #                 # '<b>', 'Notes: ', '</b>', notes, '<br/>',
                            #                 # '<b>', 'Lan/Lon ArcGIS: ', '</b>', glue('{round(lat_arc, 3)}, {round(lon_arc, 3)}'),'<br/>',
                            #                 # '<b>', 'Lan/Lon Census: ', '</b>', glue('{round(lat_census, 3)}, {round(lon_census,3)}'),'<br/>',
                            #                 # '<b>', 'Lan/Lon OSM: ', '</b>', glue('{round(lat_osm, 3)}, {round(lon_osm, 3)}'),'<br/>',
                            #                 # '<b>', 'Lan/Lon Google: ', '</b>', glue('{round(lat_google, 3)}, {round(lon_google ,3)}'),'<br/>',
                            #                 # '<b>', 'ArcGIS Geocode Address: ', '</b>', arcgis_geocode_address, '<br/>',
                            #                 # '<b>', 'Location Confidence: ', '</b>', confidence, '<br/>'
                            # ),
                            group = 'Tribal Boundaries',
                            label = ~glue('Tribal Boundary (Name: )')
                )
            
        })
    })
    
} # end of server


# 5 - run app -----------------------------------------------------------------
shinyApp(ui, server)
