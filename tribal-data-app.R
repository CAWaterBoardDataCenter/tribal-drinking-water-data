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

library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)


# 2 - set defaults ------------------------------------------------------------
## Define coordinate systems to use for transformations
projected_crs <- 3310 # see: https://epsg.io/3310 
# other options: 26910 see: https://epsg.io/26910
# resources: 
# https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=109326&inline

geographic_crs <- 4326 #4269 # see: https://epsg.io/4269
# see: https://epsg.io/4326

# Turn off s2
sf::sf_use_s2(FALSE)

## choices
list_tribal_data <- list('Bureau of Indian Affairs' = 'BIA_National_LAR.gpkg',
                         'Census Bureau' = 'native_areas_census.gpkg')

## variables for tribal lands data
tribal_data_id_field <- list('Bureau of Indian Affairs' = 'LARID',
                             'Census Bureau' = 'AFFGEOID')  # Is AFFGEOID the best value to use for the ID?
tribal_data_name_field <- list('Bureau of Indian Affairs' = 'LARName',
                               'Census Bureau' = 'NAME')

unlist(unname(list_tribal_data))




# 3 - define UI ---------------------------------------------------------------
ui <- tagList(
        tags$head(
            tags$style(HTML("
                @import url('https://fonts.googleapis.com/css2?family=Open+Sans&display=swap');
                body { 
                    font-family: 'Open Sans', sans-serif;
                }
                h4 {
                    font-size: 14px;
                    font-weight: 700;
                }
                #sidebar {
                    border-radius: 0;
                    box-shadow: inset 0 0 0 rgba(0,0,0,.05);
                    -webkit-box-shadow: inset 0 0 0 rgba(0,0,0,.05);
                }
            "))
        ),
        navbarPage(title = "California Tribal Data", 
                 # theme = shinythemes::shinytheme('flatly'),

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
                              sidebarPanel(id='sidebar',
                                     
                                     # Inputs
                                     selectInput(inputId = 'tribal_boundaries_selected',
                                                 label = 'Select Tribal Boundaries:',
                                                 choices = names(list_tribal_data)#,
                                                 # selected = initial_selected_city
                                     ), # ifelse (initial_zoom_level == 'State', 'Statewide', initial_selected_city)),
                                     h4('Tribal Lands with Mobile Home Parks'),
                                     # Chart output
                                     plotlyOutput('barplot1')
                              ), # end of sidebarPanel
                              
                              ### 3 - Main panel ----
                              # Main panel for displaying outputs 
                              mainPanel(
                                  column(width=12, style='padding:0px;',
                                      fluidRow(
                                          div(style = "display:inline-block;vertical-align:top;padding-bottom:8px",
                                              # actionButton("toggleSidebar", "Toggle sidebar"),
                                              actionButton("showpanel", "Show/Hide Sidebar", 
                                                           class = "buttonstyle", icon = icon('bars'), 
                                                           style = 'padding:4px; font-size:80%')#, # 'display:inline;'), 
                                              
                                          )
                                      ),
                                      fluidRow(
                                          leafletOutput(outputId = 'map1', width='100%', height = 400) %>% withSpinner(color="#0dc5c1")
                                      ),
                                      fluidRow(
                                          h4('Mobile Home Parks - Details'),
                                          DT::dataTableOutput('table1', width='100%')
                                      )
                                  )
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
        )
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
    sf_tribal_boundaries <- reactive({
        boundaries <- st_read(paste0('data_processed/',
        list_tribal_data[[input$tribal_boundaries_selected]]))
        boundaries <- st_transform(boundaries, 4326)
        # Fix geometries - otherwise 'invalid geometry' errors come up when running spatial operations
        boundaries = st_make_valid(boundaries)
        return(boundaries)
    }) %>%
        debounce(0) %>%
        {.}
    
    #observeEvent(sf_tribal_boundaries(), {
    #    print(user_selection$tribal)
    #    if (!is.null(user_selection$tribal)) {
    #        user_selection$tribal = NULL
     #   }
    #})
    
    tribal1 = st_read('data_processed/BIA_National_LAR.gpkg')
    tribal2 = st_read('data_processed/native_areas_census.gpkg')
    
    # Calculate the number of mobile home parks in all tribal land features
    chart_data <- reactive({
        tribal_boundaries <- sf_tribal_boundaries()
        # Add the count of intersecting mobile home parks to tribal land dataset
        tribal_boundaries$Count <- lengths(st_intersects(sf_tribal_boundaries(), sf_ca_mhp()))
        # Keep tribal land boundaries that have at least one intersecting mobile home park
        tribal_boundaries <- tribal_boundaries %>%
            filter(Count > 0)
       return(tribal_boundaries)
    })
    
    # Track user input/selection, null by default
    user_selection <- reactiveValues()
    user_selection$tribal = NULL
    
    # Read mobile home parks dataset
    mh_parks <- st_read('data_processed/ca_mhp_geocode.gpkg')
    mh_parks <- st_transform(mh_parks, 4326)
    
    # Filter mobile home parks dataset for display on map and table
    sf_ca_mhp <- reactive({
        parks <- mh_parks
        # Find mh parks that intersect with a tribal land
        # st_intersects returns a list of results for each park - evaluated against every tribal land poly
        # Find the count of intersections for each park using lengths
        parks$Intersection <- lengths(st_intersects(parks, sf_tribal_boundaries()))
        # Filter dataset to keep only the parks that intersect with a tribal land
        filtered = parks %>%
            dplyr::filter(Intersection > 0) 
        return(filtered)
    })
    
    # Old code, can delete, don't need this unless want to revert back to static loading of mhp data
    #sf_ca_mhp <- st_read('data_processed/ca_mhp_geocode.gpkg') %>%
    #sf_ca_mhp <- st_transform(sf_ca_mhp, 4326)
    #sf_ca_mhp$Intersection <- st_intersects(sf_ca_mhp, sf_tribal_boundaries, sparse=FALSE)
    # Filter dataset to keep the parks that intersect with a tribal land
    #result = mhp_data %>%
        #dplyr::filter(Intersection == TRUE) 
    #return(result)
    
    table <- reactiveValues(data = NULL)
   
    observeEvent(sf_tribal_boundaries(), {
        # Clear user selection when changing boundary datasets
        user_selection$tribal = NULL
        parks = sf_ca_mhp()
        # Drop unneeded columns, don't want to show these in the table
        drop_cols <- c('Intersection')
        parks <- parks[ , !(names(parks) %in% drop_cols)]
        parks <- st_drop_geometry(parks)
        table$data = parks
    })
    
    observeEvent(user_selection$tribal, {
        parks = sf_ca_mhp()
        # Check if the user has selected a tribal land
        if(!is.null(user_selection$tribal)) {
            # Find the mobile home parks in the selected feature
            id_field = tribal_data_id_field[[input$tribal_boundaries_selected]]
            selected_tribal_land <- sf_tribal_boundaries()[get(id_field, sf_tribal_boundaries()) == user_selection$tribal,]
            parks$Intersection <- st_intersects(parks, selected_tribal_land, sparse=FALSE)
            filtered <- parks %>%
                dplyr::filter(Intersection == TRUE)
            parks <- filtered
        }
        # Drop unneeded columns,  don't want to show these in the table
        drop_cols <- c('Intersection')
        parks <- parks[ , !(names(parks) %in% drop_cols)]
        parks <- st_drop_geometry(parks)
        table$data = parks
    })
    
    # Old, can delete, don't need anymore
    #table_data <- reactive({
        #parks <- sf_ca_mhp()
        # Check if the user has selected a tribal land
        #if(!is.null(user_selection$tribal)) {
            # Find the mobile home parks in the selected feature
            #selected_tribal_land <- sf_tribal_boundaries()[sf_tribal_boundaries()$LARID == user_selection$tribal,]
            #parks$Intersection <- st_intersects(parks, selected_tribal_land, sparse=FALSE)
            #filtered <- parks %>%
                #dplyr::filter(Intersection == TRUE)
            # <- filtered
        #}
        # Drop unneeded columns, don't want to show these in the table
        #drop_cols <- c('Intersection')
        #parks <- parks[ , !(names(parks) %in% drop_cols)]
        #parks <- st_drop_geometry(parks)
        #return(parks)
    #})
    
    
    # Add click listener on tribal boundaries data; capture user input on map
    observeEvent(input$map1_shape_click, { 
        # Get attributes (id) of clicked feature
        clicked <- input$map1_shape_click
        if(!is.null(clicked$id)) {
            # Set id to reactive value
            user_selection$tribal = clicked$id
        }
    })
    
    # Add click listener; capture user input on chart
    observeEvent(event_data("plotly_click", source = "chart1"), {
        d <- event_data("plotly_click", source = "chart1")
        if(!is.null(d$key)) {
            user_selection$tribal = d$key
            # Zoom to tribal boundary
            id_field = tribal_data_id_field[[input$tribal_boundaries_selected]]
            selected_tribal_land <- sf_tribal_boundaries()[get(id_field, sf_tribal_boundaries()) == user_selection$tribal,]
            print(selected_tribal_land)
        }
    })

    
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
            addMapPane('ca_mh_parks', zIndex=430) %>%
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
            # Maybe convert these to app-wide reactive values? I think they are used in other functions too
            field_id = tribal_data_id_field[[input$tribal_boundaries_selected]]
            field_name = tribal_data_name_field[[input$tribal_boundaries_selected]]
            # add polygons
            leafletProxy('map1') %>%
                clearGroup('Tribal Boundaries') %>%
                addPolygons(data = sf_tribal_boundaries() %>% 
                                st_transform(crs = geographic_crs), 
                            options = pathOptions(pane = "tribal_boundaries"),
                            layerId = ~get(field_id),
                            color = '#947036',
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 0.9,
                            fill = TRUE,
                            fillOpacity = 0.5,
                            fillColor = '#947036',
                            highlightOptions = highlightOptions(color = "#00FFFF", weight = 2),
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
                            label = ~get(field_name)
                )
            # Add mobile home parks
            leafletProxy('map1') %>%
            clearGroup('CA MHP') %>%
            addCircleMarkers(data = sf_ca_mhp() %>%
                             st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
                             options = pathOptions(pane = "ca_mh_parks"),
                             stroke= FALSE, 
                             color = '#f49952',
                             fill = TRUE,
                             fillColor = '#f49952',
                             fillOpacity = 1,
                             radius = 3.5,
                             popup = ~paste0('<b>', '<span style="color:#f49952">Mobile Home Park</span>', '</b>','<br/>',
                                             '<b>', 'Name: ', '</b>', ParkName, '<br/>',
                                             '<b>', 'Address: ', '</b>', ParkAddres, '</br>',
                                             '<b>', 'County: ', '</b>', County, '</br>',
                                             '<b>', 'Operated by: ', '</b>', OperatedBy, '<br/>'
                             ),
                             group = 'CA MHP',
                             label = ~paste0(ParkName)
            )
            
            # Add bar chart showing tribal lands with mh park counts
            output$barplot1 <-renderPlotly({
                if(!is.null(sf_tribal_boundaries())) {
                    name_field <- sym(tribal_data_name_field[[input$tribal_boundaries_selected]])
                    id_field <-sym(tribal_data_id_field[[input$tribal_boundaries_selected]])
                    p <- ggplot(data=chart_data(), aes(x=reorder(!!name_field, Count), y=Count, key=!!id_field))  +
                        geom_bar(stat='identity', fill='#f49952') +
                        labs(x = "Year", y = name_field) +
                        coord_flip() +
                        theme(legend.position = 'none',
                              panel.grid.major.x = element_line(color = '#e3e3e3'),
                              panel.grid.major.y = element_blank(),
                              axis.title = element_blank(),
                              axis.ticks = element_blank(),
                              panel.background = element_rect(fill = 'transparent', color = 'transparent'),
                              plot.background = element_rect(fill = 'transparent', color = 'transparent')
                        )
                    #theme(plot.title = element_text(hjust = 0.5))
                    ggplotly(p, tooltip = c("y"), source = 'chart1')
                }
            })
                    
            # Add table
            output$table1 <- DT::renderDataTable({
                dataset <- table$data
            }, options = list(scrollX=TRUE, scrollCollapse=TRUE))
        })
    })
    
} # end of server


# 5 - run app -----------------------------------------------------------------
shinyApp(ui, server)
