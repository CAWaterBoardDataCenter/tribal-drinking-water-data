# Tribal Water Data Tool

# In colaboration between Alexis Perez and Sam Prieto Serrano. 

## Note: To publish the app to shinyapps.io, you only need to upload this file 
## and the data_processed folder; no other files are needed for deploying the app


# 1 - load packages -----------------------------------------------------------
library(shiny)

# Mapping and GIS operations
library(sf)
library(leaflet)
library(htmlwidgets)

# shiny stuff
library(shinycssloaders)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(here)
library(glue)

library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(tidyverse)
library(rlang)


# 2 - set defaults ------------------------------------------------------------

# The Leaflet package expects all point, line, and shape data to be specified in latitude and longitude using 
# WGS 84 (a.k.a. EPSG:4326). By default, when displaying this data it projects everything to EPSG:3857 and expects 
# that any map tiles are also displayed in EPSG:3857
# see: https://rstudio.github.io/leaflet/projections.html#:~:text=The%20Leaflet%20package%20expects%20all,also%20displayed%20in%20EPSG%3A3857

# Define coordinate systems to use for transformations
geographic_crs <- 4326 # see: https://epsg.io/4326

# choices
list_tribal_data <- list('Bureau of Indian Affairs (BIA)' = 'native_areas_bia.gpkg', # old version: 'BIA_National_LAR_OLD.gpkg'
                         'BIA - Fed-Regulated Water' = 'yesfed_native_areas_bia.gpkg',
                         'BIA - No Fed-Regulated Water' = 'notfed_native_areas_bia.gpkg',
                         'Census Bureau' = 'native_areas_census.gpkg', 
                         'Census - Fed-Regulated Water' = 'yesfed_native_areas_census.gpkg',
                         'Census - No Fed-Regulated Water' = 'notfed_native_areas_census.gpkg')

# the id and name fields are different in both tribal land datasets
# store the field names here so that we can reference the correct fields when switching between the two datasets
tribal_data_id_field <- list('Bureau of Indian Affairs (BIA)' = 'LARID',
                             'BIA - Fed-Regulated Water' = 'LARID',
                             'BIA - No Fed-Regulated Water' = 'LARID',
                             'Census Bureau' = 'AFFGEOID', 
                             'Census - Fed-Regulated Water' = 'AFFGEOID',
                             'Census - No Fed-Regulated Water' = 'AFFGEOID')  # Is AFFGEOID the best value to use for the ID?

tribal_data_name_field <- list('Bureau of Indian Affairs (BIA)' = 'LARNAME', # Previous version of this dataset used: LARName
                               'BIA - Fed-Regulated Water' = 'LARNAME',
                               'BIA - No Fed-Regulated Water' = 'LARNAME',
                               'Census Bureau' = 'NAME', 
                               'Census - Fed-Regulated Water' = 'NAME', 
                               'Census - No Fed-Regulated Water' = 'NAME')

#unlist(unname(list_tribal_data))




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
               ## A --- MHPs / Tabular Data Tab ----
               tabPanel('MHPs', icon = icon('caravan', lib = 'font-awesome'),
                        
                        # 1 --- setup 
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
                            # 2a --- sidebarPanel
                            sidebarPanel(id='sidebar', 
                                         style='background-color:transparent; border-left:transparent;border-top:transparent;border-bottom:transparent',
                                         
                                         # Inputs
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12,
                                                             selectInput(inputId = 'tribal_boundaries_selected',
                                                                         label = 'Select Tribal Boundaries:',
                                                                         choices = names(list_tribal_data)))
                                                             # # for selectAll/None toggle checkbox
                                                             # checkboxInput('all', 'Select All/None', value = TRUE),
                                                             # 
                                                             # # for the variables names from mtcars dataset
                                                             # checkboxGroupInput('tribes', 'Select Specific Tribes', sf_tribal_boundaries()[[state$]]),)
                                             ),
                                         ), 
                                         # Outputs                                         
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12,
                                                             h4('Tribal Lands with Mobile Home Parks'),
                                                             plotlyOutput('MHPBarChart'))
                                             )
                                         )
                                         
                            ), # end of sidebarPanel
                            
                            
                            # 2b --- mainPanel
                            
                            mainPanel(
                                column(width=12, style='padding-left:0px',
                                       # fluidRow(
                                       #     div(style = "display:inline-block; vertical-align:top;padding-bottom:8px",
                                       #         actionButton("showpanel", "Show/Hide Sidebar", 
                                       #                      class = "buttonstyle", icon = icon('bars'), 
                                       #                      style = 'padding:4px; font-size:80%') 
                                       #     )
                                       # ),
                                       fluidRow(
                                           leafletOutput(outputId = 'map1', 
                                                         width ='100%', 
                                                         height = 400) %>% 
                                               withSpinner(color="#0dc5c1")
                                       ),
                                       fluidRow(
                                           hr()
                                       ),
                                       fluidRow(
                                           h4('Mobile Home Parks - Details',
                                              style='text-align:center'),
                                           # Table output
                                           DTOutput(outputId = 'MHPTable', 
                                                    width='100%')
                                       )
                                )
                            ) # end of mainPanel
                        ) # end of sidebarLayout
               ), # end of tabPanel (for MHPs)
               
               
               ## B --- Wells + SSWS Tab ----
               tabPanel('Wells + SSWS', icon = icon('water', lib = 'font-awesome'), 
                        sidebarLayout(
                            
                            sidebarPanel(style='background-color:transparent; border-left:transparent;border-top:transparent;border-bottom:transparent',
                                         
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12, 
                                                             selectInput(inputId = 'tribal_boundaries_selected2', 
                                                                         label = 'Select Tribal Boundaries:', 
                                                                         names(list_tribal_data)))   
                                             )
                                         ),
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12, 
                                                             h4('Tribal Lands with Drinking Water Wells'), 
                                                             plotlyOutput('wellsBarChart', 
                                                                          height = '500px'))
                                             )
                                         ),
                                         wellPanel(
                                             fluidRow(
                                                 column(width = 12, 
                                                        h4('Tribal Lands with State Small Water Systems (SSWS)'), 
                                                        plotlyOutput('SSWSBarChart', 
                                                                     height = '200px'))
                                             )
                                         )
                            ),
                            
                            mainPanel(
                                fluidRow(
                                    column(width=12, 
                                           style='padding-left:0px;', 
                                           leafletOutput(outputId = 'map2'), 
                                           hr())  
                                ),
                                fluidRow(
                                    column(width=12, 
                                           style='padding-left:0px;', 
                                           h4('Drinking Water Wells - Details', 
                                              style='text-align:center'), 
                                           DTOutput(outputId = 'wellsTable'), 
                                           hr())
                                ),
                                fluidRow(
                                    column(width=12, 
                                           style='padding-left:0px; margin-top: 15px', 
                                           h4('State Small Water Systems (SSWS) - Details', 
                                              style='text-align:center'), 
                                           DTOutput(outputId = 'SSWSTable'))
                                )
                            )
                        ) # end of sidebar layout
               ), # end of tabPanel (for wells + SSWS)
               
               ## C --- Compiled Water Data ----
               tabPanel('In Progress', icon = icon('paint-roller', lib = 'font-awesome'),
                        
                        # Sidebar layout with input and output definitions
                        sidebarLayout(
                            sidebarPanel(id='sidebar', 
                                         style='background-color:transparent; border-left:transparent;border-top:transparent;border-bottom:transparent',
                                         
                                         # Inputs
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12,
                                                             selectInput(inputId = 'tribal_boundaries_selected3',
                                                                         label = 'Select Tribal Boundaries:',
                                                                         choices = names(list_tribal_data)))
                                             )
                                         #)
                                         ),
                                         #HERE!
                                         #Outputs
                                         wellPanel(
                                             fluidRow(style='',
                                                      column(width = 12,
                                                             h4('Freshwater Harmful Algae Blooms by LAR'),
                                                             plotlyOutput('FHABBarChart'))
                                             )
                                         )
                                         
                            ), # end of sidebarPanel
                            
                            
                            # Main panel 
                            
                            mainPanel(
                                column(width=12, style='padding-left:0px',
                                       # fluidRow(
                                       #     div(style = "display:inline-block; vertical-align:top;padding-bottom:8px",
                                       #         actionButton("showpanel", "Show/Hide Sidebar", 
                                       #                      class = "buttonstyle", icon = icon('bars'), 
                                       #                      style = 'padding:4px; font-size:80%') 
                                       #     )
                                       # ),
                                       fluidRow(
                                           leafletOutput(outputId = 'map3', 
                                                         width ='100%', 
                                                         height = 400) %>% 
                                               withSpinner(color="#0dc5c1")
                                       ),
                                       fluidRow(
                                           h4('Dry Household Wells - Details',
                                              style='text-align:center'),
                                           # Table output
                                           DTOutput(outputId = 'HWSSTable', 
                                                    width='100%')
                                       ),
                                       fluidRow(
                                           h4('SAFER Risk Measures - Details',
                                              style='text-align:center'),
                                           # Table output
                                           DTOutput(outputId = 'SAFERTable', 
                                                    width='100%')
                                       ),
                                       fluidRow(
                                           h4('Freshwater Harmful Algae Blooms Reports - Details',
                                              style='text-align:center'),
                                           # Table output
                                           DTOutput(outputId = 'FHABTable', 
                                                    width='100%')
                                       )
                                )
                            ) # end of mainPanel
                        ) # end of sidebarLayout
               ), # end of tabPanel (for Test)
               
               ## D --- Background Info Tab ---- 
               tabPanel('Background Information', 
                        icon = icon('info-circle'), # icon = icon('book-reader')
                        fluidRow(
                            p('This tool is intended to compile and display information that can inform and help prioritize outreach related to drinking water issues in tribal areas within California. It is a work in progress, and is not intended to be a comprehensive source of tribal-related water data.'),
                            h2('Source Code / Documentation'),
                            p('Source code for this tool, as well as additional documention we\'re developing, is available on GitHub in ',
                              a(strong('this repository')),
                              ' (note that the repository is currently restricted to members of the "CAWaterboardDataCenter" GitHub Organization).'),
                            h2('Data Sources'),
                            p('The processes we used to access and transform the datasets described below are documented in the ', 
                              em('tribal-data-exploration.RMD'), 
                              ' file, which is avilable on GitHub at ', 
                              a(strong('this link'), href = 'https://github.com/CAWaterBoardDataCenter/tribal-drinking-water-data/blob/main/tribal-data-exploration.RMD'),
                              '.'
                            ),
                            h3('1. Tribal Boundaries'),
                            p('Tribal boundary datasets for California are available from multiple sources, many of which differ somewhat in terms of the spatial representation of the boundaries and/or the attribute data they contain. Currently this tool includes tribal boundary datasets from the sources described below.'),
                            h4('1.1. U.S. Census Bureau'),
                            p('The U.S. Census Bureau\'s American Indian / Alaska Native / Native Hawaiian Areas dataset is available at ',
                              a(strong('this link'), href = 'https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2020&layergroup=American+Indian+Area+Geography'),
                              '. We used the ',
                              code('tigris'),
                              ' R package to acccess this dataset (with the function ', 
                              code('tigris::native_areas()'),
                              '). The data displayed in this tool comes from a copy of the 2020 version of this dataset, which we downloaded in February 2022.'
                            ),
                            p('The Census Bureau\'s description of this dataset states that the dataset: ',
                              em('"contains both legal and statistical American Indian, Alaska Native, and Native Hawaiian entities for which the Census Bureau publishes data. The legal entities consist of federally recognized American Indian reservations and off-reservation trust land areas, state-recognized American Indian reservations, and Hawaiian home lands (HHLs)."')
                            ),
                            p('More information can be found from the Census Bureau at ',
                              a(strong('this link'), href = 'https://www2.census.gov/geo/pdfs/reference/GARM/Ch5GARM.pdf'),
                              '.'),
                            h4('1.2. Bureau of Indian Affairs'),
                            p('The U.S. Department of the Interior, Bureau of Indian Affairs\' (BIA) Land Area Representations (LAR) dataset is available for download at ',
                              a(strong('this link'), href = 'https://biamaps.doi.gov/bogs/datadownload.html#H2T2).'), 
                              '. The data displayed in this tool comes from a copy of the dataset we downloaded in September 2022.'
                            ),
                            p('The BIA\'s description of the dataset states (in part): ',
                              em('"The purpose of the American Indian and Alaska Native Land Area Representation (AIAN-LAR) Geographic Information System (GIS) dataset is to depict the external extent of Federal Indian reservations and the external extent of associated land "held in trust” by the United States, “restricted fee” or “mixed ownership” status for Federally recognized tribes and individual Native Americans. This dataset includes other land area types such as Public Domain Allotments, Dependent Indian Communities and Homesteads."') 
                            ),
                            p('More information can be found from the BIA at ',
                              a(strong('this link'), href = 'https://biamaps.doi.gov/bogs/datadownload.html#H2T2'),
                              '.'),
                            h3('2. Mobile Home Parks'),
                            p('Mobile home park information displayed in the ',
                              em('MHPs'), 
                              'tab was obtained from the California Department of Housing and Community Development, at ',
                              a(strong('this link'), href = 'https://casas.hcd.ca.gov/casas/cmirMp/onlineQuery'),
                              '. The data displayed in this tool was manually downloaded on February 22, 2022 (this data has to be downloaded manually, because it requires entering a security code to view, and no direct link to the data download is available. To download the data, leave all criteria fields blank and click the "Export to Excel" button).'
                            ),
                            p('To map the mobile home parks, we used a tool developed by the State Water Board\'s GIS Unit (called ',
                              em('Geocoding Toolbox v.1.0 for ArcGIS Pro'), 
                              ') to convert addresses to geographic coordinates. As a result, the locations displayed are estimates, and may not always be highly accurate. The geocoding tool uses the Bing geocoding service. Water Board staff can access the tool on our intranet site at ',
                              a(strong('this link'), href = 'http://wiki.waterboards.ca.gov/gis/doku.php#geocoding_toolbox'),
                              '.'),
                            p('Prior to using the geocoding tool, we processed the data and manually verified (using Google Maps) a selection of mobile home park addresses that appeared to be entered or recorded incorrectly. We updated the addresses of four parks and removed one park that did not have a valid or verifiable address. These steps are documented in a Python script on GitHub at ', 
                              a(strong('this link'), href = 'https://github.com/CAWaterBoardDataCenter/tribal-drinking-water-data/blob/main/working_files/mobile_home_parks/mobile-home-parks.py'), '.'),
                            h3('3. Drinking Water Information'),
                            h4('3.1. Drinking Water Wells'),
                            p('The well data displated in the ',
                              em('Wells + SSWS'),
                              'tab comes from the State Water Board\'s ',
                              em('Water Quality Risk Final'), 
                              ' dataset, which is available at ',
                              a(strong('this link'), href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=84dc4363570b42aba392952d8974c8ab'),
                              '. Specifically, we used the ',
                              em('Water Quality Risk by Well (All Contaminants)'), 
                              ' point layer (available at ',
                              a(strong('this link'), href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=84dc4363570b42aba392952d8974c8ab&sublayer=0'), 
                              '). The data displayed in this tool is from a copy of the dataset that we accessed on September 9, 2022.'
                            ),
                            p('The dataset description states (in part): ',
                              em('"The aquifer risk map is being developed to fulfill requirements of SB-200 and is intended to help prioritize areas where domestic wells and state small water systems may be accessing groundwater that does not meet primary drinking water standards (maximum contaminant level or MCL)... The water quality risk is based on depth-filtered, de-clustered water quality results from public and domestic supply wells for all contaminants with an MCL (plus hexavalent chromium). The methodology used to determine water quality risk is outlined ', 
                                 a(strong('here'), href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=62b116bb7e824df098b871cbce73ce3b'),
                                 '."')
                            ),
                            h4('3.2. State Small Water Systems'),
                            p('The State Small Water System data displated in the ',
                              em('Wells + SSWS'), 
                              ' tab comes from the State Water Board\'s ',
                              em('State Small Water Systems DDW'),
                              ' dataset, which is available at ',
                              a(strong('this link'), href = 'https://gispublic.waterboards.ca.gov/portal/home/item.html?id=2d34d39f75b8491d88adda57adb837ec&fromSearch=true&searchPosition=1&searchTerm=small%20state%20water%20systems'), 
                              '. The data displayed in this tool is from a copy of the dataset that we accessed on September 9, 2022.'
                            ),
                            p('The dataset description states (in part): ',
                              em('"Layer includes geocoded point locations and Administrative contact information for state smalls, collected by the Needs Analysis Unit as of 9/22/21. This information was collected as part of SB200 requirements for counties to submit state small water system location information. This layer also includes water quality risk estimates from the 2022 Aquifer Risk Map. The risk estimates are not based on samples collected directly from each state small water system, but reflect the average water quality of the raw source groundwater nearby."')
                            ), 
                            h3('4. Other Water Data Information'),
                            h4('4.1. Household Water Supply Shortage'),
                            p('The household water supply data displated in the ',
                              em('In Progress'),
                              'tab comes from the State Water Board\'s ',
                              em('Household Water Supply Shortage Reporting System Data'), 
                              ' system, which is available at ',
                              a(strong('this link'), href = 'https://services.arcgis.com/aa38u6OgfNoCkTJ6/ArcGIS/rest/services/Household_Water_Supply_Shortage_Reports/FeatureServer/0'),
                              '. The data can also be visible at the ',
                              em('Drinking Water System Outreach Tool'), 
                              ' (available at ',
                              a(strong('this link'), href = 'https://gispublic.waterboards.ca.gov/portal/apps/webappviewer/index.html?id=70d27423735e45d6b037b7fbaea9a6a6'), 
                              ').'
                            ),
                            h4('4.2. SAFER Dashboard'),
                            p('The SAFER data displayed in the ',
                              em('In Progress'), 
                              ' tab comes from the State Water Board\'s ',
                              em('SAFER Dashboard'),
                              ' tool, which is available at ',
                              a(strong('this link'), href = 'https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/saferdashboard.html'), 
                              '. '
                            ),
                            p('The dataset description states (in part): ',
                              em('"The Division of Drinking Water (DDW) identifies Failing and At-Risk community water systems and K-12 non-transient, non-community schools. This information is displayed online in the Safe and Affordable Funding for Equity and Resilience (SAFER) Dashboard. The data utilized for this assessment is derived from multiple sources: self-reported from water systems, data generated by DDW staff, other California state agencies, and U.S. Census. The data sources, calculation methods, Failing and At-Risk criteria, etc. are fully documented in the annual Drinking Water Needs Assessment report which is published annually on the State Water Board\'s website."')
                            ),
                            h4('4.3. Freshwater Harmful Algae Bloom'),
                            p('The Freshwater Harmful Algae Bloom Report data displayed in the ',
                              em('In Progress'), 
                              ' tab comes from the State\'s',
                              em('OpenDataPortal'),
                              ' site, which is available at ',
                              a(strong('this link'), href = 'https://data.ca.gov/dataset/surface-water-freshwater-harmful-algal-blooms/resource/c6f760be-b94f-495e-aa91-2d8e6f426e11#'), 
                              '. '
                            ),
                            p('The dataset description states (in part): ',
                              em('"Freshwater harmful algal bloom (HAB) data from the Freshwater Harmful Algal Bloom (FHAB) Reports database. The FHAB Reports database is the California State Water Resources Control Board\'s data system for data and information voluntarily reported through the Freshwater Incident Form found on the CA HABs Portal."')
                            )
                        )
               ) # end of tabPanel (for Info)
    )
) # end of UI




# 4 - define server logic ---------------------------------------
server <- function(input, output, session) {
    
    # Creating state ----
    state <- reactiveValues(
        selectedTribeTab1 = NULL, 
        MHPtable = NULL, 
        tab2Rendered = FALSE, 
        selectedTribeTab2Wells = NULL, 
        selectedTribeTab2SSWS = NULL,
        wellsTable = NULL,
        SSWSTable = NULL,
        tab3Rendered = FALSE, 
        selectedTribeTab3HWSS = NULL, 
        selectedTribeTab3SAFER = NULL,
        selectedTribeTab3FHAB = NULL, 
        HWSSTable = NULL,
        SAFERTable = NULL,
        FHABTable = NULL,
        tribalNameField = NULL,
        tribalIDField = NULL,
    )
    
    # Read base datasets ----
    # read CA boundary 
    sf_ca_boundary <- st_read(here('data_processed', 'ca_boundary.gpkg')) %>% 
        st_transform(geographic_crs)
    
    # read tribal boundaries, reloads when user changes tribal boundary data set
    sf_tribal_boundaries <- reactive({
        # Selected tribal boundary
        boundary <- input$tribal_boundaries_selected
        
        # Track current tribal name field and tribal id field
        state$tribalNameField = sym(tribal_data_name_field[[boundary]])
        state$tribalIDField = sym(tribal_data_id_field[[boundary]])
        
        # read data file, get the file name from list_tribal_data
        boundaries <- st_read(here('data_processed', 
                                   list_tribal_data[[boundary]])) %>% 
            st_transform(geographic_crs) %>% 
            st_make_valid(boundaries) # Fix geometries - otherwise 'invalid geometry' error
        
        return(boundaries)
    }) %>% 
        debounce(0) %>% 
        {.}
    
    # --- Read mhp dataset ----
    ca_mh_parks <- st_read(here('data_processed', 'ca_mhp_geocode.gpkg')) %>% 
        st_transform(geographic_crs)
    
    # Process mobile home parks dataset for display on map and table
    sf_ca_mhp <- reactive({
        parks <- ca_mh_parks
        # Find mh parks that intersect with a tribal land
        parks$Intersection <- lengths(st_intersects(parks, sf_tribal_boundaries()))
        # Filter dataset to keep only the parks that intersect with a tribal land
        filtered = parks %>% filter(Intersection > 0)
        return(filtered)
    }) 
    
    # intersecting_mhp <- reactive({
    #     # filter for wells in tribal boundaries
    #     mhp_filtered <- st_filter(ca_mh_parks, sf_tribal_boundaries())
    #     # join information about tribal area to filtered SSWS
    #     mhp_filtered <- mhp_filtered %>%
    #         st_join(sf_tribal_boundaries()) %>% 
    #         st_drop_geometry() #PICK SUBSET LATER HERE!
    #     # %>% 
    #     # #extract relevant columns
    #     # subset(select = c('system_nam', 'address', 'owner_type', 
    #     #                   'phys_count', 'wqrskbn', 'pwsid', 
    #     #                   'service_co', 'population', 'svc_area_t', 
    #     #                   'admin_cont', 'admin_emai',
    #     #                   'latitude', 'longitude',
    #     #                   as_string(state$tribalNameField), as_string(state$tribalIDField)))
    #     return(mhp_filtered)
        
    # })
    
    # --- Read wells dataset ----
    
    ca_wells <- st_read(here('data_processed', 'wells.gpkg')) %>% 
        st_transform(geographic_crs)
    
    intersectingWells <- reactive({
        # filter for wells in tribal boundaries
        wells_filter <- st_filter(ca_wells, sf_tribal_boundaries())
        # join information about tribal area to filtered wells
        wells_filter <- wells_filter %>%
            st_join(sf_tribal_boundaries()) %>% 
            st_drop_geometry()
        
        # extract relevant columns
        wells_filter <- if (nrow(wells_filter) > 0) {select(wells_filter, 'gm_well_ca', 'risk', 'gm_dataset', 
                                                            'gm_well_id', 'gm_dataset', 'gm_latitud', 
                                                            'gm_longitu', as_string(state$tribalNameField), 
                                                            as_string(state$tribalIDField))} else {wells_filter}
        
        if (nrow(wells_filter) == 0){
            wells_filter[1, ] <- c(NA)
        }
        
        return(wells_filter)
        
    })
    
    
    # --- Read SSWS dataset ----
    
    ca_SSWS <- st_read(here('data_processed', 'SSWS.gpkg')) %>% 
        st_transform(geographic_crs)
    
    intersectingSSWS <- reactive({
        # filter for wells in tribal boundaries
        SSWS_filtered <- st_filter(ca_SSWS, sf_tribal_boundaries())
        # join information about tribal area to filtered SSWS
        SSWS_filtered <- SSWS_filtered %>%
            st_join(sf_tribal_boundaries()) %>% 
            st_drop_geometry() 
        
        # extract relevant columns
        SSWS_filtered <- if (nrow(SSWS_filtered) > 0) {select(SSWS_filtered, 'system_nam', 'address', 'owner_type', 
                                                            'phys_count', 'wqrskbn', 'pwsid', 
                                                            'service_co', 'population', 'svc_area_t', 
                                                            'admin_cont', 'admin_emai',
                                                            'latitude', 'longitude',
                                                            as_string(state$tribalNameField), as_string(state$tribalIDField))} else {SSWS_filtered}
        
        if (nrow(SSWS_filtered) == 0){
            SSWS_filtered[1, ] <- c(NA)
        }
        
        # map '0' to 'unknown' for water quality risk
        SSWS_filtered["wqrskbn"][SSWS_filtered["wqrskbn"] == "0"] <- "unknown"
        return(SSWS_filtered)
        
    })
    
    # --- Read HWSS dataset ----
    
    ca_HWSS <- st_read(here('data_processed', 'new_household_water_supply_shortage_reports.gpkg')) %>% 
        st_transform(geographic_crs)
    
    intersectingHWSS <- reactive({
        # filter for wells in tribal boundaries
        HWSS_filtered <- st_filter(ca_HWSS, sf_tribal_boundaries())
        # join information about tribal area to filtered SSWS
        HWSS_filtered <- HWSS_filtered %>%
            st_join(sf_tribal_boundaries()) %>% 
            st_drop_geometry() #PICK SUBSET LATER HERE!
        # %>% 
        # #extract relevant columns
        # subset(select = c('system_nam', 'address', 'owner_type', 
        #                   'phys_count', 'wqrskbn', 'pwsid', 
        #                   'service_co', 'population', 'svc_area_t', 
        #                   'admin_cont', 'admin_emai',
        #                   'latitude', 'longitude',
        #                   as_string(state$tribalNameField), as_string(state$tribalIDField)))
        return(HWSS_filtered)
        
    })
    
    sf_ca_hwss <- reactive({
        reports <- ca_HWSS
        # Find reports that intersect with a tribal land
        reports$Intersection <- lengths(st_intersects(reports, sf_tribal_boundaries()))
        # Filter dataset to keep only the reports that intersect with a tribal land
        filtered = reports %>% filter(Intersection > 0)
        return(filtered)
    }) 
    
    
    # --- Read SAFER dataset ----
    
    ca_SAFER <- st_read(here('data_processed', 'new_SAFER_drinkingwatersystems.gpkg')) %>% 
        st_transform(geographic_crs)
    
    intersectingSAFER <- reactive({
        # filter for systems in tribal boundaries
        SAFER_filtered <- st_filter(ca_SAFER, sf_tribal_boundaries())
        # join information about tribal area to filtered SAFER
        SAFER_filtered <- SAFER_filtered %>%
            st_join(sf_tribal_boundaries()) %>% 
            st_drop_geometry() #PICK SUBSET LATER HERE!
        # %>% 
        # #extract relevant columns
        # subset(select = c('system_nam', 'address', 'owner_type', 
        #                   'phys_count', 'wqrskbn', 'pwsid', 
        #                   'service_co', 'population', 'svc_area_t', 
        #                   'admin_cont', 'admin_emai',
        #                   'latitude', 'longitude',
        #                   as_string(state$tribalNameField), as_string(state$tribalIDField)))
        return(SAFER_filtered)
        
    })
    
    sf_ca_SAFER <- reactive({
        systems <- ca_SAFER
        # Find systems that intersect with a tribal land
        systems$Intersection <- lengths(st_intersects(systems, sf_tribal_boundaries()))
        # Filter dataset to keep only the systems that intersect with a tribal land
        filtered = systems %>% filter(Intersection > 0)
        return(filtered)
    })
    
    # --- Read FHAB dataset ----
    
    ca_FHAB <- st_read(here('data_processed', 'new_FHAB_bloomreports.gpkg')) %>% 
        st_transform(geographic_crs)
    
    intersectingFHAB <- reactive({
        # filter for systems in tribal boundaries
        FHAB_filtered <- st_filter(ca_FHAB, sf_tribal_boundaries())
        # join information about tribal area to filtered
        FHAB_filtered <- FHAB_filtered %>%
            st_join(sf_tribal_boundaries()) %>% 
            st_drop_geometry() #PICK SUBSET LATER HERE!
        # %>% 
        # #extract relevant columns
        # subset(select = c('system_nam', 'address', 'owner_type', 
        #                   'phys_count', 'wqrskbn', 'pwsid', 
        #                   'service_co', 'population', 'svc_area_t', 
        #                   'admin_cont', 'admin_emai',
        #                   'latitude', 'longitude',
        #                   as_string(state$tribalNameField), as_string(state$tribalIDField)))
        return(FHAB_filtered)
        
    })
    
    sf_ca_FHAB <- reactive({
        blooms <- ca_FHAB
        # Find systems that intersect with a tribal land
        blooms$Intersection <- lengths(st_intersects(blooms, sf_tribal_boundaries()))
        # Filter dataset to keep only the systems that intersect with a tribal land
        filtered = blooms %>% filter(Intersection > 0)
        return(filtered)
    })
    
    
    # --- Synchronizing the selected boundary between tabs ----
    #HERE! UPDATE WITH TAB 3
    observeEvent(input$tribal_boundaries_selected, {
        updateSelectInput(inputId = 'tribal_boundaries_selected2', select = input$tribal_boundaries_selected)
    })

    observeEvent(input$tribal_boundaries_selected, {
        updateSelectInput(inputId = 'tribal_boundaries_selected3', select = input$tribal_boundaries_selected)
    })

    observeEvent(input$tribal_boundaries_selected2, {
        updateSelectInput(inputId = 'tribal_boundaries_selected', select = input$tribal_boundaries_selected2)
    })

    observeEvent(input$tribal_boundaries_selected2, {
        updateSelectInput(inputId = 'tribal_boundaries_selected3', select = input$tribal_boundaries_selected2)
    })

    observeEvent(input$tribal_boundaries_selected3, {
        updateSelectInput(inputId = 'tribal_boundaries_selected', select = input$tribal_boundaries_selected3)
    })

    observeEvent(input$tribal_boundaries_selected3, {
        updateSelectInput(inputId = 'tribal_boundaries_selected2', select = input$tribal_boundaries_selected3)
    })
    
    # --- #
    
    ## toggle sidebar panel
    observeEvent(input$showpanel,{
        session$sendCustomMessage(type = 'resize', message = 1)
    })
    
    ## --- Dropdown menu (server) ----
    # observe({
    #     
    #     # if input$all is TRUE (basically a SELECT ALL option), all choices will be selected
    #     # if input$all is FALSE (basically a NONE option), none of the choices will be selected
    #     
    #     updateCheckboxGroupInput(
    #         session, 'mtcars', choices = myChoices,
    #         selected = if(input$all) myChoices
    #     )
    #     
    # })
    
    
    # --- Data for bar charts on side panel ----
    # Calculate the number of mobile home parks located on tribal lands, summarize by tribal land feature
    # reactive - this will rerun if user changes the tribal boundary dataset
    chart_data_mhp <- reactive({
        tribal_boundaries <- sf_tribal_boundaries()
        # Count intersection
        tribal_boundaries$Count <- lengths(st_intersects(sf_tribal_boundaries(), sf_ca_mhp()))
        # Keep tribal lands that have at least one intersecting mobile home park, filter out lands with no parks
        tribal_boundaries <- tribal_boundaries %>% filter(Count > 0)
        return(tribal_boundaries)
    })
    
    chart_data_hwss <- reactive({
        tribal_boundaries <- sf_tribal_boundaries()
        # Count intersection
        tribal_boundaries$Count <- lengths(st_intersects(sf_tribal_boundaries(), sf_ca_hwss()))
        # Keep tribal lands that have at least one intersecting mobile home park, filter out lands with no parks
        tribal_boundaries <- tribal_boundaries %>% filter(Count > 0)
        return(tribal_boundaries)
    })
    
    chart_data_safer <- reactive({
        tribal_boundaries <- sf_tribal_boundaries()
        # Count intersection
        tribal_boundaries$Count <- lengths(st_intersects(sf_tribal_boundaries(), sf_ca_SAFER()))
        # Keep tribal lands that have at least one intersecting mobile home park, filter out lands with no parks
        tribal_boundaries <- tribal_boundaries %>% filter(Count > 0)
        return(tribal_boundaries)
    })
    
    chart_data_fhab <- reactive({
        tribal_boundaries <- sf_tribal_boundaries()
        # Count intersection
        tribal_boundaries$Count <- lengths(st_intersects(sf_tribal_boundaries(), sf_ca_FHAB()))
        # Keep tribal lands that have at least one intersecting mobile home park, filter out lands with no parks
        tribal_boundaries <- tribal_boundaries %>% filter(Count > 0)
        return(tribal_boundaries)
    })
    
    
    # ---- Default table view for datasets ----
    observeEvent(sf_tribal_boundaries(), {
        ## --- MHP ---- ##
        # Clear user selection when changing boundary data sets
        state$selectedTribeTab1 = NULL
        parks = sf_ca_mhp()
        # Drop unneeded columns, don't want to show these in the table
        drop_cols <- c('Intersection')
        parks <- parks[ , !(names(parks) %in% drop_cols)] %>% 
            st_drop_geometry(parks) # Drop geometry column
        state$MHPTable = parks #HERE?
        
        ## --- Wells+SSWS ---- ##
        # Clear user selection when changing boundary data sets #HERE!
        state$selectedTribeTab2Wells = NULL
        state$wellsTable = intersectingWells()
        
        # Clear user selection when changing boundary data sets
        state$selectedTribeTab2SSWS = NULL
        state$SSWSTable = intersectingSSWS()
        
        ## --- Other ---- ##
        # Clear user selection when changing boundary data sets
        state$selectedTribeTab3HWSS = NULL
        state$HWSSTable = sf_ca_hwss()
        
        # Clear user selection when changing boundary data sets
        state$selectedTribeTab3SAFER = NULL
        state$SAFERTable = sf_ca_SAFER()
        
        # Clear user selection when changing boundary data sets
        state$selectedTribeTab3FHAB = NULL
        state$FHABTable = sf_ca_FHAB()
    })
    
    # --- Tab1 table functions ----
    # Filter MHP table data, runs when user selects/clicks a tribal land feature (on map or bar chart)
    observeEvent(state$selectedTribeTab1, {
        parks = sf_ca_mhp()
        # Check if the user has selected a tribal land
        if(!is.null(state$selectedTribeTab1)) {
            # Find the mobile home parks in the selected feature
            id_field = tribal_data_id_field[[input$tribal_boundaries_selected]] # get the ID field name for current tribal boundary dataset
            selected_tribal_land <- sf_tribal_boundaries()[get(id_field, sf_tribal_boundaries()) == state$selectedTribeTab1,]
            parks$Intersection <- st_intersects(parks, selected_tribal_land, sparse=FALSE) # Find intersecting mh parks
            filtered <- parks %>%
                dplyr::filter(Intersection == TRUE)
            parks <- filtered
        }
        # Drop unneeded columns,  don't want to show these in the table
        drop_cols <- c('Intersection')
        parks <- parks[ , !(names(parks) %in% drop_cols)]
        parks <- st_drop_geometry(parks) # Drop geometry column
        state$MHPTable = parks
    })
    
    # Add click listener; capture user input on map
    observeEvent(input$map1_shape_click, { 
        # Get attributes (id) of clicked feature
        clicked <- input$map1_shape_click
        if(!is.null(clicked$id)) {
            # Set id to reactive value
            state$selectedTribeTab1 = clicked$id
        }
    })
    
    
    # #NOT FINISHED | Add click listener; capture user input on bar chart 
    # observeEvent(event_data("plotly_click", source = "MHPchart"), {
    #     d <- event_data("plotly_click", source = "MHPchart")
    #     if(!is.null(d$key)) {
    #         state$selectedTribeTab1 = d$key
    #         # Zoom to tribal boundary on map - NOT WORKING, NOT FINISHED ---
    #         # id_field = tribal_data_id_field[[input$tribal_boundaries_selected]]
    #         # selected_tribal_land <- sf_tribal_boundaries()[get(id_field, sf_tribal_boundaries()) == state$selectedTribeTab1,]
    #         # print(selected_tribal_land) # stopped here
    #     }
    # })
    
    
    ## --- Create base map (shared between the different tabs) ----
    
    baseMap <- leaflet()
    
    # Basemap Options
    # removed basemaps 3 and 5 in order to reduce clutter; 3 and 5 shared similarities with the other base mpas
    # old basemp options: ('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap')
    basemap_options <- c('Esri.WorldTopoMap', 'CartoDB.Positron','Esri.WorldImagery') 
    for (provider in basemap_options) {
        baseMap <- baseMap %>% addProviderTiles(provider, group = provider)
    }
    
    baseMap <- baseMap %>% 
        addLayersControl(baseGroups = basemap_options,
                         options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) %>% 
        # add ca boundary; NOTE boundary is not actually drawn, only used to set initial view to California
        addPolygons(data = sf_ca_boundary, 
                    # options = pathOptions(pane = "ca_boundary"),
                    color = 'black',
                    weight = 0,
                    smoothFactor = 1.0,
                    opacity = 0,
                    fill = FALSE,
                    group = 'California Boundary',
                    label = 'California Boundary') %>% 
        # add the min-map window
        addMiniMap(tiles = basemap_options[[1]],
                   toggleDisplay = TRUE,
                   position = "bottomleft") %>% 
        # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
        onRender(
            "function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                    function (e) {
                    myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                    })
                    }"
        ) # base map complete 
    
    
    
    output$map1 <- renderLeaflet({
        baseMap1 <- baseMap
        # set drawing order
        baseMap1 <- baseMap1 %>%
            addMapPane('ca_boundary', zIndex = 410) %>%
            addMapPane("tribal_boundaries", zIndex = 420) %>%
            addMapPane('ca_mh_parks', zIndex=430) %>%
            {.}
        return(baseMap1)
        
    }) 
    
    output$map2 <- renderLeaflet({
        baseMap2 <- baseMap
        baseMap2 <- baseMap2 %>% #HERE!
            addLayersControl(
                baseGroups = basemap_options,
                overlayGroups = c('Well low-risk', 'Well med-risk',
                                  'Well high-risk', 'SSWS low-risk',
                                  'SSWS high-risk', 'SSWS unknown')
            )
        
        # set drawing order
        baseMap2 <- baseMap2 %>%
            addMapPane('ca_boundary', zIndex = 410) %>%
            addMapPane("tribal_boundaries", zIndex = 420) %>%
            addMapPane('ca_wells', zIndex=430) %>%
            addMapPane('ca_SSWS', zIndex=440) %>%
            {.}
        return(baseMap2)
    })
    
    output$map3 <- renderLeaflet({
        baseMap3 <- baseMap
        # set drawing order
        baseMap3 <- baseMap3 %>%
            addMapPane('ca_boundary', zIndex = 410) %>%
            addMapPane("tribal_boundaries", zIndex = 420) %>%
            addMapPane('ca_HWSS', zIndex=430) %>%
            addMapPane('ca_SAFER', zIndex=440) %>%
            addMapPane('ca_FHAB', zIndex=450) %>%
            {.}
        return(baseMap3)
        
    }) 
    
    # renders tab2 upon initialization of the app, otherwise data is not plotted on tab2 
    # until the user changes the tribal boundary
    # see: https://stackoverflow.com/questions/62700258/leaflet-in-another-tab-not-updated-with-leafletproxy-before-visiting-tab
    outputOptions(output, "map2", suspendWhenHidden = FALSE) #HERE!
    outputOptions(output, "map3", suspendWhenHidden = FALSE)
    
    
    ## --- A: Plot/Chart MHP ----
    observe({
        withProgress(message = 'Drawing Map', value = 1, style = 'notification', {
            
            ## add tribal boundaries
            leafletProxy('map1') %>%
                clearGroup('Tribal Boundaries') %>%
                addPolygons(data = sf_tribal_boundaries() %>% 
                                st_transform(crs = geographic_crs), 
                            options = pathOptions(pane = "tribal_boundaries"),
                            layerId = ~get(as_string(state$tribalIDField)),
                            color = '#947036',
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 0.9,
                            fill = TRUE,
                            fillOpacity = 0.5,
                            fillColor = '#947036',
                            highlightOptions = highlightOptions(color = "#00FFFF", weight = 2),
                            group = 'Tribal Boundaries',
                            label = ~get(as_string(state$tribalNameField))
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
            output$MHPBarChart <-renderPlotly({
                if(!is.null(sf_tribal_boundaries())) {
                    p <- ggplot(data=chart_data_mhp(), aes(x=reorder(!!state$tribalNameField, Count), y=Count, key=!!state$tribalIDField)) +
                        geom_bar(stat='identity', fill='#f49952') +
                        labs(x = "Year", y = state$tribalNameField) +
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
                    ggplotly(p, tooltip = c("y"), source = 'MHPchart')
                }
            })
            
            # Add table
            output$MHPTable <- renderDT(
                {dataset <- state$MHPTable},
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, 
                               scrollCollapse=TRUE,
                               dom = 'Bfrtip',
                               buttons = list(
                                   list(extend = 'collection',
                                        buttons = list(list(extend='csv',
                                                            filename = 'CA_Tribal_MobileHomeParks'),
                                                       list(extend='excel',
                                                            title = NULL,
                                                            filename = 'CA_Tribal_MobileHomeParks')
                                        ),
                                        text = 'Download Data'),
                                   'colvis')
                ),
                server = FALSE, ## NOTE: TRUE may not allow for download of the full file
                rownames = FALSE
            )
        })
    })
    
    ## --- B: Plot/Chart Wells+SSWS ----
    
    observe({
        # add tribal boundaries
        leafletProxy('map2') %>%
            clearGroup('Tribal Boundaries') %>%
            addPolygons(data = sf_tribal_boundaries() %>% st_transform(geographic_crs),
                        options = pathOptions(pane = "tribal_boundaries"),
                        layerId = ~get(as_string(state$tribalIDField)),
                        color = '#947036',
                        weight = 2.0,
                        smoothFactor = 1.0,
                        opacity = 0.9,
                        fill = TRUE,
                        fillOpacity = 0.5,
                        fillColor = '#947036',
                        highlightOptions = highlightOptions(color = "#00FFFF", weight = 2),
                        group = 'Tribal Boundaries',
                        label = ~get(as_string(state$tribalNameField))
            )
        
        # plot low-risk wells
        leafletProxy('map2') %>%
            clearGroup('Well low-risk') %>%
            addCircleMarkers(data = filter(intersectingWells(), risk == 'low'),
                             options = pathOptions(pane = "ca_wells"),
                             lng = ~gm_longitu,
                             lat = ~gm_latitud,
                             stroke= FALSE,
                             color = '#2b83ba',
                             fill = TRUE,
                             fillColor = '#2b83ba',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'Well low-risk',
                             label = ~paste0('Well - ', gm_well_ca), 
                             popup = ~paste0('<b>', '<span style="color:#2b83ba"> Well low-risk </span>', '</b>','<br/>',
                                             '<b>', 'Type: ', '</b>', gm_well_ca, '<br/>',
                                             '<b>', 'ID: ', '</b>', gm_well_id, '</br>'
                             )
            )
        
        # plot med-risk wells
        leafletProxy('map2') %>%
            clearGroup('Well med-risk') %>%
            addCircleMarkers(data = filter(intersectingWells(), risk == 'med'),
                             options = pathOptions(pane = "ca_wells"),
                             lng = ~gm_longitu,
                             lat = ~gm_latitud,
                             stroke= FALSE,
                             color = '#fdae61',
                             fill = TRUE,
                             fillColor = '#fdae61',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'Well med-risk',
                             label = ~paste0('Well - ', gm_well_ca),
                             popup = ~paste0('<b>', '<span style="color:#fdae61"> Well med-risk </span>', '</b>','<br/>',
                                             '<b>', 'Type: ', '</b>', gm_well_ca, '<br/>',
                                             '<b>', 'ID: ', '</b>', gm_well_id, '</br>'
                             )
            )
        
        # plot high-risk wells
        leafletProxy('map2') %>%
            clearGroup('Well high-risk') %>%
            addCircleMarkers(data = filter(intersectingWells(), risk == 'high'),
                             options = pathOptions(pane = "ca_wells"),
                             lng = ~gm_longitu,
                             lat = ~gm_latitud,
                             stroke= FALSE,
                             color = '#d7191c',
                             fill = TRUE,
                             fillColor = '#d7191c',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'Well high-risk',
                             label = ~paste0('Well - ', gm_well_ca),
                             popup = ~paste0('<b>', '<span style="color:#d7191c"> Well high-risk </span>', '</b>','<br/>',
                                             '<b>', 'Type: ', '</b>', gm_well_ca, '<br/>',
                                             '<b>', 'ID: ', '</b>', gm_well_id, '</br>'
                             )
                             
            )
        
        
        # plot unknown risk SSWS
        leafletProxy('map2') %>%
            clearGroup('SSWS unknown') %>%
            addCircleMarkers(data = filter(intersectingSSWS(), wqrskbn == 'unknown'),
                             options = pathOptions(pane = "ca_SSWS"),
                             stroke = FALSE,
                             color = '#878787',
                             lat = ~latitude,
                             lng = ~longitude,
                             fill = TRUE,
                             fillColor = '#878787',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'SSWS unknown',
                             label = 'SSWS', 
                             popup = ~paste0('<b>', '<span style="color:#878787"> SSWS unknown risk </span>', '</b>','<br/>',
                                             '<b>', 'Name: ', '</b>', system_nam, '<br/>',
                                             '<b>', 'Address: ', '</b>', address, '</br>',
                                             '<b>', 'County: ', '</b>', phys_count, '</br>',
                                             '<b>', 'Connections: ', '</b>', service_co, '<br/>',
                                             '<b>', 'Serves: ', '</b>', svc_area_t, '<br/>',
                                             '<b>', 'ID: ', '</b>', pwsid, '<br/>'
                             )
            )
        
        # plot low-risk SSWS
        leafletProxy('map2') %>%
            clearGroup('SSWS low-risk') %>%
            addCircleMarkers(data = filter(intersectingSSWS(), wqrskbn == 'low'),
                             options = pathOptions(pane = "ca_SSWS"),
                             stroke = FALSE,
                             color = '#2b83ba',
                             lat = ~latitude,
                             lng = ~longitude,
                             fill = TRUE,
                             fillColor = '#2b83ba',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'SSWS low-risk',
                             label = 'SSWS',
                             popup = ~paste0('<b>', '<span style="color:#2b83ba"> SSWS low-risk </span>', '</b>','<br/>',
                                             '<b>', 'Name: ', '</b>', system_nam, '<br/>',
                                             '<b>', 'Address: ', '</b>', address, '</br>',
                                             '<b>', 'County: ', '</b>', phys_count, '</br>',
                                             '<b>', 'Connections: ', '</b>', service_co, '<br/>',
                                             '<b>', 'Serves: ', '</b>', svc_area_t, '<br/>',
                                             '<b>', 'ID: ', '</b>', pwsid, '<br/>'
                                             
                             )
                             
            )
        
        # plot high-risk SSWS
        leafletProxy('map2') %>%
            clearGroup('SSWS high-risk') %>%
            addCircleMarkers(data = filter(intersectingSSWS(), wqrskbn == 'high'),
                             options = pathOptions(pane = "ca_SSWS"),
                             stroke = FALSE,
                             color = '#d7191c',
                             lat = ~latitude,
                             lng = ~longitude,
                             fill = TRUE,
                             fillColor = '#d7191c',
                             fillOpacity = 1,
                             radius = 3.5,
                             group = 'SSWS high-risk',
                             label = 'SSWS',
                             popup = ~paste0('<b>', '<span style="color:#d7191c"> SSWS high-risk </span>', '</b>','<br/>',
                                             '<b>', 'Name: ', '</b>', system_nam, '<br/>',
                                             '<b>', 'Address: ', '</b>', address, '</br>',
                                             '<b>', 'County: ', '</b>', phys_count, '</br>',
                                             '<b>', 'Connections: ', '</b>', service_co, '<br/>',
                                             '<b>', 'Serves: ', '</b>', svc_area_t, '<br/>',
                                             '<b>', 'ID: ', '</b>', pwsid, '<br/>'
                                             
                             )
            )
        
        # stacked well bar chart
        output$wellsBarChart <- renderPlotly({
            if(!is.null(sf_tribal_boundaries())) {
                # summarize well data by tribal land id and name
                # count the number of low, med, and high risk wells for each tribal land,
                tribalLandsWithWellCount <- intersectingWells() %>%
                    group_by("id" = !!state$tribalIDField, "name" = !!state$tribalNameField) %>%
                    summarize(High = sum(risk == 'high'),
                              Medium = sum(risk == 'med'),
                              Low = sum(risk == 'low'))
                
                
                # convert wide format to long format
                chartDataWells <- pivot_longer(tribalLandsWithWellCount, cols=c('High', 'Medium', "Low"), names_to="risk", values_to="count") 
                
                wellsChart <- chartDataWells %>%
                    mutate(risk = factor(x = risk, levels = c('High', 'Medium', 'Low'))) %>% 
                    ggplot(aes(x = reorder(name, count), y = count, fill = risk, key = id)) +
                    geom_bar(stat = 'identity') +
                    # add labels to axes
                    labs(x = 'Tribe', y = 'Number of Wells') +
                    # flip to make bars horizontal
                    coord_flip() +
                    # unsure how to center legend title; temporary solution is to manually add empty spaces
                    scale_fill_manual(values = c('#d7191c', '#fdae61', '#2b83ba'), guide = guide_legend(title = '        Risk')) + 
                    theme(
                        panel.grid.major.x = element_line(color = '#e3e3e3'), 
                        panel.grid.major.y = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        panel.background = element_rect(fill = 'transparent', color = 'transparent'),
                        plot.background = element_rect(fill = 'transparent', color = 'transparent')
                    )
                #charts are not drawn until user selects tab2
                #state$tab2Rendered = TRUE
                return(
                    ggplotly(wellsChart, tooltip = c("y"), source = 'wellsChart') %>% 
                        # removes toolbar that appears when user hovers over chart
                        # removing it as it does not add to the chart and it blocks the top-most bar from being clicked
                        config(displayModeBar = FALSE)
                )
            }
        })

        # Wells table
        output$wellsTable <- renderDT(
            datatable(state$wellsTable,
                      extensions = c('Buttons', 'Scroller'),
                      options = list(scrollX = TRUE,
                                     scrollY = '300px',
                                     scrollCollapse = TRUE,
                                     dom = 'Bfrtip',
                                     buttons =
                                         list(list(
                                             extend = 'collection',
                                             buttons = list(list(extend='csv',
                                                                 filename = 'CA_Tribal_Wells'),
                                                            list(extend='excel',
                                                                 title = NULL,
                                                                 filename = 'CA_Tribal_Wells')
                                             ),
                                             text = 'Download Data'),
                                             'colvis'
                                         )
                      ),
                      rownames = FALSE
            ) %>%
                # add padding to each row
                formatStyle(names(state$wellsTable), height=75),
            server = FALSE
        )
        
        
        # SSWS table
        output$SSWSTable <- renderDT(
            state$SSWSTable, 
            extensions = c('Buttons', 'Scroller'),
            options = list(scrollX = TRUE, 
                           scrollY = '300px', 
                           scrollCollapse = TRUE,
                           dom = 'Bfrtip',
                           buttons = list(
                               list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv',
                                                       filename = 'CA_Tribal_SSWS'),
                                                  list(extend='excel',
                                                       title = NULL,
                                                       filename = 'CA_Tribal_SSWS')),
                                   text = 'Download Data'),
                               'colvis')),
            server = FALSE, 
            rownames = FALSE
        )
        
        
        
        # stacked SSWS bar chart
        output$SSWSBarChart <- renderPlotly({
            if(!is.null(sf_tribal_boundaries())) {
                # summarize well data by tribal land id and name
                # count the number of unknown, low, med, and high risk ssws for each tribal land,
                tribalLandsWithSSWSCount <- intersectingSSWS() %>%
                    group_by("id" = !!state$tribalIDField, "name" = !!state$tribalNameField) %>%
                    summarize(High = sum(wqrskbn == 'high'),
                              Medium = sum(wqrskbn == 'med'),
                              Low = sum(wqrskbn == 'low'),
                              Unknown = sum(wqrskbn == 'unknown')
                    )
                
                
                # convert wide format to long format
                chartDataSSWS <- pivot_longer(tribalLandsWithSSWSCount, cols=c('High', 'Medium', "Low", 'Unknown'), names_to="risk", values_to="count")
                SSWSChart <- chartDataSSWS %>%
                    mutate(risk = factor(x = risk, levels = c('High', 'Medium', 'Low', 'Unknown'))) %>%
                    ggplot(aes(x = reorder(name, count), y = count, fill = risk, key = id)) +
                    geom_bar(stat = 'identity', width = 0.4) +
                    
                    # add labels to axes
                    labs(x = 'Tribe', y = 'Number of SSWS') +
                    # flip to make bars horizontal
                    coord_flip() +
                    scale_fill_manual(values = c('#d7191c', '#fdae61', '#2b83ba', '#bababa'), guide = guide_legend(title = '        Risk')) +
                    theme(
                        panel.grid.major.x = element_line(color = '#e3e3e3'), 
                        panel.grid.major.y = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        panel.background = element_rect(fill = 'transparent', color = 'transparent'),
                        plot.background = element_rect(fill = 'transparent', color = 'transparent')
                    )
                #charts are not drawn until user selects tab2
                state$tab2Rendered = TRUE
                return(
                    ggplotly(SSWSChart, tooltip = c("y"), source = 'SSWSChart') %>% 
                        # removes toolbar that appears when user hovers over chart
                        # removing it as it does not add to the chart and it blocks the top-most bar from being clicked
                        config(displayModeBar = FALSE)
                )
            }
        })
        
    })
    
    ## --- C: Plot/Chart New ----
    observe({
            ## add tribal boundaries
            leafletProxy('map3') %>%
                clearGroup('Tribal Boundaries') %>%
                addPolygons(data = sf_tribal_boundaries() %>% 
                                st_transform(crs = geographic_crs), 
                            options = pathOptions(pane = "tribal_boundaries"),
                            layerId = ~get(as_string(state$tribalIDField)),
                            color = '#947036',
                            weight = 2.0,
                            smoothFactor = 1.0,
                            opacity = 0.9,
                            fill = TRUE,
                            fillOpacity = 0.5,
                            fillColor = '#947036',
                            highlightOptions = highlightOptions(color = "#00FFFF", weight = 2),
                            group = 'Tribal Boundaries',
                            label = ~get(as_string(state$tribalNameField))
                )
            
            # Add household water supply shortage
            leafletProxy('map3') %>%
                clearGroup('CA HWSS') %>%
                addCircleMarkers(data = sf_ca_hwss() %>%
                                     st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
                                 options = pathOptions(pane = "ca_HWSS"),
                                 stroke= FALSE,
                                 color = '#f49952',
                                 fill = TRUE,
                                 fillColor = '#f49952',
                                 fillOpacity = 1,
                                 radius = 3.5,
                                 popup = ~paste0('<b>', '<span style="color:#f49952">Water Supply Shortage Report</span>', '</b>','<br/>',
                                                 '<b>', 'County: ', '</b>', county, '</br>',
                                                 '<b>', 'Shortage type: ', '</b>', shortage_type, '<br/>'
                                 ),
                                 group = 'CA HWSS',
                                 label = ~paste0(shortage_type)
                )
            
            # Add SAFER measures of drinking water systems
            leafletProxy('map3') %>%
                clearGroup('CA SAFER') %>%
                addCircleMarkers(data = sf_ca_SAFER() %>%
                                     st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
                                 options = pathOptions(pane = "ca_SAFER"),
                                 stroke= FALSE,
                                 color = '#76EA65',
                                 fill = TRUE,
                                 fillColor = '#76EA65',
                                 fillOpacity = 1,
                                 radius = 3.5,
                                 popup = ~paste0('<b>', '<span style="color:#76EA65">Water Supply Shortage Report</span>', '</b>','<br/>',
                                                 '<b>', 'Result: ', '</b>', risk_assessment_result, '</br>',
                                                 '<b>', 'Status: ', '</b>', final_safer_status, '<br/>',
                                                 '<b>', 'CalEnviroScreen Score: ', '</b>', calenviro_screen_score, '<br/>',
                                                 '<b>', 'Affordability Risk: ', '</b>', affordability_risk_level, '<br/>'
                                 ),
                                 group = 'CA SAFER',
                                 label = ~paste0(system_name)
                )
            
            # Add FHAB reports
            leafletProxy('map3') %>%
                clearGroup('CA FHAB') %>%
                addCircleMarkers(data = sf_ca_FHAB() %>%
                                     st_transform(crs = geographic_crs), # have to convert to geographic coordinate system for leaflet
                                 options = pathOptions(pane = "ca_FHAB"),
                                 stroke= FALSE,
                                 color = '#65DAEA',
                                 fill = TRUE,
                                 fillColor = '#65DAEA',
                                 fillOpacity = 1,
                                 radius = 3.5,
                                 popup = ~paste0('<b>', '<span style="color:#65DAEA">Freshwater Hamrful Algae Bloom Report</span>', '</b>','<br/>',
                                                 '<b>', 'Where: ', '</b>', official_water_body_name, '</br>',
                                                 '<b>', 'Last: ', '</b>', bloom_last_verified_on, '<br/>',
                                                 '<b>', 'Information: ', '</b>', incident_information, '<br/>'
                                 ),
                                 group = 'CA FHAB',
                                 label = ~paste0(water_body_type)
                )
            
            # Add bar chart showing tribal lands with hwss report counts
            output$HWSSBarChart <-renderPlotly({
                if(!is.null(sf_tribal_boundaries())) {
                    p <- ggplot(data=chart_data_hwss(), aes(x=reorder(!!state$tribalNameField, Count), y=Count, key=!!state$tribalIDField)) +
                        geom_bar(stat='identity', fill='#f49952') +
                        labs(x = "Year", y = state$tribalNameField) +
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
                    ggplotly(p, tooltip = c("y"), source = 'HWSSchart')
                }
            })
            
            # Add bar chart showing tribal lands with SAFER measures
            output$SAFERBarChart <-renderPlotly({
                if(!is.null(sf_tribal_boundaries())) {
                    p <- ggplot(data=chart_data_safer(), aes(x=reorder(!!state$tribalNameField, Count), y=Count, key=!!state$tribalIDField)) +
                        geom_bar(stat='identity', fill='#f49952') +
                        labs(x = "Year", y = state$tribalNameField) +
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
                    ggplotly(p, tooltip = c("y"), source = 'SAFERchart')
                }
            })
            
            # Add bar chart showing tribal lands with FHAB report counts
            output$FHABBarChart <-renderPlotly({
                if(!is.null(sf_tribal_boundaries())) {
                    p <- ggplot(data=chart_data_fhab(), aes(x=reorder(!!state$tribalNameField, Count), y=Count, key=!!state$tribalIDField)) +
                        geom_bar(stat='identity', fill='#f49952') +
                        labs(x = "Year", y = state$tribalNameField) +
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
                    ggplotly(p, tooltip = c("y"), source = 'FHABchart')
                }
            })
            
            # Add HWSS table #HERE!
            output$HWSSTable <- renderDT(
                {dataset <- state$HWSSTable},
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, 
                               scrollCollapse=TRUE,
                               dom = 'Bfrtip',
                               buttons = list(
                                   list(extend = 'collection',
                                        buttons = list(list(extend='csv',
                                                            filename = 'CA_Tribal_DryHouseholdWells'),
                                                       list(extend='excel',
                                                            title = NULL,
                                                            filename = 'CA_Tribal_DryHouseholdWells')
                                        ),
                                        text = 'Download Data'),
                                   'colvis')
                ),
                server = FALSE, ## NOTE: TRUE may not allow for download of the full file
                rownames = FALSE
            )
            
            # Add SAFER table #HERE!
            output$SAFERTable <- renderDT(
                {dataset <- state$SAFERTable},
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, 
                               scrollCollapse=TRUE,
                               dom = 'Bfrtip',
                               buttons = list(
                                   list(extend = 'collection',
                                        buttons = list(list(extend='csv',
                                                            filename = 'CA_Tribal_SAFER_DrinkingSystems'),
                                                       list(extend='excel',
                                                            title = NULL,
                                                            filename = 'CA_Tribal_SAFER_DrinkingSystems')
                                        ),
                                        text = 'Download Data'),
                                   'colvis')
                ),
                server = FALSE, ## NOTE: TRUE may not allow for download of the full file
                rownames = FALSE
            )
            
            # Add FHAB table #HERE!
            output$FHABTable <- renderDT(
                {dataset <- state$FHABTable},
                extensions = c('Buttons', 'Scroller'),
                options = list(scrollX = TRUE, 
                               scrollCollapse=TRUE,
                               dom = 'Bfrtip',
                               buttons = list(
                                   list(extend = 'collection',
                                        buttons = list(list(extend='csv',
                                                            filename = 'CA_Tribal_FHAB_Reports'),
                                                       list(extend='excel',
                                                            title = NULL,
                                                            filename = 'CA_Tribal_FHAB_Reports')
                                        ),
                                        text = 'Download Data'),
                                   'colvis')
                ),
                server = FALSE, ## NOTE: TRUE may not allow for download of the full file
                rownames = FALSE
            )
            state$tab3Rendered = TRUE
        })

    
    
    
    ## --- Tab2 and tab3 table functions ----
    
    # NOTE: tab2 needs callback function (relayWells + relaySSWS) in order to prevent warning 'The 'plotly_click' event tied a source ID of '___' is not registered'
    # the system throws warning when observeEvent searches for well bar chart info before well and SSWS charts are drawn
    # see: https://stackoverflow.com/questions/56273889/errors-with-plotly-click-in-r-plotly-version-4-9-0-is-there-a-bug-in-the-new-ve/
    
    ## Wells ----
    # --- handles click input on wells bar chart --- #HERE!
    relayWells <- reactive({
        req(state$tab2Rendered)
        event_data('plotly_click', source = 'wellsChart')
    })
    
    observeEvent(relayWells(), {
        d <- event_data("plotly_click", source = "wellsChart")
        if(!is.null(d$key)) {
            state$selectedTribeTab2Wells = d$key
        }
    })
    
    # Filter well table data, runs when user selects/clicks a tribal land feature (from map or bar chart)
    observeEvent(state$selectedTribeTab2Wells, {
        if(!is.null(state$selectedTribeTab2Wells)) {
            # Find wells in selected tribe
            state$wellsTable = filter(intersectingWells(), !!state$tribalIDField == state$selectedTribeTab2Wells)
        }
    })
    
    # --- # --- # 
    
    ## SWSS ----
    # handles click input on SSWS bar chart ---
    relaySSWS <- reactive({
        req(state$tab2Rendered)
        event_data('plotly_click', source = 'SSWSChart')
    })
    
    observeEvent(relaySSWS(), {
        d <- event_data("plotly_click", source = "SSWSChart")
        if(!is.null(d$key)) {
            state$selectedTribeTab2SSWS = d$key
        }
    })
    
    # Filter SSWS table data, runs when user selects/clicks a tribal land feature (from map or bar chart)
    observeEvent(state$selectedTribeTab2SSWS, {
        if(!is.null(state$selectedTribeTab2SSWS)) {
            # Find SSWS in selected tribe
            state$SSWSTable = filter(intersectingSSWS(), !!state$tribalIDField == state$selectedTribeTab2SSWS)
        }
    })
    
    # --- # --- #
    
    # All Tab2 (both wells+SSWS)
    # Filters the tables in map 2 to the tribal land the user clicked on #HERE!
    observeEvent(input$map2_shape_click, { 
        tribe <- input$map2_shape_click$id
        if(!is.null(tribe)) {
            # Set id to reactive value
            state$selectedTribeTab2Wells = tribe
            state$selectedTribeTab2SSWS = tribe
        }
    })
    
    
    
    # --- ######## --- #
    # --- NEW DATA --- #
    # --- ######## --- #
    
    # NOT FINISHED
    # ## HWSS ----
    # relayHWSS <- reactive({
    #     req(state$tab3Rendered)
    #     event_data('plotly_click', source = 'HWSSChart')
    # })
    # 
    # observeEvent(relayHWSS(), {
    #     d <- event_data("plotly_click", source = "HWSSChart")
    #     if(!is.null(d$key)) {
    #         state$selectedTribeTab3HWSS = d$key
    #     }
    # })
    # 
    # # Filter well table data, runs when user selects/clicks a tribal land feature (from map or bar chart)
    # observeEvent(state$selectedTribeTab3HWSS, {
    #     if(!is.null(state$selectedTribeTab3HWSS)) {
    #         # Find wells in selected tribe
    #         state$HWSSTable = filter(intersectingHWSS(), !!state$tribalIDField == state$selectedTribeTab3HWSS)
    #     }
    # })
    # 
    # # --- # --- #
    # 
    # # SAFER ----
    # # handles click input on SSWS bar chart ---
    # relaySAFER <- reactive({
    #     req(state$tab3Rendered)
    #     event_data('plotly_click', source = 'SAFERChart')
    # })
    # 
    # observeEvent(relaySAFER(), {
    #     d <- event_data("plotly_click", source = "SAFERChart")
    #     if(!is.null(d$key)) {
    #         state$selectedTribeTab3SAFER = d$key
    #     }
    # })
    # 
    # # Filter ssws table data, runs when user selects/clicks a tribal land feature
    # observeEvent(state$selectedTribeTab3SAFER, {
    #     if(!is.null(state$selectedTribeTab3SAFER)) {
    #         # Find ssws in selected tribe
    #         state$SAFERTable = filter(intersectingSAFER(), !!state$tribalIDField == state$selectedTribeTab3SAFER)
    #     }
    # 
    # })
    # 
    # # --- # --- #
    # 
    # # Both HWSS+SAFER
    # # filters the table in map 3 to the tribal land the user clicked on #HERE!
    # observeEvent(input$map3_shape_click, {
    #     tribe <- input$map3_shape_click$id
    #     if(!is.null(tribe)) {
    #         # Set id to reactive value
    #         state$selectedTribeTab3HWSS = tribe
    #         state$selectedTribeTab3SAFER = tribe
    #         state$selectedTribeTab3FHAB = tribe
    # 
    #     }
    # })
    
    
    
    
} # end of server


# 5 - run app -----------------------------------------------------------------
shinyApp(ui, server)
