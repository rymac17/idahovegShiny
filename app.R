#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.providers)
library(sf)


# load data ----
idaho <- readRDS('data/idaho.rds')
quads <- readRDS('data/quads.rds')
rrcr <- readRDS('data/rrcr.rds')
# polygons <- readRDS('data/polygons.rds')
# polyTBL <- readRDS('data/polyTBL.rds')
qTBL <- quads %>% st_drop_geometry()

# user interface ----
ui <- fluidPage(
    # tags$style('.container-fluid {background-color: #999999; color: #000000;}'),
    
    h2(HTML('<b>Fine-scale habitat patches of Idaho attributed with climatic, 
            topographic, soil, vegetation, and disturbance variables</b>')),
    h4(HTML('<i>Interactive data repository for "Predicting fine-scale forage species to inform ungulate nutrition"</i>')),
    
    tabsetPanel(id='tab_display',
        tabPanel('Data Download', value='dl_tab',
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(
                         column(width=12, 
                             h4(HTML('<b>Select quadrangle on map to download its associated shapefile</b>')),
                             br(),
                             div(style='height: calc(100vh*.3); overflow-y: scroll',
                                 tableOutput(outputId='table')
                             ),
                             uiOutput(outputId='dl_button', inline=T),
                             br(),
                             br(),
                             h6(HTML('The stable repository for this data can be found on the
                             <a href="https://data.nkn.uidaho.edu/dataset/fine-scale-habitat-patches-idaho-attributed-climatic-topographic-soil-vegetation-and">
                             Northwest Knowledge Network.</a><br/><br/>
                             These data were developed for open access and can be used without additional permissions or fees
                             under creative commons license CC BY 4.0. If you use these data in a publication, presentation,
                             or other research product please use the following citation:<br/><br/>
                             McCarley, T. Ryan; Ball, Tara M.; Aycrigg, Jocelyn L.; Strand, Eva K.; Svancara, Leona K.;
                             Horne, Jon S.; Johnson, Tracey N.; Lonneker, Meghan K.; Hurley, Mark. 2020.
                             Predicting fine-scale forage distribution to inform ungulate nutrition.
                             Ecological Informatics 60, 101170. Doi:10.1016/j.ecoinf.2020.101170.'))
                             )
                     )
                 ),
                 mainPanel(
                     leafletOutput('dl_map'),
                     tags$style(type = "text/css", "#dl_map {height: calc(100vh - 100px) !important;}")
                 )
             )
        )
        # tabPanel('Example Data', value='ex_tab',
        #      sidebarLayout(
        #          sidebarPanel(
        #              fluidRow(
        #                  column(width=12,
        #                         h4(HTML('<b>Rinker Rock Creek Ranch</b>')),
        #                         h5(HTML('<i>Rinker Rock Creek Ranch is a research, education, and outreach facility 
        #                                 located near Hailey, ID.</i><br/><br/>
        #                                 We applied species distribution models from McCarely et al. 2020
        #                                 using distal and proximal variables to the habitat patches for Rinker Rock
        #                                 Creek Ranch and some of the surrounding area to give an example of the general
        #                                 size and structure of the habiat patches in the data repository.')),
        #                         br()
        #                         # div(style='height: calc(100vh*.3); overflow-y: scroll',
        #                         #     tableOutput(outputId='table')
        #                         # ),
        #                         # uiOutput(outputId='dl_button', inline=T),
        #                  )
        #              )
        #          ),
        #          mainPanel(
        #              leafletOutput('ex_map'),
        #              tags$style(type = "text/css", "#ex_map {height: calc(100vh - 100px) !important;}")
        #          )
        #      )
        # )
    )
)

# server ---
server <- function(input, output) {
    
    # main map to show
    output$dl_map <- renderLeaflet({
        leaflet(options=leafletOptions(minZoom=6, maxZoom=13)) %>%
            addTiles(group='base') %>%
            addPolygons(data=idaho$geometry, weight=2, fillColor='transparent', color='black') %>% 
            addPolygons(data=quads$geometry, weight=0.2, fillColor='transparent', color='black',
                        highlightOptions=highlightOptions(color='yellow', weight=2, bringToFront=T),
                        #popup=popupTable(dft, row.numbers=F, feature.id=F),
                        label=quads$NAME, layerId=quads$UID) %>%
            addScaleBar('bottomright', options=scaleBarOptions(maxWidth=300, metric=T, imperial=T)) %>% 
            addProviderTiles("Esri.WorldImagery", group='imagery') %>%
            addLayersControl(baseGroups=c('base','imagery'), options=layersControlOptions(collapsed=T)) %>% 
            setMaxBounds(-120, 38, -108, 52)
    })
    
    # create a reactive value that will store the click position
    data_of_click <- reactiveValues(clickedQuad=NULL)
    
    observeEvent(input$dl_map_shape_click, {
        # if (input$tab_display == 'Data Download'){
            data_of_click$clickedQuad <- input$dl_map_shape_click    
        # }
    })
    
    # make table
    output$table <- renderTable({
        if (is.null(data_of_click$clickedQuad)) {
            return(NULL)
        }
        return(
            subset(qTBL, UID==data_of_click$clickedQuad$id)
        )
    })
    
    # add download button
    output$dl_button <- renderUI({
        if (is.null(data_of_click$clickedQuad)) {
            return(NULL)
        }
        return(
            tagList(
                actionButton(inputId='dl',
                             label=paste0('Download q',data_of_click$clickedQuad$id,'.zip'),
                             onclick="window.open('http://google.com', '_blank')",
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
        )
    })
    
    # map for example area
    output$ex_map <- renderLeaflet({
        leaflet(options=leafletOptions(minZoom=12), height='100%') %>%
            addTiles() %>%
            addPolygons(data=rrcr$geometry, weight=2, fillColor='transparent', color='black') %>%
            # addPolygons(data=polygons$geometry, weight=0.2, fillColor='transparent', color='black') %>%
            # addPolygons(data=quads$geometry, weight=1, fillColor='transparent', color='grey30',
            #             highlightOptions=highlightOptions(color='yellow', weight=2, bringToFront=T),
            #             #popup=popupTable(dft, row.numbers=F, feature.id=F),
            #             label=quads$NAME, layerId=quads$UID) %>%
            addScaleBar('bottomright', options=scaleBarOptions(maxWidth=300, metric=T, imperial=T)) %>%
            setView(-114.3988, 43.3972, zoom=12) %>%
            setMaxBounds(-114.64, 43.23, -114.15, 43.56)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
