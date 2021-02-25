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
library(dplyr)


# load data ----
idaho <- readRDS('data/idaho.rds')
quads <- readRDS('data/quads.rds')
exmpDoc <- 'data/rrcr_example.html'
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
                             h6(HTML('<p style="color:red"><em><b>Update Feb 25, 2021: </b></em>Currently, the download 
                             button will only link to the NKN archive website. Once the data is fully loaded on the NKN 
                             site, I will update the download functionality.</p>')),
                             uiOutput(outputId='dl_button', inline=T),
                             br(),
                             textOutput(outputId='dl_size', inline=T),
                             br(),
                             textOutput(outputId='dk_size', inline=T),
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
        ),
        tabPanel('Example Data', value='ex_tab',
             sidebarLayout(
                 sidebarPanel(
                     fluidRow(
                         column(width=12,
                                h4(HTML('<b>Rinker Rock Creek Ranch</b>')),
                                h5(HTML('<i>Rinker Rock Creek Ranch is a research, education, and outreach facility
                                        located near Hailey, ID.</i><br/><br/>
                                        We applied species distribution models from McCarely et al. 2020
                                        using distal and proximal variables to the habitat patches for Rinker Rock
                                        Creek Ranch and some of the surrounding area to give an example of the general
                                        size and structure of the habiat patches in the data repository.')),
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
                     includeHTML(exmpDoc)
                 )
             )
        )
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
    click_list <- reactiveValues(ids=vector())  
    
    observeEvent(input$dl_map_shape_click, {
        click <- input$dl_map_shape_click
        proxy <- leafletProxy('dl_map')
        sel_q <- quads[quads$UID %in% click$id, ]
        
        # deselect or select
        if (click$id %in% click_list$ids){
            click_list$ids <- base::setdiff(click_list$ids, click$id)
            proxy %>% 
                removeShape(layerId=click$id)%>% 
                addPolygons(data=sel_q$geometry, weight=0.2, fillColor='transparent', color='black', layerId=sel_q$UID)
        } else{
            click_list$ids <- c(click_list$ids, click$id)
            proxy %>% 
                addPolygons(data=sel_q$geometry, weight=1, fillColor='transparent', color='blue', layerId=sel_q$UID)
        }
    })
    
    # make table
    output$table <- renderTable({
        return(subset(qTBL[,1:4], UID %in% click_list$ids))
    })
    
    # add download button
    output$dl_button <- renderUI({
        if (length(click_list$ids)==0) {
            return(NULL)
        }
        if (length(click_list$ids)==1){
            return(
                tagList(
                    actionButton(inputId='dl',
                                 label=paste0('Download q',click_list$ids,'.zip'),
                                 onclick="window.open('https://data.nkn.uidaho.edu/dataset/fine-scale-habitat-patches-idaho-attributed-climatic-topographic-soil-vegetation-and')",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
            )
        }
        if (length(click_list$ids)>1){
            return(
                tagList(
                    actionButton(inputId='dl',
                                 label='Download Multiple *.zip',
                                 onclick="window.open('https://data.nkn.uidaho.edu/dataset/fine-scale-habitat-patches-idaho-attributed-climatic-topographic-soil-vegetation-and')",
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
            )
        }
    })
        
    # add download size
    output$dl_size <- renderText({
        if (length(click_list$ids)==0) {
            return('None selected')
        }
        return(subset(qTBL, UID %in% click_list$ids) %>%
                   summarise(sizemb=sum(Mb_zip),
                             sizegb=sizemb*0.001,
                             out=ifelse(sizegb<1.0, paste0(round(sizemb,1),' Mb'), paste0(round(sizegb,1),' Gb'))) %>%
                   pull(out) %>% 
                   paste0('Zipped: ',.)
        )
            
    })
    
    # add disk size
    output$dk_size <- renderText({
        if (length(click_list$ids)==0) {
            return(invisible(NULL))
        }
        return(subset(qTBL, UID %in% click_list$ids) %>%
                   summarise(sizemb=sum(Mb_shp),
                             sizegb=sizemb*0.001,
                             out=ifelse(sizegb<1.0, paste0(round(sizemb,1),' Mb'), paste0(round(sizegb,1),' Gb'))) %>%
                   pull(out) %>% 
                   paste0('Unzipped: ',.)
        )
        
    })

    # # map for example area
    # output$ex_map <- renderLeaflet({
    #     leaflet(options=leafletOptions(minZoom=12), height='100%') %>%
    #         addTiles() %>%
    #         addPolygons(data=rrcr$geometry, weight=2, fillColor='transparent', color='black') %>%
    #         # addPolygons(data=polygons$geometry, weight=0.2, fillColor='transparent', color='black') %>%
    #         # addPolygons(data=quads$geometry, weight=1, fillColor='transparent', color='grey30',
    #         #             highlightOptions=highlightOptions(color='yellow', weight=2, bringToFront=T),
    #         #             #popup=popupTable(dft, row.numbers=F, feature.id=F),
    #         #             label=quads$NAME, layerId=quads$UID) %>%
    #         addScaleBar('bottomright', options=scaleBarOptions(maxWidth=300, metric=T, imperial=T)) %>%
    #         setView(-114.3988, 43.3972, zoom=12) %>%
    #         setMaxBounds(-114.64, 43.23, -114.15, 43.56)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
