library(sf)
library(tmap)
library(ggplot2)
library(tidyverse)
library(shinydashboard)

BCRs <- st_read('maps/pba_ecozones.shp') %>% 
    filter(!BCR %in% c('BCR10','BCR11')) %>%
    mutate(bcr = substr(BCR, 4, 5), ecozone=NULL, code=NULL, BCR=NULL, sort=NULL)
x <- read_csv('rep.csv')
features <- c('Caribou','AllBirds','ForestBirds','ConiferBirds','DeciduousBirds','MixedwoodBirds','GrasslandBirds','NeoMigrantBirds','ShortMigrantBirds','NomadicBirds','ResidentBirds','DecliningBirds','LowConcernBirds','AllWaterfowl','CavityNesters','GroundNesters','OverwaterNesters','BLBW','BOCH','BRCR','BTNW','CAWA','CMWA','OSFL','PIGR','RUBL','SWTH','WWCR')
opts <- tmap_options(basemaps = c(Canvas = "Esri.NatGeoWorldMap", Imagery = "Esri.WorldImagery"))

ui <- dashboardPage(
	dashboardHeader(title="Surrogates Evaluation"),
    dashboardSidebar(
	    sidebarMenu(
            menuItem("Study region", tabName="one", icon=icon("th")),
            menuItem("Violin plots", tabName="two", icon=icon("th")),
            menuItem("Case study", tabName="three", icon=icon("th")),
            selectInput("feature1", label="Feature 1:", choices=features, multiple=FALSE, selected="ConiferBirds"),
            selectInput("feature2", label="Feature 2:", choices=features, multiple=FALSE, selected="DeciduousBirds"),
            hr(),
            actionButton("analyse", "Create plots...")
        )
    ),
	dashboardBody(
		tabItems(
            tabItem(tabName="one",
                fluidRow(
                    tabBox(
                        id="tb1", width="12",
                        tabPanel("BCR map", tmapOutput("map", height='600'))
                    )
                )
            ),
            tabItem(tabName="two",
                fluidRow(
                    tabBox(
                        id="p1", width="12",
                        tabPanel("Feature 1", plotOutput("plot1"))
                    ),
                    tabBox(id="p2", width="12",
                        tabPanel("Feature 2", plotOutput("plot2"))
                    )
                )
            ),
            tabItem(tabName="three",
                fluidRow(
                    tabBox(
                        id="tb1", width="12",
                        tabPanel("Ecoregion 89", htmlOutput('inc'))
                    )
                )
            )
        )
    )
)

server <- function(input, output, session) {

    getPage<-function() {
        return(includeHTML("case_study_89.html"))
    }
    
    output$inc<-renderUI({getPage()})

    output$map <- renderTmap({
            tm_shape(BCRs) +  tm_polygons(col='bcr', alpha=0.5)
    })

    x1 <- eventReactive(input$analyse, {
        y = filter(x, species==input$feature1)
    })

    x2 <- eventReactive(input$analyse, {
        y = filter(x, species==input$feature2)
    })

    output$plot1 <- renderPlot({
        ggplot(x1(), aes(Networks, Dissimilarity)) +
            geom_violin(aes(fill=Networks)) +
            stat_summary(fun.y=median, geom="point", size=2, color="black") +
            geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5) +
            scale_fill_manual(values=c('#f0f0f0','#bdbdbd','#636363')) +
            facet_grid(species ~ bcr)
    }, res=96)

    output$plot2 <- renderPlot({
        ggplot(x2(), aes(Networks, Dissimilarity)) +
            geom_violin(aes(fill=Networks)) +
            stat_summary(fun.y=median, geom="point", size=2, color="black") +
            geom_hline(yintercept=0.2, linetype="dashed", colour="black", size=0.5) +
            scale_fill_manual(values=c('#f0f0f0','#bdbdbd','#636363')) +
            facet_grid(species ~ bcr)
    }, res=96)

}

shinyApp(ui, server)
