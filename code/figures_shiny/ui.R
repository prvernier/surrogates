source("global.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Surrogates analysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width=3,

    # Input: Slider for the number of bins ----
    p("Results are presented as side-by-side pairs of maps and histograms for ecozones, ecoprovinces, and ecoregions."),
    p("MAPS - the maps show the distribution of model R2 and RSE values for a given species, including two groups: all birds, and forest birds."),
    p("PLOTS - the histograms shows the distribution of all model R2 and RSE values across all species and species groups in light grey - and values for the selected species or species group in dark grey."),
    hr(),
    selectInput(inputId = "species", label = "Select species:", choices = c("all_birds","for_birds","caribou","boch","brcr","btnw","cawa","cmwa","pigr","rubl","swth","wwcr"), selected = "all_birds"), 
    #selectInput(inputId = "indicator", label = "Select model parameter:", choices = c("m1_r2"), selected = "m1_r2"), 
    sliderInput(inputId = "bins", label = "Number of histogram bins:", min = 1, max = 30, value = 10),
    hr(),
    p("NOTES:"),
    p("* Model R2 measures the strength of the relationship - higher values indicate stronger relationship"),
    p("* Model RSE measures the lack of fit of the model to the data - higher values indicates poorer fit.")

    ),

    # Main panel for displaying outputs ----
  mainPanel(
    tabsetPanel(
      tabPanel("Maps",
        #column(width="100%",
          imageOutput("distMap")
        #  )
        ),
      tabPanel("Plots",
        column(width=5,
          plotOutput("distPlot1", height=300),
          plotOutput("distPlot2", height=300),
          plotOutput("distPlot3", height=300)
          ),
        column(width=5,
          plotOutput("distPlot4", height=300),
          plotOutput("distPlot5", height=300),
          plotOutput("distPlot6", height=300)
          )
        )
      )
    )
  )
)
