server <- function(input, output) {

  output$distMap <- renderImage({
    
    list(src = paste0("png/",input$species,".png"), contentType = "image/png", alt = paste(toupper(input$species),"- no data"), width=800, height=800)
    }, deleteFile=FALSE)

  output$distPlot1 <- renderPlot({
    #ecozz = filter(ecoz, !species %in% c("all_birds","for_birds"))
    x = pull(ecoz, m1_r2)
    x = x[!is.na(x)]
    ecoz2 = filter(ecoz, species==input$species)
    x2 = pull(ecoz2, m1_r2)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, xlim=range(0,1), col = "#cccccc", border = "white", xlab = "Model R2", main=paste("Ecozones -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col = scales::alpha("#636363",1), border = "white", add=T)
    })

  output$distPlot2 <- renderPlot({
    #ecopp = filter(ecop, !species %in% c("all_birds","for_birds"))
    x = pull(ecop, m1_r2)
    x = x[!is.na(x)]
    ecop2 = filter(ecop, species==input$species)
    x2 = pull(ecop2, m1_r2)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, xlim=range(0,1), col = "#cccccc", border = "white", xlab = "Model R2", main=paste("Ecoprovinces -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col=scales::alpha("#636363",1), border = "white", main="", add=T)
    })

  output$distPlot3 <- renderPlot({
    #ecorr = filter(ecor, !species %in% c("all_birds","for_birds"))
    x = pull(ecor, m1_r2)
    x = x[!is.na(x)]
    ecor2 = filter(ecor, species==input$species)
    x2 = pull(ecor2, m1_r2)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#cccccc", border = "white", xlab = "Model R2", main=paste("Ecoregions -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col=scales::alpha("#636363",1), border = "white", main="", add=T)
    })

  output$distPlot4 <- renderPlot({
    #ecozz = filter(ecoz, !species %in% c("all_birds","for_birds"))
    x = pull(ecoz, m1_rse)
    x = x[!is.na(x)]
    ecoz2 = filter(ecoz, species==input$species)
    x2 = pull(ecoz2, m1_rse)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, xlim=range(0.02,0.14), col = "#cccccc", border = "white", xlab = "Model RSE", main=paste("Ecozones -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col = scales::alpha("#636363",1), border = "white", add=T)
    })

  output$distPlot5 <- renderPlot({
    #ecopp = filter(ecop, !species %in% c("all_birds","for_birds"))
    x = pull(ecop, m1_rse)
    x = x[!is.na(x)]
    ecop2 = filter(ecop, species==input$species)
    x2 = pull(ecop2, m1_rse)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#cccccc", border = "white", xlab = "Model RSE", main=paste("Ecoprovinces -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col=scales::alpha("#636363",1), border = "white", main="", add=T)
    })

  output$distPlot6 <- renderPlot({
    #ecorr = filter(ecor, !species %in% c("all_birds","for_birds"))
    x = pull(ecor, m1_rse)
    x = x[!is.na(x)]
    ecor2 = filter(ecor, species==input$species)
    x2 = pull(ecor2, m1_rse)
    x2 = x2[!is.na(x2)]
    bins = seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#cccccc", border = "white", xlab = "Model RSE", main=paste("Ecoregions -", toupper(input$species), "(dark grey)"))
    hist(x2, breaks = bins, col=scales::alpha("#636363",1), border = "white", main="", add=T)
    })

}