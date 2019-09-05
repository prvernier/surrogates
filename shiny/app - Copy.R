library(sf)
library(caret)
library(shiny)
library(leaflet)
library(lattice)
library(tidyverse)

load("ks_mix.Rdata")
#ks = ks_mix
ks = mutate(ks_mix, ecozone=as.character(substr(ecozone,2,nchar(ecozone)))) %>% filter(rep==0 | rep==1)
zone = select(ks, ecozone, ecoregion, intact_eco) %>% unique()
load("ks_repnorep.Rdata")
ks_rnr = mutate(ks_repnorep, ecozone=as.character(substr(ecozone,2,nchar(ecozone))))
load("stats.Rdata")
load("ecor_maps.Rdata")
songbirds = c('BLBW','BOCH','BRCR','BTNW','CAWA','CMWA','OSFL','PIGR','RUBL','SWTH','WWCR')

ui = fluidPage(
	
    headerPanel("Surrogates Evaluation"),
	
    sidebarLayout(
		
        sidebarPanel(width=3,
            selectInput(inputId = "species",
				label = "Select species:",
				choices = c('ALLBIRDS','FORESTBIRDS',songbirds,'ALLWATERFOWL','CAVITYNESTERS','GROUNDNESTERS','OVERWATERNESTERS'),
				selected = "CAWA"),
            hr(),
            #sliderInput(inputId = "ks", "Maximum ratio for rep/nonrep or nonrep/rep networks:", min = 0, max = 100, value = c(100), step=10),
            #hr(),
            checkboxInput("repnorep", "Use only rep and nonrep networks"),
            checkboxInput("rep", "Use binary variable for surrogates", FALSE),
            checkboxInput("ave", "Show averages at bottom of table", FALSE),
            hr(),
            downloadButton('downloadData',"Download data")
       ),
	
    mainPanel(
		tabsetPanel(
            tabPanel("Regression analysis",
                tableOutput("tab1")
                ),
            tabPanel("Map output",
                br(),
                leafletOutput("ecormap", height=600)
                ),
            #tabPanel("Fitted table",
            #    tableOutput("tab2")
            #    ),
            tabPanel("Plot output",
                br(),
                plotOutput("plot1")
                #plotOutput("plot2")
                ),
            tabPanel("Definitions",
                br(),
                htmlOutput("methods")
                )
            )
        )
    )
)

server = function(input, output) {

    testdata <- reactive({
        
        # assign species string to a new variable 'test' in ks table
        spp = tolower(input$species)
        assign("tmp", ks[[spp]])
        ks = mutate(ks, test = tmp)
        assign("tmp", ks_rnr[[spp]])
        ks_rnr = mutate(ks_rnr, test = tmp)
        
        # create empty summary table
        z = tibble(Ecozone=as.character(), Ecoregion=as.character(), Intactness=as.numeric(), Networks=as.integer(), CMI=as.character(), 
            GPP=as.character(), LED=as.character(), LCC=as.character(), R2 = as.numeric(), RMSE = as.numeric())
        i = 1

        # individual species or groups of species
        if (input$species %in% songbirds) {
            eco_list = stats$zone[stats$species==spp & stats$pct>0]
        } else {
            eco_list = unique(ks$ecoregion)
        }

        # run models for each ecoregion
        for (eco in eco_list) {
            if (input$repnorep==TRUE) {
                x = filter(ks_rnr, ecoregion==eco)
            } else {
                x = filter(ks, ecoregion==eco)
            }
            if (sum(!is.na(x[spp])) > 2) {
                
                # filter networks
                x = filter(x, !is.na(x[[spp]]))
                
                # select model
                if (input$repnorep==TRUE & input$rep==TRUE) {
                    m1 = lm(test ~ rep, data=x)
                } else {
                    m1 = lm(test ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=x)
                }
                
                # calculate variable importance
                vi = varImp(m1)

                # generate summary table
                z[i,"Ecozone"] = zone$ecozone[zone$ecoregion==eco]
                z[i,"Ecoregion"] = eco
                z[i,"Intactness"] = zone$intact_eco[zone$ecoregion==eco]
                rnet1 = length(x$rep[x$rep==1])
                rnet0.5 = length(x$rep[x$rep==0.5])
                rnet0 = length(x$rep[x$rep==0])
                if (input$repnorep==FALSE) {
                    #z[i,"Networks"] = paste0("allnets=",nrow(x))
                    z[i,"Networks"] = paste0("rep=",rnet1,", mix=",rnet0.5,", nrep=",rnet0)
                } else {
                    z[i,"Networks"] = paste0("rep=",rnet1,", nrep=",rnet0)
                }
                z[i,"CMI"] = paste0(sprintf("%.3f",summary(m1)$coefficients[,1]["ks_cmi"])," (",sprintf("%.2f",vi[1,1]),")")
                z[i,"GPP"] = paste0(sprintf("%.3f",summary(m1)$coefficients[,1]["ks_gpp"])," (",sprintf("%.2f",vi[2,1]),")")
                z[i,"LED"] = paste0(sprintf("%.3f",summary(m1)$coefficients[,1]["ks_led"])," (",sprintf("%.2f",vi[3,1]),")")
                z[i,"LCC"] = paste0(sprintf("%.3f",summary(m1)$coefficients[,1]["bc_lcc"])," (",sprintf("%.2f",vi[4,1]),")")
                z[i,"R2"] = sprintf("%.3f",summary(m1)$r.squared)
                z[i,"RMSE"] = sprintf("%.3f",modelr::rmse(m1,x))
                # calculate average values
                if (input$ave==TRUE) {
                    if (i==1) {
                        sum_cmi = summary(m1)$coefficients[,1]["ks_cmi"]
                        sum_cmi_vi = vi[1,1]
                        sum_gpp = summary(m1)$coefficients[,1]["ks_gpp"]
                        sum_gpp_vi = vi[2,1]
                        sum_led = summary(m1)$coefficients[,1]["ks_led"]
                        sum_led_vi = vi[3,1]
                        sum_lcc = summary(m1)$coefficients[,1]["bc_lcc"]
                        sum_lcc_vi = vi[4,1]
                        sum_r2 = summary(m1)$r.squared
                        sum_rmse = modelr::rmse(m1,x)
                    } else {
                        sum_cmi = sum_cmi + summary(m1)$coefficients[,1]["ks_cmi"]
                        sum_cmi_vi = sum_cmi_vi + vi[1,1]
                        sum_gpp = sum_gpp + summary(m1)$coefficients[,1]["ks_gpp"]
                        sum_gpp_vi = sum_gpp_vi + vi[2,1]
                        sum_led = sum_led + summary(m1)$coefficients[,1]["ks_led"]
                        sum_led_vi = sum_led_vi + vi[3,1]
                        sum_lcc = sum_lcc + summary(m1)$coefficients[,1]["bc_lcc"]
                        sum_lcc_vi = sum_lcc_vi + vi[4,1]
                        sum_r2 = sum_r2 + summary(m1)$r.squared
                        sum_rmse = sum_rmse + modelr::rmse(m1,x)
                    }
                }
                i = i + 1
            }
        }
        if (input$ave==TRUE) {
            z[i,"Ecozone"] = ""
            z[i,"Ecoregion"] = ""
            z[i,"Networks"] = ""
            z[i,"CMI"] = paste0(sprintf("%.3f",sum_cmi/(i-1))," (",sprintf("%.2f",sum_cmi_vi/(i-1)),")")
            z[i,"GPP"] = paste0(sprintf("%.3f",sum_gpp/(i-1))," (",sprintf("%.2f",sum_gpp_vi/(i-1)),")")
            z[i,"LED"] = paste0(sprintf("%.3f",sum_led/(i-1))," (",sprintf("%.2f",sum_led_vi/(i-1)),")")
            z[i,"LCC"] = paste0(sprintf("%.3f",sum_lcc/(i-1))," (",sprintf("%.2f",sum_lcc_vi/(i-1)),")")
            z[i,"R2"] = sprintf("%.3f",sum_r2/(i-1))
            z[i,"RMSE"] = sprintf("%.3f",sum_rmse/(i-1))
        }
        return(z)
   })

    output$tab1 <- renderTable({
        z = testdata()
    })


    testdata2 <- reactive({
        
        spp = tolower(input$species)
        assign("tmp", ks[[spp]])
        ks = mutate(ks, test = tmp)
        assign("tmp", ks_rnr[[spp]])
        ks_rnr = mutate(ks_rnr, test = tmp)

        if (input$species %in% songbirds) {
            eco_list = stats$zone[stats$species==spp & stats$pct>0]
        } else {
            eco_list = unique(ks_rnr$ecoregion)
        }
        i = 1
        for (eco in eco_list) {
            if (input$repnorep==TRUE) {
                x = filter(ks_rnr, ecoregion==eco)
            } else {
                x = filter(ks, ecoregion==eco)
            }
            if (sum(!is.na(x[spp])) > 2) {
                x = filter(x, !is.na(x[[spp]]))
                m1 = lm(test ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=x)
                x$fitted = fitted(m1)
            }
            if (i==1) {
                x1 = rep(eco, nrow(x))
                x2 = x$test
                x3 = x$fitted
            } else {
                x1 = c(x1, rep(eco, nrow(x)))
                x2 = c(x2, x$test)
                x3 = c(x3, x$fitted)
            }
            i = i + 1
        }
        z = tibble(ecoregion=x1, observed=x2, fitted=x3)
        return(z)
   })

    #output$tab2 <- renderTable({
    #    xyz = testdata2()
    #})

    output$plot1 <- renderPlot({
        z = testdata2()
        panel.smoother <- function(x, y) {
          panel.xyplot(x, y, col="blue", pch=1) # show points 
          panel.loess(x, y, col="red", lwd=2)  # show smoothed line 
          panel.lines(x=0:1, y=0:1, lty=2, col="black")
          #panel.lines(x=0:1, y=0.2, lty=2, col="black")
          #panel.lines(y=0:1, x=0.2, lty=2, col="black")
        }
        xyplot(z$observed~z$fitted|as.factor(z$ecoregion), 
            panel=panel.smoother,
            main=paste0(input$species,": Scatterplots by Ecoregion"), ylab="Observed KS values", xlab="Fitted KS values")
    })

    output$plot2 <- renderPlot({
        z = testdata()
        plot(z$Intactness, z$R2)
        abline(lm(z$R2 ~ z$Intactness), col="red")
        #lines(lowess(z$Intactness, z$R2), col="blue")
    }, width=600)

    output$downloadData <- downloadHandler(
        filename = function() {
            if (input$repnorep==FALSE) {
                paste(input$species,"-AllNets-", Sys.Date(), ".csv", sep="")
            } else {
                paste(input$species,"-RepNonRepNets-", Sys.Date(), ".csv", sep="")
            }
        },
        content = function(file) {
          outputdata = testdata() %>% filter(!Ecozone=="")
          write_csv(outputdata, file)
        }
    )

    output$ecormap <- renderLeaflet({
        z = testdata() %>% mutate(Ecoregion=as.numeric(Ecoregion), Species=as.numeric(R2))
        ecor_maps = left_join(ecor_maps, z, by=c("ecoreg"="Ecoregion"))
        i = ecor_maps[["Species"]]
        bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
        pal <- colorBin("YlOrRd", domain = i, bins = bins) #, na.color = "transparent")
        ecoPopup = paste0("Ecozone: ",ecor_maps$ecozone,"<br>Ecoregion: ",ecor_maps$ecoreg,"<br>Networks: ",ecor_maps$nets,"<br>CMI: ",ecor_maps[["CMI"]],"<br>GPP: ",ecor_maps[["GPP"]],"<br>LED: ",ecor_maps[["LED"]],"<br>LCC: ",ecor_maps[["LCC"]],"<br>R-squared: ",ecor_maps[["Species"]],"<br>RMSE: ",ecor_maps[["RMSE"]])
        leaflet(ecor_maps) %>%
            addProviderTiles("Esri.NatGeoWorldMap", "Esri.NatGeoWorldMap") %>%
            addPolygons(data=ecor_maps, fillColor = ~pal(unlist(i)), fill=T, weight=1, color="black", fillOpacity=1, group="Ecoregions", popup=ecoPopup) %>%
            addLayersControl(position = "topright",
            baseGroups=c("Esri.NatGeoWorldMap"),
            overlayGroups = c("Ecoregions"),
            options = layersControlOptions(collapsed = TRUE)) %>%
            addLegend(pal = pal, values = ~i, opacity = 0.7, title = "R-squared",
                position = "bottomright")
    })

    output$methods <- renderText({
        includeMarkdown("definitions.Rmd")
    })

}

shinyApp(ui, server)
