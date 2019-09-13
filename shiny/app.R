library(sf)
library(caret)
library(shiny)
library(leaflet)
library(tidyverse)

load("stats.Rdata")
load("ecor_maps.Rdata")
load("ks_mix.Rdata")

ks = mutate(ks_mix, ecozone=as.character(substr(ecozone,2,nchar(ecozone))))
zone = select(ks, ecozone, ecoregion, intact_eco) %>% unique()
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
            checkboxInput("repnorep", "Use only rep and nonrep networks", TRUE),
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
            tabPanel("Observed vs predicted",
                br(),
                plotOutput("plot1")
                ),
            tabPanel("Intactness vs R-squared",
                br(),
                plotOutput("plot2")
                ),
            tabPanel("Ecozone boxplots",
                br(),
                plotOutput("plot3")
                ),
            tabPanel("Coefficient boxplots",
                fluidRow(
                  column(6,plotOutput(outputId="plot4", width="500px",height="300px")),  
                  column(6,plotOutput(outputId="plot5", width="500px",height="300px")),
                  column(6,plotOutput(outputId="plot6", width="500px",height="300px")),  
                  column(6,plotOutput(outputId="plot7", width="500px",height="300px"))
                )
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
                x = filter(ks, ecoregion==eco & (rep==0 | rep==1))
                nr = sum(x$rep) # number of rep networks
                nnr = nrow(x) - nr # number of non-rep networks
                if (nnr > 10*nr) {
                    x = group_by(x, rep) %>% sample_n(if_else(rep==1,nr,10*nr)) %>% ungroup()
                } else if (nr > 10*nnr) {
                    x = group_by(x, rep) %>% sample_n(if_else(rep==1,10*nnr,nnr)) %>% ungroup()
                }
            } else {
                x = filter(ks, ecoregion==eco)
                minNet = min(table(x$rep))
                x = group_by(x, rep) %>% sample_n(minNet) %>% ungroup()
            }
            if (sum(!is.na(x[spp])) > 2) {
                
                # filter networks
                x = filter(x, !is.na(x[[spp]]))
                
                # select model
                m1 = lm(test ~ ks_cmi + ks_gpp + ks_led + bc_lcc, data=x)
                
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
            z[i,"Intactness"] = ""
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

        if (input$species %in% songbirds) {
            eco_list = stats$zone[stats$species==spp & stats$pct>0]
        } else {
            eco_list = unique(ks$ecoregion)
        }
        i = 1
        for (eco in eco_list) {
            if (input$repnorep==TRUE) {
                x = filter(ks, ecoregion==eco & (rep==0 | rep==1))
           } else {
                x = filter(ks, ecoregion==eco)
                minNet = min(table(x$rep))
                x = group_by(x, rep) %>% sample_n(minNet) %>% ungroup()
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
                x4 = x$rep
            } else {
                x1 = c(x1, rep(eco, nrow(x)))
                x2 = c(x2, x$test)
                x3 = c(x3, x$fitted)
                x4 = c(x4, x$rep)
            }
            i = i + 1
        }
        z = tibble(ecoregion=x1, observed=x2, fitted=x3, rep=x4)
        return(z)
   })

    output$plot1 <- renderPlot({
        z = testdata2()
        if (length(unique(z$ecoregion)) <= 12) {
            n = 4
        } else if (length(unique(z$ecoregion)) > 12 & length(unique(z$ecoregion)) <= 20) {
            n = 5
        } else if (length(unique(z$ecoregion)) > 28 & length(unique(z$ecoregion)) <= 36) {
            n = 6
        } else {
            n = 7
        }
        p <- ggplot(z, aes(fitted, observed)) + geom_point(aes(colour=factor(rep))) + geom_smooth(method='lm')
        p + facet_wrap(vars(ecoregion), ncol=n) #, scales = "free_y")
    }, height=800)

    output$plot2 <- renderPlot({
        z = testdata()
        plot(z$Intactness, z$R2)
        abline(lm(z$R2 ~ z$Intactness), col="red")
        #lines(lowess(z$Intactness, z$R2), col="blue")
    }, width=600)

    output$plot3 <- renderPlot({
        z = testdata()
        z$R2 = as.numeric(z$R2)
        bp = ggplot(z, aes(x=Ecozone, y=R2)) + geom_boxplot(aes(group=Ecozone))
        bp + ggtitle("Ecoregion-level R2 values by Ecozone")
        bp + scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1))
    }, height=600)

    output$plot4 <- renderPlot({
        z = testdata()
        a1 = unlist(str_split(z$CMI, " "))
        b1 = a1[seq(1,length(a1),2)]
        z = mutate(z, b_cmi = as.numeric(b1))
        bp1 = ggplot(z, aes(x=Ecozone, y=b_cmi)) + geom_boxplot(aes(group=Ecozone))
        bp1 + ggtitle("CMI effect size by ecozone") + geom_hline(yintercept=0, colour="blue", linetype="dashed", size=1)
    })

    output$plot5 <- renderPlot({
        z = testdata()
        a2 = unlist(str_split(z$GPP, " "))
        b2 = a2[seq(1,length(a2),2)]
        z = mutate(z, b_gpp = as.numeric(b2))
        bp2 = ggplot(z, aes(x=Ecozone, y=b_gpp)) + geom_boxplot(aes(group=Ecozone))
        bp2 + ggtitle("GPP effect size by ecozone") + geom_hline(yintercept=0, colour="blue", linetype="dashed", size=1)
    })

    output$plot6 <- renderPlot({
        z = testdata()
        a3 = unlist(str_split(z$LED, " "))
        b3 = a3[seq(1,length(a3),2)]
        z = mutate(z, b_led = as.numeric(b3))
        bp3 = ggplot(z, aes(x=Ecozone, y=b_led)) + geom_boxplot(aes(group=Ecozone))
        bp3 + ggtitle("LED effect size by ecozone") + geom_hline(yintercept=0, colour="blue", linetype="dashed", size=1)
    })

    output$plot7 <- renderPlot({
        z = testdata()
        a4 = unlist(str_split(z$LCC, " "))
        b4 = a4[seq(1,length(a4),2)]
        z = mutate(z, b_lcc = as.numeric(b4))
        bp4 = ggplot(z, aes(x=Ecozone, y=b_lcc)) + geom_boxplot(aes(group=Ecozone))
        bp4 + ggtitle("LCC effect size by ecozone") + geom_hline(yintercept=0, colour="blue", linetype="dashed", size=1)
    })

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
