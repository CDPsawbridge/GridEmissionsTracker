require(plyr)
require(dplyr)
require(reshape2)
require(ggplot2)
require(stringi)



server<-function(input,output){

link24<-paste0("https://www.bmreports.com/bmrs/?q=ajax/csv_download/FUELINST/csv/&filename=GenerationbyFuelType24HrInstantaneous_",format(Sys.time()-120,"%Y%m%d_%H%M"))
bm24orig<-read.csv(link24,skip=1,header=F,stringsAsFactors = F)



  bm24<-  bm24orig
  colVec<-c("#5B1A18", "#C93312","#29211F", "#DC863B","#899DA4","#ABDDDE","#78B7C5" ,"#FD6467", "#0B775E","#FAEFD1", "#EBCC2A","#3B9AB2", "#02401B")
  
  names(bm24)<-c("HDR","Day","halfHour","timestamp","CCGT",	"OCGT",	"Coal",	"Nuclear",	"Wind",	"Pumped","Hydro",	"Oil",	"Biomass",	"French",	"Irish",	"Dutch","EW")
  bm24<-bm24[bm24$HDR=="FUELINST",]
  
  bm24<-bm24[c("timestamp",
               "CCGT",
               "OCGT",
               "Coal",
               "Nuclear",
               "Wind",
               "Hydro",
               "Pumped",
               "Oil",
               "Biomass",
               "French",
               "Irish",
               "Dutch",
               "EW")]%>% melt(id.vars=c("timestamp"),
                              variable.name="Source",
                              value.name="MW")
  bm24<-bm24[!is.na(bm24$timestamp),]
  bm24$timestamp<-bm24$timestamp %>% strptime("%Y%m%d%H%M%S") %>% as.character()

    EF<-data.frame(Source=c(     "CCGT",
                                 "OCGT",
                                 "Coal",
                                 "Oil",
                                 "Biomass",
                                 "French",
                                 "Irish",
                                 "Dutch",
                                 "EW",
                                 "Nuclear",
                                 "Wind",
                                 "Hydro",
                                 "Pumped"),
                              EF=c(0.396,0.591,0.990,0.591,0.986,0.078,0.457,0.384,0.475,0,0,0,0),
                              colCol=as.character(colVec))

    bm24<-merge(bm24,EF)
bm24$tCO2e<-bm24$MW*bm24$EF /12 
bmPlotFrame<-subset(bm24,select=c("timestamp","colCol","Source","MW","tCO2e")) %>% 
        melt(id.vars=c("timestamp","colCol","Source")) 

# Neeed to make Reactive

#                  "CCGT"=1,
#                  "OCGT"=2,
#                  "Coal"=3,
#                  "Oil"=4,
#                  "Biomass"=5,
#                  "French"=6,
#                  "Irish"=7,
#                  "Dutch"=8,
#                  "EW"=9,
#                  "Nuclear"=10,
#                  "Wind"=11,
#                  "Hydro"=12,
#                  "Pumped"=13



# THIS BIT IS JUST TO ORDER THE PLOT AREAS

  mdf<-cbind(EF,num=1:13)
  
  bmPlotFrame2<- bmPlotFrame[bmPlotFrame$Source=="Oil",]
  bmPlotFrame2$Source<-as.character(bmPlotFrame2$Source)
  bmPlotFrame2$Source[bmPlotFrame2$Source=="Oil"]<-'Blank'  
  bmPlotFrame2$value<-0
  
    for(n in 1:length(EF$Source)){
      bmPlotFrame2<-rbind(bmPlotFrame2,bmPlotFrame[bmPlotFrame$Source==EF$Source[n],])
    }
    
  bmPlotFrame2$Source<-as.factor(bmPlotFrame2$Source)

output$bmPlot<-renderPlot({  
  if(length(input$checkGroup)>0){
    # browser()
    HScol<-mdf[mdf$num %in% input$checkGroup,] %>% arrange(Source) 
    HScol<- as.character(HScol$colCol)
    bmPlotFrame2<-bmPlotFrame2[bmPlotFrame2$variable %in% c("tCO2e","MW"),]
    labList<-c(`tCO2e`="Emissions (tCO2e)",`MW`="Power Output (MW)")
            ggplot(data=bmPlotFrame2[bmPlotFrame2$Source %in% as.character(mdf$Source[mdf$num %in% input$checkGroup ]),],
                   aes(y=value,x=as.POSIXlt(timestamp),fill=Source))+
            geom_area()+
            facet_grid(variable~.,scales="free_y", labeller = as_labeller(labList))+
              ggtitle('UK Energy Mix and Greenhouse Gas Emissions over the last 24hrs')+
            theme(strip.text.y = element_text(size = 14,face="bold"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),    
                  axis.text.x  = element_text(size=16),
                  axis.text.y  = element_text(size=16),
                  legend.text = element_text(size=16),
                  legend.title = element_text(face="bold",size=16),
                  title = element_text(face="bold",size=16))+
            scale_fill_manual(values=HScol)
  } else{
    ggplot(data=bmPlotFrame2[bmPlotFrame2$Source %in% as.character(mdf$Source[mdf$num %in% 1:13 ]),],
           aes(y=value,x=as.POSIXlt(timestamp),fill=Source))+ 
      geom_blank()+
      facet_grid(variable~.,scales="free_y")+ 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x  = element_text(size=16),
            axis.text.y  = element_text(size=16),
            legend.text = element_text(size=16))
    }                        
    })

output$value<-renderPrint({input$checkGroup})
# This bitshould be fine either way
output$LiveTime<-renderText(paste("Data last updated at",max(bm24$timestamp,na.rm=T)))
LiveTime<-max(bm24$timestamp,na.rm=T) %>% strftime(format="%H") %>% as.numeric()

output$totalTonnes<-renderPrint(paste(signif(sum(bm24$tCO2e),3), "Tonnes of CO2e over the last 24hrs"))
# reacts to clicks
observe({print(input$bmPlot_hover$x)})
output$hover_info <- renderTable({
  closesthover<-bm24$timestamp[which.min(abs(as.numeric(as.POSIXlt(bm24$timestamp))-input$bmPlot_hover$x))]
#   bm24[!is.na(bm24$timestamp) & bm24$timestamp==closestClick,][c("Source","MW","tCO2e")]
#   
  outDF<-bm24[!is.na(bm24$timestamp) & bm24$timestamp==closesthover,][c("Source","MW","tCO2e")]
  outDF$`tCO2e/hr`<-signif(outDF$tCO2e*12,3)
  outDF$`% Energy Mix`<-signif(outDF$MW/sum(outDF$MW,na.rm=T)*100,4)
  outDF<-outDF[c("Source","MW","% Energy Mix","tCO2e/hr")][outDF$`% Energy Mix`>0,]
  outDF$Source<-as.character(outDF$Source)
  outDF[,3]<-round(outDF[,3],2)
  outDF[(nrow(outDF)+1),]<-c("Total",as.numeric(sum(outDF[,2],na.rm=T)),as.numeric(100),as.numeric(sum(outDF[,4],na.rm=T)))
  
  outDF
  },include.rownames=FALSE)


}
# France 77.92
# Ireland	456.58
# Netherlands 383.97
# United Kingdom	474.44       tonnes of CO2 per MWh

ui <- fluidPage( theme = "bootstrap.css",
  conditionalPanel(condition= "input.GO == 0",
                   fluidRow(
                     column( offset = 1,width=10,
                   h1("Understanding the emissions from the National Grid"),           
                   tags$li("The amount of electricity we use during the day varies as people wake up, leave for work, come home and watch TV, there's usually a spike around 6pm."),
                   tags$li("Electricity companies use different power sources to meet this changing demand."),
                   tags$li("This is because some power sources are more flexible than others and when it's windy, the wind farms generate more than on calm days."),
                   tags$li("You can see all of this happening in this webapp"),
                   h3("The App"),
                   p("You're about to see two plots;"),
                   tags$li("The Top graph shows the UK electricity use over the last 24hrs and where it comes from"),
                   tags$li("The Bottom graph shows the emissions from those sources."),
                   h3("Instructions"),
                   p("Click the tick boxes on the left to add that energy source to the plots")
                   ,actionButton("GO", "See the data" ) 
                   ))
                   ),
  conditionalPanel(condition= "input.GO > 0",
                  
  fluidRow( 
    sidebarLayout(
    sidebarPanel( h3('Explore the data'),
                 
                
                 
#                  "CCGT"=1,
#                  "OCGT"=2,
#                  "Coal"=3,
#                  "Oil"=4,
#                  "Biomass"=5,
#                  "French"=6,
#                  "Irish"=7,
#                  "Dutch"=8,
#                  "EW"=9,
#                  "Nuclear"=10,
#                  "Wind"=11,
#                  "Hydro"=12,
#                  "Pumped"=13
                 
                 
                 
                 p('Hover your mouse the plots at different points to see the figures'),
                 tableOutput("hover_info"),   
                 textOutput("totalTonnes"),
                 hr(),
                  p('This tool uses live information from the National Grid and emissions factors from DECC to estimate the
                    Greenhouse Gas Emissions from the UK Electricity System.'),

                 textOutput("LiveTime")               
    ),
    mainPanel(h1("Live UK Grid Emissions Tracker"),
      hr(),
      checkboxGroupInput("checkGroup", inline=TRUE ,label = h6("Add/Remove Generation Sources to the plot"), 
                         choices = list(                 "CCGT"=1,
                                                         "OCGT"=2,
                                                         "Coal"=3,
                                                         "Oil"=4,
                                                         "Biomass"=5,
                                                         "French"=6,
                                                         "Irish"=7,
                                                         "Dutch"=8,
                                                         "EW"=9,
                                                         "Nuclear"=10,
                                                         "Wind"=11,
                                                         "Hydro"=12,
                                                         "Pumped"=13),selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13)),
      fluidRow( plotOutput("bmPlot",hover = "bmPlot_hover")),
      
      hr(),actionButton("ShowGloss", "Show Explanation" ),
      conditionalPanel(condition= "input.ShowGloss > 0",
                       fluidRow(
                         column( offset = 1,width=10,
                                 h4("Units"),           
                                 tags$li("MW = the power oputput from the grid."),
                                 tags$li("tCO2e = tonnes of Carbon Dioxide equivalent. Other Greenhouse Gases like Methane get converted into an equivalent amount of Carbon Dioxide based on their Global Warming potential.
                                         see", a("Global Warming Potentials - GHG Protocol", href= "http://www.ghgprotocol.org/files/ghgp/tools/Global-Warming-Potential-Values.pdf")),
                                 h4("Energy Sources"),
                                 p("NB: Flexible = how quickly each source switch on and off, this means they can react to spikes in demand."),
                                 tags$li("CCGT = Combined Cycle Gas Turbines which are larger, more efficient, but less flexible Gas Power Plants."),
                                 tags$li("OCGT = Open Cycle Gas Turbines are smaller and less efficient but more flexible."),
                                 tags$li("Coal = Coal Power Plants are generally very large, old and not very flexible."),
                                 tags$li("Oil = Oil Power Plants are small are flexible."),
                                 tags$li("Biomass = Biomass here generally means wood, miscanthus grass, waste from farming and chicken litter."),
                                 tags$li("French = France sells us some of the electricity which mainly comes from nuclear power."),
                                 tags$li("Dutch = We also have an interconnector with the Dutch."),
                                 tags$li("EW = East West and the Welsh."),
                                 tags$li("Nuclear = Nuclear Power stations are always on because they are so inflexible, turning them off and on again takes months."),
                                 tags$li("Wind = These are the larger wind famrs around the UK, smaller facilities don't get monitored by the national Grid."),
                                 tags$li("Hydro = Hydro Electric Dams"),
                                 tags$li("Pumped = Some Hydro facilities pump electricity back up to the top resevoir during the night when electricity is cheap.")
                                 
                         ))
      )
      # fluidRow( plotOutput("weatherPlot"))
    )
)
# )
)
))
 
shinyApp(server=server,ui=ui)