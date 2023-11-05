library(tigris)
library(ggplot2)
library(httr)
library(jsonlite)
library(cdlTools)
library(svDialogs)

#UI
ui <- fluidPage(

    #SIDEBAR
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "state",
                      label = "State:",
                      choices = c("AL",	"AK",	"AZ",	"AR",	"CA",	"CO",	"CT",	"DE",	"DC",	"FL",	"GA",	"HI",	
                                  "ID",	"IL",	"IN",	"IA",	"KS",	"KY",	"LA",	"ME",	"MD",	"MA",	"MI",	"MN",	
                                  "MS",	"MO",	"MT",	"NE",	"NV",	"NH",	"NJ",	"NM",	"NY",	"NC",	"ND",	"OH",	
                                  "OK",	"OR",	"PA",	"RI",	"SC",	"SD",	"TN",	"TX",	"UT",	"VT",	"VA",	"WA",	
                                  "WV",	"WI",	"WY"),
                      selected='IA'),
          
          selectInput(inputId = "level",
                      label = "Educational attainment:",
                      choices = c('LT9','LT12','HS','SCND','AA','BS','MS'),
                      selected='BS'),
          
          selectInput(inputId = "yr",
                      label = "ACS Year:",
                      choices = c(2018,2019,2020,2021),
                      selected=2021)
        ),

        #SHOW THE MAP
        mainPanel(
           plotOutput("map", height=700)
        )
    )
)

#SERVER
server <- function(input, output, session) {
  
  #STOP SESSION ON EXIT
  session$onSessionEnded(function() {
    stopApp()
  })
    #GET EDUCATION LEVEL TEXT FOR MAP TITLE
    output$map <- renderPlot({
      if (input$level=='LT9') {
        degree <- "Less Than 9th Grade"
      } else if (input$level=='LT12') {
        degree <- "Between 9th and 12th Grade"
      } else if (input$level=='HS') {
        degree <- "High School Graduate"
      } else if (input$level=='SCND') {
        degree <- "Some College, No Degree"
      } else if (input$level=='AA') {
        degree <- "Associate Degree"
      } else if (input$level=='BS') {
        degree <- "Bachelor's Degree"
      } else if (input$level=='MS') {
        degree <- "Master's Degree"
      }
      
      #GET DATA
      ea <- read.table("./data.txt", sep=",", header=TRUE, colClasses="character")
      ea <- subset(ea, ea$yr==input$yr)
      ea <- ea[,-14]
      

      #RENAME TWO COLUMNS
      colnames(ea)[2] <- "STATEFP"
      colnames(ea)[3] <- "COUNTYFP"
      
      #ADD NEW COLUMNS 
      ea$LT9 <- 0
      ea$LT12 <- 0
      ea$HS <- 0
      ea$SCND <- 0
      ea$AA <- 0
      ea$BS <- 0
      ea$MS <- 0
      
      #COMPUTE PERCENTAGE FOR EACH ATTAINMENT LEVEL
      for (r in 1:nrow(ea)) {
        for (c in 5:11) {
          assign(paste0("e", c), as.numeric(ea[r,c])/as.numeric(ea[r,4]))
        }
        ea$LT9[r] <- e5
        ea$LT12[r] <- e6
        ea$HS[r] <- e7
        ea$SCND[r] <- e8
        ea$AA[r] <- e9
        ea$BS[r] <- e10
        ea$MS[r] <- e11
      }
      
      ea <- ea[,c(2,3,14:20)]
      
      options(digits=10)
      
      #GET SHAPEFILE FOR COUNTIES IN STATE OF INTEREST
      county <- counties(input$state, cb=TRUE)
      county <- subset(county, county$NAME!='Aleutians West')
      c2 <- merge(county, ea, by=c("STATEFP","COUNTYFP"), all.x=TRUE)
      
      #SET NA TO ZERO TEMPORARILY
      c2[is.na(c2)] <- 0
      
      #FIND EDUCATION LEVEL REQUESTED
      for (i in 13:19){
        if (colnames(c2)[i]==input$level) {
          column_used=i
        }
      }
      
      #COMPUTE MAX AND MIDPOINT FOR LEGEND SCALE
      maxp <- max(c2[[column_used]])
      midp <- 0.5*maxp
      
      #SEPARATE INTO DARK AND LIGHT COLORED COUNTIES - FONT TEXT WILL BE DIFFERENT COLOR FOR EACH GROUP
      c_dark <- subset(c2, c2[[column_used]]>0.25*maxp)
      c_light <- subset(c2, c2[[column_used]]<=0.25*maxp)
      
      #SET NA BACK TO NA SO THE COUNTY WILL GET COLORED GRAY ON MAP
      for (i in 13:19){
        c2[[i]] <- ifelse(c2[[i]]==0, NA, c2[[i]])
      }
      
      #DRAW THE MAP
      ggplot() +
        geom_sf(data = c2, aes(fill=UQ(as.name(input$level))), size=0.05) +
        geom_sf_text(data=c_light, aes(label=NAME), size=3, colour="gray50", fontface="bold") +
        geom_sf_text(data=c_dark, aes(label=NAME), size=3, colour="white", fontface="bold") +
        scale_fill_gradientn(paste0("Percent\n"),
                             colors = c("cornsilk", "darkblue"),
                             na.value = "gray",
                             labels=c("0%", paste0(round(100*midp), "%"), paste0(round(100*maxp), "%")),
                             breaks=c(0, midp, maxp), limits=c(0,maxp)) +
        ggtitle(paste0("Percent of ", input$state, " Residents with ", degree, " as Highest Level of Education"),
                subtitle="counties shaded in gray have no reported data") +
        labs(caption=paste0("Source: US Census Bureau ", input$yr, " ACS Data (5-year estimates)")) +
        theme(
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_rect(fill = "snow"),
          panel.grid.major=element_line(color="snow"),
          legend.key.width=unit(0.025,'npc'),
          legend.key.height=unit(0.05,'npc'),
          legend.text=element_text(face="bold", size=8),
          legend.title=element_text(face="bold", size=8),
          plot.title=element_text(color=rgb(48/255,22/255,146/255), size=16, face="bold"),
          plot.subtitle=element_text(color="gray50", size=12),
          plot.caption = element_text(hjust = 0, color="gray50", size=10))
    })
}


shinyApp(ui=ui, server=server)
