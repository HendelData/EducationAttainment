library(tigris)
library(ggplot2)
library(httr)
library(jsonlite)
library(cdlTools)
library(svDialogs)

#COLLECT API KEY FOR US CENSUS BUREAU - IF NONE USE DOWNLOADED DATA
if (!exists("api_census")) {
  api_census <- dlgInput("Enter your API key (or NONE if you don't have one)", "")$res
} 

stateABBR <- "IA"
ed_level="BS"
apiyr <- 2021

#CHANGE PATH TO A LOCAL FOLDER WHERE YOU STORED THE DATA
path_to_saved_data <- "C:/Users/marce/Documents/R/EducationalAttainment/"

#ASSIGN TEXT FOR TITLE
if (ed_level=='LT9') {
  degree <- "Less Than 9th Grade"
} else if (ed_level=='LT12') {
  degree <- "Between 9th and 12th Grade"
} else if (ed_level=='HS') {
  degree <- "High School Graduate"
} else if (ed_level=='SCND') {
  degree <- "Some College, No Degree"
} else if (ed_level=='AA') {
  degree <- "Associate Degree"
} else if (ed_level=='BS') {
  degree <- "Bachelor's Degree"
} else if (ed_level=='MS') {
  degree <- "Master's Degree"
}

#GET DATA
if (api_census=="NONE"){
  ea <- read.table(paste0(path_to_saved_data,"data.txt"), sep=",", header=TRUE, colClasses="character")
  ea <- subset(ea, ea$yr==apiyr)
  ea <- ea[,-14]
} else {
  ea_api <- GET(paste0("https://api.census.gov/data/", apiyr,
                       "/acs/acs5/subject?get=NAME,STATE,COUNTY,S1501_C01_006E,S1501_C01_007E,S1501_C01_008E,S1501_C01_009E,S1501_C01_010E,S1501_C01_011E,S1501_C01_012E,S1501_C01_013E&for=county:*&in=state:*&key=",
                       api_census))
  ea <- as.data.frame(fromJSON(rawToChar(ea_api$content), simplifyDataFrame=TRUE))
  colnames(ea) <- ea[1,]
  ea <- ea[-1,]
}

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
county <- counties(stateABBR, cb=TRUE, resolution="20m")
county <- subset(county, county$NAME!='Aleutians West')
c2 <- merge(county, ea, by=c("STATEFP","COUNTYFP"), all.x=TRUE)

#SET NA TO ZERO TEMPORARILY
c2[is.na(c2)] <- 0

#FIND EDUCATION LEVEL REQUESTED
#for (i in 13:19){
for (i in 18:24){
  if (colnames(c2)[i]==ed_level) {
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
for (i in 13:19) {
  c2[[i]] <- ifelse(c2[[i]]==0, NA, c2[[i]])
}

#PLOT THE GRAPH
ggplot() +
  geom_sf(data=c2, aes(fill=UQ(as.name(ed_level))), size=0.05) +
  geom_sf_text(data=c_light, aes(label=NAME), size=1.5, colour="gray50", fontface="bold") +
  geom_sf_text(data=c_dark, aes(label=NAME), size=1.5, colour="white", fontface="bold") +
  scale_fill_gradientn(paste0("Percent\n"),
                       colors = c("cornsilk", "darkblue"),
                       na.value = "gray",
                       labels=c("0%", paste0(round(100*midp), "%"), paste0(round(100*maxp), "%")),
                       breaks=c(0, midp, maxp), limits=c(0,maxp)) +
  ggtitle(paste0("Percent of ", stateABBR, " Residents with ", degree, " as Highest Level of Education"),
          subtitle="counties shaded in gray have no reported data") +
  labs(caption=paste0("Source: US Census Bureau ", apiyr, " ACS Data (5-year estimates)")) +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks=element_blank(),
    panel.background=element_rect(fill = "ivory"),
    panel.grid.major=element_line(color="ivory"),
    legend.key.width=unit(0.15,'npc'),
    legend.key.height=unit(0.5,'npc'),
    legend.text=element_text(face="bold", size=8),
    legend.title=element_text(face="bold", size=8),
    plot.title=element_text(color=rgb(48/255,22/255,146/255), size=10, face="bold"),
    plot.subtitle=element_text(color="gray50", size=8),
    plot.caption = element_text(hjust = 0, color="gray50", size=6))

