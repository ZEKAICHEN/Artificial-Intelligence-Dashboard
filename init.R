library(shiny)
require(shinyBS)
require(shinydashboard)
require(shinyjs)
require(caret)
require(plyr)
require(dplyr)
require(tidyr)
require(Cairo)
require(raster)
require(gstat)
require(wesanderson)
require(nnet)
require(randomForest)
require(foreach)
library(maps)
library(mapproj)
library(readxl)
library(ggplot2)
library(leaflet)
library(textshape)
#needs(shiny, shinyBS, shinydashboard, shinyjs, caret, plyr, tidyr, Cairo, raster, gstat,
      #wesanderson, nnet, randomForest, plyr)
# car, foreach, methods, plyr, nlme, reshape2, stats, stats4, utils, grDevices


# Not all of these are required but shinyapps.io was crashing and 
# importing one of these solved the issue
require(kernlab)
require(klaR)
require(vcd)
require(e1071)
require(gam)
require(ipred)
require(MASS)
require(ellipse)
require(mda)
require(mgcv)
require(mlbench)
require(party)
require(MLmetrics)
require(Cubist)
require(testthat)
#needs(kernlab, klaR, vcd, e1071, gam, ipred, MASS, ellipse, mda, mgcv, mlbench,
      #party, MLmetrics, Cubist, testthat)
#setwd("~/Desktop/Dihuni/Artificial Intelligence Dashboard")



tableCSS <- function(model,col){
  paste0('if (data[6] == "',model,'")
         $("td", row).css("background", "',col,'");')
}  

label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

AI_company <- read_excel("updated AI company.xlsx", 
                         col_types = c("numeric", "text", "text", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "text", "text", "numeric", 
                                       "numeric", "text", "numeric", "text", 
                                       "text", "text", "numeric", "numeric", 
                                       "text"))
AI_description <- read_excel("AI company.xlsx", 
                             sheet = "Sub-category & Use Case")
productivity <- read_excel("productivity.xlsx", 
                           col_types = c("text", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))
productivity <- productivity[1:48,]
states_map <- map_data("state")
df <- data.frame(state = tolower(rownames(USArrests)),State = state.abb[match(rownames(USArrests), state.name)])
df <- df[-c(2,11),]
df <- merge(AI_company, df, by = "State")
detach("package:plyr", unload=TRUE)
AI_map <- AI_company %>% group_by(State) %>% summarise(count = n())
df <- merge(df, AI_map, by = "State")
state_name <- tolower(rownames(USArrests))
df_new <- df[c("state", "count")]
df_new <- unique(df_new)
df_com <- data.frame(state = state_name, count = rep(0,50))
for (i in df_new$state){
  df_com$count[which(df_com$state==i)] <- df_new$count[which(df_new$state==i)]
}
df_uniq <- unique(df[c("City", "W", "N")])

data <- read_excel("Autonomous Car.xlsx", 
                   col_types = c("text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))

tableau <- read_excel("tableau.xlsx")
content <- paste(sep = "<br/>",
                 paste("<b><a>Driving environment: </a></b>",tableau$`driving environment`),
                 paste("<b><a>Market size: </a></b>", tableau$`market size`),
                 paste("<b><a>Goverment policy: </a></b>", tableau$`Government Policy and Regulations`),
                 paste("<b><a>Overall: </a></b>", tableau$overall))

m = leaflet(tableau) %>% addTiles() %>% setView(lng = -95, lat = 35, zoom = 5)

content_second <- paste(sep = "<br/>",
                 paste("<b><a>Company name: </a></b>",AI_company$`Company Name`),
                 paste("<b><a>Company category: </a></b>",AI_company$`Sub-category`),
                 paste("<b><a>Start year: </a></b>", AI_company$`Start year`)
)

n = leaflet(AI_company) %>% addTiles() %>% setView(lng = -95, lat = 35, zoom = 5)



