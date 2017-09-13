source('init.R',local = T)
source('helpers.R',local = T)
#source('echodots-copy.R',local = T)


# Server ------------------------------------------------------------------

server <- function(input, output,session) {
  
  data1=data[,1:6]
  data2=data[,c("City", "State", "Driving Environment2", "Market Size2", "Government Policy and Regulations2", "Total2")]
  data3=data[,c("City", "State", "Driving Environment3", "Market Size3", "Government Policy and Regulations3", "Total3")]
  output$table1 <- DT::renderDataTable({
    DT::datatable(data1, options=list(orderClasses=TRUE))})
  output$table2 <- DT::renderDataTable({
    DT::datatable(data2, options = list(orderClasses = TRUE))})
  output$table3 <- DT::renderDataTable({
    DT::datatable(data3, options = list(orderClasses = TRUE)) })
  
  output$cityscore <- renderLeaflet({
    m %>% addAwesomeMarkers(~longitude, ~latitude, 
                            label=paste0(tableau$City,', ',tableau$State),
                            popup=content) %>%
      addCircles(~longitude, ~latitude, weight=1,
                 radius=~`driving environment` * 20000, color=c('red')) %>%
      addCircles(~longitude, ~latitude, weight=1,
                 radius=~`market size` * 20000, color=c('blue')) %>%
      addCircles(~longitude, ~latitude, weight=1,
                 radius=~`Government Policy and Regulations` * 20000, color=c('yellow'))
  })
  
  
  output$agrmap <- renderPlot({
    new_args <- switch(input$agrvar,
                   "Annual growth" = list(productivity$`Annual growth (%)`, "darkgreen", "Annual growth %"),
                   "Chemical" = list(productivity$Chemical, "black", "Chemical %"),
                   "Pesticide" = list(productivity$Pesticide, "darkorange", "Pesticide %"),
                   "Fertilizer" = list(productivity$Fertilizer, "darkviolet", "Fertilizer %"))
    
    new_args$min <- input$agrrange[1]
    new_args$max <- input$agrrange[2]
    
    do.call(state_map, new_args)
  })
  
  output$aimap <- renderPlot({
    ggplot(df_com, aes(map_id = state)) +
      geom_map(aes(fill = count), map = states_map) +
      expand_limits(x = states_map$long, y = states_map$lat) + coord_map() +
      theme(legend.position = "bottom", panel.background = element_blank()) +
      scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
      labs(x = "", y = "") + geom_point(aes(x=-df_uniq$W[1], y=df_uniq$N[1]), color="red") +
      geom_point(aes(x=-df_uniq$W[2], y=df_uniq$N[2]), color="red") +
      geom_point(aes(x=-df_uniq$W[3], y=df_uniq$N[3]), color="red") +
      geom_point(aes(x=-df_uniq$W[4], y=df_uniq$N[4]), color="red") +
      geom_point(aes(x=-df_uniq$W[5], y=df_uniq$N[5]), color="red") +
      geom_point(aes(x=-df_uniq$W[6], y=df_uniq$N[6]), color="red") +
      geom_point(aes(x=-df_uniq$W[7], y=df_uniq$N[7]), color="red") +
      geom_point(aes(x=-df_uniq$W[8], y=df_uniq$N[8]), color="red") +
      geom_point(aes(x=-df_uniq$W[9], y=df_uniq$N[9]), color="red") +
      geom_point(aes(x=-df_uniq$W[10], y=df_uniq$N[10]), color="red") +
      geom_point(aes(x=-df_uniq$W[11], y=df_uniq$N[11]), color="red") +
      geom_point(aes(x=-df_uniq$W[12], y=df_uniq$N[12]), color="red") +
      geom_point(aes(x=-df_uniq$W[13], y=df_uniq$N[13]), color="red") +
      geom_point(aes(x=-df_uniq$W[14], y=df_uniq$N[14]), color="red") +
      geom_point(aes(x=-df_uniq$W[15], y=df_uniq$N[15]), color="red") +
      geom_point(aes(x=-df_uniq$W[16], y=df_uniq$N[16]), color="red") +
      geom_point(aes(x=-df_uniq$W[17], y=df_uniq$N[17]), color="red") +
      geom_point(aes(x=-df_uniq$W[18], y=df_uniq$N[18]), color="red") +
      geom_point(aes(x=-df_uniq$W[19], y=df_uniq$N[19]), color="red") +
      geom_point(aes(x=-df_uniq$W[20], y=df_uniq$N[20]), color="red") +
      geom_point(aes(x=-df_uniq$W[21], y=df_uniq$N[21]), color="red") +
      geom_point(aes(x=-df_uniq$W[22], y=df_uniq$N[22]), color="red") +
      geom_point(aes(x=-df_uniq$W[23], y=df_uniq$N[23]), color="red")
    })
  
  output$wcloud <- renderPlot({
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  
  # Outputs ---------------------------------------------------------------------
  
  
  
  output$AIcase_1 <- renderText(AI_description$`Use Case1`[which(input$casevar==AI_description$`Sub-category`)])
  output$AIdes_1 <- renderText(AI_description$Description[which(input$casevar==AI_description$`Sub-category`)])
  
}

# UI ----------------------------------------------------------------------

ui <- bootstrapPage(useShinyjs(),
                    # Add custom CSS & Javascript;
                    tagList(tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js")
                      
                    )),
                    
                    dashboardPage(skin = 'black',
                                  dashboardHeader(title = HTML(paste(icon('cubes'),'Dihuni-DEMO'))
                                  ),
                                  dashboardSidebar(
                                    sidebarMenu(
                                      # Setting id makes input$tabs give the tabName of currently-selected tab
                                      id = "tabs",
                                     # menuItem("Step 1: Input Data", tabName = "setup", icon = icon("cog"),selected = T),
                                     # menuItem("Step 2: Training & CV",tabName = "model", icon = icon("sitemap")),
                                     # menuItem("Step 3: Model Performance",tabName = "test", icon = icon("bar-chart-o")),
                                     # menuItem("Exploration", icon = icon("info"),
                                      #         menuSubItem("Feature Importance",tabName = "imp")),
                                      #menuItem("census map", tabName = "almap", icon = icon("area-chart")),
                                      menuItem("Agriculture map", tabName = "armap", icon = icon("area-chart")),
                                      menuItem("Artificial Intelligence", icon = icon("google"),
                                               menuSubItem("Cases", tabName = "AIcase"),
                                               menuSubItem("Funding trend", tabName = "trend"),
                                               menuSubItem("AI Company map", tabName = "AImap")),
                                      menuItem("wordcloud", tabName = "wc", icon = icon("cloud")),
                                      menuItem("Autocars", tabName = "ac", icon = icon("car"))
                                      # menuSubItem("Feature Selection", tabName = "boruta"))
                                      # 
                                      # menuItem("Feature Importance",tabName = "featSel", icon = icon("sitemap"))
                                      # menuItem("Info",tabName = "Info", icon = icon("info"))
                                    ),
                                    hr(),
                                    
                                    absolutePanel(
                                      bottom = 10,
                                      left = 10,
                                      draggable = F,
                                      width='100%',
                                      height='auto',
                                      a(icon('github fa-2x'),href='https://github.com/ZEKAICHEN/Artificial-Intelligence-Dashboard',target='_blank')
                                    )                  
                                  ),
                                  dashboardBody(
                                    tabItems(
                                      
                                      tabItem("armap",
                                              fluidRow(
                                                box(width = 9, height = "440px",solidHeader = T,
                                                    plotOutput('agrmap')
                                                ),
                                                box(width = 3, height = "440px",title = "Agriculture distribution map",solidHeader = T,
                                                    helpText('This is an agriculture map'),
                                                    radioButtons("agrvar", 
                                                                 label = "Choose a variable to display",
                                                                 choices = list("Annual growth", "Chemical",
                                                                                "Pesticide", "Fertilizer"),
                                                                 selected = "Annual growth"),
                                                    sliderInput("agrrange", 
                                                                label = "Range of interest:",
                                                                min = 0, max = 3, value = c(1,2))
                                                )
                                             )
                                      ),
                                      tabItem("AIcase",
                                              fluidRow(
                                                column(width = 5,
                                                  box(width=NULL, height = 150,title = "Artificial Intelligence fields", solidHeader = T,
                                                            selectInput("casevar",
                                                                        label = "select a category:",
                                                                        choices = AI_description$`Sub-category`,
                                                                        selected = "Computer Vision")
                                                  ),
                                                  box(width = NULL, height = 250,title = "Description", solidHeader = T,
                                                      textOutput("AIdes_1")
                                                    
                                                  )
                                                ),
                                                box(height = 450,title = "Use Case",solidHeader = T,
                                                  textOutput("AIcase_1")
                                                )
                                              )
                                              
                                      ),
                                      tabItem("AImap",
                                              plotOutput('aimap')
                                      ),
                                      tabItem("wc",
                                              plotOutput('wcloud')
                                      ),
                                      tabItem("ac",
                                              h2(em("50 Most Suitable US Cities for Autonomous Cars")),
                                                tabBox(width = 14,
                                                  tabPanel("City Score", leafletOutput('cityscore')),
                                                  tabPanel("Private Fuel Car", DT::dataTableOutput("table1")),
                                                  tabPanel("Private Electric Car", DT::dataTableOutput("table2")),
                                                  tabPanel("Ride-sharing Car", DT::dataTableOutput("table3"))
                                                  ))
                                              
                                    )
                                  
                                  
                    ),
                    div(class = "busy", 
                        h4("working..."),
                        h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                    )
)
)

shinyApp(ui=ui,server = server)
