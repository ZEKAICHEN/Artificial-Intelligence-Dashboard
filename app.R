source('init.R',local = T)
source('helpers.R',local = T)
#source('echodots-copy.R',local = T)
counties <- readRDS("~/Desktop/Dihuni/census-app/data/counties.rds")


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
  
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
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
  
  CVtune <- readRDS('initState.Rdata')
  makeReactiveBinding('CVtune')
  
  rawdata <- reactive({
    datasets[[input$dataset]]
  })
  
  observe({
    updateSelectizeInput(session,'yvar',choices=names(rawdata()),selected = names(rawdata())[1])
  })
  
  
  observe({
    nms <- names(rawdata())[names(rawdata())!=input$yvar]
    updateSelectizeInput(session,'xvar',choices=nms,selected = nms)
  })
  
  dataTrain <- NULL
  dataTest <- NULL
  
  makeReactiveBinding('dataTrain')
  # makeReactiveBinding('dataTest')
  modelType <- 'Regression'
  
  makeReactiveBinding('modelType')
  
  observeEvent(modelType,{
    
    if(modelType=='Regression'){
      updateSelectizeInput(session,'slt_algo',choices = reg.mdls,selected = reg.mdls)
    } else {
      updateSelectizeInput(session,'slt_algo',choices = cls.mdls,selected = cls.mdls)
      
    }
  })
  
  observe({
    
    yvar <- input$yvar
    xvars <- input$xvar
    testsize <- input$sld_testsplit
    
    if(is.null(yvar)||yvar=='')
      return(NULL)
    
    # extract y and X from raw data
    y <- isolate(rawdata()[,yvar])
    X <-  isolate(rawdata()[,xvars])
    
    # deal with NA values
    yi <- !is.na(y)
    Xi <- complete.cases(X)
    
    df2 <- cbind(y,X)[yi&Xi,]
    
    
    c <- class(df2$y)
    lvls <- length(unique(df2$y))
    if(lvls<10|(c!='numeric'&c!='integer')){
      modelType <<-'Classification'
      df2$y <- factor(df2$y)
    } else {
      modelType <<-'Regression'
      if(input$chk_logY){df2$y <- log(df2$y+0.1)}
    }
    
    trainIndex <- createDataPartition(df2$y,
                                      p = 1-(testsize/100),
                                      list = FALSE,
                                      times = 1)
    isolate({
      dataTrain <<- df2[ trainIndex,]
      dataTest  <<- df2[-trainIndex,]
    })
  })
  
  
  
  
  observeEvent(input$btn_train,{
    
    disable('btn_train')
    on.exit(enable('btn_train'))
    
    mdls <- isolate(input$slt_algo)
    
    fitControl <- trainControl(method = "cv",savePredictions = T,
                               number = as.integer(input$rdo_CVtype))
    
    trainArgs <- list(
      'svmLinear'=list(form=y ~ .,
                       data = dataTrain,
                       preProcess = c('scale','center'),
                       method = 'svmLinear',
                       trControl = fitControl,
                       tuneGrid=tuneParams[['svmLinear']]),
      'svmPoly'= list(form=y ~ .,
                      data = dataTrain,
                      preProcess = c('scale','center'),
                      method = 'svmPoly',
                      trControl = fitControl,
                      tuneGrid=tuneParams[['svmPoly']]),
      'nnet'=list(form=y ~ .,
                  data = dataTrain,
                  preProcess = c('scale','center'),
                  method = 'nnet',
                  trControl = fitControl,
                  tuneGrid=tuneParams[['nnet']],
                  linout=T),
      'rf'=list(form=y ~ .,
                data = dataTrain,
                preProcess = c('scale','center'),
                method = 'rf',
                trControl = fitControl,
                tuneGrid=tuneParams[['rf']],
                ntree=1e3),
      'knn'=list(form=y ~ .,
                 data = dataTrain,
                 preProcess = c('scale','center'),
                 method = 'knn',
                 trControl = fitControl,
                 tuneGrid=tuneParams[['knn']]),
      'nb'=list(form=y ~ .,
                data = dataTrain,
                preProcess = c('scale','center'),
                method = 'nb',
                trControl = fitControl,
                tuneGrid=tuneParams[['nb']]),
      'glm'=list(form=y ~ .,
                 data = dataTrain,
                 preProcess = c('scale','center'),
                 method = 'glm',
                 trControl = fitControl,
                 tuneGrid=NULL),
      'gam'=list(form=y ~ .,
                 data = dataTrain,
                 preProcess = c('scale','center'),
                 method = 'gam',
                 trControl = fitControl)
    )
    
    tune <- lapply(mdls,function(m){
      do.call('train',trainArgs[[m]])
    })
    
    names(tune) <- mdls
    CVtune <<- tune
    # saveRDS(CVtune,'initState.Rdata')
    
  })
  
  
  CVres <- reactive({

    if(is.null(CVtune)) return(NULL)
    
    fits <- CVtune
    getRes <- function(i){
      name <- names(fits)[i]
      res <- fits[[i]]$results
      df <- res[(ncol(res)-3):ncol(res)]
      apply(res,1,function(r) paste(r[1:(ncol(res)-4)],collapse = '-')) %>% 
        paste(name,.,sep='-') -> model
      cbind.data.frame(model,df,name=name[[1]],stringsAsFactors =F)
    }
    
    df <- plyr::ldply(1:length(fits),getRes)
    
    if(isolate(modelType)=='Regression'){
      df$rank <- rank(rank(df$RMSE)+rank(1-df$Rsquared),ties.method = 'first')
    } else {
      df$rank <- rank(rank(1-df$Accuracy)+rank(1-df$Kappa),ties.method = 'first')
    }
    df[2:5] <- round(df[2:5],3)
    df[order(df$rank),]
  })
  
  CVpredObs <- reactive({
    
    fits <- CVtune

    getObsPred <- function(i){
      # i <- 2
      bst <- fits[[i]]$bestTune
      preds <- fits[[i]]$pred
      preds$name <- names(fits)[i]
      preds$model <- paste(bst,collapse = '-') %>% paste(names(fits)[i],.,sep='-')
      
      ii <- lapply(1:length(bst),function(p){
        preds[names(bst)[p]]==as.character(bst[p][[1]])
      })
      if(length(bst)>1) data.frame(ii) %>% apply(.,1,all) -> ii else unlist(ii) ->ii
      preds[ii,-which(names(preds)%in%names(bst))]
    }
    
    df <- plyr::ldply(1:length(fits),getObsPred)
    str(df)
    # saveRDS(df,'CVpredObs.Rdata')
    df
    
  })
  
  topModels <- reactive({
    if(is.null(CVres()))
      return()
    CVres() %>% group_by(name) %>% filter(rank==min(rank)) -> df
    # 
    lst <- df$name[order(df$rank)]
    names(lst) <- df$model[order(df$rank)]
    lst
  }) 
  
  observe({
    lst <- topModels()
    updateSelectizeInput(session,'slt_Finalalgo',choices = lst,selected = lst[1])
    
  })
  
  testPreds <- reactive({
    
    tune <- isolate(CVtune)
    if(is.null(tune)) return(NULL)
    
    lapply(CVtune[input$slt_Finalalgo],
           predict.train,isolate(dataTest)) %>% 
      data.frame() -> df
    
    if(isolate(modelType)=='Regression'){
      c <- apply(df[input$slt_Finalalgo],1,mean)
      
      s1 <- 1 - mean((dataTest$y-c)^2)/mean((dataTest$y-mean(dataTest$y))^2)
      s2 <- sqrt(mean((dataTest$y-c)^2))
      
    } else {
      c <- apply(df[input$slt_Finalalgo],1,modal)
      s1 <- sum(c==dataTest$y)/nrow(dataTest)
      s2 <- vcd::Kappa(table(c, dataTest$y))$Unweighted[1]
    }
    list(c=c,s1=s1,s2=s2)
    
  })
  
  
  makeReactiveBinding('sigTable')
  observeEvent(input$btn_sigTest,{
    
  
    permute <- function(v1,v2,nreps=1e4){
      rmsq <- function(v){sqrt(mean(v^2))}
      obs.diff = (rmsq(v1) - rmsq(v2))
      v12 = c(v1, v2)
      l12 = length(v12)
      l1 = length(v1)
      
      sim.diff = rep(0, nreps)
      for (j in 1:nreps) {
        perm = sample(v12)
        sim.diff[j] = rmsq(perm[1:l1]) - rmsq(perm[(l1 + 1):l12])
      }
      bigger = sim.diff[(sim.diff) >= obs.diff]
      pvalue = length(bigger)/(nreps)
      pvalue
    }
      
    # df <- readRDS('CVpredObs.Rdata')
    df <- CVpredObs()
    bst <- names(topModels())[[1]]
    print(bst)
    best <- df$pred[df$model==bst]-df$obs[df$model==bst]
    
    sigTable <<- df %>% #filter(model!=bst) %>% 
      group_by(model) %>% 
      summarise(`p-value`=permute(pred-obs,best)) %>% 
      .[rev(order(.$'p-value')),]
  

  })
  
  
  
  # Outputs ---------------------------------------------------------------------
  
  output$sgTable <- renderTable({
    sigTable
  })
  
  
  output$testsetPlot <- renderPlot({
    
    
    df <- data.frame(obs=dataTest$y,pred=testPreds()$c)
    
    col <- pal[topModels()[[1]]]
    
    if(isolate(modelType)=='Regression'){
      lims <- c(min(df$obs),max(df$obs))
      ggplot(df)+
        geom_abline(alpha=0.5)+
        geom_point(aes(x=obs,y=pred),color=col,size=2)+
        scale_x_continuous(limits = lims)+
        scale_y_continuous(limits = lims)+
        # scale_color_manual(values=pal)+
        coord_equal()+
        # facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
    } else {
      df$pred <- factor(df$pred,levels=levels(df$obs))
      df %>% group_by(pred,obs) %>% 
        summarise(n=n()) %>% 
        ggplot(.)+
        geom_raster(aes(x=obs,y=pred,alpha=n),fill=col)+
        geom_text(aes(x=obs,y=pred,label=n))+
        # scale_fill_manual(values=pal)+
        coord_equal()+
        # facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
      
    }
    
  })
  
  output$testsetS1 <- renderValueBox({
    
    lab <- ifelse(isolate(modelType)=='Regression','Variance explained','Accuracy')
    
    valueBox(paste(round(testPreds()$s1*100,1),'%'),lab,icon = icon('cube'))
    
  })
  
  output$testsetS2<- renderValueBox({
    lab <- ifelse(isolate(modelType)=='Regression','RMSE','Kappa')
    valueBox(round(testPreds()$s2,3),subtitle = lab,icon = icon('cube'))
  })
  
  
  
  
  output$rawdata <- renderDataTable({rawdata()},
                                    options = list(pageLength = 10,searching = FALSE))
  
  output$model_info <- renderDataTable({
    CVres()[c(7,1:6)]
    
  },    options = list(rowCallback = I(
    lapply(1:length(mdls),function(i) tableCSS(mdls[i],pal[i])) %>% 
      unlist %>% 
      paste(.,collapse = '') %>% 
      paste('function(row, data) {',.,'}')
  ),
  pageLength = 10,searching = FALSE
  )
  )
  
  
  output$CVplot2 <- renderPlot({
    
    type <- isolate(modelType)
    df <-CVpredObs()

    if(type=='Regression'){
      lims <- c(min(df$obs),max(df$obs))
      ggplot(df)+
        geom_abline(alpha=0.5)+
        geom_point(aes(x=obs,y=pred,col=name))+
        scale_x_continuous(limits = lims)+
        scale_y_continuous(limits = lims)+
        scale_color_manual(values=pal)+
        coord_equal()+
        facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
    } else {
      df %>% group_by(pred,obs,name) %>% 
        summarise(n=n()) %>% 
        ggplot(.)+
        geom_raster(aes(x=obs,y=pred,fill=name,alpha=n))+
        geom_text(aes(x=obs,y=pred,label=n))+
        scale_fill_manual(values=pal)+
        coord_equal()+
        facet_wrap(~name)+
        theme_bw()+
        xlab('Observed')+
        ylab('Predicted')+
        theme(legend.position='none')
      
    }
  })
  
  output$CVplot1 <- renderPlot({
    resdf <- CVres()
    type <- isolate(modelType)
    
    resdf$model <- factor(resdf$model,levels = rev(resdf$model[resdf$rank]))
    if(type=='Regression'){
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=RMSE-RMSESD,ymax=RMSE+RMSESD),size=1)+
        geom_point(aes(y=RMSE),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p1
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Rsquared-RsquaredSD,ymax=Rsquared+RsquaredSD),size=1)+
        geom_point(aes(y=Rsquared),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p2
    } else {
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Kappa-KappaSD,ymax=Kappa+KappaSD),size=1)+
        geom_point(aes(y=Kappa),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p1
      ggplot(resdf,aes(x=model,color=name))+
        geom_errorbar(aes(ymin=Accuracy-AccuracySD,ymax=Accuracy+AccuracySD),size=1)+
        geom_point(aes(y=Accuracy),size=3)+
        scale_color_manual(values=pal)+
        coord_flip()+
        theme_bw()+
        xlab('')+
        theme(legend.position='none') -> p2
    }
    
    gridExtra::grid.arrange(p2,p1,ncol=2)
    
  })
  
  output$AIcase_1 <- renderText(AI_description$`Use Case1`[which(input$casevar==AI_description$`Sub-category`)])
  output$AIdes_1 <- renderText(AI_description$Description[which(input$casevar==AI_description$`Sub-category`)])
  
  
  output$Ytype <- renderText(class(dataTrain$y))
  output$txt_dataset <- renderPrint(cat('Dataset:',input$dataset))
  output$txt_n <- renderPrint(cat('n obs:',nrow(rawdata())))
  output$txt_Yvar <- renderPrint(cat('Y var:',input$yvar))
  output$txt_testSet <- renderPrint(cat('Test set:',input$sld_testsplit,'%'))
  output$txt_Type <- renderPrint(cat('Model Type:',modelType))
  output$txt_CV <- renderPrint(cat('CV folds:',input$rdo_CVtype))
  output$txt_nModels <- renderPrint(cat('Models trained:',nrow(CVres())))
  output$txt_bestModel <- renderPrint(cat('Best Model:',(CVres()$model[1])))
  output$txt_bestModelStat1 <- renderPrint({
    if(modelType=='Regression'){
      cat('Variance Explained:',(CVres()$Rsquared[1]*100),'%')
    } else {
      cat('Accuracy:',(CVres()$Accuracy[1]))
    }
  })
  output$txt_bestModelStat2 <- renderPrint({
    if(modelType=='Regression'){
      cat('RMSE:',(CVres()$RMSE[1]))
    } else {
      cat('Kappa:',(CVres()$Kappa[1]))
    }
  })
  
  
  
  
  output$Ystats <- renderPrint({
    
    summary(dataTrain$y)
    
  })
  
  output$Yplot <- renderPlot({
    
    if(modelType=='Regression'){
      
      
      ggplot(dataTrain,aes(x=y))+
        geom_density(alpha=0.7,adjust=0.5,fill="#5BBCD6")+
        theme_bw()+
        ggtitle('Y Distribution')+
        xlab('')
      
      
      # wes_palettes$Darjeeling
      
    } else {
      pal <- wes_palette('Darjeeling',n = length(unique(dataTrain$y)),type = 'c')
      ggplot(dataTrain,aes(x=y,fill=y))+
        geom_bar(stat='count')+
        scale_fill_manual(values=pal)+
        xlab('')+
        ggtitle('Y Class Frequency')+
        coord_flip()+
        theme(legend.position='none')
    }
    
  })
  
  output$featImp <- renderPlot({
    
    rf <- randomForest(y~.,dataTrain)
    vi <- as.data.frame(varImpPlot(rf))
    vi$Feature <- row.names(vi)
    names(vi)[1] <- 'Score'
    vi$Feature <- factor(vi$Feature,levels=vi$Feature[order(vi$Score)])
  str(vi)  
    ggplot(vi,aes(x=Feature,y=Score))+
      geom_bar(stat='identity',fill="#5BBCD6")+
      coord_flip()+
      xlab('')+
      ylab('Relative Importance Score')
    
  })
  
  
}


# UI ----------------------------------------------------------------------

ui <- bootstrapPage(useShinyjs(),
                    # Add custom CSS & Javascript;
                    tagList(tags$head(
                      tags$link(rel="stylesheet", type="text/css",href="style.css"),
                      tags$script(type="text/javascript", src = "busy.js"),
                      lapply(1:length(mdls),function(i) modelCSS(mdls[i],pal[i]))
                      
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
                                      menuItem("census map", tabName = "almap", icon = icon("area-chart")),
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
                                    fluidRow(
                                      column(width=1),
                                      column(width=10,
                                             h5(textOutput('txt_dataset')),
                                             h5(textOutput('txt_n')),
                                             h5(textOutput('txt_Yvar')),
                                             h5(textOutput('txt_testSet'))
                                             
                                             
                                      ),
                                      column(width=1)
                                    ),
                                    absolutePanel(
                                      bottom = 10,
                                      left = 10,
                                      draggable = F,
                                      width='100%',
                                      height='auto',
                                      a(icon('github fa-2x'),href='https://github.com/ZEKAICHEN/shiny-examples/tree/master/063-superzip-example',target='_blank')
                                    )                  
                                  ),
                                  dashboardBody(
                                    tabItems(
                                      tabItem("setup",
                                              box(width = 'auto', height = 'auto', title = 'Input Dataset',solidHeader = T,color="light-blue",
                                                  selectInput('dataset',label = 'Choose Dataset',
                                                              choices = names(datasets),selected='iris'),
                                                  fileInput('fileIn',label = 'Upload data'), #%>% disabled(),
                                                  actionButton('btn_viewData',label = 'View Data',icon=icon('table')),
                                                  hr(),
                                                  #-------------------------------------------------------------------------------------------------------------------
                                                  
                                                  sliderInput('sld_testsplit',label = label.help('Test set %','lbl_testsplit'),min = 33,max = 90,step = 1,value = 33),
                                                  bsTooltip(id = "lbl_testsplit", title = "% of data to set aside for test data", 
                                                            placement = "right", trigger = "hover")
                                                  
                                                  
                                              ),
                                              # box(width=4,title = 'Prediction',solidHeader = T,color="light-blue",#status = 'primary',background = "black",
                                              #     helpText('Select the variable you would like to predict'),
                                              #     selectizeInput('yvar',label=label.help('variable','lbl_yvar'),choices = character(0)),
                                              #     #helpText(HTML(paste('data type:', textOutput('Ytype')))),
                                              #     bsTooltip(id = "lbl_yvar", title = "Variable to predict", 
                                              #               placement = "right", trigger = "hover"),
                                              #     hr(),
                                              #     plotOutput('Yplot',height=260),
                                              #     conditionalPanel("output.Ytype == 'numeric'|output.Ytype == 'integer'",
                                              #                      checkboxInput('chk_logY',label = 'log transform')
                                              #     ),
                                              #     verbatimTextOutput('Ystats')
                                              #     
                                              # ),
                                              # box(width=4,title = 'Features',solidHeader = T,color="light-blue",#status = 'primary',
                                              #     helpText('Select features you would like to use'),
                                              #     selectizeInput('xvar',label=label.help('X (Predict Y as function of):','lbl_xvar'),choices = character(0),multiple = T),
                                              #     bsTooltip(id = "lbl_xvar", title = "Try and predict Y as function of these variables", 
                                              #               placement = "right", trigger = "hover")
                                              # ),
                                              bsModal('data',title = 'Dataset',trigger = 'btn_viewData',size = 'large',
                                                      dataTableOutput('rawdata')
                                              )
                                      ),
                                      tabItem("model",
                                              # bsModal('mdl_tune','Tuning Options',trigger = 'btn_tune',
                                              
                                              # ),
                                              column(width=3,
                                                     box(width = 14,title = 'Model Options',solidHeader = T,
                                                         selectInput('slt_algo',label = 'Algorithm:'%>%label.help('lbl_algo'),
                                                                     choices = reg.mdls,selected = reg.mdls,multiple=T),
                                                         selectizeInput('slt_Tune','Parameter Tuning'%>%label.help('lbl_Tune'),
                                                                        choices = c('Coarse auto-tune (fast)','Fine auto-tune (slow)','manual')),
                                                         # actionButton('btn_tune',label = 'Tuning Options',icon = icon('sliders')
                                                         # ),
                                                         # p(),
                                                         
                                                         selectizeInput('rdo_CVtype',label = 'Cross-validation folds'%>%label.help('lbl_CV'),
                                                                        choices = c('3-fold'=3,'5-fold'=5,'10-fold'=10)),
                                                         
                                                         actionButton('btn_train',label = 'Train Models',
                                                                      icon = icon('rocket'),#'bullseye','rocket','cogs'
                                                                      class='btn-danger fa-lg',
                                                                      width='100%'),
                                                         bsTooltip(id = "lbl_algo", title = "Which algorithms to test", 
                                                                   placement = "right", trigger = "hover"),
                                                         bsTooltip(id = "lbl_Tune", title = "Type of tuning which is performed to optimize model parameters", 
                                                                   placement = "right", trigger = "hover"),
                                                         bsTooltip(id = "lbl_CV", title = "Number of splits of training data used to tune parameters", 
                                                                   placement = "right", trigger = "hover")
                                                         
                                                     ),
                                                     box(width = 14,title = 'Summary',solidHeader = T, status = 'primary',
                                                         helpText(textOutput('txt_bestModel')),
                                                         helpText(textOutput('txt_bestModelStat1')),
                                                         helpText(textOutput('txt_bestModelStat2')),
                                                         hr(),
                                                         helpText(textOutput('txt_Type')),
                                                         helpText(textOutput('txt_CV')),
                                                         helpText(textOutput('txt_nModels'))
                                                         
                                                     )
                                              )
                                              ,
                                              tabBox(width = 9,
                                                     tabPanel(title = 'CV Model Rank',#icon = icon('sort-amount-asc'),
                                                              h4('Cross-validation results'),
                                                              plotOutput('CVplot1',height=600)
                                                     ),
                                                     tabPanel(title = 'CV Pred vs Obs',
                                                              h4('Observed vs Predicted (best candidate for algorithm)'),
                                                              plotOutput('CVplot2',height=600)
                                                     ),
                                                     tabPanel(title = 'CV Stats',
                                                              h4('Performance statiscs from cross-validation'),
                                                              
                                                              dataTableOutput('model_info')
                                                     )
                                                     # tabPanel(title = 'CV Sig testing',
                                                     #          h4('Statisical significance of cross-validation results'),
                                                     #          helpText('Perform permutation test of error statisic to determine
                                                     #                   whether the score of the best model was significantly higher
                                                     #                   than the other candidates.'),
                                                     #          actionButton('btn_sigTest',label = 'Perform permutation test'),
                                                     #          tableOutput('sgTable')
                                                     #          
                                                     #          
                                                     # )
                                              )
                                      ),
                                      tabItem("test",
                                              column(width=3,
                                                     box(width = 12,title = 'Test Set Predictions',solidHeader = F,status = 'primary',
                                                         # radioButtons('rdo_finalModel','Final model',
                                                         #              c('Best Model','Ensemble of top models')),
                                                         selectInput('slt_Finalalgo',label = 'Final Model:'%>%label.help('lbl_Finalalgo'),
                                                                     choices=mdls,multiple=T),
                                                         helpText('The best cross-validated model is selected by default. 
                                                                  Multiple models can be selected to make ensemble predictions'),
                                                         bsTooltip(id = "lbl_Finalalgo", title = "Which algorithms to use to predict test", 
                                                                   placement = "right", trigger = "hover")
                                                         
                                                         ),
                                                     valueBoxOutput('testsetS1',width=12),
                                                     valueBoxOutput('testsetS2',width=12)
                                              ),
                                              box(width = 6,title = 'Test Set observed vs Predicted',
                                                     solidHeader = T,status = 'primary',
                                                     plotOutput('testsetPlot')
                                              )
                                      ),
                                      tabItem("imp",
                                              box(width = 6,title = 'Feature importance', solidHeader = T, status = 'primary',
                                                  helpText('Relative feature importance indicated from randomForest'),
                                                  
                                                  plotOutput('featImp')
                                              )
                                      ),
                                      tabItem("almap",
                                              fluidRow(
                                                     box(width = 9, height = "440px",solidHeader = T,
                                                         plotOutput('map')
                                                     ),
                                                     box(width = 3, height = "440px",title = "Agriculture distribution map",solidHeader = T,
                                                         helpText('This is an agriculture map'),
                                                         radioButtons("var", 
                                                                      label = "Choose a variable to display",
                                                                      choices = list("Percent White", "Percent Black",
                                                                                     "Percent Hispanic", "Percent Asian"),
                                                                      selected = "Percent White"),
                                                         sliderInput("range", 
                                                                     label = "Range of interest:",
                                                                     min = 0, max = 100, value = c(10, 90))
                                                     )
                                              )
                                      ),
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
