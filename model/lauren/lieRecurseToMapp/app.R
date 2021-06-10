
# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(stats4)
# library(rsconnect)
# rsconnect::deployApp('/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/lieRecurseToMapp')


# Load data
source("app_functions.R")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Model Predictions"),
                fluidRow(
                  column(width = 4, 
                         wellPanel(
                           selectInput('condition', HTML("conditions"), 
                                       choices=c("all","red","blue",paste(rep(c("red","blue"),each=3),rep(c(0.2,0.5,0.8),2)))),
                           sliderInput('alpha', HTML("&alpha;"), value = 0.15, min = 0, max = 1, step=0.05),
                           sliderInput('eta', HTML("&eta;<sub>S</sub>"), value = 11, min = -5, max = 25),
                           sliderInput('weight', HTML("lapse rate"), value = 0.95, min = 0, max = 1, step=0.05),
                           numericInput('lambda', HTML("levels of recursion"), 7, min = 1, max = 15),
                           checkboxInput('recursiveToM', HTML("recursive ToM"), value = TRUE),
                           checkboxInput('noToM', HTML("0th Order ToM"), value = TRUE)
                         )
                  ),
                  column(width = 6,
                         conditionalPanel(condition = "input.recursiveToM", plotOutput(outputId = "recursePlot", height = "450px", width = "700px")),
                         conditionalPanel(condition = "input.recursiveToM", htmlOutput(outputId = "recurseTxt"))
                  ),
                  column(width = 6, 
                         conditionalPanel(condition = "input.noToM", plotOutput(outputId = "noPlot", height = "450px", width = "700px")),
                         conditionalPanel(condition = "input.noToM", htmlOutput(outputId = "noTxt"))
                  )
                ),
                
                # fluidRow(
                #   
                #   )
                # ),
                
                fluidRow(
                  column(width = 3,
                         plotOutput(outputId = "exptPlot", height = "300px", width = "360px")
                  ),
                  column(width = 3,
                         plotOutput(outputId = "someLiesPlot", height = "300px", width = "360px"),
                         htmlOutput(outputId = "someLiesTxt")
                  ),
                  column(width = 3,
                         plotOutput(outputId = "recurseToMPlot", height = "300px", width = "360px"),
                         htmlOutput(outputId = "recurseToMTxt")
                  )
                )
)


# Define server function
server <- function(input, output) {
  
  # Subset data
  recurseDat <- reactive({
    modelPreds = recurseToM.pred(
      input$alpha,
      input$eta,
      0, #recurseToMeval@coef['eta.R','Estimate'],
      log(input$lambda),
      probToLogit(input$weight))[[2]]
  })
  
  noDat <- reactive({
    modelPreds = noToM.s.pred(
      input$alpha,
      input$eta,
      probToLogit(input$weight))
  })
  
  condition <- reactive({
    cond <- strsplit(input$condition, split=" ")[[1]]
    condexpt <- cond[1]
    condp <- as.numeric(cond[2])
    return(list(condexpt, condp))
  })
  
  condView <- reactive({
    cond <- condition()
    condStartNum <- case_when(
      cond[[1]] == "all" ~ 1,
      cond[[1]] == "red" & is.na(cond[[2]]) ~ 1,
      cond[[1]] == "blue" & is.na(cond[[2]]) ~ 4,
      cond[[1]] == "red" & cond[[2]] == 0.2 ~ 1,
      cond[[1]] == "red" & cond[[2]] == 0.5 ~ 2,
      cond[[1]] == "red" & cond[[2]] == 0.8 ~ 3,
      cond[[1]] == "blue" & cond[[2]] == 0.2 ~ 4,
      cond[[1]] == "blue" & cond[[2]] == 0.5 ~ 5,
      cond[[1]] == "blue" & cond[[2]] == 0.8 ~ 6
    )
    condEndNum <- case_when(
      cond[[1]] == "all" ~ 6,
      cond[[1]] == "red" & is.na(cond[[2]]) ~ 3,
      cond[[1]] == "blue" & is.na(cond[[2]]) ~ 6,
      cond[[1]] == "red" & cond[[2]] == 0.2 ~ 1,
      cond[[1]] == "red" & cond[[2]] == 0.5 ~ 2,
      cond[[1]] == "red" & cond[[2]] == 0.8 ~ 3,
      cond[[1]] == "blue" & cond[[2]] == 0.2 ~ 4,
      cond[[1]] == "blue" & cond[[2]] == 0.5 ~ 5,
      cond[[1]] == "blue" & cond[[2]] == 0.8 ~ 6
    )
    if(cond[[1]] == "all"){
      cond[[1]] = c("red", "blue")
    }
    if(is.na(cond[[2]])){
      cond[[2]] = c(0.2,0.5,0.8)
    }
    return(list(cond[[1]],cond[[2]],condStartNum,condEndNum))
  })
  
  # Create tile recursive plot
  output$recursePlot <- renderPlot({
    cond = condView()
    datToTibble <- recurseDat() %>% 
      asTibble() %>%
      filter(expt %in% cond[[1]] & p %in% cond[[2]])
    
    datToTibble %>%
      ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
      geom_tile() +
      geom_text(size=2.5) +
      ggtitle("Recursive ToM") +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
      facet_grid(expt ~ p) +
      theme(plot.title=element_text(size=24, face="bold"))
  })
  
  recurse <- reactive({
    cond = condView()
    dat = recurseDat()[,,cond[[3]]:cond[[4]]]
    humans = array(
      humanLieCounts, 
      dim=c(11,11,6))[,,cond[[3]]:cond[[4]]]
    
    recurseToMeval.s.diag = -2*eval.s(
      getDiag(dat),
      getDiag(humans)
    )
    recurseToMeval.s.lies = -2*eval.s(
      getLies(dat),
      getLies(humans)
    )
    return(list(recurseToMeval.s.diag, recurseToMeval.s.lies))
  })
  
  # Pull in description of trend
  output$recurseTxt <- renderText({
    eval = recurse()
    trueFit = paste("&emsp;&emsp;&emsp; truth (diagonal) model deviance:", round(eval[[1]]))
    liesFit = paste("&emsp;&emsp;&emsp; lies (non-diagonal) model deviance: ", round(eval[[2]]))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(eval[[1]] + eval[[2]]))
    paste(paste(trueFit, liesFit, totalFit, sep="<br/>"),"<br/><br/><br/>")
  })
  
  
  # Create tile 0th order ToM plot
  output$noPlot <- renderPlot({
    cond = condView()
    datToTibble <- noDat() %>% 
      asTibble() %>%
      filter(expt %in% cond[[1]] & p %in% cond[[2]])
    
    datToTibble %>%
      ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
      geom_tile() +
      geom_text(size=2.5) +
      ggtitle("0th Order ToM") +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
      facet_grid(expt ~ p) +
      theme(plot.title=element_text(size=24, face="bold"))
  })
  
  noToM <- reactive({
    cond = condView()
    dat = noDat()[,,cond[[3]]:cond[[4]]]
    humans = array(
      humanLieCounts, 
      dim=c(11,11,6))[,,cond[[3]]:cond[[4]]]
    
    recurseToMeval.s.diag = -2*eval.s(
      getDiag(dat),
      getDiag(humans)
    )
    recurseToMeval.s.lies = -2*eval.s(
      getLies(dat),
      getLies(humans)
    )
    return(list(recurseToMeval.s.diag, recurseToMeval.s.lies))
  })
  
  # Pull in description of trend
  output$noTxt <- renderText({
    eval = noToM()
    trueFit = paste("&emsp;&emsp;&emsp; truth (diagonal) model deviance:", round(eval[[1]]))
    liesFit = paste("&emsp;&emsp;&emsp; lies (non-diagonal) model deviance: ", round(eval[[2]]))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(eval[[1]] + eval[[2]]))
    paste(paste(trueFit, liesFit, totalFit, sep="<br/>"),"<br/><br/><br/>")
  })
  
  
  output$exptPlot <- renderPlot({
    expt.S %>%
      ggplot(aes(x=k, y=ksay, fill=prob, label=probTxt)) +
      geom_tile() +
      geom_text(size=1.5) +
      ggtitle("Experiment Data") +
      guides(fill = FALSE) +
      scale_x_continuous("", expand=c(0,0)) +
      scale_y_continuous("", expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5) +
      facet_grid(expt ~ p)
  })
  
  
  origModel <- reactive({
    cond = condition()
    load(paste0("Rdata/recurseToMfit_",cond[[1]],cond[[2]],".Rdata"))
    origModelPred <- recurseToM.pred(
      recurseToMeval@coef['alph','Estimate'],
      recurseToMeval@coef['eta.S','Estimate'],
      recurseToMeval@coef['eta.R','Estimate'],
      recurseToMeval@coef['lambda','Estimate'],
      recurseToMeval@coef['weight','Estimate'])[[2]] 
    return(origModelPred)
  })
  
  origEval <- reactive({
    cond = condView()
    origModelPred <- origModel()[,,cond[[3]]:cond[[4]]]
    humans <- array(
      humanLieCounts, 
      dim=c(11,11,6))[,,cond[[3]]:cond[[4]]]
    
    origModel.diag = -2*eval.s(
      getDiag(origModelPred),
      getDiag(humans)
    )
    origModel.lies = -2*eval.s(
      getLies(origModelPred),
      getLies(humans)
    )
    return(list(origModel.diag, origModel.lies))
  })
  
  
  
  output$recurseToMPlot <- renderPlot({
    cond = condView()
    origModel() %>%
      asTibble() %>%
      filter(expt %in% cond[[1]] & p %in% cond[[2]]) %>%
      ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
      geom_tile() +
      geom_text(size=1.5) +
      ggtitle("Recursive ToM (Optimized)") +
      guides(fill = FALSE) +
      scale_x_continuous("", expand=c(0,0)) +
      scale_y_continuous("", expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
      facet_grid(expt ~ p)
  })
  
  output$recurseToMTxt <- renderText({
    eval = origEval()
    trueFit = paste("&emsp;&emsp;&emsp; truth model deviance:", round(eval[[1]]))
    liesFit = paste("&emsp;&emsp;&emsp; lies model deviance: ", round(eval[[2]]))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(eval[[1]] + eval[[2]]))
    paste(trueFit, liesFit, totalFit, sep="<br/>")
  })
  
  
  someLiesModel <- reactive({
    someLies.LL <- function(pTrue, lambda, weight){
      ns.l = humanLieCounts
      
      -eval.s(
        someLies.pred(pTrue, lambda, weight),
        ns.l
      )
    }
    someLies.fit <- summary(mle(someLies.LL,
                                start=list(pTrue=rnorm(1,0,1),
                                           lambda=rnorm(1,0,1),
                                           weight=rnorm(1,0,1)),
                                method = "BFGS"))
    
    someLies.preds <- someLies.pred(
      someLies.fit@coef['pTrue','Estimate'], 
      someLies.fit@coef['lambda','Estimate'],
      someLies.fit@coef['weight','Estimate']
    )
    return(someLies.preds)
  })
  
  someEval <- reactive({
    cond <- condView()
    someLies.preds <- array(
      someLiesModel(),
      dim=c(11,11,6))[,,cond[[3]]:cond[[4]]]
    humans <- array(
      humanLieCounts, 
      dim=c(11,11,6))[,,cond[[3]]:cond[[4]]]
    
    somePeopleLie.diag = -2*eval.s(
      getDiag(someLies.preds),
      getDiag(humans)
    )
    somePeopleLie.lies = -2*eval.s(
      getLies(someLies.preds),
      getLies(humans)
    )
    return(list(somePeopleLie.diag, somePeopleLie.lies))
  })
  
  output$someLiesPlot <- renderPlot({
    cond = condView()
    someLies = someLiesModel()
    
    someLies %>%
      as_tibble() %>% 
      mutate(ksay = rep(0:10, 11)) %>%
      pivot_longer(-ksay, names_to = 'condition', values_to='probability') %>%
      mutate(condition = as.numeric(substr(condition, 2, 10))-1,
             expt = ifelse(condition < ceiling(max(condition)/2), "red", "blue"),
             expt = factor(expt, levels=c("red","blue")),
             p = condition %% 3,
             p = 0.2 + 0.3*p,
             k = rep(0:10, each=66),
             probTxt = paste0(round(probability*100), "%")) %>%
      select(-condition) %>%
      relocate(c(expt, p, k), .before = ksay) %>%
      arrange(expt, p, k, ksay) %>%
      filter(expt %in% cond[[1]] & p %in% cond[[2]]) %>%
      ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
      geom_tile() +
      geom_text(size=1.5) +
      ggtitle("Some People Lie") +
      guides(fill = FALSE) +
      scale_x_continuous("", expand=c(0,0)) +
      scale_y_continuous("", expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
      facet_grid(expt ~ p)
  })
  
  output$someLiesTxt <- renderText({
    somePeopleLie <- someEval()
    trueFit = paste("&emsp;&emsp;&emsp; truth model deviance:", round(somePeopleLie[[1]]))
    liesFit = paste("&emsp;&emsp;&emsp; lies model deviance: ", round(somePeopleLie[[2]]))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(somePeopleLie[[1]] + somePeopleLie[[2]]))
    paste(trueFit, liesFit, totalFit, sep="<br/>")
  })
  
}




# Create Shiny object
shinyApp(ui = ui, server = server)

