#setwd("/Users/loey/Desktop/Research/FakeNews/Bullshitter/model/lauren/mleFits/")

# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)


# Load data
source("app_functions.R")


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Recursive ToM Predictions"),
                fluidRow(
                  column(width = 4, 
                         wellPanel(
                           sliderInput('alpha', HTML("&alpha;"), value = 0.15, min = 0, max = 1, step=0.05),
                           sliderInput('eta', HTML("&eta;<sub>S</sub>"), value = 9, min = -5, max = 25),
                           sliderInput('weight', HTML("lapse rate"), value = 0.9, min = 0, max = 1, step=0.05),
                           numericInput('lambda', HTML("levels of recursion"), 7, min = 1, max = 15)
                         )
                  ),
                  column(width = 6,
                         plotOutput(outputId = "tilePlot", height = "450px", width = "700px"),
                         htmlOutput(outputId = "evalTxt")
                  )
                ),
                
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
  dat <- reactive({
    modelPreds = recurseToM.pred(
      input$alpha,
      input$eta,
      recurseToMeval@coef['eta.R','Estimate'],
      log(input$lambda),
      probToLogit(input$weight))[[2]]
  })
  
  # Create tile plot
  output$tilePlot <- renderPlot({
    datToTibble <- dat() %>% 
      as_tibble() %>% 
      mutate(ksay = 0:10) %>% 
      pivot_longer(-ksay, names_to = 'k', values_to='probability') %>% 
      mutate(k = as.numeric(substr(k, 2, 10))-1,
             expt = ifelse(k < ceiling(max(k)/2), "red", "blue"),
             expt = factor(expt, levels=c("red","blue"))) %>%
      relocate(k, .before = ksay) %>%
      arrange(k, ksay) %>%
      mutate(p = rep(rep(c(0.2, 0.5, 0.8), each=121),2),
             p = as.factor(p),
             k = k %% 11,
             probTxt = paste0(round(probability*100),"%")) %>%
      relocate(c(expt,p), .before = k) %>%
      arrange(expt, p, k, ksay) 
    
    datToTibble %>%
      ggplot(aes(x=k, y=ksay, fill=probability, label=probTxt)) +
      geom_tile() +
      geom_text(size=2.5) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_gradient2(low="white", mid="darkorchid", high="blue", midpoint=0.5, limits=c(0,1)) +
      facet_grid(expt ~ p)
  })
  
  eval <- reactive({
    dat = dat()
    humans = array(
      humanLieCounts, 
      dim=c(11,11,6))
    
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
  output$evalTxt <- renderText({
    eval = eval()
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
  
  
  
  output$recurseToMPlot <- renderPlot({
    asTibble(origModelPred) %>%
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
    trueFit = paste("&emsp;&emsp;&emsp; truth model deviance:", round(origModel.diag))
    liesFit = paste("&emsp;&emsp;&emsp; lies model deviance: ", round(origModel.lies))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(origModel.diag + origModel.lies))
    paste(trueFit, liesFit, totalFit, sep="<br/>")
  })
  
  
  
  output$someLiesPlot <- renderPlot({
    someLies.preds.df %>%
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
    trueFit = paste("&emsp;&emsp;&emsp; truth model deviance:", round(somePeopleLie.diag))
    liesFit = paste("&emsp;&emsp;&emsp; lies model deviance: ", round(somePeopleLie.lies))
    totalFit = paste("&emsp;&emsp;&emsp; total model deviance: ", round(somePeopleLie.diag + somePeopleLie.lies))
    paste(trueFit, liesFit, totalFit, sep="<br/>")
  })
  

}




# Create Shiny object
shinyApp(ui = ui, server = server)

