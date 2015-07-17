#### Load necessary packages and data ####
library(shiny)
library(networkD3)

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Leaf Number of a Circulant Network"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N",  "Input the number of nodes: ",
                   min = 5, max = 1000, value = 10, step = 1),
      numericInput("K", "Input an even number of links per node: ",
                   min = 2, max = 500, value = 4, step = 2),
      
      helpText("Note: Two types of maximum leaf spanning tree (MLST)", 
               "of all undirected circulant networks: ",
               "Spanning Tree I: One branch from root 0, and",
               "Spanning Tree II: Two branches from root 0.",
               "Nodes are labelled from 0, 1, up to N-1.",
               "*****************************************",
               "Please scroll down to see the ui.R and server.R scripts below."),
        
      submitButton("Update View")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Circulant Network", simpleNetworkOutput("simple")),
        tabPanel("Max. Spanning Tree I", treeNetworkOutput("onebranchtree")),
        tabPanel("Max. Spanning Tree II", treeNetworkOutput("twobranchtree")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
  
))
