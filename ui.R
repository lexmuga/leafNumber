#### Load necessary packages and data ####
library(shiny)
library(networkD3)

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Leaf Number of a Circulant Network"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N",  "Input the number of nodes: ",
                   min = 5, max = 1000, value = 91, step = 1),
      numericInput("K", "Input an even number of links per node: ",
                   min = 2, max = 500, value = 14, step = 2),
      
      helpText("Note: Two types of maximum leaf spanning tree (MLST)", 
               "of all undirected circulant networks: ",
               "Spanning Tree I: One branch from root 0, and",
               "Spanning Tree II: Two branches from root 0.",
               "They are the same if the number of internal nodes",
               "is at most 2.",
               "Nodes are labelled from 0, 1, up to N-1.",
               "*****************************************",
               "Please scroll down to see the ui.R and server.R scripts below."),
        
      submitButton("Update Network")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Circulant Network", simpleNetworkOutput("simple")),
        tabPanel("Jump Sizes", tableOutput("jumpSizes")),
        tabPanel("Node Types", tableOutput("nodeTypes")),
        tabPanel("MLST I", treeNetworkOutput("oneBranchTree")),
        tabPanel("Branch: MLST I", tableOutput("intNodes1")),
        tabPanel("MLST II", treeNetworkOutput("twoBranchTree")),
        tabPanel("Branches: MLST II", tableOutput("intNodes2"))
      )
    )
  )
))
