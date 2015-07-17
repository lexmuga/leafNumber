#### Load necessary packages and data ####
library(shiny)
library(networkD3)

#### Server ####
server <- function(input, output) {
  
  output$simple <- renderSimpleNetwork({
    N <- input$N
    k <- round(input$K/2,0)
    S <- findJumpSizes(N,k)
    circulantData <- sourceTargetTable(N,S)
    simpleNetwork(circulantData, opacity = 0.9)
  })
  
  output$onebranchtree <- renderTreeNetwork({
    N <- input$N
    k <- round(input$K/2,0)
    S <- findJumpSizes(N,k)
    treeData <- findPC1B(N, k)
    rsTreeData <- rsplit(recursiveStructure(treeData$p, treeData$L))[[1]]
    treeNetwork(rsTreeData, height = NULL, width = NULL, fontSize = 10,
                fontFamily = "serif", linkColour = "#ccc", nodeColour = "#fff",
                nodeStroke = "steelblue", textColour = "#111", opacity = 0.9,
                margin = 0)
  })
  
  output$twobranchtree <- renderTreeNetwork({
    N <- input$N
    k <- round(input$K/2,0)
    S <- findJumpSizes(N,k)
    ifelse(N <= 4 * k,
           treeData <- findPC1B(N, k),
           treeData <- findPC2B(N, k)
    )
    rsTreeData <- rsplit(recursiveStructure(treeData$p, treeData$L))[[1]]
    treeNetwork(rsTreeData, height = NULL, width = NULL, fontSize = 10,
                fontFamily = "serif", linkColour = "#ccc", nodeColour = "#fff",
                nodeStroke = "steelblue", textColour = "#111", opacity = 0.9,
                margin = 0)
  })
  
  output$table <- renderTable({
    N <- input$N
    k <- round(input$K/2,0)
    L <- round(leafNumber(N,k),0)
    M <- round(connectedDominationNumber(N,k),0)
    data.frame(Leaf_Number = L, Internal_Nodes=M, Total_Nodes=N)
  })

## createCirculant
sourceTargetTable <- function(N,S) {
  nodeSet <- c(0:N-1)
  degree <- length(S)
  sourceNodes <- vector()
  targetNodes <- vector()
  for(node in 1:N-1){
    sourceNodes <- append(sourceNodes, rep(node, degree))
    targetNodes <- append(targetNodes, (node+S) %% N)
  }
  data.frame(src = sourceNodes, target = targetNodes)
}

drawCirculant <- function(N,S){
  circulantData <- sourceTargetTable(N,S)
  simpleNetwork(circulantData)
}

leafNumber <- function(N,k){
  q <- (N-2) %/% (2*k - 1)
  r <- (N-2) %% (2*k - 1)
  ifelse(r == 0,
         maxLeafNumber <- (2*k - 2)*q + 2,
         maxLeafNumber <- (2*k - 2)*q + r + 1
  )
}

connectedDominationNumber <- function(N,k){
  N - leafNumber(N, k)
}

findJumpSizes <- function(N,k){
  if(N - 2*k - 1 < 0){L <- NA}
  if(N - 2*k - 1 >= 0){
    S <- vector()
  }
  q <- (N - 2) %/% (2*k - 1)
  r <- (N - 2) %% (2*k - 1)
  if( r == 0 | r == 1){
    for(i in 1:k-1){
      u <- (i*q + 1) %% N
      S <- append(S, c(u, N-u))
    }
  }
  if(r >= 2){
    r1 <- floor(r / 2)
    for(i in 1:(k-r1)-1){
      u1 <- (i * q + 1) %% N
      S <- append(S, c(u1, N-u1))
    }
    for(j in 1:r1 - 1){
      u2 <- ((k - r1 + j)*q + j +2) %% N
      S <- append(S, c(u2, N-u2))
    }
  }
  S <- sort(S)
  S
}

findPC1B <- function(N,k){
  if(N - 2*k - 1 < 0){
    p <- NULL
    L <- NULL
    list(p=p,L=L)
  }
  if(N - 2*k - 1 >= 0){
    S <- findJumpSizes(N,k)
    L <- vector(mode="list", length=N)
    names(L) <- c(1:N-1)
    p <- vector(mode="list", length=N)
    names(p) <- c(1:N-1)
    
    q <- (N - 2) %/% (2*k - 1)
    r <- (N - 2) %% (2*k - 1)
    
    L['0'] <- list(S)
    
    for(v in S){
      p[as.character(v)] <- 0
    }
    
    for(i in 1:(2*k-1)-1){
      d <- S[i+2] - S[i+1] - 1
      if(d > 0){
        for(j in 1:d){
          v <- (S[i+1]+j) %% N
          if(v <= q){
            p[as.character(v)] <- v - 1
            L[as.character(v-1)][[1]] <- append(L[as.character(v-1)][[1]],v)
          }
          if(v > q){
            p[as.character(v)] <- j
            L[as.character(j)][[1]] <- append(L[as.character(j)][[1]],v)
          }
        }
      }
    }
    list(p=p,L=L)
  }
}

findPC2B <- function(N,k){
  if(N - 2*k - 1 < 0){L <- NA}
  if(N - 2*k - 1 >= 0){
    if(N <= 4*k){
      pL <- findPC1B(N,k)
      pL
    }
    if(N > 4*k){
      S <- findJumpSizes(N,k)
      L <- vector(mode="list", length=N)
      names(L) <- c(1:N - 1)
      p <- vector(mode="list", length=N)
      names(p) <- c(1:N - 1)
      
      q <- (N - 2) %/% (2*k - 1)
      r <- (N - 2) %% (2*k - 1)
      
      L['0'] <- list(S)
      S2 <-S[-(2*k)]
      T2 <- S[-1]
      d <- numeric()
      for(i in 1:(2*k-1)){
        d[i] <- S[i+1] - S[i] - 1
      }
      delta <- max(d)
      c1 <- ceiling(delta/2)
      c2 <- floor(delta/2)
      
      for(v in S){
        p[as.character(v)] <- 0
      }
      
      for(i1 in 1:c1){
        for(s in S2){
          p[as.character(s+i1)] <- i1
        }
        L[as.character(i1)] <- list((S2+i1) %% N)
      }
      for(i2 in 1:c2){
        for(t in T2){
          p[as.character(t-i2)] <- N-i2
        }
        L[as.character(N-i2)] <- list((T2-i2) %% N)
      }
      list(p=p, L=L)
    }
  }
}

classifyNodes <- function(L){
  N <- length(L); k <- length(L['0'][[1]])
  nodeMat <- matrix(nrow=N, ncol=3); nodeMat[, 1] <- c(1:N - 1)
  M <- 0
  for(v in 1:N - 1){
    m <- length(L[as.character(v)][[1]])
    if(m == 0){
      nodeMat[v+1,2] <- 1
    }
    if(m > 0){
      nodeMat[v+1,2] <- 0
      M <- M + 1
    }
  }
  nodeMat[1,3] <- 0
  j <- 0
  branch <- 1
  children <- L['0'][[1]]
  lenChildren <- length(children)
  for(i in 1:(M)){
    if(lenChildren > 0){
      for(child in children){
        idx <- child + 1
        nodeMat[idx, 3] <- j+1
      }
    }
    if(lenChildren == 0){
      branch <- 2
      for(child in L[as.character(N-1)][[1]]){
        idx <- child + 1
        j <- 1
        nodeMat[idx, 3] <- j+1
      }
    }
    j <- j+1
    ifelse(branch == 1,
           lenChildren <- length(L[as.character(j)][[1]]),
           lenChildren <- length(L[as.character(N-j)][[1]])
    )
    if(lenChildren > 0){
      ifelse(branch == 1,
             children <- L[as.character(j)][[1]],
             children <- L[as.character(N-j)][[1]]
      )
    }
  }
  
  nodeTypeDist <- data.frame(nodeMat)
  names(nodeTypeDist) <- c("node", "type", "dist")
  nodeTypeDist
}


recursiveStructure <- function(p,L){
  nodeData <- classifyNodes(L)
  numOfCol <- max(nodeData$dist)+1
  leaves <- subset(nodeData, nodeData$type == 1)
  numOfRow <- nrow(leaves)
  recMat <-matrix(nrow=numOfRow,ncol=numOfCol)
  recMat[,1] <- 0
  for(i in 1:numOfRow){
    node <- leaves$node[i]
    distance <- leaves$dist[i]
    recMat[i,distance+1] <- node
    parent <- p[as.character(node)][[1]]
    while(distance > 0){
      recMat[i, distance] <- parent
      distance <- distance - 1
      parent <- p[as.character(parent)][[1]]
    }
  }
  
  recMat
  recData <- data.frame(recMat)
  names(recData) <- paste('h',0:(numOfCol-1),sep="")
  recData
}

rsplit <- function(x) {
  x <- x[!is.na(x[,1]),,drop=FALSE]
  if(nrow(x)==0) return(NULL)
  if(ncol(x)==1) return(lapply(x[,1], function(v) list(name=v)))
  s <- split(x[,-1, drop=FALSE], x[,1])
  unname(mapply(function(v,n) {if(!is.null(v)) 
    list(name=n, children=v) else list(name=n)}, 
    lapply(s, rsplit), names(s), SIMPLIFY=FALSE))
}

drawMLST <- function(N,k,method=1){
  ifelse(method == 1,
         pL <- findPC1B(N,k),
         pL <- findPC2B(N,k)
  )
  rdata <- rsplit(recursiveStructure(pL$p,pL$L))[[1]]
  treeNetwork(rdata)
}
}