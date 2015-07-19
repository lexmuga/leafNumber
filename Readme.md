This app shows the leaf number of a family of undirected and connected circulant networks of order N and degree 2k. The leaf number of a graph is defined to be the maximum number of leaf nodes of all the spanning trees of a graph.

In a communication network based on a circulant graph if a message is sent within the network, this app presents a configuration with the maximum number of receiver-nodes (leaves) in a minimum number of repeaters (internal nodes including the sender). This is shown by the two maximum spanning trees in the app. Maximum Spanning Tree II minimizes the number of steps.

An undirected circulant graph is denoted by C(N,S) where N is the number of nodes and S are its jump sizes. 

The nodes in a circulant graph are labelled 0,1,..., N-1. 

Its jump sizes are s1, s2, ..., sk, N-sk, ..., N-s2, N-s1 where s_ = 1 and N > 2k + 1. 

A node v in a circulant graph is adjacent to v + si  and v + N - si for all i=1,2,...,k.


A spanning tree of a graph is a subgraph of the graph and contains all the nodes of the graph. A rooted tree is a tree in which a special node called the root is singled out. A leaf of a rooted tree is node with one link only. An internal node is a node with two or more links. The root is considered as an internal node.

The Maximum Leaf Spanning Tree of a graph is a spanning tree of the graph which has the maximum number of nodes.

References:

Felix P. Muga II, On Circulant Graphs with the Maximum Leaf Number Property and its One-to-Many Communication Scheme, Transactions on Engineering Technologies, pp. 327-341, Springer 2015.

Felix P. Muga II, On the Maximum Leaf Number of a Family of Circulant Graphs, http://www.iaeng.org/publication/WCECS2014/WCECS2014_pp46-49.pdf.

Christopher Gandrud and JJ Allaire, networkD3: D3 Javascript Network Graphs from R, https://christophergandrud.github.io/ networkD3/

http://shiny.rstudio.com, an RStudio project, 2014 RStudio, Inc.
