# Load igraph
library(igraph)

# Plot the network
plot(g1)

# Convert the gender attribute into a numeric value
values <- as.numeric(factor(V(g1)$gender))

# Calculate the assortativity of the network based on gender
assortativity(g1, values)

# Calculate the assortativity degree of the network
assortativity.degree(g1, directed = FALSE)

# Make a plot of the chimp grooming network
plot(g,
     edge.color = "black",
     edge.arrow.size = 0.3,
     edge.arrow.width = 0.5)

# Calculate the reciprocity of the graph
reciprocity(g)


# Perform fast-greedy community detection on network graph
kc = fastgreedy.community(g)

# Determine sizes of each community
sizes(kc)

# Determine which individuals belong to which community
membership(kc)

# Plot the community structure of the network
plot(kc, g)


# Perform edge-betweenness community detection on network graph
gc = edge.betweenness.community(g)

# Determine sizes of each community
sizes(gc)

# Plot community networks determined by fast-greedy and edge-betweenness methods side-by-side
par(mfrow = c(1, 2)) 
plot(kc, g)
plot(gc, g)


library(igraph)
library(threejs)

# Set a vertex attribute called 'color' to 'dodgerblue' 
g <- set_vertex_attr(g, "color", value = "dodgerblue")

# Redraw the graph and make the vertex size 1
graphjs(g, vertex.size = 1)



# Create numerical vector of vertex eigenvector centralities 
ec <- as.numeric(eigen_centrality(g)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v <- 5*sqrt(ec)

# Plot threejs plot of graph setting vertex size to v
graphjs(g, vertex.size = v)


# Create an object 'i' containin the memberships of the fast-greedy community detection
i <-  membership(kc)

# Check the number of different communities
sizes(kc)

# Add a color attribute to each vertex, setting the vertex color based on community membership
g <- set_vertex_attr(g, "color", value = c("yellow", "blue", "red")[i])

# Plot the graph using threejs
graphjs(g)


# Calculate the observed assortativity
observed.assortativity <- assortativity(g1, values)

# Calculate the assortativity of the network randomizing the gender attribute 1000 times
results <- vector('list', 1000)
for(i in 1:1000){
  results[[i]] <- assortativity(g1, sample(values))
}

# Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results))
abline(v = observed.assortativity, col = "red", lty = 3, lwd=2)