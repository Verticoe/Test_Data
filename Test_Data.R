################################################################################
# Distinct Clusters
################################################################################
#library(ggplot2)

set.seed(1)
class <- "DistinctClusters"


# Cluster 1
Cluster_1_X <- round(rnorm(400, mean = 15, sd = 5), digits = 2)
Cluster_1_Y <- round(rnorm(400, mean = 75, sd = 5), digits = 2)

# Cluster 2
Cluster_2_X <- round(rnorm(400, mean = 50, sd = 6), digits = 2)
Cluster_2_Y <- round(rnorm(400, mean = 20, sd = 6), digits = 2)

# Cluster 3
Cluster_3_X <- round(rnorm(400, mean = 80, sd = 6), digits = 2)
Cluster_3_Y <- round(rnorm(400, mean = 60, sd = 6), digits = 2)



x <- c(Cluster_1_X, Cluster_2_X, Cluster_3_X)
y <- c(Cluster_1_Y, Cluster_2_Y, Cluster_3_Y)


n <- 400
z <- as.character(rep(1:3, each=n))



Distinct_Clusters <- data.frame(class,x,y,z)

remove(Cluster_1_X, Cluster_2_X, Cluster_3_X, 
       Cluster_1_Y, Cluster_2_Y, Cluster_3_Y,
       x, y, n, z)


################################################################################
# Mixed clusters
################################################################################


set.seed(1)
class <- "FuzzyClusters"

# Cluster 1
Cluster_1_X <- round(rnorm(400, mean = 30, sd = 8), digits = 2)
Cluster_1_Y <- round(rnorm(400, mean = 65, sd = 8), digits = 2)
# Cluster 2
Cluster_2_X <- round(rnorm(400, mean = 45, sd = 9), digits = 2)
Cluster_2_Y <- round(rnorm(400, mean = 30, sd = 9), digits = 2)
# Cluster 3
Cluster_3_X <- round(rnorm(400, mean = 70, sd = 9), digits = 2)
Cluster_3_Y <- round(rnorm(400, mean = 60, sd = 9), digits = 2)

x <- c(Cluster_1_X, Cluster_2_X, Cluster_3_X)
y <- c(Cluster_1_Y, Cluster_2_Y, Cluster_3_Y)


n <- 400
z <- as.character(rep(1:3, each=n))

FuzzyClusters <- data.frame(class, x,y,z)

remove(class,
       Cluster_1_X, Cluster_2_X, Cluster_3_X, 
       Cluster_1_Y, Cluster_2_Y, Cluster_3_Y,
       x, y, n, z)

################################################################################
# Halos
################################################################################

set.seed(1)
class <- "Halos"

deg2rad <- function(deg) {(deg * pi) / (180)}

# Radius des Kreises
#radius <- 3 # exakt 
#radius <- round(runif(100, min = 2.8, max = 3.2), digits = 2) # uniform
set.seed(1)
r_out <- rnorm(600, mean = 40, sd = 1.5) # normalverteilt
r_in <- rnorm(600, mean = 25, sd = 1.5)  # normalverteilt

points_deg_out <- round(runif(600, min = 0, max = 360), digits = 2)
points_deg_in  <- round(runif(600, min = 0, max = 360), digits = 2)
points_rad_out <- deg2rad(points_deg_out)
points_rad_in  <- deg2rad(points_deg_in)
# 
points_x_out <- sin(points_rad_out)*r_out+50
points_y_out <- cos(points_rad_out)*r_out+50

points_x_in <- sin(points_rad_in)*r_in+50
points_y_in <- cos(points_rad_in)*r_in+50


x <- c(points_x_out, points_x_in)
y <- c(points_y_out, points_y_in)

n <- 600
z <- as.character(rep(1:2,each=n)) 

Halos <- data.frame(class, x, y, z)

remove(class,
       r_out, r_in,
       points_deg_in, points_deg_out, points_rad_in, points_rad_out,
       points_x_in, points_x_out, points_y_in, points_y_out,
       x,y, z)

################################################################################
# Split Circle
################################################################################

set.seed(1)
class <- "SplitCircle"

radius <- rnorm(800, mean = 35, sd = 1.5)

points_deg_low <- round(runif(800, min = 90, max = 270), digits = 2)
points_rad_low <- deg2rad(points_deg_low)

points_deg_up <- round(runif(800, min = 270, max = 450), digits = 2)
points_rad_up <- deg2rad(points_deg_up)
# 
x_low <- (sin(points_rad_low)*radius)+75
y_low <- (cos(points_rad_low)*radius)+60

x_up <- (sin(points_rad_up)*radius)+45
y_up <- (cos(points_rad_up)*radius)+40

x <- c(x_low, x_up)
y <- c(y_low, y_up)

n <- 800;
z <- as.character(rep(1:2, each = n));

SplitCircle <- data.frame(class, x, y, z)

remove(radius, 
       points_deg_low, points_deg_up, points_rad_low, points_rad_up,
       x_low, x_up, y_low, y_up,
       x, y, n, z)

################################################################################
# Long clusters
################################################################################

set.seed(1)
class <- "LongClusters"

# Cluster 1
Cluster_1_X <- round(rnorm(400, mean = 35, sd = 14), digits = 2)
Cluster_1_Y <- round(rnorm(400, mean = 55, sd = .5), digits = 2)
# Cluster 2
Cluster_2_X <- round(rnorm(400, mean = 60, sd = 14), digits = 2)
Cluster_2_Y <- round(rnorm(400, mean = 45, sd = .5), digits = 2)
# Cluster 3
Cluster_3_X <- round(rnorm(400, mean = 75, sd = 14), digits = 2)
Cluster_3_Y <- round(rnorm(400, mean = 35, sd = .5), digits = 2)

x <- c(Cluster_1_X, Cluster_2_X, Cluster_3_X)
y <- c(Cluster_1_Y, Cluster_2_Y, Cluster_3_Y)

n <- 400
z <- as.character(rep(1:3, each=n))

LongClusters <- data.frame(class,x,y,z)

remove(class,
       Cluster_1_X, Cluster_2_X, Cluster_3_X, 
       Cluster_1_Y, Cluster_2_Y, Cluster_3_Y,
       x, y, n, z)

################################################################################
# No Clusters
################################################################################

set.seed(1)
class <- "NoClusters"

x <- round((runif(1200, min=0, max=100)),digits = 2)
y <- round((runif(1200, min=0, max=100)),digits = 2)

z <- 1

NoClusters = data.frame(class,x,y,z)

remove(class,x,y,z)

################################################################################
################################################################################

# Put everything together

AllClusters <- rbind(Distinct_Clusters,
                     FuzzyClusters,
                     Halos,
                     LongClusters,
                     NoClusters,
                     SplitCircle)