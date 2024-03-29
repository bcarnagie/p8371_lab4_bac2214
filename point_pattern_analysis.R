 #### Spatial Point Pattern Analysis ####

# Step 1: Install packages
install.packages("maptools")
install.packages("spatstat")
install.packages("sf")

# Step 2: Load packages
library(maptools)
library(spatstat)
library(sf)
library(ggplot2)



# Step 3: Set working directory 
setwd("~/Library/CloudStorage/OneDrive-Personal/PH GIS/Labs/Lab4")


# Read in soho shp
soho <- st_read("soho.shp")
plot(soho["area"])


# Set as owin - observation window 
soho_window <- as.owin(soho)

# Read in cholera deaths 
deaths <- st_read("cholera_death_cases.shp")


# Set death as a point pattern process
deaths_ppp <- as.ppp(deaths)


# Combine Spatial Points with Observation Window 

death_soho <- ppp(deaths_ppp$x,
                  deaths_ppp$y,
                  window = soho_window)
plot(death_soho)

# Create heatmap of TRI sites
plot(density(death_soho),
     main= "Kernel density map of cholera deaths")

# Change the buffer to 50 meters
plot(density(death_soho, sigma = 50),
     main= "Kernel density map of cholera deaths")


# Quadrat Test
qt <- quadrat.test(death_soho, nx=6, ny=10)
qt
plot(qt, 
     cex=.7,
     main = "Quadrat Test")


###Run L function with 10 simulations and 95% confidence interval (rank=2) on all points (global=T)###
L <- envelope(death_soho, 
              Lest, 
              nsim = 20,
              rank=2, 
              global=T)


###From the results we can see that at very small distances between points (x-axis) there is an 
###immediate spike in density (y-axis)

L_plot = plot(L, 
     main = "Global Clustering Results", 
     ylab = "Density Function",
     xlab = "Distance") |> ggsave("ripleysK.png")



library(ggplot2)

# Create a base R plot (replace this with your actual plot code)
L <- ...  # Your data and plot code here

# Convert the base R plot to a ggplot2 object
L_plot_gg <- as_ggplot(L)

# Customize the ggplot2 plot (add titles, labels, etc.)
L_plot_gg <- L_plot_gg +
  labs(
    title = "Global Clustering Results",
    y = "Density Function",
    x = "Distance"
  )

# Save the ggplot2 plot as an image file using ggsave
ggsave("ripleysK.png", plot = L_plot_gg)













