# Install the pxweb package if you haven't already
install.packages("pxweb")

# Load the pxweb package
library(pxweb)

# Load the data using pxweb_interactive
#data <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/FordonTrafik")

#data <- read.csv("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/FordonTrafik", fill = TRUE)
data <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarA")

head(data)
1
