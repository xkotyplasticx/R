# Load the data
my_data <- read.csv("C:\\Users\\kamif\\Documents\\EC_UTBILDNING\\R\\2404152328_Cars.csv", header = TRUE, sep = ",")

head(my_data)

# Remove columns 1 and from 11
my_data1 <- my_data[,1:10]
colnames(my_data1)[10] <- "Pris"

# Get the number of empty cells in each column and print the result
empty_cells <- sapply(my_data1, function(x) sum(is.na(x)))
print(empty_cells)

# Clean everything from spaces
my_data_ws <- apply(my_data1, 2, function(x) gsub(" ", "", x))

# Delete [] brackets and any other symbols
my_data_ws1 <- apply(my_data_ws, 2, function(x) gsub("[[:punct:]]", "", x))

# Convert all characters in the columns 3, 6 - 9 to lowercase
my_data_ws1[,c(3,6:9)] <- sapply(my_data_ws1[,c(3,6:9)], function(x) tolower(x))

# Convert specific columns to integer
my_data_ws1[, c("Miltal", "Pris")] <- sapply(my_data_ws1[, c("Miltal", "Pris")], as.integer)

# Convert matrix back to dataframe
my_data_clean <- as.data.frame(my_data_ws1)

# Remove rows with empty cells (NA values)
my_data_clean <- my_data_clean[complete.cases(my_data_clean), ]

print(class(my_data_clean$Pris))

# Convert my_data_clean$Pris to numeric
my_data_clean$Pris <- as.integer(my_data_clean$Pris)


# Get the number of empty cells in each column and print the result
empty_cells <- colSums(is.na(my_data_clean))
print(empty_cells)

#change mistakes in data
my_data_clean$Modellår[my_data_clean$Modellår == "214"] <- "2014"
my_data_clean$Län[my_data_clean$Län == "stcokholm"] <- "stockholm"


#Change some cities to län
my_data_clean$Län[my_data_clean$Län == "borås"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "göteborg"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "falun"] <- "dalarna"
my_data_clean$Län[my_data_clean$Län == "hörby"] <- "skåne"
my_data_clean$Län[my_data_clean$Län == "lund"] <- "skåne"
my_data_clean$Län[my_data_clean$Län == "malmö"] <- "skåne"
my_data_clean$Län[my_data_clean$Län == "järfälla"] <- "stockholm"
my_data_clean$Län[my_data_clean$Län == "karlskrona"] <- "blekinge"
my_data_clean$Län[my_data_clean$Län == "karlstad"] <- "värmland"
my_data_clean$Län[my_data_clean$Län == "knivsta"] <- "uppsala"
my_data_clean$Län[my_data_clean$Län == "kristianstad"] <- "skåne"
my_data_clean$Län[my_data_clean$Län == "kungälv"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "lidköping"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "linköping"] <- "östergötland"
my_data_clean$Län[my_data_clean$Län == "llinköping"] <- "östergötland"
my_data_clean$Län[my_data_clean$Län == "nässjö"] <- "jönköping"
my_data_clean$Län[my_data_clean$Län == "norrköping"] <- "östergötland"
my_data_clean$Län[my_data_clean$Län == "nyköping"] <- "södermanland"
my_data_clean$Län[my_data_clean$Län == "örnsköldsvik"] <- "västernorrland"
my_data_clean$Län[my_data_clean$Län == "österåker"] <- "stockholm"
my_data_clean$Län[my_data_clean$Län == "skövde"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "tranemo"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "trollhättan"] <- "västragötaland"
my_data_clean$Län[my_data_clean$Län == "värnamo"] <- "jönköping"
my_data_clean$Län[my_data_clean$Län == "västerås"] <- "västmanland"
my_data_clean$Län[my_data_clean$Län == "växjö"] <- "kronoberg"
my_data_clean$Län[my_data_clean$Län == "vetlanda"] <- "jönköping"
my_data_clean$Län[my_data_clean$Län == "vimmerby"] <- "kalmar"


# Print the modified dataframe
print(my_data_clean)

# Boxplot of price range for electric cars
library(scales)
boxplot(my_data_clean$Pris, xlab = "", 
        ylab = "Pris (K)",  # Set y-axis label
        main = "Boxplot of Pris",
        ylim = c(0, max(my_data_clean$Pris) * 1.1),  # Extend the y-axis range
        axes = FALSE)  # Turn off automatic axis generation
axis(side = 1)  # Add x-axis
axis(side = 2, 
     at = pretty(my_data_clean$Pris), 
     labels = format(pretty(my_data_clean$Pris) / 1000, big.mark = " ", scientific = FALSE))  # Add y-axis with labels in "K" format
box()  # Add box around the plot

# Show the unique values in the column 1 of feed_characters with its frequency
print(table(my_data_clean[,1])) # Miltal
print(table(my_data_clean[,2])) # Modellår
print(table(my_data_clean[,3])) # Biltyp
print(table(my_data_clean[,4])) # Drivning
print(table(my_data_clean[,5])) # Hästkrafter
print(table(my_data_clean[,6])) # Färg
print(table(my_data_clean[,7])) # Märke
print(table(my_data_clean[,8])) # Modell
print(table(my_data_clean[,9])) # Län
print(table(my_data_clean[,10])) # Pris

#change mistakes in data
my_data_clean$Modellår[my_data_clean$Modellår == "214"] <- "2014"



# Print the modified dataframe
print(my_data_clean)

# Save the data into a new file called "carclean.csv"
write.csv(my_data_clean, file = "carclean.csv")


