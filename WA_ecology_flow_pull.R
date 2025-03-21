# Pull flow data off the WA Ecology Website
# Kevin Swager 

# Packages needed
library(plotly)
library(ggplot2)

# pull tarboo data from the ecology website
# example you can find your station url navigating at
# https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationDetails?sta=17D060
# the data you are pulling is the 15 minute table located down on the flow page
# your url is found by clicking the 15 minute data table

# URL will be unique for each site, need to find the url for each.
url<- "https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/17G060/17G060_DSG_FM.TXT"

# read the data in, they give it as a text file
data <- read.table(url, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Rename the one column so we can work with it. 
colnames(data)<-c("Names")

# Pulls the text apart from the data so you don't have the handle any non data rows 
data <- data[grepl("^\\d{2}/\\d{2}/\\d{4}", data$Names), , drop = FALSE]

# Split the column by spaces
split_data <- strsplit(as.character(data$Names), "\\s+")

# find the max number of columns
max_cols <- max(sapply(split_data, length))

# Pad shorter rows with NA so column split correctly
padded_data <- lapply(split_data, function(x) {
  length(x) <- max_cols
  return(x)
})

# Convert the list to a data frame
data <- do.call(rbind, lapply(padded_data, function(x) data.frame(matrix(x, ncol = max_cols, byrow = TRUE))))

# Removes 
data<- data[complete.cases(data),]

# Set column names into something usable
colnames(data) <- c("Date", "Time", "CFS", "Quality")

# Convert the date/time for plotting 
data$Datetime <- as.Date(paste(data$Date, data$Time), format = "%m/%d/%Y %H:%M")

# cut down the number of rows
data<-data[,c(5,3)]

# Cut down the number of records, I only want to display the last 30 days
data<-filter(data,data$Datetime>= (Sys.Date()-30))

# Turn the CFS column to numeric 
data$CFS<-as.numeric(data$CFS)

# Completed data set (this step is used for RShiny)
data<-data

# Graph the data with plotly for interactivity
ggplotly(ggplot(data, aes(x=Datetime, y=CFS)) +
           geom_line(color="blue ") +
           xlab("Time"))

# and finished!
