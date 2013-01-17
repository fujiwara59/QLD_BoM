# Example preprocessing script.

## Download the bom data
# Specify is the url of the data file
data_url <- 'http://www.bom.gov.au/clim_data/cdio/tables/text/IDCJCM0036_040209.csv'
# sfuj: You can append this to an url prevent redirection -> &ndplr=1
# Download the data and loading it into R memory
bom.climate <- read.csv(data_url, skip = 10)
# Take a quick look at the data
View(bom.climate)

# sfuj: my shortlist of items to grep
mean.max.t <- "Mean maximum temperature" #(Degrees C)

target.var.list <- list("Mean maximum temperature",
                        "Mean 9am temperature",
                        "Mean 3pm temperature",
                        "Mean minimum temperature",
                        "Decile 1 monthly rainfall",
                        "Decile 5",
                        "Decile 9 monthly rainfall")


## locating a given variable in the data table
# locating the row number of one variable
target.row <- grep(pattern = mean.max,
                   x = bom.climate[ , 1]
                   )
# locating the row number of multiple variables. 
target.var.rownums<-list()
for (target.var in target.var.list){
  target.var.rownums[[target.var]] <- grep(pattern = target.var,
                     x = bom.climate[ , 1]
                                           )
}

# dropping the metadata and summary columns
bom.climate.subset <- bom.climate[ , 1:13]
head(bom.climate.subset)
dim(bom.climate.subset)

# subsetting one target variable
single.var <- bom.climate.subset[target.row, ]
target.var.name <- single.var[1]
single.var <- single.var[2:13]
single.var.t <- t(single.var)
single.var.t <-data.frame("Month_alpha" = row.names(single.var.t), "Value" = as.numeric(single.var.t))
single.var.t$Variable <- unlist(target.var.name)


#plotting a single variable
single.var.t$Month <- 1:12
ggplot(data=single.var.t, aes(x=Month, y = Value)) + 
  geom_path() + geom_point() +
  scale_x_discrete('Month') +
  scale_y_continuous(unlist(target.var.name))

# subsetting multiple target variables
target.rows <- as.numeric(target.var.rownums)
bom.climate.subset.2 <-bom.climate.subset[target.rows, ] 

# stacking the data
# bom.climate.molten <- melt(bom.climate.subset, id = 'Statistic.Element')
bom.climate.molten <- melt(bom.climate.subset.2, id = 'Statistic.Element')
colnames(bom.climate.molten)[2] <- 'Month'
colnames(bom.climate.molten)[3] <- 'Value' 
bom.climate.molten[,'Value']<- as.numeric(bom.climate.molten[,'Value'])

## plotting stacked multivariable data table

# ggplot can't plot factorial months with paths or lines, so start w points
class(bom.climate.molten$variable)
ggplot(data=bom.climate.molten, aes(x=Month, y = Value, colour = Statistic.Element)) + 
  geom_point()

# convert month to numeric, then can use lines
bom.climate.molten$Month <- unclass(bom.climate.molten$Month)
ggplot(data=bom.climate.molten, aes(x=Month, y = Value, colour = Statistic.Element)) + 
  geom_line()

## plotting stacked multivariate data table, dividing temp and rainfall into two facets

climate.tall <- bom.climate.molten
climate.tall$TempQ <- grepl(pattern='temperature', x=climate.tall[ ,1])
climate.tall$Category <- grepl(pattern='temperature', x=climate.tall[ ,1])
#climate.tall$Category <- as.factor(climate.tall$Category)
#levels(climate.tall$Category) <- list(TRUE = "Temperature", FALSE = "Rainfall")


ggplot(data=climate.tall, aes(x=Month, y = Value, colour = Statistic.Element)) + 
  geom_line() + facet_grid(facets='TempQ~.', scales='free_y', )

## plotting stacked multivariate data table, dividing temp and rainfal with gridExtra
# subsetting rainfall measurements
climate.tall.rf <- subset(x=climate.tall, subset= TempQ == TRUE)
# plotting rainfall measurements
ggplot(data=climate.tall.rf, aes(x=as.factor(Month), y=Value, colour = Statistic.Element)) + geom_bar()
ggplot(data=climate.tall.rf, aes(x=Month, y=Value, colour = Statistic.Element)) + geom_point() + theme_bw()
