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
climate.tall$RainfallQuery <- grepl(pattern='rainfall', x=climate.tall[ ,1])
climate.tall$TempQuery <- grepl(pattern='temperature', x=climate.tall[ ,1])


ggplot(data=climate.tall, aes(x=Month, y = Value, colour = Statistic.Element)) + 
  geom_line() + facet_grid(facets='TempQ~.', scales='free_y', )

## plotting stacked multivariate data table, dividing temp and rainfal with gridExtra
# subsetting temperature 
climate.tall.temp <- subset(x=climate.tall, subset = TempQuery == TRUE)
# subsetting rainfall
climate.tall.rf <- subset(x=climate.tall, subset = RainfallQuery == TRUE)
# plotting temperature 
ggplot(data=climate.tall.temp, aes(x=as.factor(Month), y=Value, colour = Statistic.Element)) + geom_bar()
ggplot(data=climate.tall.temp, aes(x=Month, y=Value, colour = Statistic.Element)) + geom_line() + theme_bw()
plot.df <- climate.tall.rf
ggplot( data = plot.df, aes(x=as.factor(Month),
                            y = Value, 
                            fill = Statistic.Element)) + 
  geom_bar(position='dodge') + theme_bw()

subset(x=climate.tall.rf, subset= Statistic.Element=="Decile 1 monthly rainfall (mm) for years 1997 to 2012")

reshape(data=climate.tall.rf, drop=c('RainfallQuery'), direction='wide', )

DF <- climate.tall.rf
DF$RainfallQuery <- NULL
DF$TempQuery <- NULL
DF <- reshape(DF, timevar= "Month", idvar = "Statistic.Element", direction = 'wide')
DF <- (t(DF))
DF <- DF[, c(2,1,3)]
colnames(DF) <- DF[1, ]
DF <- DF[2:nrow(DF), ]

DF <- data.frame("Decile_5" = as.numeric(DF[ ,1]),
                 "Decile_1" = as.numeric(DF[ ,2]),
                 "Decile_9" = as.numeric(DF[ ,3]),
                 "Month" = 1:12)
limits <- aes(ymax = Decile_9,
              ymin = Decile_1)
#limits <- aes(ymax = resp + se, ymin=resp - se)

ggplot(DF, aes(y = Decile_5, x = Month)) + geom_point()
p <- ggplot(DF, aes(y = Decile_5, x = as.factor(Month))) 
p + geom_bar(position='dodge')

# Because the bars and errorbars have different widths
# we need to specify how wide the objects we are dodging are
dodge <- position_dodge(width=0.9)

p + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25) +
  theme_bw() + 
  scale_y_continuous('Monthly rainfall (mm)') +
 scale_x_discrete('Month', 
                  labels = c('1' = 'Jan',
                             '2' = 'Feb', 
                             '3' = 'Mar', 
                             '4' = 'Apr',
                             '5' = 'May',
                             '6' = 'Jun',
                             '7' = 'Jul',
                             '8' = 'Aug',
                             '9' = 'Sep',
                             '10'= 'Oct',
                             '11'= 'Nov',
                             '12'= 'Dec'))

