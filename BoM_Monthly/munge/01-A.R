
# Changelog
# 2013-01-24
#  Added EPS output
#  Removed lower margin from top plot, and upper margin from bottom plot.
#  Added brackets around (mm) and (deg C) in axis labels

# user specification block
spec.fixed.y <- FALSE

## Download the bom data
# Specify is the url of the data file
data_url <- list()
print('North Stradbroke Island QLD is station 1')
data_url[[1]] <- 'http://www.bom.gov.au/clim_data/cdio/tables/text/IDCJCM0036_040209.csv'
print('East Kangaloon NSW is station 2')
data_url[[2]] <- 'http://www.bom.gov.au/clim_data/cdio/tables/text/IDCJCM0037_068239.csv'
print('Beerburrum Forest Station QLD is station 3')
data_url[[3]] <- 'http://www.bom.gov.au/clim_data/cdio/tables/text/IDCJCM0036_040284.csv'
print('Williamtown Raaf (Tomago) NSW is station 4')
data_url[[4]] <- 'http://www.bom.gov.au/clim_data/cdio/tables/text/IDCJCM0037_061078.csv'

target.station <- readline(prompt = "Please enter a station number from those above")
target.station <- as.numeric(target.station)
# sfuj: You can append this to an url prevent redirection -> &ndplr=1


# Download the data and loading it into R memory
bom.climate <- read.csv(data_url[[target.station]], skip = 10)
# Take a quick look at the data
View(bom.climate)

# Grab the station identification
bom.climate.meta <- read.csv(data_url[[target.station]])
station_id <- bom.climate.meta[2,1]

print(station_id)

# sfuj: variables to locate within the dataframe, using grep
target.var.list <- list("Mean maximum temperature",
                        "Mean 9am temperature",
                        "Mean 3pm temperature",
                        "Mean minimum temperature",
                        "Decile 1 monthly rainfall",
                        "Decile 5",
                        "Decile 9 monthly rainfall")


## locating a given variable in the data table
# # locating the row number of one variable
# target.row <- grep(pattern = mean.max,
#                    x = bom.climate[ , 1]
#                    )
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

# # subsetting one target variable
# single.var <- bom.climate.subset[target.row, ]
# target.var.name <- single.var[1]
# single.var <- single.var[2:13]
# single.var.t <- t(single.var)
# single.var.t <-data.frame("Month_alpha" = row.names(single.var.t), "Value" = as.numeric(single.var.t))
# single.var.t$Variable <- unlist(target.var.name)
# 
# 
# #plotting a single variable
# single.var.t$Month <- 1:12
# ggplot(data=single.var.t, aes(x=Month, y = Value)) + 
#   geom_path() + geom_point() +
#   scale_x_discrete('Month') +
#   scale_y_continuous(unlist(target.var.name))

# subsetting multiple target variables
target.rows <- as.numeric(target.var.rownums)
bom.climate.subset.2 <-bom.climate.subset[target.rows, ] 

# cropping the variable names
bom.climate.subset.2[ ,1] <- sub(x=bom.climate.subset.2[ ,1], pattern=" [[:punct:]][[:print:]]{2,9}[[:punct:]] for years [[:digit:]]* to [[:digit:]]* ", replacement="")

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

## cropping out the words temperature and rainfall from the variable labels
## we need to do this to get the legends to fit
# cropping the variable names
climate.tall[ ,1] <- sub(x=climate.tall[ ,1], pattern=" rainfall", replacement="")
climate.tall[ ,1] <- sub(x=climate.tall[ ,1], pattern=" temperature", replacement="")

# map facet labels from codes with a labeller function
rt_labeller <- function(var, value){
  if (var == "TempQuery") {
    value[value == "TRUE"] <- "Temperature"
    value[value == "FALSE"] <- "Rainfall"
  }
  return(value)
}

ggplot(data=climate.tall, aes(x=Month, y = Value, colour = Statistic.Element)) + 
   geom_line() + 
  facet_grid(facets='TempQuery~.', scales='free_y', labeller=rt_labeller) +
  theme_bw()

### plotting stacked multivariate data table, dividing temp and rainfal with gridExtra

## subsetting
# subsetting temperature 
climate.tall.temp <- subset(x=climate.tall, subset = TempQuery == TRUE)
# subsetting rainfall
climate.tall.rf <- subset(x=climate.tall, subset = RainfallQuery == TRUE)

## plotting temperature 

climate.tall.temp$Month <- as.numeric(climate.tall.temp$Month)
ggplot(data=climate.tall.temp, aes(x=as.factor(Month), y=Value, colour = Statistic.Element)) + geom_bar()

plot.df <- climate.tall.temp
names(plot.df)[[1]] <- "Variable"
plot.o <- ggplot(data=plot.df, aes(x=Month, y=Value, lty = Variable)) + geom_line()

if (spec.fixed.y == TRUE) {
  plot.o <- plot.o + scale_y_continuous(expression(paste("Temperature ("* degree, "C)")),
    limits = c(-2, 35))
} else {
  plot.o <- plot.o + scale_y_continuous(expression(paste("Temperature ("* degree, "C)")))  
}
plot.o

plot.o <- plot.o + scale_x_discrete('Month', 
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
plot.o <- plot.o + theme_bw() + theme(legend.position = "bottom") +
  theme(plot.margin = unit(c(0,1,1,1), "mm")) #top, right, bottom, and left
plot.o


# plot.df <- climate.tall.temp
# ggplot( data = plot.df, aes(x=as.factor(Month),
#                             y = Value, 
#                             fill = Statistic.Element)) + 
#   geom_bar(position='dodge') + theme_bw()

## PLOTTING RAINFALL

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

# in the following plot
# errors bars show decile 9 (top end) and decile 1 (low end)
plot.p <- p + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25) +
  theme_bw() + 
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
if (spec.fixed.y == TRUE) {
  plot.p <- plot.p + scale_y_continuous('Rainfall (mm)', limits = c(0, 450))  
  } else {
    plot.p <- plot.p + scale_y_continuous('Rainfall (mm)')
  }

  
  
# same plot without axis label or tick mark labels. 
(plot.p <- p + geom_bar(position=dodge) + geom_errorbar(limits, position=dodge, width=0.25) +
  theme_bw() + 
  scale_y_continuous('Rainfall (mm)') +
  scale_x_discrete('', labels = NULL) +
   theme(plot.margin = unit(c(1,1,0,1), "mm"))) #top, right, bottom, and left)


grid.arrange(plot.p, plot.o)


## move to source

wd.backup <- getwd()
setwd("./BoM_Monthly/")
setwd("./graphs/")

fn <- paste(Sys.Date(), station_id, "temp and rainfall plot w gridextra")
spec.res <- 300
spec.res <- 90

W = 1680
H = 1050

pdf(file = paste(fn, "defaults", ".pdf"))
 grid.arrange(plot.p, plot.o)
dev.off()


png(filename = paste(fn, "defaults", ".png"))
grid.arrange(plot.p, plot.o)
dev.off()

png(filename = paste(fn, spec.res, ".png"), res= spec.res, width=14, height = 10, units = 'cm')
grid.arrange(plot.p, plot.o)
dev.off()

source("http://gridextra.googlecode.com/svn/trunk/R/arrange.r")
g1 = arrangeGrob(plot.p, plot.o)


setEPS()
postscript(file = paste(fn, ".nogaps.eps", sep = ""))
grid.arrange(plot.p, plot.o)
dev.off()

# TODO change legend location in plot.o DONE