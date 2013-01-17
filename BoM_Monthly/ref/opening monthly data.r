# file to load and plot Monthly rainfall data
# can include code to remove nas

# set working directory
my_wd <-  "C:/Documents and Settings/11154895/My Documents/Dropbox/phd/kangaloon shared/rainfall data kangaloon/monthly data"
#normalizePath(my_wd)
setwd(my_wd)
getwd()

  # check what is in this working directory
  shell("dir", intern=TRUE)

# importing data in 2 steps
 # 1
data.file.name <-   "IDCJAC0001_068243_Data1.csv"
 # 2
bom.rainfall <- read.csv(
                  #paste(
                  #       getwd(),
                  #       "/2 extracted data/",
                          data.file.name,
                  sep = ","
                  #)
                )


##############################
###  correcting column names #
##############################

cols <- c(
          'product code',
          'BOM station number',
          'Year',
          'Month',
          #'Rainfall amount (mm)',
          'Rainfall',
          'Quality'
          )

names(bom.rainfall)
names(bom.rainfall) <- cols

head(bom.rainfall)


#####################################
#create column of dates. in 2 steps #
#####################################
  # note: we need to do this to do a timeseries plot.
  
  #step_1 Creating column of Months with fixed width. (ie month 1 goes to month 01)
   # making sure this column is blank.
   bom.rainfall$Month_XX <- NULL
     # option 1 using ifelse
     for (i in 1:length(bom.rainfall$Month)){
       ifelse(nchar(bom.rainfall$Month[i])==1,
         bom.rainfall$Month_XX[i] <- paste("0",bom.rainfall$Month[i], sep=""),
         bom.rainfall$Month_XX[i] <- bom.rainfall$Month[i]
         )
       }
     # option 2 using if else
        #for (i in 1:length(bom.rainfall$Month)){
        # if
        # (nchar(bom.rainfall$Month[[i]])==1){
        #   bom.rainfall$Month_XX[[i]] <- paste("0",bom.rainfall$Month[[i]], sep="")
        #   }
        # else   {
        # bom.rainfall$Month_XX[[i]] <- bom.rainfall$Month[[i]]
        #  }
        # }

  #step_2 joining the two columns Year and Month_XX
    bom.rainfall$Date <- do.call(paste, c(bom.rainfall[c('Year', 'Month_XX')], sep="-"))


  # step_3_optional
  for (i in 1:length(bom.rainfall$Month)){
       bom.rainfall$Day_XX[i] <- "01"
       }
  bom.rainfall$Date_dummy <- do.call(paste, c(bom.rainfall[c('Year', 'Month_XX', 'Day_XX')], sep="-"))

  ######################
  ## saving the data frame
  ########################
  
  #what do you want to append to the original data file name?
  " sfuj_r_data" -> append_this

  write.csv(bom.rainfall, file = paste(substr(
                                         data.file.name,
                                         1,
                                         nchar(data.file.name)-4),
                                      append_this,
                                      ".csv",
                                      sep="")
            )


  #to read this file in again use
  read.csv(
            paste(substr(
                     data.file.name,
                     1,
                     nchar(data.file.name)-4),
                  append_this,
                  ".csv",
                  sep=""),
            row.names = 1)


##########
##plot with ggplot
##############
library(ggplot2)

    # histogram
    c <- ggplot(data=bom.rainfall, aes(Rainfall))
    c + geom_bar()

#these two are the same
qq <- qplot(Date, Rainfall, data=bom.rainfall, geom="bar", fill=factor(Month))
gg <- ggplot(bom.rainfall, aes(Date, Rainfall)) + geom_bar(aes(fill=factor(Month)))

gg + scale_x_discrete("Month")

gg2 <- ggplot(bom.rainfall, aes(Date_dummy, Rainfall)) + geom_bar(aes(fill=factor(Month)))
gg2
gg2 + scale_x_date(
limits = as.Date(c("2004-01-01", "2005-01-01")),
format = "%Y-%m-%d"  # half works
)

gg2 + scale_x_date(major= "1 years") # doesnt work

gg3 <- ggplot(bom.rainfall, aes(as.Date(Date_dummy), Rainfall))
gg3 + geom_line()
gg3 + scale_x_date(
limits = as.Date(c("2004-01-01", "2005-01-01")),
format = "%Y-%m-%d"  # half works
)

# years stacked
gg4 <- ggplot(bom.rainfall, aes(Year, Rainfall))
gg4 + geom_bar(aes(fill=factor(Month_XX)), stat="identity")

# using Date_dummy seems to work!! but only if we use as.Date, otherwise same using Date column.
gg5 <- ggplot(bom.rainfall, aes(as.Date(Date_dummy), Rainfall))
gg5 + geom_bar(aes(fill=factor(Month_XX)), stat="identity")

  # what happens if I apply as.Date to the Date column. DOESNT WORK.
    #gg5.1 <- ggplot(bom.rainfall, aes(as.Date(Date, format="%Y-%m"), Rainfall))
    #gg5.1 + geom_bar(aes(fill=factor(Month_XX)), stat="identity")

  # what happens if I drop the stat=identity? DOESNT WORK.
    #gg5.2 <- ggplot(bom.rainfall, aes(as.Date(Date_dummy), Rainfall))
    #gg5.2 + geom_bar(aes(fill=factor(Month_XX)))
    
  #I think it might look better without the leading zeroes for the month
    gg5.3 <- ggplot(bom.rainfall, aes(as.Date(Date_dummy), Rainfall))
    gg5.3 + geom_bar(aes(fill=factor(Month)), stat="identity")+
    ylab("Monthly rainfall, summed(mm)")+
    xlab("Year")+
    opts(title ="Rainfall at Burrawang weather station November 2000 to March 2011")

   # now lets examine those missing months
   
   subset(bom.rainfall, Year==2001)
     # indeed it looks like there is some months missing here.
     # confirmed, they are missing, checked the original .csv files


#boxplot
# http://www.r-bloggers.com/summarising-data-using-box-and-whisker-plots/
ggplot(bom.rainfall, aes(Month_XX, Rainfall)) + geom_boxplot()+
ylab("Summmed monthly rainfall (mm)")+
opts(title ="Rainfall at Burrawang weather station November 2000 to March 2011")



qplot(as.Date(Date_dummy), Rainfall, data=bom.rainfall)

head(
as.Date(bom.rainfall$Date,
format = "%Y-%m" )
)

gg + scale_x_date(
limits = as.Date(c("2002-01-01", "2004-01-01")),
format = "%Y-%m"
)

head(bom.rainfall$Date)

#ggplot(bom.rainfall, aes(Date, Rainfall)) + geom_line() +
#  scale_x_date(format = "%Y-%b") + xlab("") + ylab("Rainfall (mm)")

##########################
##  using time series
##########################
 # defining time series
 bom.rainfall.timeseries <- ts(bom.rainfall$Rainfall, frequency = 12, start=c(2000, 11))


 #checking
       length(bom.rainfall.timeseries)
       length(bom.rainfall$Rainfall)
      #these two are the same

        tail(bom.rainfall)
        bom.rainfall.timeseries
      #discrepancy here - 2011?

 #plotting
 plot(bom.rainfall.timeseries)

 