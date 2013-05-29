# GRAB SILO DATA FOR INTRO PLOTS
file.copy(from='~/Dropbox/phd/Tomago/AOG/tomago.csv',
          to='data/tomago.csv',
          overwrite=FALSE)

file.copy(from='~/Dropbox/phd/Tomago/AOG/tomago.headers.csv',
          to='data/tomago.headers.csv',
          overwrite=FALSE)

# fix 'Date' column
tomago[,'Date'] <- as.Date(as.character(tomago[,1]), format = "%Y%m%d")

# drop non-numeric columns
factor.cols <- grep('factor', sapply(tomago,class))
tomago.xts <- tomago[,-c(factor.cols)]
tomago.xts <- sapply(tomago.xts, as.numeric)

# 
tomago.xts <- xts(tomago.xts, order.by= tomago[,'Date'])
summary(tomago.xts)

# timeseries plot
ggplot(tomago.xts, aes(index(tomago.xts), VPD )) + geom_line()
plot.zoo(tomago.xts[,'VPD'])

# monthly plots
monthplot(tomago.xts[,'VPD'])

plot.v <- ggplot(tomago.xts, aes(mm, VPD)) + 
  geom_bar(position=dodge, stat='identity')+
  theme_bw()+
  scale_y_continuous('VPD (kPa)') +
  scale_x_discrete('Month', limits=c(1:12),
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
#define errror bars
#limits <- aes(ymax = resp + se, ymin=resp - se)

