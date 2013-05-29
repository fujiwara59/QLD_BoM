# save plots

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

g2 <- arrangeGrob(plot.o, plot.p, plot.v)

my.png.large('tomago.covariates.seasonality', height = 20)
g2
dev.off()