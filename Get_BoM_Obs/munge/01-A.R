# Identify monthly climate observations (from BoM) in memory and save them to
#  a dataframe suitable for input to bootRes

objNames <- ls(pattern="IDCJAC")
if (length(objNames) == 0) 
  stop('Error: no climate data objects found in memory')
  print(paste(length(objNames), 'climate data objects found in memory'))

objList <- lapply(objNames, get)
names(objList) <- objNames
if (attr(summary(objList), 'dim')[[1]] != length(objNames))
  stop ('Error: not all climate data objects were gathered correctly')

for (obj in names(objList)) {
  objList[[obj]] <- my.xts(objList[[obj]])
  print(paste(obj, 'converted to xts using the helper function my.xts.'))
}

if ( class(objList[[1]])[1] != 'xts' ) 
  stop ('Error: Conversion of objs in objList to xts failed. This is a requirement for merging.')
clim <- do.call(merge, objList) # only works for xts objects
print(paste(length(objList),'climate xts objects merged successfully.'))

clim <- na.contiguous(clim) # crop the dataframe to remove all na vals


tom.clim <- as.data.frame(clim) # change from xts back to normal dataframe
date1 <- as.yearmon(x=rownames(tom.clim), format = '%b %Y')
tom.clim$year <- format(date1, "%Y")
tom.clim$month <- format(date1, "%m") # bootRes examples use lowercase colnames
rm(date1)

tom.clim <- RecodeColnames(tom.clim) # change BoM filenames to colnames to suit bootRes

tom.clim <- tom.clim[c("year", "month", "tempMax", "tempMin", 'prec')]
print('First 6 lines of output climate dataframe looks like:')
print(head(tom.clim))

## saving to cache/
# csv
write.csv(tom.clim, file = 'cache/tom.clim.csv', row.names = FALSE)
# sqlite
tc <- tom.clim
if (require(RSQLite) == FALSE) stop ('Error: The package RSQLite could not be loaded.')
con <- dbConnect(SQLite(), dbname = "cache/tom.clim.db")
dbWriteTable(con, "tc", tc, row.names = FALSE)
dbDisconnect(con)
rm(tc)