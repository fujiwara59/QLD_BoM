library('ProjectTemplate')
setwd('BoM_Monthly/')

load.project()

for (dataset in project.info$data)
{
  message(paste('Showing top 5 rows of', dataset))
  print(head(get(dataset)))
}

