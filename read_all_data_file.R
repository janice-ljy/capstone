#####################################
# 
# 1. read .csv data into R
# 2. function: printoutVariables(keyWords)
# 3. function: printoutDatasets(filename)
# 
# 2.13
# 
# https://aosmith.rbind.io/2017/12/31/many-datasets/
#
#####################################

library(here)
library(stringr)
library(purrr)
getwd()
setwd('/Users/jiayingliang/Desktop/data4961/data/MO_HEALTH_Covid_Tracking/data')
# 1. getting a list of files to read
allfiles = list.files(path = here('data','MO_HEALTH_Covid_Tracking','data'),
                        pattern = '.csv',
                        full.names = TRUE,
                        recursive = TRUE)

# 2. figure out the steps needed to read and manipulate a single file
data_dictionary = read.csv(allfiles[4])

allfiles = allfiles[-4]

allnames = str_split(allfiles, pattern = '/')

newnames = vector(mode='list', length = 29)

for (i in 1:29) 
{
  newnames[[i]][1] = allnames[[i]][9]
  newnames[[i]][2] = allnames[[i]][10]
} 

newnames
data_dictionary

# print out variable name with specific key words

# list of variables for searching key words
search_list <- data_dictionary$variable_name
var_meaning <- data_dictionary$explanation
n <- length(data_dictionary$variable_name)

# example key words
keyWords = 'avg'

printoutVariables <- function(keyWords){
  keyWords_ind <- str_which(search_list, keyWords)
  
  for (i in keyWords_ind){
    cat(search_list[i],': ',var_meaning[i],'\n')
  }
}

#printoutVariables(keyWords)

# list of data sets with key words
n_data <- length(newnames)

# example file name
filename = 'zip'

printoutDatasets <- function(filename){
  lis <- vector()
  for (i in 1:n_data){
    if (newnames[[i]][1] == filename){
      lis <- c(lis, newnames[[i]][2])
      #cat(newnames[[i]][2], '\n')
    }
  }
  return(lis)
}


#dt <- read.csv(paste('county/',county[1],sep = ''))
#dt

#printoutDatasets(filename)

#str(colnames(dt))
#str(unlist(colnames(dt), recursive=F))

#x <- paste('/Users/jiayingliang/Desktop/data4961/data/MO_HEALTH_Covid_Tracking/data/', 
#      'county',
#      '/', 
#      #eval(parse(text = paste0(input$datafolder, "[1]"))),
#      'county_full.csv',
#      sep = ''
#      )

#y <- read.csv(x)
#colnames(y)
