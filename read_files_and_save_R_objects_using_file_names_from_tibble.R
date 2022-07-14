install.packages("xlsx")
library("xlsx")

# data <- list(iris, cars, faithful) # using built in r data sets
# names <- list("iris.csv", "cars.csv", "faithful.csv")
# 
# # write the files, so we can show how to read them in
# mapply(write.csv, data, names) # save the files as csvs
# 
# # read the files into R and create a single list of data sets
# new_dataA <- lapply(names, read.csv)
# 
# # save as rds - one list object with all three datasets
# saveRDS(new_dataA) 
# 
# # save as three separate rds files
# # change the file extension
# new_names <- sub(".csv", ".rds", names)
# mapply(saveRDS, new_dataA, new_names)
# 
###very good### start HERHE
# note: `list.files` can create a list of all files in a folder for you
 
##Check how CREATE OUTPUTS IN OTHER FOLDER OR MEANWHILE READ DATA@ COPY
files <- list.files("data2",pattern = ".csv", full.names = T )
 

# new_data <- lapply(files, read.csv)
# names(new_data) <- gsub(".csv", "", files)
# list2env(new_data, globalenv())

 data <- lapply(files, reading_data)
names(data) <- gsub(".csv", "", files)
list2env(data, globalenv())



#  data.2 <- lapply(files, reading_data)
# names(new_data.2) <- gsub(".csv", "", files)
# list2env(new_data.2, globalenv())
# bb <- list(new_data.2)
# my idea is to create  list of dataframe s and then with two or 3 functions run a a loop 

filtering <- function(filename) {
  data <- filename  %>%
    group_by(ID, dose) %>%
    mutate(growth_range = list(get_range(growth))) %>%
    unnest(cols = c(growth_range)) %>%
    filter(growth <= upper & growth >= lower) %>%
    rename(response = growth) %>%
    ungroup() %>%
    select(c(ID, experimental_replicate, repeats, dose, response))
  return(data)
}
# for(filename in data){
#   filtering(filename )
# }
#new_data.2 <- lapply(new_data, as_data_frame)
#lapply(names(new_data), filtering)
data.2 <- lapply(data, filtering)
data.3 <- lapply(data.2, getting_EC50)

dir.create("outputs_2")
path_out <-  "/Users/enieto/Documents/Mexico-Fungicides/outputs_2"

# lapply(data.3, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
# for (i in seq_along(data.3)) {
#   filename = paste(names(data.3)[i], ".csv")
#   write.csv(path_out, data.3[[i]], filename)
# }
for (i in seq_along(data.3)) {
  filename = paste(names(data.3)[i], "OUTPUT.csv")
  # dir.create("outputs_2")
  #dir.create(paste0("folder", i), showWarnings = FALSE)
  write.csv(data.3[[i]], filename)
  #file.path(paste0("folder",i)
  #write.csv(dt[i,], file = paste0("loop_", i, ".csv"))
}

summary_EC50 <- function(filename) {
  data <- filename
  data <- data %>%
    summarise_at(vars(EC50), list(
      Min = min,
      Mean = mean,
      Max = max,
      Sd = sd
    ), na.rm = TRUE)
  # A tibble: 2 x 5
}

data.4 <- lapply(data.3, summary_EC50)
#summay_data_table<-data.frame(do.call(dplyr::bind_rows, .id = "File", data.4))
summay_data_table <- data.4 %>%  bind_rows( .id = "NAME",  )

write.csv(summay_data_table, "summay_data_table.csv")


mydata %>% 
  map(~summary(.)) %>% 
  rbind.data.frame

purrr::map_df(data.3,
              ~ data.frame(mean = mean(.x$EC50), med = median(.x$EC50)))

function(x){
  c(mean = mean(x), sd = sd(x))
}
my_summary()
###

summarise_at(vars(EC50), list(Min = min, Mean = mean, Max = max, Sd = sd)na.rm = TRUE)
### NOt worthy
# file_object <- file...
# exfile_object <- exfile...
# for(file in 1:length(files)) { 
#   file_name <- paste(c("file00",file), collapse = " ")
#   file_name <- gsub(" ", "", file_name, fixed = TRUE)
#   ex_file_name <- paste(c("exfile00",file), collapse = " ")
#   ex_file_name <- gsub(" ", "", ex_file_name, fixed = TRUE)
#   
#   file_object <- read.csv(file = paste(file_name, ".csv", sep=""),fileEncoding="UTF-8-BOM")
#   exfile_object <- read.csv(file = paste(ex_file_name, ".csv", sep=""),fileEncoding="UTF-8-BOM")
# }
# 
# 
# #get_range(unnest( new_data))
# 
# 
# ## Automatically using the object name as file name with write.table or write.csv
# 
# write.csv.named <- function(x, ...){
#   fname <- sprintf('%s.csv',deparse(substitute(x)))
#   write.csv(x=x, file =  fname, ...)
# }
# ###
# mycomparisons <-list (unique_downa = a[!(a%in%b)], unique_downb = b[!(b%in%a)])
# mapply (write.csv, mycomparisons, paste (names (mycomparisons), ".csv", sep =""))
# 
# 
# 
# reading_data <- function(filename){
#   data <-
#     read.csv(filename)
#   # data <- subset(data, select = -X)
#   data$repeats <- rep_len(1:3, length.out = nrow(data))
#   
#   data <- data %>%
#     mutate(polar= replace(growth, growth == 0, 6)) %>%  #replacing 0 cm growth for the size of plug that is 0.6
#     group_by(ID, experimental_replicate, concentration, growth, repeats) %>%
#     rename(dose = concentration )
# }
# 
 #reading_data("../data/thiophanate_rhizoctonia.csv")

##Read files and save R objects using file names from tibble
#EXMPLE
#list datasets
