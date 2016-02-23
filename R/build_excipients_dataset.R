source("collect_excipients.R")

# path = '/Users/Pg/Documents/CNR/FarmaEqui/eccipienti/formulazioni'
#' Building the dataset containing the excipients of each drug
#' @param filepath the path of the directory containing the data of each drug of interest
#' @return a dataframe with the following columns: the drug name, the aic owner, the State and the list of all excipients
#' extracted from all files included in the directory
#' @author Piergiorgio Palla
#' @export
build_excipients_dataset <- function(filepath, extension = ".csv") {
    files = dir(filepath, pattern = extension)
    
    excipients = c()
    headers = c()
    
    # file_name = 'ilaria.txt'
    
    for (i in seq_along(files)) {
        file = file.path(filepath, files[i])
        data = read.csv(file, header = T)
        if (length(headers) == 0) {
            headers = names(data)[c(4, 5, 1)]
        }
        
        
        buffer <- collect_excipients(file)
        
        # write(paste('\n****', files[i]), file = 'ilaria.txt', append = T) write(buffer, file = 'ilaria.txt', append = T)
        
        excipients <- append(buffer, excipients)
        
        
    }
    
    # Here we create the dataframe of the excipients of each drug
    excipients <- (unique(excipients))
    headers <- append(headers, excipients)
    tmp_mat <- matrix(nrow = 0, ncol = length(headers))
    dataset <- data.frame(tmp_mat)
    names(dataset) <- headers
    
    dataset
}

#' Populating the dataframe of the excipients
#' @param dataframe the data structure created to contain the excipients of each drug
#' @param path the path of the directory containg the csv files of each drug
#' @author Piergiorgio Palla
#' @return a populated dataframe
populateExcipients <- function(dataframe, path, extension = ".csv", save = F) {
    
    files <- dir(path, pattern = extension)
    column_indexes <- c(10, 11, 12, 13, 14, 15)
    df_index <- 0
    for (i in seq_along(files)) {
        file <- file.path(path, files[i])
        
        
        data <- read.csv(file, header = T)
        for (k in 1:nrow(data)) {
            excipients <- c()
            dataframe[df_index + k, 1] <- as.vector(data[k, 4])  #drug name
            dataframe[df_index + k, 2] <- as.vector(data[k, 5])
            dataframe[df_index + k, 3] <- as.vector(data[k, 1])
            excipients <- extract_drug_excipients(data, k, column_indexes)
            if (!is.null(excipients) && !is.na(excipients) && length(excipients) != 0) {
                dataframe[df_index + k, excipients] <- 1
            } else {
                dataframe[df_index + k, 4:ncol(dataframe)] <- 0
            }
            
        }
        df_index = df_index + nrow(data)
        
    }
    
    dataframe[is.na(dataframe)] <- 0
    if (save == T) {
        write.csv(dataframe, file = "excipients_dataset.csv", sep = ",", col.names = T)
    }
    dataframe
    # print(dataframe[1:10, 5:15])
    
} 
