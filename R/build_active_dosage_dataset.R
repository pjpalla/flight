build_active_dosage_dataset <- function(filepath, extension = ".csv") {
    
    
    files = dir(filepath, pattern = extension)
    col_ids <- c(2, 3, 7, 9)
    headers <- c("active_sub", "form", "form_charact", "active_sub_dosage")
    m = matrix(nrow = 0, ncol = length(headers))
    dataset <- data.frame(m)
    
    
    for (k in seq_along(files)) {
        file = file.path(filepath, files[k])
        tmp_data <- list()
        data = read.csv(file, header = T)
        for (i in seq_along(col_ids)) {
            col <- data[, col_ids[i]]
            tmp_data[[i]] <- col
        }
        tmp_data <- as.data.frame(tmp_data)
        names(tmp_data) <- headers
        dataset <- rbind(dataset, tmp_data)
        
    }
    
    
    # names(dataset) <- headers
    dataset
    
} 
