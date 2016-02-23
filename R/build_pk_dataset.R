build_pk_dataset <- function(filepath, extension = ".csv") {
    
    
    files = dir(filepath, pattern = extension)
    col_ids <- 17:21
    headers <- c("cmax", "cmax sd", "auc", "auc sd", "auc0inf", "auc0inf sd", "tmax", "tmax sd", "t1/2", "t1/2 sd")
    dataset <- c()
    
    for (k in seq_along(files)) {
        file = file.path(filepath, files[k])
        tmp_data <- c()
        data = read.csv(file, header = T)
        for (i in col_ids) {
            col <- data[, i]
            tmp_col <- extract_pk_data(col)
            tmp_data <- cbind(tmp_data, tmp_col)
        }
        dataset <- rbind(dataset, tmp_data)
        
    }
    
    dataset <- as.data.frame(dataset)
    names(dataset) <- headers
    dataset
    
}



extract_pk_data <- function(column) {
    
    
    
    if (all(is.na(column))) {
        c1 <- rep(NA, length(column))
        c2 <- rep(NA, length(column))
        
    } else {
        
        c1 <- c()
        c2 <- c()
        
        column <- as.vector(column)
        column <- tolower(column)
        
        for (i in seq_along(column)) {
            
            if (!is.na(column[i]) && !is.null(column[i]) && column[i] != "") {
                
                cl <- trim(column[i])
                # cl <- gsub('\\%|(\\([-|\\d+]\\))|(\\[\\d*\\])', '', x)
                cl <- gsub("\\%|\\(|\\)|-|(\\[\\d+\\])*", "", cl)
                cl <- trim(cl)
                
                tmp <- unlist(strsplit(cl, " "))
                c1[i] <- round(as.numeric(tmp[1]), 3)
                if (length(tmp) >= 2) {
                  c2[i] <- round(as.numeric(tmp[2]), 3)
                } else {
                  c2[i] <- NA
                }
            } else {
                c1[i] = NA
                c2[i] = NA
            }
        }
    }
    
    output <- cbind(c1, c2)
    
}


trim_space <- function(x) gsub("^\\s+|\\s+$", "", x)
 
