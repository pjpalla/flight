build_ratio_dataset <- function(filepath, extension = ".csv") {
    
    files = dir(filepath, pattern = extension)
    col_ids <- 16
    headers <- c("as_exc_ratio")
    # m = matrix(nrow = 0, ncol = length(headers)) dataset <- data.frame(m)
    dataset <- c()
    
    
    for (k in seq_along(files)) {
        file = file.path(filepath, files[k])
        tmp_data <- list()
        data = read.csv(file, header = T)
        for (i in seq_along(col_ids)) {
            col <- data[, col_ids[i]]
            tmp_col <- extract_ratio(col)
            # tmp_data[[i]] <- tmp_col
        }
        
        # tmp_data <- as.data.frame(tmp_data)
        dataset <- append(dataset, tmp_col)
        
        
    }
    
    dataset <- data.frame(dataset)
    names(dataset) <- headers
    dataset
    
}



extract_ratio <- function(column) {
    
    if (all(is.na(column))) {
        c1 <- rep(NA, length(column))
        
    } else {
        
        c1 <- c()
        column <- as.vector(column)
        column <- tolower(column)
        
        for (i in seq_along(column)) {
            if (!is.null(column[i]) && column[i] != "") {
                
                cl <- trim_space(column[i])
                
                tmp <- unlist(strsplit(cl, "\\s*:\\s*"))
                x = unlist(strsplit(tmp[2], "/"))
                num = as.numeric(x[1])
                den = as.numeric(x[2])
                
                c1[i] = round(num/den, 4)
            } else {
                c1[i] = NA
            }
        }
        
    }
    return(c1)
}



build_disaggregation_dataset <- function(filepath, extension = ".csv") {
    
    files = dir(filepath, pattern = extension)
    col_ids <- 26
    headers <- c("disaggr_12", "disaggr_45", "disaggr_68")
    m = matrix(nrow = 0, ncol = length(headers))
    dataset <- data.frame(m)
    
    for (k in seq_along(files)) {
        file = file.path(filepath, files[k])
        tmp_data <- list()
        data = read.csv(file, header = T)
        dis_col <- extract_disaggregation(data[, col_ids])
        tmp_data <- dis_col
        dataset <- rbind(dataset, tmp_data)
        
        
    }
    
    names(dataset) <- headers
    dataset
    
}

filter_space <- function(column) {
    
    if (all(is.na(column))) {
        c1 <- rep(NA, length(column))
        
    } else {
        c1 <- c()
        column <- as.vector(column)
        column <- tolower(column)
        
        for (i in seq_along(column)) {
            if (!is.null(column[i]) && column[i] != "") {
                c1[i] = gsub("\\s*", "", column[i])
            } else {
                c1[i] = NA
            }
        }
    }
    c1
}


extract_disaggregation <- function(column) {
    
    if (all(is.na(column))) {
        c1 <- rep(NA, length(column))
        c2 <- rep(NA, length(column))
        c3 <- rep(NA, length(column))
        
    } else {
        
        c1 <- c()
        c2 <- c()
        c3 <- c()
        
        column <- as.vector(column)
        column <- tolower(column)
        
        for (i in seq_along(column)) {
            
            if (!is.null(column[i]) && column[i] != "") {
                
                cl <- gsub("\\s*", "", column[i])
                
                tmp <- unlist(strsplit(cl, ";"))
                c1[i] <- tmp[1]
                # c1[i] <- round(as.numeric(tmp[1]), 3)
                
                if (length(tmp) == 1) {
                  c1[i] <- tmp[1]
                  c2[i] <- NA
                  c3[i] <- NA
                  
                } else if (length(tmp) == 2) {
                  c1[i] <- tmp[1]
                  c2[i] <- tmp[2]
                  c3[i] <- NA
                } else if (length(tmp) == 3) {
                  c1[i] <- tmp[1]
                  c2[i] <- tmp[2]
                  c3[i] <- tmp[3]
                }
            } else {
                c1[i] <- NA
                c2[i] <- NA
                c3[i] <- NA
            }
        }
    }
    
    data.frame(c1, c2, c3)
    
} 
