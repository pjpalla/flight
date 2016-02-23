

build_dissolution_dataset <- function(filepath, extension = ".csv") {
    
    
    files = dir(filepath, pattern = extension)
    col_ids <- 22:24
    headers <- c("dis_12", "dis_12_t", "dis_45", "dis_45_t", "dis_68", "dis_68_t")
    m = matrix(nrow = 0, ncol = length(headers))
    dataset <- data.frame(m)
    
    for (k in seq_along(files)) {
        file = file.path(filepath, files[k])
        tmp_data <- list()
        data = read.csv(file, header = T)
        for (i in seq_along(col_ids)) {
            col <- data[, col_ids[i]]
            tmp_col <- extract_dissolution(col)
            tmp_data[[i]] <- tmp_col
        }
        
        tmp_data <- as.data.frame(tmp_data)
        dataset <- rbind(dataset, tmp_data)
        
    }
    
    # dataset <- as.data.frame(dataset)
    names(dataset) <- headers
    dataset
    
}





trim_space <- function(x) gsub("^\\s+|\\s+$", "", x)

extract_dissolution <- function(column) {
    
    if (all(is.na(column))) {
        c1 <- rep(NA, length(column))
        c2 <- rep(NA, length(column))
        
    } else {
        
        c1 <- c()
        c2 <- c()
        
        column <- as.vector(column)
        column <- tolower(column)
        
        for (i in seq_along(column)) {
            
            if (!is.null(column[i]) && column[i] != "") {
                
                cl <- trim_space(column[i])
                cl <- gsub("^>(\\s)*", ">", cl)
                cl <- gsub("^≥(\\s)*", "≥", cl)
                cl <- gsub("^<(\\s)*", "<", cl)
                cl <- gsub("\\%|\\(|\\)|min", "", cl)
                
                
                tmp <- unlist(strsplit(cl, " "))
                c1[i] <- tmp[1]
                # c1[i] <- round(as.numeric(tmp[1]), 3)
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
    
    output <- data.frame(c1, c2)
    
} 
