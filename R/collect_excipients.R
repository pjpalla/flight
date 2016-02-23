collect_excipients <- function(file) {
    
    data <- read.csv(file, sep = ",")
    excipients = c()
    
    for (i in 1:nrow(data)) {
        
        ### Excipients from the italian information pamphlet
        exc1 <- extract_excipients(data[i, 10])
        exc2 <- extract_excipients(data[i, 11])
        exc3 <- extract_excipients(data[i, 12])
        
        exca <- union(exc2, union(exc1, exc3))
        exca = unlist(exca)
        
        
        
        ##### excipients from literature
        
        if (length(exca) != 0) {
            exc = exca
        } else {
            exc4 <- extract_excipients(data[i, 13])
            exc5 <- extract_excipients(data[i, 14])
            exc6 <- extract_excipients(data[i, 15])
            excb = union(exc6, union(exc4, exc5))
            excb = unlist(excb)
            
            exc = excb
            
        }
        
        # cat(paste('Line ', i, ' length exc: ', length(exc), '\n'))
        
        for (k in seq_along(exc)) {
            if (!is.element(exc[k], excipients)) {
                excipients <- append(excipients, exc[k])
            }
        }
    }
    
    
    unique(excipients)
}


### This function is used to remove the spaces at the beginning and at the end of each excipient name
trim <- function(x) gsub("^\\s+|\\n|(\\s*>|\\s*<)|\\(|\\)|\\s+$", "", x)


### This function is used to remove the quantifications of the excipents and to extract only their names.
filter_percent <- function(x) {
    ### this regex allows to remove the percentage value or the ≤ and percentage at the end of the string
    
    pattern = "(\\s*(≤)?\\s*\\d+(\\.|:)?\\d*\\%$)"
    gsub(pattern, "", x)
    
}

extract_excipients <- function(column) {
    column <- as.vector(column)
    
    if (!is.null(column) && !is.na(column) && column != "") {
        
        column <- tolower(column)
        tmp <- strsplit(column, ";\\s?")
        excipients <- unlist(tmp)
        excipients <- sapply(excipients, trim, USE.NAMES = F)
        excipients <- sapply(excipients, filter_percent, USE.NAMES = F)
    } else {
        
        excipients = c()
    }
    excipients
}

extract_drug_excipients <- function(data, row_index, column_indexes) {
    
    drug_excipients <- c()
    
    ### This vector is used to check the presence of excipients in the columns from 10 to 12 In this case the remaining columns won't be considered;
    v = c()
    
    
    for (i in seq_along(column_indexes)) {
        buffer <- extract_excipients(data[row_index, column_indexes[i]])
        l = length(buffer)
        
        # cat(paste('col', i, 'number of exc:', l, '\n', sep = ' '))
        
        if (l != 0) {
            v <- append(v, l)
            
        }
        
        drug_excipients <- append(drug_excipients, buffer)
        
        if (i == 3 && length(v) != 0) {
            break
        }
    }
    drug_excipients <- unique(drug_excipients)
    
} 
