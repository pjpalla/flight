source("build_excipients_dataset.R")
source("build_pk_dataset.R")
source("dissolution.R")
source("hardness.R")
source("build_active_dosage_dataset.R")
source("ratio.R")


build_global_dataset <- function(filepath, save = F) {
    
    
    OUTPUT_PATH = "generated_datasets/"
    
    ### All files with the drugs must be collected into the same folder (specified in the filepath argument)
    
    ### First of all we extract the excipients
    
    d1 <- build_excipients_dataset(filepath)
    d1 <- populateExcipients(d1, filepath)
    
    ## Then we will extract the other parameters
    
    d2 <- build_pk_dataset(filepath)
    d3 <- build_dissolution_dataset(filepath)
    d4 <- build_hardness_dataset(filepath)
    d5 <- build_ratio_dataset(filepath)
    d6 <- build_disaggregation_dataset(filepath)
    
    
    l = list(d1, d2, d3, d4, d5, d6)
    
    d <- as.data.frame(l)
    
    if (save == T) {
        tag <- Sys.time()
        filename <- paste(OUTPUT_PATH, "dataset_", sep = "")
        output_filename <- paste(filename, tag, ".csv", sep = "")
        print(output_filename)
        write.csv(d, file = output_filename, sep = ",", col.names = T, row.names = F)
    }
    
    
    d
    
}


### This function adds to the global_dataset the columns related to the active_sub, the form, the form characteristics and the active substance dosage (4
### columns)
build_global_extended <- function(filepath, save = F) {
    
    d1 <- build_active_dosage_dataset(filepath)
    
    d2 = build_global_dataset(filepath)
    
    d = cbind(d1, d2)
    
    
    
}

# source('build_excipients_dataset.R') source('build_pk_dataset.R') build_global_dataset <- function(filepath, save = F){ OUTPUT_PATH <-
# '/Users/Pg/Documents/CNR/FarmaEqui/generated_datasets/' ### Here we extract the excipients from the csv files ### dataframe1 <-
# build_excipients_dataset(filepath) dataframe1 <- populateExcipients(dataframe1, filepath) ### Here we extract the pharmacokinetic parameters ### dataframe2
# <- build_pk_dataset(filepath) dataset <- cbind(dataframe1, dataframe2) if (save == T){ tag <- Sys.time() filename <- paste(OUTPUT_PATH, 'dataset_', sep =
# '') output_filename <- paste(filename, tag, '.csv', sep='') print(output_filename) write.csv(dataset, file = output_filename, sep=',', col.names = T,
# row.names = F) } dataset } 
