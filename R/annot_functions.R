#annot0 <- annot
#annot0 -> annot
.get_desc_list <- function(annot){
    bad_columns <- c("^status$", "submission_date", "last_update_date", "^type$", "channel_count",
                     "label_protocol_ch1", "taxid_ch1", "hyb_protocol", "scan_protocol",
                     "description", "data_processing", "contact_*", "supplementary_file*", 
                     "data_row_count", "relation")
    
    bad_inds <- sapply(bad_columns, function(column){
        grep(column, names(annot))
    })
    bad_inds <- unlist(bad_inds)
    annot <- annot[, -bad_inds]
    
    col_names <- names(annot)
    desc_list <- lapply(col_names, function(col_name){
        #i <- which(col_names == col_name)
        
        column <- annot[, col_name]
        
        if(is.factor(column)){
            column <- levels(column)[column]
        }
        #message(c(i, ") ", col_name, " :"))
        res <- paste(unique(column), collapse=" ")
        #print(res)
        res
    })
    names(desc_list) <- col_names
    return(desc_list)
}

.show_list <- function(desc_list){
    col_names <- names(desc_list)
    bla <- lapply(col_names, function(col_name){
        i <- which(col_names == col_name)
        
        column <- desc_list[[col_name]]
        
        #if(is.factor(column)){
        #    column <- levels(column)[column]
        #}
        message(c(i, ") ", col_name, " :"))
        res <- paste(unique(column), collapse=" ")
        print(res)
        #res
    })
}

.read_selection <- function(){
    n <- readline(prompt="Select number of column to be used as class:")
    if(!is.na(as.numeric(x))){
        print(n)
    }else if(n == "NA"){
        
    }
}

annotate_studies <- function(annot){
    desc_list <- .get_desc_list(annot)
    .show_list(desc_list)
}

column_hist <- function(study_folder){
    study_columns <- sapply(list.files(study_folder), function(folder){
        print(folder)
        tryCatch(
            annot <- data.table::fread(file.path(study_folder, 
                                             folder, 
                                             paste0(folder, "_annotation.tsv")), 
                                   data.table=FALSE), 
            error=function(e){
                unlink(folder, recursive=TRUE)
            }
        )
        colnames(annot)
    })
    names(study_columns) <- list.files(study_folder)
}










