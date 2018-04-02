#' @import GEOquery 
#' @import Biobase
NULL

#' Run CEMiTool
#' 
#' This function is used to run the CEMiTool.R script included in the CEMiTool package.
#'
#' @param expression The name of the expression file to be analyzed 
#' @param output_dir The name of the folder to output the results
#' @param study_id The GEO accession number (GSE) of the study
#' @param annotation The name of the sample annotation file
#' @param interactions The name of the interactions file 
#' @param gmt_fname The name of the gmt file
#'
#' @rdname run_cemitool
#' @export
run_cemitool <- function(expression, output_dir, study_id, annotation, interactions, gmt_fname){
    system(
        paste("Rscript ./src/CEMiTool.R", 
              expression, 
              "-o", file.path(output_dir, study_id),
              "-s", annotation,
              "-i", interactions,
              "-p", gmt_fname,
              "-v")
    )
}

#' Get platform GSE ids
#'
#' This function is used to get the GEO accession numbers of all studies in a platform.
#'
#' @param gpl_id The ID of the platform of interest
#'
#' @rdname get_platform_gse_ids
#' @export

get_platform_gse_ids <- function(gpl_id){
    gpl <- GEOquery::getGEO(gpl_id)
    gpl_studies <- GEOquery::Meta(gpl)$series_id
    return(gpl_studies)
}

#' Get GSE object from GEO using the study GSE accession number
#'
#' This function is a wrapper function for getGEO, in which it will 
#' download the GSE matrix and get gpl data.
#' @param gse_id The GEO accession ID for the study
#'
#' @rdname get_gse
#' @export
get_gse <- function(gse_id){
    gse <- GEOquery::getGEO(gse_id, GSEMatrix=TRUE, getGPL=TRUE)
    return(gse)
}

#' Get expression data from a GSE object
#' 
#' This function is used to extract expression data of a 
#' GSE object as given by get_gse.
#' @param gse The GSE object
#'
#' @rdname get_expr_from_gse
#' @export

get_expr_from_gse <- function(gse){
    if(length(gse) > 1){
        return(NULL)
    }else{
        expr <- as.data.frame(Biobase::exprs(gse[[1]]))
        if(nrow(expr)>0){
            expr$GeneSymbol <- Biobase::fData(gse[[1]])[["Gene Symbol"]]
            return(expr)
        }else{
            return(NULL)
        }
    }
}

#' Get annotation data from GEO using a GSE object and
#' create a tentative sample_annotation file. 
#' 
#' The function tries to see which 
#' columns in the data provide information about each sample. 
#' @param gse The GSE object for the study
#'
#' @rdname get_annot_from_gse
#' @export

get_annot_from_gse <- function(gse){
    if(length(gse) > 1){
        return(NULL)
    }else{
        pheno <- pData(phenoData(gse[[1]]))
        # uniques <- apply(pheno, 2, function(x) length(unique(x)))
        # uniques2 <- uniques[uniques != nrow(pheno) & uniques != 1]
        # 
        # if(length(uniques2) == 0){
        #     return(pheno)
        # }
        # 
        # if("characteristics_ch1" %in% names(uniques2)){
        #     class_column <- "characteristics_ch1"
        # }else if("status:ch1" %in% names(uniques2)){
        #     class_column <- "status:ch1"
        # }else if("source_name_ch1" %in% names(uniques2)){
        #     class_column <- "source_name_ch1"
        # }else if("title" %in% names(uniques2)){
        #     class_column <- "title"
        # }else{
        #     class_column <- names(uniques2[which.max(uniques2)])
        # }
        # 
        # print(class_column)
        # 
        # pheno$Class <- pheno[, class_column]
        return(pheno) 
    }
}


#' Get expression and annotation tables
#' 
#' This function takes a vector of GEO study IDs, selects those with only one platform and 
#' those with samples within a given range. It then uses the collapse_rows function to collapse
#' probes, and finally writes the expression and annotation data to files in the given directory.
#'
#' @param gse_ids A character vector containing GEO study accession IDs
#' @param min_samples The minimum number of samples for studies (inclusive)
#' @param max_samples The maximum number of samples for studies (inclusive)
#' @param directory The directory to save files
#' @param force If TRUE, overwrites the given directory.
#' @param force_repeat If FALSE, avoids downloading study data already present
#' in the 'directory' folder (as informed by folder names).
#' @param get_expr If TRUE, get expression data.
#' @param get_annot If TRUE, get annotation data.
#'
#' @rdname write_expr_annot
#' @export
write_expr_annot <- function(gse_ids, 
                            min_samples=15, 
                            max_samples=200, 
                            directory=file.path(getwd(), "data"), 
                            force=FALSE, 
                            force_repeat=FALSE,
                            get_expr=TRUE,
                            get_annot=TRUE){
        
    if(dir.exists(directory)){
        if(!force){
            stop("Stopping analysis: ", directory, " already exists! Use force=TRUE to overwrite.")
        }
    }else{
        dir.create(directory, recursive=TRUE)
    }

    if(!force_repeat){
        message("Downloading remaining studies...")
        study_folders <- dir(directory)
        gse_ids <- gse_ids[which(!gse_ids %in% study_folders)]
    }
    
    lapply(gse_ids, function(gse_id){
        print(gse_id)
        tryCatch(
            gse <- get_gse(gse_id), 
            error=function(e){
                print(e)
            }, 
            finally={
                gc()
            }
        )
        if(get_expr){
            expr <- get_expr_from_gse(gse)
         
            if(!is.null(expr)){
                if(ncol(expr)-1 >= min_samples & ncol(expr)-1 <= max_samples){
                    collapsed <- .process_and_collapse(expr)
                                
                    dir.create(file.path(directory, gse_id))
                
                    write.table(collapsed, 
                                file.path(directory, gse_id, paste0(gse_id, "_expression", ".tsv")), 
                                sep="\t", quote=FALSE, col.names = NA)
                    
                    if(get_annot){
                        annot <- get_annot_from_gse(gse)
                        dir.create(file.path(directory, gse_id))
                        write.table(annot, 
                                    file.path(directory, gse_id, paste0(gse_id, "_annotation", ".tsv")),
                                    sep="\t", quote=FALSE, col.names = NA)
                    }           
                }
            }
        }else{
            if(get_annot){
                annot <- get_annot_from_gse(gse)
                dir.create(file.path(directory, gse_id))
                write.table(annot, 
                            file.path(directory, gse_id, paste0(gse_id, "_annotation", ".tsv")),
                            sep="\t", quote=FALSE, col.names = NA)
            }
        }
        gc()
    })    
}


