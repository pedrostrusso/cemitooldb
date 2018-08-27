#' @import GEOquery 
#' @import Biobase
NULL

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

#' Get platform GSE ids which have entries in GEO datasets
#' 
#' This function is used to get the GEO accession numbers of studies of a given 
#' platform which have an entry in GEO datasets.
#' 
#' @param gpl_id The ID of the platform of interest
#' 
#' @rdname get_gds_gse_ids
#' @export
get_gds_gse_ids <- function(gpl_id){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gpl_id, "[ACCN]", " AND gds[ETYP]"), 
                                         use_history = TRUE)
    study_count <- gds_search$count
    
    gse_id_vec <- c()
    
    for(seq_start in seq(0, study_count, 50)){
        message(seq_start)
        search_res <- rentrez::entrez_search(db="gds", term=paste0(gpl_id, "[ACCN]", " AND gds[ETYP]"), 
                                             use_history = TRUE,
                                             retmax=50, retstart=seq_start)
        print(search_res$ids)
        #id_vec <- c(id_vec, search_res$ids)
        
        res <- entrez_summary(db="gds", id=search_res$ids)
        gse_id_vec <- c(gse_id_vec, paste0("GSE", res[[1]]$gse))
        gse_id_vec <- c(gse_id_vec, unlist(lapply(res, function(x) paste0("GSE", x$gse))))
    }
    #id_vec <- unlist(id_vec)
    return(gse_id_vec)
}


#' Get GDS ids whose related studies are in the given platform
#' 
#' @param gpl_id The ID of the platform of interest
#' 
#' @rdname get_gpl_related_ids
#' @export
get_gpl_related_gds <- function(gpl_id, min_sample_num=NULL, max_sample_num=NULL, verbose=FALSE){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gpl_id, "[ACCN]", " AND gds[ETYP]"), 
                                         use_history = TRUE)
    study_count <- gds_search$count
    
    id_vec <- c()
    
    for(seq_start in seq(0, study_count, 50)){
        if(verbose) message(seq_start)
        search_res <- rentrez::entrez_search(db="gds", term=paste0(gpl_id, "[ACCN]", " AND gds[ETYP]"), 
                                             use_history = TRUE,
                                             retmax=50, retstart=seq_start)
        res_ids <- search_res$ids
        
        if(!is.null(min_sample_num) | !is.null(max_sample_num)){
            for(id in res_ids){
                summary_res <- rentrez::entrez_summary(db="gds", id=id)
                if(summary_res$n_samples <= min_sample_num | summary_res$n_samples >= max_sample_num){
                    res_ids <- setdiff(res_ids, id)
                }
            }
        }
        if(verbose) print(res_ids)
        id_vec <- c(id_vec, res_ids)
        #res <- entrez_summary(db="gds", id=search_res$ids)
    }
    id_vec <- unlist(id_vec)
    id_vec <- paste0("GDS", id_vec)
    return(id_vec)
}


#' Get GSE object from GEO using the study GSE accession number
#'
#' This function is a wrapper function for getGEO, in which it will 
#' download GDS data.
#' @param gse_id The GEO accession ID for the study
#'
#' @rdname get_gse
#' @export
get_gds <- function(gds_id){
    gds <- GEOquery::getGEO(gds_id)
}

#' Get expression data from a GDS object
#' 
#' This function is used to extract expression data of a 
#' GDS object as given by get_gds.
#' @param gds The GDS object
#' @param do_log2 Whether or not to apply log2 transformation on the data
#' (Default: TRUE)
#'
#' @rdname get_expr_from_gds
#' @export

get_expr_from_gds <- function(gds, do_log2=TRUE){
    eset <- GEOquery::GDS2eSet(gds, do.log2=do_log2)
    expr <- as.data.frame(Biobase::exprs(eset))
    if(nrow(expr) > 0){
        expr$GeneSymbol <- Biobase::fData(eset)[[3]]
        return(expr)
    }else{
        return(NULL)
    }
    return(expr)
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


