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


#' Get expression data from GEO using the study's GSE accession number
#' 
#' This function is used to query GEO and extract expression data of a 
#' study using the accession number as ID.
#' @param gse_id The GEO accession ID for the study
#'
#' @rdname get_expr_from_gse
#' @export

get_expr_from_gse <- function(gse_id){
    gse <- GEOquery::getGEO(gse_id, GSEMatrix=TRUE, getGPL=TRUE)
    if(length(gse) > 1){
        return(NULL)
    }else{
        expr <- as.data.frame(Biobase::exprs(gse[[1]]))
        expr$GeneSymbol <- Biobase::fData(gse[[1]])[["Gene Symbol"]]
        return(expr)
    }
}

#' Get studies with a specified range of samples
#' 
#' This function filters a list of expression data and returns the list 
#' without studies which fall outside of the specified sample number range.
#' It assumes that there is a column with gene symbols in each expression data, 
#' so it subtracts 1 from the number of columns. 
#'
#' @param expr_list A list object in which each element is an expression data.frame with 
#' a column with gene symbols and probe names in the rownames
#' @param min_samples The minimum number of samples to be included (inclusive)
#' @param max_samples The maximum number of samples to be included (inclusive)
#'
#' @rdname get_sample_number_studies
#' @export 
get_sample_number_studies <- function(expr_list, min_samples, max_samples){
    list_condition <- sapply(expr_list, function(x) ncol(x)-1 >= min_samples & ncol(x)-1 <= max_samples)
    output_list <- expr_list[list_condition]        
}


