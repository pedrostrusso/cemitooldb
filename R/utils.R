#' @import data.table
#' @import stats

.datatable.aware <- TRUE
#' Collapse redundant probes into gene symbols
#'
#' This function collapses redundant probe expression rows into single rows
#' based on repeated gene symbols in a given column. 
#' 
#' @param expr Object of class \code{data.frame} containing expression values,
#' a column with probe symbols and a column with gene symbols
#' @param probe_col Character string specifying the name of the column containing 
#' probe names. Can also take rownames as input using the string "rownames".
#' @param gene_col Character string specifying the name of the column containing 
#' gene symbols to be used to collapse. 
#' @param data_table Logical. If \code{TRUE} will return a \code{data.table} object.
#' Defaults to \code{FALSE}. 
#' @param method Method used to collapse the probes. Can take one of "max_mean", 
#' "min_mean", "col_mean" or "col_median".
#'
#' @return Object of class \code{data.frame} or \code{data.table}
#' 
#' @rdname collapse_rows 
#' @export

collapse_rows <- function(expr, probe_col, gene_col, data_table=FALSE, 
                          method=c("max_mean", "min_mean", "col_mean", "col_median")){
    if(!requireNamespace("data.table", quietly=TRUE)){
        stop("Package data.table is required for this function to work. Please install it.", 
             call. = FALSE)
    }else{
        suppressPackageStartupMessages(requireNamespace("data.table"))
    }
    
    if (probe_col == "rownames"){
        expr <- data.table(expr, keep.rownames=TRUE)
        setnames(expr, "rn", "rownames")
    }else{
        expr <- data.table(expr)
    }
        
    if(method=="max_mean" | method=="min_mean"){ 
        expr[, rowmean := rowMeans(.SD[, !c(probe_col, gene_col), with=FALSE])]
        if(method=="max_mean"){
            res <- expr[order(rowmean, decreasing=TRUE)][, .SD[1], by=gene_col][, rowmean:=NULL]
        }else if(method=="min_mean"){
            res <- expr[order(rowmean, decreasing=TRUE)][, .SD[.N], by=gene_col][, rowmean:=NULL]
        }
    }else if(method=="col_mean"){
        res <- expr[, lapply(.SD[, !c(probe_col), with=FALSE], mean), by=gene_col]
    }else if(method=="col_median"){
        res <- expr[, lapply(.SD[, !c(probe_col), with=FALSE], median), by=gene_col]   
    }
    else stop("method must be 'max_mean', 'min_mean', 'col_mean' or 'col_median'\n")
                        
    if(!data_table){
        return(data.frame(res))
    }else{ return(res[]) }
}

#' Collapse studies in a list
#'
#' This function takes a list of expression data and applies the collapse_rows function
#'
#' @param expr_list A list with expression data elements
#'
#' @return A list with collapsed expression data
#'
#' @rdname collapse_list
#' @export
collapse_list <- function(expr_list){
    collapsed_list <- lapply(expr_list, function(expression){
        expression <- expression[!expression$GeneSymbol=="", ] 
        # Remove " /// " from composite gene names and keep the first
        expression$GeneSymbol  <- sapply(expression$GeneSymbol, function(x) {
                                         strsplit(x, split=" /// ")[[1]][1]
                                         })
        col <- collapse_rows(expression, 
                             probe_col = "rownames", 
                             gene_col="GeneSymbol", 
                             method="max_mean")    
                                         
        rownames(col) <- col$GeneSymbol
        col$GeneSymbol <- NULL
        col$rownames <- NULL
        col
    })
    names(collapsed_list) <- names(expr_list)
    return(collapsed_list)
}

.process_and_collapse <- function(expr){
    genes <- as.character(expr$GeneSymbol)
    # Remove " /// " from composite gene names and keep the first
    genes <- sapply(genes, function(x) {
        strsplit(x, split=" /// ")[[1]][1]
    })
    
    probes <- rownames(expr)
    names <- names(expr)
    
    expr <- apply(expr[, 1:ncol(expr)-1], 2, function(f){
        as.numeric(as.character(f))
    })
    expr <- as.data.frame(expr)
    #expr <- as.data.frame(cbind(expr, genes), stringsAsFactors=FALSE)
    expr$GeneSymbol <- genes
    
    names(expr) <- names
    rownames(expr) <- probes
    expr <- expr[!is.na(expr$GeneSymbol), ]
    
    collapsed <- collapse_rows(expr, probe_col = "rownames", gene_col="GeneSymbol")
    rownames(collapsed) <- collapsed$GeneSymbol
    collapsed$GeneSymbol <- NULL
    collapsed$rownames <- NULL
    
    return(collapsed)
}


annotate_studies <- function(annot){
    
}













