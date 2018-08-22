stop_if <- function (expr, error) {
    if (expr) stop(error, call. = FALSE)
}

message_if <- function (expr, msg) {
    if (expr) message(msg)
}

collapse_rows <- function(expr, probe_col, gene_col, data_table=FALSE, 
                          method=c("max_mean", "min_mean", "col_mean", "col_median")){
    
    if (probe_col == "rownames"){
        expr <- data.table::data.table(expr, keep.rownames=TRUE)
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

process_and_collapse <- function(expr){
    genes <- as.character(expr$GeneSymbol)
    # Remove " /// " from composite gene names and keep the first
    genes <- sapply(genes, function(gene) gsub("\\s?\\/\\/\\/\\s?.*$", "", gene))
    
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
    expr <- expr[expr$GeneSymbol != "", ]
    
    collapsed <- collapse_rows(expr, probe_col = "rownames", gene_col="GeneSymbol")
    rownames(collapsed) <- collapsed$GeneSymbol
    collapsed$GeneSymbol <- NULL
    collapsed$rownames <- NULL
    
    return(collapsed)
}
