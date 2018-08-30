
make_gds_annot <- function(gds){
    eset <- GEOquery::GDS2eSet(gds)
    annot <- pData(phenoData(eset))
    annot$sample <- NULL
    annot <- select_class(annot)
    annot$SampleName <- rownames(annot)
    
    return(annot)
}

select_class <- function(annot){
    priority_list <- c("disease.state", "protocol", "agent", "genotype/variation")
    
    if(any(priority_list %in% names(annot))){
        for(col_name in priority_list){
            if(col_name %in% names(annot)){
                annot <- dplyr::rename(annot, "Class" = col_name)
                break
            }
        }    
    }else{
        if(ncol(annot) > 2){
            # se numero de colunas for maior que dois, pegar a primeira coluna desde que nao seja "individual"
            if(names(annot[, 1, drop=FALSE]) == "individual"){
                annot <- dplyr::rename(annot, "Class" = names(annot[, 2, drop=FALSE]))
            }else{
                annot <- dplyr::rename(annot, "Class" = names(annot[, 1, drop=FALSE]))
            }
        }else{
            annot <- dplyr::rename(annot, "Class" = names(annot[, 1, drop=FALSE]))
        }
    }
    annot$Class <- gsub("'", "", annot$Class)
    return(annot)
}

make_gse_annot <- function(gse){
    annot <- data.frame("Sample_geo_accession"=gse[[1]]$geo_accession,
                        "Sample_title"=gse[[1]]$title,
                        "Sample_source_name_ch1"=gse[[1]]$source_name_ch1)
    tmp <- pData(phenoData(gse[[1]]))
    tmp <- tmp[, grepl("characteristics_ch1*", names(tmp))]
    annot$Sample_characteristics_ch1 <- Reduce(function(x, y) paste(x, y, sep="; "), tmp)
    return(annot)
}
