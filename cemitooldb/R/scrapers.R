
scrape_study_info <- function(gds_id){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gds_id, "[ACCN] AND gds[ETYP])"))
    search_res <- rentrez::entrez_summary(db="gds", id=gds_search$ids)
    title <- search_res$title
    summary <- trimws(search_res$summary)
    platform <- paste0("GPL", search_res$gpl)
    organism <- search_res$taxon
    related_gse <- paste0("GSE", search_res$gse)
    
    pubmedid <- search_res$pubmedids
    if(length(pubmedid) == 0){
        tmp <- rentrez::entrez_search(db="gds", term=paste0(related_gse, "[ACCN] AND gse[ETYP])"))
        tmp_res <- rentrez::entrez_summary(db="gds", id=tmp$ids)
        pubmedid <- tmp_res$pubmedids
    }
    if(!length(pubmedid) == 0){
        keywords <- get_keywords(pubmedid)
    }else{
        pubmedid <- "NULL"
        keywords <- "NULL"
    }
    
    study_info <- list(title, summary, platform, organism, pubmedid, keywords)
    study_info <- lapply(study_info, function(x) gsub("'s", "", x))
    study_info <- lapply(study_info, function(x) gsub("'", "", x))
    study_info <- lapply(study_info, function(x) gsub('"', "", x))
    
    names(study_info) <- c("title", "summary", "platform", "organism", "pubmedid", "keywords")
    
    return(study_info)
}

get_keywords <- function(pubmedid){
    xml_rec <- entrez_fetch(db="pubmed", id=pubmedid, rettype="xml", parsed=TRUE)
    
    nodes <- getNodeSet(xml_rec, '//DescriptorName')
    keywords <- unlist(lapply(nodes, xmlValue))
    return(keywords)
}
