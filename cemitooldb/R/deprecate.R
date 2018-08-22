library(rvest)

scrape_sample_annot <- function(gse_id){
    gds_search <- rentrez::entrez_search(db="gds", term=paste0(gse_id, "[ACCN] AND gsm[ETYP]"))
    search_res <- rentrez::entrez_summary(db="gds", id=gds_search$ids)
    res <- lapply(search_res, unlist)
    res <- plyr::ldply(res)
    gsm_ids <- res$accession
    bla <- sapply(gsm_ids, scrape_sample_char)
    
    res <- subset(res, select=c("gse", "accession", "gpl", "ftplink", "title", "summary", SAMPLE_CHARACTERISTICS))
}

scrape_sample_char <- function(gsm_id){
    url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", sample_id)
    webpage <- read_html(url)
    chars <- html_nodes(webpage, xpath="//td[.='Characteristics']/following-sibling::td[1]")
    chars <- gsub("<br>|\n", " ", chars)
    chars <- html_text(xml2::as_xml_document(chars))
    chars <- trimws(chars)    
    return(chars)
}
