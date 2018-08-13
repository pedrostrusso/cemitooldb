# "INSERT INTO study_info (study, disease, keywords, platform, organism) 
#                    VALUES ('%s', '%s', '%s', '%s', '%s');",

get_user_data <- function(user, dbname, host){
    password <- getPass::getPass()
    user_data <- list(user, password, dbname, host)
    names(user_data) <- c("user", "password", "dbname", "host")
    return(user_data)
}

populate_sql <- function(gds_id, user_data){
    
    con <- RMySQL::dbConnect(RMySQL::MySQL(),
                             user=user_data$user,
                             password=user_data$password,
                             dbname=user_data$dbname,
                             host=user_data$host)
    on.exit(RMySQL::dbDisconnect(con))
    
    res <- dbSendQuery(con, paste0("SELECT studyID FROM study_info WHERE study = '", gds_id, "';"))
    study_id <- dbFetch(res)
    dbClearResult(res)
    
    if(nrow(study_id) == 0){
        study_info <- scrape_study_info(gds_id)
        disease <- "NULL"
        
        insert_study_info(con=con, gds_id=gds_id, disease=disease, keywords=study_info$keywords, 
                          platform=study_info$platform, organism=study_info$organism, 
                          summary=study_info$summary, title=study_info$title, 
                          pubmedid=study_info$pubmedid)
    }
    
}

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
    names(study_info) <- c("title", "summary", "platform", "organism", "pubmedid", "keywords")
    return(study_info)
}

get_keywords <- function(pubmedid){
    xml_rec <- entrez_fetch(db="pubmed", id=pubmedid, rettype="xml", parsed=TRUE)
    
    nodes <- getNodeSet(xml_rec, '//DescriptorName')
    keywords <- unlist(lapply(nodes, xmlValue))
    return(keywords)
}

insert_study_info <- function(con, study, disease, keywords, platform, 
                              organism, summary, title, pubmedid){
    # con <- RMySQL::dbConnect(RMySQL::MySQL(),
    #                          user=user, 
    #                          password=password,
    #                          dbname=dbname,
    #                          host=host)
    # on.exit(RMySQL::dbDisconnect(con))
    
    sql <- sprintf("INSERT INTO study_info (study, disease, keywords, platform, organism, summary, title, pubmedid) 
                   VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');",
                   study, disease, keywords, platform, organism, summary, title, pubmedid)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
    id <- DBI::dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}
