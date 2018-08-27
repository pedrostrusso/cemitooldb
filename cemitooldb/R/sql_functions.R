#' @import RMySQL
#' @import DBI
#' @import getPass
#' 
NULL

#' Insert study information
#'
#' This function takes information about a study and adds it into the 
#' study_info table in the database.
#'
#' @param study_id Character string with the GEO accession number to be added
#' @param disease Character string describing the disease or condition the 
#' study refers to
#' @param keywords Character string with keywords describing the study to be added,
#' with multiple keywords separated by semicolons 
#' @param platform Character string specifying the GEO platform used by the study
#' @param organism Character string specifying the study organism.
#' 
#' @rdname insert_study_info
#' @export
insert_study_info <- function(pool, gds_id, disease, platform, organism, summary, title, pubmedid){
    sql <- sprintf("INSERT INTO study_info (study, disease, platform, organism, summary, title, pubmedid) 
                   VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s');",
                   gds_id, disease, platform, organism, summary, title, pubmedid)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    #rs <- RMySQL::dbSendQuery(pool, sql)
    rs <- pool::dbGetQuery(pool, sql)
    #RMySQL::dbClearResult(rs)
    #id <- pool::dbGetQuery(pool, "select last_insert_id();")[1,1]
    #return(id)
}

#' Insert sample information
#'
#' This function takes information about samples and adds it into the 
#' sample_annot table in the database.
#'
#' @param pool A connection object
#' @param annot A data frame containing sample annotation information
#' @param study_id ID of the study in the study_info table
#' 
#' @rdname insert_sample_info
#' @export
insert_sample_info <- function(pool, annot, study_id){
    for(sample in rownames(annot)){
        sql <- sprintf("INSERT INTO sample_annot (sample_name, studyID, class) 
                   VALUES ('%s', %s, '%s');",
                       sample, study_id, annot[sample, "Class"])
        sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
        rs <- pool::dbExecute(pool, sql)
    }
}

#' Insert CEMiTool run information
#'
#' This function takes information about a call to the CEMiTool.R script
#' and adds the information into the cemitool_run table in the database.
#'
#' @param study_id Character string with the GEO accession number to be added
#' @param date Date and time the CEMiTool analysis was run 
#' @param results Path to the CEMiTool report file
#' @param plots Path to the CEMiTool plots directory
#' @param reports Path to the CEMiTool reports directory
#'
#' @rdname insert_run
#' @export
insert_run <- function(pool, study_id, results_dir, run_date){
    tables <- file.path(results_dir, gds_id, "Tables")
    plots <- file.path(results_dir, gds_id, "Plots")
    reports <- file.path(results_dir, gds_id, "Reports")
    sql <- sprintf("INSERT INTO cemitool_run (studyID, run_date, tables, plots, reports) 
                   VALUES (%s, '%s', '%s', '%s', '%s');",
                   study_id, run_date, tables, plots, reports)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    
    conn <- poolCheckout(pool)
    rs <- pool::dbSendQuery(conn, sql)
    poolReturn(conn)
    RMySQL::dbClearResult(rs)
    id <- pool::dbGetQuery(pool, "select last_insert_id();")[1,1]
    return(id)
}

insert_keywords <- function(pool, study_id, keywords){
    sql <- "INSERT INTO keywords (studyID, keyword) VALUES "
    add_string <- paste(paste0("(", study_id, ", '", keywords, "')"), collapse=", ")
    sql <- paste0(sql, add_string, ";")    
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- pool::dbExecute(pool, sql)
    
}

truncate_tables <- function(pool){
    rs <- pool::dbExecute(pool, "SET foreign_key_checks = 0;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE study_info;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE cemitool_run;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE keywords;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE sample_annot;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE modules;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE ora;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE module_genes;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE enrichment;")
    rs <- pool::dbExecute(pool, "TRUNCATE TABLE interactions;")
    rs <- pool::dbExecute(pool, "SET foreign_key_checks = 1;")
}

delete_study <- function(pool, gds_id, which_rows=c('last', 'duplicates', 'all')){
    which_rows <- match.arg(which_rows)
    
    conn <- poolCheckout(pool)
    res <- pool::dbSendQuery(conn, paste0("SELECT studyID FROM study_info WHERE study = '", gds_id, "';"))    
    poolReturn(conn)
    study_id <- dbFetch(res)
    dbClearResult(res)
    
    conn <- poolCheckout(pool)
    res <- pool::dbSendQuery(conn, paste0("SELECT runID FROM cemitool_run WHERE studyID = '", study_id, "';"))    
    poolReturn(conn)
    run_id <- dbFetch(res)
    dbClearResult(res)
    
    if(nrow(run_id) > 1){
        if(which_rows == "last"){
            message("Deleting last inserted row for study ", gds_id, "...")
            run_id <- tail(run_id, 1)
        }else if(which_rows == "duplicates"){
            message("Deleting duplicate rows...")
            run_id <- run_id[-1, , drop=FALSE]
        }else{
            message("Deleting all rows...")
        }
    }else if(nrow(run_id) == 1){
        if(which_rows == "last"){
            stop("Only one entry for study ", gds_id, ". Please use which_rows='all' to delete it.")
        }
    }
    
    add_sql <- paste("'", paste(run_id[, 1], collapse="', '"), "'", sep="")
    
    conn <- poolCheckout(pool)
    res <- pool::dbSendQuery(conn, paste0("SELECT moduleID FROM modules WHERE runID IN (", add_sql, ");"))    
    poolReturn(conn)
    module_ids <- dbFetch(res)
    dbClearResult(res)
    
    add_sql2 <- paste("'", paste(module_ids[, 1], collapse="', '"), "'", sep="")
    
    rs <- pool::dbExecute(pool, paste0("DELETE FROM interactions WHERE moduleID IN (", add_sql2, ");"))
    rs <- pool::dbExecute(pool, paste0("DELETE FROM enrichment WHERE moduleID IN (", add_sql2, ");"))
    rs <- pool::dbExecute(pool, paste0("DELETE FROM module_genes WHERE moduleID IN (", add_sql2, ");"))
    rs <- pool::dbExecute(pool, paste0("DELETE FROM ora WHERE moduleID IN (", add_sql2, ");"))
    
    rs <- pool::dbExecute(pool, paste0("DELETE FROM modules WHERE runID IN (", add_sql, ");"))
    
    rs <- pool::dbExecute(pool, paste0("DELETE FROM cemitool_run WHERE runID IN (", add_sql, ");"))
    
    if(which_rows == "all"){
        rs <- pool::dbExecute(pool, paste0("DELETE FROM sample_annot WHERE studyID = ", study_id, ");"))
        rs <- pool::dbExecute(pool, paste0("DELETE FROM keywords WHERE studyID = ", study_id, ");"))
    }
}



