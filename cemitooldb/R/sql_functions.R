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
insert_study_info <- function(con, gds_id, disease, platform, organism, summary, title, pubmedid){
    sql <- sprintf("INSERT INTO study_info (study, disease, platform, organism, summary, title, pubmedid) 
                   VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s');",
                   gds_id, disease, platform, organism, summary, title, pubmedid)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
    id <- DBI::dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}

#' Insert sample information
#'
#' This function takes information about samples and adds it into the 
#' sample_annot table in the database.
#'
#' @param con A connection object
#' @param annot A data frame containing sample annotation information
#' @param study_id ID of the study in the study_info table
#' 
#' @rdname insert_study_info
#' @export
insert_sample_info <- function(con, annot, study_id){
    for(sample in rownames(annot)){
        sql <- sprintf("INSERT INTO sample_annot (sample_name, studyID, class) 
                   VALUES ('%s', %s, '%s');",
                       sample, study_id, annot[sample, "Class"])
        sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
        rs <- RMySQL::dbSendQuery(con, sql)
        RMySQL::dbClearResult(rs)
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
insert_run <- function(con, study_id, results_dir, run_date){
    tables <- file.path(results_dir, gds_id, "Tables")
    plots <- file.path(results_dir, gds_id, "Plots")
    reports <- file.path(results_dir, gds_id, "Reports")
    sql <- sprintf("INSERT INTO cemitool_run (studyID, run_date, tables, plots, reports) 
                   VALUES (%s, '%s', '%s', '%s', '%s');",
                   study_id, run_date, tables, plots, reports)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
    id <- DBI::dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}

insert_keywords <- function(con, study_id, keywords){
    sql <- "INSERT INTO keywords (studyID, keyword) VALUES "
    add_string <- paste(paste0("(", study_id, ", '", keywords, "')"), collapse=", ")
    sql <- paste0(sql, add_string, ";")    
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
}

truncate_tables <- function(con){
    rs <- RMySQL::dbSendQuery(con, "SET foreign_key_checks = 0;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE study_info;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE cemitool_run;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE keywords;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE sample_annot;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE modules;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE ora;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE module_genes;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE enrichment;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "TRUNCATE TABLE interactions;")
    RMySQL::dbClearResult(rs)
    rs <- RMySQL::dbSendQuery(con, "SET foreign_key_checks = 1;")
    RMySQL::dbClearResult(rs)
}



