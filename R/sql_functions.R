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
insert_study_info <- function(study, disease, keywords, platform, organism, 
                              user, password, dbname, host="127.0.0.1"){
    con <- RMySQL::dbConnect(RMySQL::MySQL(),
                     user=user, 
                     password=password,
                     dbname=dbname,
                     host=host)
    on.exit(RMySQL::dbDisconnect(con))
    
    sql <- sprintf("INSERT INTO study_info (study, disease, keywords, platform, organism) 
                   VALUES ('%s', '%s', '%s', '%s', '%s');",
                   study, disease, keywords, platform, organism)
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
    id <- DBI::dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
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
insert_run <- function(study_id, date, results, plots, reports){
    con <- RMySQL::dbConnect(MySQL(),
                     user="prusso", 
                     password=getPass(),
                     dbname="cemitooldb2",
                     host="127.0.0.1")
    on.exit(RMySQL::dbDisconnect(con))
    
    sql <- sprintf("INSERT INTO cemitool_run (studyID, Date, Results, Plots, Reports) 
                   VALUES ('%s', '%s', '%s', '%s', '%s');",
                   study_id, date, results, plots, reports)
    sql <- gsub(pattern='\\n', replacement="", x=sql)
    rs <- RMySQL::dbSendQuery(con, sql)
    RMySQL::dbClearResult(rs)
    id <- DBI::dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}


