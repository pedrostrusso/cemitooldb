insert_study_info <- function(study_id, disease, keywords, platform, organism){
    con <- dbConnect(MySQL(),
                     user="prusso", 
                     password=getPass(),
                     dbname="cemitooldb",
                     host="127.0.0.1")
    on.exit(dbDisconnect(con))
    
    sql <- sprintf("INSERT INTO cemitool_run (studyID, Disease, Keywords, Platform, Organism) 
                   VALUES ('%s', '%s', '%s', '%s', '%s');",
                   study_id, disease, keywords, platform, organism)
    sql <- gsub(pattern='\\s', replacement="", x=sql)
    rs <- dbSendQuery(con, sql)
    dbClearResult(rs)
    id <- dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}

insert_run <- function(study_id, date, results, plots, reports){
    con <- dbConnect(MySQL(),
                     user="prusso", 
                     password=getPass(),
                     dbname="cemitooldb",
                     host="127.0.0.1")
    on.exit(dbDisconnect(con))
    
    sql <- sprintf("INSERT INTO cemitool_run (studyID, Date, Results, Plots, Reports) 
                   VALUES ('%s', '%s', '%s', '%s', '%s');",
                   study_id, date, results, plots, reports)
    sql <- gsub(pattern='\\n', replacement="", x=sql)
    rs <- dbSendQuery(con, sql)
    dbClearResult(rs)
    id <- dbGetQuery(con, "select last_insert_id();")[1,1]
    
    return(id)
}


