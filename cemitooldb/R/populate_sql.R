# "INSERT INTO study_info (study, disease, keywords, platform, organism) 
#                    VALUES ('%s', '%s', '%s', '%s', '%s');",

get_user_data <- function(user, dbname, host){
    password <- getPass::getPass()
    user_data <- list(user, password, dbname, host)
    names(user_data) <- c("user", "password", "dbname", "host")
    return(user_data)
}

populate_sql <- function(pool, gds_id, user_data, run_date, cem){
    
    # pool <- pool::dbPool(drv=RMySQL::MySQL(), 
    #                      dbname=user_data$dbname, 
    #                      host=user_data$host, 
    #                      user=user_data$user,
    #                      password=user_data$password)
    
    res <- dbGetQuery(pool, paste0("SELECT studyID FROM study_info WHERE study = '", gds_id, "';"))
    #study_id <- dbFetch(res)
    study_id <- res
    #dbClearResult(res)
    
    if(nrow(study_id) == 0){
        study_info <- scrape_study_info(gds_id)
        disease <- "NULL"
        
        insert_study_info(pool=pool, gds_id=gds_id, disease=disease,  
                          platform=study_info$platform, organism=study_info$organism, 
                          summary=study_info$summary, title=study_info$title, 
                          pubmedid=study_info$pubmedid)
        conn <- poolCheckout(pool)
        res <- pool::dbSendQuery(conn, paste0("SELECT studyID FROM study_info WHERE study = '", gds_id, "';"))
        poolReturn(conn)
        study_id <- dbFetch(res)
        dbClearResult(res)
        
        insert_keywords(pool=pool, study_id=study_id, keywords=study_info$keywords)
    }
    
    annot <- sample_annotation(cem)
    
    insert_sample_info(pool=pool, annot=annot, study_id=study_id)
    
    run_id <- insert_run(pool=pool, study_id=study_id, results_dir=results_dir, run_date=run_date)
    
    insert_modules(pool=pool, run_id=run_id, cem=cem)
}

insert_modules <- function(pool, run_id, cem){
    ora_df <- ora_data(cem)
    genes_df <- module_genes(cem)
    cem_adj <- adj_data(cem)
    gsea_df <- plyr::ldply(gsea_data(cem))
    gsea_df <- reshape2::melt(gsea_df, id.vars=c(".id", "pathway"))
    interactions <- interactions_data(cem)
    
    for(module_name in mod_names(cem)){
        message(module_name)
        sql <- sprintf("INSERT INTO modules (module_name, runID) 
                   VALUES ('%s', %s);",
                       module_name, run_id)
        sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
        
        conn <- pool::poolCheckout(pool)
        rs <- pool::dbSendQuery(conn, sql)
        poolReturn(conn)
        RMySQL::dbClearResult(rs)
        
        mod_id <- pool::dbGetQuery(pool, "select last_insert_id();")[1,1]
        
        ora_mod <- ora_df[ora_df$Module == module_name, ]
        message("...ORA")
        insert_ora(pool, mod_id, ora_mod)
        
        genes_mod <- genes_df[genes_df$modules == module_name, "genes"]
        message("...Module genes")
        insert_genes(pool, mod_id, genes_mod)
        
        adj_mod <- cem_adj[genes_mod, genes_mod]
        message("...Interactions")
        insert_interactions(pool, mod_id, adj_mod)

        if(module_name %in% unique(gsea_df$pathway)){
            gsea_mod <- gsea_df[gsea_df$pathway == module_name, ]
            message("...GSEA")
            insert_gsea(pool, mod_id, gsea_mod)
        }
        
    }
}

insert_ora <- function(pool, mod_id, ora_mod){
    pathway_name <- ora_mod[, "ID"]
    pathway_name <- gsub("'", "", pathway_name)
    pathway_name <- gsub('"', "", pathway_name)
    sql <- "INSERT INTO ora (moduleID, pathway_name, gene_ratio, 
                            bg_ratio, pvalue, padjust, qvalue, genes, count) VALUES "
    add_string <- paste(paste0("(", mod_id, ", '", pathway_name, "', '", 
                               ora_mod$GeneRatio, "', '",  ora_mod$BgRatio, "', '",
                               ora_mod$pvalue, "', '", ora_mod$p.adjust, "', '", ora_mod$qvalue, "', '",
                               ora_mod$geneID, "', '", ora_mod$Count, "')"), collapse=", ")
    sql <- paste0(sql, add_string, ";")    
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- pool::dbGetQuery(pool, sql)
}

insert_genes <- function(pool, mod_id, genes_mod){
    sql <- "INSERT INTO module_genes (moduleID, gene_symbol) VALUES "
    add_string <- paste(paste0("(", mod_id, ", '", genes_mod, "')"), collapse=", ")
    sql <- paste0(sql, add_string, ";")    
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    rs <- pool::dbGetQuery(pool, sql)
}

insert_gsea <- function(pool, mod_id, gsea_mod){
    for(row in seq_len(nrow(gsea_mod))){
        gsea_row <- gsea_mod[row, ]
        sql <- sprintf("INSERT INTO enrichment (moduleID, class, type, value) 
                   VALUES (%s, '%s', '%s', '%s');", 
                       mod_id, gsea_row$variable, gsea_row$.id, gsea_row$value)
        sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
        rs <- pool::dbGetQuery(pool, sql)
    }
}

insert_interactions <- function(pool, mod_id, adj_mod){
    edge_list <- reshape2::melt(as.matrix(adj_mod), id.vars=c("rownames", "colnames"))
    
    sql <- "INSERT INTO interactions (moduleID, gene1, gene2, value) VALUES"
    add_string <- paste(paste0("(", mod_id, ", '", 
                               edge_list$Var1, "', '", 
                               edge_list$Var2,"', ",
                               edge_list$value, ")"), collapse=", ")
    sql <- paste0(sql, add_string, ";")   
    sql <- gsub(pattern='\\s{2}', replacement="", x=sql)
    #rs <- RMySQL::dbSendQuery(con, sql)
    rs <- pool::dbGetQuery(pool, sql)
}



