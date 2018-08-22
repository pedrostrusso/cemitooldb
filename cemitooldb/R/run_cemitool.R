run_cemitool <- function(gds_id, expr, annot, results_dir, p){

    p$expr <- expr
    p$annot <- annot
    
    possible_error <- tryCatch(cem <- do.call(cemitool, p),
                               error=function(e) e)
    if(inherits(possible_error, "error")){
        if(!dir.exists(file.path(results_dir, gds_id))){
            dir.create(file.path(results_dir, gds_id))
        }
        cat(paste(gds_id, as.character(possible_error)), 
            file=file.path(results_dir, gds_id, "log.txt"), append=TRUE)
    }else{
        cem <- possible_error
        possible_error2 <- tryCatch({
            generate_report(cem, directory=file.path(results_dir, gds_id, "Reports"))
            write_files(cem, directory=file.path(results_dir, gds_id, "Tables"))
            save_plots(cem, directory=file.path(results_dir, gds_id, "Plots"))
        },
        error=function(e2) e2)
        if(inherits(possible_error2, "error")){
            if(!dir.exists(file.path(results_dir, gds_id))){
                dir.create(file.path(results_dir, gds_id))
            } 
            cat(paste(gds_id, as.character(possible_error2)), 
                file=file.path(results_dir, gds_id, "log.txt"), append=TRUE)
        }else{
            return(cem)
        }
    }
}
