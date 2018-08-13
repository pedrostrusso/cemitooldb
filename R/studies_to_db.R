gpl_id <- "GPL570"
user_data <- get_user_data("prusso", "cemitooldb2", "127.0.0.1")
gmt_fname <- system.file("extdata", "pathways.gmt", package = "CEMiTool")
gmt_in <- CEMiTool::read_gmt(gmt_fname)
## Get example interactions file
int_df <- read.delim(system.file("extdata", "interactions.tsv", package = "CEMiTool"))

studies_to_db <- function(user_data,
                          gpl_id, 
                          min_sample_num=18, max_sample_num=200, 
                          results_dir="/home/prusso/Documents/CSBL/new_database/intermediate/CEMiTool"){
    gds_studies <- get_gpl_related_gds(gpl_id, min_sample_num=min_sample_num, 
                                       max_sample_num=max_sample_num, ...)
    
    for(gds_id in gds_studies){
        possible_error <- tryCatch(gds <- get_gds(gds_id),
                                   error=function(e) e)
        if(inherits(possible_error, "error")){
            message("Error in GEOquery")
            next
        }else{
            gds <- possible_error
        }
           
        expr <- get_expr_from_gds(gds)
        
        sample_num <- ncol(expr) - 1
        if(sample_num > max_sample_num | sample_num < min_sample_num) {
            message(paste("Sample size for study", gds_id, 
                          "is out of the range defined by max_sample_num and min_sample_num"))
            next
        }
         
        expr <- process_and_collapse(expr)
        annot <- make_gds_annot(gds)
        #names_list[[gds_id]] <- annot
        
        run_cemitool(gds_id, expr, annot, results_dir, ...)
        
        populate_sql(gds_id, user_data)
    }
}
