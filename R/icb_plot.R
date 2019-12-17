riskplot.icb <- function(icb_object = NULL,
                         pred_object = NULL,
                         multiple = FALSE,
                         data_subset = NULL,
                         type = "table",
                         plot.which = "Loss",
                         data_names = NULL,
                         fcount = FALSE){
  
  
  if(isFALSE(multiple) & is.null(data_subset) & !is.null(icb_object)){
    print(stage_risk(micb_object = icb_object))
  }
  
  if(isFALSE(multiple) & is.null(data_subset) & !is.null(pred_object)){
    print(stage_risk(pred_object = pred))
  }
  
  if(isTRUE(multiple) & !is.null(icb_object)){
    print(data_risk_table(icb_list = icb_object, train = T, 
                    data_names = data_names))
  }
  
  if(isTRUE(multiple) & !is.null(pred_object)){
    print(data_risk_table(icb_list = pred_object, train = F, 
                    data_names = data_names))
  }
  
  if(!is.null(pred_object) & !is.null(data_subset) & type == "table"){
    print(individual_stage_risk(pred_object, subset = data_subset))
  }
  
  if(!is.null(pred_object) & !is.null(data_subset) & type == "barplot"){
    individual_barplot(pred_object, subset = data_subset, plot.which = plot.which)
  }
  
  if(type == "histogram"){
    plot.icb(micb_object = icb_object, predict_object = pred_object, fcount = fcount,
             data_name = data_names)
  }
    
    
  
  
}


featureplot.icb <- function(icb_object, type = "pdp", col = "black", feature = NULL,
                            data = NULL){
  
  if(type == "pdp"){
    pdp_function(icb_object = icb_object, col = col)
  }
  
  if(type == "ale"){
    library(checkmate)
    library(data.table)
    main_effect_plot(icb_object, data = data, feature = feature)
  }


}