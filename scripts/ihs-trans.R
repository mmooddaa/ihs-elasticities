ihsElasticity <- function (model, 
                           explanatory_var, 
                           type,
                           data,
                           conf_level = .95,
                           iv = FALSE) {
  
  if (class(model) == 'fixest') {
    data <- data[obs(model), ]  
    outcome_var <- as.character(model$fml[[2]])
  } else if (class(model) %in% c('lm', 'lm_robust')) {
    data <- model.frame(model)
    outcome_var <- as.character(model$terms[[2]])
  } else {
    stop('Only "fixest", "lm_robust", and "lm" model types are supported.')
  }
  
  
  if (iv) {
    explanatory_var_name <- paste0('fit_', explanatory_var)  
  } else {
    explanatory_var_name <- explanatory_var
  }
  
  if (type == 'linear-arcsinh') {
    form <- paste0('(', explanatory_var_name ,'/ ', mean( data[[outcome_var]] ), 
                   ') * (',
                   mean( sinh(data[[explanatory_var]]) ), 
                   ' / ', 
                   sqrt( mean( sinh(data[[explanatory_var]]) )^2 + 1 ),
                   ') = 0') 
  } else if (type == 'arcsinh-linear') {
    form <- paste0('(', explanatory_var_name ,' * ', mean(data[[explanatory_var]]), 
                   ') * (', 
                   sqrt(mean( sinh(data[[outcome_var]]) )^2 + 1),
                   ' / ', 
                   mean( sinh(data[[outcome_var]]) ),
                   ') = 0') 
  } else if (type == 'arcsinh-linear-dummy-exact') {
    y_hat_mean <- mean( predict(m_ihs) )
    
    form <- paste0( '(',
                    sinh(y_hat_mean), 
                    ' / sinh(', 
                    y_hat_mean,
                    '-', explanatory_var_name, ')',
                    ' - 1',
                    ') = 0'
    ) # end paste0 
  } else if (type == 'arcsinh-linear-dummy-approx') {
    var_b <- model$se[explanatory_var]^2 * .5
    form <- paste0('exp(', explanatory_var_name,' - ', var_b, ') - 1 = 0')
    
  } else if (type == 'arcsinh-arcsinh') {
    form <- paste0(explanatory_var_name ,' * (', 
                   ' sqrt(', mean( sinh(data[[outcome_var]]) )^2, '+ 1 ) / ', 
                   mean( sinh(data[[outcome_var]]) ), ') * (',
                   mean( sinh(data[[explanatory_var]]) ), ' / ',
                   ' sqrt(', mean( sinh(data[[explanatory_var]]) )^2, ' + 1)',
                   ') = 0'
    ) # end paste0 
  } else {
    return(stop('Incompatible specification type given. Must be either "arcsinh-linear", "arcsinh-linear-dummy-exact", "arcsinh-linear-dummy-approx", "linear-arcsinh", or "arcsinh-arcsinh".'))
  }
  
  
  return(hypotheses(model = model, 
                    hypothesis = form, 
                    conf_level = conf_level))
}
