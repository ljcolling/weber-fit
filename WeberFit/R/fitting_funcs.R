# This contains the main fitting functions

# Class definition

#setOldClass("nls")
#wFit<-setClass("wFit",
#               slots = list(
#                 accuracy = "numeric",
#                 fit = "nls",
#                 df = "data.frame",
#                 w = "numeric",
#                 fitinfo = "list",
#                 modeltype = "character"
#               ))
#


wFit<-setClass("wFit",
               slots = list(
                 data = "list"
               ))


# show method

setMethod("show",
          "wFit",
          function(object) {
            cat("Object of class",class(object),"(Weber Fit) \n")
            cat("Model type:", object@data$modeltype,"\n")
            cat("Accuracy:", object@data$accuracy,"\n")
            cat("Weber fraction:", object@data$w)
          })


setMethod("$", signature = "wFit",
          function(x, name) {
            returnval = x@data[[name]]
            names(returnval) = name
            return(returnval)}
)


setMethod("names", signature = "wFit",
          function(x) { return(names(x@data)) }
)



GeneratePlotData <- function(w,x) 0.5 * (1 + VGAM::erf((x - 1) / (sqrt(2) * w * sqrt( (x^2) + 1))))

#setMethod("$",
#          "wFit",
#          function(x,name){
#            if(name == "w"){
#              returnval = x@w
#              names(returnval) = "w"
#              return(returnval)
#            }
#            if(name == "accuracy"){
#              returnval = x@accuracy
#              names(returnval) = "accuracy"
#              return(returnval)
#            }
#          })
#
# function that does the actual fitting

FitLinearModel<-function(ratio, outcomes, start.values = c(1,4), lower.limit = .55){

  `%>%` = tidyr::`%>%`

  # calculate the accuracy

  df = data.frame(x = ratio, y = outcomes)

  accuracy = df %>% dplyr::mutate(acc = dplyr::case_when(x < 1 ~ 1 - y, x > 1 ~ y)) %>% dplyr::pull(acc) %>% mean(na.rm = T)


  fit = 'No Fit'
  class(fit) <- "nls"
  range = NULL
  vari = NULL
  w = NA_real_
  if(accuracy > lower.limit && accuracy < 1){
    fits <- purrr::map(start.values, function(s) try(expr = {
      nls(formula = y ~ 0.5 * (1 + VGAM::erf((x - 1) / (sqrt(2) * w * sqrt( (x^2) + 1)))), start = c(w = s), trace = F,
          data = df, control = list(maxiter = 50000))
    },silent = T))

    fits <- fits[purrr::map_lgl(fits, function(x) length(x) > 1)]

    fit <- fits[[which.min(purrr::map_dbl(fits, function(x) deviance(x)))]] # take the one that fits best
    range = purrr::map_dbl(fits, function(x) coef(x)) %>% range() %>% diff()
    vari = purrr::map_dbl(fits, function(x) coef(x)) %>% sd()
    w = coef(fit)
  }

  new(Class = 'wFit',
      data = list(accuracy = accuracy,
                  fit = fit,
                  df = df,
                  w = w,
                  fitinfo = list(range = range,
                     variability = vari,
                     lower.limit = lower.limit,
                     start.values = start.values),
                  modeltype = "linear model"))




}
