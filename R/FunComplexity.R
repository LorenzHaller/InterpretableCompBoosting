# This code comes from 
# https://github.com/compstat-lmu/paper_2019_iml_measures/tree/master/R

# TODO
# Write tests
# Add some comments

# TODO when paper is done
# Document FunComplexity
# Profile code to see where bottleneck is

FunComplexity = R6::R6Class(
  "FunComplexity",
  inherit = iml::FeatureEffects,
  public = list(
    # user defined max. segments approx error
    epsilon = NULL,
    # The maximum complexity a feature can have
    max_seg_cat = NULL,
    max_seg_num = NULL,
    # How well the main effects model approximates total model
    r2 = NULL,
    # The mean complexity per ALE plot, weighted by its variance
    c_wmean = NULL,
    # Number of features used in predictor
    n_features = NULL,
    # The approximation models of the ALE plots
    approx_models = NULL,
    # Number of sample for estimating whether a feature was used
    m_nf = NULL,
    initialize = function(predictor, grid.size = 100, parallel = FALSE,
                          epsilon = 0.05, max_seg_cat = 5, max_seg_num = 5, m_nf = 500, post_process = TRUE) {
      if(predictor$task == "classification" & is.null(predictor$class)) {
        stop("Please set class in Predictor")
      }
      assert_numeric(epsilon, lower = 0, upper = 1, any.missing = FALSE, len = 1)
      assert_numeric(max_seg_cat, len = 1, any.missing = FALSE, upper = grid.size)
      assert_numeric(max_seg_num, len = 1, any.missing = FALSE, upper = grid.size)
      assert_numeric(m_nf, len = 1, any.missing = FALSE, lower = 1)
      self$max_seg_cat = max_seg_cat
      self$max_seg_num = max_seg_num
      self$epsilon = epsilon
      self$m_nf = m_nf
      super$initialize(predictor, features = predictor$data$feature.names,
                       method = "ale", grid.size = grid.size, center.at = NULL,
                       parallel = parallel)
      private$X = data.frame(self$predictor$data$get.x())
      private$mean_pred = mean(self$predictor$predict(private$X)[[1]])
      private$measure_r2_1st_ale()
      private$measure_non_linearities(post_process)
      self$n_features = sum(unlist(lapply(self$approx_models, function(x) x$feature_used)))
    },
    # 1st-order ALE model predictions
    predict = function(dat) {
      res = data.frame(lapply(self$effects, function(eff) {
        eff$predict(dat)
      }))
      rowSums(res) + private$mean_pred
    },
    # 1st-order ALE approximiation model predictions
    predict_approx = function(dat, features = NULL){
      if(is.null(features)) features = self$features
      res = data.frame(lapply(self$approx_models[features], function(mod) {
        mod$predict(dat)
      }))
      rowSums(res) + private$mean_pred
    },
    plot_complexity = function(feature) {
      self$approx_models[[feature]]$plot()
    }
    
  ),
  private = list(
    # Feature matrix
    X = NULL,
    # SST of black box model
    ssq_bb = NULL,
    # SSE of 1st-order ALE model
    ssq_1st_order_e = NULL,
    # The mean prediction of the black box predictor
    mean_pred = NULL,
    measure_r2_1st_ale = function(){
      if(is.null(private$multiClass) || !private$multiClass) {
        predictions = self$predictor$predict(private$X)[[1]]
        ale_predictions = self$predict(private$X)
        private$ssq_bb = ssq(predictions - private$mean_pred)
        if(private$ssq_bb == 0) {
          self$r2 = 1
        } else {
          private$ssq_1st_order_e = ssq(ale_predictions - predictions)
          self$r2 = 1 - private$ssq_1st_order_e/private$ssq_bb
        }
      } else {
        stop("Does not work for multiClass")
      }
    },
    measure_non_linearities = function(post_process){
      self$approx_models = lapply(self$effects, function(eff) {
        feature_name = eff$feature.name
        if(eff$feature.type == "numerical") {
          AleNumApprox$new(ale = eff, epsilon = self$epsilon, max_seg = self$max_seg_num, m_nf = self$m_nf, post_process)
        } else {
          AleCatApprox$new(ale = eff, epsilon = self$epsilon, max_seg = self$max_seg_cat, m_nf = self$m_nf)
        }
      })
      am_coefs = unlist(lapply(self$approx_models, function(x) x$n_coefs))
      am_weights = unlist(lapply(self$approx_models, function(x) x$var))
      self$c_wmean = weighted.mean(am_coefs, w = am_weights)
      if(all(am_coefs == 0)) self$c_wmean = 0
    },
    generatePlot = function(features = NULL, ncols = NULL, nrows = NULL, fixed_y = TRUE, del_zero=TRUE,...) {
      assert_character(features, null.ok = TRUE)
      if(length(features) > 0) {
        assert_true(all(features %in% self$features))
      } else {
        features = self$features
      }
      
      if(del_zero){
        features = features[sapply(self$approx_models, function(x) x$feature_used)]
      }
      
      # Compute size of gtable
      layout = iml:::get_layout(length(features), nrows, ncols)
      # Based on layout, infer which figures will be left and or bottom
      del_ylab_index = setdiff(1:length(features), 1:min(layout$nrows, length(features)))
      
      if(fixed_y) {
        res = unlist(lapply(features, function(fname){
          cname = ifelse(self$method == "ale", ".ale", ".y.hat")
          values = self$effects[[fname]]$results[cname][[1]]
          values = c(values, self$approx_models[[fname]]$approx_values)
          c(min(values), max(values))
        }))
        ylim = c(min(res), max(res))
      } else {
        ylim = c(NA, NA)
      }
      maxv = max(unlist(lapply(self$approx_models, function(x) x$var)))
      plts = lapply(features, function(fname) {
        gg = self$approx_models[[fname]]$plot(..., ylim = ylim, maxv = maxv) +
          theme(axis.title.y=element_blank())
        ggplotGrob(gg)
      })
      y_axis_label = self$effects[[1]]$.__enclos_env__$private$y_axis_label
      # Fill gtable with graphics
      ml = marrangeGrob(grobs = plts, nrow = layout$nrows, ncol = layout$ncols,
                        left = y_axis_label, top = sprintf("ALE main effects, R squared %.2f", self$r2))
      ml
    }
  )
)



ssq = function(x) {
  assert_numeric(x, any.missing = FALSE, min.len = 1)
  sum(x^2)
}



#' Approximate ALE curve
AleApprox = R6::R6Class("AleApprox",
                        public = list(
                          # ALE to be approximated
                          ale = NULL,
                          # R-squared of first order ale model
                          r2 = NULL,
                          # Number of coefficients
                          n_coefs = NULL,
                          # The maximum number of breaks allowed
                          max_breaks = NULL,
                          # Name of the feature
                          feature = NULL,
                          # Maximal allowed approximation error
                          epsilon = NULL,
                          # prediction function
                          predict = NULL,
                          # SST of ALE model
                          ssq_ale = NULL,
                          var = NULL,
                          shapley_var = NULL,
                          max_complex = FALSE,
                          feature_used = TRUE,
                          approx_values = NULL,
                          # Number of iterations used to estimate if feature was used
                          m_nf = NULL,
                          #' @param ale A FeatureEffet object
                          #' @param epsilon The allowed approximation error
                          #' @param max_breaks The maximum number of segments allowed
                          initialize = function(ale, epsilon, max_breaks, m_nf){
                            assert_class(ale, "FeatureEffect")
                            assert_numeric(epsilon, lower = 0, upper = 1, len = 1,
                                           any.missing = FALSE)
                            assert_numeric(max_breaks, len = 1)
                            assert_numeric(m_nf, len = 1, lower = 1)
                            self$ale = ale
                            self$epsilon = epsilon
                            self$max_breaks = max_breaks
                            self$feature = ale$feature.name
                            self$m_nf = m_nf
                            private$x = self$ale$predictor$data$X[, self$feature, with = FALSE][[1]]
                            private$ale_values = self$ale$predict(private$x)
                            self$ssq_ale  = ssq(private$ale_values)
                            # Variance of the ALE plot weighted by data density
                            self$var = self$ssq_ale  / length(private$x)
                          }
                        ),
                        private = list(
                          x = NULL,
                          ale_values = NULL,
                          is_null_ale = function() {
                            if(!feature_used(self$ale$predictor, self$feature, sample_size = self$m_nf)) {
                              self$r2 = 1
                              self$n_coefs = 0
                              self$predict = function(X) {
                                times = ifelse(is.data.frame(X), nrow(X), length(X))
                                rep(0, times = times)
                              }
                              self$feature_used = FALSE
                              self$approx_values = rep(0, times = self$ale$predictor$data$n.rows)
                              self$max_complex = FALSE
                              TRUE
                            } else {
                              FALSE
                            }
                          }
                        )
)

AleCatApprox = R6::R6Class(classname = "AleCatApprox",
                           inherit = AleApprox,
                           public = list(
                             # Table holding the level/new_level info
                             tab = NULL,
                             initialize = function(ale, epsilon, max_seg, m_nf) {
                               assert_true(all.equal(ale$feature.type,"categorical", check.attributes = FALSE))
                               super$initialize(ale, epsilon, max_breaks = max_seg, m_nf = m_nf)
                               if(!private$is_null_ale()) {
                                 self$approximate()
                                 self$n_coefs = ifelse(self$max_complex, max_seg - 1, length(unique(self$tab$lvl)) - 1)
                                 self$predict = function(dat){
                                   merge(dat, self$tab, by.x = self$feature, by.y = "x", sort = FALSE)[["pred_approx"]]
                                 }
                                 self$approx_values = self$predict(self$ale$predictor$data$get.x())
                                 ssq_approx_error = ssq(self$approx_values -  private$ale_values)
                                 self$r2 = 1 - ssq_approx_error / self$ssq_ale
                               }
                             },
                             approximate = function(){
                               x = private$x
                               # Create table with x, ale, n
                               df = data.table(ale =  private$ale_values, x = x)
                               df = df[,.(n = .N), by = list(ale, x)]
                               df$x = factor(df$x, self$ale$results[,self$feature])
                               df = df[order(df$x),]
                               max_breaks = min(self$max_breaks, nlevels(x) - 1)
                               for(n_breaks in 1:max_breaks) {
                                 BREAK_SAMPLE_SIZE = 30
                                 # keep splits from before and try all additional splits.
                                 splits = t(combn(1:(nlevels(x) - 1), n_breaks))
                                 
                                 if(nrow(splits) > BREAK_SAMPLE_SIZE) splits = splits[sample(1:nrow(splits), BREAK_SAMPLE_SIZE),,drop = FALSE]
                                 ssms = apply(splits, 1, function(splitx) {
                                   step_fn(as.numeric(splitx), df, ssq_ale = self$ssq_ale )
                                 })
                                 min_ssms = min(ssms)
                                 best_split_index = which(ssms == min_ssms)[1]
                                 pars = splits[best_split_index,]
                                 if(n_breaks == nlevels(x)) {
                                   pars = 1:(nlevels(x) - 1)
                                   break()
                                 }
                                 if(min_ssms <= self$epsilon)  break()
                               }
                               if(min_ssms > self$epsilon)  self$max_complex = TRUE
                               # Create table for predictions
                               breaks = unique(round(pars, 0))
                               df$lvl = cut(1:nrow(df), c(0, breaks, nrow(df)))
                               df_pred = df[,.(pred_approx = weighted.mean(ale, w = n)),by = lvl]
                               self$tab = merge(df, df_pred, by.x = "lvl", by.y = "lvl")
                             },
                             plot = function(ylim = c(NA,NA), maxv = NULL) {
                               assert_numeric(maxv, null.ok=TRUE)
                               dat = self$ale$predictor$data$get.x()
                               dat = unique(data.frame(x = dat[[self$feature]], y = self$approx_values))
                               max_string = ifelse(self$max_complex, "+", "")
                               varv = ifelse(is.null(maxv), self$var, self$var/maxv)
                               self$ale$plot(ylim = ylim) + geom_point(aes(x = x, y = y), data = dat, color = "red", size = 2) +
                                 ggtitle(sprintf("C: %i%s, R2: %.3f, V: %.3f", self$n_coefs, max_string, self$r2, varv))
                             }
                           )
)


#' Compute fit of step approximation
#'
#' @param par cutoff points
#' @param dat data.frame with columns ale and n, number of instances
#' @param ssq_ale sum of squares for ALE
#' @return sum of squared errors
step_fn = function(par, dat, ssq_ale){
  expect_data_table(dat, any.missing = FALSE)
  breaks = unique(round(par, 0))
  dat$lvl = cut(1:nrow(dat), unique(c(0, breaks, nrow(dat))))
  dat2 = dat[, .(ale_mean = weighted.mean(ale, w = n), n = sum(n)), by = lvl]
  # ALE plots have mean zero
  ssq_approx = sum( (dat2$ale_mean) ^ 2 * dat2$n)
  1 - (ssq_approx / ssq_ale)
}

AleNumApprox = R6::R6Class(classname = "AleNumApprox",
                           inherit = AleApprox,
                           public = list(
                             # Table holding the level/new_level info
                             model = NULL,
                             breaks = NULL,
                             # Table for intervals with intercept and slope
                             segments = NULL,
                             initialize = function(ale, epsilon, max_seg, m_nf = 200, post_process = TRUE) {
                               assert_true(all.equal(ale$feature.type, "numerical", check.attributes = FALSE))
                               assert_numeric(max_seg)
                               # only makes
                               max_breaks = max_seg  - 1
                               super$initialize(ale, epsilon, max_breaks, m_nf = m_nf)
                               if(!private$is_null_ale()) {
                                 self$approximate(post_process)
                                 # Don't count the intercept
                                 n_coefs = nrow(self$segments) + sum(self$segments$slope != 0) - 1
                                 self$n_coefs = min(max_seg * 2, n_coefs)
                                 self$predict = function(dat) {
                                   if(is.data.frame(dat)) {
                                     x = dat[[self$feature]]
                                   } else {
                                     x = dat
                                   }
                                   x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
                                   dat = data.table(x, interval = x_interval)
                                   mx = merge(dat, self$segments, by.x = "interval", by.y = "interval", sort = FALSE)
                                   mx$intercept + mx$slope * mx$x
                                 }
                                 self$approx_values = self$predict(self$ale$predictor$data$get.x())
                                 ssq_approx_error = ssq(self$approx_values -  private$ale_values)
                                 self$r2 = 1 - ssq_approx_error / self$ssq_ale
                               }
                             },
                             approximate = function(post_process){
                               x = private$x
                               # test 0 breaks
                               mod = lm(private$ale_values ~ x)
                               ssq_approx_error = ssq(private$ale_values - predict(mod))
                               if( self$ssq_ale  == 0 || (ssq_approx_error/self$ssq_ale ) < self$epsilon) {
                                 self$r2 = get_r2(predict(mod), private$ale_values)
                                 self$approx_values = predict(mod)
                                 model = mod
                                 self$breaks = c(min(x), max(x))
                                 x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
                                 self$segments = extract_segments(model, self$breaks, levels(x_interval))
                                 return()
                               }
                               pars = c()
                               lower = as.numeric(min(x))
                               upper = as.numeric(max(x))
                               ale_breaks = self$ale$results[[self$ale$feature.name]]
                               for( n_breaks in 1:self$max_breaks) {
                                 #init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
                                 #init_breaks = as.numeric(median(x))
                                 opt = lapply(ale_breaks, segment_fn, ale = self$ale,
                                              ssq_ale = self$ssq_ale, x = x,
                                              ale_prediction = private$ale_values,
                                              prev_breaks = pars)
                                 
                                 #opt_gensa = optim(par = init_breaks, segment_fn, lower = lower,
                                 #		  upper = upper, ale = self$ale,
                                 #		  ssq_ale = self$ssq_ale, x = x,
                                 #		  ale_prediction = private$ale_values,
                                 #		  prev_breaks = pars, method = "Brent")
                                 opt = unlist(opt)
                                 min.opt = which.min(opt)[[1]]
                                 pars = c(pars, ale_breaks[min.opt])
                                 #pars = opt_gensa$par
                                 vv = opt[min.opt]
                                 if (vv <= self$epsilon)  break()
                               }
                               if (vv > self$epsilon)  self$max_complex = TRUE
                               # fit lm with par as cut points
                               self$breaks = sort(unique(c(min(x), pars, max(x))))
                               x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
                               dat = data.frame(x = x, interval = x_interval, ale = private$ale_values)
                               model = lm(ale ~ x * interval, data = dat)
                               segments = extract_segments(model, self$breaks, levels(x_interval))
                               if (post_process) {
                                 self$segments = eliminate_slopes(segments, x, private$ale_values,
                                                                  self$epsilon, self$breaks)
                               } else {
                                 self$segments = segments
                               }
                             },
                             plot = function(ylim = c(NA, NA), maxv = NULL) {
                               assert_numeric(maxv, null.ok = TRUE)
                               fdat = self$ale$predictor$data$get.x()[[self$feature]]
                               x = seq(from = min(fdat), to = max(fdat), length.out = 200)
                               y = self$predict(x)
                               intervals = cut(x, breaks = self$breaks, include.lowest = TRUE)
                               dat = data.frame(x = x, y = y, interval = intervals)
                               max_string = ifelse(self$max_complex, "+", "")
                               varv = ifelse(is.null(maxv), self$var, self$var/maxv)
                               p = self$ale$plot(ylim = ylim) +
                                 geom_line(aes(x = x, y = y, group = interval), color = "red",
                                           data = dat, lty = 2) +
                                 ggtitle(sprintf("C: %i%s, R2: %.3f, V: %.3f", self$n_coefs, max_string,self$r2, varv))
                               if(length(self$breaks) > 2) {
                                 breaks = self$breaks[2:(length(self$breaks) - 1)]
                                 p = p + geom_vline(data = data.frame(breaks = self$breaks), aes(xintercept = self$breaks))
                               }
                               p
                             }
                           )
)


#' Function to optimize for ALE approx
#'
#' @param par The breakpoints
segment_fn = function(par, ale, ssq_ale, x, ale_prediction, prev_breaks){
  breaks = unique(c(min(x), par, prev_breaks, max(x)))
  x_interval = cut(x, breaks =  breaks,  include.lowest = TRUE)
  dat = data.table(xv = x, interval = x_interval, alev = ale_prediction)
  res = dat[, .(ssq(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals)), by = interval]
  error = sum(res$V1)/ssq_ale
  return(error)
}


# TODO: Test
feature_used = function(pred, feature, sample_size = 500){
  dat = pred$data$get.x()
  fvalues = dat[, ..feature][[1]]
  # permute feature
  dat2 = dat[sample(1:nrow(dat), size = sample_size, replace = TRUE)]
  prediction1 = pred$predict(dat2)
  sampled_fvalues = sapply(dat2[,..feature][[1]], function(x){
    sample(setdiff(fvalues, x), size = 1)
  })
  dat2 = dat2[, (feature) := sampled_fvalues]
  prediction2 = pred$predict(dat2)
  if (any(( prediction1 - prediction2) != 0)) return(TRUE)
  FALSE
}


# Extract the slope information
extract_segments = function(model, breaks, intervals, feature.cname = "x") {
  assert_class(model, "lm")
  stopifnot(!any(duplicated(breaks)))
  cfs = coef(model)
  coef_slope = cfs[grep(feature.cname, names(cfs))]
  coef_intercept = cfs[setdiff(names(cfs), names(coef_slope))]
  if(length(cfs) == 2) {
    data.frame(
      xstart = breaks[1],
      xend = breaks[2],
      intercept = coef_intercept,
      slope = coef_slope,
      interval = intervals
    )
  } else {
    coef_slope[2:length(coef_slope)] =  coef_slope[1] + coef_slope[2:length(coef_slope)]
    coef_intercept[2:length(coef_slope)] =  coef_intercept[1] + coef_intercept[2:length(coef_intercept)]
    data.frame(
      xstart = breaks[1:(length(breaks) - 1)],
      xend = breaks[2:length(breaks)],
      intercept = coef_intercept,
      slope = coef_slope,
      interval = intervals
    )
  }
}


eliminate_slopes = function(segments, x, ale_values, epsilon, breaks){
  if(nrow(segments) == 1) return(segments)
  # order of slopes by increasing absolute slope
  
  # can happen that segment slope is NA because only one point is in there
  segments$slope[is.na(segments$slope)] = 0
  
  x_interval = cut(x, breaks = breaks, include.lowest = TRUE)
  dat = data.frame(x, interval = x_interval)
  
  pr = function(segs) {
    mx = merge(data.table(dat), segs, by.x = "interval", by.y = "interval", sort = FALSE)
    mx$intercept + mx$slope * mx$x
  }
  
  slope_order = order(abs(segments$slope))
  for (i in slope_order) {
    segments_new = segments
    segments_new[i, "slope"] = 0
    new_intercept = mean(ale_values[dat$interval == segments_new$interval[i]])
    segments_new[i, "intercept"] = new_intercept
    stopifnot(!any(is.na(pr(segments_new))))
    stopifnot(!any(is.na( ale_values)))
    if (get_r2(pr(segments_new), ale.values = ale_values) < epsilon) {
      segments = segments_new
    }
  }
  segments
}

# TODO: Test
get_r2 = function(seg.predictions, ale.values) {
  SST = ssq(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = ssq(seg.predictions - ale.values)
  SSE / SST
}



#' Draw sparkline for ALE or ALEApprox
#'
#' TODO: Create version for categorical features
#'
#' @param obj Either a AleNumApprox or a FeatureEffect
#' @param ylim The y limits, can be set to NA
#' @param approx Should the approximation be plotted instead of real ALE curve?
#' @param ... Further arguments for ltxsparklines::sparline
spark  = function(obj, ylim = c(NA, NA), approx = FALSE, color = "black", height = 2,...){
  assert_numeric(ylim, len = 2, null.ok = TRUE)
  assert_multi_class(obj, c("AleNumApprox", "FeatureEffect"))
  
  if(inherits(obj, "AleApprox")) {
    # removes intervals at min and max
    xspikes = obj$breaks[-c(1,length(obj$breaks))]
    ale = obj$ale
    feature = ale$feature.name
    x = ale$results[,feature]
    y = ale$results$.ale
    # min and max should remain same as ALE, even if approx is plotted
    ymin = ifelse(is.na(ylim[1]), min(ale$results$.ale), ylim[1])
    ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
    if(approx) {
      N_POINTS = 50
      true.x = obj$.__enclos_env__$private$x
      x = seq(from = min(true.x), to = max(true.x), length.out = N_POINTS)
      y = obj$predict(x)
      yspikes = rep(ymax, times = length(xspikes))
      color = "gray"
    } else {
      yspikes = rep(ymax, times = length(xspikes))
    }
  } else {
    ale = obj
    feature = ale$feature.name
    x = ale$results[,feature]
    y = ale$results$.ale
    ymin = ifelse(is.na(ylim[1]), min(ale$results$.ale), ylim[1])
    ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
    xspikes = NULL
    yspikes = NULL
  }
  stopifnot(!any(is.na(c(x,y))))
  sparkline_string = sparkline(x = x, y = y,
                               xspikes = xspikes, yspikes = yspikes, ylim = c(ymin, ymax),...)
  sprintf("{\\renewcommand{\\sparklineheight}{%s}\\definecolor{sparklinecolor}{named}{%s}%s}",
          height, color, sparkline_string)
}



#' Compute model summary
#'
#' @param pred Predictor
#' @param ylim the y-axis limits for the sparklines
#' @return character vector with NF, IA, AMEC and sparklines for all features
get_spark_col = function(pred, ylim = c(NA, NA), width = 5, ...) {
  assert_class(pred, "Predictor")
  assert_numeric(ylim, len = 2)
  # a bit smoother for plotting
  fc = FunComplexity$new(pred, grid.size = 50, epsilon = 0.05)
  sparklns = sapply(fc$effects, function(eff) {
    if(all(eff$results$.ale == 0)) {
      ""
    } else {
      spark(eff, width = width, ylim = ylim, ...)
    }
  })
  as.character(sparklns)
}

#' Compute model summaries for subset off pareto set
#'
#' @param paretor_set the mbo pareto set
#' @param indices the subset indices for which to compute the summaries
#' @param ylim the y-axis limits for the sparklines
#' @return data.frame with NF, IA, AMEC and sparklines for all features. columns are models
get_spark_table = function(mbo_obj, indices, ylim = c(NA, NA), log_params,...) {
  assert_class(mbo_obj, "MBOMultiObjResult")
  assert_numeric(indices, any.missing = FALSE)
  assert_numeric(ylim, len = 2)
  pareto_set = mbo_obj$pareto.set
  pareto_front = mbo_obj$pareto.front
  res = lapply(indices, function(i){
    pp = pareto_set[[i]]
    pp = pp[!is.na(pp)]
    lparams = intersect(names(pp), log_params)
    if(length(lparams) > 0) pp[lparams] = lapply(pp[lparams], function(x) 2^x)
    lrn = setHyperPars(lrn.regr, par.vals = pp)
    mod = train(lrn, task)
    pred = Predictor$new(mod, task.dat)
    c(pareto_front[i, "MAE"], pareto_front[i, "MEC"], pareto_front[i, "IAS"], pareto_front[i, "NF"],
      unlist(get_spark_col(pred, ylim = ylim, ...)))
  })
  data.frame(res)
}