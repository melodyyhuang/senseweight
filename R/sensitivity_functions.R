#' Robustness Value
#'
#' Returns the estimated robustness value, for a specified proportion change
#' @param b_star Threshold that corresponds to a substantively meaningful change to the research conclusion. For example, a value of 0 denotes that the bias from omitting a variable was sufficiently large to change the estimate to zero. 
#' @param estimate Weighted estimate
#' @param sigma2 Estimated variance of the outcome (i.e., var(Y) for obervational setting; var(tau) for generalization setting)
#' @param weights Vector of estimated weights
#' @return Robustness value for a specified proportion change
#' @export
robustness_value<-function(estimate, b_star = 0, sigma2, weights){
    a = (estimate-b_star)^2/(sigma2*var(weights))
    RV_est = (sqrt(a^2 + 4*a) - a)/2
    names(RV_est) = NULL
    return(RV_est)
}


#' Formal benchmarking for sensitivity parameters
#'
#' Returns parameter estimates for an omitted variable with specified relative confounding strength to an observed covariate (or set of covariates)
#' @param estimate Weighted estimate
#' @param weights Vector of estimated weights
#' @param weights_benchmark Vector of estimated weights, omitting the covariate (or set of covariates) being used to benchmark
#' @param k_sigma Relative ability of omitted confounder to explain variation in the true weights. If \code{k_sigma > 1}, then we expect the omitted confounder to explain more variation in the true weights than the benchmarked covariate(s). If \code{k_sigma < 1}, then we expect the omitted confounder to explain less of the variation in the true weights than the benchmarked covariate(s). Default is set to 1.
#' @param k_rho Relative correlation of omitted confounder with the outcome. If \code{k_rho > 1}, then we expect the omitted confounder to be more correlated with the outcome than the benchmarked covariate(s). If \code{k_rho < 1}, then we expect the omitted confounder to be less correlated with the outcome than the benchmarked covariate(s). Default is set to 1.
#' @param Y Vector of outcomes 
#' @param Z Vector of treatment assignment (Only necessary if \code{estimand = "PATE"} in order to estimate covariances)
#' @param sigma2 Estimated variance of the outcome (i.e., var(Y) for obervational setting; var(tau) for generalization setting)
#' @param estimand String specifying the estimand of interest. Valid inputs are "PATE", "Augmented", "ATT", or "Survey". 
#' @return \code{data.frame} containing estimated parameter values for a confounder with specified relative confounder strength to an observed covariate (or set of covariates), as well as the estimated bias from such an omitted confounder. 
#' @export
benchmark_parameters<-function(weights, weights_benchmark, k_sigma = 1, k_rho = 1, 
                               Y, Z, sigma2, estimand="ATT"){
    #Estimate informal calibrated components
    eps_benchmark = weights_benchmark - weights
    R2 =  var(eps_benchmark)/var(weights)
    if(estimand == "PATE"){
        cov_w_calib = cov(Y[Z==1], weights_benchmark[Z==1])-
                      cov(Y[Z==0], weights_benchmark[Z==0])
        cov_w = cov(Y[Z==1], weights[Z==1])-cov(Y[Z==0], weights[Z==0])
        cov_benchmark = cov_w_calib - cov_w
        rho = cov_benchmark/(sqrt(sigma2*var(eps_benchmark)))
    }else{
        if(!estimand %in% c("Augmented", "ATT", "Survey")){
            return("Invalid Estimand. Estimand must be ATT, PATE, or Survey.")
        }
        #Estimand is ATT or Survey:
        rho = cor(Y, eps_benchmark)
    }

    #Calculate parameters:
    R2_benchmark = k_sigma*R2/(1+k_sigma*R2)
    rho_benchmark = k_rho*rho

    if(abs(rho_benchmark) > 1){
        return("Infeasible correlation value obtained.")
    }
    #Expected bias:
    bias = rho_benchmark*sqrt(var(weights)*R2_benchmark/(1-R2_benchmark))*sqrt(sigma2)

    return(data.frame(R2_benchmark, rho_benchmark, bias))
}


#' Bias Contour Plots
#'
#' Generates bias contour plots to aid with sensitivity analysis 
#' @param varW Variance of the estimated weights 
#' @param sigma2 Estimated variance of the outcome (i.e., var(Y) for obervational setting; var(tau) for generalization setting)#' 
#' @param killer_confounder Threshold for bias considered large enough to be a killer confounder. For example, if researchers are concerned about the bias large enough to reduce an estimated treatment effect to zero or change directional sign, set \code{killer_confounder} equal to the point estimate.
#' @param df_benchmark Data frame containing formal benchmarking results. The data.frame  must contain the columns \code{variable} (for the covariate name), \code{R2_benchmark}, and \code{rho_benchmark}. 
#' @param benchmark Flag for whether or not to display benchmarking results (\code{benchmark = TRUE} if we want to add benchmarking results to plot, \code{benchamrk=FALSE} otherwise). If set to \code{TRUE}, \code{df_benchmark} must contain valid benchmarking results.
#' @param shade Flag for whether or not a specific benchmarking covariate (or set of benchmarked covariates) should be shaded a different color (\code{shade = TRUE} indicates that we want to highlight specific variables)
#' @param shade_var If \code{shade = TRUE}, this contains either a vector containing the variables we want to highlight 
#' @param print If set to \code{TRUE}, the function will return a list with two elements: \code{plot} which contains the generated bias contour plot, and \code{data}, which provides the data.frame for generating the contour plot. If set to \code{FALSE}, the function will simply generate the bias contour plot. Default is set to \code{FALSE}. 
#' @return A ggplot2 object, containing the bias contour plot
#' @export
contour_plot<-function(varW, sigma2, killer_confounder, df_benchmark, benchmark=TRUE,
                       shade=FALSE, shade_var = NULL, print=FALSE, binwidth=NULL){
    r2_vals <- seq(0.0, 0.95, by=0.05)
    rho_vals<-seq(-1, 1, by=0.05)
    data.fit <-  expand.grid(rho_vals, r2_vals)
    names(data.fit) = c("rho", "r2")
    bias = data.fit$rho*sqrt(varW*(data.fit$r2/(1-data.fit$r2)))*sqrt(sigma2)
    df_plot = data.frame(R2=data.fit$r2, rho = data.fit$rho, bias = bias)

    
    if(benchmark==TRUE){
        df_benchmark_plot = df_benchmark
        df_benchmark_plot$rho_benchmark[abs(df_benchmark_plot$rho_benchmark) > 1] = NA
    }

    p1 = df_plot %>%
        ggplot(aes(x = R2, y = rho, z = bias)) +
        geom_contour(col="black") + ggtitle("")+xlab(expression(R[epsilon]^2))+
        ylab(expression(rho[epsilon*","*tau]))+metR::geom_text_contour(aes(z = bias), stroke=0.2)

      if(!is.null(binwidth)){
      p1 = df_plot %>%
        ggplot(aes(x = R2, y = rho, z = bias)) +
        geom_contour(col="black", binwidth=binwidth) + ggtitle("")+xlab(expression(R[epsilon]^2))+
        ylab(expression(rho[epsilon*","*tau]))+metR::geom_text_contour(aes(z = bias), stroke=0.2, binwidth=binwidth)
    }
    if(!is.na(killer_confounder)){
        p1 = p1 + metR::geom_contour_fill(breaks = c(killer_confounder, 1000*killer_confounder), fill='blue', alpha=0.25)+
        geom_contour(breaks=c(killer_confounder), col='blue', size=1)
    }

    if(benchmark == TRUE){
        if(shade==TRUE){
            p1 = p1+geom_point(data = df_benchmark, aes(x = R2_benchmark, y = rho_benchmark,
                color=(variable %in% shade_var))) +
                ggrepel::geom_label_repel(data = df_benchmark, aes(x=R2_benchmark, y = rho_benchmark, label = variable,
                color=(variable %in% shade_var)), nudge_y = 0.05, fill='white')+
                scale_colour_manual(values=c("slategray", "black"))+theme(legend.position='none')
        }else{
            p1 = p1+geom_point(data = df_benchmark, aes(x = R2_benchmark, y = rho_benchmark)) +
                ggrepel::geom_label_repel(data = df_benchmark, aes(x=R2_benchmark, y = rho_benchmark, label = variable),
                        nudge_y = 0.05)
        }
    }
    if(print == TRUE){
        return(list(plot = p1, data = df_plot))
    }else{
        return(p1)
    }
}


#' Extreme Scenario Plots
#'
#' Generates extreme scenario plots, with varying thresholds for the correlation between the true weights and the outcomes
#' @param rho_w Correlation between the estimated weights and the outcomes
#' @param sigma2 Estimated variance of the outcome (i.e., var(Y) for obervational setting; var(tau) for generalization setting)#' 
#' @param estimate Weighted estimate
#' @param correlations A vector containing possible correlation values between the true weights and the outcomes 
#' @return A ggplot2 object, genearting an extreme scenario analysis 
#' @export
extreme_scenario_plot<-function(rho_w, sigma2, estimate, correlations = c(0.25, 0.5, 0.9, 1)){
    df_plot = calculate_extreme_scenario(rho_w, sigma2, correlations)
    p1 = df_plot[which(estimate - df_plot$bias >= 0),] %>%
        ggplot(aes(x = R2_vals, y=estimate-bias, label=label,
            group=type, linetype=as.factor(abs(type))))+geom_line()+
        geom_hline(yintercept = 0, alpha=0.5)+geom_text(nudge_x = 0.035, size=3)+
        theme(legend.position='none')+
        geom_line(data=df_plot[which(estimate - df_plot$bias < 0),],
            aes(x = R2_vals, y = estimate-bias,
            group=type, linetype=as.factor(abs(type))), color='red')+
        geom_text(data=df_plot[which(estimate - df_plot$bias < 0),], aes(label = label),
            color='black', nudge_x = 0.035, size=3)+
        xlab(expression(R[epsilon]^2))+
        ylab("Adjusted Point Estimate (Estimate - Est. Bias)")+
        ggtitle("Weighted Estimator")+ylim(-25, 25)
    return(p1)
}