## This script solves the varius pricing problems related to teh U2 Vertigo Case

##Define Demand Functions for Members and General Public
Demand_M <- function(p) max(0,10000-125*p)  #Members
Demand_G <- function(p) max(0,40000-200*p)  #General Public
Demand_T <- function(p) Demand_M(p)+Demand_G(p) #Total Demand

####Part I) Optimal Single Price ##############

  #Define Revenue (objective function). We must define the negative of the the Revenue function
  #because the solver minimizes the objective function
  Neg_Revenue_I <- function(p) -Demand_T(p)*p 
  
  #Define Available Capacity (number of seats)
  Cap <- 20000 #we assume 30 cabins 
  
  #Define capacity constraint. 
  Cap_Constr_I <- function(p) Demand_T(p)-Cap
  
  #Initial price needed by the solver
  p0_I <- 50
  
  #Define Optimization Engine and Error Tolerance
  Opt <- list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-8)
  
  ##Load the "nloptr" engine
  #install.packages("nloptr") #Use this command only if you have not yet installed the optimzation package "nloptr" 
  library("nloptr")
  
  ##Run the Optimization
  res <- nloptr(x0 = p0_I, eval_f = Neg_Revenue_I, eval_g_ineq = Cap_Constr_I, opts = Opt)
  OptPrice_I=res$solution #Optimal
  OptRevenue_I=-res$objective #Maximum Revenue
  
  
  
#### Part II) Optimal Differentiated Prices ##############
  #Define Revenue Functions
  Neg_Revenue_II <- function(p) -Demand_M(p[1])*p[1]-Demand_G(p[2])*p[2] 
  
  #Define capacity constraint. 
  Cap_Constr_II <- function(p) Demand_M(p[1])+Demand_G(p[2])-Cap
  
  #Initial price needed by the solver
  p0_II <- c(50,50)
  
  ##Run the Optimization
  res <- nloptr(x0 = p0_II, eval_f = Neg_Revenue_II, eval_g_ineq = Cap_Constr_II, opts = Opt)
  OptPrice_II=res$solution #Optimal Prices
  OptRevenue_II=-res$objective # Maximum Revenues
  
#### Part III) Optimal Incentive Compatible (IC) Differentiated Prices ##############
  #Define Revenue Functions
  Neg_Revenue_III <- function(p) -Demand_M(p[1])*p[1]-Demand_G(p[2])*p[2] 
  
  #Define capacity constraint. 
  Constr_III <- function(p) {
    constr <- c(Demand_M(p[1])+Demand_G(p[2])-Cap, p[2]-p[1]-30)
    return (constr)
  }
  
  #Initial price needed by the solver
  p0_III <- c(50,50)
  
  ##Run the Optimization
  res <- nloptr(x0 = p0_III, eval_f = Neg_Revenue_III, eval_g_ineq = Constr_III, opts = Opt)
  OptPrice_III=res$solution #Optimal Prices
  OptRevenue_III=-res$objective # Maximum Revenues
  
  
#### Part IV) Optimal Price Seated and Standing ##############
  
  ##Define demand functions
  Demand_S <- function(p) max(0,22000-100*p)  #Seated
  Demand_N <- function(p) max(0,20000-100*p)  #Non-Seated
  
  
  #Define Revenue Functions
  Neg_Revenue_IV <- function(p) -Demand_S(p[1])*p[1]-Demand_N(p[2])*p[2] 
  
  #Define capacity constraint. 
  Cap_Constr_IV <- function(p) 1.5*Demand_S(p[1])+Demand_N(p[2])-Cap
  
  #Initial price needed by the solver
  p0_IV <- c(50,50)
  
  ##Run the Optimization
  res <- nloptr(x0 = p0_IV, eval_f = Neg_Revenue_IV, eval_g_ineq = Cap_Constr_IV, opts = Opt)
  OptPrice_IV=res$solution #Optimal Prices
  OptRevenue_IV=-res$objective # Maximum Revenues
  
  