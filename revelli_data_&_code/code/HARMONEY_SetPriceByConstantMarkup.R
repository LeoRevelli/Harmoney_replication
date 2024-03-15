# This code is a model for the integrated biophysical - economy model, MORE.
rm(list=ls(all=TRUE))
#set the current directory that has the data for plotting
setwd("C:/Carey/Research/Writings/2018/HANDYEconomicModel/Submissions/EcologicalEconomics/Submission3/Code")

## ==========================================================================================
## About the mode
## ==========================================================================================
## This integrated model has a biophysical part and macroeconomy part.
## The model consist of two sectors, (1) extraction sector, and (2) goods sector.
## There are thirteen model states that include wealth, labor, capital,
## population, wages, nature, debt, capacity utilization, inventory coverage, prices, profits,
## net outputs and value added.
##
## This model is a complete mixed biophysical and macroeconomic model that simulates a rise
## and decline of poulation and capital along with other physical and economic variables of interst.
## As part of this, there is a set of "if-then" statements that are needed to guide
## the simulation in different "constrained" situation. The "normal growth" situation is #4 below (economy is constrained by capital (not labor), 
## and there is enough net output to allow for minimum household consumption per person)
## THERE ARE EIGHT SITUATIONS to solve for X, Y, C, I, CU, IC, PROFITS, VA, and L:
## 1. (<<<) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
## 2. (<<>) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
## 3. (<><) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
## 4. (<>>) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
## 5. (><<) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
## 6. (><>) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
## 7. (>><) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
## 8. (>>>) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
## These are important when growth is no longer sufficient to stay below maximum employment, and
## maximum employment is based on the idea that 100% of the population cannot work. Also, 
## the minimum amount of phsyical nature consumption per person (= rho.extract) assumes that some policy will 
## maintain this level of minimum consumption, and if desired it can be set to zero in the simulation.
##
## ==========================================================================================

## DEFINING STATE VARIABLES IN THE SYSTEM
#########################################################################################
## This section defines the State Variables in the system
#########################################################################################
library(deSolve)  

goodwinhandy <- function(t,y,parms){
    
    a <- y[1];                   # Labor Productivity
    w_h <- y[2];                 # Wealth
    K.goods <- y[3];             # Total Goods Capital 
    x_hc <- y[4];                # Total Population
    w <- y[5];                   # Real wage per person
    y_h <- y[6];                 # Available Nature
    D.goods <- y[7];             # Debt for goods sector
    CU.extract <- y[8];          # Capacity Utilization of extraction capital
    IC.extract_perceived <- y[9] # Perceived inventory coverage
    P.extract <- y[10]           # price of extracted nature last time period
    CU.goods <- y[11]            # Capacity Utilization of goods capital
    IC.goods_perceived <- y[12]  # Perceived inventory coverage of goods
    g <- y[13]                   # Stock of goods (inventory)  
    K.extract <- y[14]           # Total Extraction Capital
    D.extract <- y[15]           # Debt for extraction sector
    P.goods <- y[16]             # Price of goods last time period
    profit.extract <- y[17]      # Profits of the extraction sector (last time period)
    profit.goods <- y[18]        # Profits of the goods sector (last time period)
    Y.extract <- y[19]           # Net output of extraction sector [money units]
    Y.goods <- y[20]             # Net output of goods sector [money units]
    VA.extract <- y[21]          # Value added of extraction sector [money units]
    VA.goods <- y[22]            # Value added of goods sector [money units]
    lambda_h1 <- y[23]           # Initial value of each stock
    lambda_h2 <- y[24]           # Initial value of each stock
    lambda_h3 <- y[25]           # Initial value of each stock
    Inv_Cumu.ext <- y[26]        # Cumulative Investments that determines the changes in eta.Kextract 
    Inv_Cumu.goods <- y[27]      # Cumulative Investments that determines the changes in  eta.Kgoods 
    value_inventory.extract <- y[28]        # Value of inventory for extraction sector
    value_inventory.goods <- y[29]          # Value of inventory for goods sector
    C.extract_past <- y[30]      # The past (percieved) household consumption of extraction sector output
    C.goods_past <- y[31]        # The past (percieved) household consumption of goods sector output
    I.extract_past <- y[32]      # The past (percieved) investment in extraction sector capital
    I.goods_past <- y[33]        # The past (percieved) investment in goods sector capital
    L.extract_past <- y[34]      # The past (percieved) labor for extraction sector output
    L.goods_past <- y[35]        # The past (percieved) labor for goods sector output
    #################################################################################################

    ##############################################################################################
    ## Setting the maximum amount of nature (lambda_h)  
    ##############################################################################################
    lambda_h <- lambda_h3            # The value of "lambda_h" (maximum amount of nature) is equal to "lambda_h3" which is the 3rd order delay if the user chooses to increase the maximum "lambda" at some time
    if (t < tcritical.lambda_h) {    # Setting the time of agrarian society
      lambda_h.now <- lambda_h.max1  # Setting all the parameter values for an agrarian society
      delta_y <- delta_y.1
      inv_kappa0 <- inv_kappa0.1
      inv_kappa1 <- inv_kappa1.1
      delta <- delta1
      v <- v1
    } else {                         # Setting the time of industrial society
      lambda_h.now <- lambda_h.max2  # Setting all the parameter values for an agrarian society
      delta_y <- delta_y.2           
      inv_kappa0 <- inv_kappa0.2
      inv_kappa1 <- inv_kappa1.2
      delta <- delta2
      v <- v2
    }
    
    ########################################################################################
    ## Equations for total debt, household deposits, total value added, and total net output
    ########################################################################################
    D <- D.goods + D.extract            # Total debts calculation
    HH_deposits <- D                    # Household deposits is set to equal total firm debts (Godley and Lavoei, 2007)
    VA.total <- VA.extract + VA.goods   # Total economic value added calculation
    Y.total <- Y.extract + Y.goods      # Total net output calculation
    
    #############################
    ## Equations for Profit share 
    #############################
    profit_share.goods <- profit.goods/VA.goods        # Profit share calculation for goods sector       
    profit_share.extract <- profit.extract/VA.extract  # Profit share calculation for extraction sector
    profit_share <- (profit.goods + profit.extract)/(VA.goods + VA.extract) # Total economy profit share calculation

    #############################
    ## Solving for interest rates 
    #############################
    ## The 'tcritical.r' is the critical time to change the interest rate parameters
    if (t < tcritical.r) {
      xi_now.L <- xi.L
      phi_now.L <- phi.L
      xi_now.D <- xi.D
      phi_now.D <- phi.D
    } else {
      xi_now.L <- xi2.L
      phi_now.L <- phi2.L
      xi_now.D <- xi2.D
      phi_now.D <- phi2.D
    }
    
    ######################################################
    ## Calculating the interest rate on loans and deposits 
    ######################################################
    r.L = xi_now.L + phi_now.L*D/VA.total       # Interest rate on loans
    r.D = xi_now.D + phi_now.D*D/VA.total       # Interest rate on deposits
    
    #############################
    ## Equations for Profit rate 
    #############################
    K <- K.goods + K.extract                                   # Total capital calculation
    profit_rate.goods <- profit.goods/(P.goods*K.goods)        # Profit rate for goods sector calculation based on price, profit and capital from goods sector
    profit_rate.extract <- profit.extract/(P.goods*K.extract)  # Profit rate for extraction sector calculation based on the fact that capital goods are produced by goods sector itself
    profit_rate <- (profit.goods + profit.extract)/(P.goods*K) # Total economy profit rate calculation
    
    ########################################################
    ## Equations for Investment as a function of Profit Rate  
    ########################################################
    ## Investment function is modeled as linear function of profit share [profit share = I/Value Added = kappa0 + kappa1*profit_share] (Bovari et al., 2018 in Ecological Economics)
    if (type.invest == 1) {
      I.goods <- max(0,inv_kappa1*profit.goods + inv_kappa0*P.goods*delta*K.goods)
      I.extract <- max(0,inv_kappa1*profit.extract + inv_kappa0*P.goods*delta*K.extract)
    } else if (type.invest == 2) {
      Invest_func.goods <- max(0,inv_kappa0 + inv_kappa1*profit_rate.goods)      # Linear investment function for goods sector (must be >= 0)
      Invest_func.extract <- max(0,inv_kappa0 + inv_kappa1*profit_rate.extract)  # Linear investment function for extraction sector (must be >= 0)
      I.goods <- max(0,Invest_func.goods*P.goods*K.goods)        # Total gross investment in goods sector with the criterion of investment > 0
      I.extract <- max(0,Invest_func.extract*P.goods*K.extract)  # Total gross investment in extraction sector with the criterion of investment > 0
    }

    ########################################################
    ## Defining Efficiency as a function of Total Investment
    ########################################################
    ## The efficiencies (eta) for extraction and goods sectors are modeled as a function of total investment in a logistic curve framework 
    ## The 'tcritical.Inv_Cum_extract' and 'tcritical.Inv_Cum_goods' are the critical time to change the cumulative investment function for extraction and goods sectors respectively
    if (t > tcritical.Inv_Cum_extract) {
      eta.Kextract <- (eta.Kextract_min - eta.Kextract_max)/(1 + exp(eta.Kextract_s * (Inv_Cumu.ext - 2*Inv_Cumu.mid))) + eta.Kextract_max
     } else {
      eta.Kextract <- eta.Kextract_max
    }
    if (t > tcritical.Inv_Cum_goods) {
      eta.Kgoods <- (eta.Kgoods_min - eta.Kgoods_max)/(1 + exp(eta.Kgoods_s * (Inv_Cumu.goods - 2*Inv_Cumu.mid))) + eta.Kgoods_max
    } else {
      eta.Kgoods <- eta.Kgoods_max
    }
    
    #############################
    ## FIRST: Assume Gross Output is limited by Capital
    #############################
    X.goods <- K.goods*CU.goods/v      # Gross goods calculation is a Leontief production function; based on capital, capacity utilization and capital-output ratio for goods sector
    nature_extraction <- delta_y*y_h*K.extract*CU.extract   # Nature extraction calculation
    X.extract <- nature_extraction     # Gross extraction is a Leontief production function; set equal to nature extraction
    X <- matrix(c(X.goods,X.extract),nrow=2,ncol=1,byrow = TRUE)        # Matirx representation for total gross outputs from extraction & goods sectors 
    Xhat <- matrix(c(X.goods,0,0,X.extract),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for gross goods and gross extracts 
    
    #############################
    ## Solve for labor and employment
    #############################
    a.extract <- a                       # Labor productivity of extraciton sector labor productivity of goods sector
    L.goods <- X.goods/a                 # Direct labor for industrial output 
    L.extract <- X.extract/a.extract     # Direct labor for extraction 
    L <- L.goods + L.extract             # Total labor for goods and extraction sectors
    participation_rate <- L/x_hc            # Employment rate as a function of total labor and total population 
    
    #####################################################################
    ## Defining Leontief technical coefficients matrix (A)in native units 
    #####################################################################
    ## 'A.ij' implies the quantity of sector i's input to produce one physical unit of j's output. 
    A.ee <- eta.Kextract/(delta_y*y_h) # The the quantity of extraction sector's input to produce one physical unit of extraction sector's output 
    A.eg <- eta.Kgoods*v + nature_per_unit_physical_good*v  # The the quantity of extraction sector's input to produce one physical unit of goods sector's output: "nature_per_unit_physical_good" is operating input; " nature_per_unit_physical_good" is nature embodied in physical capital
    A.ge <- a_ge  # The quantity of goods sector's input to produce one physical unit of extraction sector's output; and set as constant
    A.gg <- a_gg  # The quantity of goods sector's input to produce one physical unit of goods sector's output; and set as constant
    A <- matrix(c(A.gg,A.ge,A.eg,A.ee),nrow=2,ncol=2,byrow = TRUE)  # Leontief technical coefficients matrix for specifying intermediate demand for the two sector model
    
    #################################################################
    ## CALCULATE CONSUMPTION AND NET OUTPUT FROM A AND GROSS OUTPUT AS AN ASSUMED INPUT,
    #################################################################
    Eye <- matrix(c(1,0,0,1),nrow=2,ncol=2,byrow = TRUE)                # Identity matrix 
    I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
    consumption_threshold.nature <- rho.extract*x_hc   # minimum phyiscal nature consumption
    consumption_threshold.goods <- rho.goods*x_hc      # minimum phyiscal goods consumption
    
    ### Used for debugging
    if (is.na(participation_rate)==TRUE) {
      hey = 1### Used for debugging
    }
    ### Used for debugging
    t_thresh = 4000 ## arbitrary time threshold for debugging
    if (t >= t_thresh) {
      hey = 1### Used for debugging
    }
    
    #############################
    ## THERE ARE EIGHT SITUATIONS to solve for X, Y, C, I, CU, IC, PROFITS, VA, and L:
    ## 1. (<<<) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
    ## 2. (<<>) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
    ## 3. (<><) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
    ## 4. (<>>) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
    ## 5. (><<) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
    ## 6. (><>) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
    ## 7. (>><) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
    ## 8. (>>>) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
    ## These are important when growth is no longer sufficient to stay below maximum employment, and
    ## maximum employment is based on the idea that 100% of the population cannot work. Also, 
    ## the minimum amount of physical nature consumption per person (= rho.extract) assumes that some policy will 
    ## maintain this level of minimum consumption, and if desired it can be set to zero in the simulation. Setting 
    ## rho.extract > 0 is somewhat like providing a subsistence wage or minimum/universal basic income.
    #############################
    
    if (participation_rate <= participation_rate_max*0.99) {
      
      #################################################
      ## Equations for Wages 
      #################################################
      wages.goods <- w*L.goods             # Total wages for goods sector
      wages.extract <- w*L.extract         # Total wages for extraction sector
      wages <- wages.goods + wages.extract # Total wages calculated
      
      #################################
      ## Equations for update of prices 
      #################################
      ## The 'price updates' are the prices for current time period 't' 
      Vtilde.goods <- (wages.goods + r.L*D.goods)/X.goods # Normalized value added  for goods sector
      Vtilde.extract <- (wages.extract + r.L*D.extract)/X.extract               # Normalized value added  for extraction sector
      Vtilde <- matrix(c(Vtilde.goods,Vtilde.extract),nrow=2,ncol=1,byrow = TRUE)      # Normalized value added vector
      Depreciation <- matrix(c(delta*K.goods/X.goods,0,delta*K.extract/X.extract,0),nrow=2,ncol=2,byrow=TRUE) # Depreciation vector for goods & extraction sectors
      Markup_hat <- matrix(c(1/(1+markup.goods),0,0,1/(1+markup.extract)),nrow=2,ncol=2,byrow = TRUE)
      Atilde.ee <- A.ee
      Atilde.eg <- A.eg  
      Atilde.ge <- A.ge
      Atilde.gg <- A.gg
      Atilde <- matrix(c(Atilde.gg,Atilde.ge,Atilde.eg,Atilde.ee),nrow = 2,ncol = 2,byrow = TRUE) # Leontief technical coefficients matrix for specifying intermediate demand for the two sector model
      Atilde_transpose <- t(Atilde) # Transpose of Leontief technical coefficients matrix
      P <- solve(Markup_hat - Atilde_transpose - Depreciation)%*%(Vtilde)  # Update of prices matrix
      P.goods_update <- P[1]    # The 1st row, 1st column of price update matrix that defines updates of goods price                    
      P.extract_update <- P[2]  # The 2nd row, 1st column of price update matrix that defines updates of extraction price
      
      ###############################
      ## Equations for Net Output (Y.update) updates 
      ###############################
      ## The 'net output updates' are the net outputs for current time period 't' 
      ## NOTE: Here we must ensure that the population is >= total labor = L.goods + L.extract
      Phat.update <- matrix(c(P.goods_update,0,0,P.extract_update),nrow=2,ncol=2,byrow = TRUE)  # Matrix with goods and extraction sector price updates on the diagonal
      
      ###############################
      ## Equations for Net Output, Profit, Value Added, cost, and value_inventory updates
      ###############################
      ## Change in value of inventory; cost is "per unit of gross output"
      cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
      cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
      value_inventory.extract_update <- cost.extract_update*w_h
      value_inventory.goods_update <- cost.goods_update*g
      
      ## The 'profit updates' are the profits for current time period 't' 
      profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
      profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money

      VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
      VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
      
      Y.update <- Phat.update%*%X - Phat.update%*%A%*%X  # Net output vector in units of money
      Y.goods_update <- Y.update[1]     # The 1st row, 1st column of net output vector that defines net goods output updates
      Y.extract_update <- Y.update[2]   # The 2nd row, 1st column of net output vector that defines net extracts output updates

      #################################################
      ## Household Consumption  
      #################################################
      # Account for change in value of inventory for calculating consumption
      change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
      change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
      delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
      
      C = Y.update - I - delta_value_inventory   ## Does this inherently assume no "change in inventory value" or incorrectly neglect "change in inventory value"
      C.goods = C[1]
      C.extract = C[2]
      C.goods_physical <- C.goods/P.goods_update  # Physical (units) consumption of goods as a function of its price
      C.extract_physical <- C.extract/P.extract_update          # physical human nature consumption based on monetary nature consumption
      
      XX.gg <- P.goods_update*A.gg*X.goods     ## I-O transactions matrix in money units
      XX.ge <- P.goods_update*A.ge*X.extract   ## I-O transactions matrix in money units
      XX.eg <- P.extract_update*A.eg*X.goods   ## I-O transactions matrix in money units
      XX.ee <- P.extract_update*A.ee*X.extract ## I-O transactions matrix in money units
      
      #################################################
      ## Change in physical inventory and physical intermediate demand  
      #################################################
      ## Monetary value of inventory is based on cost, not price. 
      ## So physical inventory is monetary value of inventory divided by cost of inventory.
      change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
      change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
      delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
      Intermediate <- A%*%X # Intermediate consumption in physical units
      Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
      Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
      
      if (C.extract_physical < consumption_threshold.nature) {
        
        ## In the present model, the total population is assumed equal to the commoners population
        ## Human consumption of nature (wealth) is minimum of "subsitence consumption" where death rates are at minimum that is dictated by setting a threshold consumption per person (= rho.extract)
        C.extract_physical.nature_difference <- consumption_threshold.nature - C.extract_physical
        C.extract_physical <- consumption_threshold.nature
        ## This solves for a reduced X.goods needed to ensure there is not too much
        ## intermediate nature consumption needed by the goods sector that prevents the needed minimum household consumption of nature. 
        ## Here this assumes that there is no change in the stock of wealth, w_h (or inventory of extraction sector).
        ## This equation used is that tracking the physical flow of nature: X.extract = Aee*X.extract + Aeg*X.goods + consumption of nature by people + change in physical inventory
        X.goods_new <- (X.extract*(1-A.ee) - consumption_threshold.nature - change_inventory.extract)/A.eg
        X.goods_pct_change <- (X.goods - X.goods_new)/X.goods
        
        #################################################
        ## Update Investment  
        #################################################
        ## Need to reduce I.goods, I.extract, and C.goods to account for reduction in X.goods
        ## I assume all of the reduction is in those three factors (keep change_inventory the same) and they all decrease by the same percentage
        ## This reduction in I.goods and I.extract NEEDS TO BE IN PHYSICAL UNITS ... ??
        I.goods <- I.goods*(1-X.goods_pct_change)                           ## Decrease in I.goods (money units) due to reduction in gross physical goods output
        I.extract <- I.extract*(1-X.goods_pct_change)                       ## Decrease in I.extract (money units) due to reduction in gross physical goods output
        I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
        
        # Goods sector capacity utilization adjusts downward to account for lower extraction
        # We do not use the lookup function to determine CU.goods in this case.
        CU.goods_indicated <- X.goods_new/X.goods*CU.goods     
        X.goods <- X.goods_new              ## Update X.goods with new value
        ## Create updated gross output vector 
        X <- matrix(c(X.goods,X.extract),nrow=2,ncol=1,byrow = TRUE)        # Matirx representation for total gross outputs from extraction & goods sectors 
        Xhat <- matrix(c(X.goods,0,0,X.extract),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for gross goods and gross extracts 
        
        ## Recalculate L.goods and wages.goods since there is new X.goods
        L.goods <- X.goods/a
        wages.goods <- w*L.goods
        
        ## Resolve for prices with the new X and Xhat
        Vtilde.goods <- (wages.goods + r.L*D.goods)/X.goods # Normalized value added  for goods sector
        Vtilde.extract <- (wages.extract + r.L*D.extract)/X.extract               # Normalized value added  for extraction sector
        Vtilde <- matrix(c(Vtilde.goods,Vtilde.extract),nrow=2,ncol=1,byrow = TRUE)      # Normalized value added vector
        Markup_hat <- matrix(c(1/(1+markup.goods),0,0,1/(1+markup.extract)),nrow=2,ncol=2,byrow = TRUE)
        P <- solve(Markup_hat - Atilde_transpose - Depreciation)%*%(Vtilde)  # Update of prices matrix
        P.goods_update <- P[1]    # The 1st row, 1st column of price update matrix that defines updates of goods price                    
        P.extract_update <- P[2]  # The 2nd row, 1st column of price update matrix that defines updates of extraction price
        
        Y.update <- Phat.update%*%X - Phat.update%*%A%*%X  # Net output vector in units of money
        Y.goods_update <- Y.update[1]     # The 1st row, 1st column of net output vector that defines net goods output updates
        Y.extract_update <- Y.update[2]   # The 2nd row, 1st column of net output vector that defines net extracts output updates

        ## Change in value of inventory; cost is "per unit of gross output"
        cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
        cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
        value_inventory.extract_update <- cost.extract_update*w_h
        value_inventory.goods_update <- cost.goods_update*g
        
        change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
        change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
        ## account for change in value of inventory for calculating C (comsumption)
        delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
        
     
        #################################################
        ## Consumption and death rate threshold functions  
        #################################################
        C = Y.update - I - delta_value_inventory   ## Does this inherently assume no "change in inventory value" or incorrectly neglect "change in inventory value"
        C.goods = C[1]
        C.extract = C[2]
        C.goods_physical <- C.goods/P.goods_update  # Physical (units) consumption of goods as a function of its price
        C.extract_physical <- C.extract/P.extract_update         # physical human nature consumption based on monetary nature consumption
        
        #################################################
        ## Change in physical inventory based upon the monetary 
        ## change in the value of inventory divided by its cost.
        #################################################
        ## Should this be divided by cost or price?
        change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
        change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
        delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
        
        #################################################
        ## Recalculate intermediate (physical) demand
        #################################################
        Intermediate <- A%*%X # Intermediate consumption in physical units
        Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
        Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
        
        if (C.goods_physical < consumption_threshold.goods) {
          ## 1. (<<<) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          ## 1. (<<<) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          C.goods_physical_difference <- consumption_threshold.goods - C.goods_physical
          C.goods_physical <- consumption_threshold.goods
          ## Must reduce total investment by the same amount that C.goods is negative.
          ## After this, Y.goods and X.goods should be the same as before, as these lines only
          ## shift some of Y.goods from I.goods to C.goods so that C.goods is = the threshold.
          I.reduce_fraction <- min(1,C.goods_physical_difference*P.goods_update/(I.goods + I.extract)) ## Cannot reduce invesment more than 100%
          I.goods <- I.goods*(1-I.reduce_fraction)
          I.extract <- I.extract*(1-I.reduce_fraction)
          I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
          
          ## Calculate consumption of goods in money units
          C.goods <- C.goods_physical*P.goods_update

          ## I think need to update CU.goods_indicated and calculate IC.goods
          targeted_consumption.goods <- C.goods_physical + (I.goods + I.extract)/P.goods_update + Intermediate.goods # Targeted goods consumption
          IC.goods <- (g/CU.goods_delay)/targeted_consumption.goods;        # Perceived inventory coverage for goods sector
         
          ## Update CU.extract_indicated and calculate IC.extract
          targeted_consumption.nature <- C.extract_physical + Intermediate.nature        # Targeted nature consumption
          IC.extract <- (w_h/CU.extract_delay)/targeted_consumption.nature # Perceived inventory coverage for extraction sector
          CU.extract_1 <- approx(CU.extract_x,CU.extract_y,1/IC.extract_perceived) # Extraction sector capacity utilization set as a lookup function
          CU.extract_indicated <- CU.extract_1$y  # Extraction sector capacity utilization for the current time period 't'
          
          ## Update net output, Y
          Y.goods_update <- C.goods + I.goods + change_value_inventory.goods
          Y.extract_update <- C.extract + I.extract + change_value_inventory.extract
          Y.update <- matrix(c(Y.goods_update,Y.extract_update),nrow=2,ncol=1,byrow = TRUE)        # Matrix representation for total gross outputs from extraction & goods sectors 
          
          ### Update gross output vector, X
          X <- solve(Eye-A)%*%Y.update
          X.goods <- X[1]
          X.extract <- X[2]
          Xhat <- matrix(c(X.goods,0,0,X.extract),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for gross goods and gross extracts 

          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
          cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
          cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
          value_inventory.extract_update <- cost.extract_update*w_h
          value_inventory.goods_update <- cost.goods_update*g
          change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
          change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
          ## account for change in value of inventory for calculating C (comsumption)
          delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
          change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
          delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          Intermediate <- A%*%X  # Intermediate consumption in physical units
          Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
          Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
          
        } else {
          ## 2. (<<>) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          ## 2. (<<>) participation_rate < participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1 ### Used for debugging
          }
          
          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates

          ####################################################
          ## Equations for Capacity Utilization for extraction 
          ## and Inventory Coverage for both extraction and goods.
          ## Before getting to this point, CU.goods has already been
          ## reduced to account for maintaining the "consumption_threshold.nature".
          #################################################### 
          targeted_consumption.nature <- C.extract_physical + Intermediate.nature        # Targeted nature consumption
          IC.extract <- (w_h/CU.extract_delay)/targeted_consumption.nature # Perceived inventory coverage for extraction sector
          CU.extract_1 <- approx(CU.extract_x,CU.extract_y,1/IC.extract_perceived) # Extraction sector capacity utilization set as a lookup function
          CU.extract_indicated <- CU.extract_1$y  # Extraction sector capacity utilization for the current time period 't'

          targeted_consumption.goods <- C.goods_physical + (I.goods + I.extract)/P.goods_update + Intermediate.goods # Targeted goods consumption
          IC.goods <- (g/CU.goods_delay)/targeted_consumption.goods;        # Perceived inventory coverage for goods sector

        } ## if (C.goods_physical < consumption_threshold.goods)
        
        
      } else { ## (C.extract_physical > consumption_threshold.nature) 
        
        ###############################################
        ## Equations for Capacity Utilization for GOODS 
        ## If X.goods does not have to be reduced because
        ## "C.extract_physical < consumption_threshold.nature" was true.
        ###############################################
        ## Targeted consumption of goods (everything consuming in "g_dot" equation)
        targeted_consumption.goods <- C.goods_physical + (I.goods + I.extract)/P.goods_update + Intermediate.goods # Targeted goods consumption
        IC.goods <- (g/CU.goods_delay)/targeted_consumption.goods;        # Perceived inventory coverage for goods sector
        CU.goods_1 <- approx(CU.goods_x,CU.goods_y,1/IC.goods_perceived); # Goods sector capacity utilization set as a lookup function
        CU.goods_indicated <- CU.goods_1$y;  # Goods sector capacity utilization for the current time period 't'
        
        if (C.goods_physical < consumption_threshold.goods) {
          ## 3. (<><) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          ## 3. (<><) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          
          C.goods_physical_difference <- consumption_threshold.goods - C.goods_physical
          C.goods_physical <- consumption_threshold.goods
          ## Must reduce total investment by the same amount that C.goods is negative
          I.reduce_fraction <- min(1,C.goods_physical_difference*P.goods_update/(I.goods + I.extract)) ## Cannot reduce invesment more than 100%
          I.goods <- I.goods*(1-I.reduce_fraction)
          I.extract <- I.extract*(1-I.reduce_fraction)
          I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
          
          ## Calculate consumption of goods in money units
          C.goods <- C.goods_physical*P.goods_update
          
          ###############################################
          ## Equations for Capacity Utilization for GOODS 
          ## If I.goods and I.extract have to be reduced 
          ## because "C.goods_physical < < consumption_threshold.goods") was true.
          ###############################################
          ## Targeted consumption of goods (everything consuming in "g_dot" equation)
          targeted_consumption.goods <- C.goods_physical + (I.goods + I.extract)/P.goods_update + Intermediate.goods # Targeted goods consumption
          IC.goods <- (g/CU.goods_delay)/targeted_consumption.goods;        # Perceived inventory coverage for goods sector
          CU.goods_1 <- approx(CU.goods_x,CU.goods_y,1/IC.goods_perceived); # Goods sector capacity utilization set as a lookup function
          CU.goods_indicated <- CU.goods_1$y  # Goods sector capacity utilization for the current time period 't'
          
          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
          Y.goods_update <- profit.goods_update+r.L*D.goods+P.goods_update*delta*K.goods+wages.goods-P.goods_update*A.ge*X.extract+P.extract_update*A.eg*X.goods
          Y.extract_update <- profit.extract_update+r.L*D.extract+P.goods_update*delta*K.extract+wages.extract+P.goods_update*A.ge*X.extract-P.extract_update*A.eg*X.goods
          cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
          cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
          value_inventory.extract_update <- cost.extract_update*w_h
          value_inventory.goods_update <- cost.goods_update*g
          change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
          change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
          ## account for change in value of inventory for calculating C (comsumption)
          delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
          change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
          delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          Intermediate <- A%*%X  # Intermediate consumption in physical units
          Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
          Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
          
          
      } else {
        ## This is "normal growth" condition where capital is limiting (limiting factors are not labor, consumption of nature, or consumption of goods)
        ## 4. (<>>) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
        ## 4. (<>>) participation_rate < participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
        if (t>t_thresh) {
          hey = 1### Used for debugging
        }
        
        ## DO NO NEW CALCULATIONS
        hey = 1### Used for debugging
        
      } ## if (C.extract_physical < consumption_threshold.nature)
    
      ## If capital is the limiting factor (as is the case if simulation gets here),
      ## then Capacity Utilization for goods is solved using the lookup functions.
      ####################################################
      ## Equations for Capacity Utilization for extraction 
      #################################################### 
      targeted_consumption.nature <- C.extract_physical + Intermediate.nature        # Targeted nature consumption
      IC.extract <- (w_h/CU.extract_delay)/targeted_consumption.nature # Perceived inventory coverage for extraction sector
      CU.extract_1 <- approx(CU.extract_x,CU.extract_y,1/IC.extract_perceived) # Extraction sector capacity utilization set as a lookup function
      CU.extract_indicated <- CU.extract_1$y  # Extraction sector capacity utilization for the current time period 't'
      
        
      }  ## This ends the loop started by "} else { ## (C.extract_physical > consumption_threshold.nature)"
      ## Now ready to perform calculations as if labor has reached its maximum limit
          
      
      
    } else { ## then (participation_rate > participation_rate_max)
    
      
      ####################################################
      ## Now Labor is the limiting factor in gross output that is a Leontief production function assumption ( = min(L*a,K*CU/v))
      ## Solve for reduced labor such that "participation_rate" is equal to "participation_rate_max"
      ####################################################
      L.reduce_fraction <- (participation_rate - participation_rate_max)/participation_rate
      L.goods <- L.goods*(1-L.reduce_fraction)        ## Reduce L.goods in proportion to how big it is compared to L.extract
      L.extract <- L.extract*(1-L.reduce_fraction)    ## Reduce L.extract in proportion to how big it is compared to L.goods
      L <- L.goods + L.extract             # Total labor for goods and extraction sectors
      participation_rate <- L/x_hc            # Employment rate as a function of total labor and total population 
      X.goods <- L.goods*a                            ## Gross output is based on labor
      X.extract <- L.extract*a.extract                ## Gross output is based on labor
      
      ####################################################
      ## Equations for Capacity Utilization
      #################################################### 
      CU.extract_indicated <- X.extract/(K.extract*delta_y*y_h) ## Capacity utilization is reduced based upon gross output calculated from labor constraint
      CU.goods_indicated <- X.goods/(K.goods/v)                 ## Capacity utilization is reduced based upon gross output calculated from labor constraint

      ## Create updated gross output vector
      X <- matrix(c(X.goods,X.extract),nrow=2,ncol=1,byrow = TRUE)        # Matirx representation for total gross outputs from extraction & goods sectors
      Xhat <- matrix(c(X.goods,0,0,X.extract),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for gross goods and gross extracts
      
      #################################################
      ## Equations for Wages 
      #################################################
      wages.goods <- w*L.goods             # Total wages for goods sector
      wages.extract <- w*L.extract         # Total wages for extraction sector
      wages <- wages.goods + wages.extract # Total wages calculated
      
      #################################
      ## Equations for update of prices 
      #################################
      ## The 'price updates' are the prices for current time period 't' 
      Vtilde.goods <- (wages.goods + r.L*D.goods)/X.goods # Normalized value added  for goods sector
      Vtilde.extract <- (wages.extract + r.L*D.extract)/X.extract               # Normalized value added  for extraction sector
      Vtilde <- matrix(c(Vtilde.goods,Vtilde.extract),nrow=2,ncol=1,byrow = TRUE)      # Normalized value added vector
      Depreciation <- matrix(c(delta*K.goods/X.goods,0,delta*K.extract/X.extract,0),nrow=2,ncol=2,byrow=TRUE) # Depreciation vector for goods & extraction sectors
      Atilde.ee <- A.ee
      Atilde.eg <- A.eg  
      Atilde.ge <- A.ge
      Atilde.gg <- A.gg
      Atilde <- matrix(c(Atilde.gg,Atilde.ge,Atilde.eg,Atilde.ee),nrow = 2,ncol = 2,byrow = TRUE) # Leontief technical coefficients matrix for specifying intermediate demand for the two sector model
      Atilde_transpose <- t(Atilde) # Transpose of Leontief technical coefficients matrix
      P <- solve(Markup_hat - Atilde_transpose - Depreciation)%*%(Vtilde)  # Update of prices matrix
      P.goods_update <- P[1]    # The 1st row, 1st column of price update matrix that defines updates of goods price                    
      P.extract_update <- P[2]  # The 2nd row, 1st column of price update matrix that defines updates of extraction price
      
      ###############################
      ## Equations for Net Output (Y.update) updates 
      ###############################
      ## The 'net output updates' are the net outputs for current time period 't' 
      ## NOTE: Here we must ensure that the population is >= total labor = L.goods + L.extract
      Phat.update <- matrix(c(P.goods_update,0,0,P.extract_update),nrow=2,ncol=2,byrow = TRUE)  # Matrix with goods and extraction sector price updates on the diagonal
      
      ###############################
      ## Equations for Net Output, Profit, Value Added, cost, and value_inventory updates
      ###############################
      ## The 'profit updates' are the profits for current time period 't' 
      profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
      profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
      VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
      VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
      
      Y.update <- Phat.update%*%X - Phat.update%*%A%*%X  # Net output vector in units of money
      Y.goods_update <- Y.update[1]     # The 1st row, 1st column of net output vector that defines net goods output updates
      Y.extract_update <- Y.update[2]   # The 2nd row, 1st column of net output vector that defines net extracts output updates

      ## Change in value of inventory; cost is "per unit of gross output"
      cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
      cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
      value_inventory.extract_update <- cost.extract_update*w_h
      value_inventory.goods_update <- cost.goods_update*g
      
      change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
      change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
      ## account for change in value of inventory for calculating C (comsumption)
      delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
      
      #################################################
      ## Consumption and death rate threshold functions  
      #################################################
      C = Y.update - I - delta_value_inventory   ## Does this inherently assume no "change in inventory value" or incorrectly neglect "change in inventory value"
      C.goods = C[1]
      C.extract = C[2]
      C.extract_physical <- C.extract/P.extract_update         # physical human nature consumption based on monetary nature consumption
      C.goods_physical <- C.goods/P.goods_update  # Physical (units) consumption of goods as a function of its price
      
      #################################################
      ## Change in physical inventory and physical intermediate demand  
      #################################################
      ## Should this be divided by cost or price?
      change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
      change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
      delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
      Intermediate <- A%*%X  # Intermediate consumption in physical units
      Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
      Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
      
      
      if (C.extract_physical < consumption_threshold.nature) {
        
        
        ## In the present model, the total population is assumed equal to the commoners population
        ## Human consumption of nature (wealth) is minimum of "subsitence consumption" where death rates are at minimum and a ratio of calcualted consumption over a threshold
        C.extract_physical.nature_difference <- consumption_threshold.nature - C.extract_physical
        C.extract_physical <- consumption_threshold.nature
        ## This solves for a reduced X.goods needed to ensure there is not too much
        ## intermediate nature consumption needed by the goods sector that prevents the needed minimum household consumption of nature. 
        ## Here this assumes that there is no change in the stock of wealth, w_h (or inventory of extraction sector).
        ## This equation used is that tracking the physical flow of nature: X.extract = Aee*X.extract + Aeg*X.goods + conumption of nature by people + change in physical inventory
        X.goods_new <- (X.extract*(1-A.ee) - consumption_threshold.nature - change_inventory.extract)/A.eg
        X.goods_pct_change <- (X.goods - X.goods_new)/X.goods
        I.goods <- I.goods*(1-X.goods_pct_change)                           ## Decrease in I.goods due to reduction in gross physical goods output
        I.extract <- I.extract*(1-X.goods_pct_change)                       ## Decrease in I.extract due to reduction in gross physical goods output
        C.goods_physical <- C.goods/P.goods_update*(1-X.goods_pct_change)   ## Decrease in C.goods (physical) due to reduction in gross physical goods output
        I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
        
        ## Calculate updated consumption in money units
        C.goods <- C.goods_physical*P.goods_update
        C.extract <- C.extract_physical*P.extract_update
        
        ## Create updated gross output vector 
        X.goods <- X.goods_new
        X <- matrix(c(X.goods,X.extract),nrow=2,ncol=1,byrow = TRUE)        # Matirx representation for total gross outputs from extraction & goods sectors 
        Xhat <- matrix(c(X.goods,0,0,X.extract),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for gross goods and gross extracts 
        
        ## Now have to update L.goods and wages.goods again because X.goods has decreased
        L.goods <- X.goods/a
        wages.goods <- w*L.goods
        ## Need to update CU.goods_indicated ???
        CU.goods_indicated <- X.goods/(K.goods/v)                 ## Capacity utilization is reduced based upon gross output calculated from labor constraint
        
        if (C.goods_physical < consumption_threshold.goods) {
          ## 5. (><<) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          ## 5. (><<) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          C.goods_physical_difference <- consumption_threshold.goods - C.goods_physical
          C.goods_physical <- consumption_threshold.goods
          ## Must reduce total investment by the same amount that C.goods is negative
          I.reduce_fraction <- min(1,C.goods_physical_difference*P.goods_update/(I.goods + I.extract)) ## Cannot reduce invesment more than 100%
          I.goods <- I.goods*(1-I.reduce_fraction)
          I.extract <- I.extract*(1-I.reduce_fraction)
          I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
          
          ## Calculate consumption of goods in money units
          C.goods <- C.goods_physical*P.goods_update
          
          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
          Y.goods_update <- profit.goods_update+r.L*D.goods+P.goods_update*delta*K.goods+wages.goods-P.goods_update*A.ge*X.extract+P.extract_update*A.eg*X.goods
          Y.extract_update <- profit.extract_update+r.L*D.extract+P.goods_update*delta*K.extract+wages.extract+P.goods_update*A.ge*X.extract-P.extract_update*A.eg*X.goods
          cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
          cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
          value_inventory.extract_update <- cost.extract_update*w_h
          value_inventory.goods_update <- cost.goods_update*g
          change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
          change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
          ## account for change in value of inventory for calculating C (comsumption)
          delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
          change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
          delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          Intermediate <- A%*%X  # Intermediate consumption in physical units
          Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
          Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
          
          
        } else { ## then ... (C.goods_physical > consumption_threshold.goods) 
          ## 6. (><>) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          ## 6. (><>) participation_rate > participation_rate_max & C.extract_physical < minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          ## Do nothing more, but update money balance equations
          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
          Y.goods_update <- profit.goods_update+r.L*D.goods+P.goods_update*delta*K.goods+wages.goods-P.goods_update*A.ge*X.extract+P.extract_update*A.eg*X.goods
          Y.extract_update <- profit.extract_update+r.L*D.extract+P.goods_update*delta*K.extract+wages.extract+P.goods_update*A.ge*X.extract-P.extract_update*A.eg*X.goods
          cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract + r.L*D.extract/X.extract + delta*P.goods_update*K.extract/X.extract
          cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods + r.L*D.goods/X.goods + delta*P.goods_update*K.goods/X.goods
          value_inventory.extract_update <- cost.extract_update*w_h
          value_inventory.goods_update <- cost.goods_update*g
          change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
          change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
          ## account for change in value of inventory for calculating C (comsumption)
          delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
          change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
          delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          Intermediate <- A%*%X  # Intermediate consumption in physical units
          Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
          Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
          
        } ## if (C.goods_physical < consumption_threshold.goods) 
        
        
      } else { ## (C.extract_physical > consumption_threshold.nature)
        
        
        if (C.goods_physical < consumption_threshold.goods) {
          ## 7. (>><) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          ## 7. (>><) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical < minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          C.goods_physical_difference <- consumption_threshold.goods - C.goods_physical
          C.goods_physical <- consumption_threshold.goods
          ## Must reduce total investment by the same amount that C.goods is negative
          I.reduce_fraction <- min(1,C.goods_physical_difference*P.goods_update/(I.goods + I.extract))  ## You cannot reduce invesment more than 100%, as minimum investment = 0.
          I.goods <- I.goods*(1-I.reduce_fraction)
          I.extract <- I.extract*(1-I.reduce_fraction)
          I <- matrix(c(I.goods+I.extract,0),nrow=2,ncol=1,byrow = TRUE)      # Investement vector in money units
          
          ## Calculate consumption of goods in money units
          C.goods <- C.goods_physical*P.goods_update
          
          ## Calculate sector money balance factors 
          profit.goods_update <- P.goods_update*X.goods - P.goods_update*A.gg*X.goods - P.extract_update*A.eg*X.goods - wages.goods - r.L*D.goods - P.goods_update*delta*K.goods   # Good sector Profit updates in units of money
          profit.extract_update <- P.extract_update*X.extract - P.extract_update*A.ee*X.extract - P.goods_update*A.ge*X.extract - wages.extract - r.L*D.extract - P.goods_update*delta*K.extract # Extraction sector Profit updates in units of money
          VA.goods_update <- profit.goods_update + wages.goods + r.L*D.goods + P.goods_update*delta*K.goods            # Good sector value added updates
          VA.extract_update <- profit.extract_update + wages.extract + r.L*D.extract + P.goods_update*delta*K.extract  # Extraction sector value added updates
          Y.update <- Phat.update%*%X - Phat.update%*%A%*%X  # Net output vector in units of money
          Y.goods_update <- Y.update[1]     # The 1st row, 1st column of net output vector that defines net goods output updates
          Y.extract_update <- Y.update[2]   # The 2nd row, 1st column of net output vector that defines net extracts output updates
          cost.extract_update <- P.extract_update*A.ee + P.goods_update*A.ge + wages.extract/X.extract 
          cost.goods_update <- P.goods_update*A.gg + P.extract_update*A.eg + wages.goods/X.goods 
          value_inventory.extract_update <- cost.extract_update*w_h
          value_inventory.goods_update <- cost.goods_update*g
          change_value_inventory.extract <- value_inventory.extract_update - value_inventory.extract
          change_value_inventory.goods <- value_inventory.goods_update - value_inventory.goods
          ## account for change in value of inventory for calculating C (comsumption)
          delta_value_inventory <- matrix(c(change_value_inventory.goods,change_value_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          change_inventory.goods <- delta_value_inventory[1]/cost.goods_update
          change_inventory.extract <- delta_value_inventory[2]/cost.extract_update
          delta_inventory <- matrix(c(change_inventory.goods,change_inventory.extract),nrow=2,ncol=1,byrow = TRUE)      # "change in value of inventory" vector in money units
          Intermediate <- A%*%X  # Intermediate consumption in physical units
          Intermediate.goods <- Intermediate[1]   # Intermediate consumption of goods in physical units
          Intermediate.nature <- Intermediate[2]  # Intermediate consumption of extraction which includes the nature needed for investment since the same is included in 'A' matrix
          
          
        } else {
          ## 8. (>>>) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          ## 8. (>>>) participation_rate > participation_rate_max & C.extract_physical > minimum physical consumption of nature (= rho.extract*x_hc) & C.goods_physical > minimum physical consumption of goods (= rho.goods*x_hc)
          if (t>t_thresh) {
            hey = 1### Used for debugging
          }
          
          ## DO NO NEW CALCULATIONS
          hey = 1### Used for debugging
        }
      } ## if (C.extract_physical < consumption_threshold.nature)
      
      ###############################################
      ## Equations for Inventory Coverage under labor-constrained growth
      ###############################################
      ## Targeted consumption of goods (everything consuming in "g_dot" equation)
      targeted_consumption.goods <- C.goods_physical + (I.goods + I.extract)/P.goods_update + Intermediate.goods # Targeted goods consumption
      IC.goods <- (g/CU.goods_delay)/targeted_consumption.goods;        # Perceived inventory coverage for goods sector
      targeted_consumption.nature <- C.extract_physical + Intermediate.nature        # Targeted nature consumption
      IC.extract <- (w_h/CU.extract_delay)/targeted_consumption.nature # Perceived inventory coverage for extraction sector
      
    } ## if (participation_rate <= participation_rate_max)

        
    ####################################################
    ## Equations for population death rate
    #################################################### 
    v.c <- 1-(C.extract_physical/(S*x_hc))                 # Consumption rates of population that defines the famine occurance 
    alpha_hc <- alpha.m + ((pmax(0,v.c,na.rm = TRUE))*(alpha.M - alpha.m)) # Death rate for population

    #######################################################################################
    ## Defining the Phillips Curve as a nonlinear exponential function for wage calculation
    #######################################################################################
    ph_func <- (ph_yval - ph_min)*exp((ph_s/(ph_yval - ph_min)) * (participation_rate - ph_xval)) + ph_min; # Phillips Curve as a nonlinear exponential function
    
    ####################################################
    ### Internal Check if "net saving = net investment" 
    ### Used for debugging
    #################################################### 
    # profit.bank <- (r.L-r.D)*(D.extract+D.goods)
    # HH_saving <- wages.extract + wages.goods + r.D*(D.extract+D.goods) - (C.goods + C.extract) + profit.bank  # Annual household saving
    # NetInvestment <- (I.goods + I.extract) - P.goods_update*delta*(K.goods+K.extract) + (delta_value_inventory[1]+delta_value_inventory[2]) # Net investments from the sectors
    # Firm_saving <- profit.extract_update + profit.goods_update
    # Saving_Investment_check = HH_saving + Firm_saving - NetInvestment
    # if ( is.na(Saving_Investment_check)==TRUE) {
    #   hey = 1
    # }
    # if ( abs(Saving_Investment_check)>1e-12) {
    #   hey = 1
    # }
    
    ################################################################
    ### EQUATIONS FOR STATE/LEVEL VARIABLES IN THE SYSTEM
    ####################################################################################
    ## This section sets the state differential equations that defines the system states   
    ####################################################################################
    
    ## WEALTH
    w_h_dot <- (IC.extract_reference - IC.extract)*targeted_consumption.nature

    ### LABOR PRODUCTIVITY
    a_dot <- alpha*a;                                            # Equation for labor productivity

    ### CAPITAL
    K.goods_dot <- I.goods/P.goods_update - delta*K.goods        # Equation for goods capital
    K.extract_dot <- I.extract/P.goods_update - delta*K.extract  # Equation for extraction capital

    ### POPULATION
    x_hc_dot <- beta_hc*x_hc - alpha_hc*x_hc;                    # Change in population

    ### WAGES
    w_dot <- ph_func*w                                           # Change in real Wage according to the Phillips Curve
    
    ### Equations for a 3rd order delay of "lambda_h" if you select a time, tcritical.lambda_h, 
    ### at which one assumes that more resources become available.
    lambda_h3_dot <- (lambda_h2 - lambda_h3)/(lambda_h.delay/3)  # Equations to incorportae 3rd order delay for nature carrying capacity
    lambda_h2_dot <- (lambda_h1 - lambda_h2)/(lambda_h.delay/3)
    lambda_h1_dot <- (lambda_h.now - lambda_h1)/(lambda_h.delay/3)
    
    ### Equations for Efficiency as function of Cumulative Investment.
    ### These are used if one assumes that the amount of nature needed to
    ### operate capital, eta.Kextract and eta.Kgoods, changes at some time
    ### "tcritical.Inv_Cum_extract" and "tcritical.Inv_Cum_goods"
    if (t > tcritical.Inv_Cum_extract) {
      Inv_Cumu.ext_dot <- I.extract/P.goods_update 
    } else {
      Inv_Cumu.ext_dot <- 0
    }

    if (t > tcritical.Inv_Cum_goods) {
      Inv_Cumu.goods_dot <- I.goods/P.goods_update 
    } else {
      Inv_Cumu.goods_dot <- 0
    }
    
    ### NATURE
    y_h_dot <- lambda_h3_dot + gamma_hc*y_h*(lambda_h - y_h) - nature_extraction; # Change in available nature
    
    ### DEBT
    D.goods_dot <- I.goods - profit.goods_update - P.goods_update*delta*(K.goods)         # Change in goods sector debt
    D.extract_dot <- I.extract - profit.extract_update - P.goods_update*delta*(K.extract)   # Change in extraction sector debt
    debt_ratio <- (D.goods+D.extract)/(VA.goods_update+VA.extract_update)

    ## Extraction sector CAPACITY UTILIZATION (actual)
    CU.extract_dot <- (CU.extract_indicated - CU.extract)/CU.extract_delay # Equation for capacity utilization in extraction sector 
    CU.goods_dot <- (CU.goods_indicated - CU.goods)/CU.goods_delay         # Equation for capacity utilization in goods sector 
    
    ## Perceived INVENTORY COVERAGE
    IC.extract_perceived_dot <- (IC.extract - IC.extract_perceived)/IC.extract_perception_delay # Equation for perceived inventory coverage in extraction sector 
    IC.goods_perceived_dot <- (IC.goods - IC.goods_perceived)/IC.goods_perception_delay         # Equation for perceived inventory coverage in goods sector      
    
    ### PRICES
    P.extract_dot <- (P.extract_update - P.extract)/P.extract_delay  # Equation for extraction sector price updates
    P.goods_dot <- (P.goods_update - P.goods)/P.goods_delay          # Equation for goods sector price updates
    
    ### PROFITS update
    profit.extract_dot <- (profit.extract_update - profit.extract)/profit.extract_delay # Equation for extraction sector profit updates
    profit.goods_dot <- (profit.goods_update - profit.goods)/profit.goods_delay         # Equation for goods sector profit updates

    ### INVENTORY OF GOODS, g
    g_dot <- (IC.goods_reference - IC.goods)*targeted_consumption.goods
    
    ### NET OUTPUT update, Y.update
    Y.extract_dot <- (Y.extract_update - Y.extract)/Y.extract_delay # Equation for net output update for extraction sector
    Y.goods_dot <- (Y.goods_update - Y.goods)/Y.goods_delay         # Equation for net output update for goods sector
    
    ### VALUE ADDED, update
    VA.extract_dot <- (VA.extract_update - VA.extract)/Y.extract_delay # Equation for value added for extraction sector
    VA.goods_dot <- (VA.goods_update - VA.goods)/Y.goods_delay         # Equation for value added for goods sector

    ### VALUE OF INVENTORY, update
    value_inventory.extract_dot <- change_value_inventory.extract/P.extract_delay
    value_inventory.goods_dot <- change_value_inventory.goods/P.goods_delay

    ### CONSUMPTION (of households), update (for tracking so as not to have to recalculate during post-processing)
    C.extract_dot <- (C.extract - C.extract_past)/Y.extract_delay
    C.goods_dot <- (C.goods - C.goods_past)/Y.goods_delay
    
    ### INVESTMENT, update (for tracking so as not to have to recalculate during post-processing)
    I.extract_dot <- (I.extract - I.extract_past)/Y.extract_delay
    I.goods_dot <- (I.goods - I.goods_past)/Y.goods_delay

    ### LABOR, update (for tracking so as not to have to recalculate during post-processing)
    L.extract_dot <- (L.extract - L.extract_past)/Y.extract_delay
    L.goods_dot <- (L.goods - L.goods_past)/Y.goods_delay 
    
    ### The list of all differential equations
    ### The below is the list of all differential equations in the model to be identified by the software
    list(c(a_dot,w_h_dot,K.goods_dot,x_hc_dot,w_dot,y_h_dot,D.goods_dot,
           CU.extract_dot,IC.extract_perceived_dot,
           P.extract_dot,
           CU.goods_dot,IC.goods_perceived_dot,g_dot,
           K.extract_dot,D.extract_dot,P.goods_dot,profit.extract_dot,profit.goods_dot,
           Y.extract_dot,Y.goods_dot,
           VA.extract_dot,VA.goods_dot,
           lambda_h1_dot,lambda_h2_dot,lambda_h3_dot,Inv_Cumu.ext_dot,Inv_Cumu.goods_dot,
           value_inventory.extract_dot,value_inventory.goods_dot,
           C.extract_dot,C.goods_dot,I.extract_dot,I.goods_dot,
           L.extract_dot,L.goods_dot))
} 
## This ends the funcitons with the Odinary Differential Equations
##########################################################################################################

###################################################################################
## Choose Investment Function to use
## type.invest=1: Invest_func.goods <- max(0,inv_kappa0 + inv_kappa1*profit_share.goods)      # Linear investment function for goods sector (must be >= 0)
## type.invest=2: I.goods <- max(0,inv_kappa1*profit.goods + inv_kappa0*P.goods*delta*K.goods)*VA.goods
assign("type.invest",1)     # 1:  

                      
### SETTING PARAMETERS IN THE SYSTEM
################################################################################################
##  This section sets the system parameters that can be varied for sensitivity/scenario analysis 
################################################################################################
assign("lambda_h.max1",1.0e2);       # Nature carrying capacity before "tcritical.lambda_h"
assign("lambda_h.max2",1.0*lambda_h.max1);       # Nature carrying capacity after "tcritical.lambda_h"
assign("tcritical.lambda_h",1000) #115  # The delay time for the 3rd order delay for natuer carrying capacity
assign("lambda_h.delay",40)        # The delay time for the 3rd order delay for natuer carrying capacity
assign("delta_y.1",1.2e-02);       # Depletion (production) factor before "tcritical.lambda_h" (1/[Capital * yr])
assign("delta_y.2",delta_y.1);     # Depletion (production) factor after "tcritical.lambda_h" (1/[Capital * yr])

assign("delta1",0.03);        # Depreciation rate of capital (%/yr)
assign("delta2",delta1);      # Depreciation rate of capital (%/yr)
assign("v1",1.5);             # "Goods capital / gross physical output" ratio
assign("v2",v1*1);            # "Goods capital / gross physical output" ratio
assign("alpha",0.00);         # Rate of increase in labor productivity for goods sector
                              # The extraction sector labor productivity is a function of goods sector labor productivity

## Set factors for defining the technical coefficients of the "A matrix" (input-output matrix)
assign("a_gg",0.1)  # I-O coefficient: goods consumption of goods sector per unit of goods sector output
assign("a_ge",0.2)  # I-O coefficient: goods consumption of extraction sector per unit of extraction sector output 
### Values for different Constants in defining efficiencies (eta) as a function of total investment 
assign("tcritical.Inv_Cum_extract",1000); # Critical time to change the cumulative investment function that changes eta.Kextract
assign("tcritical.Inv_Cum_goods",tcritical.Inv_Cum_extract); # Critical time to change the cumulative investment function that changes eta.Kgoods
assign("eta.Kextract_max",0.16*1.0);      # The maximum value of the logistic curve for extracts
assign("eta.Kextract_s",-0.015);  #-0.015     # The slope of the logistic curve for extracts
assign("eta.Kextract_min",eta.Kextract_max/3);      # The minimum value of the logistic curve for extracts
assign("eta.Kgoods_max",eta.Kextract_max);        # The maximum value of the logistic curve for goods
assign("eta.Kgoods_s",eta.Kextract_s);     # The slope of the logistic curve for goods
assign("eta.Kgoods_min",eta.Kextract_min); # The minimum value of the logistic curve for goods
assign("Inv_Cumu.mid",150);        # The amount of "cumulative invesement" when the logisitic curve is at its inflection point (halfway between upper and lower bound)
assign("nature_per_unit_physical_good",0.1)  # The quantity of nature needed to become a unit of capital

assign("markup.goods",0.07)   # fraction to mark up costs that determines profits, goods sector
assign("markup.extract",markup.goods)   # fraction to mark up costs that determines profits, extraction sector

assign("CU.extract_x",c(0,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.25,1e6))     # Represents the x-axis variable "1/inventory_coverage" for extraction sector capacity utilization lookup table function  
assign("CU.extract_y",c(0,0.30,0.55,0.75,0.85,0.90,0.94,0.98,0.99,1.00,1.0))     # Represents the y-axis variable "CU.extract" for extraction sector capacity utilization lookup table function
assign("CU.goods_x",CU.extract_x)  # Represents the x-axis variable "1/inventory_coverage" for goods sector capacity utilization lookup table function  
assign("CU.goods_y",CU.extract_y)  # Represents the y-axis variable "CU.goods" for goods sector capacity utilization lookup table function
assign("CU.extract_delay",0.3)            # The time delay to use for looking up past Capacity Utilization values
assign("CU.goods_delay",CU.extract_delay) # The time delay to use for looking up past Capacity Utilization values

assign("IC.goods_reference",1)     # Target inventory coverage of goods
assign("IC.extract_reference",1)   # Target inventory coverage of nature
assign("IC.extract_perception_delay",CU.extract_delay)          # The time delay for updating inventory coverage
assign("IC.goods_perception_delay",IC.extract_perception_delay) # The time delay for updating inventory coverage
assign("P.extract_delay",CU.extract_delay)                     # The time delay for updated expected price of nature
assign("P.goods_delay",P.extract_delay)           # The time delay for updated expected price of goods
assign("profit.extract_delay",CU.extract_delay)                # The time delay for updated profits for extraction sector
assign("profit.goods_delay",profit.extract_delay) # The time delay for updated profits for goods sector
assign("Y.extract_delay",CU.extract_delay)                     # The time delay for updated Net output from extraction sector
assign("Y.goods_delay",Y.extract_delay)           # The time delay for updated net output from goods sector

###  Parameters for population dynamics and household (physical) consumption 
assign("beta_hc",0.03);   # Birth rate of the population
assign("gamma_hc",0.01);  # Regeneration rate of nature
assign("alpha.m",0.01);   # Normal (minimimum) death rate per year
assign("alpha.M",0.07);   # Famine (maximum) death rate per year
assign("S",8e-2);         # Subsistence salary per capita (consumption per person when not in famine in nature/(person * yr))
assign("rho.extract",0.010);      # Threshold nature consumption per capita (consumption per person below which there is "famine": nature/(person * yr))
assign("rho.goods",0.0);          # Threshold goods consumption per capita (consumption per person below which is not allowed to happen: goods/(person * yr))

### Values for different Constants in the Phillips Curve (per Keen, 2013)
assign("ph_xval",0.60);   # The minimum value for 'employment rate' which is the x-axis of Phillips curve 
assign("ph_yval",0);      # The maximum value for 'wage share of output percent' which is the y-axis of Phillips curve 
assign("ph_s",0.05*1.0);  # The slope of Phillips curve
assign("ph_min",-0.05);   # The minimum value for 'wage share of output percent' which is the y-axis of Phillips curve 
assign("participation_rate_max",0.80);   # The maximum employment rate allowed (assumes 1 minus this fraction are too old or too young to work)

### Parameters for the investment function
assign("inv_kappa0.1",1.0)   # The intercept for investment function when t < tcritical.lambda_h
assign("inv_kappa1.1",1.5) #*1.2667*1.2667  # The slope for investment function when t < tcritical.lambda_h
assign("inv_kappa0.2",inv_kappa0.1)   # The intercept for investment function when t > tcritical.lambda_h
assign("inv_kappa1.2",inv_kappa1.1)   # The slope for investment function when t > tcritical.lambda_h

### Parameters for Interest Rates calculations (if interest rates will be a function of debt or not)
assign("tcritical.r",10000) # The critical time to change the interest rate parameters
assign("xi.L",0.05);        # The constant for interest rate on loans from firms to banks before 'tcritical.r'
assign("xi2.L",xi.L);       # The constant for interest rate on loans from firms to banks after 'tcritical.r'
assign("phi.L",0.00);       # The linear rate of increase in interest rate per unit of debt before 'tcritical.r'
assign("phi2.L",phi.L);     # The linear rate of increase in interest rate per unit of debt after 'tcritical.r'
assign("xi.D",0.00);        # The constant for interest rate on deposits from banks to households before 'tcritical.r'
assign("xi2.D",xi.D);       # The constant for interest rate on DEPOSITS from banks to households after 'tcritical.r'
assign("phi.D",0.00);       # The linear rate of increase in interest rate per unit of debt before 'tcritical.r'
assign("phi2.D",phi.D);     # The linear rate of increase in interest rate per unit of debt after 'tcritical.r'
################################################################################
## This ends setting of Parameter values
################################################################################


## SETTING INITIAL CONDITIONS
#######################################################################################
## This section sets the initial conditions for the state/level variables in the system
#######################################################################################
  
### Interest rate
r.L_o = xi.L  # Initial interest rate

## Initial population
x_hc_o <- 30;            # Initial population

## Initial nature
y_h_o <- lambda_h.max1;  # Initial available nature and is set equal to the maximum size of nature with no depletion

### Inventory Coverage
IC.extract_perceived_o <- 1  # Initial value for perceived inventory coverage for extraction sector
IC.goods_perceived_o <- 1    # Initial value for perceived inventory coverage for goods sector

### Initial Capacity Utilization
CU.extract_1 <- approx(CU.extract_x,CU.extract_y,1/IC.extract_perceived_o);
CU.extract_o <- CU.extract_1$y;   # Initial capacity utilization for extraction sector
CU.goods_1 <- approx(CU.goods_x,CU.goods_y,1/IC.goods_perceived_o);
CU.goods_o <- CU.goods_1$y;       # Initial capacity utilization for goods sector

### Initial Capital
K.extract_o <- 1.6*0.8           # Initial extraction capital
K.goods_o <- 4.4*0.5             # Initial goods capital
K_o <- K.extract_o + K.goods_o  # Initial total capital

### Initial Gross Output, X
nature_extraction_o <- delta_y.1*K.extract_o*CU.extract_o*y_h_o  # Initial nature extraction calculation
X.extract_o <- nature_extraction_o                               # Initial gross extract output set equal to initial nature extraction 
goods_output_o <- K.goods_o*CU.goods_o/v1                        # Initial goods stock
X.goods_o <- goods_output_o                                      # Initial gross goods output
X_o <- matrix(c(X.goods_o,X.extract_o),nrow=2,ncol=1,byrow = TRUE)         # Matirx representation for initial total gross outputs from extraction & goods sectors
X_o_hat <- matrix(c(X.goods_o,0,0,X.extract_o),nrow=2,ncol=2,byrow = TRUE) # Hat matrix for initial gross goods and gross extracts

### Initial Labor 
a_o <- 1;                                 # Initial value for labor productivity
L_o.goods <- X.goods_o/a_o                # Intial goods sector labor
a.extract_o <- a_o                        # Initial extraction labor productivity as a function of initial goods labor productivity 
L_o.extract <- X.extract_o/a.extract_o    # Intial extraction sector labor
L_o <- L_o.goods + L_o.extract            # Initial total labor 

### Initial Wage and Wage share
w_o <- 1.0 # Initial wage per capita 
wages.goods_o <- w_o*L_o.goods
wages.extract_o <- w_o*L_o.extract

### Debt
D.goods_o <- 0      # Initial debt for goods sector
D.extract_o <- 0    # Initial debt for extraction sector

### Cumulative investments that determines how the eta.Kextract and eta.Kgoods Change
Inv_Cumu.ext_o <- 0    # Initial cumulative investments for extraction sector
Inv_Cumu.goods_o <- 0  # Initial cumulative investments for goods sector

### Initial price
Vtilde.goods_o <- (wages.goods_o + r.L_o*D.goods_o)/X.goods_o # Normalized value added  for goods sector
Vtilde.extract_o <- (wages.extract_o + r.L_o*D.extract_o)/X.extract_o               # Normalized value added  for extraction sector
Vtilde_o <- matrix(c(Vtilde.goods_o,Vtilde.extract_o),nrow=2,ncol=1,byrow=TRUE)  # Vector of initial "Vtilde" values for goods and extract sector
Depreciation_hat_o <- matrix(c(delta1*K.goods_o/X.goods_o,0,delta1*K.extract_o/X.extract_o,0),nrow=2,ncol=2,byrow=TRUE) # Matrix of initial values for goods and extract sector capital depreciations 
Atilde_o.ee <- eta.Kextract_max/(delta_y.1*y_h_o) # The quantity of extraction sector's input to produce one physical unit of extraction sector's output 
Atilde_o.eg <- eta.Kgoods_max*v1  # This removes the nature consumption for making investment goods
Atilde_o.ge <- a_ge  # The quantity of goods sector's input to produce one physical unit of extraction sector's output
Atilde_o.gg <- a_gg  # The quantity of goods sector's input to produce one physical unit of goods sector's output
Atilde_o <- matrix(c(Atilde_o.gg,Atilde_o.ge,Atilde_o.eg,Atilde_o.ee),nrow = 2,ncol = 2,byrow = TRUE)
Atilde_o_transpose <- t(Atilde_o) 
Markup_hat <- matrix(c(1/(1+markup.goods),0,0,1/(1+markup.extract)),nrow=2,ncol=2,byrow = TRUE)
P_o <- solve(Markup_hat - Atilde_o_transpose - Depreciation_hat_o)%*%(Vtilde_o)  # Update of prices matrix
P.goods_o = P_o[1]    # Initial price of goods in $/nature units
P.extract_o = P_o[2]  # Initial price of nature in $/nature units


### Stocks of wealth and goods
A.ee <- eta.Kextract_max/(delta_y.1*y_h_o) # A.ee calculated with initial value for available nature
A.eg <- eta.Kgoods_max*v1 + nature_per_unit_physical_good*v1  # The the quantity of extraction sector's input to produce one physical unit of goods sector's output: "nature_per_unit_physical_good" is operating input; " nature_per_unit_physical_good" is nature embodied in physical capital
A.ge <- a_ge                               # Set as constant
A.gg <- a_gg                               # Set as constant
A_o <- matrix(c(A.gg,A.ge,A.eg,A.ee),nrow=2,ncol=2,byrow = TRUE)  # Initial values assigned for technical coefficients matrix
Intermediate_o <- A_o%*%X_o                                       # Initial values for intermediate consumption in physical units
Intermediate.goods_o <- Intermediate_o[1]                         # Initial values for intermediate goods consumption in physical units
Intermediate.nature_o <- Intermediate_o[2]                        # Initial values for intermediate extract consumption in physical units which includes the nature needed for investment since that is included in A matrix
Phat_o <- matrix(c(P.goods_o,0,0,P.extract_o),nrow=2,ncol=2,byrow = TRUE)
XX_o.gg <- P.goods_o*A.gg*X.goods_o     # Initial values for I-O transactions matrix in money units
XX_o.ge <- P.goods_o*A.ge*X.extract_o   # Initial values for I-O transactions matrix in money units
XX_o.eg <- P.extract_o*A.eg*X.goods_o   # Initial values for I-O transactions matrix in money units
XX_o.ee <- P.extract_o*A.ee*X.extract_o # Initial values for I-O transactions matrix in money units

### Initial costs (per unit of output) &  Profits
## Cost of inventory (= intermediate costs + wages + interest payments)
cost.extract_o <- P.extract_o*A.ee + P.goods_o*A.ge + w_o*L_o.extract/X.extract_o 
cost.goods_o <- P.goods_o*A.gg + P.extract_o*A.eg + w_o*L_o.goods/X.goods_o 
profit.goods_o <- (markup.goods)*cost.goods_o*X.goods_o          # Goods sector Profit updates (in units of money) is (1+markup)*(unit cost)*(gross output)
profit.extract_o <- (markup.extract)*cost.extract_o*X.extract_o  # Extraction sector Profit updates (in units of money) is (1+markup)*(unit cost)*(gross output)

### Initial Value Added
## Value added is the sum of profits, wages, interest payments and depreciation as in Input-Output matix
VA.goods_o <- profit.goods_o + wages.goods_o + r.L_o*D.goods_o + P.goods_o*delta1*K.goods_o # Initial value added of goods sector
VA.extract_o <- profit.extract_o + wages.extract_o + r.L_o*D.extract_o +  P.goods_o*delta1*K.extract_o  # Initial value added of extraction sector
VA_o <- VA.goods_o + VA.extract_o # Total initial value added for goods and extraction sectors

### Initial Profit share and Profit rate
profit_rate.goods_o <- profit.goods_o/(P.goods_o*K.goods_o)       # Initial goods sector profit rate                                       
profit_rate.extract_o <- profit.extract_o/(P.goods_o*K.extract_o) # Initial extraction sector profit rate 
profit_share.goods_o <- profit.goods_o/VA.goods_o                 # Initial goods sector profit share out of initial goods value added
profit_share.extract_o <- profit.extract_o/VA.extract_o           # Initial extraction sector profit share out of initial extractions value added


### Intial Investment
## The initial investment is assumed to equal depreciation
if (type.invest == 1) {
  I.goods_o <- max(0,inv_kappa1.1*profit.goods_o + inv_kappa0.1*P.goods_o*delta1*K.goods_o)
  I.extract_o <- max(0,inv_kappa1.1*profit.extract_o + inv_kappa0.1*P.goods_o*delta1*K.extract_o)
} else if (type.invest == 2) {
  Invest_func.goods_o <- max(0,inv_kappa0.1 + inv_kappa1.1*profit_rate.goods_o)      # Linear investment function for goods sector (must be >= 0)
  Invest_func.extract_o <- max(0,inv_kappa0.1 + inv_kappa1.1*profit_rate.extract_o)  # Linear investment function for extraction sector (must be >= 0)
  I.goods_o <- max(0,Invest_func.goods_o*VA.goods_o)        # Total gross investment in goods sector with the criterion of investment > 0
  I.extract_o <- max(0,Invest_func.extract_o*VA.extract_o)  # Total gross investment in extraction sector with the criterion of investment > 0
}

### Net output, Y (money units)
Y_o <- Phat_o%*%X_o - Phat_o%*%A_o%*%X_o  # Matrix for initial net output
Y_o.goods <- Y_o[1]                       # Initial net goods output
Y_o.extract <- Y_o[2]                     # Initial net extracts output

## Houseold consumption (assume no initial change in value of inventories)
C.extract_o <- Y_o.extract
C.goods_o <- Y_o.goods - I.extract_o - I.goods_o

### wealth and goods (initial stocks)
targeted_consumption.nature_o <- nature_extraction_o        # Initial targeted nature consumption
w_h_o <- targeted_consumption.nature_o/(1/CU.extract_delay) # Initial wealth

targeted_consumption.goods_o <- goods_output_o              # Initial targeted goods consumption which is set equal to the initial goods output
g_o <- targeted_consumption.goods_o/(1/CU.goods_delay)      # Initial goods

## value of inventory = (cost per unit output)*(units of inventory)
value_inventory.goods_o <- cost.goods_o*g_o
value_inventory.extract_o <- cost.extract_o*w_h_o

### Nature carrying capacity (lambda_h) with delay
lambda_h1_o <- lambda_h.max1  # Setting for 3rd order delay function for nature carrying capacity
lambda_h2_o <- lambda_h1_o
lambda_h3_o <- lambda_h2_o

## The below code initializes the time and the initial state variable values for the ODE system
yini <-  c(a=a_o, w_h=w_h_o,K.goods=K.goods_o,x_hc=x_hc_o,w=w_o,y_h=y_h_o,D.goods=D.goods_o,
           CU.extract=CU.extract_o,
           IC.extract_perceived=IC.extract_perceived_o,
           P.extract=P.extract_o,
           CU.goods=CU.goods_o,
           IC.goods_perceived=IC.goods_perceived_o,
           g=g_o,K.extract=K.extract_o,D.extract=D.extract_o,
           P.goods=P.goods_o,profit.extract=profit.extract_o,profit.goods=profit.goods_o,
           Y.extract=Y_o.extract,Y.goods=Y_o.goods,
           VA.extract=VA.extract_o,VA.goods=VA.goods_o,
           lambda_h1=lambda_h1_o,lambda_h2=lambda_h2_o,lambda_h3=lambda_h3_o,Inv_Cumu.ext=Inv_Cumu.ext_o,Inv_Cumu.goods=Inv_Cumu.goods_o,
           value_inventory.extract=value_inventory.extract_o,value_inventory.goods=value_inventory.goods_o,
           C.extract_past=C.extract_o,C.goods_past=C.goods_o,
           I.extract_past=I.extract_o,I.goods_past=I.goods_o,
           L.extract_past=L_o.extract,L.goods_past=L_o.goods)
times <- seq(from = 0, to = 160, by = 0.1);
#out <- ode(times = times, y = yini, func = goodwinhandy, method = c("ode45"), parms = NULL);
out <- ode(times = times, y = yini, func = goodwinhandy, method = c("rk4"), parms = NULL);

##########################################################################################################

### OUTPUT FOR DIFFERENT VARIABLES

############################################################
## This section generates the output for the simulated model 
############################################################
time <- out[,1]
labor_productivity <- out[,2]    # Labor productivity
wealth <- out[,3]                # Wealth
K.goods <- out[,4]               # Goods capital
population <- out[,5]            # Population
w <- out[,6]                     # Wage per person
nature <- out[,7]                # Available nature
D.goods <- out[,8]               # Debts of goods sector
CU.extract <- out[,9]            # Capacity Utilization of extraction capital
IC.extract_perceived <- out[,10] # Perceived inventory coverage of extractions
P.extract <- out[,11]            # Price of extraction output for last time period
CU.goods <- out[,12]             # Capacity Utilization of goods capital
IC.goods_perceived <- out[,13]   # Perceived inventory coverage of goods
g <- out[,14]                    # stock of goods (inventory)  
K.extract <- out[,15]            # Extraction capital
D.extract <- out[,16]            # Debts of extraction sector
P.goods <- out[,17]              # Price of goods output for last time period
profit.extract <- out[,18]       # Profits of the extraction sector (last time period)
profit.goods <- out[,19]         # Profits of the goods sector (last time period)
Y.extract <- out[,20]            # Net extraction sector output
Y.goods <- out[,21]              # Net goods sector output
VA.extract <- out[,22]           # Value added for extraction sector
VA.goods <- out[,23]             # Value added for goods sector
lambda_h1 <- out[,24]            # Value of each stock
lambda_h2 <- out[,25]            # Value of each stock
lambda_h3 <- out[,26]            # Value of each stock
Inv_Cumu.ext <- out[,27]         # Cumulative investments in extraction sector
Inv_Cumu.goods <- out[,28]       # Cumulative investments in goods sector
value_inventory.extract <- out[,29]     # Value of inventory of extraction sector
value_inventory.goods <- out[,30]       # Value of inventory of goods sector
C.extract_sim <- out[,31]        # houshold consumption of extraction sector output
C.goods_sim <- out[,32]          # houshold consumption of extraction sector output
I.extract_sim <- out[,33]        # houshold consumption of extraction sector output
I.goods_sim <- out[,34]          # houshold consumption of extraction sector output
L.extract_sim <- out[,35]        # houshold consumption of extraction sector output
L.goods_sim <- out[,36]          # houshold consumption of extraction sector output
######################################################

###########################################################################################
###
### POST - PROCESSING
### This post-processing section recalculates the needed variables for analysis and plotting
###
###########################################################################################

## Nature (or resources) carrying capacity
lambda_h <- lambda_h3

### Gross output
delta_y <- rep(0,length(time))
eta.Kextract <- rep(0,length(time))
eta.Kgoods <- rep(0,length(time))
inv_kappa0 <- rep(0,length(time))
inv_kappa1 <- rep(0,length(time))
delta <- rep(0,length(time))
v <- rep(0,length(time))
if (time[length(time)] > tcritical.lambda_h) {
  delta_y[1:which(time==tcritical.lambda_h)] <- delta_y.1
  delta_y[(which(time==tcritical.lambda_h)+1):length(time)] <- delta_y.2
  inv_kappa0[1:which(time==tcritical.lambda_h)] <- inv_kappa0.1
  inv_kappa0[(which(time==tcritical.lambda_h)+1):length(time)] <- inv_kappa0.2
  inv_kappa1[1:which(time==tcritical.lambda_h)] <- inv_kappa1.1
  inv_kappa1[(which(time==tcritical.lambda_h)+1):length(time)] <- inv_kappa1.2
  delta[1:which(time==tcritical.lambda_h)] <- delta1
  delta[(which(time==tcritical.lambda_h)+1):length(time)] <- delta2
  v[1:which(time==tcritical.lambda_h)] <- v1
  v[(which(time==tcritical.lambda_h)+1):length(time)] <- v2
  } else {
  delta_y <- rep(delta_y.1,length(time))
  inv_kappa0 <- rep(inv_kappa0.1,length(time))
  inv_kappa1 <- rep(inv_kappa1.1,length(time))
  delta <- rep(delta1,length(time))
  v <- rep(v1,length(time))
 }

### Total Net output and Value Added
Y.total <- Y.goods + Y.extract     # Total net output
VA.total <- VA.goods + VA.extract  # Total Value added

### Capital
capital <- K.goods + K.extract # Total capital which is the sum of goods capital and extraction capital

### Debt
D.total <- D.goods + D.extract              # Total debt
debt_ratio <- D.total/(VA.goods+VA.extract) # Debt ratio as a function of total debts, and extraction and goods sectors value added
debt_ratio.goods <- D.goods/VA.goods        # Debt ratio for goods sector as a function of goods sector debts and value added
debt_ratio.extract <- D.extract/VA.extract  # Debt ratio for extraction sector as a function of extraction sector debts and value added

### Profit rate
profit_rate.goods <- profit.goods/(P.goods*K.goods)       # Goods sector profit rate as a function of goods sector profits, price and capital
profit_rate.extract <- profit.extract/(P.goods*K.extract) # Extraction sector profit rate as a function of extraction sector profits and capital, and goods sector price
profit_rate <- (profit.goods + profit.extract)/(K.goods + K.extract) # Total profit rate

### Profit share
profit_share.goods <- profit.goods/VA.goods       # Goods sector profit share as a function of goods sector profits and value added
profit_share.extract <- profit.extract/VA.extract # Extraction sector profit share as a function of extraction sector profits and value added
profit_share <- (profit.goods + profit.extract)/(VA.goods + VA.extract) # Total share

###  Use Investment from differential equation that treats investment as a lagged variable
I.goods <- I.goods_sim
I.extract <- I.extract_sim

###  Use Consumption from differential equation that treats investment as a lagged variable
C.goods <- C.goods_sim
C.extract <- C.extract_sim
### Back calculate consumption (optional method)
# C.goods[1] = Y.goods[1] - I.goods[1] - I.extract[1]  ## assume no change in inventory for the start of simulation
# C.goods[2:length(time)] = Y.goods[2:length(time)] - I.goods[2:length(time)] - I.extract[2:length(time)] - (value_inventory.goods[2:length(time)] - value_inventory.goods[1:length(time)-1])
# C.extract[1] = Y.extract[1]   ## assume no change in inventory for the start of simulation
# C.extract[2:length(time)] = Y.extract[2:length(time)] - (value_inventory.extract[2:length(time)] - value_inventory.extract[1:length(time)-1])
# wealth_threshold <- rho*population
# C.extract_physical <- pmin(1,(wealth/wealth_threshold))*C.extract/P.extract      # Physical consumption in (money consumption of nature)/($/nature) units

## Get total gross output (physical)
nature_extraction <- delta_y*nature*K.extract*CU.extract  # Nature extraction as a function of Capital
C.extract_physical <- C.extract/P.extract     # Physical (household) consumption of nature
C.goods_physical <- C.goods/P.goods     # Physical (household) consumption of goods
X.extract <- nature_extraction  # Gross extracts set equal to nature extractions
X.goods <- K.goods*CU.goods/v   # Gross goods as a function of goods sector capital, goods sector capacity utilisation, and goods sector capital to output ratio

###  Back calculate labor and wages [money units] 
a.extract <- labor_productivity  # Converts labor productivity of goods to labor productivity of extraction
# L.goods <- X.goods/labor_productivity   # Labor for goods (not using the labor differential equation)
# L.extract <- X.extract/a.extract        # Labor for extraction (not using the labor differential equation)
L.goods <- L.goods_sim                # Labor for goods (using the labor differential equation)
L.extract <- L.extract_sim            # Labor for extraction (using the labor differential equation)
L <- L.goods + L.extract                # Total labor
participation_rate <- L/population      # Employment rate 
wages.goods <- w*L.goods                # Total wages for goods sector labor 
wages.extract <- w*L.extract            # Total wages for extraction sector labor 
wages <- wages.goods + wages.extract    # Total wages for goods and extraction sector
wage_share <- (wages.extract + wages.goods)/(VA.goods+VA.extract) # Total wage share for goods and extraction sector


### Solving for interest rate for post-processing
if (is.na(which(time>tcritical.r)[1])==TRUE) {
  xi_now.L <- rep(1,length(time))*xi.L
  phi_now.L <- rep(1,length(time))*phi.L
  xi_now.D <- rep(1,length(time))*xi.D
  phi_now.D <- rep(1,length(time))*phi.D
} else {
  ind <- min(which(time>tcritical.r))
  xi_now.L <- c(rep(1,ind)*xi.L,rep(1,(length(time)-ind))*xi2.L)
  phi_now.L <- c(rep(1,ind)*phi.L,rep(1,(length(time)-ind))*phi2.L)
  xi_now.D <- c(rep(1,ind)*xi.D,rep(1,(length(time)-ind))*xi2.D)
  phi_now.D <- c(rep(1,ind)*phi.D,rep(1,(length(time)-ind))*phi2.D)
}
r.L = xi_now.L*rep(1,length(time))+ phi_now.L*D.total/VA.total  # Set as constant interest rate
r.D = xi_now.D*rep(1,length(time))+ phi_now.D*D.total/VA.total  # Set as constant interest rate

### Solving for inflation, Consumer Price Index, and GDP Deflator
inflation <- c(0,diff(P.goods)  /P.goods[2:length(time)]  *C.goods[2:length(time)]  /(C.goods[2:length(time)]+C.extract[2:length(time)]) + 
                 diff(P.extract)/P.extract[2:length(time)]*C.extract[2:length(time)]/(C.goods[2:length(time)]+C.extract[2:length(time)]))
r.real <- r.L-inflation     ## Real interest rate
GDP_deflator <- (Y.goods + Y.extract) /
  (P.goods[1]*(Y.goods/P.goods) + P.extract[1]*(Y.extract/P.extract))
CPI <- rep(0,length(time))  ## initialize consumer price index (CPI)
for (i in 1:length(time)) {
  if (i == 1) {
    CPI[i] <- 1   ## initial Consumer Price Index = 1 in year 0
  } else {
    CPI[i] <- CPI[i-1]*(1+inflation[i])  ## CPI = product of (1+inflation) from each time step
  } ## if (i == 1) {
} ## for (i in 1:length(time)) {

### Real economic variables
## Real prices
P.goods_real <- P.goods/CPI
P.extract_real <- P.extract/CPI
## Real wage and total wages
w_real <- w/CPI                                     # Real wage per person
wages.goods_real <- w_real*L.goods                  # Total wages for goods sector labor from last time period
wages.extract_real <- w_real*L.extract              # Total wages for extraction sector labor from last time period
wages_real <- wages.goods_real + wages.extract_real # Total wages for goods and extraction sector
## Real net output and value added
Y.goods_real <- Y.goods/GDP_deflator
Y.extract_real <- Y.extract/GDP_deflator
Y.total_real <- Y.goods_real + Y.extract_real
VA.goods_real <- VA.goods/GDP_deflator
VA.extract_real <- VA.extract/GDP_deflator
VA.total_real <- VA.goods_real + VA.extract_real
### Real value of capital, investment, and debt
I.goods <- I.goods_sim
I.extract <- I.extract_sim
I.goods_real <- I.goods/GDP_deflator
I.extract_real <- I.extract/GDP_deflator
I.total_real <- I.extract_real + I.goods_real
D.goods_real <- D.goods/GDP_deflator
D.extract_real <- D.extract/GDP_deflator
D.total_real <- D.goods_real + D.extract_real
### Real consumption
C.goods_real <- C.goods/GDP_deflator
C.extract_real <- C.extract/GDP_deflator
C.total_real <- C.goods_real + C.extract_real

### Calculate intermediate consumption and net output, Y (money units)
Eye <- matrix(c(1,0,0,1),nrow=2,ncol=2,byrow = TRUE)  # Identity matrix
Intermediate.goods <- rep(0,length(time))
Intermediate.nature <- rep(0,length(time))
Intermediate.gg <- rep(0,length(time))
Intermediate.ge <- rep(0,length(time))
Intermediate.eg <- rep(0,length(time))
Intermediate.ee <- rep(0,length(time))
cost.extract <- rep(0,length(time))
cost.goods <- rep(0,length(time))
Y.goods_update <- rep(0,length(time))
Y.extract_update <- rep(0,length(time))
L.goods_check <- rep(0,length(time))
L.extract_check <- rep(0,length(time))
C.goods_YminusI <- rep(0,length(time))
C.extract_YminusI <- rep(0,length(time))
change_value_inv.goods <- rep(0,length(time))
change_value_inv.extract <- rep(0,length(time))
XX.gg <- rep(0,length(time))
XX.eg <- rep(0,length(time))
XX.ge <- rep(0,length(time))
XX.ee <- rep(0,length(time))
a_ge.max <- rep(0,length(time))
nature_per_net_nature <- rep(0,length(time))
nature_per_net_good <- rep(0,length(time))
for (i in 1:length(time)) {
  ### Calculate the amount of nature needed to operate capital (eta.Kextract, eta.Kgoods), to use for later calculations
  if (time[i] > tcritical.Inv_Cum_extract) {
    eta.Kextract[i] <- (eta.Kextract_min - eta.Kextract_max)/(1 + exp(eta.Kextract_s * (Inv_Cumu.ext[i] - Inv_Cumu.mid))) + eta.Kextract_max;
  } else {
    eta.Kextract[i] <- eta.Kextract_max
  }
  if (time[i] > tcritical.Inv_Cum_goods) {
    eta.Kgoods[i] <- (eta.Kgoods_min - eta.Kgoods_max)/(1 + exp(eta.Kgoods_s * (Inv_Cumu.goods[i] - Inv_Cumu.mid))) + eta.Kgoods_max;
  } else {
    eta.Kgoods[i] <- eta.Kgoods_max
  }
  X <- matrix(c(X.goods[i],X.extract[i]),nrow=2,ncol=1,byrow = TRUE)  
  A.ee <- eta.Kextract[i]/(delta_y[i]*nature[i])
  A.eg <- eta.Kgoods[i]*v[i] + nature_per_unit_physical_good*v[i] # For now, no intermediate demand for goods, so Y.goods = Q.goods (or net output of goods = gross output of goods)
  A.ge <- a_ge  # A constant; This should somehow relate to I-O matrix in money units where x.eg = x.ge should hold (the diagonal terms are equal)
  A.gg <- a_gg  # A constant
  a_ge.max[i] <- (X.goods[i]-X.goods[i]*a_gg-delta[i]*K.goods[i])*(X.extract[i]-eta.Kextract[i]*K.extract[i]*CU.extract[i])/(X.extract[i]*eta.Kgoods[i]*K.goods[i]*CU.goods[i]) + delta[i]*K.extract[i]/X.extract[i]
  A <- matrix(c(A.gg,A.ge,A.eg,A.ee),nrow=2,ncol=2,byrow = TRUE)        # Technical coefficients matrix
  Phat <- matrix(c(P.goods[i],0,0,P.extract[i]),nrow=2,ncol=2,byrow = TRUE)
  Y <- Phat%*%X - Phat%*%A%*%X              # Net output vector in money units
  Y.physical <- X - A%*%X                   # Net output vector in physical units
  I <- matrix(c(I.goods[i]+I.extract[i],0),nrow=2,ncol=1,byrow = TRUE)  # Investement vector in units of goods
  cost.extract[i] <- P.extract[i]*A.ee + P.goods[i]*A.ge + wages.extract[i]/X.extract[i]
  cost.goods[i] <- P.goods[i]*A.gg + P.extract[i]*A.eg + wages.goods[i]/X.goods[i]
  PP <- matrix(c(P.goods[i],P.extract[i]),nrow=1,ncol=2,byrow = TRUE)
  Intermediate <- A%*%X                     # Intermediate consumption in physical units
  Intermediate.goods[i] <- Intermediate[1]
  Intermediate.nature[i] <- Intermediate[2] # This includes the nature needed for investment since that is included in A matrix
  Intermediate.gg[i] <- A.gg*X[1]
  Intermediate.ge[i] <- A.ge*X[2]
  Intermediate.eg[i] <- A.eg*X[1]
  Intermediate.ee[i] <- A.ee*X[2]
  Y.goods_update[i] <- Y[1]
  Y.extract_update[i] <- Y[2]
  Xhat <- matrix(c(X.goods[i],0,0,X.extract[i]),nrow = 2,ncol = 2,byrow = TRUE)  # A 2x2 matrix with gross outputs on the diagonal
  change_value_inventory <- matrix(c(change_value_inv.goods[i],change_value_inv.extract[i]),nrow=2,ncol=1,byrow = TRUE)
  C <- Y - I - change_value_inventory  # Vector of Consumer consumption in money units
  C.goods_YminusI[i] <- C[1]
  C.extract_YminusI[i] <- C[2]
  XX.gg[i] <- P.goods[i]*A.gg*X.goods[i]     ## I-O transactions matrix in money units
  XX.ge[i] <- P.goods[i]*A.ge*X.extract[i]   ## I-O transactions matrix in money units
  XX.eg[i] <- P.extract[i]*A.eg*X.goods[i]   ## I-O transactions matrix in money units
  XX.ee[i] <- P.extract[i]*A.ee*X.extract[i] ## I-O transactions matrix in money units
  
  ## Calculate per-sector nature_intensities using I-O matrix approach: epsilon = Nature input*(Xhat-X)^(-1)
  nature_hat <- matrix(c(0,0,0,nature_extraction[i]),nrow=2,ncol=2,byrow = TRUE)
  XX <- matrix(c(Intermediate.gg[i],Intermediate.ge[i],Intermediate.eg[i],Intermediate.ee[i]),nrow = 2,ncol = 2,byrow = TRUE)
  nature_intensity_matrix <- nature_hat%*%solve(Xhat)%*%solve(Eye-A)
  nature_per_net_good[i] <- nature_intensity_matrix[2,1]  ## Gross Nature Extracted / net good output
  nature_per_net_nature[i] <- nature_intensity_matrix[2,2] ## Gross Nature Extracted / net nature output
  }

## Change in value of inventory
change_value_inventory.goods <- c(0,(value_inventory.goods[2:length(time)] - value_inventory.goods[1:length(time)-1]))
change_value_inventory.extract <- c(0,(value_inventory.extract[2:length(time)] - value_inventory.extract[1:length(time)-1]))

### Consumption from the Transactions Flow Table
C.goods_alt <- profit.goods+r.L*D.goods+P.goods*delta*K.goods+wages.goods-XX.ge+XX.eg-I.extract-I.goods - c(0,(value_inventory.goods[2:length(time)] - value_inventory.goods[1:length(time)-1]))
C.extract_alt <- profit.extract+r.L*D.extract+P.goods*delta*K.extract+wages.extract+XX.ge-XX.eg - c(0,(value_inventory.extract[2:length(time)] - value_inventory.extract[1:length(time)-1]))

X.goods_check_VA <- XX.gg + XX.eg + VA.goods
X.extract_check_VA <- XX.ee + XX.ge + VA.extract
X.goods_check_Y <- XX.gg + XX.ge + C.goods + I.goods + I.extract + change_value_inventory.goods
X.extract_check_Y <- XX.ee + XX.eg + C.extract + change_value_inventory.extract
X.goods_check_Yalt <- XX.gg + XX.ge + C.goods_alt + I.goods + I.extract + change_value_inventory.goods
X.extract_check_Yalt <- XX.ee + XX.eg + C.extract_alt + change_value_inventory.extract


### Interest and Bank Share
interest_share <- r.L*debt_ratio  # Share of value added paid to interest by firms to banks and households
profit.bank <- (r.L-r.D)*D.total  # Profits of banks as a function of interest rate on loans, interest rate on deposits and total debts
profit_share.bank <- (r.L-r.D)*D.total/VA.total # Profit share of banks which is profit.banks/total value added

### Depreciation (total in economy)
depreciation_total <- P.goods*delta*(K.goods+K.extract)

### Household Saving and Net Investment
HH_saving <- wages.extract + wages.goods + r.D*(D.extract+D.goods) - (C.goods + C.extract) + profit.bank - (change_value_inventory[1]+change_value_inventory[2]) # Annual household saving
NetInvestment <- (I.goods + I.extract) - P.goods*delta*(K.goods+K.extract) + 0*(change_value_inventory[1]+change_value_inventory[2]) # Net investments from the sectors
Firm_saving <- profit.extract + profit.goods
Saving_Investment_check = HH_saving + Firm_saving - NetInvestment

HH_saving_alt <- wages.extract + wages.goods + r.D*(D.extract+D.goods) - (C.goods_alt + C.extract_alt) + profit.bank
Saving_Investment_check_alt = HH_saving_alt + Firm_saving - NetInvestment

### Net Power metrics    
nature_to_operate_Ke <- Intermediate.ee    # Nature input to operate extraction capital
nature_to_invest_Ie <- nature_per_net_good*(I.extract/P.goods)   # Nature for extraction sector investments
net_nature_extraction <- nature_extraction - nature_to_operate_Ke - nature_to_invest_Ie  # Net nature output from Extraction sector and does not account for 100% of intermediate nature consumption
net_nature_extraction_direct <- nature_extraction - nature_to_operate_Ke  # Net nature output from Extraction sector and does not account for 100% of intermediate nature consumption
NEPR <- net_nature_extraction/(nature_to_operate_Ke + nature_to_invest_Ie)  # Net External Power Ratio
NEPR_direct <- net_nature_extraction_direct/(nature_to_operate_Ke)  # Net External Power Ratio
GEPR <- nature_extraction/(nature_to_operate_Ke + nature_to_invest_Ie)      # Gross External Power Ratio
GEPR_direct <- nature_extraction/(nature_to_operate_Ke)      # Gross External Power Ratio
NPR <- 1/(nature_per_net_nature - 1) # Net Power Ratio = 1/(nature_per_net_nature - 1)
GPR <- nature_per_net_nature/(nature_per_net_nature - 1) # Net Power Ratio = 1/(nature_per_net_nature - 1)

### Spending on natural resources, nature intensity (and comparative ratios) 
nature_spending_per_VA <- (XX.ee+XX.eg+C.extract)/(VA.total)             # Nature spending per value added
extraction_sector_spending_per_VA <- (XX.ee+XX.ge+I.extract)/(VA.total)  # It is the extraction sector spending to create a unit of value added
Yext_per_Ytotal <- Y.extract/Y.total          # Share of net extraction output out of total net output i.e. net output from extraction and goods sectors
VAext_per_VAtotal <- VA.extract/VA.total      # The share of extraction sector value added out of the total goods and extraction sector value added
nature_intensity <- nature_extraction/Y.total # The units of nature extracted per unit of total net output 
nature_intensity_real <- nature_extraction/(Y.total/GDP_deflator) # The units of nature extracted per unit of total net output 

## DEATH RATES
v.c <- rep(1,length(time)) - (C.extract_physical/(S*population))                                               # Consumption rates of population that defines the famine occurance 
alpha_hc <- alpha.m + ((pmax(0,v.c,na.rm = TRUE))*(alpha.M - alpha.m)) # Death rate for population

## Per capita calculations
nature_extraction_per_person <- nature_extraction/population
nature_consumption_per_person <- C.extract_physical/population
goods_physical_consumption_per_person <- C.goods/P.goods/population

## Elasticities of factor inputs
elasticity.K <- c(0,diff(Y.total)/diff(capital))*capital/Y.total
elasticity.L <- c(0,diff(Y.total)/diff(L.extract+L.goods))*(L.extract+L.goods)/Y.total
elasticity.nature <- c(0,diff(Y.total)/diff(nature_extraction))*(nature_extraction)/Y.total

#######################################################################################################
### CODES TO WRITE DATA TO FILE
##################################################################
## The below code will write and save data to file in .csv format
##################################################################
# init2 <- out[length(time),] ## To save final state if wanting to use it as initial conditions in another simulation
# data_to_write = data.frame(time,population,labor_productivity,wealth,
#                            K.goods,K.extract,L.goods,L.extract,participation_rate,
#                            w,nature,nature_extraction,debt_ratio,capital,wage_share,
#                            Y.goods,Y.extract,I.goods,I.extract, C.extract,C.goods,
#                            profit.goods,profit.extract,profit_share.goods,profit_share.extract,
#                            profit_rate.goods,profit_rate.extract,NEPR,HH_saving,NetInvestment,
#                            D.goods,D.extract,XX.gg,XX.ge,XX.eg,XX.ee,net_nature_extraction,
#                            IC.extract_perceived,IC.goods_perceived,CU.goods,CU.extract,
#                            VA.goods,VA.extract,a.extract,VA.total,Y.total,P.goods,P.extract,
#                            interest_share,profit.bank,profit_share.bank,r.L,r.D,lambda_h,
#                            nature_spending_per_VA,extraction_sector_spending_per_VA,
#                            Yext_per_Ytotal, VAext_per_VAtotal,eta.Kextract,eta.Kgoods,
#                            inv_kappa0,inv_kappa1,delta,nature_intensity,C.extract_alt,C.goods_alt,
#                            Firm_saving,HH_saving,NetInvestment,Firm_saving,Saving_Investment_check,Saving_Investment_check_alt,
#                            nature_extraction_per_person,nature_consumption_per_person,goods_physical_consumption_per_person,
#                            elasticity.K,elasticity.L,elasticity.nature,r.real,CPI,inflation,NPR,
#                            nature_intensity_real,P.goods_real,P.extract_real,Y.goods_real,Y.extract_real,Y.total_real,
#                            VA.goods_real,VA.extract_real,VA.total_real,D.goods_real,D.extract_real,D.total_real,
#                            C.goods_real,C.extract_real,C.total_real,I.goods_real,I.extract_real,I.total_real,
#                            w_real,wages.goods_real,wages.extract_real,wages_real,g,lambda_h1,lambda_h2,lambda_h3,value_inventory.goods,value_inventory.extract,GDP_deflator)
# 
# parameters_to_write = data.frame(beta_hc,alpha.M,alpha.m,alpha,lambda_h.max1,lambda_h.max2,tcritical.lambda_h,delta_y,
#                                 v,CU.extract_delay,
#                                 IC.extract_perception_delay,P.extract_delay,ph_xval,ph_yval,ph_min,ph_s,
#                                 S,K_o,a_gg,a_ge,gamma_hc,
#                                 tcritical.Inv_Cum_extract,tcritical.Inv_Cum_goods,eta.Kextract_max,eta.Kextract_min,eta.Kextract_s,eta.Kgoods_max,eta.Kgoods_min,eta.Kgoods_s,
#                                 markup.goods,markup.extract,nature_per_unit_physical_good)
# 
# ## The codes below is to add all other parameters to the data.frame "data_to_write"
# write.csv(data_to_write,file = paste("Temp_Output",".csv"))
# write.csv(parameters_to_write,file = paste("Temp_Parameters",".csv"))
#########################################################################################################

### PLOT RESULTS

############################################
##  This section is for plotting the results  
############################################
par(bg="white", fg="black", col.axis="black", col.lab="black", col.main="black")
## Define plot arrangement and margins (in inches)
par(mfrow = c(4,2))
bmar <- 4 # Bottom margin (1)
lmar <- 5 # Left margin (2)
tmar <- 1 # Top margin (3)
rmar <- 1 # Right margin (4)
# Define plot margins and axis tick label placement
par(mar=c(bmar,lmar,tmar,rmar))
# Make plots where the axis limits exist at the borders of the plot area
par(xaxs="i")
par(yaxs="i")
# Define axis label sizes
axis_size <- 1
lab_size <- 1

capital.max <- max(capital,na.rm=TRUE)
plot(time,capital,xlab = "Years",ylab = "Capital",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,capital.max))
par(new=TRUE)
plot(time,K.extract,xlab = "Years",ylab = "Capital",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim=c(0,capital.max))
legend(max(time),capital.max*.95,c("Total","Extract"),lty = c(1,2))
plot(time,wealth,xlab = "Years",ylab = "Wealth",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,population,xlab = "Years",ylab = "Population",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(population)*1.1))
plot(time,g,xlab = "Years",ylab = "Goods",type = "l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,nature,xlab = "Years",ylab = "Nature",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(lambda_h)*1.1))
plot(time,debt_ratio.goods,xlab = "Years",ylab="Debt Ratio",type = "l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim = c(0,max(debt_ratio.goods,debt_ratio.extract)*1.1))
par(new=TRUE)
plot(time,debt_ratio.extract,xlab = "",ylab="",type = "l",cex.lab=lab_size,cex.axis=axis_size,lty=3,ylim = c(0,max(debt_ratio.goods,debt_ratio.extract)*1.1))
legend(0.01*max(time),0.9*max(debt_ratio.goods,debt_ratio.extract),c("Goods","Extract"),lty = c(2,3),cex=0.8)
plot(time,debt_ratio,xlab = "Years",ylab="Debt Ratio",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,max(debt_ratio)*1.1))
plot(time,profit_rate,xlab = "Years",ylab = "Profit Rate",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,max(profit_rate)*1.1))
plot(time,wage_share,xlab="Years",ylab = "Wage Share",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,interest_share,xlab = "Years",ylab = "Int. Share",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,max(interest_share)*1.1))
plot(time,r.L,xlab = "Years",ylab="Loan Rate",type = "l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,w,xlab = "Years",ylab = "Wages/person",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,profit_share,xlab = "Years",ylab = "Prof. Share",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,profit_share.goods,xlab = "Years",ylab = "Prof. Share G",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0.0,0.3))
plot(time,profit_share.extract,xlab = "Years",ylab = "Prof. Share E",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0.0,0.3))
plot(time,(L.goods_sim+L.extract_sim)/population,xlab = "Years",ylab = "Part. Rate",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,1))
par(new=TRUE)
plot(time,L.goods_sim/population,xlab = "",ylab = "",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim=c(0,1))
par(new=TRUE)
plot(time,L.extract_sim/population,xlab = "",ylab = "",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=3,ylim=c(0,1))
legend(0.1*max(time),1,c("Total","Goods","Extract"),lty = c(1,2,3),cex=0.6)
plot(time,nature/nature_extraction,xlab = "Years",ylab = "R-P Ratio",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,P.extract,xlab = "Years",ylab = "P.extract",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,P.goods,xlab = "Years",ylab = "P.goods",type="l",cex.lab=lab_size,cex.axis=axis_size)

plot(time,(I.goods+I.extract)/(Y.goods+Y.extract),xlab = "",ylab = "Invest Share",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=1,ylim = c(0,.5))
par(new=TRUE)
plot(time,I.goods/(Y.goods),xlab = "Years",ylab="",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,.5),lty=2)
par(new=TRUE)
plot(time,I.extract/(Y.extract),xlab = "",ylab="",type = "l",cex.lab=lab_size,cex.axis=axis_size,lty=3,ylim = c(0,.5))
legend(0.6*max(time),0.5,c("Total","Goods","Extract"),lty = c(1,2,3),cex=0.6)
plot(time,nature_extraction,xlab = "Years",ylab = "Extraction",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=1,ylim=c(0,max(nature_extraction)))
par(new=TRUE)
plot(time,net_nature_extraction,xlab = "",ylab = "",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim=c(0,max(nature_extraction)))
plot(time,Y.goods_real+Y.extract_real,xlab = "Years",ylab = "Y total (real)",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=1)
plot(time,C.goods_real,xlab = "Years",ylab = "C.goods (real)",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,C.extract_real,xlab = "Years",ylab = "C.extract (real)",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,I.goods_real,xlab = "Years",ylab = "I.goods (real)",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,I.extract_real,xlab = "Years",ylab = "I.extract (real)",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,CU.goods,xlab = "Years",ylab = "CU_goods",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,CU.extract,xlab = "Years",ylab = "CU_ext",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,IC.goods_perceived,xlab = "",ylab = "IC.goods",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim=c(0,3))
plot(time,IC.extract_perceived,xlab = "",ylab = "IC.extract",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=2,ylim=c(0,3))
plot(time,nature_intensity,xlab = "Years",ylab = "Nature intensity",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=1)
plot(time,nature_intensity_real,xlab = "Years",ylab = "Real Nature intensity",type="l",cex.lab=lab_size,cex.axis=axis_size,lty=1)
plot(time,NEPR,xlab = "Years",ylab = "NEPR",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,profit_share.bank,xlab = "Years",ylab = "Bank Share",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,Y.extract/(Y.goods+Y.extract),xlab = "Years",ylab = "Yext/Ytot",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,(C.extract+I.extract)/(Y.goods+Y.extract),xlab = "Years",ylab = "(C+I)ext/Ytot",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,(XX.ee+XX.ge+I.extract)/(VA.total),xlab = "Years",ylab = "Ext. Sector spend/VA",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,(XX.ee+XX.eg+C.extract)/(VA.total),xlab = "Years",ylab = "Nature spend/VA",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,nature_extraction_per_person,xlab = "Years",ylab = "Nature Extract / Person",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,X.goods_check_VA-X.goods_check_Yalt,xlab = "Years",ylab = "VA and Y check: goods",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,X.extract_check_VA-X.extract_check_Yalt,xlab = "Years",ylab = "VA and Y check: extract",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,Saving_Investment_check,xlab = "Years",ylab = "(nominal) Saving-Investment",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,Saving_Investment_check_alt,xlab = "Years",ylab = "(alt) Saving-Investment",type="l",cex.lab=lab_size,cex.axis=axis_size)
plot(time,elasticity.K,xlab = "Years",ylab = "K elasticity",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(-1,2))
plot(time,elasticity.L,xlab = "Years",ylab = "L elasticity",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(-2,10))
plot(time,elasticity.nature,xlab = "Years",ylab = "Nature elasticity",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(-2,10))
plot(time,r.real,type="l",ylim = c(-0.1,0.1))
##########################################################################################################
## End of Code
##########################################################################################################