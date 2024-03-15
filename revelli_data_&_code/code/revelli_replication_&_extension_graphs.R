#reproduce plot
fossil_output <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Output_fossil2 .csv")
fossil_parameters <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Parameters_fossil2 .csv")
renew_high_output <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Output_renew_high .csv")
renew_high_parameters <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Parameters_renew_high .csv")

"""
# Specify the dimensions of the dataframe
num_rows <- 980
num_cols <- 112
# Create a matrix filled with zeros
zeros_matrix <- matrix(-10, nrow = num_rows, ncol = num_cols)
# Convert the matrix to a dataframe
zeros_df <- data.frame(zeros_matrix)
# Optionally, you can name the columns if needed
colnames(zeros_df) <- colnames(fossil_output)
fossil_zero <- subset(zeros_df, select = c(-time))
time_fossil <- seq(from = 62.1, to = 160.0, by = 0.1)
fossil_zero$time <- time_fossil
fossil_output_all_time <- rbind(fossil_output,fossil_zero)
"""

####################################### Figure 2 - reproduction and check if fossil scenario works#############################################################
par(bg="white", fg="black", col.axis="black", col.lab="black", col.main="black")
## Define plot arrangement and margins (in inches)
par(mfrow = c(2.5,4))
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

# fig 2_a available nature ressource
plot(renew_high_output$time,renew_high_output$nature,xlab = "Years",ylab = "Nature",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,150))
lines(fossil_output$time,fossil_output$nature/10,lty=2,col="red")
# fig 2_b nature extraction
plot(renew_high_output$time,renew_high_output$nature_extraction,xlab = "Years",ylab = "Nature extraction",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,30))
lines(fossil_output$time,fossil_output$nature_extraction,lty=2,col="red")
#capital graph - fig 2_c
capital.max <- max(renew_high_output$capital,na.rm=TRUE)
plot(renew_high_output$time,renew_high_output$capital,xlab = "Years",ylab = "Capital",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,capital.max))
lines(fossil_output$time,fossil_output$capital,lty=2,col='red')
#population fig_2_d
plot(renew_high_output$time,renew_high_output$population,xlab = "Years",ylab = "Population",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(renew_high_output$population)*1.1))
lines(fossil_output$time,fossil_output$population, lty=2, col="red")
#participation rate fig_2_e
plot(renew_high_output$time,renew_high_output$participation_rate,xlab = "Years",ylab = "Participation rate",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(renew_high_output$participation_rate)*1.1))
lines(fossil_output$time,fossil_output$participation_rate, lty=2, col="red")
#debt ratio fig 2_f
plot(renew_high_output$time,renew_high_output$debt_ratio,xlab = "Years",ylab="Debt Ratio",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,max(renew_high_output$debt_ratio)*1.1))
lines(fossil_output$time,fossil_output$debt_ratio, lty=2, col='red')
#price of goods fig 2_g
plot(renew_high_output$time,renew_high_output$P.goods_real,xlab = "Years",ylab="Price of goods",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$P.goods_real)*0.9,max(renew_high_output$P.goods_real)*1.1))
lines(fossil_output$time,fossil_output$P.goods_real, lty=2, col='red')
#price of resources fig 2_h
plot(renew_high_output$time,renew_high_output$P.extract_real,xlab = "Years",ylab="Price of resources",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$P.extract_real)*0.9,max(renew_high_output$P.extract_real)*1.1))
lines(fossil_output$time,fossil_output$P.extract_real, lty=2, col='red')

#Net output real, total, fig 2_i
plot(renew_high_output$time,renew_high_output$Y.total_real,xlab = "Years",ylab="Net output",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$Y.total_real)*0.9,max(renew_high_output$Y.total_real)*1.1))
lines(fossil_output$time,fossil_output$Y.total_real, lty=2, col='red')
#wage share, fig 2_j
plot(renew_high_output$time,renew_high_output$wage_share,xlab = "Years",ylab="Wage share",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$wage_share)*0.9,max(renew_high_output$wage_share)*1.1))
lines(fossil_output$time,fossil_output$wage_share, lty=2, col='red')
#resource extraction per person, fig 2_m
plot(renew_high_output$time,renew_high_output$nature_extraction_per_person,xlab = "Years",ylab="Resource extraction per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$nature_extraction_per_person)*0.9,max(renew_high_output$nature_extraction_per_person)*1.1))
lines(fossil_output$time,fossil_output$nature_extraction_per_person, lty=2, col='red')
#fig 2_n wage per person (real)
plot(renew_high_output$time,renew_high_output$wage_share,xlab = "Years",ylab="wage (real) per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$wage_share)*0.9,max(renew_high_output$wage_share)*1.1))
lines(fossil_output$time,fossil_output$wage_share, lty=2, col='red')
#fig 2_o CU goods sector
plot(renew_high_output$time,renew_high_output$CU.goods,xlab = "Years",ylab="CU, good sector",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$CU.goods)*0.9,max(renew_high_output$CU.goods)*1.1))
lines(fossil_output$time,fossil_output$CU.goods, lty=2, col='red')
#fig 2_p Resource consumption per person
plot(renew_high_output$time,renew_high_output$nature_consumption_per_person,xlab = "Years",ylab="Resource consumption per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$nature_consumption_per_person)*0.9,max(renew_high_output$nature_consumption_per_person)*1.1))
lines(fossil_output$time,fossil_output$nature_consumption_per_person, lty=2, col='red')
#fig 2_q good physical consumption per person
plot(renew_high_output$time,renew_high_output$goods_physical_consumption_per_person,xlab = "Years",ylab="Good physical consumption per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$goods_physical_consumption_per_person)*0.9,max(renew_high_output$goods_physical_consumption_per_person)*1.1))
lines(fossil_output$time,fossil_output$goods_physical_consumption_per_person, lty=2, col='red')
#fig 2_r total net investment
plot(renew_high_output$time,renew_high_output$I.total_real,xlab = "Years",ylab="Net investment",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$I.total_real)*0.9,max(renew_high_output$I.total_real)*1.1))
lines(fossil_output$time,fossil_output$I.total_real, lty=2, col='red')

#profit share, fig 2_k
total_profit_renew=renew_high_output$profit_share.goods+renew_high_output$profit_share.extract+renew_high_output$profit_share.bank
total_profit_fossil=fossil_output$profit_share.good+fossil_output$profit_share.extract+fossil_output$profit_share.bank
plot(renew_high_output$time,renew_high_output$profit_share.goods,xlab = "Years",ylab="Profit share",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,0.6),col='darkblue')
lines(fossil_output$time,fossil_output$profit_share.goods, lty=2,col='darkblue')
lines(renew_high_output$time,renew_high_output$profit_share.extract, col='red')
lines(fossil_output$time,fossil_output$profit_share.extract, col='red', lty=2)
lines(renew_high_output$time,renew_high_output$profit_share.bank, col='blue')
lines(fossil_output$time,fossil_output$profit_share.bank, col='blue', lty=2)
lines(renew_high_output$time,total_profit_renew, col='brown')
lines(fossil_output$time,total_profit_fossil, col='brown', lty=2)
legend(1,0.6,c("Renewable-high","Fossil"),lty = c(1,2))
legend(100,0.6,c("Good sector","Extraction sector","Banking sector","All sectors")
       ,col=c('darkblue','red','blue','brown'), lty=c(1,1,1,1))
#NEPR, fig 2_l
plot(renew_high_output$time,renew_high_output$NEPR,xlab = "Years",ylab="NEPR",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$NEPR)*0.9,max(renew_high_output$NEPR_direct)*1.1))
lines(fossil_output$time,fossil_output$NEPR, lty=2, col='red')
lines(renew_high_output$time,renew_high_output$NEPR_direct, type="l", col="grey")
lines(fossil_output$time,fossil_output$NEPR_direct, lty=2, col='darkred')
legend(60,7.5,c("RESOURCE INPUT ASSUMPTION","Operate K_e (only)",' ',"Operate and invest in K_e",' ')
       ,col=c('white','darkred','grey','black','red'), lty=c(1,2,1,2,1))
################ Extension ###########################
renew_high_glow_output <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Output_renew_high_g_low .csv")
renew_high_glow_parameters <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Parameters_renew_high_g_low .csv")
renew_high_ghigh_output <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Output_renew_high_g_high .csv")
renew_high_ghigh_parameters <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Parameters_renew_high_g_high .csv")
renew_high_output <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Output_renew_high .csv")
renew_high_parameters <- read.csv("C:/Users/leore/OneDrive/Bureau/Master 2 - ENS/Macroeconomics/replication_macro_Harmoney/Temp_Parameters_renew_high .csv")

par(bg="white", fg="black", col.axis="black", col.lab="black", col.main="black")
## Define plot arrangement and margins (in inches)
par(mfrow = c(2.5,4))
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

# fig 2_a available nature ressource
plot(renew_high_output$time,renew_high_output$nature,xlab = "Years",ylab = "Nature",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,150), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$nature,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$nature/10,lty=2,col="red")
# fig 2_a available nature ressource
plot(renew_high_output$time,renew_high_output$nature_extraction,xlab = "Years",ylab = "Nature extraction",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,30), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$nature_extraction,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$nature_extraction/10,lty=2,col="red")
#capital graph - fig 2_c
capital.max <- max(renew_high_output$capital,na.rm=TRUE)
plot(renew_high_output$time,renew_high_output$capital,xlab = "Years",ylab = "Capital",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,capital.max), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$capital,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$capital,lty=2,col="red")
#population fig_2_d
plot(renew_high_output$time,renew_high_output$population,xlab = "Years",ylab = "Population",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(renew_high_output$population)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$population,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$population,lty=2,col="red")
#participation rate fig_2_e
plot(renew_high_output$time,renew_high_output$participation_rate,xlab = "Years",ylab = "Participation rate",type="l",cex.lab=lab_size,cex.axis=axis_size,ylim=c(0,max(renew_high_output$participation_rate)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$participation_rate,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$participation_rate,lty=2,col="red")
#debt ratio fig 2_f
plot(renew_high_output$time,renew_high_output$debt_ratio,xlab = "Years",ylab="Debt Ratio",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,max(renew_high_output$debt_ratio)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$debt_ratio,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$debt_ratio,lty=2,col="red")
#price of goods fig 2_g
plot(renew_high_output$time,renew_high_output$P.goods_real,xlab = "Years",ylab="Price of resources",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$P.goods_real)*0.9,max(renew_high_ghigh_output$P.goods_real)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$P.goods_real,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$P.goods_real,lty=2,col="red")
#price of resources fig 2_h
plot(renew_high_output$time,renew_high_output$P.extract_real,xlab = "Years",ylab="Price of resources",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$P.extract_real)*0.9,max(renew_high_ghigh_output$P.extract_real)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$P.extract_real,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$P.extract_real,lty=2,col="red")

#Net output real, total, fig 2_i
plot(renew_high_output$time,renew_high_output$Y.total_real,xlab = "Years",ylab="Net output",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$Y.total_real)*0.9,max(renew_high_ghigh_output$Y.total_real)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$Y.total_real,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$Y.total_real,lty=2,col="red")
#wage share, fig 2_j
plot(renew_high_output$time,renew_high_output$wage_share,xlab = "Years",ylab="Wage share",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_ghigh_output$wage_share)*0.9,max(renew_high_output$wage_share)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$wage_share,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$wage_share,lty=2,col="red")
#profit share, fig 2_k
total_profit_renew=renew_high_output$profit_share.goods+renew_high_output$profit_share.extract+renew_high_output$profit_share.bank
total_profit_ghigh=renew_high_ghigh_output$profit_share.goods+renew_high_ghigh_output$profit_share.extract+renew_high_ghigh_output$profit_share.bank
total_profit_glow=renew_high_glow_output$profit_share.goods+renew_high_glow_output$profit_share.extract+renew_high_glow_output$profit_share.bank
plot(renew_high_output$time,total_profit_renew,xlab = "Years",ylab="Profit share",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(0,0.6),col='grey')
lines(renew_high_ghigh_output$time,total_profit_ghigh,lty=1,col="black")
lines(renew_high_glow_output$time,total_profit_glow,lty=2,col="red")
#NEPR, fig 2_l
plot(renew_high_output$time,renew_high_output$NEPR,xlab = "Years",ylab="NEPR",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$NEPR)*0.9,max(renew_high_output$NEPR_direct)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$NEPR,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$NEPR/10,lty=2,col="red")
#resource extraction per person, fig 2_m
plot(renew_high_output$time,renew_high_output$nature_extraction_per_person,xlab = "Years",ylab="Resource extraction per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$nature_extraction_per_person)*0.9,max(renew_high_output$nature_extraction_per_person)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$nature_extraction_per_person,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$nature_extraction_per_person/10,lty=2,col="red")
#fig 2_n wage per person (real)
plot(renew_high_output$time,renew_high_output$wage_share,xlab = "Years",ylab="wage (real) per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$wage_share)*0.9,max(renew_high_output$wage_share)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$wage_share,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$wage_share,lty=2,col="red")
#fig 2_o CU goods sector
plot(renew_high_output$time,renew_high_output$CU.goods,xlab = "Years",ylab="CU, good sector",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$CU.goods)*0.9,max(renew_high_output$CU.goods)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$CU.goods,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$CU.goods,lty=2,col="red")
#fig 2_p Resource consumption per person
plot(renew_high_output$time,renew_high_output$nature_consumption_per_person,xlab = "Years",ylab="Resource consumption per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$nature_consumption_per_person)*0.9,max(renew_high_output$nature_consumption_per_person)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$nature_consumption_per_person,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$nature_consumption_per_person,lty=2,col="red")

#fig 2_q good physical consumption per person
plot(renew_high_output$time,renew_high_output$goods_physical_consumption_per_person,xlab = "Years",ylab="Good physical consumption per person",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$goods_physical_consumption_per_person)*0.9,max(renew_high_output$goods_physical_consumption_per_person)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$goods_physical_consumption_per_person,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$goods_physical_consumption_per_person,lty=2,col="red")
#fig 2_r total net investment
plot(renew_high_output$time,renew_high_output$I.total_real,xlab = "Years",ylab="Net investment",type = "l",cex.lab=lab_size,cex.axis=axis_size,ylim = c(min(renew_high_output$I.total_real)*0.9,max(renew_high_output$I.total_real)*1.1), col="grey")
lines(renew_high_ghigh_output$time,renew_high_ghigh_output$I.total_real,lty=1,col="black")
lines(renew_high_glow_output$time,renew_high_glow_output$I.total_real,lty=2,col="red")