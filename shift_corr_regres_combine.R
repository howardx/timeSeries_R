library(delftfews)
library(xlsx)
library(ggplot2)

shift_corr <- function(target, indicator, max_lead)
{
	i <- 0
	correlation <- matrix(nrow = max_lead, ncol = 1)
	while ( i < max_lead )
	{
		corr<-cor(target,shift.vector(indicator,i),use="complete.obs")
		i <- i + 1
		correlation[i,] <- corr
	}
#final_output<-apply(parameter_dataframe,2,shift_corr,as.matrix(target),9)

return(correlation)
}

# Arguments:
# parameter_dataframe is the (leading) indicators' data frame
# 9 is the max leading/lagging window testing

go_industry <- function(target_frame, indicator_frame, max_lag, filename)
{
	lag <- c(0:(max_lag-1))
	target_count <- length(target_frame[,1])
	j <- 1
	while ( j < target_count )
	{
		shift_correlation<-apply(indicator_frame,2,shift_corr,as.matrix(target_frame[,j]),max_lag)
		shift_correlation<-cbind(lag,shift_correlation)
		sheetname <- colnames(target_frame)[j]
		if ( j == 1)
		{
			write.xlsx(shift_correlation,file=filename,sheetName=sheetname,col.names=TRUE,row.names=FALSE,append=FALSE)
		}
		else
		{
			write.xlsx(shift_correlation,file=filename,sheetName=sheetname,col.names=TRUE,row.names=FALSE,append=TRUE)
		}
		j <- j + 1
	}
}

lm_industry <- function(target_frame, indicator_frame)
{
	quarter <- as.Date(as.character(target_frame[,1]), format="%d/%m/%Y") # keep track of the quarter count for plotting
	target_frame <- target_frame[,-1] # get rid of date column
	indicator_frame <- indicator_frame[,-1] # get rid of date column
	j <- 1
	while ( j <= length(target_frame) )
	{
		target_name <<- colnames(target_frame)[j]
		target <- target_frame[,j] # will be plotted with BLACK, actual history value
		dat <- cbind(as.data.frame(target),indicator_frame)
		linear_model <- lm(target~.,data=dat)
		fitted_history <<- as.data.frame(linear_model$fitted.values) # will be plotted with RED, predicted history value
		indecies <- as.numeric(rownames(fitted_history))
		quarter_P <<- quarter[indecies] # only plotting Quarters with sufficient data
		target_P <<- target[indecies] 
                # painting output plot with actual and predicted historical values
		####   E <- environment()
		#jpeg(filename=paste(target_name, ".jpeg", sep=""), width=1280, height=1000)
		#p<-ggplot(mapping=aes(x=quarter_P,y=target_P),geom="line")+geom_line()+layer(mapping=aes(x=quarter_P,y=fitted_history[,1]),geom="line",colour="red")+geom_line()+opts(title=paste(target_name, "actual in BLACK, predicted in RED", sep=" "))

		#print(p)
		#dev.off()

# writing output Excel file for linear model summary
		model_summary <- summary(linear_model)
		presentable <- as.data.frame(round(model_summary$coefficients, 2)) # losing stars but 3 digits are enough for viewing 2 star level
		#indicator<-rep("",length(presentable[,4])) # indicating which indicator is SIGNIFICANT
		#indicator<-as.data.frame(indicator[which(presentable[,4]<.001)]<-"SIGNIFICANT")
		#colnames(indicator)<-"sig"
		#presentable<-cbind(indicator,presentable) # "SIG" sign at very left
		#filename <- paste(target_name, ".xlsx",sep="") # file name for text file of summary
		if (j==1)
		{
			write.xlsx(presentable,file="Model_summaries.xlsx",sheetName=target_name,col.names=TRUE,row.names=TRUE,append=FALSE)
		}	
		else
		{
			write.xlsx(presentable,file="Model_summaries.xlsx",sheetName=target_name,col.names=TRUE,row.names=TRUE,append=TRUE)
		}
		j <- j + 1

		#sink(file=filename, append=FALSE) # creating a file for capturing the follwoing R commands' outputs
		#print(target_name)
		#print(model_summary)
		#sink() # closing the file
	}
}

target_frame <- read.csv("Q_industry_SPX_1989.csv",sep="\t") # will generate same amount of image&txt file as the amount of target indecies
indicator_frame <- read.csv("Q_new_macroE_1987.csv", sep="\t")
lm_industry(target_frame, indicator_frame)
#go_industry(target, para, 9, "industry_shift_corr_with_new_macroE.xlsx")
