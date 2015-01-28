# Probability weighted multistep EMM prediction 
library('rEMM')

temp<-read.csv('Training.csv')
train<-temp[,-c(1,2)] #first column is DATE(mostly time), second column is special case
test<-read.csv('Testing.csv')
steps = length(test[,1])

emm_fun <- function(thresh,train,steps)
{
# building EMM model and getting the node index containing the current/last vector
	emm_obj <- EMM(threshold = thresh, measure = 'Euclidean')
	emm <- build(emm_obj, train)
	node_info <- find_clusters(emm, train, match_cluster = c('exact', 'nn'), dist = FALSE)
	last_node <- node_info[length(node_info)]

# predict future with probability weighted average from nodes	
	i <- 1
	prediction <- matrix(nrow = steps, ncol = 1)
	prediction <- data.frame(prediction)
	center <- cluster_centers(emm)
	while (i <= steps)
	{
		future_nd <- predict(emm, n = i, current = last_node, probabilities = TRUE)
		weighted_sum <- sum((future_nd * center[,1]), na.rm = TRUE)
		non_zero <- (future_nd > 0) 
		possible_nd_cnt <- length(grep(TRUE, non_zero))
		prediction[i,1] <- weighted_sum/possible_nd_cnt
		i <- i+1
	}
	return (prediction)
}

pred <- emm_fun(thresh = .2, train = train, steps = steps)
