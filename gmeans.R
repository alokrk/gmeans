#Implementation of G-Means
#Learning k in k-means {Greg Hamerly, Charles Elkan}

#for clusplot
library(cluster)
#for ad-test
library(nortest)

#read question data
data = read.csv("data.csv")

#INPUT VALUE IN CONSOLE BEFORE WE PROCEED
alpha <- readline("Enter alpha value : ")
alpha = as.double(alpha)

#initial value of k = 1
k=1

#inital cluster for checking data before g-means has been applied
clusters = kmeans(data,k)
clusplot(data,clusters$cluster)
initialCentre = clusters$centers

#G-Means function with added centre paramter passed
#besides the alpha and data mentioned in the paper

gmeans = function(data, alpha, centre){
  
  #running k-means based on center values
  if(dim(centre)[1]==1){
    mod = kmeans(data,1)
  }
  
  else{
    mod = kmeans(data, centre)
  }
  
  temp = c()
  
  for(i in 1:max(mod$cluster)){
    newData = data[mod$cluster==i,]
    c = mod$centers[i,]
    
    #calc as req by paper
    pca = prcomp(newData)
    lambda = pca$sdev[1]^2
    eigen = pca$rotation[,1]
    m = eigen*sqrt(2*lambda/pi)
    
    #calc child centers as per above values
    c1 = c+m
    c2 = c-m
    c12 = rbind(c1,c2)
    
    #reapply k-means on the new data
    rekm = kmeans(newData,c12)
    c12 = rekm$centers
    
    #find v and normalise it
    v = rekm$centers[1,] - rekm$centers[2,]
    vnorm = norm(as.matrix(v))
    
    finData = as.matrix(newData) %*% v / (vnorm^2)
    finData = scale(finData)
    
    #Anderson-Darling test using nortest library
    adtest = ad.test(finData)
    pv = adtest$p.value
    
    #p-value, alpha comparison
    if(pv < alpha)
      temp = rbind(temp, rekm$centers[1,],rekm$centers[2,])
    else
      temp = rbind(temp,c)
  }
  return(temp)
}

#intialise temporary variables for assignment
centre = initialCentre
temp = c()

#repeatedly call until we find the optimal value of k
repeat{
  temp = gmeans(data,alpha,centre)
  if(dim(centre)[1] == dim(temp)[1]){
    break
  }
  centre = temp
}

#optimal value of k
k = dim(centre)[1]
cat("k- value for alpha =", alpha, "using G-Means =", k)

#k-means plot as per new centers and optimal k
final = kmeans(data,centers = centre)
clusplot(data,final$cluster, lines=0, color=TRUE)