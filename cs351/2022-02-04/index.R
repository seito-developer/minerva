## Neural Network Code
## Adapted (tweaked, annotated) from:
## https://tinyurl.com/ynf7drre

N = 50 # total number of data points each group


# The 2 lines below induce "separation" between the 2 groups
# Bigger offsets equate to more separation (harder to classify)
x_offset = 0.5 # group seperation on x axis
y_offset = 0.5 # group seperation on y axis

#############################################

## Create some random data defining two groups
group1_x = runif(N, min = 0, max = 1)
group1_y = runif(N, min = 0, max = 1)

group2_x = runif(N, min = 0+x_offset, max = 1+x_offset)
group2_y = runif(N, min = 0+y_offset, max = 1+y_offset)

x1 = c(group1_x, group2_x)
x2 = c(group1_y, group2_y)

# Lable the first group "1" and the second group "-1"
group_identifier = c(rep(-1,N), rep(1,N))

#############################################

# print all the values of "x1"
print(x1)

# print all the values of "x2"
print(x2)

# print the group identifiers (-1s and 1s)
print(group_identifier)


plot(x1, x2, type='n', xlab='x1', ylab='x2')
points(group1_x, group1_y, col='red')
points(group2_x, group2_y, col='blue')

##########################

# Set some arbitrary initial bias and weights
bias = 0.1 # initial weight
w1 = 0.2 # initial weight
w2 = 0.3 # initial weight

M = 15                 # maximum number of epochs to run
learning_rate = 0.005  # learning rate
th = 0.99              # threshold to stop
verbose = F            # whether detailed weight update info is printed


for (i in 1:M){
  print(paste('Epoch starts: ', i))
  
  ## We reshuffle the order of the datapoint for each epoch.
  index = 1:(2*N)
  index = sample(index)
  
  for (j in index){
    
    # determine whether it classifies as -1 or 1
    activ = bias + w1*x1[j] + w2*x2[j]
    
    if (activ >= 0){
      pred_j = 1   #then we classify as a 1
    }else{
      pred_j = -1} #then we classify as a -1
    
    # tweak the weights if missclassification (if group != pred_j)
    bias = bias + learning_rate*(group_identifier[j] - pred_j)*1.0
    w1 = w1 + learning_rate*(group_identifier[j] - pred_j)*x1[j]
    w2 = w2 + learning_rate*(group_identifier[j] - pred_j)*x2[j]
   
    # tells the user what's going on during the loop...
     if (verbose == T){
      print(paste('  -> updating data point ', j, ' : '))
      print(paste('     -> bias: ' , bias))
      print(paste('     -> w1: ' , w1))
      print(paste('     -> w2: ' , w2))
    }
  }  
  
  # generate all the predicted ys.
  y_pred = bias + w1*x1 + w2*x2 # first run the activ for all points
  y_pred[y_all >= 0] = 1        # convert pos and 0 activ values to +1s 
  y_pred[y_all< 0] = -1         # convert neg activ values to -1s
  
  # accuracy equals the number of correct classifications / N
  acc = sum(y_pred == group_identifier)/length(group_identifier)
  print(paste('Epoch ends: ', i, ' WITH accuracy: ', acc))
    
    # if success criterion for classification is met, then stop
    if (acc >= th){
    break
  }
}        


# Get the info about accuracy 
# (see annotation in function above, where this code also appears)
y_pred = bias + w1*x1 + w2*x2
y_pred[y_all >= 0] = 1
y_pred[y_all< 0] = -1

print(y_pred)

acc = sum(y_pred == group_identifier)/length(group_identifier)
print(acc)

# plot the data with 2 categories, showing classification line
plot(x1, x2, type='n', xlab='X', ylab='Y')
points(x1[which(group_identifier == 1)], x2[which(group_identifier == 1)], col='red')
points(x1[which(group_identifier == -1)], x2[which(group_identifier == -1)], col='blue')
abline(a = -1.0*(bias/w2), b = -1.0*(w1/w2), col='dark green', lwd=3, lty=2)