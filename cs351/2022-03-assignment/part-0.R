install.packages("rgenoud")
library(rgenoud)

# If you have questions, first try the help file
?genoud
?optim

# and, by the way, the value of X that minimizes this is 3.
#ちなみに、これを最小化するXの値は3です。

# First create an objective function to be minimized-- call it f1...
# まず、最小化する目的関数を作ります--f1...と呼びます。
# I started for you below. Just fill in the ???s
# 以下で始めてみました。???を埋めるだけです。
# hint: here, the best 'solution' is the x that minimizes (x^2 - 6*x + 9)
# ヒント：ここで、最良の「解」は (x^2 - 6*x + 9) を最小化する x です。
f1 <- function(x) {
  y = (x^2 - 6*x + 9)
  return(y)
}
# once you have f1, then use the genetic algorithm to find the MINIMUM
# f1 が得られたら、次に遺伝的アルゴリズムで MINIMUM を求めます。
GA <- genoud(f1, nvars = 1, max = FALSE) # max = FALSE b/c it's a minimization}
###
# then, use the 'optim' hill-climbing algorithm:
# その後、'optim' hill-climbing アルゴリズムを使用します。
# the ???s prompt you to pick a starting value (btw -100 and +100)
# ???はあなたに開始値を選ばせる (-100 と +100 の間)
result <- optim(GA$par, f1, method = "Brent", lower = -100, upper = 100)

print(result)

#Ref
#https://medium.com/genetic-algorithm-ga-using-r-package-rgenoud/genetic-algorithm-ga-with-r-package-rgenoud-d176daa5543e
