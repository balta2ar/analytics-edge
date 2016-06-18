
#=---------------------------------------------------------
#
# Script by Paul Thomas - @maxthomais - June 2016
# Help automate data analysis for analytics edge kaggle competition
#
#=---------------------------------------------------------

train = read.csv("train2016.csv")

# helper to convert to 2 decimal places ...
asPrcnt <- function( arg1 ) {
  round( 100 * arg1, digits = 2 )
}


# check to see if the democrat v republican 
# split ratio for an answer is very biased!
checkForAnswerBias <- function( arg1Table, arg2Desc, arg3DiffMin, arg4DiffMax )
{
  # see how many possible answers ...
  numCols = ncol(arg1Table)

  # loop over all the answers ...
  for (i in 1:numCols)
  {
    # calc diff
    diff = abs(arg1Table[1,i] - arg1Table[2,i]) / (arg1Table[1,i] + arg1Table[2,i])
    
    # is there is bias to be seen!?
    if ((diff > arg3DiffMin) && (diff <= arg4DiffMax))
    {
      # calc splits
      dems = arg1Table[1,i] / (arg1Table[1,i] + arg1Table[2,i])
      reps = arg1Table[2,i] / (arg1Table[1,i] + arg1Table[2,i])

      #print out result
      cat(arg2Desc,i,"Dem% =", asPrcnt(dems), "Rep% =", asPrcnt(reps), "Diff% =", asPrcnt(dems-reps), "\n")
    }
  }
}


# e.g. mac v pc, seems that MAC users tend to more pro democrat ...
compTable = table(train$Party,train$Q110740)
compTable
checkForAnswerBias( compTable, "Q110740", 0.1, 1.0 )


# note we just want to check the Question columns!
questions = grep("^Q",names(train))

# loop over all the questions
for(i in questions)
{
  resultsTable = table(train$Party,train[[i]])
  #checkForAnswerBias( resultsTable, names(train)[i], 0.05, 0.10)   # 5 - 10 %
  checkForAnswerBias( resultsTable, names(train)[i], 0.10, 0.25)   # 10 - 25 %
  checkForAnswerBias( resultsTable, names(train)[i], 0.25, 1.00)   # > 25% diff
}

