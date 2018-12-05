## it seems that there is some sequencing error.
##
##  The following code will not run with the following error MATLAB:

##  Error in .jcall(fsdaEngine, returnType, fsdaFunction, as.integer(nargout),  :
##    com.mathworks.toolbox.javabuilder.MWException: BLAS loading error:
##  mkl.dll: The specified module could not be found.

## Simply commenting out the first call to resindexplot() will fix it and it will work.


## Does not work
library(fsdaR)
resindexplot(rnorm(100))

data(stackloss)
stats <- lm(stack.loss~., data=stackloss)
resindexplot(stats$residuals)

out <- fsreg(stack.loss~., data=stackloss)


## But this works
library(fsdaR)
##resindexplot(rnorm(100))

data(stackloss)
stats <- lm(stack.loss~., data=stackloss)
resindexplot(stats$residuals)

out <- fsreg(stack.loss~., data=stackloss)
