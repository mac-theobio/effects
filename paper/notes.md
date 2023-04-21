Build some glm examples, based on code in ../manuscript and newer examples in ../varpred, which is documented at https://cygubicko.github.io/varpred/index.html

Think about relationship between bias adjustment and reference point. Try to harmonize between paper and package.

The bias is really a bias because we are averaging the random effects and other orthogonal components on a different scale than we should (we take the mean too early, and so are comparing the function of the mean to values that correspond to the mean of the function). READ and see what good text we have about this in the old paper.
