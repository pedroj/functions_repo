stratified = function(df, group, size) {
    #  USE: * Specify your data frame and grouping variable (as column 
    #         number) as the first two arguments.
    #       * Decide on your sample size. For a sample proportional to the
    #         population, enter "size" as a decimal. For an equal number 
    #         of samples from each group, enter "size" as a whole number.
    #
    #  Example 1: Sample 10% of each group from a data frame named "z",
    #             where the grouping variable is the fourth variable, use:
    # 
    #                 > stratified(z, 4, .1)
    #
    #  Example 2: Sample 5 observations from each group from a data frame
    #             named "z"; grouping variable is the third variable:
    #
    #                 > stratified(z, 3, 5)
    #
    require(sampling)
    temp = df[order(df[group]),]
    if (size < 1) {
        size = ceiling(table(temp[group]) * size)
    } else if (size >= 1) {
        size = rep(size, times=length(table(temp[group])))
    }  
    strat = strata(temp, stratanames = names(temp[group]), 
                   size = size, method = "srswor")
    (dsample = getdata(temp, strat))
}