#Coursera, course 'R Programming, an introduction to the R Language', week 3, assignment 2
# Piet Muis, 2014-12-18, The Hague

#####################################################################################
#
# general comment
#
# Given are two functions, named 'makeVector' and 'cachemean'. The first one creates
# a list of four functions, and its purpose is to cahce the mean of the data involved.
# The other function, 'cachemean', is the one that we actually use.
# I'll leave these two functions without further comment. They are given in the lecture
# and they are well-discussed in the forums.
#
#####################################################################################

#--------------------------------------------------
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

#--------------------------------------------------
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

############################################################################
#
# my own work
#
# We simply copy the functions above, give them the correct names and change
# whatever is needed to get a matrix inversed. I have tried to document these
# functions to the best of my abilities and unbderstanding.
#
############################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    # we create the 'local variable' cachedValue. The other
    # functions within this function do then use this
    # variable, since because of the lexical scoping of R,
    # the variable in those functions will be the one that
    # is defined in the scope of this upper function.
    # Sounds complex, and actually, I find it rather complex.
    # But I got it to work!
    #
    # note that I don not store the input value 'x'. I use a copy
    # of it, called 'data'
        
    data        <- x
    cachedValue <- NULL
    
    #-------------------------------------------------------------
    # we now make a function that sets the data, at the
    # same time setting the cachedValue to NULL again, to
    # avoid the situation that we have a cached value for
    # data that is no longer valid.
    # This function is not needed, after all we supply data
    # when creating a 'makeCacheMatrix', but with this
    # function you can re-use this thing for other data.
    #-------------------------------------------------------------

    set <- function(y) {
        data        <<- y
        cachedValue <<- NULL
    }
    
    #-------------------------------------------------------------
    # and a function that returns the data, for inspection
    # I do not like the 'bold' 'no' parenthesis notation of
    # the course, so I use parenthesis
    #-------------------------------------------------------------
    get <- function() {
        data
    }
    
    #-------------------------------------------------------------
    # now comes a function that simply sets the inverse of the
    # data x, whatever that inverse may be.
    #-------------------------------------------------------------
    setInverse <- function(inverse) {
        cachedValue <<- inverse
        cachedValue
    }
    
    #-------------------------------------------------------------
    # and a function that shows us this inverse    
    #-------------------------------------------------------------
    getInverse <- function() {
        cachedValue
    }
    
    #-------------------------------------------------------------
    # and finally we return the value of this upper function
    # 'makeCacheMatrix', it is a list of four elements, each
    # element being a function, namely one of the above.
    # These elements are named, so that we can call them as
    # list$getInverse(), comparable to mtCars$mpg which was an
    # example used in the lectur.
    #-------------------------------------------------------------
    
    list( set = set, 
          get = get,
          setInverse = setInverse, 
          getInverse = getInverse
        )
}

##########################################################################################
#
# and now for the function 'cacheSolve'. Not much to tell, since it is almost
# identical to the supplied 'cachemean' function
#
##########################################################################################

#-----------------------------------------------------------------
# the parameter, x, is of the type 'makeCacheMatrix'
#-----------------------------------------------------------------

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # coming here, there is no inverse available, so we have to
    # calculate it. Fingers crossed that there is data AND that
    # data is of the correct type...
    data <- x$get()
    return(x$setInverse(solve(data)))
}

###########################################################################################
#
# end of assignment.
#
###########################################################################################

###########################################################################################
#
# additional stuff, not part of the assignment. I wrote this purely to increase my
# experience with R, and also to simply try out some things. For the peer reviewer:
# if you are interested, then read it, else just go on with the next review.
# 
# -----------------------------------------------------------------------------------
#
# I'm not pleased with this assignment. There are two shortcomings that I noticed.
# 
# First of all, there are two variables involved, a 'makeCacheMatrix' object, that contains
# the value or will be doing so, and secondly a 'cacheSolve' object, that gives the cached
# value, or calculates it first. Now, this is what I do not like: you can set the inverse
# in 'makeCacheMatrix' with the function 'setInverse()', to any value you like, no matter
# what data are in it. That should not be possible! One way to prevent this possibility
# (and that I tried to achieve in the following) is to hide this 'makeCacheMatrix'. A user
# then only deals with 'cacheSolve', without knowing anything about 'makeCacheMatrix'.
# Although this 'mCM' is there, it is in the background, hidden away from the user.
# To optimize this, I think this 'mCM' function should be an inner function to 'cacheMatrix',
# so that the mere existence of it is hidden away from the user. Well, given the time
# available (one week to perform this assignment), and my very lacking knowledge of R
# (being now 2 and a half week underway with this course), I did not.
# 
# The other aspect that I did not like is that we now have two objects: a 'cachemean' and a
# 'cacheSolve'. Now, I can imagine that sooner or later we also need a 'cacheVariance', a
# 'cacheMin', a 'cacheStandardDeviation', and so on. The problem is that in the current
# way of coding, we need to program a 'cacheXXX' for every statistic we need. Since the code
# needed is practically the same for each object, that would be a waste. So the second
# item is to somehow make the function in 'cacheXXX' a parameter.
# 
# Now, that has some consequences for the design. For instance: if a Value (like the mean 
# or the inverse matrix) is cached, yet we call 'cache$getValue(function)', then the cached
# value will be recomputed. Unfortunately, I currently lack the knowledge to store information
# about the function that is used in determining the cached value.
#
# Well, anaway, here is what I produced so far.
#
###########################################################################################

###########################################################################################
#
# As explained above, the following function should actually be an internal
# function to the 'cachedGeneralValue' further on.
#
###########################################################################################

makeGeneralVector <- function() {
    data  <- NULL
    value <- NULL
    info  <- c("data not specified", "value not specified", "no other info available")
    
    ############################################################
    #  set up of data, value and info
    ############################################################
    
    #------- data -----------------------------
    setData <- function(y = matrix()) {
        data  <<- y
        value <<- NULL
        info[1] <<- "data is specified, use 'getData' to see it"
        info[2] <<- "value not yet calculated"
    }
    
    getData <- function() {
        data
    }
    
    ####################################################
    
    #------- value ---------------------------
    setValue <- function(val) {
        info[2] <<- "value is specified, use 'getValue' to see it"
        value <<- val
        value
    }
    
    getValue <- function() {
        value
    }
    
    ####################################################
    
    #------- info ----------------------------
    setInfo <- function(information = "No Information Available") {
        info[3] <<- information
    }
    
    getInfo <- function() {
        info
    }
    
    ####################################################
    # return value
    ####################################################
    
    list( setData  = setData,  getData  = getData, 
          setValue = setValue, getValue = getValue, 
          setInfo  = setInfo,  getInfo  = getInfo
    )
}

#####################################################################################
#
# cachedGeneralValue
#
#####################################################################################
#
# returns a value, calculated fron the available data. If this value is not
# calculated yet, it will be, then it gets stored, and finally returned
#-------------------------
#
# Usage
# 
# - to create a cachedGeneralValue: cgv <- cachedGeneralValue()
# - to set the data: cgv$setData(1: 100000)  # for instance 
# - to see the data: cgv$getData()
# - to see the info, use: cgv$getInfo()
#        this returns three things: whether the data has been set, 
#                                   whether the Value is known or calculated,
#                                   a free line of info, to be set with: cgv$setInfo("This is info")
# - to get the Value use:
#        cgv$getValue(function), where function is f.i. mean, min, max, var, function(x) {mean(x) + 2})
#    or  cgv$getValue(), that simply returns the present Value
# 
# - the value is reset when either the data is renewed, or cgv$getValue has a non-null argument
# 
# _ one can see the caching in this example:
#     cgv <- cachedGeneralValue()
#     cgv$setData(1: 100000000)
#     cgv$getValue(var)
#     cgv$getValue()
#
# - assignment 2 should go like this:
#     assignment2 <- cachedGeneralValue()
#     assignment2$setData(matrix(rnorm(100, 10, 10), nrow = 10, ncol = 10))
#     assignment2$getValue(solve)
#     assignment2$getValue()
#
#####################################################################################

cachedGeneralValue <- function() {
    
    mgv <- makeGeneralVector()  # hiding it from the user
    
    ########################################################
    # usage functions
    ########################################################
    
    #-------------------------------------------------------
    setData <- function(x) {
        mgv$setData(x)
    }
    
    #-------------------------------------------------------
    getData <- function() {
        mgv$getData()
    }
    
    #------------------------------------------------------
    getInfo <- function() {
        mgv$getInfo()
    }
    
    #------------------------------------------------------
    setInfo <- function(text = "Info not yet present") {
        mgv$setInfo(text)
    }
    
    #####################################################
    #
    #  here we go!
    #
    #####################################################
    
    getValue <- function(func = NULL) {
        
        # is a function supplied? If not, then see if there's
        # already a value present
        
        if(is.null(func)) {
            
            # no function supplied. Do we have data available?
            # if not, then we return NA
            
            # message("func is NULL!!!")
            
            data <- mgv$getData()
            if(is.null(data)) {
                return(NA)
            }
            
            # alright, so we have data. We can return the value,
            # since either there's no value calculated yet, in
            # which case the value in the mgv is NULL, or we
            # do have calculated a value. In both cases, we 
            # simply return the value
            
            return(mgv$getValue())
        }
        
        # coming here, we do have been sent a function. Let's
        # retrieve the data first
        
        # message("func is NIET NULL!!!!!!!!!!!!!!!!!")
        
        data = mgv$getData()
        
        # do we have data?
        if(is.null(data)) return(NA)
        
        #oke, calculate!
        mgv$setValue(func(data))
    }
    
    list(setData = setData, getData = getData, 
         setInfo = setInfo, getInfo = getInfo,
         getValue = getValue
    )
}