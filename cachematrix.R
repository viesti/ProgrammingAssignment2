# In Clojure, and other languages as well, there is a concept called memoization.
# The idea is to have a higher order function that takes another function and
# caches the value returned by the function based on the original arguments.

# The neat thing is that the original API is preserved and one can drop in the
# memoizing function for example by passing it as an argument.

# For the implementation, I took the idea from this paper:
# http://vita.had.co.nz/papers/mutatr.pdf
# Since R has a concept of attributes, I used it to attach the cache as "metadata"
# into the resulting memoized function. This provides a neat way to look at the
# contents of the cache with ls(), and if necessary, evict entries if needed.

# The idea is to use the digest of the arguments to the original function as a
# cache key. This is a slight performance hit, since we calculate the digest every
# time we call the resulting function, but it depends on the situation if this turns
# out to be too much.
library(digest)

# A factory function that takes another function f and returns a function
# with the same arity as the original function but which remembers the result
# calculated by f.
memoize <- function(f) {
	cache <- list()
	# We'll use a custom enrivonment as cache
	cache$map <- new.env(hash = TRUE, parent = emptyenv())
	# Getter for a cached value
	cache$get <- function(key) {
		get(key, env = cache$map)
	}
	# Setter for a result to be cached for a key
	cache$set <- function(key, value) {
		assign(key, value, env = cache$map)
	}
	# Predicate to test if cache has a value for a key
	cache$has_key <- function(key) {
		exists(key, env = cache$map)
	}
	# The memoized function. Passes all arguments to f.
	# When called, checks if cache contains a value that matches the arguments.
	wrapper <- function(...) {
		key <- digest(list(...))
		if (cache$has_key(key)) {
			cache$get(key)
		} else {
			value <- f(...)
			cache$set(key, value)
			value
		}
	}
	# Let's attach the cache to the result for later analysis.
	attr(wrapper, "cache") <- cache
	wrapper
}

# Let's create a memoizing version of the solve() function
memoizingSolve <- memoize(solve)

# Example:
# > m <- matrix(rnorm(1000000), nrow=1000)
# > system.time(str(memoizingSolve(m)))
#  num [1:1000, 1:1000] -0.01536 0.00194 0.00768 0.05821 0.03947 ...
#    user  system elapsed 
#   2.117   0.013   2.118 
# > system.time(str(memoizingSolve(m)))
#  num [1:1000, 1:1000] -0.01536 0.00194 0.00768 0.05821 0.03947 ...
#    user  system elapsed 
#   0.050   0.006   0.071 
