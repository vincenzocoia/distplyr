# distplyr (development version)

# distplyr 0.1.2

- If you have the tibble package installed, distplyr will now output tibbles wherever data frames were previously output.  

## Breaking changes

- The `get_` prefix has been removed from distributional quantities. `get_mean()` is now `mean()`, etc.
	- For now, the `get_` prefix still holds for distributional representations, like `get_cdf()`. 
- Make your own distribution object with `distribution()` instead of `dst()`, and checked with `is_distribution()`. 


# distplyr 0.1.1

This patch both fixes some problems in the previous release, as well as offering a step towards a bigger expansion.

- Some change in the functional representations: 
	- Changed random number generation from `randfn`, a functional representation, to the `realise()` and `realize()` functions. 
	- Changed `probfn` representation to be more specific: `pmf` or `density`
- Added the `enframe` suite of functions.
- Implement the beginnings of being able to specify your own distribution, with the `set_` suite of functions, after making an empty distribution with `dst()`. 

Additionally, there's some internal rearrangement, where the `get` functions call the `eval` functions, not vice versa.

# distplyr 0.1.0

The first version of `distplyr` is now available! Its functionality is rather limited at the moment, but is still useful, especially for its capability to handle a discrete component of a distribution. Here are the main features:

- Base distributions include step distributions, Gaussian, Uniform, and generalized Pareto.
- Operations include grafting (right) and mixing
- Distribution properties included are moment-related quantities, and extreme value index.
- Distribution representations are mostly comprehensive, perhaps only missing mean excess function and moment generating function.

Take a look at the "Vision" vignette to get a sense of where this package is headed.
