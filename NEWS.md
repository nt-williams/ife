# ife 0.2.4

* Propagate `critical_value` through arithmetic operations. Binary operations between two estimates use the maximum critical value.

# ife 0.2.3

* Adding `^` method for raising estimates to a power via the delta method.

# ife 0.2.2

* Speed improvements through `collapse::fmean` and `collapse::fvar`.
* `std_error` is only calculated once.
* All properties, excluding `conf_int`, are now immutable.

# ife 0.2.1

* Previous version incorrectly ignored cluster level ids in standard error calculations (see issue #2)

# ife 0.2.0

* Adding `log` and `exp` methods
* Printing now returns up to 3 digits

# ife 0.1.12

* Conditional `importFrom` statement to fix bug when R version is less than 4.3.0.

# ife 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Added `critical_value` parameter
