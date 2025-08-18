# Basic TVM Calculator in R

## Overview

`tvm.R` is an [R](https://www.r-project.org/) script which allows for basic [time value of money (TVM)](https://www.investopedia.com/terms/t/timevalueofmoney.asp) calculations.

## Requirements

None.

## Usage

Download the R script into your project folder and include it in one of your R files with `source("tvm.R")`.

Call the function `tvm()`, passing values for 4 parameters, and the function will solve for the remaining variable and return it as a data frame.

| Parameter | Explanation |
| --- | --- |
| `pv` | present value |
| `fv` | future value |
| `pmt` | payment per period |
| `n` | number of periods |
| `i` | interest rate per period in percent |

Pay special attention to the algebraic signs – cash inflows should be positive and cash outflows should be negative.

*Hint: An optional `precision` parameter specifies the maximum number of decimal places of the output (standard is 2).*

## Examples

### Example 1

**Problem**

Consider an investment of USD 2000 for 6 periods at an interest rate of 5% per period. What will be the cash inflow at maturity?

**Command**

```r
tvm(pv=-2000, pmt=0, n=6, i=5)
```

**Output**

```
       fv
1 2680.19
```

**Answer**

The cash inflow will be USD 2680.19.

### Example 2

**Problem**

What is the implied market interest rate if the current price of a bond with a par value of EUR 1000, a coupon rate of 4%, and 3 years to maturity is EUR 895.82?

**Command**

```r
tvm(pv=-895.82, fv=1000, pmt=40/2, n=3*2) * 2
```

**Output**

```
     i
1 7.96
```

**Answer**

The implied interest rate is 7.96%.

### Example 3

**Problem**

What is the remaining principal balance on a fully amortising USD 150,000 30-year mortgage loan at an interest rate of 4.5% after 12 years and 4 months?

**Command**

```r
monthly.pmt <- tvm(pv=150000, fv=0, n=30*12, i=4.5/12)$pmt
tvm(fv=0, pmt=monthly.pmt, n=(30-12)*12-4, i=4.5/12)
```

**Output**

```
        pv
1 111014.4
```

**Answer**

The remaining balance on the loan is USD 111,014.40.

## Contact

If you have suggestions or feedback, feel free to contact me at <ntntnldmg@gmail.com>.
