## Patch

Patch for one function resolving stability issue revealed by M1mac check with development version of R
N.B. "R Mac Builder" and check_mac_release() currently not working so patch not tested as thoroughly as normal

## Resubmission
This is a resubmission. In this version I have:

* Added ISBN reference to DESCRIPTION
* Added return values to print.ANSMstat() and print.ANSMtest() functions
* Removed rogue markers commenting out examples for functions kstest.ANSM(), med.test(), rng.test() and sgn.test()

N.B. I do not believe that ch3.Rd, ch4.Rd, ch5.Rd, ch6.Rd, ch7.Rd, ch9.Rd contain commented out code lines, as suggested in previous submission response

## Resubmission
This is a resubmission. In this version I have:

* Incremented version number to 1.0.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
