# MicrodensiteR

This package is made to manipulate microdensity profiles. The target of this profile are people working with ".pxb" file. It allows to add/delete earlywood (EW) and latewood (LW) limits, and to calculate microdensity variables (as: mean ring density, earlywood density, latewood density, ring width,...)    

## Installation

to install the development version:

```
devtools::install_github("ThChauvin/MicrodensiteR")
```

## Example

```
Import.tot("pxb",pathway="D:/Mdm/Profiles") # to import all profile with "pxb" in their name in the directory "D:/Mdm/Profiles".
ajustepositionlimite("pxb", intliss1 = 1/100, intliss2 = 1/80) # small correction of ring limits using smoothing functions (supsmu).  
k.bibf.tot("pxb") # positioning of the EW/LW limits. EW/LW limits is determined using the mean of the extremes.
calcul.mat("pxb", 0.025, subst1 = 1 ,subst2 = 3) # Calculation microdensity variables (mean ring density, earlywood density, latewood density, ring width,...) 
