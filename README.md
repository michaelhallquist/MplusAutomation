MplusAutomation
===============

The `MplusAutomation` package leverages the flexibility of the `R`
language to automate latent variable model estimation and interpretation
using Mplus, a powerful latent variable modeling program developed by
MuthÃ©n and MuthÃ©n (www.statmodel.com). Specifically, `MplusAutomation`
provides routines for creating related groups of models, running batches
of models, and extracting and tabulating model parameters and fit
statistics.

Installation
------------

You can install the latest release of `MplusAutomation` directly from
[CRAN](http://cran.r-project.org/package=MplusAutomation) by running

    install.packages("MplusAutomation")

Alternately, if you want to try out the latest development
`MplusAutomation` code, you can install it straight from github using
Hadley Wickham's `devtools` package. If you do not have `devtools`
installed, first install it and then install `MplusAutomation`.

    #install.packages("devtools")
    require(devtools)

    install_github("MplusAutomation", "michaelhallquist")

Questions
---------

For questions, answers, and updates on the status of the
`MplusAutomation` package, email or subscribe to the Google group
[list](https://groups.google.com/forum/#!forum/mplusautomation).

Examples
--------

You can find a detailed example of how to use the `MplusAutomation`
package in the
[vignette](https://github.com/michaelhallquist/MplusAutomation/blob/master/inst/doc/Vignette.pdf)
.

Here is an example of using the package to run a simple path model using
the `mtcars` dataset built into `R`.

    require(MplusAutomation)

    ## Loading required package: MplusAutomation
    ## Loading required package: plyr
    ## Loading required package: boot

    pathmodel <- mplusObject(
       TITLE = "MplusAutomation Example - Path Model;",
       MODEL = "
         mpg ON hp;
         wt ON disp;",
       rdata = mtcars)

    ## No R variables to use specified. 
    ## Selected automatically as any variable name that occurs in the MODEL or DEFINE section.

    fit <- mplusModeler(pathmodel, "mtcars.dat", modelout = "model1.inp", run = 1L)

    ## Wrote model to: model1.inp
    ## Wrote data to: mtcars.dat

    ## Warning: The file 'mtcars.dat' currently exists and will be overwritten

    ## 
    ## Running model: model1.inp 
    ## System command: C:\Windows\system32\cmd.exe /c cd "c:\Users\Joshua Wiley\OneDrive\Rpackages\MplusAutomation" && "Mplus" "model1.inp" 
    ## Reading model:  model1.out

That is all it takes to run Mplus! `MplusAutomation` takes care of
figuring out which variables from your `R` dataset are used in the model
and which are not (if it get's confused, you can also specify
`usevariables`). It creates a dataset suitable for Mplus, calls Mplus to
run the model on the dataset, and reads it back into `R`.

There is even pretty printing now. To see the results:

    require(texreg)
    htmlreg(extract(fit, summaries = c("Observations", "CFI", "SRMR")), single.row=TRUE)

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">
Statistical models
</caption>
  <tr>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Model 1</b>
</th>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-HP
</td>
    <td style="padding-right: 12px; border: none;">
-0.06 (0.01)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-DISP
</td>
    <td style="padding-right: 12px; border: none;">
0.01 (0.00)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-\>MPG
</td>
    <td style="padding-right: 12px; border: none;">
-1.02 (0.38)<sup style="vertical-align: 4px;">\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-Intercepts
</td>
    <td style="padding-right: 12px; border: none;">
29.59 (1.53)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-Intercepts
</td>
    <td style="padding-right: 12px; border: none;">
1.82 (0.18)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-\>MPG
</td>
    <td style="padding-right: 12px; border: none;">
14.04 (3.52)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-\>WT
</td>
    <td style="padding-right: 12px; border: none;">
0.21 (0.06)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="border-top: 1px solid black;">
Observations
</td>
    <td style="border-top: 1px solid black;">
32
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">
CFI
</td>
    <td style="padding-right: 12px; border: none;">
0.87
</td>
  </tr>
  <tr>
    <td style="border-bottom: 2px solid black;">
SRMR
</td>
    <td style="border-bottom: 2px solid black;">
0.14
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;" colspan="2">
<span
style="font-size:0.8em"><sup style="vertical-align: 4px;">***</sup>p \<
0.001, <sup style="vertical-align: 4px;">**</sup>p \< 0.01,
<sup style="vertical-align: 4px;">*</sup>p \< 0.05</span>
</td>
  </tr>
</table>

The fit is not great, to add some extra paths we can update the model.

    pathmodel2 <- update(pathmodel, MODEL = ~ . + "
        mpg ON disp;
        wt ON hp;")

    fit2 <- mplusModeler(pathmodel2, "mtcars2.dat", modelout = "model2.inp", run = 1L)

    ## Wrote model to: model2.inp
    ## Wrote data to: mtcars2.dat

    ## Warning: The file 'mtcars2.dat' currently exists and will be overwritten

    ## 
    ## Running model: model2.inp 
    ## System command: C:\Windows\system32\cmd.exe /c cd "c:\Users\Joshua Wiley\OneDrive\Rpackages\MplusAutomation" && "Mplus" "model2.inp" 
    ## Reading model:  model2.out

We can make some pretty output of both models:

    htmlreg(list(
      extract(fit, summaries = c("Observations", "CFI", "SRMR")),
      extract(fit2, summaries = c("Observations", "CFI", "SRMR"))),
      single.row=TRUE)

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
  <caption align="bottom" style="margin-top:0.3em;">
Statistical models
</caption>
  <tr>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;"></th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Model 1</b>
</th>
    <th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Model 2</b>
</th>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-HP
</td>
    <td style="padding-right: 12px; border: none;">
-0.06 (0.01)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
-0.02 (0.01)
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-DISP
</td>
    <td style="padding-right: 12px; border: none;">
0.01 (0.00)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
0.01 (0.00)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-\>MPG
</td>
    <td style="padding-right: 12px; border: none;">
-1.02 (0.38)<sup style="vertical-align: 4px;">\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
-0.73 (0.26)<sup style="vertical-align: 4px;">\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-Intercepts
</td>
    <td style="padding-right: 12px; border: none;">
29.59 (1.53)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
30.74 (1.27)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-Intercepts
</td>
    <td style="padding-right: 12px; border: none;">
1.82 (0.18)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
1.68 (0.19)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-\>MPG
</td>
    <td style="padding-right: 12px; border: none;">
14.04 (3.52)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
8.86 (2.21)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-\>WT
</td>
    <td style="padding-right: 12px; border: none;">
0.21 (0.06)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
    <td style="padding-right: 12px; border: none;">
0.19 (0.05)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
MPG\<-DISP
</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">
-0.03 (0.01)<sup style="vertical-align: 4px;">\*\*\*</sup>
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;"> 
WT\<-HP
</td>
    <td style="padding-right: 12px; border: none;"></td>
    <td style="padding-right: 12px; border: none;">
0.00 (0.00)
</td>
  </tr>
  <tr>
    <td style="border-top: 1px solid black;">
Observations
</td>
    <td style="border-top: 1px solid black;">
32
</td>
    <td style="border-top: 1px solid black;">
32
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;">
CFI
</td>
    <td style="padding-right: 12px; border: none;">
0.87
</td>
    <td style="padding-right: 12px; border: none;">
1.00
</td>
  </tr>
  <tr>
    <td style="border-bottom: 2px solid black;">
SRMR
</td>
    <td style="border-bottom: 2px solid black;">
0.14
</td>
    <td style="border-bottom: 2px solid black;">
0.00
</td>
  </tr>
  <tr>
    <td style="padding-right: 12px; border: none;" colspan="3">
<span
style="font-size:0.8em"><sup style="vertical-align: 4px;">***</sup>p \<
0.001, <sup style="vertical-align: 4px;">**</sup>p \< 0.01,
<sup style="vertical-align: 4px;">*</sup>p \< 0.05</span>
</td>
  </tr>
</table>

How to Help
-----------

If you have a tutorial or examples using `MplusAutomation`, please add
them to the github
[Wiki](https://github.com/michaelhallquist/MplusAutomation/wiki).

In addition, on the
[Wiki](https://github.com/michaelhallquist/MplusAutomation/wiki), is a
list of publications that cite or use `MplusAutomation`. If you use
`MplusAutomation` in your own work --- papers, posters, presentations,
etc. --- please add a citation to the list, and if possible, include an
abstract or link to the full text. This helps us get to know our users
and how `MplusAutomation` is being used.

Finally, if you find bugs or have suggestions for new features or ways
to enhance `MplusAutomation`, please let us know! Just click the
'Issues' button at the top of the github page or go
[here](https://github.com/michaelhallquist/MplusAutomation/issues?state=open)
and open a New Issue.

Lastly, if you use `MplusAutomation` and have space, we greatly
appreciating citations. In addition to being easier to track, the
recognition and credit help make it easier for us to continue putting
our time into developing and sharing this software!
