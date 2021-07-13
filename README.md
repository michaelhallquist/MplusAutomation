MplusAutomation
===============

The `MplusAutomation` package leverages the flexibility of the `R`
language to automate latent variable model estimation and interpretation
using Mplus, a powerful latent variable modeling program developed by
Muthén and Muthén (www.statmodel.com). Specifically, `MplusAutomation`
provides routines for creating related groups of models, running batches
of models, and extracting and tabulating model parameters and fit
statistics.

Installation
------------

You can install the latest release of `MplusAutomation` directly from
[CRAN](https://CRAN.R-project.org/package=MplusAutomation) by running

    install.packages("MplusAutomation")

Alternately, if you want to try out the latest development
`MplusAutomation` code, you can install it straight from github using
Hadley Wickham's `devtools` package. If you do not have `devtools`
installed, first install it and then install `MplusAutomation`.

    #install.packages("devtools")
    library(devtools)

    install_github("michaelhallquist/MplusAutomation")

Questions
---------

For questions, answers, and updates on the status of the
`MplusAutomation` package, email or subscribe to the Google group
[list](https://groups.google.com/forum/#!forum/mplusautomation).

Examples
--------

You can find a detailed example of how to use the `MplusAutomation`
package in the
[vignette](https://michaelhallquist.github.io/MplusAutomation/articles/vignette.html)
.

Here is an example of using the package to run a simple path model using
the `mtcars` dataset built into `R`.

    library(MplusAutomation)

    pathmodel <- mplusObject(
       TITLE = "MplusAutomation Example - Path Model;",
       MODEL = "
         mpg ON hp;
         wt ON disp;",
	   OUTPUT = "CINTERVAL;",
       rdata = mtcars)

    ## R variables selected automatically as any variable name that occurs in the MODEL, VARIABLE, or DEFINE section.
    ## If any issues, suggest explicitly specifying USEVARIABLES.
    ## A starting point may be:
    ## USEVARIABLES = mpg disp hp wt;

    fit <- mplusModeler(pathmodel, modelout = "model1.inp", run = 1L)


That is all it takes to run Mplus! `MplusAutomation` takes care of
figuring out which variables from your `R` dataset are used in the model
and which are not (if it get's confused, you can also specify
`usevariables`). It creates a dataset suitable for Mplus, calls Mplus to
run the model on the dataset, and reads it back into `R`.

There is even pretty printing now. To see the results:

```r
library(texreg)
screenreg(fit, summaries = c("Observations", "CFI", "SRMR"), single.row=TRUE)
```

<pre><code>
==================================
                  Model 1
----------------------------------
 MPG&lt;-HP          -0.06 (0.01) ***
 WT&lt;-DISP          0.01 (0.00) ***
 WT&lt;-&gt;MPG         -1.02 (0.38) **
 MPG&lt;-Intercepts  29.59 (1.53) ***
 WT&lt;-Intercepts    1.82 (0.18) ***
 MPG&lt;-&gt;MPG        14.04 (3.52) ***
 WT&lt;-&gt;WT           0.21 (0.06) ***
----------------------------------
Observations      32
CFI                0.87
SRMR               0.14
==================================
*** p &lt; 0.001, ** p &lt; 0.01, * p &lt; 0.05
</code></pre>

The fit is not great, to add some extra paths we can update the model.

    pathmodel2 <- update(pathmodel, MODEL = ~ . + "
        mpg ON disp;
        wt ON hp;")

    fit2 <- mplusModeler(pathmodel2, modelout = "model2.inp", run = 1L)


We can make some pretty output of both models:

```r
screenreg(list(
  extract(fit, summaries = c("Observations", "CFI", "SRMR")),
  extract(fit2, summaries = c("Observations", "CFI", "SRMR"))),
  single.row=TRUE)
```

<pre><code>
====================================================
                  Model 1           Model 2
----------------------------------------------------
 MPG&lt;-HP          -0.06 (0.01) ***  -0.02 (0.01)
 WT&lt;-DISP          0.01 (0.00) ***   0.01 (0.00) ***
 WT&lt;-&gt;MPG         -1.02 (0.38) **   -0.73 (0.26) **
 MPG&lt;-Intercepts  29.59 (1.53) ***  30.74 (1.27) ***
 WT&lt;-Intercepts    1.82 (0.18) ***   1.68 (0.19) ***
 MPG&lt;-&gt;MPG        14.04 (3.52) ***   8.86 (2.21) ***
 WT&lt;-&gt;WT           0.21 (0.06) ***   0.19 (0.05) ***
 MPG&lt;-DISP                          -0.03 (0.01) ***
 WT&lt;-HP                              0.00 (0.00)
----------------------------------------------------
Observations      32                32
CFI                0.87              1.00
SRMR               0.14              0.00
====================================================
*** p &lt; 0.001, ** p &lt; 0.01, * p &lt; 0.05
</code></pre>


If you want confidence intervals, those can also be printed, 
so long as they were requested as part of the output
(we did in the initial model, which propogates to later models
that were updated()ed based on the original model):

```r
screenreg(list(
  extract(fit, cis=TRUE, summaries = c("Observations", "CFI", "SRMR")),
  extract(fit2, cis=TRUE, summaries = c("Observations", "CFI", "SRMR"))),
  single.row=TRUE)
```

<pre><code>
================================================================
                  Model 1                 Model 2               
----------------------------------------------------------------
 MPG<-HP          -0.06 [-0.08; -0.05] *  -0.02 [-0.05;  0.00]  
 WT<-DISP          0.01 [ 0.00;  0.01] *   0.01 [ 0.01;  0.01] *
 WT<->MPG         -1.02 [-1.77; -0.27] *  -0.73 [-1.25; -0.21] *
 MPG<-Intercepts  29.59 [26.59; 32.58] *  30.74 [28.25; 33.22] *
 WT<-Intercepts    1.82 [ 1.46;  2.17] *   1.68 [ 1.31;  2.04] *
 MPG<->MPG        14.04 [ 7.14; 20.95] *   8.86 [ 4.52; 13.20] *
 WT<->WT           0.21 [ 0.10;  0.32] *   0.19 [ 0.10;  0.28] *
 MPG<-DISP                                -0.03 [-0.04; -0.02] *
 WT<-HP                                   -0.00 [-0.00;  0.00]  
----------------------------------------------------------------
Observations      32                      32                    
CFI                0.87                    1.00                 
SRMR               0.14                    0.00                 
================================================================
* 0 outside the confidence interval
</code></pre>


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
