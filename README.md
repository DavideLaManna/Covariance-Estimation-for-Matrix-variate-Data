<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covariance Estimation for Matrix-variate Data

## Week 1<br>

Workflow setup on github<br>

## week 2<br>

Review of chapters 3 and 4 by Murphy (2012) and general reading of the papers provided on covariance estimation methods<br>

## week 3<br>

Preprocessing the data<br> the entire preprocessed data set can be found [here](https://drive.google.com/file/d/1fgwxos-W09WDOus4223DKIhLCvJItQsz/view?usp=share_link)

UPDATE: I realized that the time interval 99 is never taken by creating a null integer vector for each observation, so I modified the code to save matrices with T=98 instead of 99<br> with this code:<br>

``` r
file_list=list.files()
for (file in file_list) {
 load(file) 
  mfccs<- mfccs[-99,,]
  save(mfccs, file = gsub(" ", "", file))
}
```

Classification of the words "dog" and "cat" with QDA and LDA with MLE covariance estimation <br>

## Week 4<br>

Fixed the data preprocessing algorithm, which now only contains files of 1 minute duration and does not contain arrays with NA elements. T=98 appears to be the number of framerates per 1 second file and not 99, using a proper function ('compute_mfccs.R') to load the data and preprocess it.<br>

## Week 5<br>

set wintime=0.01 in compute_mfccs to obtain a right time partition. <br> created from scratch algorithm for LDA classification using an inverse problem to not compute the inverse of the covariance matrix.<br> Compute the same LDA classifier for classes of arbitrary amplitude <br>

## Week 6<br>
Reading of Masak, Sarkar & Panaretos (2022) and Hoff, McCormack & Zhang (2022), first attempt of implementation separable covariance estimation
<br>

## Week 7<br>
preparation of the mid-semester presentation

## Week 12<br>
<ul>
  <li>Implementation of separable covariance R algorithms for R=2 and 3 in LDA and QDA cases, comparing them with previous algorithms.</li>
  <li>Implementation of CSE algorithm and comparison with previous algorithms.</li>
  <li>Reading Lynch et. al's article on weak stability and Zatapa et. al's article on partial separability.</li>
  <li>Homogenization class test. As expected, unsatisfactory results, what little is gained in the accuracy of the algorithm is lost by reducing the class size.</li>
  <li>Starting experiments on calculating the logarithm of the determinant of covariance matrix estimates in QDA. Error: infinite value. We decide to calculate the sum of the logarithms of the (positive) eigenvalues, which brings reasonable results.</li>
  <li>Implementation of a QDA algorithm that takes these values into account in the score.</li>
</ul>


## References

-   Murphy (2012) Machine learning: a probabilistic perspective
-   Masak, Sarkar & Panaretos (2022) Separable Expansions for Covariance Estimation via the Partial Inner Product
-   Hoff, McCormack & Zhang (2022) Core Shrinkage Covariance Estimation for Matrix-variate Data
-  Dutilleul (1999) The mle algorithm for the matrix normal distribution
-  Lynch & Chen (2018) A test of weak separability for multi-way functional data, with application to brain connectivity studies
-  Zapata, Oh & Petersen (2022) Partial Separability and Functional Graphical Models for Multivariate Gaussian Processes
