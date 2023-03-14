
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covariance Estimation for Matrix-variate Data

Step 1<br>
Preprocessing the data<br>
the entire preprocessed data set can be found [here](https://drive.google.com/file/d/1T1Na_GSJeHfgSbME0Y7FJluwIs_9qfD9/view?usp=share_link)

UPDATE:
I realized that the time interval 99 is never taken by creating a null integer vector for each observation, so I modified the code to save matrices with T=98 instead of 99<br>
with this code:<br>
```R
file_list=list.files()
for (file in file_list) {
 load(file) 
  mfccs<- mfccs[-99,,]
  save(mfccs, file = gsub(" ", "", file))
}
```<br>
Step 2<br>
Classification of the words "dog" and "cat" with QDA and LDA with MLE covariance estimation <br>
