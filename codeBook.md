
### Code Book of the TidyDataSet

### Overview 

the TidyDataSet represents a tidy subset of the Human Activity Recognition Using Smartphones Data available at the UCI HAR Dataset obtained from : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

the [TidyDataSet.txt](./TidyDataSet.txt) represents the wide form of tidy data, in our case this data set was generated by the [run_analysis.R](./run_analysis.R) script that was used to convert the raw dataset to this form.
more information about the run_analysis script and detailed walk through the data cleaning and transformation steps is available in the [READMRE.md](./README.md) file.

### Data Dictionary

1. **"Subject"** 
  + a factor descriping the subjects participated in the experiment.
  + values are from 1 to 30.
2. **"Activity"** 
  + a factor of activity name perofmed by the subjects. 
  + values are ( WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING).

  ***the feature variables below (#3 - #81) are each an average of the values collected for the test subject and activity specified in the data row. For each, the value is a numeric normalized and bounded within [-1, 1], more information about the original data is available in [features_info.txt](./features_info.txt) file that comes with the raw data set ***

3. **"TimeDomainBodyAccelerometerMeanX"** - the mean of the accelerometer measurement in time domain for X axis.
4. **"TimeDomainBodyAccelerometerMeanY"** - the mean of the accelerometer measurement in time domain for Y axis.                 
5. **"TimeDomainBodyAccelerometerMeanZ"** - the mean of the accelerometer measurement in time domain for Z axis.
6. **"TimeDomainBodyAccelerometerStandardDeviationX"** - the standard deviation of the accelerometer measurements in time domain for X axis.            
7. **"TimeDomainBodyAccelerometerStandardDeviationY"** - the standard deviation of the accelerometer measurements in time domain for Y axis.            
8. **"TimeDomainBodyAccelerometerStandardDeviationZ"** - the standard deviation of the accelerometer measurements in time domain for Z axis.             
9. **"TimeDomainGravityAccelerometerMeanX"** - the mean of the Gravity Accelerometer measurement in time domain for X axis.
10.**"TimeDomainGravityAccelerometerMeanY"** - the mean of the Gravity Accelerometer measurement in time domain for Y axis.
11. **"TimeDomainGravityAccelerometerMeanZ"** - the mean of the Gravity Accelerometer measurement in time domain for Z axis.
12. **"TimeDomainGravityAccelerometerStandardDeviationX"** - the standard deviation of the Gravity Accelerometer measurement in time domain for X axis.
13. **"TimeDomainGravityAccelerometerStandardDeviationY"** - the standard deviation of the Gravity Accelerometer measurement in time domain for Y axis.
14. **"TimeDomainGravityAccelerometerStandardDeviationZ"** - the standard deviation of the Gravity Accelerometer measurement in time domain for Z axis.
15. **"TimeDomainBodyAccelerometerJerkMeanX"** - the mean of the jerk signal derived from the Accelerometer measurement in time domain for X axis.
16. **"TimeDomainBodyAccelerometerJerkMeanY"** - the mean of the jerk signal derived from the Accelerometer measurement in time domain for Y axis.
17. **"TimeDomainBodyAccelerometerJerkMeanZ"** - the mean of the jerk signal derived from the Accelerometer measurement in time domain for Z axis.
18. **"TimeDomainBodyAccelerometerJerkStandardDeviationX"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in time domain for X axis.
19. **"TimeDomainBodyAccelerometerJerkStandardDeviationY"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in time domain for Y axis.
20. **"TimeDomainBodyAccelerometerJerkStandardDeviationZ"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in time domain for Z axis.
21. **"TimeDomainBodyGyroscopeMeanX"** - the mean of the Gyroscope measuremnet in time domain for X axis.
22. **"TimeDomainBodyGyroscopeMeanY"** - the mean of the Gyroscope measuremnet in time domain for Y axis.
23. **"TimeDomainBodyGyroscopeMeanZ"** - the mean of the Gyroscope measuremnet in time domain for Z axis.
24. **"TimeDomainBodyGyroscopeStandardDeviationX"** - the standard deviation of the Gyroscope measuremnet in time domain for X axis.
25. **"TimeDomainBodyGyroscopeStandardDeviationY"** - the standard deviation of the Gyroscope measuremnet in time domain for Y axis.
26. **"TimeDomainBodyGyroscopeStandardDeviationZ"** - the standard deviation of the Gyroscope measuremnet in time domain for Z axis.
27. **"TimeDomainBodyGyroscopeJerkMeanX"** - the mean of the jerk signal derived from the Gyroscope measuremnet in time domain for X aixs.
28. **"TimeDomainBodyGyroscopeJerkMeanY"** - the mean of the jerk signal derived from the Gyroscope measuremnet in time domain for Y aixs.
29. **"TimeDomainBodyGyroscopeJerkMeanZ"** - the mean of the jerk signal derived from the Gyroscope measuremnet in time domain for Z aixs.
30. **"TimeDomainBodyGyroscopeJerkStandardDeviationX"** - the standard deviation of the jerk signal derived from the Gyroscope measuremnet in time domain for X aixs.
31. **"TimeDomainBodyGyroscopeJerkStandardDeviationY"** - the standard deviation of the jerk signal derived from the Gyroscope measuremnet in time domain for Y aixs.
32. **"TimeDomainBodyGyroscopeJerkStandardDeviationZ"** - the standard deviation of the jerk signal derived from the Gyroscope measuremnet in time domain for Z aixs.
33. **"TimeDomainBodyAccelerometerMagnitudeMean"** - the mean of magnitude of Accelerometer measurement in time domain.
34. **"TimeDomainBodyAccelerometerMagnitudeStandardDeviation"** - the standard deviation of magnitude of Accelerometer measurement in time domain.
35. **"TimeDomainGravityAccelerometerMagnitudeMean"** - the mean of magnitude of Gravity Accelerometer measurement in time domain.
36. **"TimeDomainGravityAccelerometerMagnitudeStandardDeviation"** - the standard deviation of magnitude of Gravity Accelerometer measurement in time domain.
37. **"TimeDomainBodyAccelerometerJerkMagnitudeMean"** - the mean of magnitude of jerk signal derived from  Accelerometer measurement in time domain.
38. **"TimeDomainBodyAccelerometerJerkMagnitudeStandardDeviation"** - the standard deviation of magnitude of jerk signal derived from  Accelerometer measurement in time domain.
39. **"TimeDomainBodyGyroscopeMagnitudeMean"** - the mean of magnitude of Gyroscope measurement in time domain.
40. **"TimeDomainBodyGyroscopeMagnitudeStandardDeviation"** - the standard deviation of magnitude of Gyroscope measurement in time domain.
41. **"TimeDomainBodyGyroscopeJerkMagnitudeMean"** - the mean of magnitude of jerk signal derived from Gyroscope measurement in time domain.
42. **"TimeDomainBodyGyroscopeJerkMagnitudeStandardDeviation"** - the standard deviation of magnitude of jerk signal derived from Gyroscope measurement in time domain.
43. **"FrequencyDomainBodyAccelerometerMeanX"** - the mean of the Accelerometer measurement in frequency domain for X axis.
44. **"FrequencyDomainBodyAccelerometerMeanY"** - the mean of the Accelerometer measurement in frequency domain for Y axis.
45. **"FrequencyDomainBodyAccelerometerMeanZ"** - the mean of the Accelerometer measurement in frequency domain for Z axis.
46. **"FrequencyDomainBodyAccelerometerStandardDeviationX"** - the standard deviation of the Accelerometer measurement in frequency domain for X axis.
47. **"FrequencyDomainBodyAccelerometerStandardDeviationY"** - the standard deviation of the Accelerometer measurement in frequency domain for Y axis.
48. **"FrequencyDomainBodyAccelerometerStandardDeviationZ"** - the standard deviation of the Accelerometer measurement in frequency domain for Z axis.
49. **"FrequencyDomainBodyAccelerometerMeanFrequencyX"** - the mean frequency of the Accelerometer measurement in frequency domain for X axis.
50. **"FrequencyDomainBodyAccelerometerMeanFrequencyY"** - the mean frequency of the Accelerometer measurement in frequency domain for Y axis.
51. **"FrequencyDomainBodyAccelerometerMeanFrequencyZ"** - the mean frequency of the Accelerometer measurement in frequency domain for Z axis.
52. **"FrequencyDomainBodyAccelerometerJerkMeanX"** - the mean of the jerk signal derived from the Accelerometer measurement in frequency domain for X axis.
53. **"FrequencyDomainBodyAccelerometerJerkMeanY"** - the mean of the jerk signal derived from the Accelerometer measurement in frequency domain for Y axis.
54. **"FrequencyDomainBodyAccelerometerJerkMeanZ"** - the mean of the jerk signal derived from the Accelerometer measurement in frequency domain for Z axis.
55. **"FrequencyDomainBodyAccelerometerJerkStandardDeviationX"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in frequency domain for X axis.
56. **"FrequencyDomainBodyAccelerometerJerkStandardDeviationY"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in frequency domain for Y axis.
57. **"FrequencyDomainBodyAccelerometerJerkStandardDeviationZ"** - the standard deviation of the jerk signal derived from the Accelerometer measurement in frequency domain for Z axis.        
58. **"FrequencyDomainBodyAccelerometerJerkMeanFrequencyX"** - the mean of the frequency of the jerk signal derived from the Accelerometer measurement in frequency domain for X axis.            
59. **"FrequencyDomainBodyAccelerometerJerkMeanFrequencyY"** - the mean of the frequency of the jerk signal derived from the Accelerometer measurement in frequency domain for Y axis.            
60. **"FrequencyDomainBodyAccelerometerJerkMeanFrequencyZ"** - the mean of the frequency of the jerk signal derived from the Accelerometer measurement in frequency domain for Z axis.            
61. **"FrequencyDomainBodyGyroscopeMeanX"** - the mean of the Gyroscope measurement in frequency domain for X axis.            
62. **"FrequencyDomainBodyGyroscopeMeanY"** - the mean of the Gyroscope measurement in frequency domain for Y axis.            
63. **"FrequencyDomainBodyGyroscopeMeanZ"** - the mean of the Gyroscope measurement in frequency domain for Z axis. 
64. **"FrequencyDomainBodyGyroscopeStandardDeviationX"** - the standard deviation of the Gyroscope measurement in frequency domain for X axis.
65. **"FrequencyDomainBodyGyroscopeStandardDeviationY"** - the standard deviation of the Gyroscope measurement in frequency domain for Y axis.                
66. **"FrequencyDomainBodyGyroscopeStandardDeviationZ"** - the standard deviation of the Gyroscope measurement in frequency domain for Z axis.
67. **"FrequencyDomainBodyGyroscopeMeanFrequencyX"** - the mean of the frequency of Gyroscope measurement in frequency domain for X axis.                   
68. **"FrequencyDomainBodyGyroscopeMeanFrequencyY"** - the mean of the frequency of Gyroscope measurement in frequency domain for Y axis.
69. **"FrequencyDomainBodyGyroscopeMeanFrequencyZ"** - the mean of the frequency of Gyroscope measurement in frequency domain for Z axis.                   
70. **"FrequencyDomainBodyAccelerometerMagnitudeMean"** - the mean of the magnitude of the Accelerometer measurement in frequency domain.                  
71. **"FrequencyDomainBodyAccelerometerMagnitudeStandardDeviation"** - the standard deviation  of the magnitude of the Accelerometer measurement in frequency domain.                  
72. **"FrequencyDomainBodyAccelerometerMagnitudeMeanFrequency"**- the mean of the frequency of the magnitude of the Accelerometer measurement in frequency domain.                          
73. **"FrequencyDomainBodyAccelerometerJerkMagnitudeMean"** - the mean of the magnitude of the jerk signal derived from the Accelerometer measurement in frequency domain.          
74. **"FrequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation"**- the standard deviaton of the magnitude of the jerk signal derived from the Accelerometer measurement in frequency domain.
75. **"FrequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency"** - the mean of the frequency of the magnitude of jerk signal derived from the Accelerometer measurement in frequency domain.  
76. **"FrequencyDomainBodyGyroscopeMagnitudeMean"** - the mean of the magnitude of the Gyroscope measurement in frequency domain.
77. **"FrequencyDomainBodyGyroscopeMagnitudeStandardDeviation"** - the standard deviation of the magnitude of the Gyroscope measurement in frequency domain.        
78. **"FrequencyDomainBodyGyroscopeMagnitudeMeanFrequency"** - the mean of the frequency of the magnitude of the Gyroscope measurement on frequency domain.
79. **"FrequencyDomainBodyGyroscopeJerkMagnitudeMean"** - the mean of the magnitude of the jerk signal derived form the Gyroscope measurement in frequency domain.
80. **"FrequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation"** - the standard deviation of the magnitude of jerk signal derived from the Gyroscope measurement in frequency domain.
81. **"FrequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency"** - the mean of the frequency of the magnitude of the jerk signal derived from the Gyroscope measurement in frequency domain.
     
### New Variable Naming

- all pracets and special characters were removed by `make.names()` function and `gsub()`.
- if the variable name has a 't' prefix at the beginning then it was replaced by 'TimeDomain'.
- if the variable name has a 'f' prefix at the beginning then it was replaced by 'FrequencyDomain'.
- 'BodyBody' was replaced by 'Body'
- 'Acc' was replaced by 'Accelerometer'.
- 'Mag' was replaced by 'Magnitude'.
- 'Gyro' was replaced by 'Gyroscope'.
- 'Freq' was replaced by 'Frequency'.
- 'std' was replaced by 'StandardDeviation'.
- 'mean' was replaced by 'Mean'.
