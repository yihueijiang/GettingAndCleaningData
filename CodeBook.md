# Code Book Description
This code book describes the variables, summarize of the data, and any transformations or work that performed to clean up the data. This work results a tidy data, named tidy.txt in relevant repo.

# Data Description
1. The experiments have been carried out with a group of 30 volunteers (age 19-48 years old). 
2. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 
3. captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
4. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

### For each record it is provided:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### The dataset includes the following files:

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

### Features 
The features selected for this database come from the accelerometer and gyroscope three-axial raw signals tAcc-XYZ and tGyro-XYZ.The features were further combined with a variety of estimated variables, such as mean value, standard deviation, largest and smalles value in the set etc. This adds up to over 550 of different indicators in total. The file 'features.txt' lists all of the variables.


# Transformations
The transformations are achieved by the follws steps, script in run_analysis.R.
1. Install packages, "dplyr","reshape2", and "data.table"
2. Download the zip file and unzip it.
3. Loads activity label names and features datasets
4. Extract the measurements on the mean and standard deviation for each measurement
5. Load training datasets and give it column names with labels
6. Load test datasets and give it column names with labels
7. Merge training and test datasets
8. Label activity name in the merged data
9. Creates an independent tidy dataset based on mean and standard deviations




