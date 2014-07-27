{\rtf1\ansi\ansicpg1252\cocoartf1187\cocoasubrtf400
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1440\margr1440\vieww25400\viewh13380\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural

\f0\fs24 \cf0 README\
--------------------------------------------------\
Contents:-\
1-	Introduction\
2-	Description of Run_Analysis.R\
\
\
Introduction:-\
Experiments were carried out on the SAMSUNG smartphone, to test the [****]. 30 volunteers were selected between the ages of 19 and 48. 70% of these volunteers were chosen to create the training data set (refereed to from here on as the train data set) and 30% were chosen to create the test set. The raw data had already been processed and we will be working on the processed data in this project. The aim is to take the the train and test data and merge them to form one data set which contains all the test subjects, and then calculates the mean for each activity for each test subjects on all the variables.\
\
Description:-\
The Run_Analysis obtains the average of the variables which measure the mean or STD. It produces the mean for each activity that each volunteer took part in.\
\
It works as follows:\
\
Step1: The script reads the activity_labels file which assigns a number to each activity. Then it reads the features file which identifies the column number for each variable. \
\
Step2: For the train set, the scrip reads the y_train file and subject_train file which identifies the ID of the subject/volunteer who performed the activities defined in the y_train file. It also reads the X_train file which contains the values of all the variables.\
\
Step3: The script then combines the subject ID, the activity ID and the variable ID into one data set.\
\
Step4: The script then searched for all the variables which contain the word "mean" or "std" in the variable name. REASON: due to the ambiguity of the project description we decided to choose all variables which included mean or std in the variable name.\
\
Step5: the script then creates a new data frame, which contains the subject id, the activity id and the columns defined in step4.\
\
Step6: steps 2 to 5 are repeated for the test data set.\
\
Step 7: we use the merge command to merge the train and test data sets.\
\
Step 8: we use the melt command to melt the data set created in step 7 to identify the variables from the subjects\
\
Step 9: We use the dcast command on the data frame created in Step 8 to calculate the mean of all the variables for each subject and each activity.\
\
Step 10: using the mapply and gsub functions we substitute the numerical IDs of the activities with the descriptive names defined in the activity labels file.\
\
Step 11: we load the data file which contains the descriptive labels of the variables and assign them to the data frame created in step 9.\
\
Step 12: the tidy data file is then saved as a csv file.\
\
}