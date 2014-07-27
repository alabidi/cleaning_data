{\rtf1\ansi\ansicpg1252\cocoartf1187\cocoasubrtf400
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural

\f0\fs24 \cf0 The Code Book\
_____________\
subject.id\
	Type: ID\
	Identifies the volunteer that carried out the activities, ranges between 1 and 30\
activity.id\
	Type: ID\
	Identifies the activity type, and can take on the following values\
		WALKING\
		WALKING_UPSTAIRS\
		WALKING_DOWNSTAIRS\
		SITTING\
		STANDING\
		LAYING\
\
The variable names can be understood as follows\
	Time prefix\
		Indicates that the variable is a time domain variable\
	Freq prefix\
		Indicated that the variable is a frequency domain variable\
	angle prefix\
		Indicates that the variable is the angle between two vectors, defined in the parenthesis.\
	Body\
		Indicates that the variable is a measure of the body's movement\
	Gravity\
		Indicates that the variable is a measure of the gravitational component of the motion.\
	Acceleration\
		Indicates that the variable is a measure of acceleration\
	Magnitude\
		Indicates that the variable is a measure of Magnitude\
	Gyro\
		Indicates that the variable was obtained from the Gyroscope\
	Jerk\
		Indicates that the variable is a measure of Jerk\
	Mean\
		Indicates that the variable is the average of all the measurements.\
	STD\
		Indicates that the variable is the standard deviation of all the measurements.\
	meanFreq\
		Weighted average of the frequency components to obtain a mean frequency\
	gravitymean\
		Weighted mean of the gravitational component\
	x,y,z \
		Indicates the direction of the measurement (i.e. was the body moving sideways or vertically etc.)\
\
\
EXAMPLE\
_________\
Putting the definitions of the variable names together, the variable:\
\
	Time.Body.Gyro.Jerk.Magnitude.STD\
\
Is read as:\
\
	The standard deviation of the magnitude of the body's Jerk obtained from the Gyroscope; measured in the time domain.}