% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missed_quiz_checker.R
\name{track_students}
\alias{track_students}
\title{Missed Quiz Checker}
\usage{
track_students(tracker_path, output_path, roster, current_gb,
  assignment_column_name)
}
\arguments{
\item{tracker_path}{A path to the file you use to keep track of student progress or the path to where you want to create that file}

\item{output_path}{A path to a location where you want the output files saved. Must end with '.csv'.  Output file will include students who have increased their missed assignment count. Tracker file itself will remain in tracker_path location.}

\item{roster}{A dataframe (probably created from CLIPS) with a roster and email addresses for all students in class. Must be supplied if the tracker file does not already exist. Make sure that this dataframe has three columns with these exact titles: "Name", "EID", and "Email".}

\item{current_gb}{A dataframe that has grades for the most recent assignment in the series being tracked. Do not set a default grade before running this function or it will not work.}

\item{assignment_column_name}{The name of the column that contains the new assignment to test}
}
\value{
Outputs two files. The first is an updated version of the tracker file (or creates this if it does not already exist). The second is a file that contains only those entries that have increased their missed assignment number.

Optionally, will output a third file if the function detects that your gradebook file contains new students not included in the tracker file.
}
\description{
Use this function to track students who do not complete assignments. If you have a series of similar assignments in a given category, you can use this to identify students who are struggling before it is too late for them to save their grade.
}
