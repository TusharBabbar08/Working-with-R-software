#Reading bigger data files in R

When dataset is quite large , in those cases we prepare data in
a spreadsheet like , Excel , notepad etc.

In such case of complex data , we use read.csv() command to read those files

syntax:
read.csv(file.choose(),sep="," , header=T, row.names=n)

#where n= column number , with this we can change row names.
#header=T means,by default it reads the first row of the csv file and 
set this a name for each column.


other option of seperators are : sep="\t"

#Note: Save the excel file in csv format in working directory.

#Alternating command to read data in R

read.table(file.choose(),sep=";",header=F)
read.delim(file.choose(),sep=" ",header=T)
read.csv2(fule.choose())

#search for by default options yourself.
#Find the difference between read.csv() and read.csv2()

#Note: If length of column is not same in csv file. 
Then NA( not applicable or not availiable) will be shown after calling in 
R to make it rectangular object.

#Note: these commands generate data frame.
#Ques: check the class of these commands. 

#Converting from one object form to another
#Convert a data frame into a list
we can change data frame into a list by as.list() command


#matrix to data frame
use as.data.frame()

#convert matrix into a list
matrix->dataframe->list
as.list(as.data.frame(x))

#convert list into data frame
#use stack() to create two column data frame
# to get back the list, use unstack() command

#data frame to matrix
#use as.matrix()
