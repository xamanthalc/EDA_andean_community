This text file provides general recommendations/information on how to navigate this folder. 

Main documents: 

	- codebook.txt: codebook of the final data set obtained after tidying data process. 
	- Long Form EDA.Rmd: long EDA, does not produce an html output because it contains shiny apps. 
	- executive_summary.Rmd and executive_summary.html: executive summary of EDA (and its html output). 
	- EDA_without_shiny.Rmd and EDA_without_shiny.html: same as long EDA but without interactive shiny apps. Made in case something goes wrong. Can also check the following link where it is posted: https://rpubs.com/xamanthalc/845804?fbclid=IwAR0i9Y96lp5DTxpUjI4nwAB3wuLY2lAmUQ0wdT9Bfb_IaLWW5wOUCPGqhG0 

Other documents:
	- Data Collected (folder): 
		- Raw Data.tsv: original data set obtained from source. 

	- Data Information (folder):
		- Bibliography.txt: citations of the original data set. 
		- Description of Survey.html: description of methodology of collection of data. 
		- Breakdown of Questions.pdf: explanation of every variable in the original questionary. 

	- Exploring Raw Data.R: exploration of original data set with comments. 
	- Processing data.R: script with the comments on all the process of tidying data. 

	- Data sets made in process (folder): contains versions of the data sets I created while cleaning the original data 
		- ready_to_use_49.csv: data set after first subsetting of variables and filtering of countries. It had 49 variables. 
		- sample_of_data_andean.csv: data set after taking a sample of according to the minimum combination of observations per year and country.
		- final.csv: final data set after all the tidying process.

	- univariate_big_topics_app: script with app to bar chart about main topics in the univariate section of long EDA. 
	- univariate_demo_app: script with app to bar chart about demographics in the univariate section of long EDA. 
	- univariate_plots: ll plot-making process with comments for static visualizations produced for the univariate section in long EDA.

	- bivariate_pie_chart_app.R: script with app to produce pie chart in the Bivariate section of long EDA. 
	- bivariate_plots.R: all plot-making process with comments for static visualizations produced for the Bivariate section in long EDA.
	- space_temp_plots: all plot-making process with comments for static visualizations produced for the space-temporal section in long EDA.
	- multivariate_plots.R: all plot-making process with comments for static visualizations produced for the Multivariate section in long EDA.
	- images(folder): this folder contains mages I attached in some documents.  
