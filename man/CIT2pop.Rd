% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence interval for mean for two pop.R
\name{CIT2pop}
\alias{CIT2pop}
\title{Confidence intervals (CI) and tests (T) for the mean of two populations.
Tests H0: mu1 = mu2 vs H1: mu1 != mu2
This function is a guide to the test between for the mean of two population when the samples are independent.}
\usage{
CIT2pop(x, y, datas, alpha, step = "1: Describe")
}
\arguments{
\item{x}{it is a numerical variable.}

\item{y}{it is a categorical variable.}

\item{datas}{it is the dataset in data.frame format.}

\item{alpha}{significance level.}

\item{step}{if it is \emph{"4: t-test for two pop. means"} it is the fourth step and it is based on t-test; the t-test is provided with equal variances (pooled variance in t-statistics) if it was not rejected the null hypothesis at the third step; the t-test for unequal variances (Welch test) if it was rejected the null hypothesis at the third step.}
}
\value{
four steps: 1-descriptive statistics for each group; 2-confidence interval for the mean and the variance for each group; 3-test F check if the variances can be assumed equal or not; 4-t-test based on the assumption or not of equal variances. For step 3 and 4 are drawn plots in order to reject the null hypothesis
}
\description{
Confidence intervals (CI) and tests (T) for the mean of two populations.
Tests H0: mu1 = mu2 vs H1: mu1 != mu2
This function is a guide to the test between for the mean of two population when the samples are independent.
}
\examples{
library(BAStat)

xy<-c(29,	25,	24,	26,	29,	31,	30,	28,	32,	22, 14,	12,	11,	15,	26,	18,	27,	25,	48,	11)
group<-c(rep("Bus A",10),rep("Bus B",10))
data1<-data.frame(xy,group)

CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="1: Describe")
CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="2: Analyze per group")
CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="3: Check the variances")
CIT2pop(x=data1$xy,y=data1$group,datas=data1,alpha=0.05,step="4: t-test for two pop. means")
#one more example
ex2<-c(29,	35,	41,	26,	29,	37,	31,	28,	32, 34,	22,	19,	25,	26,	28,	35)		
group<-c(rep("Bus A",9),rep("Bus B",7))
data2<-data.frame(ex2,group)

CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="1: Describe")
CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="2: Analyze per group")
CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="3: Check the variances")
CIT2pop(x=data2$ex2,y=data2$group,datas=data2,alpha=0.05,step="4: t-test for two pop. means")
}
