# Project 4: Algorithm implementation and evaluation: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Spring 2020

+ Team 4
+ Projec title: Algorithm implementation and evaluation: Collaborative Filtering
+ Team members
	+ Huize Huang
	+ Hanbo Jiao
	+ Guoying Li
	+ Yuqiao Liu
	+ Jinxu Xiang
+ Project summary: In this project, our group implement matrix factorization by focusing on different versions of alternating least squares algorithm for different Regularizations and Kernel Ridge Regression as Post-processing. The objective is to produce a good prediction of users’ preferences for movies on on the MovieLens dataset. For this project, our team is assigned with Pairing combination 12 + 14 from the Collaborative, which is showed below. For evaluation, we compared RMSE results for different methods. Our group used R language to product model and reports.
	+ Alternating Least Squares + Kernel Ridge Regression
	+ Alternating Least Squares + Penalty of magnitudes + Temporal Dynamics + Kernel Ridge Regression

Please see [main_final_group4.pdf](doc/main_final_group4.pdf) for final report.

	
**Contribution statement**: [default] All team members listed above discussed the work of this project. 
For the algorithms, Yuqiao Liu worked on ALS + KRR functions and conducted them on the dataset. Hanbo Jiao and Jinxu Xiang worked on ALS + R1R3 + KRR functoins and conducted them on the dataset. Huize Huang helped them conduct the functions on the dataset. 
For the other work, Guoying Li contriubted to the main_basic.Rmd. Huize integrated the codes of the final main file, with Yuqiao and Hanbo providing some edits. Hanbo worked on the presentation slides. Huize contributed to the readme files on GitHub. All team members contributed to the GitHub files organizations.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
