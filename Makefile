copy:
	cp */*.tex /home/vegayon/Dropbox/Apps/Overleaf/MURI\ regressions/figures/ && \
	cp analysis/*.png  /home/vegayon/Dropbox/Apps/Overleaf/MURI\ regressions/figures/ 
analyze: 
	$(MAKE) clean && \
	R CMD BATCH analysis/univariate.R && \
	R CMD BATCH analysis/multivariate_step1.R && \
	R CMD BATCH analysis/multivariate_step2.R && \
	R CMD BATCH analysis/multivariate_step3.R && \
	R CMD BATCH analysis/multivariate_step4.R && \
	R CMD BATCH analysis/multivariate_step5.R &
clean:
	rm -f analysis/*.rds analysis/*.tex analysis/*.png

