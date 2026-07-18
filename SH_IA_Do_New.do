*******************************************************
*   STATA DO-FILE: Latent Variable & Regression Analysis
*   Purpose: To analyze the relationship between 'perceived belongingness' and
*            'internet and phone dependency'.
*******************************************************

*------------------------------------------------------*
* 1. FACTOR ANALYSIS (Initial)
*------------------------------------------------------*
* Perform exploratory factor analysis on psychological, social, and behavioral indicators
factor friend2 cell1_1 cell1_3 cell1_5 worrya worryb worryc limita limitb limitc post1a post1b post1e group1 soc4a soc4b soc4c soc4d cell3a cell3b cell3c intreq

* Sampling adequacy checks- Kaiser-Meyer-Olkin measure of sampling adequacy
estat kmo                  

* Bartlett's test of sphericity 
factortest friend2 cell1_1 cell1_3 cell1_5 worrya worryb worryc limita limitb limitc post1a post1b post1e group1 soc4a soc4b soc4c soc4d cell3a cell3b cell3c intreq


*------------------------------------------------------*
* 2. DETERMINE NUMBER OF FACTORS
*------------------------------------------------------*
* Generate scree plot to visually determine the optimal number of factors to retain
screeplot

* Conduct factor analysis using 3 factors. [According to The Kaiser Criterion, Eigenvalue > 1 signals that the associated term, factor, or mode is significant and contributes meaningfully. From the screeplot, we see, Factor 1 has Eigenvalue of ~3.3, Factor 2 has ~1.6, Factor 3 has ~1.2, and Factor 4 has ~0.6. So we can choose 1,2,3. Choosing 3 factors because choosing 1 or 2 factors may ignore meaningful variance (in factors 2 & 3, or 3, respectively)].
 
factor friend2 cell1_1 cell1_3 cell1_5 worrya worryb worryc limita limitb limitc post1a post1b post1e group1 soc4a soc4b soc4c soc4d cell3a cell3b cell3c intreq, factors(3)

* Apply orthogonal varimax rotation to the 3-factor solution so the loadings are interpretable.[A factor loading of 0.30 is the minimum threshold for practical significance (Hair et al., 2010), as it indicates that the factor explains at least 9% of the variance in that variable (0.30² = 0.09).]
rotate, varimax blanks(0.30)

*[The rotation matrix confirms the validity of the three-factor solution, as the near-unity diagonal values (0.9858, 0.9064, and 0.9206) indicate that each factor retained its distinctiveness after varimax rotation, with minimal overlap between factors.]


*[The rotated factor loadings reveal a clear three-factor structure: representing general attitudes and behavioral intentions (Factor 1), social norms (Factor 2), and perceived limitations (Factor 3), whereby varimax rotation improved the interpretability of the solution by redistributing variance more distinctly across factors, consistent with Hair et al.'s (2010) recommendation that loadings ≥ 0.30 be retained for meaningful factor interpretation.] 

*------------------------------------------------------*
* 3. STRUCTURAL EQUATION MODEL (SEM)
*------------------------------------------------------*
* Run the validated three-factor SEM measurement model, where Attitudes, SocialNorms, and Limitations are specified as latent constructs measured by their respective observed indicator variables confirmed through varimax-rotated exploratory factor analysis 
sem (Attitudes -> cell3a cell3b cell3c worrya worryb post1a post1b post1e)(SocialNorms -> soc4a soc4b soc4c soc4d) (Limitations -> limita limitb limitc)

*All p-values = 0.000, meaning every variable significantly measures its assigned latent variable.

* Assess overall model fit using multiple indices (CFI, TLI, RMSEA, SRMR) given that the LR test alone is insufficient to evaluate model fit, particularly in large samples
estat gof, stats(all)

*Add modification indices to see what Stata suggests
estat mindices

* Re-specify the three-factor SEM allowing correlated residuals between conceptually similar item pairs identified through modification indices,specifically worrya/worryb, post1b/post1e, and cell3a/cell3b, which share systematic variance beyond their assigned latent constructs, this is theoretically justified as these pairs measure closely related aspects of the same concept (Byrne, 2016)
sem (Attitudes -> cell3a cell3b cell3c worrya worryb post1a post1b post1e) (SocialNorms -> soc4a soc4b soc4c soc4d) (Limitations -> limita limitb limitc), cov(e.worrya*e.worryb) cov(e.post1b*e.post1e) cov(e.cell3a*e.cell3b)

* Re-assess model fit after allowing correlated residuals to determine whether the modification improved CFI, TLI, and RMSEA towards their recommended thresholds of >0.95 and <0.06 respectively 
estat gof, stats(all)

*------------------------------------------------------*
* 4. Generate Phone Dependency
*------------------------------------------------------*

* Extract each latent factor score separately by explicitly naming  the latent variable being predicted — required syntax after sem
predict att_score, latent(Attitudes)
predict soc_score, latent(SocialNorms)
predict lim_score, latent(Limitations)

* Construct phone dependency as a composite score by summing the three latent factor scores, reflecting the multidimensional nature of phone dependency across attitudes, social norms, and perceived limitations
generate phone_dependency = att_score + soc_score + lim_score

*------------------------------------------------------*
* 5. Cronbach Alpha
*------------------------------------------------------*

* Assess internal consistency reliability for each factor separately using Cronbach's alpha, where a value of 0.70 or above indicates acceptable reliability, confirming that the observed variables within each factor consistently measure their respective latent construct
alpha cell3a cell3b cell3c worrya worryb post1a post1b post1e
alpha soc4a soc4b soc4c soc4d
alpha limita limitb limitc

* Examine item-total statistics for the Social Norms scale to identify whether removing any individual item would improve scale reliability above the conventional 0.70 threshold
alpha soc4a soc4b soc4c soc4d, item

* Examine item-total statistics for the Perceived Limitations scale to identify whether removing any individual item would improve scale reliability, noting that the scale contains only three items which may naturally constrain the achievable alpha value
alpha limita limitb limitc, item

* Re-estimate alpha for the Perceived Limitations scale restricting to complete cases only (n=601), consistent with the SEM estimation sample, to determine whether the lower alpha was driven by missing data in limitc rather than poor item quality
alpha limita limitb limitc if limitc != .

* Re-estimate alpha for the Social Norms scale restricting to complete cases across all four items to ensure reliability is assessed on a consistent sample free from missing data distortion
alpha soc4a soc4b soc4c soc4d if soc4a != . & soc4b != . & soc4c != . & soc4d != .

*------------------------------------------------------*
* 6. MULTIPLE REGRESSION ANALYSIS
*------------------------------------------------------*
* First, estimate the baseline regression model with perceived social  belonging (fitin), gender, and age as main effects alongside all control variables, using robust standard errors to account for potential heteroscedasticity in the data.
regress phone_dependency i.fitin i.gender i.age i.p_educ i.racethnicity i.income i.internet i.housing i.home_type i.phoneservice i.region4 i.hhsize i.metro, vce(robust)

* Given the non-significant main effect of perceived social belonging in the baseline model, a three-way interaction model was estimated to examine whether the effect of fitting in on phone dependency varies conditionally across gender and age groups, as suggested  by prior literature on adolescent phone use patterns
regress phone_dependency i.fitin##i.gender##i.age i.p_educ i.racethnicity i.income i.internet i.housing i.home_type i.phoneservice i.region4 i.hhsize i.metro, vce(robust)

*------------------------------------------------------*
* 7. DISPLAY TABLE
*------------------------------------------------------*
eststo interaction: regress phone_dependency i.fitin##i.gender##i.age i.p_educ  i.racethnicity i.income i.internet i.housing  i.home_type i.phoneservice i.region4 i.hhsize i.metro, vce(robust)
esttab interaction using "D:\IBA 29\7th Semester\Economterics\76_SabibaHossain_Econometrics\results.rtf", replace b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) r2 ar2  title("Table: Multiple Regression Results — Interaction Model") label compress

*------------------------------------------------------*
* 8. Margin Analysis
*------------------------------------------------------*
margins gender#age, dydx(fitin)

*******************************************************
*   END OF DO-FILE
*******************************************************
