# Social-Assistance-Analysis-Geneva

The project studies the impact of the 2007 LIASI reform in the Canton of Geneva using a DiD approach. The reform modified the Genevan social assistance system by introducing incentives and sanctions aimed at encouraging beneficiaries to reintegrate into the labor market.

The main objectives are:
- to assess whether the reform reduced the duration of social assistance,
- to analyze whether more forward-looking individuals reacted differently to these incentives (heterogeneity).

The analysis relies on two 20-year panel datasets from the Swiss Household Panel (SHP), containing demographic, economic, and behavioral information on nearly 50,000 individuals.

At this stage, the project already highlights several important challenges. In particular, unconditional parallel trends do not hold.

To address these issues, more flexible methods are being considered. In particular, Double Machine Learning (Double Lasso for average effects, Causal Random Forest with CLAN for heterogeneity) could help address selection and composition biases. Synthetic Control methods are also considered to deal with structural differences and diverging trends.

This work is still a draft. Not only does it need to be completed, but the existing code may also be revised. In particular, I may refine and streamline it (e.g. reduce redundancies), as well as improve certain aspects that require further development, such as implementing a leakage-safe NA imputation pipeline, refining the forward-looking score, or making the comments clearer and more concise.

Note: The original SHP data cannot be shared for confidentiality reasons.

