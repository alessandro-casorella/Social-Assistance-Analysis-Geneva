# Social-Assistance-Analysis-Geneva

This study evaluates, through a Difference-in-Differences specification, the impact of the Loi sur l’Insertion et l’Aide Sociale Individuelle (LIASI), promulgated in 2007 in the Canton of Geneva. This law transformed the Genevan social assistance system from a contributory to an incentive-based one, introducing a system of sanctions and incentives to encourage social assistance beneficiaries toward professional and social integration.

The objective of this analysis is to understand:
1.	Whether the law significantly reduced the duration of social assistance (other outcomes are also considered, which I will not elaborate on here).
2.	Whether forward-looking individuals reacted more to the incentives (heterogeneity analysis).

Using a 20-year panel from the Swiss Household Panel (SHP)—containing over 900 variables concerning numerous demographic, psychological, financial, and social data for nearly 50,000 individuals in Switzerland—I have laid the foundation for a causal analysis that involves many challenges (in particular, unconditional parallel trends do not hold).

To resolve identification issues, a Double Machine Learning approach (Double Lasso for the global causal effect, and a Causal Random Forest for heterogeneity analysis with CLAN) is envisioned to resolve selection and composition biases. A Synthetic Control, on the other hand, would help resolve trend divergence issues arising from institutional and structural factors, as well as the economic climate.
In any case, this is a work in progress and methodological adjustments remain to be made. A conceptual report will be produced in the coming weeks.

Note on Confidentiality: For confidentiality reasons, the original SHP data will not be published.

