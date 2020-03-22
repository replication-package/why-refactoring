## Replication data of "Why Developers Refactor Source Code: A Mining-based Study" paper
###	Folder structure:
   * **list-of-150-analyzed-projects.csv** — List of 150 randomly selected projects we used in our study. (For the selection process see section 2.1 Study Context.) 
   * **qualitative-analysis/** — data related to 551 pull requests. Random representative sample selected out of 2400 pull requests. (For the selection process see Section 2.3 Qualitative Analysis of Refactoring Discussions in Pull Requests (RQ2).) 
        * **pullRequests.csv** — List of refactorings detected by RMiner. 
	* **detectedRefactoringOperations.csv** — List of refactorings with PR commit Ids. 
	* **refactoringKeywords.pdf** — List of refactoring keywords. 
        * **tags.csv** — List of pull requests manually tagged by authors.  
   * **mapping-taxonomy-Silva-etal/** — Similarities and differences between motivations of our taxonomy and motivations reported in Silva et. al paper "Why We Refactor? Confessions of GitHub Contributors". 
        * **diff-our-taxonomy-and-Silva's.pdf** — Marked categories covered by Silva et al. on our taxonomy. 
        * **mappingBtwMotivations.xlsx** — Mapping of motivations from Silva et al. paper to our taxonomy.  
   * **quantitative-analysis/** — Results and raw data for RQ1.  
        * **RQ1-raw-data/ **
		     * **aggregatedResults/** — Aggregated data of metrics (see Table 1 in our paper), smells (Table 2) and factors (Table 3) for every modified file in each commit. 
			 * **refactorings/** — All refactoring operations extracted by RMiner in 150 projects on every snapshot (commit) of each system. 
			 * **distancePreviousRelease.csv** — Closeness to a previous release for every commit (see Table 3, metric 19). 
			 * **distanceNextRelease.csv** — Closeness to the next release for every commit (Table 3, metric 20). 
			 * **summaryRefactorings.csv** — Number of refactored files in every commit.
		* **RQ1-script.R** — Script for our model (see Section 2.2.3 Mixed Model Building, and Table 4). 
		* **RQ1-data-analysis-table.csv** - Merged raw data used by the R script.
