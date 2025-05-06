# Felidae-Genetic-Diversity
Code used for the analysis in Meeus et al. (2025).

Genomes were downloaded using rule fastql in the "Genomescope" Snakefile. The "VCF" Snakefile was used for subsequent mapping of the reads and variant calling. Heterozygosity was then calculated using a Python script ("HeterozygosityCalculation.ipynb"). Geographic range sizes were calculated by utilising a python script ("IUCN_range_calculation.py") in the QGIS program (http://www.qgis.org). Statistical analysis was performed using an R script ("HeterozygosityFelidsDataAnalysis.R").

##Citation:
[Pending]

##Abstract:

As the world is hit by the sixth mass extinction, it becomes increasingly important to understand the factors relevant to the conservation of species, so that we may protect biodiversity to the best of our abilities. Although genetic diversity is known to reflect population demography and contribute to genetic health and adaptability, it is not explicitly used as a criterion in assessments by the International Union for the Conservation of Nature (IUCN). Additionally, studies comparing diversity estimates between species often rely on summarizing results across studies, which use different methodologies and may not be suited for direct comparison. Here we performed a family-wide assessment of genomic diversity in Felidae, covering most extant species. We tested for correlations between autosomal heterozygosity and ecological traits across (sub)species, and whether a subspecies’ genetic diversity was associated with its IUCN threat category. We found evidence for genetic diversity to be strongly positively correlated with both geographic range size and population density, but not with census size. Furthermore, although genetic diversity was not significantly correlated with IUCN status overall, threatened cat species had significantly lower levels of genetic diversity than non-threatened species. Our results confirm the association of population parameters and assessment of extinction risk with genetic diversity in one of the most iconic and threatened families of land carnivores. While mechanisms and causality behind these associations will need to be the subject of further investigation, our study adds further credence to the importance of incorporating genomic information in risk assessment and conservation efforts.



##Data access:

Additional data including shapefiles and VCF files can be found on the Open Science Framework: https://osf.io/5pkue/
