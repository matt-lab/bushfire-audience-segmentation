# Australian climate change audience segments and bushfire perceptions

This repository contains the data and analysis scripts for the manuscript "Does extreme climate event exposure influence climate-related opinions? The case of the 2019--2020 Australian Black Summer bushfires" by Matthew Andreotta, Fabio Boschetti, Simon Farrell, Cecile Paris, Iain Walker, Mark J. Hurlstone.
Any questions can be sent to matthewandreotta@gmail.com.

Studies were approved by the University of Western Australia Human Research Ethics Office (RA/4/20/5104), and reciprocated by the Commonwealth Scientific and Industrial Research Organisation Human Research Ethics Committee (026/19).

Study 1 was pre-registered using the Open Science Framework (https://osf.io/e7zhx/).

## Repository Structure

### Studies

**study-1**: Audience segmentation Q sort alongside surveys of psychological characteristics (including mental models).

**study-2**: Audience segmentation Q sort alongside belief-updating tasks.

**study-3**: Audience segmentation Q sort alongside surveys of psychological characteristics and a bushfire perception scale.


### File Structure

**data**: Rawest version of anonymised data available.

**analysis**: Documentation of the main analysis.

**supplement**: Additional analyses.

**study1, study2, ..., studyX**: Stimuli and Qualtrics surveys (in .QSF file for easy import) for each study.


## Requirements

Scripts run in `R`.

### TeX Installation

All LaTeX documents are generated using R sweave with a local TeX installation. All documents were compiled using Quarto and XeLaTex.

### SVG

All .SVG files were created with Inkscape and converted to .tex files.
