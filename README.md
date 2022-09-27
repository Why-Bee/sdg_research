# sdg_research

## Introduction

This repository contains all code relevant to the research project conducted at McMaster University by the Faculty of Engineering in collaboration with the Academic Sustainability Programs Office and McMaster University Libraries.

## Project Information:
- This project aims to plot all recent research pertaining to the Sustainable Development Goals at McMaster University. It also aims to identify possible research collaborations between different Faculties.
- The project is currently under progress
- The reference to the research paper with more information and the conclusions of this project will be posted once the paper is published. It is currently in the editing stages.

## Primary Contributors:
- Data Lead: Jeffrey Demaine (Bibliometrics and Research Impact Librarian, McMaster University)
- Code Lead: Yash Bhatia (Computer Engineering Level 2, McMaster University)
- Non-Technical Lead: Kate Whalen (Associate Director- Academic Sustainability Programs Office, McMaster University)

## File Descriptors:
- main: Base file, used to build out the other files.
- KweryKombinor_SDG: Main file, contains the code needed to query Dimensions servers, obtain records, convert to human-readable formats, and catalogue results.
- dsApi2DfRev: Expansion/revision of github user massimoaria's GitHub project dimensionsR, by remaking the dsApi2Df function and expanding its uses for this project.
- dsApi2Df_ExtendOA: Fork extension of the dsApi2Df function, but for expanding upon Open Access publication data.
- README.md: This file

## Run Instructions: 
Compile dsApi2dfRev, and run KweryKombinor_SDG in an R development environment of your choice.
