# trajectoree - a package linking move data to satellite imagery and visualizing occurence probability and homerange

A package, integrating continuous time movement modeling (ctmm) with data access through rgee. Based on move files of animal trajectories, satellite imagery from eitehr Landsat-8 or Sentinel-2 can be downloaded to local and plotted. Plotting of occurence and home range estimations based on ctmm is possible, including plots for band value probability based on occurence and home range estimations.
## rgee, ctmm and move
Animal movement data can be accessed through the [move package](https://cran.r-project.org/web/packages/move/index.html).
Instructions on how to install rgee can be found [here](https://github.com/r-spatial/rgee).
Further information on the [ctmm package](https://ctmm-initiative.github.io/ctmm/) an learning resources can be found [here](https://animove.org/elearning/).
## Usage 
## Installation 
```
devtools::install_github("clearclar/trajectoree")
```
## Workflow 

```
library(rgee)
library(move)
library(trajectoree)

ee_Initialize(drive = TRUE)
```
#### Import a move file
```
move_data <- move("move_data.csv")
```
#### Resample file size
```
traj <- intergrate_move(move_data)
```
#### Workflow part I
<img src="flowchart1.png" alt="drawing" width="400"/>

## References
## Further development
* Depending on the continuation of the rgee package, the trajectoree package may switch to Copernicus as a data source. Although this drastically reduces the variety of available data, it may reduce buggines.
* Alterations to extend the move and ctmm packages more efficiently.
* Incorporate move2.
