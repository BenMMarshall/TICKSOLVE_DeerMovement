# TICKSOLVE_DeerMovement

Code based used to explore the movements of Roe and Fallow Deer, as well as estimate functional landscape connectivity for deer and rodent species. The code is used in the following publications that provide additional detail on it's running and motivation:

-   Marshall et al., 2026. Roe Deer show an affinity for woodland and reluctance to cross roads. [[MANUSCRIPT IN PREP]]

-   Venkatesan et al., 2026. Roe deer prefer more connected patches of woodland with higher tree density in a woodland-agricultural mosaic landscape. [[MANUSCRIPT IN PREP]]

The inter-relationship of functions is governed by the \_targets.R script, that uses the targets package to organise and operate the entire analysis pipeline. The entire pipeline should be operable usingtargets::tar*\_*make(), provided the data has been made accessible.

### Data Required

The (most) data required to re-run this analysis could not be shared alongside due to licensing restrictions. They are used in the function with "read\_" prefixes. However, data can be obtained from the following locations:

Deer movement is shared as part of this repository, but can be additionally found at Movebank.
Aberdeen Roe Deer movement data: (<https://www.movebank.org>); Movebank ID 2890266958. 
Wessex Deer movement data: (<https://www.movebank.org>); Movebank ID - - - TBC - - - -.

OS Road data: <https://osdatahub.os.uk/downloads/open/OpenRoads>

UKCEH Land cover: <https://catalogue.ceh.ac.uk/id/ab10ea4a-1788-4d25-a6df-f1aff829dfff>

UKCEH Woody linear features: <https://catalogue.ceh.ac.uk/id/d7da6cb9-104b-4dbc-b709-c1f7ba94fb16>

Homerange database: <https://datadryad.org/dataset/doi:10.5061/dryad.d2547d85x>

Human footprint: <https://figshare.com/articles/figure/An_annual_global_terrestrial_Human_Footprint_dataset_from_2000_to_2018/16571064>

GBIF Occurrence data:

-   FALLOW DEER GBIF.org (14 February 2025) GBIF Occurrence Download <https://doi.org/10.15468/dl.wz4az9>

-   RODENTIA GBIF.org (14 February 2025) GBIF Occurrence Download <https://doi.org/10.15468/dl.jhgrh7>

-   SOREX 1 GBIF.org (14 February 2025) GBIF Occurrence Download <https://doi.org/10.15468/dl.jhgrh7>

-   SOREX 2 GBIF.org (31 July 2025) GBIF Occurrence Download <https://doi.org/10.15468/dl.thgskh>

NBN Atlas occurrence downloads for *Apodemus flavicollis*, *Apodemus sylvaticus*, *Dama dama*, *Myodes glareolus*, *Sciurus carolinensis*, *Sciurus vulgaris*, *Sorex araneus*, and *Sorex minutus* via <https://nbnatlas.org>

------------------------------------------------------------------------

The research forms part of the TickSolve project (<https://ticksolve.ceh.ac.uk/>) and was funded by UK Research and Innovation.
