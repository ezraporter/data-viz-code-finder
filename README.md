# data-viz-code-finder

A computer vision model to classify images of plots by `R` functions used to generate them 

## To train the model

1. Install python dependencies

```sh
pip install -r requirements.txt
```

2. Run [`train.ipynb`](train.ipynb)

Google Colab: https://colab.research.google.com/drive/1MxyUbv9AzxFteM834J0svV1gTEZj4Guh

## To regenerate training data

1. Clone the [R-graph-gallery](https://r-graph-gallery.com/) repo

```sh
git clone https://github.com/ezraporter/R-graph-gallery.git R-graph-gallery
```

2. Install R dependencies

```r
renv::restore()
```

3. Run [`create-graph-gallery-data.R`](create-graph-gallery-data.R)
