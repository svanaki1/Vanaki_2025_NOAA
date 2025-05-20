# How Many Dollars Are in the Sea?

**Estimating sand dollar (*Echinarachnius parma*) abundance using an iteratively trained convolutional neural network and generalized additive models**

Welcome! This repository contains the data and human-in-the-loop app used in our project to estimate sand dollar abundance on the seafloor.

Our paper, *"How many dollars are in the sea?"*, accepted (minor revisions) in *Ecological Informatics*, describes an iterative process combining machine learning and human expertise to count sand dollars in underwater images. This repo shares our initial model outputs and a labeling app used to train a more accurate second model.

---

## 🔍 Project Overview

In this work, we:
- Used a convolutional neural network (CNN) to detect sand dollars in seafloor images.
- Built a Shiny app to allow human reviewers to correct or confirm the model’s predictions.
- Used these corrected labels to train a second, more accurate model — which we present in the paper.

The app in this repository represents the *human-in-the-loop* step of our workflow: users review images and validate or fix predicted bounding boxes. The result is a refined dataset that improves model performance in the next iteration.

---

## 📁 Repository Structure


