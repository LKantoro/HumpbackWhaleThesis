Humpback Whale Vocalization Detector Using Machine Learning Models
================

### California Polytechnic State Univeristy, 2025-2026 Thesis Project

#### Author

Lucas Kantorowski

### Project Overview

This thesis develops machine learning models to detect humpback whale
song vocalizations. Data were collected of the coast of Monterey Bay,
California in September 2023. We apply Mel-frequency cepstrum
coefficients, context windows, and traditional machine learning models

### Data

- **Source**: Recorder deployed in Monterey Bay, California
- **Date**: September 2023
- **Volume**: 3 hour WAV File with manual annotations for training and
  validation

### Technical Approach

#### Time Transformations

**Time-Splitting** - split continuous time axis into 0.1 second windows

**Context Windows** - accounts for the complex nature of humpback
vocalizations - captures the evolution of vocalizations - optimal length
is based on observed vocalization duration - captures vocalization
attributes, while avoiding capturing too much noise

#### Audio Transformations

**Mel-Frequency Cepstral Coefficients (MFCCs)** - set of numbers
designed to captures human perceivable differences in sound - 12
coefficients - taken on the frequencies between 0 and 2000 Hz.

#### Machine Learning Models

- **Random Forest**: Great for recall, better recall for longer context
  windows
- **K-Nearest Neighbors (KNN)**: Great for precision, better precision
  for longer context windows

### Acknowledgments

- Cal Poly Professors
  - Maddie Schroth
  - Dr. Kelly Bodwin
- Cal Poly Marine Science Students
