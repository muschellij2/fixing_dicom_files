
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fixing DICOM Files

<!-- badges: start -->

<!-- badges: end -->

The goal of this repository is to fix some problems with problematic
DICOM files. The end goal is likely to convert these DICOM files into a
NIfTI image using
[`dcm2niix`](https://github.com/rordenlab/dcm2niix).

| file                             | link                                                                      | Title                | Description                                                                                                                                                                                                                                                             |
| :------------------------------- | :------------------------------------------------------------------------ | :------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| study\_time\_variation/index.Rmd | [Link](https://johnmuschelli.com/fixing_dicom_files/study_time_variation) | Study Time Variation | This rundown shows how to force the study time to be the same in DICOM files that are definitely from the same series. This may happen due to issues on the scanner side. Also, we show how to put a slice thickness value into the DICOM header and resave the images. |
