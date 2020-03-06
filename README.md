# Plasticity

An R package to determine several plasticity indices, as in Valladares et al. 2006, Journal of Ecology.

At the current moment, the package is still under development, and only one of the plasticity indices is available:  RDPI (Relative Distance Plasticity Index). If you want to contribute with a function to calculate any other of the indices included in that paper (or elsewhere), please feel free to contact the author of the package or do a pull request in GitHub.

## Installation
To install the package `Plasticity`in your computer, please run the following code:

```
devtools::install_github("ameztegui/Plasticity")
```

## Dependencies
The package also requires four additional packages to be installed in your computer: `agricolae`, `psych`, `dplyr`and `ggplot2`. If you don't have them you can install them with the following code

```
install.packages("agricolae")
install.packages("psych")
install.packages("dplyr")
install.packages("ggplot2")
```

## License
The package is released under a MIT license, so you are free to use, distribute or modify it, under appropriate attribution (see [LICENSE.txt](LICENSE.txt) for details). 


## Citation

If you use `Plasticity`, please cite it as:

Ameztegui, A (2017) Plasticity: An R package to determine several plasticity indices. GitHub repository, https://github.com/ameztegui/Plasticity
See [CITATION.txt](CITATION.txt) for further details. 
