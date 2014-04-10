## Scenario-based Portfolio Optimization

(c) 2013-2014 Ronald Hochreiter <ron AT hochreiter.net>

For more information please refer to [Finance with R @ finance-r.com](http://www.finance-r.com/)

### Installation

    # install package devtools from CRAN
    if(!("devtools" %in% rownames(installed.packages()))) { install.packages("devtools") }

    # install modopt.matlab and scenportopt from GitHub
    require("devtools")
    install_github("modopt.matlab", "rhochreiter")
    install_github("scenportopt", "rhochreiter")
