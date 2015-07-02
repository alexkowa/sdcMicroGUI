sdcMicroGUI
===========

Readme for R-Package sdcMicroGUI.

*INSTALLATION*

**install gtk+**

  If you have already installed a gtk envinronment (such as, gtksharp), it's better to uninstall it. You have to setup PATH correctly as well.

- Install R from http://cran.uib.no/bin/windows/base/, following the given instructions in the webpage.
- If having no GTK+ environment in windows, GTK+ has to be installed . Download GTK+ from (http://ftp.gnome.org/mirror/gnome.org/binaries/win32/gtk+/2.22/ http://ftp.gnome.org/mirror/gnome.org/binaries/win64/gtk+/2.22/ ), download gtk+-bundle into local file system, for example c:/gtk, and setup bin - of GTK+ in PATH environment.
- Open R console 
- install.packages(‘sdcMicro’)
- install.packages("/path/to/sdcMicroGUI_1.1.6.zip", repos=NULL) or install it over http install.packages("sdcMicroGUI", repos="http://extweb3.nsd.uib.no/users/yl/R")
- library(sdcMicroGUI)
- sdcGUI()

**install R and Rstudio**

- http://www.rstudio.com/products/RStudio/
- INSTALL GTK+ environment for building sdcMicroGUI, see Step 2 in the following installation 
- clone sdcMicroGUI from https://gitlab.nsd.uib.no/riskutility/sdcmicrogui
- open project file from RStudio
- Build sdcMicroGUI via RStudio menu-->Build-->Build and Reload.