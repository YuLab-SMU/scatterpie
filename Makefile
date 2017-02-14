PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd check clean

rd:
	Rscript -e 'roxygen2::roxygenise(".")'


vignette:
	cd vignettes;\
	Rscript -e 'rmarkdown::render("scatterpie.Rmd")';\
	mv scatterpie.html ../docs/index.html

build:
	cd ..;\
	R CMD build $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz", args="--as-cran")'

check2: build
	cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/



windows:
	Rscript -e 'rhub::check_on_windows(".")';\
	sleep 10;

addtorepo: windows
	Rscript -e 'drat:::insert("../$(PKGNAME)_$(PKGVERS).tar.gz", "../drat/docs")';\
	Rscript -e 'drat:::insert(ypages::get_windows_binary(), "../drat/docs")';\
	cd ../drat;\
	git add .; git commit -m '$(PKGNAME)_$(PKGVERS)'; git push -u origin master
