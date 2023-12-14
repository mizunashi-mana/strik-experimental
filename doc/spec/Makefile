# build settings

RM ?= rm -rf
FIND ?= find

LATEXMK ?= latexmk
LATEXPAND ?= latexpand
LATEXDIFF ?= latexdiff
EXTRACTBB ?= extractbb

POETRY ?= poetry

LATEX_MAKE ?= $(POETRY) run $(LATEXMK)

# source files

TARGET ?= main.pdf

MAIN_SOURCE := $(TARGET:.pdf=.tex)

ASSET_PDF_SOURCE := $(wildcard asset/*.pdf)

SUB_SOURCE := title.tex $(wildcard */*.tex)
ASSET_SOURCE := $(ASSET_PDF_SOURCE)

MAIN_SOURCE_BACKUP := $(MAIN_SOURCE:.tex=.backup.tex)
DIFF_TARGET := $(TARGET:.pdf=.diff.pdf)
MAIN_DIFF_SOURCE := $(DIFF_TARGET:.pdf=.tex)

# main tasks

.SUFFIXES: .tex
.SUFFIXES: .dvi
.SUFFIXES: .pdf
.SUFFIXES: .bib

.PHONY: all
all: $(TARGET)
	echo $(SUB_SOURCE)

$(TARGET): .poetry-installed.stamp .tlmgr-installed.stamp .extractbb.stamp $(MAIN_SOURCE) $(SUB_SOURCE)
	rm -rf $@
	$(LATEX_MAKE) $(MAIN_SOURCE)

.poetry-installed.stamp: pyproject.toml poetry.lock
	$(POETRY) install --no-root
	@touch $@

.tlmgr-installed.stamp: tlmgr_requirement.bash
	./tlmgr_requirement.bash || echo "run ./tlmgr_requirement.bash manually"
	@touch $@

.extractbb.stamp: $(ASSET_SOURCE)
	@touch $@

.backup.stamp: $(MAIN_SOURCE) $(SECTION_SOURCE)
	$(LATEXPAND) --makeatletter $(MAIN_SOURCE) > $(MAIN_SOURCE_BACKUP)
	@touch $@

.diff.stamp: .backup.stamp
	$(LATEXPAND) --makeatletter $(MAIN_SOURCE) > $(MAIN_SOURCE:.tex=.expand.tex)
	$(LATEXDIFF) -e utf8 -t CFONT $(MAIN_SOURCE_BACKUP) $(MAIN_SOURCE:.tex=.expand.tex) > $(MAIN_DIFF_SOURCE)
	$(RM) $(MAIN_SOURCE:.tex=.expand.tex)
	$(LATEX_MAKE) $(DIFF_TARGET)
	@touch $@

.PHONY: watch
watch:
	$(LATEX_MAKE) -pvc $(MAIN_SOURCE)

.PHONY: watch-without-prev
watch-without-prev:
	$(LATEX_MAKE) -pvc -view=none $(MAIN_SOURCE)

.PHONY: distclean
distclean: clean
	$(RM) $(TARGET)

.PHONY: clean
clean: lessclean
	$(FIND) . -name '*.snm' -delete
	$(FIND) . -name '*.fls' -delete
	$(FIND) . -name '*.*pk' -delete
	$(FIND) . -name '*.tfm' -delete
	$(FIND) . -name '*.tmp' -delete

.PHONY: lessclean
lessclean:
	$(LATEXMAKE) -c -bibtex
	$(FIND) . -name '*.aux' -delete
	$(FIND) . -name '*.synctex.gz*' -delete
	$(FIND) . -name '*.dvi' -delete
	$(FIND) . -name '*.nav' -delete
	$(FIND) . -name '*.vrb' -delete
	$(FIND) . -name '*.toc' -delete
