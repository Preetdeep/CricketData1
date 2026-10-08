# Reproduce every table, figure, number and the PDF from the raw data.
PY ?= python3
A = analysis

.PHONY: all analysis paper clean

all: analysis paper

analysis:
	cd $(A) && $(PY) build_panel.py
	cd $(A) && $(PY) descriptives.py
	cd $(A) && $(PY) replicate_original.py
	cd $(A) && $(PY) main_models.py
	cd $(A) && $(PY) robustness.py
	cd $(A) && $(PY) momentum.py
	cd $(A) && $(PY) figures.py

paper:
	cd paper && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex

clean:
	rm -rf data/derived output/tables output/figures output/numbers
	cd paper && latexmk -C
