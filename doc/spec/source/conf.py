# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'quell-spec'
copyright = '2023, Mizunashi Mana'
author = 'Mizunashi Mana'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.githubpages',
    'sphinxcontrib.katex',
    'sphinxcontrib.bibtex',
]

bibtex_bibfiles = ['reference.bib']

templates_path = ['_templates']

source_suffix = ['.rst']

master_doc = 'index'

exclude_patterns = []



# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'

html_theme_options = {
  'github_user': 'mizunashi-mana',
  'github_repo': 'quell-experimental',
  'github_banner': True,
}

html_static_path = ['_static']
