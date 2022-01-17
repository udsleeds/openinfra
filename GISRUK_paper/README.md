
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 1 gisruk-rmd

<!-- badges: start -->
<!-- badges: end -->

The goal of gisruk-rmd is to provide a template for GISRUK paper
submissions that enables reproducible manuscripts to be submitted with
embedded R, Python or other code, based on the [RMarkdown superset of
LaTeX](https://rmarkdown.rstudio.com/). At some point it may also work
with ‘.qmd’ [Quarto](https://quarto.org/) documents, which allows
[conversion](https://quarto.org/docs/tools/jupyter-lab.html#converting-notebooks)
to and from IPython notebooks.

It will create nicely formatted pdf documents, like this:
<https://github.com/Robinlovelace/gisruk-rmd/releases/download/0.22/example-paper.pdf>
and this [README as a PDF in GISRUK’s
style](https://github.com/Robinlovelace/gisruk-rmd/releases/download/0.22/README.pdf).

Reproduce that file and create your own reproducible geographic data
analysis paper as follows:

1.  Edit the example-paper.Rmd or README.Rmd file, e.g. by [downloading
    this repo as a .zip
    file](https://github.com/Robinlovelace/gisruk-rmd/archive/refs/heads/main.zip),
    unzipping it, opening the RStudio project and entering the
    following:

``` r
file.edit("example-paper.Rmd")
```

2.  Edit the minimal abstract and authors section in the updated
    `GISRUKPaperTemplate.tex` file, e.g. with:

``` r
file.edit("GISRUKPaperTemplate.tex")
```

When you’d like to see if it works, run the following command:

``` r
rmarkdown::render("example-paper.Rmd")
```

You can also press Ctrl+Shift+K in RStudio, which should result in
something like this:

<img src="output.png" width="100%" />

To produce this document the original LaTeX template was downloaded as
follows:

``` bash
wget http://leeds.gisruk.org/paper_templates/GISRUKPaperTemplate2015-Latex.zip
unzip GISRUK*
```

See commits to see how it was modified to work as a .Rmd template.

To show it can output in any format the below is a modified example from
the original. See Python code below, which results in Figure
<a href="#fig:plot1">1.1</a>.

``` python
import matplotlib.pyplot as plt
import geopandas
from cartopy import crs as ccrs

path = geopandas.datasets.get_path('naturalearth_lowres')
df = geopandas.read_file(path)
# Add a column we'll use later
df['gdp_pp'] = df['gdp_md_est'] / df['pop_est']
df.plot()
```

![Figure 1.1: Made with Python](README_files/figure-gfm/plot1-1.png)

# 2 Introduction to guidelines

The purpose of providing these notes is to standardise the format of the
short papers submitted to GISRUK 2022. These notes are based on author
guidelines previously produced for the GISRUK conference series which in
turn were based on other guidelines.

The pages should have margins of 2.5 cm all round. The base font should
be Times New Roman 11pt, or closest equivalent and text should be single
spaced. Each section of the paper should be numbered. Section headings
should be left-justified and given in bold type. A slightly larger font
should be used for the title of the paper and the authors (16pt and 14pt
respectively). The first line of each paragraph in each section should
**NOT** be indented.

## 2.1 Sub-sections

Sub-sections should also be numbered as shown here. The sub-section
heading should be left-justified and given in bold type (11pt).

# 3 Figures, Tables and Equations,

Equations should be centred on the page and numbered consecutively in
the right-hand margin, as below. They should be referred to in the text
as
Equation <a href="#first_equation" data-reference-type="ref" data-reference="first_equation"><span class="math display"><em>f</em><em>i</em><em>r</em><em>s</em><em>t</em><sub><em>e</em></sub><em>q</em><em>u</em><em>a</em><em>t</em><em>i</em><em>o</em><em>n</em></span></a>.

*E* = *m**c*<sup>2</sup>

Figures should be presented as an integral part of the paper and should
be referred to as
Figure <a href="#first_figure" data-reference-type="ref" data-reference="first_figure"><span class="math display"><em>f</em><em>i</em><em>r</em><em>s</em><em>t</em><sub><em>f</em></sub><em>i</em><em>g</em><em>u</em><em>r</em><em>e</em></span></a>
in the text.

<!-- Original LaTeX: -->
<!-- \begin{figure}[htbp] \begin{center}  -->
<!-- \resizebox{0.3\textwidth}{!}{  -->
<!--    \includegraphics{lancaster.png} -->
<!-- } \caption{Location of Lancaster University} \label{first_figure} \end{center} \end{figure} % -->

<img src="lancaster.png" title="Location of Lancaster University" alt="Location of Lancaster University" width="30%" style="display: block; margin: auto;" />

# 4 References and Citations

A list of references cited should be provided at the end of the paper
using the Harvard format as shown below. Citations of these within the
text should be given as follows: papers such as (Openshaw 1991) or a
paper by Rey (2009).

Books can also be cited (Graser 2014).

# 5 File format

Papers should be submitted in unrestricted **pdf** format. Authors are
requested to keep to the word limit of 1500 words.

# 6 Acknowledgements

Acknowledgement should be made of any funding bodies who have supported
the work reported in the paper, of those who have given permission for
their work to be reproduced or of individuals whose particular
assistance is due recognition. Acknowledge data providers here where
appropriate.

# 7 Biography

All contributing authors should include a biography of no more than 50
words each outlining their career stage and research interests.

# 8 References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-graser_learning_2014" class="csl-entry">

Graser, Anita. 2014. *Learning Qgis*. Packt Publishing Ltd.

</div>

<div id="ref-openshaw_view_1991" class="csl-entry">

Openshaw, Stan. 1991. “A View on the GIS Crisis in Geography, or, Using
GIS to Put Humpty-Dumpty Back Together Again.” *Environment and Planning
A* 23 (5): 621–28.

</div>

<div id="ref-rey_show_2009" class="csl-entry">

Rey, Sergio J. 2009. “Show Me the Code: Spatial Analysis and Open
Source.” *Journal of Geographical Systems* 11 (2): 191–207.

</div>

</div>
