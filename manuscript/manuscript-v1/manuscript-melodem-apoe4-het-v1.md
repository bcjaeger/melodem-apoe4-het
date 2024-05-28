---
title: "MELODEM Workshop Paper (title TBD)"
bibliography: refs.bib
csl: jama.csl
always_allow_html: true
output: 
  officedown::rdocx_document:
    reference_docx: style_manuscript_times_new_roman.docx
    keep_md: true
---




Your name here, your degree,^1^ 

^1^Your department, University, City, State. `<w:pPr><w:pStyle w:val="Normal"/><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="0" w:before="0" w:line="240"/><w:ind w:left="0" w:right="0" w:firstLine="0" w:firstLineChars="0"/></w:pPr>`{=openxml}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

**Correspondence to:**\
Byron C. Jaeger, PhD\
Department of Biostatistics and Data Science\
Division of Public Health Sciences\
Wake Forest School of Medicine\
Medical Center Boulevard\
Winston-Salem, NC 27154\
336-716-6956\
[bjaeger\@wakehealth.edu](mailto:bjaeger@wakehealth.edu){.email} `<w:pPr><w:pStyle w:val="Normal"/><w:jc w:val="left"/><w:pBdr><w:bottom w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:top w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:left w:val="none" w:sz="0" w:space="0" w:color="000000"/><w:right w:val="none" w:sz="0" w:space="0" w:color="000000"/></w:pBdr><w:spacing w:after="0" w:before="0" w:line="240"/><w:ind w:left="0" w:right="0" w:firstLine="0" w:firstLineChars="0"/></w:pPr>`{=openxml}

**Word Count:** Abstract: ; Manuscript: 

\newpage




# ABSTRACT

\newpage

# INTRODUCTION


Here's how to cite one thing.[@r_language]

Here's how to cite two or more things.[@ambrosius_design_2014; @sprint_research_group_randomized_2015]

# METHODS

Chronic kidney disease was defined by estimated glomerular filtration rate <60 ml/min/1.73 meters squared based on the 2021 CKD-EPI creatinine equation.

# RESULTS

# DISCUSSION

\newpage

# REFERENCES

<div id="refs"></div>

\newpage

# ACKNOWLEDGMENTS 

# FINANCIAL DISCLOSURE 





<!-- new page not needed at the end of a block section -->

```{=openxml}
<w:p><w:pPr><w:sectPr w:officer="true"><w:pgMar w:header="720" w:bottom="1440" w:top="1440" w:right="1440" w:left="1440" w:footer="720" w:gutter="0"/><w:pgSz w:h="16838" w:w="11906" w:orient="portrait"/><w:type w:val="continuous"/><w:cols/></w:sectPr></w:pPr></w:p>
```

To make a landscape section, use the block_section code here. Note that the section will be applied to all content prior to the block reaching up to the most recent block.


```r
block_section(
  prop_section(
    page_size = page_size(orient = "landscape"),
    page_margins = page_mar(bottom = 1/8, top = 1/8, 
                            right = 1/4, left = 1/4),
    type = "continuous"
  )
)
```

```{=openxml}
<w:p><w:pPr><w:sectPr w:officer="true"><w:pgMar w:header="720" w:bottom="180" w:top="180" w:right="360" w:left="360" w:footer="720" w:gutter="720"/><w:pgSz w:h="11906" w:w="16838" w:orient="landscape"/><w:type w:val="continuous"/><w:cols/></w:sectPr></w:pPr></w:p>
```


To switch back to regular portrait section, add another portrait block. Notice how the block below acts on all content above it up to the prior block.



```r
block_section(
  prop_section(
    page_size = page_size(orient = "portrait"),
    type = "continuous",
    page_margins = margins_normal
  )
)
```

```{=openxml}
<w:p><w:pPr><w:sectPr w:officer="true"><w:pgMar w:header="720" w:bottom="1440" w:top="1440" w:right="1440" w:left="1440" w:footer="720" w:gutter="0"/><w:pgSz w:h="16838" w:w="11906" w:orient="portrait"/><w:type w:val="continuous"/><w:cols/></w:sectPr></w:pPr></w:p>
```

