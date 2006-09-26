(TeX-add-style-hook "manual"
 (function
  (lambda ()
    (LaTeX-add-environments
     "RArgs")
    (LaTeX-add-labels
     "fig:pmg-gui-sample-dialog"
     "fig:pmg-gui-pmg-dataviewer"
     "fig:pmg-gui-plotnotebook-dialog"
     "fig:data-viewer-with-subset"
     "sec:numeric-summaries"
     "fig:dynamic-summaries")
    (TeX-add-symbols
     '("RListel" 1)
     '("RArg" 1)
     '("RPackage" 1)
     '("RFunc" 1)
     '("RCode" 1))
    (TeX-run-style-hooks
     "fancyhdr"
     "fancyvrb"
     "color"
     "url"
     "amsfonts"
     "amsmath"
     "relsize"
     "floatflt"
     "graphicx"
     "mathptm"
     "geometry"
     "times"
     "latex2e"
     "art12"
     "article"
     "12pt"))))

