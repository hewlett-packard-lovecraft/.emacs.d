;; ;; Org.el  -*- lexical-binding: t; -*-
;; org files

;; LaTeX
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("note"
		 "\\input{notes-setup-file.tex}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	       )

  (add-to-list 'org-latex-classes
	       '("minty-org-article"
		 "\\input{minted-article.tex}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	       )
  )
