; ;; Org.el  -*- lexical-binding: t; -*-
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

(with-eval-after-load 'org
  ;; syntax highlighting with minted (requires minted in miktex)
  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (setopt org-latex-src-block-backend 'minted)

  )


;; #+OPTIONS for PDF:
;;     toc:t or nil: Toggle Table of Contents.
;;     num:t or nil: Toggle section numbering.
;;     tex:t or dvipng: Handle LaTeX fragments.
;;     prop:t: Include property drawers.
;;     ^:t or nil: Toggle super/subscripts (use ^{} for explicitly bracing

;; https://orgmode.org/manual/Export-Settings.html

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (julia . t)
;;    (python . t)
;;    (jupyter . t)
;;    ))
