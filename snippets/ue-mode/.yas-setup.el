(defun ue--yas-can-expand-p ()
  "Check if UE snippet can be expanded"
  (and ue-expand-snippets
       (or (derived-mode-p 'c++-mode)
	   (derived-mode-p 'c-mode))))
