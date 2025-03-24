;;; core/variables.el -*- lexical-binding: t; -*-

;;; VARIABLES
(defvar zeds/library-path "~/Documents/Library of Alexandria/"
  "Directory where my documents collection lives.")

(defvar zeds/notes-path "~/Documents/Library of Alexandria/notes/"
  "General Notes.")

(defvar zeds/roam-notes-path (concat zeds/notes-path "org-roam/zettels/")
  "Org-Roam Zettlekasten")

(defvar zeds/roam-dailies-path (concat zeds/notes-path "org-roam/dailies/")
  "Journal entries.")

(defvar zeds/org-path (concat zeds/notes-path "org/")
  "Org path.")

(provide 'core/variables)
