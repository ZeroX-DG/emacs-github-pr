;;; github-pr.el --- A tool for easier github PR reviewing process

;; Copyright (C) 2019 Nguyen Viet Hung

;; Author: Nguyen Viet Hung <viethungax@gmail.com>
;; URL: https://github.com/ZeroX-DG/emacs-github-pr
;; Version: 1.0.0

;;; Commentary:

;; Github-PR.el is a tool that helps speed up the process
;; of reviewing github PRs.

;;; Code:

(require 'request)
(require 'json)
(require 'git)
(require 'ido)

(defvar github-pr-current-repo "")

(defvar github-pr-pr-list '())

(defun github-pr-find-repo ()
  "let user pick other repo if the current directory is not a repo"
  (let ((current-dir (file-name-directory (buffer-file-name (window-buffer (minibuffer-selected-window))))))
    (if (not (git-repo? current-dir))
	(progn
	  (interactive)
	  (setq user-repo-input (read-directory-name "Github repo: "))
	  (if (not (git-repo? user-repo-input))
	      (message "%s is not a git repo" user-repo-input)
	    (setq github-pr-current-repo user-repo-input)))
      (setq github-pr-current-repo current-dir))))

(defun github-pr-fetch-all-prs ()
  (message "Fetching all pull requests")
  (let ((git-repo github-pr-current-repo))
    (dolist (remote (git-remotes))
      (github-pr-fetch-prs remote))
    (github-pr-display-pr-list)))

(defun github-pr-display-pr-list ()
  (switch-to-buffer "Github PRs")
  (erase-buffer)
  (mapcar (lambda (pr)
	    (let ((number (concat "[#" (number-to-string (assoc-recursive pr 'number)) "]"))
		  (title (assoc-recursive pr 'title))
		  (creator (concat "<" (assoc-recursive pr 'creator) ">")))
	      (insert
	       (concat
		(propertize number 'font-lock-face '(:foreground "IndianRed1"))
		" -- "
		(propertize creator 'font-lock-face '(:foreground "DodgerBlue1"))
		" "
		title
		"\n")))) github-pr-pr-list))

(defun github-pr-fetch-prs (remote)
  "Fetch all PRs from a remote and save to central list"
  (let ((api (github-pr-get-api-by-remote remote)))
    (when (string-prefix-p "http" api)
      (request api
	       :params '(("state" . "open"))
	       :headers '(("Content-Type" . "application/json"))
	       :parser 'json-read
	       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
				     (message "Got error: %S" error-thrown)))
	       :success (cl-function (lambda (&key data &allow-other-keys)
				       (github-pr-add-prs-to-list data)))))))

(defun github-pr-add-prs-to-list (prs)
  "Add pr into pr list"
  (setq github-pr-pr-list '())
  (let ((pr-index 0))
    (while (< pr-index (length prs))
      (let ((pr (aref prs pr-index)))
	(setq github-pr-pr-list (append github-pr-pr-list (list (list (cons 'title (assoc-recursive pr 'title))
								(cons 'number (assoc-recursive pr 'number))
								(cons 'creator (assoc-recursive pr 'user 'login))
								(cons 'body (assoc-recursive pr 'body)))))))
      (setq pr-index (1+ pr-index)))))

(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun github-pr-get-api-by-remote (remote)
  "Get api url of the specified remote"
  ;; api url syntax: https://api.github.com/repos/<owner>/<repo>/pulls
  (let ((git-repo github-pr-current-repo))
    (let ((remote-url (git-run "config" "--get" (concat "remote." remote ".url"))))
      (save-match-data
	(and (string-match "^https?://github.com/\\(.*?\\)/\\(.*?\\).git" remote-url)
	     (let ((owner (match-string 1 remote-url))
		   (repo (match-string 2 remote-url)))
	       (concat "https://api.github.com/repos/" owner "/" repo "/pulls")))))))

(defun github-pr-start (&optional repo)
  "Fetch all PRs in repo"
  (if (not repo)
      (github-pr-find-repo)
    (setq github-pr-current-repo repo))
  (when (not (equal github-pr-current-repo ""))
    (github-pr-fetch-all-prs)))
  
(github-pr-start)

;;; github-pr.el ends here
