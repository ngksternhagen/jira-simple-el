;;; jira-simple.el -- Interact with JIRA REST API, simply.

;; Copyright (C) 2012 Matt DeBoard
;; Copyright (C) 2016 NGK Sternhagen

;; This work is incorporates many concepts & code from, and is heavily
;; influenced by jira.el, the work of Brian Zwahr and Dave Benjamin:
;; http://emacswiki.org/emacs/JiraMode

;; Documentation of JIRA REST API can be found at URL:
;; https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

;;; Code:
(require 'cl)
(require 'json)
(require 'url)

;; ********************************
;; JIRA Simple -- based on...
;; JIRA REST Mode - By Matt DeBoard
;; ********************************

(defgroup jira-simple nil
  "JIRA customization group."
  :group 'applications)

(defgroup jira-simple-faces nil
  "Faces for displaying JIRA information."
  :group 'jira)

(defvar jira-simple-auth-info nil
  "The auth header used to authenticate each request. Please
see URL https://developer.atlassian.com/display/JIRADEV/JIRA+REST+API+Example+-+Basic+AuthenticationConsists for more information.")

(defun load-auth-info ()
  "load Jira credentials, prompting user for input"
  (setq jira-username (read-string "enter Jira username > "))
  (setq jira-passwd (read-passwd "enter Jira passwd > ")))

(defun jira-simple-login ()
  (if (load-auth-info)
      (let ((enc (base64-encode-string
                  (concat jira-username ":" jira-passwd))))
        (setq jira-simple-auth-info (concat "Basic " enc)))
    (message "You must provide your login information.")))

(defcustom jira-simple-endpoint ""
  "The URL of the REST API endpoint for user's JIRA
 installation."
  :group 'jira-simple
  :type 'string
  :initialize 'custom-initialize-set)

;; TODO should we continue to hard-code faces in this way
;; since we don't know anything about the user's themes?
(defface jira-simple-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira-simple-faces)

(defface jira-simple-issue-info-header-face
  '((t (:bold t :inherit 'jira-simple-issue-info-face)))
  "Base face for issue headers."
  :group 'jira-simple-faces)

(defface jira-simple-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira-simple-faces)

(defface jira-simple-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira-simple-faces)

(defface jira-simple-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira-simple-faces)

(defface jira-simple-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira-simple-faces)

(defface jira-simple-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira-simple-faces)

(defface jira-simple-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira-simple-faces)

(defvar jira-simple-mode-hook nil)

(defvar jira-simple-mode-map nil)

(if jira-simple-mode-map
    nil
  (progn
    (setq jira-simple-mode-map (make-sparse-keymap))
    (define-key jira-simple-mode-map "c" 'jira-simple-create-issue)
    (define-key jira-simple-mode-map "di" 'jira-simple-delete-issue)
    (define-key jira-simple-mode-map "a" 'jira-simple-change-assignee)
    (define-key jira-simple-mode-map "gg" 'jira-simple-get-watchers)
    (define-key jira-simple-mode-map "ga" 'jira-simple-add-watcher)
    (define-key jira-simple-mode-map "gr" 'jira-simple-remove-watcher)
    (define-key jira-simple-mode-map "\S-q" 'jira-simple-mode-quit)))

(defun jira-simple-mode ()
  "A mode for working with JIRA's JSON REST API. The full
specification for the API can be found at URL
https://developer.atlassian.com/display/JIRADEV/JIRA+REST+APIs

Requires JIRA 5.0 or greater.

\\{jira-simple-mode-map}"
  (interactive)
  (jira-simple-login)
  (if (or (equal jira-simple-endpoint nil)
          (equal jira-simple-endpoint ""))
      (message "jira-simple-endpoint not set! Please set this
value in .jira-auth-info.el.")
    (progn
      (switch-to-buffer "*JIRA-SIMPLE*")
      (kill-all-local-variables)
      (setq major-mode 'jira-simple-mode)
      (setq mode-name "JIRA-SIMPLE")
      (use-local-map jira-simple-mode-map)
      (run-hooks 'jira-simple-mode-hook)
      (insert "Welcome to jira-simple-mode!")
      (message "jira rest mode loaded!"))))

(defvar jira-simple-current-issue nil
  "This holds the currently selected issue.")

(defvar jira-simple-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira-simple-types nil
  "This holds a list of issues types.")

(defvar jira-simple-statuses nil
  "This holds a list of statuses.")

(defvar jira-simple-priorities nil
  "This holds a list of priorities.")

(defvar jira-simple-user-fullnames nil
  "This holds a list of user fullnames.")

(defvar response nil)

(defun jira-simple-api-interact (method data &optional path)
  "Interact with the API using method 'method' and data 'data'.
Optional arg 'path' may be provided to specify another location further
down the URL structure to send the request."
  (if (not jira-simple-auth-info)
      (message "You must login first, 'M-x jira-simple-login'.")
    (let ((url-request-method method)
          (url-request-extra-headers
           `(("Content-Type" . "application/json")
             ("Authorization" . ,jira-simple-auth-info)))
          (url-request-data data)
          (target (concat jira-simple-endpoint path)))
      (with-current-buffer (current-buffer)
        (url-retrieve target 'my-switch-to-url-buffer `(,method))))))

(defun my-switch-to-url-buffer (status method)
  "Callback function to capture the contents of the response."
  (with-current-buffer (current-buffer)
    ;; Don't try to read the buffer if the method was DELETE,
    ;; since we won't get a response back.
    (if (not (equal method "DELETE"))
        (let ((data (buffer-substring (search-forward-regexp "^$")
                                      (point-max))))
          (setq response (json-read-from-string data))))
    (kill-buffer (current-buffer))))

(defun jira-simple-mode-quit ()
  (interactive)
  (kill-buffer "*JIRA-SIMPLE*"))

(defun id-or (s)
  "Return ':id' if 's' is a numeric string. Otherwise, return
nil. The idea here is that the JIRA REST API spec allows the 'project'
and 'issuetype' keys to be either 'id' or some other value (in the
case of 'project', the other is 'key'; for 'issuetype', 'name'). This fn
enables us to allow either type of user input."
  (if (not (equal 0 (string-to-number s)))
      "id"))

(defun jira-simple-create-issue (project summary description issuetype)
  "File a new issue with JIRA."
  (interactive (list (read-string "Project Key: ")
                     (read-string "Summary: ")
                     (read-string "Description: ")
                     (read-string "Issue Type: ")))
  (if (or (equal project "")
          (equal summary "")
          (equal description "")
          (equal issuetype ""))
      (message "Must provide all information!")
    (let ((field-hash (make-hash-table :test 'equal))
          (issue-hash (make-hash-table :test 'equal))
          (project-hash (make-hash-table :test 'equal))
          (issuetype-hash (make-hash-table :test 'equal)))
      ;; Create the JSON string that will be passed to create the ticket.
      (progn
        ;; Populate our hashes, from bottom to top. The format for these
        ;; nested hash tables follow the format outlined in the JIRA REST
        ;; API documentation.
        (puthash (or (id-or project) "key") project project-hash)
        (puthash (or (id-or issuetype) "name") issuetype issuetype-hash)
        (puthash "project" project-hash issue-hash)
        (puthash "issuetype" issuetype-hash issue-hash)
        (puthash "summary" summary issue-hash)
        (puthash "description" description issue-hash)
        (puthash "fields" issue-hash field-hash)
        ;; Return the JSON-encoded hash map.
        (jira-simple-api-interact "POST" (json-encode field-hash))
        response))))

(defun jira-simple-delete-issue (k)
  "Delete an issue with unique identifier 'k'. 'k' is either an
issueId or key."
  (interactive (list (read-string "Issue Key or ID: ")))
  (jira-simple-api-interact "DELETE" nil k))

(defun jira-simple-get-watchers (k)
  "Get all the watchers for an issue."
  (interactive (list (read-string "Issue Key or ID: ")))
  (jira-simple-api-interact "GET" nil (concat k "/watchers")))

(defun jira-simple-add-watcher (k name)
  "Add a watcher to an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "Username to Add as Watcher: ")))
  (jira-simple-api-interact "POST" (json-encode name) (concat k "/watchers")))

(defun jira-simple-remove-watcher (k name)
  "Remove a watcher from an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "Username to Remove as Watcher: ")))
  (jira-simple-api-interact "DELETE" nil (concat k "/watchers?" name)))

(defun jira-simple-change-assignee (k &optional name)
  "Change the assignee for an issue."
  (interactive (list (read-string "Issue Key or ID: ")
                     (read-string "New Assignee: ")))
  (let ((name-hash (make-hash-table :test 'equal)))
    (progn
      (puthash "name" name name-hash)
      (jira-simple-api-interact "PUT" (json-encode name-hash)
                              (concat k "/assignee")))))

(provide 'jira-simple)
;;; jira-simple.el ends here
