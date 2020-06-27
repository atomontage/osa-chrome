;;; osa-chrome.el --- Google Chrome remote tab control -*- lexical-binding: t; -*-

;; Copyright (C) 2020 xristos@sdf.org
;; All rights reserved

;; Modified: 2020-05-10
;; Version: 0.5
;; Author: xristos <xristos@sdf.org>
;; URL: https://github.com/atomontage/osa-chrome
;; Package-Requires: ((emacs "25.1") (osa "1.0"))
;; Keywords: comm

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; osa-chrome.el is an Emacs Mac port interface to Google Chrome.
;;
;; The current focus is on ultra-fast tab manipulation that scales to huge
;; numbers of tabs. More functionality may be added in future releases.
;;
;; Communication takes place over Apple Events using osa.el
;;
;; Please see README.org for documentation.
;;
;;; Usage:
;;
;; M-x osa-chrome


;;; Code:

(require 'osa)
(require 'subr-x)
(require 'cl-lib)
(require 'auth-source)

(defgroup osa-chrome nil
  "Google Chrome remote tab control."
  :group 'comm)

(defface osa-chrome-tab-filter-face
  '((((class color) (background dark))  (:foreground "#aaffaa"))
    (((class color) (background light)) (:foreground "#5faf00")))
  "Face used to display current filter."
  :group 'osa-chrome)

(defface osa-chrome-tab-active-face
  '((((class color) (background dark))  (:foreground "#aaffaa"))
    (((class color) (background light)) (:foreground "#5faf00")))
  "Face used to display tabs that are active in the browser."
  :group 'osa-chrome)

(defface osa-chrome-tab-marked-face
  '((((class color) (background dark))  (:foreground "#ffaaff"))
    (((class color) (background light)) (:foreground "#d70008")))
  ""
  :group 'osa-chrome)

(defface osa-chrome-tab-marked-active-face
  '((((class color) (background dark))  (:foreground "#ffffaa"))
    (((class color) (background light)) (:foreground "#800080")))
  ""
  :group 'osa-chrome)

(defvar osa-chrome-application-name "Google Chrome"
  "Name to use when retrieving application instance reference.
Change this if you are using Google Chrome Canary.")

(defvar osa-chrome-single-instance t
  "If non-nil, get all tabs from all windows from a single Chrome instance.
This is the simplest and most common scenario requiring no extra
configuration, but in the case where multiple Chrome instances are running,
it is not possible to choose which one will be used.

If nil, get all tabs from all windows belonging to all currently running
Chrome instances. You need to enable Remote Apple Events for this, as
described in the documentation.")

(defvar osa-chrome-machine-url ""
  "Used to control multiple Chrome processes through Remote Apple Events.
Only needed when `osa-chrome-single-instance' is nil.

It can either be empty or a complete eppc://user:password@host string.
If it's empty, an auth source search query is done to retrieve values.
The auth entry should normally look like:

machine localhost port eppc login user password pass")

(defvar osa-chrome-render-function #'osa-chrome-render-tab
  "Function that renders a tab into a string for display.

The function must accept one argument, an osa-chrome-tab instance,
and return a string that must not span more than one line.")

(defvar osa-chrome-limit-function #'osa-chrome-limit-tab
  "Function that limits visible tabs based on certain criteria.

Function must accept one argument, an osa-chrome-tab instance, and
return t if the tab is included in the limit, nil otherwise.")

(defvar osa-chrome-filter-function #'osa-chrome-filter-tab
  "Function that filters visible tabs based on a user-typed regexp.

Function must accept one argument, osa-chrome-tab instance, and
return t if the tab passes the filter, nil otherwise. The current
filter can be retrieved by calling `osa-chrome-active-filter'.")

(defvar osa-chrome-show-timing t
  "Measure and display elapsed time after every operation.

This can be toggled by `osa-chrome-toggle-timing'.")

(defvar osa-chrome-default-view :title
  "Show tab titles when equal to :title, URLs otherwise.
This can be toggled by `osa-chrome-toggle-url-view'.")

(defvar osa-chrome-default-limit :all
  "Default limit.

Can be one of :all, :mark, :dup, :active or an integer specifying a PID.
This can be toggled by:

 `osa-chrome-limit-none'
 `osa-chrome-limit-marked'
 `osa-chrome-limit-dup'
 `osa-chrome-limit-active'
 `osa-chrome-limit-pid'.")

(defvar osa-chrome-auto-retrieve nil
  "If non-nil, retrieve all tabs after every operation.

Note that every retrieval recreates tab state in Emacs and thus discards
what was previously there (except filter and limit).
Currently this only applies to `osa-chrome-visit-tab'.

Delete operations always trigger a tab retrieval post-operation.")

(defvar osa-chrome-script-directory
  (and load-file-name
       (concat (file-name-directory load-file-name)
               (file-name-as-directory "scripts")))
  "Directory that contains JXA Chrome control scripts.
Set this manually if auto-detection fails.")

(cl-defstruct (osa-chrome-tab
               (:constructor osa-chrome-tab-create)
               (:copier nil))
  (pid       nil :read-only t)    ; PID of Chrome instance that contains this tab
  (id        nil :read-only t)    ; Unique id of tab in this Chrome instance
  (window-id nil :read-only t)    ; Unique id of window that contains this tab
  (url       nil :read-only t)    ; URL of tab
  (title     nil :read-only t)    ; Title of tab
  is-active                       ; Is tab selected in OSA Chrome buffer?
  is-marked                       ; Is tab marked in Emacs?
  is-duplicate                    ; Is tab a duplicate of another? (based on URL)
  line)                           ; Tab line number in OSA Chrome buffer


;;;
;;; Internal API
;;;


(defvar-local osa-chrome--process-index nil
  "Hash table that contains indexed tabs (osa-chrome-tab instances).

Keys are integers (PIDs).
Values are conses of form:

  ((tab-count . window-count) tab-list)

Tabs in tab-list are ordered as they appear in Chrome:
Older tabs before newer tabs.")

(defvar-local osa-chrome--cached-tabs nil
  "Hash table that contains indexed tabs (osa-chrome-tab instances).

Keys are conses of form: (pid . tab-id)
Values are osa-chrome-tab instances.")

(defvar-local osa-chrome--header-update nil)
(defvar-local osa-chrome--header-cache nil)

(defun osa-chrome--reindex-tabs (tabs)
  "Index TABS into `osa-chrome--process-index' and `osa-chrome--cached-tabs'.
TABS must be an alist as returned from `osa-chrome-get-tabs'."
  (clrhash osa-chrome--process-index)
  (clrhash osa-chrome--cached-tabs)
  (cl-loop
   for (pid-str . data) in (cdr tabs)
   for pid                = (string-to-number pid-str)
   for windows            = (aref data 0)
   for active-tab-windows = (aref data 1)
   for tab-id-windows     = (aref (aref data 2) 0)
   for url-windows        = (aref (aref data 2) 1)
   for title-windows      = (aref (aref data 2) 2)
   for process-tabs       = nil
   for tab-count          = 0
   for window-count       = 0
   with seen-urls         = (make-hash-table :test 'equal)
   do
   (cl-loop
    for window-id     across windows
    for active-tab-id across active-tab-windows
    for tab-ids       across tab-id-windows
    for urls          across url-windows
    for titles        across title-windows do
    (cl-incf window-count)
    (cl-loop
     for tab-id across tab-ids
     for url    across urls
     for title  across titles do
     (let ((tab (osa-chrome-tab-create :pid pid :id tab-id :url url
                                       :title title
                                       :window-id window-id
                                       :is-active (= tab-id active-tab-id))))
       (push tab process-tabs)
       (cl-incf tab-count)
       (if (gethash url seen-urls)
           (setf (osa-chrome-tab-is-duplicate tab) t)
         (puthash url t seen-urls))
       (puthash (cons pid tab-id) tab osa-chrome--cached-tabs))))
   ;; A hash table indexed by pid containing all tabs
   (setf (gethash pid osa-chrome--process-index)
         (cons (cons tab-count window-count)
               (nreverse process-tabs)))))

(defvar-local osa-chrome--visible-tabs nil)
(defvar-local osa-chrome--marked-tabs 0)

(defun osa-chrome--init-caches ()
  (setq osa-chrome--process-index (make-hash-table)
        osa-chrome--visible-tabs  (make-hash-table)
        osa-chrome--cached-tabs   (make-hash-table :test 'equal)))

(defvar-local osa-chrome--cached-auth nil)

(defun osa-chrome--machine-url ()
  (cond ((and osa-chrome-machine-url
              (string-prefix-p "eppc://" osa-chrome-machine-url))
         osa-chrome-machine-url)
        (osa-chrome--cached-auth)
        (t
         (or
          (when-let ((auth (car (auth-source-search :port "eppc"
                                                    :require '(:port))))
                     (host   (plist-get auth :host))
                     (user   (plist-get auth :user))
                     (secret (plist-get auth :secret)))
            (when (functionp secret) (setq secret (funcall secret)))
            (setq osa-chrome--cached-auth
                  (format "eppc://%s:%s@%s" user secret host)))
          (error "Missing URL, see osa-chrome-machine-url")))))

(defvar-local osa-chrome--start-time nil)
(defvar-local osa-chrome--elapsed-time nil)

(defun osa-chrome--start-timer ()
  (unless osa-chrome--start-time
    (setq osa-chrome--start-time (current-time))))

(defun osa-chrome--stop-timer ()
  (when osa-chrome--start-time
    (setq osa-chrome--elapsed-time
          (float-time (time-subtract
                       (current-time)
                       osa-chrome--start-time))
          osa-chrome--start-time nil)))

(cl-defmacro osa-chrome--with-timing (&body body)
  (declare (indent defun))
  `(unwind-protect
       (progn
         (osa-chrome--start-timer)
         ,@body)
     (osa-chrome--stop-timer)
     (setq osa-chrome--header-update t)))

(defun osa-chrome--message (format-string &rest args)
  (let ((message-truncate-lines t))
    (message "osa-chrome: %s" (apply #'format format-string args))))

(cl-defmacro osa-chrome--check-error ((res) call &body body)
  (declare (indent defun))
  (let ((err      (cl-gensym))
        (err-data (cl-gensym)))
    `(let* ((osa-strict-unpacking t)
            (,res ,call))
       (if-let ((,err       (cdr (assoc "error" ,res))))
           (let ((,err-data (cdr (assoc "error-data" ,res))))
            (osa-chrome--message "%s%s" ,err
                                 (if ,err-data
                                     (format " [%s]" ,err-data)
                                   "")))
         ,@body))))


;;;
;;; Filtering
;;;


(defvar-local osa-chrome--active-filter nil)

(defun osa-chrome--find-script (name)
  (unless osa-chrome-script-directory
    (error "Script directory is unset (osa-chrome-script-directory)"))
  (concat osa-chrome-script-directory name))

(defvar-local osa-chrome--last-tab nil)

(defsubst osa-chrome--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defsubst osa-chrome--render-tab (tab &optional skip-goto)
  (unless skip-goto (osa-chrome-goto-tab tab))
  (delete-region (line-beginning-position) (line-end-position))
  (insert (funcall osa-chrome-render-function tab)))

(defsubst osa-chrome--limit-tab (tab)
  (funcall osa-chrome-limit-function tab))

(defsubst osa-chrome--filter-tab (tab)
  (funcall osa-chrome-filter-function tab))

(defun osa-chrome--filter-tabs ()
  (when-let ((current-tab (osa-chrome-current-tab)))
    (setq osa-chrome--last-tab current-tab))
  (when (> (buffer-size) 0)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (clrhash osa-chrome--visible-tabs))
  (osa-chrome--with-timing
    (cl-loop
     with active-tabs
     for pid being the hash-keys of osa-chrome--process-index
     with line        = 1
     for process-tabs = (gethash pid osa-chrome--process-index)
     for tabs         = (cdr process-tabs) do
     (cl-loop
      for tab in tabs do
      ;; Matching
      (if (and (osa-chrome--limit-tab tab)
               (osa-chrome--filter-tab tab))
          ;; Matches filter+limit
          (let ((inhibit-read-only t))
            (setf (osa-chrome-tab-line tab) line
                  (gethash line osa-chrome--visible-tabs) tab
                  line (1+ line))
            (osa-chrome--render-tab tab t)
            (insert "\n")
            (when (osa-chrome-tab-is-active tab) (push tab active-tabs)))
        ;; Doesn't match filter/limit
        (setf (osa-chrome-tab-line tab) nil)))
     finally do
     ;; After all tabs have been filtered, determine where to set point
     (when (> line 1)
       ;; Previously selected tab if it's still visible and not deleted
       (if-let ((last-tab osa-chrome--last-tab)
                (last-tab (gethash (cons (osa-chrome-tab-pid last-tab)
                                         (osa-chrome-tab-id  last-tab))
                                   osa-chrome--cached-tabs))
                (last-line (osa-chrome-tab-line last-tab)))
           (osa-chrome-goto-tab last-tab)
         ;; First active tab if there is one visible
         (if-let ((tab (car active-tabs)))
             (osa-chrome-goto-tab tab)
           ;; Last tab
           (goto-char (point-max))
           (forward-line -1))))))
  (force-mode-line-update))


;;;
;;; Header
;;;


(defvar-local osa-chrome--header-function #'osa-chrome--header
  "Function that returns a string for tab view header line.")

(defun osa-chrome--header-1 ()
  "Generate string for tab view header line."
  (let* ((total-tabs   (hash-table-count osa-chrome--cached-tabs))
         (visible-tabs (hash-table-count osa-chrome--visible-tabs))
         (total-procs  (hash-table-count osa-chrome--process-index))
         (visible-procs
          (if (= visible-tabs total-tabs) total-procs
            (cl-loop with result and count = 0
                     for tab in (hash-table-values osa-chrome--visible-tabs)
                     for pid = (osa-chrome-tab-pid tab)
                     unless (memq pid result)
                     do (push pid result) (cl-incf count)
                     when (= count total-procs) return count
                     finally return count))))
    (cl-flet ((align (width str)
                     (let ((spec (format "%%%ds" width)))
                       (format spec str)))
              (size10 (x) (if (= x 0) 1 (1+ (floor (log x 10))))))
      (concat
       (align (+ 1 (* 2 (size10 total-tabs)))
              (propertize (format "%s/%s" visible-tabs total-tabs)
                          'help-echo "Visible / total tabs"))
       " "
       (align (size10 total-tabs)
              (propertize (int-to-string osa-chrome--marked-tabs)
                          'help-echo "Marked tabs"
                          'face 'osa-chrome-tab-marked-face))
       " "
       (align (1+ (* 2 (size10 total-procs)))
              (propertize (format "(%s/%s)" visible-procs total-procs)
                          'help-echo "Visible / total processes"))
       " "
       (format "By: %5s" (if (eq osa-chrome-default-view :title) "title" "URL"))
       " "
       (format "Limit: %6s"
               (pcase osa-chrome-default-limit
                 (:all    "all")
                 (:mark   "mark")
                 (:dup    "dup")
                 (:active "active")
                 (other other)))
       " "
       (when osa-chrome-show-timing
         (propertize (format " %.4fs " osa-chrome--elapsed-time)
                     'help-echo "Elapsed time for last operation"))
       (when-let ((filter (osa-chrome-active-filter)))
         (format "Filter: %s"
                 (propertize filter
                             'help-echo "Search filter"
                             'face 'osa-chrome-tab-filter-face)))))))

(defun osa-chrome--header ()
  "Return string for tab view header line.
If a previously cached string is still valid, it is returned.
Otherwise, a new string is generated and returned by calling
`osa-chrome--header-1'."
  (if (and (null osa-chrome--header-update)
           (eql (car osa-chrome--header-cache) (buffer-modified-tick)))
      (cdr osa-chrome--header-cache)
    (let ((header (osa-chrome--header-1)))
      (prog1 header
        (setq osa-chrome--header-cache (cons (buffer-modified-tick) header)
              osa-chrome--header-update nil)))))


;;;
;;; Major mode
;;;


(defvar osa-chrome-mode-map
  ;; Override self-insert-command with fallback to global-map
  (let* ((map        (make-keymap))
         (prefix-map (make-sparse-keymap))
         (char-table (cl-second map)))
    ;; Rebind keys that were bound to self-insert-command
    (map-keymap
     (lambda (event def)
       (when (eq def 'self-insert-command)
         (set-char-table-range
          char-table event 'osa-chrome--self-insert-command)))
     global-map)
    ;; Standard bindings
    (define-key map (kbd "DEL")      'osa-chrome--self-insert-command)
    (define-key map (kbd "C-l")      'osa-chrome-retrieve-tabs)
    (define-key map (kbd "C-k")      'osa-chrome-reset-filter)
    (define-key map (kbd "C-t")      'osa-chrome-toggle-timing)
    (define-key map (kbd "C-w")      'osa-chrome-copy-url)
    (define-key map (kbd "C-v")      'osa-chrome-view-source)
    (define-key map (kbd "C-d")      'osa-chrome-delete-tab)
    (define-key map (kbd "RET")      'osa-chrome-visit-tab)
    (define-key map (kbd "M-m")      'osa-chrome-mark-tab)
    (define-key map (kbd "M-d")      'osa-chrome-delete-marked-tabs)
    (define-key map (kbd "M-M")      'osa-chrome-mark-all-tabs)
    (define-key map (kbd "M-u")      'osa-chrome-unmark-tab)
    (define-key map (kbd "M-U")      'osa-chrome-unmark-all-tabs)
    (define-key map [(tab)]          'osa-chrome-goto-active)
    (define-key map (kbd "C-<up>")   'previous-line)
    (define-key map (kbd "C-<down>") 'next-line)
    (define-key map (kbd "\\")       'osa-chrome-toggle-url-view)
    ;; Prefix bindings
    (define-key map (kbd "/")         prefix-map)
    (define-key prefix-map (kbd "m") 'osa-chrome-limit-marked)
    (define-key prefix-map (kbd "'") 'osa-chrome-limit-pid)
    (define-key prefix-map (kbd "d") 'osa-chrome-limit-dup)
    (define-key prefix-map (kbd "a") 'osa-chrome-limit-active)
    (define-key prefix-map (kbd "/") 'osa-chrome-limit-none)
    map)
  "Keymap for `osa-chrome-mode'.")

(defun osa-chrome--self-insert-command ()
  "Like `self-insert-command' but used to create active filter."
  (interactive)
  (let ((event last-input-event)
        updated)
    (cond ((characterp event)
           (if (and (= 127 event)
                    (not (display-graphic-p)))
               (pop osa-chrome--active-filter)
               (push event osa-chrome--active-filter))
           (setq updated t))
          ((eql event 'backspace)
           (pop osa-chrome--active-filter)
           (setq updated t))
          (t (osa-chrome--message "Unknown event %s" event)))
    (when updated (osa-chrome--filter-tabs))))

(defun osa-chrome-mode ()
  "Major mode for manipulating Google Chrome tabs.
\\<osa-chrome-mode-map>
Tabs are retrieved from Chrome and displayed in an Emacs buffer, one tab
per line. Display takes place in ordered fashion, with tabs appearing as
they are in Chrome, older ones before newer ones.

Tabs can be further filtered in realtime by a user-specified regular
expression and limited by certain criteria described below. This mode tries
to remember point so that it keeps its associated tab selected across
filtering/limiting operations, assuming the tab is visible.

To minimize the feedback loop, this mode does not use the minibuffer
for input (e.g. when typing a filter regular expression).
You can start typing immediately and the filter updates, visible on
the header line.

Other than regular keys being bound to `osa-chrome--self-insert-command',
the following commands are available:

Type \\[osa-chrome-visit-tab] to switch to tab at point in Chrome. This brings Chrome
into focus and raises the window that contains the tab. With a prefix
argument, switch to the tab in Chrome but keep input focus in Emacs and
do not raise Chrome window.

Type \\[osa-chrome-retrieve-tabs] to retrieve tabs from Chrome.
This wipes and recreates all tab state in Emacs but keeps the current
filter and limit.

Type \\[osa-chrome-goto-active] to move point to the next active tab.
By repeatedly typing \\[osa-chrome-goto-active], you can cycle through all active tabs.

Type \\[osa-chrome-reset-filter] to kill the current filter.

Type \\[osa-chrome-toggle-url-view] to toggle tabs being shown as titles or URLs.

Type \\[osa-chrome-toggle-timing] to toggle timing information on the header line.

Type \\[osa-chrome-copy-url] to copy URL belonging to tab at point.

Type \\[osa-chrome-view-source] to view HTML source of tab at point in side buffer.
You need to enable `Allow JavaScript from Apple Events'
under View->Developer in Chrome to use this command.

Limiting tabs:

Type \\[osa-chrome-limit-marked] to only show marked tabs.

Type \\[osa-chrome-limit-dup] to only show duplicate tabs (by URL).

Type \\[osa-chrome-limit-active] to only show active tabs (selected in Chrome).

Type \\[osa-chrome-limit-pid] to only show tabs belonging to a specific Chrome
instance by PID. Since you can't directly input the PID,
by repeatedly typing \\[osa-chrome-limit-pid] you can cycle through all PIDs.

Type \\[osa-chrome-limit-none] to remove the current limit and show all tabs.

Marking and deleting:

Type \\[osa-chrome-mark-tab] to mark tab at point.

Type \\[osa-chrome-unmark-tab] to unmark tab at point.

Type \\[osa-chrome-mark-all-tabs] to mark all tabs currently visible in Emacs.
If there is a region, only mark tabs in region.

Type \\[osa-chrome-unmark-all-tabs] to unmark all tabs currently visible in Emacs.
If there is a region, only unmark tabs in region.

Type \\[osa-chrome-delete-marked-tabs] to delete all marked tabs.

Type \\[osa-chrome-delete-tab] to delete tab at point.

Deleting a single or all marked tabs always triggers a full
tab retrieval from Chrome.

\\{osa-chrome-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map osa-chrome-mode-map)
  (font-lock-mode -1)
  (make-local-variable 'font-lock-function)
  (buffer-disable-undo)
  (setq major-mode 'osa-chrome-mode
        mode-name "OSA Chrome"
        truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall osa-chrome--header-function))
        font-lock-function (lambda (_) nil))
  (setq-local revert-buffer-function #'osa-chrome-revert-buffer)
  (osa-chrome--init-caches)
  (hl-line-mode 1)
  (osa-chrome--with-timing
    (osa-chrome--reindex-tabs (osa-chrome-get-tabs))
    (osa-chrome--filter-tabs))
  (run-mode-hooks 'osa-chrome-mode-hook))


;;;
;;; API
;;;


(cl-defun osa-chrome-revert-buffer (_ noconfirm)
  (unless noconfirm
    (unless (y-or-n-p "Revert buffer by retrieving tabs from Chrome? ")
      (cl-return-from 'osa-chrome-revert-buffer)))
  (osa-chrome-retrieve-tabs))

(defun osa-chrome-active-filter ()
  "Return currently active filter string or nil."
  (when osa-chrome--active-filter
    (apply #'string (reverse osa-chrome--active-filter))))

(defun osa-chrome-render-tab (tab)
  "Return string representation of TAB.
String is used as is to display TAB in *chrome-tabs* buffer.
It must not span more than one line but it may contain text properties."
  (let ((url       (osa-chrome-tab-url tab))
        (title     (osa-chrome-tab-title tab))
        (is-active (osa-chrome-tab-is-active tab))
        (is-marked (osa-chrome-tab-is-marked tab)))
    (let ((str (concat
                (if is-marked "* " "  ")
                (if (eq osa-chrome-default-view :title)
                    (if (string-equal "" title) url title)
                  url))))
      (cond ((and is-marked is-active)
             (setq str (propertize str 'face 'osa-chrome-tab-marked-active-face)))
            (is-marked
             (setq str (propertize str 'face 'osa-chrome-tab-marked-face)))
            (is-active
             (setq str (propertize str 'face 'osa-chrome-tab-active-face))))
      str)))

(defun osa-chrome-limit-tab (tab)
  "Limits TAB by pid, duplicate, marked or active status.
Limiting operation depends on `osa-chrome-default-limit'."
  (cl-case osa-chrome-default-limit
    (:all    t)
    (:mark   (osa-chrome-tab-is-marked tab))
    (:dup    (osa-chrome-tab-is-duplicate tab))
    (:active (osa-chrome-tab-is-active tab))
    (t (equal osa-chrome-default-limit (osa-chrome-tab-pid tab)))))

(defun osa-chrome-filter-tab (tab)
  "Filters TAB using a case-insensitive match on either URL or title."
  (let ((filter (osa-chrome-active-filter)))
    (or (null filter)
        (let ((case-fold-search t)
              (url   (osa-chrome-tab-url tab))
              (title (osa-chrome-tab-title tab)))
          (or (string-match (replace-regexp-in-string " " ".*" filter) url)
              (string-match (replace-regexp-in-string " " ".*" filter) title))))))

(defun osa-chrome-current-tab ()
  "Return tab at point or nil."
  (gethash (line-number-at-pos (point))
           osa-chrome--visible-tabs))

(defun osa-chrome-goto-tab (tab)
  "Move point to TAB if it is visible."
  (when-let ((line (osa-chrome-tab-line tab)))
    (osa-chrome--goto-line line)))

(defun osa-chrome-get-tabs ()
  "Return a record (alist) containing tab information.

The alist contains (pid . [window-ids, active-tab-ids, tabs]) pairs,
where:

window-ids and active-tab-ids are vectors of same length, length
being equal to the number of windows.

tabs is a vector of 3 elements: [tab-ids, urls, titles] where
tab-ids, urls and titles are vectors of same length as window-ids
and active-tab-ids."
  (osa-chrome--check-error (ret)
    (osa-eval-file
     (osa-chrome--find-script
      (if osa-chrome-single-instance "get-tabs-single.js" "get-tabs-multi.js"))
     :lang "JavaScript"
     :call (if osa-chrome-single-instance "get_tabs_single" "get_tabs_multi")
     :args (list osa-chrome-application-name
                 (unless osa-chrome-single-instance (osa-chrome--machine-url))))
    ret))


;;;
;;; Interactive
;;;


(defun osa-chrome-toggle-timing ()
  "Toggle timing information on the header line."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (let ((timingp osa-chrome-show-timing))
    (setq-local osa-chrome-show-timing (if timingp nil t))
    (setq osa-chrome--header-update t))
  (force-mode-line-update))

(defun osa-chrome-toggle-url-view ()
  "Toggle tabs being displayed as titles or URLs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (let ((view osa-chrome-default-view))
    (setq-local osa-chrome-default-view
                (if (eq view :title) :url :title)))
  (osa-chrome--filter-tabs))

(defun osa-chrome-limit-marked ()
  "Only show marked tabs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (setq-local osa-chrome-default-limit :mark)
  (osa-chrome--filter-tabs))

(defun osa-chrome-limit-pid ()
  "Only show tabs belonging to a specific Chrome instance, by PID.
Since you can't directly input the PID, by repeatedly invoking this command
you can cycle through all PIDs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (let* ((limit osa-chrome-default-limit)
         (pids (cl-loop for pid in (hash-table-keys osa-chrome--process-index)
                        vconcat (list pid)))
         (npids (length pids)))
    (setq-local osa-chrome-default-limit
                (aref pids (if-let ((pos (cl-position limit pids)))
                               (mod (1+ pos) npids)
                             0)))
    (osa-chrome--filter-tabs)))

(defun osa-chrome-limit-dup ()
  "Only show duplicate tabs (by URL)."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (setq-local osa-chrome-default-limit :dup)
  (osa-chrome--filter-tabs))

(defun osa-chrome-limit-active ()
  "Only show active tabs (selected in Chrome)."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (setq-local osa-chrome-default-limit :active)
  (osa-chrome--filter-tabs))

(defun osa-chrome-limit-none ()
  "Remove current limit and show all tabs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (unless (eq :all osa-chrome-default-limit)
    (setq-local osa-chrome-default-limit :all)
    (osa-chrome--filter-tabs)))

(defun osa-chrome-copy-url ()
  "Copy URL belonging to tab at point."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when-let ((tab (osa-chrome-current-tab)))
    (let ((url (osa-chrome-tab-url tab)))
      (kill-new url)
      (message "Copied: %s" url))))

(defun osa-chrome-retrieve-tabs ()
  "Retrieve and filter all Chrome tabs.
This wipes and recreates all tab state in Emacs but keeps the current filter
and limit."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (osa-chrome--with-timing
    (setq osa-chrome--marked-tabs 0)
    (osa-chrome--reindex-tabs (osa-chrome-get-tabs))
    (osa-chrome--filter-tabs)))

(defun osa-chrome-reset-filter ()
  "Kill current tab filter."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (setq osa-chrome--active-filter nil)
  (osa-chrome--filter-tabs))

(defsubst osa-chrome--delete-single (tab-ids)
  (osa-eval-file (osa-chrome--find-script "delete-tabs-single.js")
                 :lang "JavaScript"
                 :call "delete_tabs_single"
                 :args (list osa-chrome-application-name tab-ids)))

(defsubst osa-chrome--delete-multi (pid tab-ids)
  (osa-eval-file (osa-chrome--find-script "delete-tabs-multi.js")
                 :lang "JavaScript"
                 :call "delete_tabs_multi"
                 :args (list (osa-chrome--machine-url) pid tab-ids)))

(defun osa-chrome-delete-tab ()
  "Delete tab at point."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when-let ((tab (osa-chrome-current-tab)))
    (let* ((pid       (osa-chrome-tab-pid tab))
           (tab-id    (osa-chrome-tab-id tab))
           (window-id (osa-chrome-tab-window-id tab))
           (tab-ids   (list :reco (cons window-id (vector tab-id)))))
      (osa-chrome--with-timing
       (osa-chrome--check-error (ret)
         (if osa-chrome-single-instance
             (osa-chrome--delete-single tab-ids)
           (osa-chrome--delete-multi pid tab-ids))
         (forward-line)
         (osa-chrome-retrieve-tabs)
         (message "Deleted %d tabs" (cdr (assoc "count" ret))))))))

(defun osa-chrome-delete-marked-tabs ()
  "Delete all marked tabs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when (> osa-chrome--marked-tabs 0)
    (osa-chrome--with-timing
      (cl-loop
       for pid being the hash-keys of osa-chrome--process-index
       for process-tabs      = (cdr (gethash pid osa-chrome--process-index))
       for grouped-tabs      = nil
       with grouped-tab-list = nil do
       (cl-loop for tab in process-tabs
                when (osa-chrome-tab-is-marked tab) do
                (let ((window-id (osa-chrome-tab-window-id tab)))
                  (push (osa-chrome-tab-id tab)
                        (alist-get window-id grouped-tabs)))
                finally do
                (when grouped-tabs
                  (setq grouped-tabs
                        (cons :reco
                              (mapcar (lambda (c) (cons (car c) (vconcat (cdr c))))
                                      grouped-tabs)))
                  (push (cons pid grouped-tabs) grouped-tab-list)))
       finally do
       (cl-loop
        with deleted-tab-count = 0
        for pid-count from 0
        for (pid . grouped-tabs) in grouped-tab-list do
        (osa-chrome--check-error (ret)
          (if osa-chrome-single-instance
              (osa-chrome--delete-single grouped-tabs)
            (osa-chrome--delete-multi pid grouped-tabs))
          (cl-incf deleted-tab-count (cdr (assoc "count" ret))))
        finally do
        (message "Deleted %d tabs from %d processes" deleted-tab-count pid-count)
        (osa-chrome-retrieve-tabs))))))

(defun osa-chrome-mark-tab (&optional tab)
  "Mark TAB at point."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (let ((move-forward (if tab nil t)))
    (when-let ((tab (or tab (osa-chrome-current-tab))))
      (unless (osa-chrome-tab-is-marked tab)
        (setf (osa-chrome-tab-is-marked tab) t)
        (cl-incf osa-chrome--marked-tabs)
        (let ((inhibit-read-only t)
              (point (point)))
          (unwind-protect
              (osa-chrome--render-tab tab)
            (goto-char point))))
      (when move-forward (forward-line)))))

(defun osa-chrome-unmark-tab (&optional tab)
  "Unmark TAB at point."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (let ((move-forward (if tab nil t)))
    (when-let ((tab (or tab (osa-chrome-current-tab))))
      (when (osa-chrome-tab-is-marked tab)
        (setf (osa-chrome-tab-is-marked tab) nil)
        (cl-decf osa-chrome--marked-tabs)
        (let ((inhibit-read-only t)
              (point (point)))
          (unwind-protect
              (osa-chrome--render-tab tab)
            (goto-char point))))
      (when move-forward (forward-line)))))

(defsubst osa-chrome-do-visible-tabs (function)
  "Call FUNCTION once for each visible tab, passing tab as an argument."
  (mapc function
        (if (region-active-p)
            (save-excursion
              (let ((begin (region-beginning))
                    (end   (region-end)))
                (goto-char begin)
                (cl-loop for pos = (point) while (< pos end)
                         collect (osa-chrome-current-tab)
                         do (forward-line))))
          (hash-table-values osa-chrome--visible-tabs))))

(defun osa-chrome-mark-all-tabs ()
  "Mark all tabs currently visible in Emacs.
If there is a region, only mark tabs in region."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (osa-chrome-do-visible-tabs #'osa-chrome-mark-tab))

(defun osa-chrome-unmark-all-tabs ()
  "Unmark all tabs currently visible in Emacs.
If there is a region, only unmark tabs in region."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (osa-chrome-do-visible-tabs #'osa-chrome-unmark-tab))


(defsubst osa-chrome--view-source-single (window-id tab-id)
  (osa-eval-file (osa-chrome--find-script "view-source-single.js")
                 :lang "JavaScript"
                 :call "view_source_single"
                 :args (list osa-chrome-application-name
                             window-id tab-id)))

(defsubst osa-chrome--view-source-multi (pid window-id tab-id)
  (osa-eval-file (osa-chrome--find-script "view-source-multi.js")
                 :lang "JavaScript"
                 :call "view_source_multi"
                 :args (list (osa-chrome--machine-url)
                             pid window-id tab-id)))

(defun osa-chrome-view-source ()
  "View HTML source of tab at point in side buffer.
You need to enable `Allow JavaScript from Apple Events' under View->Developer
in Chrome to use this command."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when-let ((tab (osa-chrome-current-tab)))
    (osa-chrome--with-timing
      (let* ((osa-strict-unpacking t)
             (window-id (osa-chrome-tab-window-id tab))
             (tab-id    (osa-chrome-tab-id tab))
             (pid       (osa-chrome-tab-pid tab)))
        (osa-chrome--check-error (ret)
          (if osa-chrome-single-instance
              (osa-chrome--view-source-single window-id tab-id)
            (osa-chrome--view-source-multi pid window-id tab-id))
          (let ((buf (get-buffer-create "*chrome-source*")))
            (with-current-buffer buf
              (erase-buffer)
              (insert (cdr (assoc "html" ret)))
              (goto-char (point-min)))
            (display-buffer buf)))))
    (force-mode-line-update)))

(defsubst osa-chrome--visit-tab-single (window-id tab-id noraise)
  (osa-eval-file (osa-chrome--find-script "set-tab-single.js")
                 :lang "JavaScript"
                 :call "set_tab_single"
                 :args (list osa-chrome-application-name window-id
                             tab-id (not noraise))))

(defsubst osa-chrome--visit-tab-multi (pid window-id tab-id noraise)
  (osa-eval-file (osa-chrome--find-script "set-tab-multi.js")
                 :lang "JavaScript"
                 :call "set_tab_multi"
                 :args (list (osa-chrome--machine-url) pid window-id
                             tab-id (not noraise))))

(defun osa-chrome-visit-tab (&optional noraise)
  "Switch to tab at point in Chrome.
This brings Chrome into focus and raises the window that contains the tab.
With a prefix argument NORAISE, switch to the tab in Chrome but keep input
focus in Emacs and do not raise Chrome window."
  (interactive "P")
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when-let ((tab (osa-chrome-current-tab)))
    (osa-chrome--with-timing
      (let* ((window-id (osa-chrome-tab-window-id tab))
             (tab-id    (osa-chrome-tab-id tab))
             (pid       (osa-chrome-tab-pid tab)))
        (osa-chrome--check-error (ret)
          (if osa-chrome-single-instance
              (osa-chrome--visit-tab-single window-id tab-id noraise)
            (osa-chrome--visit-tab-multi pid window-id tab-id noraise))
          (when-let ((warn (cdr (assoc "warn" ret))))
            (osa-chrome--message "%s" warn))
          (if osa-chrome-auto-retrieve (osa-chrome-retrieve-tabs)
            ;; Need to manually mark the current tab as active and the
            ;; previously active tab in this window as inactive then render
            ;; both of them.
            (let ((pos (point))
                  (inhibit-read-only t))
              ;; Mark current tab as active and render it.
              (setf (osa-chrome-tab-is-active tab) t)
              (osa-chrome--render-tab tab t)
              ;; Search for previously active tab in this window, mark it as
              ;; inactive and if it's visible render it.
              (cl-loop for tab in (cdr (gethash pid osa-chrome--process-index))
                       for tid = (osa-chrome-tab-id tab)
                       for wid = (osa-chrome-tab-window-id tab) do
                       (when (and (= wid window-id)
                                  ;; Skip currently active tab
                                  (/= tid tab-id)
                                  (osa-chrome-tab-is-active tab))
                         (setf (osa-chrome-tab-is-active tab) nil)
                         (when (gethash (osa-chrome-tab-line tab)
                                        osa-chrome--visible-tabs)
                           (osa-chrome--render-tab tab))
                         (cl-return)))
              (goto-char pos))))))))

(defun osa-chrome-goto-active ()
  "Move point to the next active tab.
By repeatedly invoking this command, you can cycle through all active tabs."
  (interactive)
  (cl-assert (eq major-mode 'osa-chrome-mode) t)
  (when (> (hash-table-count osa-chrome--visible-tabs) 0)
    (cl-loop with pos   = (point)
             with tab   = (osa-chrome-current-tab)
             with line  = (if tab (osa-chrome-tab-line tab) 1)
             with start = (if tab (1+ line) line)
             with end   = (if tab line (save-excursion
                                         (goto-char (point-max))
                                         (line-number-at-pos)))
             ;; Starting either from the next line if a tab is selected
             ;; or beginning of buffer, scan each line for a tab that is
             ;; active. If an active tab is found, immediately return,
             ;; keeping it selected. If no active tab is found and the end
             ;; of the buffer is reached, start scanning from the beginning
             ;; until initial starting position is reached. If no active tab
             ;; is found, go to initial starting position and return.
             initially do (osa-chrome--goto-line start)
             for current   = start then (1+ current)
             for maybe-tab = (osa-chrome-current-tab)
             for is-active = (and maybe-tab (osa-chrome-tab-is-active maybe-tab))
             while (/= current end) do
             (if maybe-tab
                 (if is-active (cl-return) (forward-line))
               ;; Reached end of buffer, start from beginning
               (setq current 0)
               (goto-char (point-min)))
             ;; No active tab found, go to starting position
             finally (goto-char pos))))

;;;###autoload
(defun osa-chrome ()
  "Google Chrome remote tab control."
  (interactive)
  (let ((buf (get-buffer-create "*chrome-tabs*")))
    (switch-to-buffer buf)
    (unless (eq major-mode 'osa-chrome-mode)
      (osa-chrome-mode))))

(provide 'osa-chrome)
;;; osa-chrome.el ends here
