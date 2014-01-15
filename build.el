;; build.el --- Assists programmers manage project builds.
;;
;; Copyright (C) 2013 Miguel Guedes
;;
;; Author: Miguel Guedes <miguel.a.guedes@gmail.com>
;; URL:
;; Keywords: completion, convenience
;; Version: 1.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Comments:
;;
;;

;; requires
(require 'compile)
(require 'ansi-color)

;; variable definition
(defgroup build nil
  "Build."
  :prefix "bl-")

(defvar bl-keymap (make-sparse-keymap)
  "Build mode keymap.")

(defcustom bl-projects
  '()
  "List of buildable projects.  Element structure:

< base-dir:string         ; ndx 0
  output-dir:string       ; ndx 1
  bin-name:string         ; ndx 2
  bin-args:string         ; ndx 3
  cmd-build:string        ; ndx 4
  cmd-rebuild:string      ; ndx 5
  cmd-run:string >        ; ndx 6

BASE-DIR
An absolute (or expandable) path to the project's base directory.

OUTPUT-DIR
A path to the project's output directory relative to the project's base
directory.

CMD-BUILD
The command to execute when building the project.

CMD-REBUILD
The command to execute when rebuilding the project.

CMD-RUN
The command to execute when running the project.

BIN-NAME
The file name of the binary produced by the project, if any.

BIN-ARGS
Arguments to pass on to BIN-NAME when executing it.
"
  :type '(repeat symbol))

(defvar bl-default-cmd-build "sh/build -pe"
  "The default command that builds a project.")

(defvar bl-default-cmd-rebuild "sh/build -pre"
  "Default command that rebuilds a project.")

(defvar bl-default-cmd-gdb "gdb -i=mi "
  "The default command that invokes gdb, including flags." )

(defvar bl-compilation-failed-hook nil
  "List of hooks to call when a compilation failed.")

(defvar bl-compilation-success-hook nil
  "List of hooks to call when a compilation finishes successfully.")

(defvar bl-temporary-project nil)


(define-minor-mode build-mode
  "Toggle build mode."
  :lighter " B"
  :keymap bl-keymap
  :group 'build
  (if build-mode
      (progn
        ;; Setup build
        (setq compilation-disable-input         nil
              compilation-scroll-output         t
              mode-compile-always-save-buffer-p t
              gdb-many-windows                  1)

        (add-hook 'compilation-filter-hook 'bl-colorize-compilation-buffer)
        
        (define-key bl-keymap [f5]      'bl-gdb-go);        ; gdb run/continue
        (define-key bl-keymap [S-f5]    'bl-launch-gdb)     ; run: debug
        (define-key bl-keymap [C-f5]    'bl-run)            ; execute project
                                                            ; binary
        (define-key bl-keymap [f6]      'gud-next)          ; step over
        (define-key bl-keymap [f7]      'gud-step)          ; step into
        (define-key bl-keymap [S-f7]    'gud-finish)        ; step out
        (define-key bl-keymap [f8]      'bl-gud-set-brkpt)  ; set breakpoint
        (define-key bl-keymap [S-f8]    'bl-gud-del-brkpt)  ; del breakpoint
        (define-key bl-keymap [C-f8]    'gud-until)         ; run to cursor
        (define-key bl-keymap [C-pause] 'gud-stop-subjob)   ; interrupt
        (define-key bl-keymap [f9]      'bl-build)          ; build
        (define-key bl-keymap [C-f9]    'bl-rebuild)        ; rebuild
        )

    ;; Uninstall mode
    (remove-hook 'compilation-filter-hook 'bl-colorize-compilation-buffer)
    
    (define-key bl-keymap [f5]      nil)
    (define-key bl-keymap [S-f5]    nil)
    (define-key bl-keymap [C-f5]    nil)
    (define-key bl-keymap [f6]      nil)
    (define-key bl-keymap [f7]      nil)
    (define-key bl-keymap [S-f7]    nil)
    (define-key bl-keymap [f8]      nil)
    (define-key bl-keymap [S-f8]    nil)
    (define-key bl-keymap [C-f8]    nil)
    (define-key bl-keymap [C-pause] nil)
    (define-key bl-keymap [f9]      nil)
    (define-key bl-keymap [C-f9]    nil)))


(define-globalized-minor-mode global-build-mode build-mode build-mode)

(defun bl-get-project ()
  (if (bl-gdb-running-p)
      (progn
        (message "error: gdb is currently running")
        nil)
    (if (= (length bl-projects) 0)
        (progn
          (message "error: no projects defined yet")
          nil)
      (let ((file-name (buffer-file-name)))
        (when file-name
          (let ((len-file  (length file-name))
                (pos       '(-1 nil)))
            (dolist (item bl-projects)
              (let* ((path (expand-file-name (car item)))
                     (len-p (length path)))
                (when (and (>= len-file len-p)
                           (string= (substring file-name 0 len-p) path))
                  (when (> len-p (car pos))
                    (setq pos (list len-p item))))))
            ;; Return our findings
            (if (cadr pos)
                (cadr pos)
              (message "error: buffer not bound to any project")
              nil)))))))

(defun bl-gdb-running-p()
  "Returns t if gdb is found to be running."
  (and (boundp 'gud-comint-buffer)
       gud-comint-buffer
       (buffer-name gud-comint-buffer)
       (get-buffer-process gud-comint-buffer)))

(defun bl-path-append(&rest paths)
  "Appends a list of paths together whilst making sure the
previous path appended ended with a '/' character."
  (let ((result (car paths)))
    (dolist (p (cdr paths))
      (if (string-match "/$" result)
          (setq result (concat result p))
        (setq result (concat result "/" p))))
    result))

(defun bl-compile(base-dir cmd)
  "Smartly concatenates BASE-DIR and CMD and starts a compilation
on the result."
  (if (not cmd)
      (message "error: no command given")
    (bl-setup-on-compilation-finish)
    (compile (bl-path-append base-dir cmd))))

(defun bl-build(&optional project)
  "Builds the project that the current buffer belongs to."
  (interactive)
  (let ((prj))
    (setf prj (or project (bl-get-project)))
    (when prj
      (let ((cmd (nth 4 prj)))          ; cmd-build
        (unless cmd
          (setq cmd bl-default-cmd-build))
        (bl-compile (car prj)           ; base-dir
                    cmd)))))

(defun bl-rebuild()
  "Rebuilds the project that the current buffer belongs to."
  (interactive)
  (let ((project (bl-get-project)))
    (when project
      (let ((cmd (nth 5 project)))      ; cmd-rebuild
        (unless cmd
          (setq cmd bl-default-cmd-rebuild))
        (bl-compile (car project)       ; base-dir
                    cmd)))))

(defun bl-run()
  "Executes the project."
  (interactive)
  (let ((project (bl-get-project)))
    (when project
      (let ((cmd (nth 6 project)))          ; cmd-run
        (unless cmd
          (setq cmd (cadr (cdr project)))   ; bin-name
          (when cmd
            (setq cmd (bl-path-append (cadr project)    ; output-dir
                                      cmd))))
        (if (not cmd)
            (message "error: project binary not set")
          (shell-command (bl-path-append (car project)  ; base-dir
                                         cmd)))))))

(defun bl-setup-on-compilation-finish ()
  "Sets up the defun bl-on-compilation-finish to receive notifications from
finished compilations."
  (add-hook 'compilation-finish-functions 'bl-on-compilation-finish))

(defun bl-on-compilation-finish (buffer status)
  "Invoked when the compilation initiated by `bl-launch-gdb'
finishes."
  (remove-hook 'compilation-finish-functions 'bl-on-compilation-finish)
  (if (not (string-prefix-p "finished" status))
      (run-hooks 'bl-compilation-failed-hook)
    (let ((window (get-buffer-window "*compilation*")))
      (set-window-buffer window (other-buffer)))
    (run-hooks 'bl-compilation-success-hook)))

(defun bl-gdb-go()
  "Launches gdb if currently not active or sends the 'continue'
command to gdb."
  (interactive)
  (if (not (bl-gdb-running-p))
      (bl-launch-gdb)
    (gud-go t)))

(defun bl-stop-gdb()
  "Terminates gdb if currently active."
  (interactive)
  (when (bl-gdb-running-p)
    (let ((proc (get-buffer-process gud-comint-buffer)))
      (setq gud-overlay-arrow-position  nil
            gud-comint-buffer           nil)

      (set-process-buffer   proc nil)
      (set-process-sentinel proc nil)

      (kill-process proc)
      (kill-process "gdb-inferior")

      (gdb-reset)
      (jump-to-register 99)
      (message "gdb killed"))))

(defun bl-launch-gdb()
  "Launches gdb."
  (interactive)
  (let* ((project   (bl-get-project))
         (bin-name  (cadr (cdr project))))
    (when project
      (if (not bin-name)
          (message "error: project binary not given")
        (add-hook 'bl-compilation-success 'bl--launch-gdb-hook)
        (bl-stop-gdb)
        (bl-build project)))))

(defun bl--launch-gdb-hook ()
  "Internal hook.  Launches gdb after a compilation was successful."
  (when bl-temporary-project
    (window-configuration-to-register 99)
    (gdb (concat
          bl-default-cmd-gdb
          "--args "
          (concat (car bl-temporary-project)                    ; base-dir
                  (cadr bl-temporary-project)                   ; output-dir
                  (cadr (cdr bl-temporary-project))             ; bin-name
                  (concat " " (nth 3 bl-temporary-project)))))  ; bin-args
    (if (> gdb-many-windows 0)
        (run-at-time 0.5 nil
                     (lambda()
                       (other-window 2))))
    (message "gdb started")))

(defun bl-gud-set-brkpt()
  "Sets breakpoint at point."
  (interactive)
  (when (bl-gdb-running-p)
    (gud-brkpt (point))))

(defun bl-gud-del-brkpt()
  "Removes breakpoint at point."
  (interactive)
  (when (bl-gdb-running-p)
    (gud-remove (point))))

(defun bl-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))