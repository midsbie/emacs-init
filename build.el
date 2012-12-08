;; requires
(require 'compile)
(require 'ansi-color)

;; variable definition
(defvar build-base-dir nil
  "The base directory of the active project.")

(defvar build-output-dir nil
  "The directory where the project will be compiled into, relative to
  BUILD-BASE-DIR.")

(defvar build-cmd-build "sh/build -p"
  "The command that runs the project's compilation sequence.")

(defvar build-cmd-rebuild "sh/build -prd"
  "Command that fully rebuilds the project.")

(defvar build-cmd-run "sh/run"
  "The command that runs the project's binary.")

(defvar build-bin-name nil
  "The filename of the executable binary." )

(defvar build-bin-args nil
  "The parameters to feed to BUILD-BIN-NAME when launching it from within gdb")

(defvar build-cmd-gdb "gdb -i=mi "
  "The command that invokes gdb, including flags." )

;; various settings
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)
(setq gdb-many-windows 1)

;; defuns
(defun build-gdb-running()
  (and (boundp 'gud-comint-buffer)
       gud-comint-buffer
       (buffer-name gud-comint-buffer)
       (get-buffer-process gud-comint-buffer))
  )

(defun build-do()
  (interactive)
  (if (not (build-gdb-running))
      (progn
        (compile (concat build-base-dir build-cmd-build)))
    (error "error: not building project as gdb is running"))
  )

(defun build-rebuild()
  (interactive)
  (if (not (build-gdb-running))
      (progn
        (compile (concat build-base-dir build-cmd-rebuild)))
    (error "error: not building project as gdb is running"))
  )

(defun build-run()
  (interactive)
  (if (not (build-gdb-running))
      (shell-command build-cmd-run)
    (error "error: not running binary as gdb is running"))
  )

(defun build-on-compilation-finish (buffer status)
  (remove-hook 'compilation-finish-functions 'build-on-compilation-finish)
  (when (string-prefix-p "finished" status)
    (let ((window (get-buffer-window "*compilation*")))
      (set-window-buffer window (other-buffer)))
    (window-configuration-to-register 99)
    (gdb (concat
          build-cmd-gdb
          "--args "
          (concat build-base-dir
                  build-output-dir
                  build-bin-name)
          (concat " " build-bin-args)))
    (if (> gdb-many-windows 0)
        (run-at-time 0.5 nil
                     (lambda()
                       (other-window 2))))
    (message "gdb started"))
)

(defun build-gdb-go()
  (interactive)
  (if (not (build-gdb-running))
      (build-launch-gdb)
    (gud-go t)))

(defun build-stop-gdb()
  (interactive)
  (when (build-gdb-running)
    (let ((proc (get-buffer-process gud-comint-buffer)))
      (setq gud-overlay-arrow-position nil)
      (set-process-buffer proc nil)
      (setq gud-comint-buffer nil)
      (set-process-sentinel proc nil)
      (kill-process proc)
      (kill-process "gdb-inferior")
      (gdb-reset)
      (jump-to-register 99)
      (message "gdb killed"))
    )  
  )

(defun build-launch-gdb()
  (interactive)
  (if (not (build-gdb-running))
      (if (not build-bin-name)
          (error "error: build-bin-name not given")
        (progn 
          (add-hook 'compilation-finish-functions 'build-on-compilation-finish)
          (build-do)))
    (build-stop-gdb)
    )
  )

(defun build-gud-set-break()
  (interactive)
  (gud-break (point)))

(defun build-gud-del-break()
  (interactive)
  (gud-remove (point)))

(add-hook 'c-mode-common-hook
           (lambda ()
             (local-set-key [f9] 'build-do)
             (local-set-key [C-f9] 'build-rebuild)))

(defun build-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'build-colorize-compilation-buffer)

;; global shortcut definition
(global-set-key [f5]      'build-gdb-go);         ; gdb run/continue
(global-set-key [S-f5]    'build-launch-gdb)      ; run: debug
(global-set-key [C-f5]    'gud-run)               ; gdb: (re)run
(global-set-key [f6]      'gud-step)              ; step into
(global-set-key [f7]      'gud-next)              ; step over
(global-set-key [C-f7]    'gud-finish)            ; step out
(global-set-key [f8]      'build-gud-set-break)   ; set breakpoint
(global-set-key [S-f8]    'build-gud-del-break)   ; del breakpoint
(global-set-key [C-f8]    'gud-until)             ; run to cursor
(global-set-key [C-pause] 'gud-stop-subjob)       ; interrupt
