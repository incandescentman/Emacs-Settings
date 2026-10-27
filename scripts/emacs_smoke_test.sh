#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EMACS_BIN="${EMACS_BIN:-emacs}"
FULL_INIT=0

if [[ "${1:-}" == "--full-init" ]]; then
  FULL_INIT=1
elif [[ -n "${1:-}" ]]; then
  echo "Usage: $0 [--full-init]"
  exit 2
fi

echo "[1/5] Tangling core literate files..."
"$EMACS_BIN" --batch --eval \
  "(progn
     (require 'org)
     (require 'ob-tangle)
     (org-babel-tangle-file \"$ROOT/shared-functions.org\" \"$ROOT/shared-functions.el\" \"emacs-lisp\")
     (org-babel-tangle-file \"$ROOT/web-capture.org\" \"$ROOT/web-capture.el\" \"emacs-lisp\")
     (org-babel-tangle-file \"$ROOT/gnu-emacs-startup.org\" \"$ROOT/gnu-emacs-startup.el\" \"emacs-lisp\"))"

echo "[2/5] Running check-parens on key files..."
FILES=(
  "shared-functions.el"
  "web-capture.el"
  "spacemacs-new-config.el"
  "gnu-emacs-startup.el"
  "pasteboard-copy-and-paste-functions.el"
  "jay-org-roam-suite/jay-org-roam-core.el"
)

for rel in "${FILES[@]}"; do
  "$EMACS_BIN" --batch "$ROOT/$rel" --eval '(check-parens)'
  echo "  ok: $rel"
done

echo "[3/5] Running capture cleanup ERT tests..."
# -Q disables package activation; use the existing Spacemacs dependencies.
"$EMACS_BIN" --batch -Q \
  --eval '(progn
            (require (quote package))
            (setq package-user-dir (expand-file-name "elpa/develop" user-emacs-directory))
            (package-initialize))' \
  -L "$ROOT/org-web-tools-master" \
  -l "$ROOT/tests/capture-cleanup-tests.el" \
  -l "$ROOT/tests/org-web-tools-pandoc-tests.el" \
  -f ert-run-tests-batch-and-exit

echo "[4/5] Minimal runtime health checks..."
"$EMACS_BIN" --batch --eval \
  "(progn
     (require 'org)
     (princ
      (format \"org=%s | org-link-preview=%s | org-support-shift-select-bound=%s\n\"
              org-version
              (if (fboundp 'org-link-preview) 'yes 'no)
              (if (boundp 'org-support-shift-select) 'yes 'no))))"

for fix in org-roam-id-fix.el org-roam-db-fix.el; do
  if [[ -f "$ROOT/jay-org-roam-suite/$fix" ]]; then
    echo "  fix file $fix: suite dir"
  else
    echo "  fix file $fix: MISSING"
    exit 1
  fi
done

if [[ "$FULL_INIT" -eq 1 ]]; then
  echo "[5/5] Full-init smoke check..."
  "$EMACS_BIN" --batch \
    --load "$HOME/.emacs.d/early-init.el" \
    --load "$HOME/.emacs.d/init.el" --eval \
    "(progn
       ;; Batch mode does not run the startup hook that loads user-config.
       (run-hooks 'emacs-startup-hook)
       (with-temp-buffer
         (org-mode)
         (message \"shift %S\" org-support-shift-select))
       (unless (and (featurep 'web-capture)
                    (equal (file-truename (symbol-file 'jay/org-web-tools-insert-web-page-as-entry-clean))
                           (file-truename \"$ROOT/web-capture.el\"))
                    (eq (lookup-key key-minor-mode-map (kbd \"s-P\"))
                        'jay/org-web-tools-insert-web-page-as-entry-clean))
         (error \"Web capture module or s-P binding did not load\"))
       (princ \"Web capture module and s-P startup checks passed\n\"))"
else
  echo "[5/5] Skipping full-init check (pass --full-init to enable)."
fi

echo "Smoke test complete."
