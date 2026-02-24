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

echo "[1/4] Tangling core literate files..."
"$EMACS_BIN" --batch --eval \
  "(progn
     (require 'org)
     (require 'ob-tangle)
     (org-babel-tangle-file \"$ROOT/shared-functions.org\" \"$ROOT/shared-functions.el\" \"emacs-lisp\")
     (org-babel-tangle-file \"$ROOT/gnu-emacs-startup.org\" \"$ROOT/gnu-emacs-startup.el\" \"emacs-lisp\"))"

echo "[2/4] Running check-parens on key files..."
FILES=(
  "shared-functions.el"
  "gnu-emacs-startup.el"
  "pasteboard-copy-and-paste-functions.el"
  "jay-org-roam-suite/jay-org-roam-core.el"
  "org-roam-config.el"
)

for rel in "${FILES[@]}"; do
  "$EMACS_BIN" --batch "$ROOT/$rel" --eval '(check-parens)'
  echo "  ok: $rel"
done

echo "[3/4] Minimal runtime health checks..."
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
  elif [[ -f "$ROOT/$fix" ]]; then
    echo "  fix file $fix: parent dir"
  else
    echo "  fix file $fix: MISSING"
    exit 1
  fi
done

if [[ "$FULL_INIT" -eq 1 ]]; then
  echo "[4/4] Full-init smoke check..."
  "$EMACS_BIN" --batch --load "$HOME/.emacs.d/init.el" --eval \
    "(with-temp-buffer
       (org-mode)
       (message \"shift %S\" org-support-shift-select))"
else
  echo "[4/4] Skipping full-init check (pass --full-init to enable)."
fi

echo "Smoke test complete."
