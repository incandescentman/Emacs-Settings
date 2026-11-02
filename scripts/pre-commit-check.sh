#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"

mapfile -t STAGED_EL < <(git diff --cached --name-only --diff-filter=ACMR -- '*.el' ':(exclude)archive/**')

if [ "${#STAGED_EL[@]}" -eq 0 ]; then
  exit 0
fi

echo "[pre-commit] Checking staged .el files (${#STAGED_EL[@]})..."

FAIL=0
for rel in "${STAGED_EL[@]}"; do
  file="$ROOT/$rel"
  [ -f "$file" ] || continue
  first_line="$(head -n 1 "$file" || true)"
  # An explicit nil cookie is allowed: it keeps a dynamically scoped file's
  # behavior while declaring the choice, which Emacs 31 otherwise warns about.
  if ! grep -Eq 'lexical-binding:[[:space:]]*(t|nil)([[:space:];]|$)' <<<"$first_line"; then
    echo "[pre-commit] Missing lexical-binding cookie (t or nil) on first line: $rel"
    FAIL=1
  fi
done

if [ "$FAIL" -ne 0 ]; then
  echo "[pre-commit] lexical-binding header check failed."
  exit 1
fi

EMACS_APP="$(/Users/jay/Dropbox/apps/bash-profile/bin/emacs-app 2>/dev/null || echo /Applications/Emacs.app)"
if [ -x "$EMACS_APP/Contents/MacOS/Emacs" ]; then
  EMACS_BIN="$EMACS_APP/Contents/MacOS/Emacs"
elif command -v emacs >/dev/null 2>&1; then
  EMACS_BIN="$(command -v emacs)"
else
  echo "[pre-commit] Emacs binary not found."
  exit 1
fi

for rel in "${STAGED_EL[@]}"; do
  file="$ROOT/$rel"
  [ -f "$file" ] || continue
  echo "[pre-commit] check-parens $rel"
  FILE_TO_CHECK="$file" "$EMACS_BIN" --batch \
    --eval '(let ((f (getenv "FILE_TO_CHECK")))
              (with-temp-buffer
                (insert-file-contents f)
                (emacs-lisp-mode)
                (check-parens)))' >/dev/null
done

echo "[pre-commit] OK"
