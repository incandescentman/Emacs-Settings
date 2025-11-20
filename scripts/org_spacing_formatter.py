#!/usr/bin/env python3
"""
Formatter for org-mode heading spacing.

Rules implemented (per vibecoding spacing guide):
1. There must be exactly one blank line before any heading.
2. Insert a blank line between consecutive headings (achieved by rule 1).
3. Do not insert blank lines between a heading and its immediate content.

Usage:
    python scripts/org_spacing_formatter.py path1.org [path2.org ...]
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from typing import Iterable, List


HEADING_RE = re.compile(r"^\*+\s")


def is_heading(line: str) -> bool:
    """Return True when the line is an org heading (starts with '*' and space)."""
    return bool(HEADING_RE.match(line))


def format_lines(lines: Iterable[str]) -> List[str]:
    """
    Apply spacing rules to a sequence of lines without trailing newlines.

    The algorithm enforces a single blank line before headings, removes blank
    lines directly after headings (unless another heading follows, which will
    get its own preceding blank line), and collapses duplicate blank lines.
    """
    result: List[str] = []
    last_type: str | None = None  # 'heading', 'content', 'blank'

    for line in lines:
        if is_heading(line):
            # Strip trailing blanks to ensure we add exactly one blank.
            while result and result[-1] == "":
                result.pop()
            if result:
                result.append("")
            result.append(line)
            last_type = "heading"
            continue

        if line.strip() == "":
            if last_type == "heading":
                # Headings shouldn't be separated from their content by blanks.
                continue
            if result and result[-1] == "":
                continue
            result.append("")
            last_type = "blank"
            continue

        result.append(line)
        last_type = "content"

    # Remove trailing blanks at EOF for a consistent ending.
    while result and result[-1] == "":
        result.pop()

    return result


def format_text(text: str) -> str:
    """Return formatted text while preserving a trailing newline."""
    has_trailing_newline = text.endswith("\n")
    lines = text.splitlines()
    formatted_lines = format_lines(lines)
    formatted_text = "\n".join(formatted_lines)
    if formatted_text and (has_trailing_newline or text == ""):
        formatted_text += "\n"
    return formatted_text


def process_file(path: pathlib.Path, check: bool) -> int:
    """Format a file in place (unless --check). Return 0 on success."""
    original = path.read_text(encoding="utf-8")
    formatted = format_text(original)
    if original == formatted:
        return 0
    if check:
        print(f"{path}: needs formatting", file=sys.stderr)
        return 1
    path.write_text(formatted, encoding="utf-8")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Apply org-mode heading spacing rules."
    )
    parser.add_argument(
        "paths",
        nargs="+",
        type=pathlib.Path,
        help="Org files to format",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Only check for formatting issues (non-zero exit if changes needed)",
    )
    args = parser.parse_args(argv)

    exit_code = 0
    for path in args.paths:
        if not path.exists():
            print(f"{path}: not found", file=sys.stderr)
            exit_code = 1
            continue
        result = process_file(path, check=args.check)
        exit_code = exit_code or result
    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
