
  # Workflowy Rapid Capture (with optional Outline Editor)

  ## Install
  ```bash
  cd workflowy-chatgpt
  python3 -m venv .venv && source .venv/bin/activate
  pip install -r requirements.txt
  ```

  ## Usage
  ```bash
  # Capture into today's file
  python3 -m workflowy

  # Capture into a specific file
  python3 -m workflowy notes.txt

  # Append to an existing file when needed
  python3 -m workflowy notes.txt --append

  # Launch outline editor after capture
  python3 -m workflowy --outline
  ```