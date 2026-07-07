# Backbone Feedback Automation

Automated processing and labeling of GBIF backbone feedback issues from GitHub.

## Overview

This repository contains tools to automatically:
1. Process JSON feedback tags from GitHub issues
2. Check issue status against the GBIF ChecklistBank API
3. Update GitHub issue labels based on the validation results

## Components

### R Package (`gbifbf/`)
R package with functions for validating taxonomic feedback against ChecklistBank.

### Python Agent (`agent/`)
Interactive command-line tool for testing and validating JSON feedback.

### Shell Scripts

#### `issue_check.sh`
Main orchestrator that fetches GitHub issues and processes them.

**Usage:**
```bash
# Process all open issues (labels updated immediately, no report)
./issue_check.sh

# Process a single issue
./issue_check.sh 123

# Process without updating labels (dry-run)
./issue_check.sh --no-label

# Process single issue without updating label
./issue_check.sh --no-label 123

# Process all open issues AND generate report.tsv for review
./issue_check.sh --report

# Process closed issues with report
./issue_check.sh --closed --report

# Process single closed issue
./issue_check.sh --closed 456

# Save output to a log file
./issue_check.sh --log output.log

# Use default log file (log.txt)
./issue_check.sh --log

# Show detailed validation reports (verbose mode)
./issue_check.sh --verbose

# Create/update automated validation report comments on issues
./issue_check.sh --report-comment

# Combine options: process open issues with report and logging
./issue_check.sh --report --log check-$(date +%Y%m%d).log

# Full automation: update labels AND post validation reports
./issue_check.sh --report-comment --verbose
```

**Options:**
- `--closed`: Process closed issues instead of open ones
- `--report`: Generate a TSV report file (report.tsv or report-closed.tsv)
- `--no-label`: Run in dry-run mode without updating GitHub labels
- `--verbose`: Show detailed validation reports for each issue (by default, reports are silenced)
- `--report-comment`: Create/update automated validation report comments on GitHub issues. Only updates when status changes (disabled by default)
- `--log [FILE]` or `--logfile [FILE]`: Save all output to a log file (also displayed on console). Defaults to log.txt if no filename provided.
- `[issue_number]`: Process a single issue by number instead of all issues

**Statuses:**
- `ISSUE_OPEN`: Issue is still open in ChecklistBank
- `ISSUE_CLOSED`: Issue has been resolved in ChecklistBank
- `JSON-TAG-ERROR`: Unable to parse or process the JSON feedback

**Labels applied:**
- `autocheck - issue open on xRelease`: Issue still open in ChecklistBank
- `autocheck - issue closed on xRelease`: Issue resolved in ChecklistBank
- `autocheck - status unclear on xRelease`: Unable to determine status

**Automated Validation Report Comments:**

When the `--report-comment` option is enabled, the script will create or update automated validation report comments directly on the GitHub issue. These comments:
- Display structured validation results from the R report functions
- Include the issue type and validation status
- Show detailed findings (name existence, COL IDs, classifications, synonym relationships, etc.)
- Are automatically updated ONLY when the validation status changes
- Are marked with a 🤖 emoji and "AUTOMATED VALIDATION REPORT" header
- Track the status in HTML comments for smart update detection

This feature is disabled by default to allow manual testing before enabling automation. To use it:
```bash
# Enable automated report comments
./issue_check.sh --report-comment

# Combine with verbose mode to see reports in console too
./issue_check.sh --report-comment --verbose
```

The script uses the same status-change detection logic as label updates, ensuring comments are only posted when there's meaningful new information to share.

#### `create_github_comment.sh`
Creates or updates automated validation report comments on GitHub issues. This script is called by `issue_check.sh` when the `--report-comment` option is enabled.

**Usage:**
```bash
./create_github_comment.sh <issue_number> <comment_id> <status> <issue_type> <json_comment_body>
```

**Parameters:**
- `issue_number`: GitHub issue number
- `comment_id`: ID of the comment containing the JSON tag
- `status`: Validation status (ISSUE_OPEN, ISSUE_CLOSED, JSON-TAG-ERROR)
- `issue_type`: Type of issue (missingName, nameChange, wrongGroup, wrongRank, wrongStatus)
- `json_comment_body`: The JSON comment body to process

**Behavior:**
- Searches for existing automated report comment on the issue (identified by marker)
- Extracts previous status from existing comment (if any)
- Only creates/updates comment if status has changed
- Generates detailed validation report by calling process_json.R with --report-output flag
- Uses GitHub API via `gh` CLI to POST new comment or PATCH existing comment

This script implements the same smart update logic as `create_github_label.sh` to avoid comment spam.

