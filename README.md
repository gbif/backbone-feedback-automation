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
```

**Statuses:**
- `ISSUE_OPEN`: Issue is still open in ChecklistBank
- `ISSUE_CLOSED`: Issue has been resolved in ChecklistBank
- `JSON-TAG-ERROR`: Unable to parse or process the JSON feedback

**Labels applied:**
- `autocheck - issue open on xRelease`: Issue still open in ChecklistBank
- `autocheck - issue closed on xRelease`: Issue resolved in ChecklistBank
- `autocheck - status unclear on xRelease`: Unable to determine status

