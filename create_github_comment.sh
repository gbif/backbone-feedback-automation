#!/bin/bash

# Script to create or update automated validation report comments on GitHub issues
# Usage: ./create_github_comment.sh <issue_number> <comment_id> <status> <issue_type> <json_comment_body>

issue=$1
comment_id=$2
status=$3
issue_type=$4
json_comment_body=$5

if [ -z "$issue" ] || [ -z "$comment_id" ] || [ -z "$status" ] || [ -z "$issue_type" ]; then
    echo "Error: Missing required arguments"
    echo "Usage: $0 <issue_number> <comment_id> <status> <issue_type> <json_comment_body>"
    exit 1
fi

echo "Creating/updating validation report for issue #$issue (comment: $comment_id, status: $status)"

MARKER="🤖 AUTOMATED VALIDATION REPORT"
REPO="gbif/backbone-feedback"

# Get all comments on this issue
all_comments=$(gh api "/repos/$REPO/issues/$issue/comments" --jq '.[] | {id: .id, body: .body}')

# Find existing automated report comment
existing_report_id=$(echo "$all_comments" | jq -r --arg marker "$MARKER" 'select(.body | contains($marker)) | .id' | head -1)

previous_status=""
if [ -n "$existing_report_id" ]; then
    # Get the full comment body to extract status
    comment_body=$(gh api "/repos/$REPO/issues/comments/$existing_report_id" --jq '.body')
    # Extract status using sed (more portable than grep -P)
    previous_status=$(echo "$comment_body" | sed -n 's/.*<!-- STATUS: \([^>]*\) -->.*/\1/p' | head -1)
    if [ -z "$previous_status" ]; then
        previous_status=""
    fi
fi

echo "Existing report ID: $existing_report_id"
echo "Previous status: '$previous_status'"
echo "Current status: '$status'"

# Determine if we need to create/update the comment
if [ -z "$existing_report_id" ]; then
    echo "No existing report found - will create new comment"
elif [ "$previous_status" = "$status" ]; then
    echo "Status unchanged ($status) - skipping comment update"
    exit 0
else
    echo "Status changed from '$previous_status' to '$status' - will update comment"
fi

# Generate report by calling process_json.R with --report-output flag
echo "Generating validation report..."
report_output=$(Rscript process_json.R "$json_comment_body" "$issue" "report.tsv" "--report-output" 2>&1)

if [ -z "$report_output" ]; then
    echo "Error: Failed to generate report output"
    exit 1
fi

# Format status with symbols
case "$status" in
    "ISSUE_CLOSED")
        status_display="✅ CLOSED"
        ;;
    "ISSUE_OPEN")
        status_display="❌ OPEN"
        ;;
    "JSON-TAG-ERROR")
        status_display="⚠️ ERROR"
        ;;
    *)
        status_display="❓ $status"
        ;;
esac

# Construct the comment body with marker and status metadata
timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ" 2>/dev/null || date -u +"%Y-%m-%d %H:%M:%S")

comment_body="## $MARKER

<!-- STATUS: $status -->
<!-- COMMENT_ID: $comment_id -->
<!-- GENERATED: $timestamp -->

**Issue Type:** \`$issue_type\` | **Status:** $status_display

$report_output

---

<sub>🔄 This comment is automatically updated when the validation status changes. Last updated: $timestamp</sub>"

# Create or update the comment
if [ -z "$existing_report_id" ]; then
    echo "Creating new comment..."
    gh api "/repos/$REPO/issues/$issue/comments" -X POST -f body="$comment_body"
    echo "Comment created successfully"
else
    echo "Updating existing comment (ID: $existing_report_id)..."
    gh api "/repos/$REPO/issues/comments/$existing_report_id" -X PATCH -f body="$comment_body"
    echo "Comment updated successfully"
fi

exit 0
