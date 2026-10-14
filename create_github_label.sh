
#!/bin/bash

FOCAL_ISSUE_REPO="${FOCAL_ISSUE_REPO:-gbif/backbone-feedback}"

# Function to update label for a single issue
update_issue_label() {
    local issue=$1
    local status=$2
    
    echo "Processing issue #$issue with status: $status"
    
    case "$status" in
        "ISSUE_CLOSED")
            update_label="autocheck - issue closed on COL"
            ;;
        "ISSUE_OPEN")
            update_label="autocheck - issue open on COL"
            ;;
        "JSON-TAG-ERROR")
            update_label="autocheck - issue unclear on COL"
            ;;
        *)
            echo "Unknown status: $status"
            return 1
            ;;
    esac
    
    echo "Target label: $update_label"
    
    labels=$(gh issue view "$issue" --repo "$FOCAL_ISSUE_REPO" --json labels --jq '.labels[].name')
    
    current_label="" 
    if echo "$labels" | grep -qF "autocheck - issue open on COL"; then
        current_label="autocheck - issue open on COL"
    elif echo "$labels" | grep -qF "autocheck - issue closed on COL"; then
        current_label="autocheck - issue closed on COL"
    elif echo "$labels" | grep -qF "autocheck - issue unclear on COL"; then
        current_label="autocheck - issue unclear on COL"
    fi
    
    echo "Current label: $current_label"
    
    if [ "$current_label" = "$update_label" ]; then
        echo "No change in label required for issue #$issue"
        return 0
    fi
    
    # Remove current and legacy autocheck labels before applying the new one.
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue open on COL' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue closed on COL' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue unclear on COL' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue open on COL XR' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue closed on COL XR' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue unclear on COL XR' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue open on xRelease' 2>/dev/null || true
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --remove-label 'autocheck - issue closed on xRelease' 2>/dev/null || true
    
    # Add the new label
    echo "Updating label to: $update_label"
    gh issue edit "$issue" --repo "$FOCAL_ISSUE_REPO" --add-label "$update_label"
}

# Check if arguments are provided for single-issue mode
if [ $# -eq 2 ]; then
    # Single issue mode: issue number and status provided as arguments
    update_issue_label "$1" "$2"
    exit 0
fi

# Batch mode: read from TSV file (backward compatibility)
tsv_file="${1:-report.tsv}"

if [ ! -f "$tsv_file" ]; then
    echo "Error: TSV file not found: $tsv_file"
    echo "Usage: $0 [issue_number status] OR $0 [tsv_file]"
    exit 1
fi

echo "Running in batch mode with TSV file: $tsv_file"

header_skipped=true
while IFS=$'\t' read -r issue status type; do
    if [ "$header_skipped" = true ]; then
        header_skipped=false
        continue
    fi
 
    status=${status//\"/}  # Unquote the status variable
    issue=${issue//\"/}  # Unquote the issue variable
    
    update_issue_label "$issue" "$status"

done < "$tsv_file"


