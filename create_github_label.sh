
#!/bin/bash

# Function to update label for a single issue
update_issue_label() {
    local issue=$1
    local status=$2
    
    echo "Processing issue #$issue with status: $status"
    
    case "$status" in
        "ISSUE_CLOSED")
            update_label="autocheck - issue closed on xRelease"
            ;;
        "ISSUE_OPEN")
            update_label="autocheck - issue open on xRelease"
            ;;
        "JSON-TAG-ERROR")
            update_label="autocheck - status unclear on xRelease"
            ;;
        *)
            echo "Unknown status: $status"
            return 1
            ;;
    esac
    
    echo "Target label: $update_label"
    
    labels=$(gh issue view "$issue" --repo gbif/backbone-feedback --json labels --jq '.labels[].name')
    
    current_label="" 
    if echo "$labels" | grep -qF "autocheck - issue open on xRelease"; then
        current_label="autocheck - issue open on xRelease"
    elif echo "$labels" | grep -qF "autocheck - issue closed on xRelease"; then
        current_label="autocheck - issue closed on xRelease"
    elif echo "$labels" | grep -qF "autocheck - status unclear on xRelease"; then
        current_label="autocheck - status unclear on xRelease"
    fi
    
    echo "Current label: $current_label"
    
    if [ "$current_label" = "$update_label" ]; then
        echo "No change in label required for issue #$issue"
        return 0
    fi
    
    # Remove all autocheck labels
    gh issue edit $issue --repo gbif/backbone-feedback --remove-label 'autocheck - issue open on xRelease' 2>/dev/null || true
    gh issue edit $issue --repo gbif/backbone-feedback --remove-label 'autocheck - issue closed on xRelease' 2>/dev/null || true
    gh issue edit $issue --repo gbif/backbone-feedback --remove-label 'autocheck - status unclear on xRelease' 2>/dev/null || true
    
    # Add the new label
    echo "Updating label to: $update_label"
    gh issue edit $issue --repo gbif/backbone-feedback --add-label "$update_label"
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





