#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Parse command-line options
ISSUE_STATE="open"
REPORT_FILE="report.tsv"
ENABLE_REPORT=""
SKIP_LABEL=""
LOG_FILE=""
VERBOSE=""
REPORT_COMMENT=""
FORCE_UPDATE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --closed)
            ISSUE_STATE="closed"
            REPORT_FILE="report-closed.tsv"
            shift
            ;;
        --report)
            ENABLE_REPORT="--report"
            shift
            ;;
        --no-label)
            SKIP_LABEL="true"
            shift
            ;;
        --verbose)
            VERBOSE="--verbose"
            shift
            ;;
        --report-comment)
            REPORT_COMMENT="true"
            shift
            ;;
        --force-update)
            FORCE_UPDATE="true"
            shift
            ;;
        --log|--logfile)
            # If next argument exists and doesn't start with --, use it as filename
            if [ -n "$2" ] && [[ ! "$2" =~ ^-- ]] && [[ ! "$2" =~ ^[0-9]+$ ]]; then
                LOG_FILE="$2"
                shift 2
            else
                # Default to log.txt if no filename provided
                LOG_FILE="log.txt"
                shift
            fi
            ;;
        [0-9]*)
            # Numeric argument is an issue numbe
            SINGLE_ISSUE="$1"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--closed] [--report] [--no-label] [--verbose] [--report-comment] [--force-update] [--log [FILE]] [issue_number]"
            exit 1
            ;;
    esac
done

# Initialize log file if specified
if [ -n "$LOG_FILE" ]; then
    echo "Logging output to: $LOG_FILE"
    echo "========================================" > "$LOG_FILE"
    echo "Issue Check Log - $(date)" >> "$LOG_FILE"
    echo "========================================" >> "$LOG_FILE"
    echo "" >> "$LOG_FILE"
    # Define a function to log messages to both console and file
    log() {
        echo "$@" | tee -a "$LOG_FILE"
    }
else
    # If no log file, just use regular echo
    log() {
        echo "$@"
    }
fi

# Install the latest version of gbifbf package from source
log "Installing gbifbf package from source..."
Rscript -e 'install.packages("./gbifbf", repos = NULL, type = "source")'

# Check if issue number is provided as command-line argument
if [ -n "$SINGLE_ISSUE" ]; then
    log "Processing single issue: $SINGLE_ISSUE"
    issue_array=("$SINGLE_ISSUE")
else
    # Fetch issues based on state
    log "Fetching all $ISSUE_STATE issues from project..."
    issues=$(gh issue list --repo gbif/backbone-feedback --search "is:issue is:$ISSUE_STATE project:gbif/23" --json number --jq '.[].number' --limit 500)
    log $issues
    
    for issue in $issues; do
        issue_array+=("$issue")
    done
fi

for issue in "${issue_array[@]}"
do
    log "Processing issue: $issue"
    COMMENTS=$(curl -H "Authorization: token $GH_TOKEN" -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/gbif/backbone-feedback/issues/$issue/comments)
    # echo $COMMENTS
    if [ -z "$COMMENTS" ]; then
        log "Error: No comments received for issue $issue"
        continue
    fi

    if ! echo "$COMMENTS" | jq empty; then
        log "Error: Invalid JSON received for issue $issue"
        continue
    fi
    
    # Process comments with "// json for auto-checking" UNLESS they have an unchecked checkbox
    # Skip ONLY if checkbox is explicitly unchecked: "- [ ] **Accept AI suggestion**"
    # Process if: (1) checkbox is checked, OR (2) no checkbox exists (legacy comments)
    JSON=$(echo "$COMMENTS" | jq '.[] | select(.body | contains("// json for auto-checking") and (contains("- [ ] **Accept AI suggestion**") | not)) | {id, body}')
    COMMENT_ID=$(echo "$JSON" | jq '.id')
    COMMENT_BODY=$(echo "$JSON" | jq '.body')
    # echo $COMMENT_BODY
    if [ "$COMMENT_BODY" != "null" ] && [ -n "$COMMENT_BODY" ]; then
        # Run process_json.R and capture output (format: issue|status|type)
        OUTPUT=$(Rscript process_json.R "$COMMENT_BODY" "$issue" "$REPORT_FILE" $ENABLE_REPORT $VERBOSE)
        log "Process output: $OUTPUT"
        
        # Parse output
        IFS='|' read -r issue_num status type <<< "$OUTPUT"
        
        # Immediately update GitHub label for this issue (unless --no-label flag is set)
        if [ -n "$status" ] && [ -z "$SKIP_LABEL" ]; then
            ./create_github_label.sh "$issue_num" "$status"
        elif [ -n "$SKIP_LABEL" ]; then
            log "Skipping label update (--no-label flag set)"
        fi
        
        # Create/update validation report comment (if --report-comment flag is set)
        if [ -n "$REPORT_COMMENT" ] && [ -n "$status" ] && [ "$COMMENT_ID" != "null" ]; then
            log "Creating/updating validation report comment..."
            if [ -n "$FORCE_UPDATE" ]; then
                ./create_github_comment.sh "$issue_num" "$COMMENT_ID" "$status" "$type" "$COMMENT_BODY" --force
            else
                ./create_github_comment.sh "$issue_num" "$COMMENT_ID" "$status" "$type" "$COMMENT_BODY"
            fi
        fi
    else
        log "No processable JSON comments found for issue $issue (may have unchecked checkbox)"
    fi
done

# Final log message if logging enabled
if [ -n "$LOG_FILE" ]; then
    log ""
    log "========================================"
    log "Completed at $(date)"
    log "========================================"
fi






