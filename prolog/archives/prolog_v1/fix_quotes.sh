#!/bin/bash
#
# fix_quotes.sh - Verify Prolog files have correct syntax
#

TARGET_DIR="${1:-.}"

echo "================================================================"
echo "  PROLOG SYNTAX VERIFICATION"
echo "================================================================"
echo ""

error_count=0
total_files=0

for file in $(find "$TARGET_DIR" -name "*_data.pl" -type f | sort); do
    ((total_files++))
    basename_file=$(basename "$file")
    
    # Check for double quotes in predicates (should be single quotes)
    if grep -q 'recommendation([^,]*, "' "$file" || \
       grep -q 'scenario_description("' "$file" || \
       grep -q 'intent_description("' "$file"; then
        echo "❌ $basename_file: Contains double quotes in predicates"
        ((error_count++))
        continue
    fi
    
    # Check for common unescaped nested quote patterns
    # Look for patterns like: 'text 'word' text' where the inner quotes aren't doubled
    if grep -E "recommendation\([^,]+, '[^']*'[a-zA-Z]" "$file" | grep -qv "''"; then
        # This might be a false positive, so let's check more carefully
        suspicious_lines=$(grep -n "recommendation.*'.*'[^'].*'" "$file" | grep -v "''" || true)
        if [ -n "$suspicious_lines" ]; then
            echo "⚠️  $basename_file: Possibly unescaped nested quotes (check manually)"
            echo "    $suspicious_lines"
            ((error_count++))
            continue
        fi
    fi
    
    echo "✓ $basename_file"
done

echo ""
echo "================================================================"
echo "  SUMMARY"
echo "================================================================"
echo "Files checked: $total_files"
echo "Files with issues: $error_count"
echo ""

if [ $error_count -eq 0 ]; then
    echo "🎉 All files pass basic syntax checks!"
    echo ""
    exit 0
else
    echo "⚠️  Some files may have syntax issues."
    echo "Run the quote fixer if needed:"
    echo "  ./fix_quotes.sh $TARGET_DIR"
    exit 1
fi
