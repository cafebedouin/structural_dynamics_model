#!/bin/bash
# Clean citation syntax from data files

echo "Cleaning citation syntax from data files..."

for file in *.pl; do
    echo "Processing: $file"
    # Remove [cite_start] and [cite: ...] patterns
    sed -i 's/\[cite_start\]//g; s/\[cite: [^]]*\]//g' "$file"
    echo "  ✓ Cleaned"
done

echo ""
echo "All files cleaned. You can now run:"
echo "  ?- make."
echo "  ?- test_diversity."
