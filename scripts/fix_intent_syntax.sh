#!/bin/bash
# Fix intent_viable_alternative syntax across all data files
# Converts "Quoted_String" to 'quoted_atom' format

echo "Fixing intent alternative syntax in all data files..."

# Process all data files
for file in *_data.pl; do
    echo "Processing: $file"
    
    # Convert double quotes to single quotes for intent predicates
    # This handles: intent_viable_alternative(..., "String") -> intent_viable_alternative(..., 'String')
    sed -i 's/intent_viable_alternative(\([^,]*\), \([^,]*\), "\([^"]*\)")/intent_viable_alternative(\1, \2, '\''\3'\'')/g' "$file"
    sed -i 's/intent_alternative_rejected(\([^,]*\), \([^,]*\), "\([^"]*\)")/intent_alternative_rejected(\1, \2, '\''\3'\'')/g' "$file"
    
    echo "  ✓ Fixed intent syntax"
done

echo ""
echo "All files fixed. Test with:"
echo "  ?- make."
echo "  ?- test_diversity."
