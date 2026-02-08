#!/bin/bash
# Fix deprecated constraint_claim types by extracting the analytical classification
# from constraint_classification/3 and updating constraint_claim/2.
#
# Deprecated types: coordination, enforcement, natural_law, constructed
# Valid types: mountain, rope, tangled_rope, snare, scaffold, piton

TESTSET_DIR="/home/scott/bin/structural_dynamics_model/prolog/testsets"
VALID_TYPES="mountain|rope|tangled_rope|snare|scaffold|piton"
DEPRECATED="coordination|enforcement|natural_law|constructed"

fixed=0
skipped=0
errors=0

for file in "$TESTSET_DIR"/*.pl; do
    basename=$(basename "$file")

    # Check if this file has a deprecated constraint_claim
    deprecated_match=$(grep -oP "constraint_claim\(\w+,\s*($DEPRECATED)\)" "$file")
    if [ -z "$deprecated_match" ]; then
        continue
    fi

    # Extract the constraint ID and deprecated type
    constraint_id=$(echo "$deprecated_match" | grep -oP "constraint_claim\(\K\w+(?=,)")
    old_type=$(echo "$deprecated_match" | grep -oP ",\s*\K($DEPRECATED)" )

    # Extract the analytical classification type
    analytical_type=$(grep -oP "constraint_classification\($constraint_id,\s*($VALID_TYPES),\s*\n?\s*context\(agent_power\(analytical\)" "$file" | grep -oP "($VALID_TYPES)")

    # If single-line grep didn't work, try multiline
    if [ -z "$analytical_type" ]; then
        analytical_type=$(perl -0777 -ne "print \$1 if /constraint_classification\($constraint_id,\s*($VALID_TYPES),\s*\n\s*context\(agent_power\(analytical\)/" "$file")
    fi

    if [ -z "$analytical_type" ]; then
        echo "ERROR: $basename - Could not find analytical classification for $constraint_id"
        ((errors++))
        continue
    fi

    # Replace the deprecated type in constraint_claim
    sed -i "s/constraint_claim($constraint_id, $old_type)/constraint_claim($constraint_id, $analytical_type)/" "$file"

    if [ $? -eq 0 ]; then
        echo "FIXED: $basename - $old_type -> $analytical_type"
        ((fixed++))
    else
        echo "ERROR: $basename - sed replacement failed"
        ((errors++))
    fi
done

echo ""
echo "================================"
echo "  Fixed:   $fixed"
echo "  Skipped: $skipped"
echo "  Errors:  $errors"
echo "================================"
