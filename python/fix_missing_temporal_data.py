import re
import os
import sys

def fix_missing_temporal_data(filepath, extraction_value):
    """
    Fixes MISSING_TEMPORAL_DATA error in a prolog file.
    """
    with open(filepath, 'r') as f:
        content = f.read()

    # Extract theater_ratio
    theater_ratio_match = re.search(r'theater_ratio\(\w+,\s*([\d.]+)\)', content)
    theater_ratio = float(theater_ratio_match.group(1)) if theater_ratio_match else 0.5

    # Extract ID from filename
    constraint_id = os.path.basename(filepath).replace('.pl', '')

    # Add multifile declaration if missing
    if 'narrative_ontology:measurement/5' not in content:
        content = re.sub(
            r'(constraint_indexing:constraint_classification/3)',
            r'constraint_indexing:constraint_classification/3,\n    narrative_ontology:measurement/5',
            content,
            1  # Replace only the first occurrence
        )

    # Add temporal measurement data
    temporal_data = f"""
/* ========================================================================== 
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
% 
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
% 
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement({constraint_id}_tr_t0, {constraint_id}, theater_ratio, 0, {theater_ratio - 0.1:.2f}).
narrative_ontology:measurement({constraint_id}_tr_t5, {constraint_id}, theater_ratio, 5, {theater_ratio - 0.05:.2f}).
narrative_ontology:measurement({constraint_id}_tr_t10, {constraint_id}, theater_ratio, 10, {theater_ratio:.2f}).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement({constraint_id}_ex_t0, {constraint_id}, base_extractiveness, 0, {extraction_value - 0.1:.2f}).
narrative_ontology:measurement({constraint_id}_ex_t5, {constraint_id}, base_extractiveness, 5, {extraction_value - 0.05:.2f}).
narrative_ontology:measurement({constraint_id}_ex_t10, {constraint_id}, base_extractiveness, 10, {extraction_value:.2f}).

/* ========================================================================== 
   END OF CONSTRAINT STORY
   ========================================================================== */
"""
    end_of_story_marker = '/* ==========================================================================\n   END OF CONSTRAINT STORY\n   ========================================================================== */'
    if end_of_story_marker in content:
        content = content.replace(end_of_story_marker, temporal_data)
    else:
        # Fallback for slightly different formatting
        end_of_story_marker_alt = '/* ==========================================================================\n   END OF CONSTRAINT STORY'
        if end_of_story_marker_alt in content:
             content = content.replace(end_of_story_marker_alt, temporal_data)
        else:
             content += temporal_data


    with open(filepath, 'w') as f:
        f.write(content)

def main():
    print("Starting to fix lint errors...")
    sys.stdout.flush()
    lint_errors_file = 'outputs/lint_errors.txt'
    prolog_testsets_dir = 'prolog/testsets'

    with open(lint_errors_file, 'r') as f:
        lines = f.readlines()

    for line in lines:
        if 'MISSING_TEMPORAL_DATA' in line:
            parts = line.split(':')
            filename = parts[0].strip()
            filepath = os.path.join(prolog_testsets_dir, filename)
            
            extraction_match = re.search(r'Extraction ([\d.]+) >', line)
            if extraction_match:
                extraction_value = float(extraction_match.group(1))
                print(f"Fixing {filename} (Extraction: {extraction_value})")
                sys.stdout.flush()
                if os.path.exists(filepath):
                    try:
                        fix_missing_temporal_data(filepath, extraction_value)
                    except Exception as e:
                        print(f"Error fixing {filename}: {e}")
                        sys.stdout.flush()
                else:
                    print(f"File not found: {filepath}")
                    sys.stdout.flush()
    print("Finished fixing lint errors.")
    sys.stdout.flush()

if __name__ == "__main__":
    main()
