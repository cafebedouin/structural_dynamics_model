import os
import re
import sys

def get_metric_names_from_config(config_path):
    metric_names = {}
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            content = f.read()
            for line in content.splitlines():
                match = re.search(r"param\((\w+_metric_name),\s*(\w+)\)", line)
                if match:
                    param_name = match.group(1)
                    metric_name = match.group(2)
                    metric_names[param_name] = metric_name
    except Exception as e:
        print(f"Warning: Could not read or parse {config_path}: {e}", file=sys.stderr)
    return metric_names

def lint_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    errors = []

    # Get metric names from config.pl
    prolog_dir = os.path.dirname(os.path.dirname(filepath))
    config_path = os.path.join(prolog_dir, 'config.pl')
    metric_names = get_metric_names_from_config(config_path)
    extractiveness_metric = metric_names.get('extractiveness_metric_name', 'extractiveness')
    suppression_metric = metric_names.get('suppression_metric_name', 'suppression_requirement')
    theater_metric = metric_names.get('theater_metric_name', 'theater_ratio')

    # 1. CORE STRUCTURE & v4.0 INTEGRATION HOOKS
    if not re.search(r':- module\(', content):
        errors.append("MISSING_MODULE: Prolog files must begin with :- module(id, []).")

    if "narrative_ontology:interval(" not in content:
        errors.append("MISSING_HOOK: Missing narrative_ontology:interval/3.")

    # Updated to check for v4.0 indexed classification
    if "constraint_classification(" not in content:
        errors.append("OUTDATED_HOOK: v4.0 requires constraint_indexing:constraint_classification/3.")

    # Extract classified types once — reused by rules 2 and 4.
    found_types = set(re.findall(
        r'constraint_classification\(.*?,[\s\n\r]*(mountain|rope|snare|tangled_rope|scaffold|piton)',
        content, re.DOTALL))
    # Mountain-only and rope-only are valid single-type patterns:
    #   NL(C) → ■C[I] for all I  (natural laws)
    #   Pure coordination → ⊞C[I] for all I
    uniform_type = found_types in ({'mountain'}, {'rope'})

    # 2. PERSPECTIVAL MINIMUMS
    # Uniform-type files are exempt: the classification is identical from all
    # perspectives, so requiring specific power atoms would force boilerplate.
    if not uniform_type:
        if "agent_power(powerless)" not in content:
            errors.append("MISSING_PERSPECTIVE: Must include agent_power(powerless).")

        if "agent_power(institutional)" not in content:
            errors.append("MISSING_PERSPECTIVE: Must include agent_power(institutional).")

    # 3. SPATIAL SCOPE VALIDATION
    # Since scope now affects chi via σ(S), validate scope atoms are from the valid set.
    valid_scopes = {'local', 'regional', 'national', 'continental', 'global', 'universal'}
    found_scopes = set(re.findall(r'spatial_scope\((\w+)\)', content))
    invalid_scopes = found_scopes - valid_scopes
    if invalid_scopes:
        errors.append(f"INVALID_SCOPE: spatial_scope values {sorted(invalid_scopes)} not in valid set {sorted(valid_scopes)}.")

    # 4. TYPE VARIANCE CHECK (Updated for v3.4 Categories)
    if len(found_types) < 2 and not uniform_type:
        errors.append(f"INSUFFICIENT_VARIANCE: Found {list(found_types)}. Need at least 2 different types across indices.")

    # 5. DEPRECATED TERMINOLOGY
    # Only flag 'noose' when used as a classification type atom (e.g., in
    # constraint_classification or constraint_claim), not in narrative comments
    # where it may be a direct literary quotation (e.g., Joyce's "three nooses").
    if re.search(r'(?:constraint_classification|constraint_claim)\([^)]*noose', content, re.IGNORECASE):
        errors.append("DEPRECATED_TERM: 'noose' has been replaced by 'snare' in v3.4.")

    # 6. EXTRACT BASE METRICS for downstream checks
    # Try domain_priors facts first, fall back to constraint_metric/3
    # (the classification engine reads from constraint_metric via config metric names)
    ext_match = re.search(r'base_extractiveness\(\w+,\s*([\d.]+)\)', content)
    if not ext_match:
        ext_match = re.search(r'constraint_metric\(\w+,\s*extractiveness,\s*([\d.]+)\)', content)
    ext_val = float(ext_match.group(1)) if ext_match else None

    supp_match = re.search(r'suppression_score\(\w+,\s*([\d.]+)\)', content)
    if not supp_match:
        supp_match = re.search(r'constraint_metric\(\w+,\s*suppression_requirement,\s*([\d.]+)\)', content)
    supp_val = float(supp_match.group(1)) if supp_match else None

    theater_match = re.search(r'theater_ratio\(\w+,\s*([\d.]+)\)', content)
    if not theater_match:
        theater_match = re.search(r'constraint_metric\(\w+,\s*theater_ratio,\s*([\d.]+)\)', content)
    theater_val = float(theater_match.group(1)) if theater_match else None

    # 7. MANDATROPHY & OMEGA VALIDATION
    if ext_val is not None:
        if ext_val > 0.7:
            if "is_mandatrophy_resolved" not in content and "[RESOLVED MANDATROPHY]" not in content:
                errors.append(f"UNRESOLVED_MANDATROPHY: Extraction {ext_val} requires resolution hook.")
        if ext_val > 0.46 and "omega_variable(" not in content:
            errors.append("MISSING_OMEGA: High-extraction constraints (> 0.46) require omega_variable/5.")

    # 8. TEMPORAL MEASUREMENT DATA (LIFECYCLE DRIFT)
    if ext_val is not None and ext_val > 0.46:
        has_measurements = re.search(r'narrative_ontology:measurement\(', content)
        if not has_measurements:
            errors.append(
                f"MISSING_TEMPORAL_DATA: Extraction {ext_val} > 0.46 requires "
                "narrative_ontology:measurement/5 facts for drift detection. "
                f"Add at least 3 time points for {theater_metric} and base_extractiveness."
            )
        else:
            # Check for minimum time point coverage
            measurement_count = len(re.findall(r'narrative_ontology:measurement\(', content))
            if measurement_count < 6:
                errors.append(
                    f"INSUFFICIENT_TEMPORAL_DATA: Found {measurement_count} measurement(s), "
                    f"need at least 6 (3 time points x 2 metrics: {theater_metric}, base_extractiveness)."
                )

    # 9. SCAFFOLD-SPECIFIC CHECKS
    if 'scaffold' in found_types:
        if 'has_sunset_clause(' not in content:
            errors.append("MISSING_SUNSET_CLAUSE: Scaffold classification requires narrative_ontology:has_sunset_clause/1.")

    # 10. PITON-SPECIFIC CHECKS
    if 'piton' in found_types:
        if theater_val is None:
            errors.append("MISSING_THEATER_RATIO: Piton classification requires domain_priors:theater_ratio/2.")
        elif theater_val < 0.70:
            errors.append(f"LOW_THEATER_RATIO: Piton requires theater_ratio >= 0.70, found {theater_val}.")

    # 11. TANGLED ROPE STRUCTURAL PROPERTY CHECKS
    # classify_from_metrics/6 requires all three for tangled_rope:
    #   requires_active_enforcement(C) — asserted directly
    #   has_coordination_function(C)   — derived from constraint_beneficiary/2
    #   has_asymmetric_extraction(C)   — derived from constraint_victim/2
    if 'tangled_rope' in found_types:
        has_beneficiary = 'constraint_beneficiary(' in content
        has_victim = 'constraint_victim(' in content
        has_enforcement = 'requires_active_enforcement(' in content
        if not has_beneficiary:
            errors.append("MISSING_BENEFICIARY: Tangled rope requires constraint_beneficiary/2 (derives has_coordination_function).")
        if not has_victim:
            errors.append("MISSING_VICTIM: Tangled rope requires constraint_victim/2 (derives has_asymmetric_extraction).")
        if not has_enforcement:
            errors.append("MISSING_ENFORCEMENT: Tangled rope requires requires_active_enforcement/1.")

    # 12. SCAFFOLD COORDINATION CHECK
    # classify_from_metrics/6 requires has_coordination_function for scaffold,
    # which is derived from constraint_beneficiary/2.
    if 'scaffold' in found_types:
        if 'constraint_beneficiary(' not in content:
            errors.append("MISSING_BENEFICIARY: Scaffold requires constraint_beneficiary/2 (derives has_coordination_function).")

    # 13. BENEFICIARY/VICTIM CHECK for high-extraction constraints
    if ext_val is not None and ext_val > 0.46:
        has_beneficiary = 'constraint_beneficiary(' in content
        has_victim = 'constraint_victim(' in content
        if not has_beneficiary and not has_victim:
            # Not an error if the bridge derives these, but flag as advisory
            pass  # Bridge auto-derives from metrics; no lint error needed

    return errors
