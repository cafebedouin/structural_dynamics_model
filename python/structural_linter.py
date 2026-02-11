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

    if '% lint: skip' in content:
        return []

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

    # 9b. SCAFFOLD DANGER ZONE (v6.0)
    # Catch constraints whose metrics could produce false scaffold classifications
    # at the engine level. The scaffold gate fires when:
    #   chi <= 0.30 AND has_coordination_function AND NOT requires_active_enforcement
    #   AND theater < 0.70
    # If a constraint has eps <= 0.30, beneficiary data (coordination), no enforcement,
    # and no sunset clause, it's in the scaffold danger zone — the engine may classify
    # it as scaffold even though it lacks temporality (scaffold's defining feature).
    if ext_val is not None and ext_val <= 0.30:
        # Mountains naturally have low extraction, beneficiary data, no enforcement,
        # no sunset clause — the same profile as scaffold-danger. But mountains can't
        # be misclassified as scaffolds (signature override catches this). Skip the
        # check when any indexed classification is mountain — these are structural-type
        # constraints (natural laws, math theorems) where scaffold concern doesn't apply.
        all_mountain = 'mountain' in found_types
        if not all_mountain:
            has_beneficiary_data = 'constraint_beneficiary(' in content
            has_enforcement = 'requires_active_enforcement(' in content
            has_sunset = 'has_sunset_clause(' in content
            low_theater = theater_val is None or theater_val < 0.70
            if has_beneficiary_data and not has_enforcement and not has_sunset and low_theater:
                errors.append(
                    f"SCAFFOLD_DANGER_ZONE: eps={ext_val} with beneficiary data, no enforcement, "
                    "no sunset clause, and low theater. The engine's scaffold gate may fire "
                    "at moderate/analytical perspectives. Either add has_sunset_clause/1 (if "
                    "genuinely a scaffold) or verify that the intended classification is rope."
                )

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

    # 13. BENEFICIARY/VICTIM CHECK for all non-mountain constraints (v6.0)
    # The directionality derivation chain requires beneficiary/victim data to
    # compute constraint-specific d values. Without these, the engine falls back
    # to generic canonical d from the power atom alone.
    has_beneficiary = 'constraint_beneficiary(' in content
    has_victim = 'constraint_victim(' in content
    is_mountain_only = found_types == {'mountain'}
    # Check for explicit "no enrichment needed" marker (mountains, symmetric ropes)
    has_no_enrichment_marker = 'No enrichment needed' in content or 'no enrichment needed' in content

    if not is_mountain_only and not has_no_enrichment_marker:
        if not has_beneficiary:
            errors.append(
                "MISSING_BENEFICIARY: Non-mountain constraints require "
                "narrative_ontology:constraint_beneficiary/2 for directionality derivation. "
                "Without it, the engine falls back to canonical d (power atom only)."
            )

    # Snare-specific victim requirement
    if 'snare' in found_types:
        if not has_victim:
            errors.append(
                "MISSING_VICTIM: Snare classification requires "
                "narrative_ontology:constraint_victim/2 to identify extraction target."
            )

    # 14. CONSTRAINT_CLAIM CHECK (v5.1)
    # If file has constraint_classification/3, it must also have constraint_claim/2.
    # The claim type must be one of the 6 valid classification types.
    valid_claim_types = {'mountain', 'rope', 'tangled_rope', 'snare', 'scaffold', 'piton'}
    if 'constraint_classification(' in content:
        if 'constraint_claim(' not in content:
            errors.append(
                "MISSING_CLAIM: File declares constraint_classification/3 but no "
                "constraint_claim/2. Add narrative_ontology:constraint_claim(id, Type) "
                "where Type matches the analytical perspective classification."
            )
        else:
            # Validate claim type is one of the 6 valid types.
            # Only check ground atoms (lowercase first char) — skip Prolog
            # variables (uppercase) used in test predicates.
            claim_types = re.findall(
                r'constraint_claim\(\s*\w+\s*,\s*(\w+)\s*\)', content)
            for ct in claim_types:
                if ct[0].isupper():
                    continue  # Prolog variable, not a ground atom
                if ct not in valid_claim_types:
                    errors.append(
                        f"INVALID_CLAIM_TYPE: constraint_claim type '{ct}' is not valid. "
                        f"Must be one of: {sorted(valid_claim_types)}."
                    )

    # 15. COORDINATION_TYPE CONSISTENCY (v5.0)
    # If coordination_type/2 is declared, validate the atom is one of the 4 valid types.
    valid_coord_types = {
        'information_standard', 'resource_allocation',
        'enforcement_mechanism', 'global_infrastructure'
    }
    coord_type_matches = re.findall(
        r"coordination_type\(\s*'?\w+'?\s*,\s*(\w+)\s*\)", content)
    for ct in coord_type_matches:
        if ct not in valid_coord_types:
            errors.append(
                f"INVALID_COORDINATION_TYPE: coordination_type value '{ct}' is not valid. "
                f"Must be one of: {sorted(valid_coord_types)}."
            )

    # 16. AFFECTS_CONSTRAINT SELF-REFERENCE CHECK (v5.2)
    # affects_constraint(X, X) is a self-referential error.
    for match in re.finditer(
            r"affects_constraint\(\s*'?(\w+)'?\s*,\s*'?(\w+)'?\s*\)", content):
        source, target = match.group(1), match.group(2)
        if source == target:
            errors.append(
                f"SELF_REFERENCE: affects_constraint({source}, {target}) is "
                "self-referential. A constraint cannot structurally influence itself."
            )
        # Check that target is not a Prolog variable (starts with uppercase)
        if target[0].isupper():
            errors.append(
                f"VARIABLE_IN_AFFECTS: affects_constraint({source}, {target}) uses "
                "a variable as target. Both arguments must be ground atoms."
            )

    # 17. BOLTZMANN_FLOOR_OVERRIDE RANGE CHECK (v5.0)
    for match in re.finditer(
            r"boltzmann_floor_override\(\s*'?\w+'?\s*,\s*([\d.]+)\s*\)", content):
        floor_val = float(match.group(1))
        if floor_val < 0.0 or floor_val > 1.0:
            errors.append(
                f"INVALID_FLOOR_OVERRIDE: boltzmann_floor_override value {floor_val} "
                "is out of range. Must be in [0.0, 1.0]."
            )
        if ext_val is not None and floor_val > ext_val:
            errors.append(
                f"FLOOR_EXCEEDS_EXTRACTION: boltzmann_floor_override ({floor_val}) "
                f"exceeds base_extractiveness ({ext_val}). Floor should be <= extraction."
            )

    # 18. DIRECTIONALITY_OVERRIDE VALIDATION (v6.0)
    # directionality_override(ConstraintID, PowerAtom, D_Value)
    valid_power_atoms = {
        'powerless', 'moderate', 'powerful', 'organized', 'institutional', 'analytical'
    }
    for match in re.finditer(
            r"directionality_override\(\s*'?\w+'?\s*,\s*(\w+)\s*,\s*([\d.]+)\s*\)", content):
        power_atom = match.group(1)
        d_val = float(match.group(2))
        if power_atom not in valid_power_atoms:
            errors.append(
                f"INVALID_POWER_ATOM: directionality_override power atom '{power_atom}' "
                f"is not valid. Must be one of: {sorted(valid_power_atoms)}."
            )
        if d_val < 0.0 or d_val > 1.0:
            errors.append(
                f"INVALID_D_VALUE: directionality_override d value {d_val} "
                "is out of range. Must be in [0.0, 1.0]."
            )

    # 19. GENERIC PLACEHOLDER GROUP CHECK (v6.0)
    # Beneficiary/victim group names must be domain-specific, not generic.
    generic_groups = {
        'stakeholders', 'affected_parties', 'the_public', 'society',
        'everyone', 'all_parties', 'general_public', 'participants',
        'users', 'people', 'individuals', 'entities',
    }
    for pred in ('constraint_beneficiary', 'constraint_victim'):
        for match in re.finditer(
                rf"{pred}\(\s*'?\w+'?\s*,\s*(\w+)\s*\)", content):
            group = match.group(1)
            if group.lower() in generic_groups:
                errors.append(
                    f"GENERIC_GROUP: {pred} uses generic group '{group}'. "
                    "Use a domain-specific group name (e.g., 'payday_lenders' not 'stakeholders')."
                )

    # 20. MULTIFILE BLOCK COMPLETENESS (v6.0)
    # If directionality_override is used, it must be in the multifile block.
    if re.search(r'(?<!%).*directionality_override\(', content):
        if 'constraint_indexing:directionality_override/3' not in content:
            errors.append(
                "MISSING_MULTIFILE: File uses directionality_override/3 but "
                "does not declare it in the :- multifile block."
            )

    # 21. VACUOUS_TEST DETECTION
    # Finds test/1 clauses where a comparison operator uses a Prolog variable
    # that is never bound (singleton in clause body). These tests always
    # succeed or fail vacuously.
    comparison_ops = r'(?:>=|=<|(?<!-)>|<|=:=|=\\=)'  # (?<!-) avoids matching -> as >
    # Match test(Name) :- Body. across lines (clauses end with '.\n' or EOF)
    for test_match in re.finditer(
            r'test\((\w+)\)\s*:-\s*(.*?)\.(?:\n|$)', content, re.DOTALL):
        test_name = test_match.group(1)
        raw_body = test_match.group(2)
        body_start_line = content[:test_match.start()].count('\n') + 1

        # Strip Prolog line comments (% to EOL) to avoid false positives
        # from variable names mentioned in comments.
        body_lines = raw_body.split('\n')
        stripped_body = '\n'.join(
            re.sub(r'%.*$', '', line) for line in body_lines
        )

        # Find all comparisons in the stripped body
        for cmp_match in re.finditer(
                rf'(\b[A-Z_]\w*)\s*{comparison_ops}|{comparison_ops}\s*(\b[A-Z_]\w*)',
                stripped_body):
            var = cmp_match.group(1) or cmp_match.group(2)
            if var and var != '_':
                # Count occurrences of this variable in the stripped body
                var_count = len(re.findall(rf'\b{re.escape(var)}\b', stripped_body))
                if var_count == 1:
                    cmp_line = body_start_line + stripped_body[:cmp_match.start()].count('\n')
                    errors.append(
                        f"VACUOUS_TEST: test({test_name}) at line {cmp_line} uses "
                        f"unbound variable '{var}' in comparison. The variable appears "
                        f"only once in the clause body (singleton), making the test "
                        f"vacuous. Bind '{var}' to a value before comparing."
                    )

    # 22. DUPLICATE MEASUREMENT DETECTION
    # drl_lifecycle.pl uses msort to order measurement points by time.
    # If two measurements share the same (Constraint, Metric, Time) but different
    # values, msort orders them by value, creating a phantom step that can
    # incorrectly trigger non_monotonic_trajectory.
    measurements = {}  # (constraint, metric, time) -> [(id, value), ...]
    for m_match in re.finditer(
            r"narrative_ontology:measurement\(\s*(\w+)\s*,\s*(\w+)\s*,"
            r"\s*([^,]+?)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)",
            content):
        m_id = m_match.group(1)
        constraint = m_match.group(2)
        metric = m_match.group(3).strip()
        time_pt = m_match.group(4)
        value = m_match.group(5)
        key = (constraint, metric, time_pt)
        measurements.setdefault(key, []).append((m_id, value))

    for (constraint, metric, time_pt), entries in measurements.items():
        if len(entries) > 1:
            values = [v for _, v in entries]
            unique_values = set(values)
            if len(unique_values) > 1:
                errors.append(
                    f"DUPLICATE_MEASUREMENT: ({constraint}, {metric}, {time_pt}) "
                    f"appears {len(entries)} times with different values. "
                    f"This can cause phantom steps in monotonicity detection. "
                    f"Use unique time points or consolidate measurements."
                )
            else:
                errors.append(
                    f"REDUNDANT_MEASUREMENT: ({constraint}, {metric}, {time_pt}, "
                    f"{values[0]}) is declared more than once. Remove the duplicate."
                )

    return errors
