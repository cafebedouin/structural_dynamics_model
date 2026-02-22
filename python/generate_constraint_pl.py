#!/usr/bin/env python3
"""Generate a v6.0 constraint story .pl file from a JSON authoring file.

Usage:
    python generate_constraint_pl.py input.json > output.pl
    python generate_constraint_pl.py --validate-only input.json
    python generate_constraint_pl.py --no-validate input.json > output.pl

Requires: Python 3.8+ (stdlib only). Optional: jsonschema for validation.
"""

import json
import os
import sys

# ---------------------------------------------------------------------------
# Schema validation
# ---------------------------------------------------------------------------

def _load_schema():
    """Load the JSON schema from the same directory as this script."""
    schema_path = os.path.join(os.path.dirname(__file__), "constraint_story_schema.json")
    with open(schema_path, "r", encoding="utf-8") as f:
        return json.load(f)


def validate_json(data):
    """Validate data against the constraint story schema.

    Returns a list of error strings (empty = valid).
    Falls back to basic structural checks if jsonschema is not installed.
    """
    try:
        import jsonschema
        schema = _load_schema()
        # Try Draft 2020-12 first, fall back to Draft7 for older jsonschema
        validator_cls = getattr(jsonschema, "Draft202012Validator", None)
        if validator_cls is None:
            validator_cls = getattr(jsonschema, "Draft7Validator", None)
        if validator_cls is None:
            validator_cls = jsonschema.Draft4Validator
        validator = validator_cls(schema)
        return [e.message for e in sorted(validator.iter_errors(data), key=lambda e: list(e.path))]
    except (ImportError, Exception):
        return _basic_validate(data)


def _basic_validate(data):
    """Minimal validation without jsonschema."""
    errors = []
    for key in ("header", "base_properties", "perspectives", "interval"):
        if key not in data:
            errors.append(f"Missing required top-level key: {key}")
    if "header" in data:
        for key in ("constraint_id", "version", "generated_date", "status"):
            if key not in data["header"]:
                errors.append(f"Missing required header field: {key}")
    if "base_properties" in data:
        for key in ("extractiveness", "suppression", "theater_ratio",
                     "claimed_type", "human_readable", "topic_domain"):
            if key not in data["base_properties"]:
                errors.append(f"Missing required base_properties field: {key}")
    if "perspectives" in data:
        if not isinstance(data["perspectives"], list) or len(data["perspectives"]) < 2:
            errors.append("perspectives must be an array with at least 2 items")
    if "interval" in data:
        for key in ("start", "end"):
            if key not in data["interval"]:
                errors.append(f"Missing required interval field: {key}")
    return errors


# ---------------------------------------------------------------------------
# Measurement ID generation
# ---------------------------------------------------------------------------

_METRIC_ABBREVS = {
    "theater_ratio": "tr",
    "base_extractiveness": "be",
}


def _measurement_id(prefix, metric, time_point):
    abbrev = _METRIC_ABBREVS.get(metric, metric[:2])
    return f"{prefix}_{abbrev}_t{time_point}"


def _get_measurement_prefix(data):
    header = data["header"]
    if "measurement_id_prefix" in header:
        return header["measurement_id_prefix"]
    # Auto-derive: first 4 chars of constraint_id, or full if shorter
    cid = header["constraint_id"]
    return cid[:4] if len(cid) > 4 else cid


# ---------------------------------------------------------------------------
# Multifile block generation
# ---------------------------------------------------------------------------

def _build_multifile_declarations(data):
    """Build the list of multifile predicates actually used."""
    bp = data["base_properties"]
    cid = data["header"]["constraint_id"]

    # 8 core predicates (always included)
    decls = [
        "domain_priors:base_extractiveness/2",
        "domain_priors:suppression_score/2",
        "domain_priors:theater_ratio/2",
        "domain_priors:requires_active_enforcement/1",
        "narrative_ontology:has_sunset_clause/1",
        "narrative_ontology:interval/3",
        "narrative_ontology:measurement/5",
        "narrative_ontology:constraint_metric/3",
        "narrative_ontology:constraint_beneficiary/2",
        "narrative_ontology:constraint_victim/2",
        "narrative_ontology:constraint_claim/2",
    ]

    # Conditional predicates
    if data.get("network", {}).get("affects_constraints"):
        decls.append("narrative_ontology:affects_constraint/2")

    if data.get("boltzmann", {}).get("coordination_type"):
        decls.append("narrative_ontology:coordination_type/2")

    if data.get("boltzmann", {}).get("boltzmann_floor_override") is not None:
        if "narrative_ontology:coordination_type/2" not in decls:
            pass  # coordination_type might not be needed
        decls.append("narrative_ontology:boltzmann_floor_override/2")

    decls.append("constraint_indexing:constraint_classification/3")

    if data.get("directionality_overrides"):
        decls.append("constraint_indexing:directionality_override/3")

    if bp.get("emerges_naturally"):
        decls.append("domain_priors:emerges_naturally/1")

    # Always include omega_variable/3 if omegas present
    if data.get("omegas"):
        decls.append("narrative_ontology:omega_variable/3")

    # human_readable and topic_domain
    decls.append("narrative_ontology:human_readable/2")
    decls.append("narrative_ontology:topic_domain/2")

    return decls


# ---------------------------------------------------------------------------
# Test auto-generation
# ---------------------------------------------------------------------------

def _generate_tests(data):
    """Generate validation test predicates based on constraint properties."""
    cid = data["header"]["constraint_id"]
    bp = data["base_properties"]
    perspectives = data["perspectives"]
    tests = []

    # Collect distinct types
    types_seen = set()
    for p in perspectives:
        types_seen.add(p["classification_type"])

    # Find powerless perspective
    powerless_persp = None
    first_different = None
    for p in perspectives:
        if p["agent_power"] == "powerless":
            powerless_persp = p
            break

    # perspectival_gap: multiple distinct types across perspectives
    if len(types_seen) >= 2 and powerless_persp:
        # Find the first perspective with a different type
        for p in perspectives:
            if p["classification_type"] != powerless_persp["classification_type"]:
                first_different = p
                break

        if first_different:
            tests.append(
                f"test(perspectival_gap) :-\n"
                f"    constraint_indexing:constraint_classification({cid}, TypePowerless, "
                f"context(agent_power(powerless), _, _, _)),\n"
                f"    constraint_indexing:constraint_classification({cid}, TypeOther, "
                f"context(agent_power({first_different['agent_power']}), _, _, _)),\n"
                f"    TypePowerless \\= TypeOther."
            )

    # extraction_signature: extractiveness > 0.46
    if bp["extractiveness"] > 0.46:
        tests.append(
            f"test(extraction_signature) :-\n"
            f"    domain_priors:base_extractiveness({cid}, E),\n"
            f"    E >= 0.46. % Ensures high-extraction Snare/Tangled territory."
        )

    # Mountain-specific tests
    if bp["claimed_type"] == "mountain":
        tests.append(
            f"test(invariance_check) :-\n"
            f"    % Verify that as a Mountain, the classification is uniform across perspectives.\n"
            f"    constraint_indexing:constraint_classification({cid}, TypeTarget, "
            f"context(agent_power(powerless), _, _, _)),\n"
            f"    constraint_indexing:constraint_classification({cid}, TypeBeneficiary, "
            f"context(agent_power(institutional), _, _, _)),\n"
            f"    TypeTarget == TypeBeneficiary,\n"
            f"    TypeTarget == mountain."
        )
        tests.append(
            f"test(mountain_threshold_validation) :-\n"
            f"    config:param(extractiveness_metric_name, ExtMetricName),\n"
            f"    narrative_ontology:constraint_metric({cid}, ExtMetricName, E),\n"
            f"    domain_priors:suppression_score({cid}, S),\n"
            f"    E =< 0.25,\n"
            f"    S =< 0.05."
        )
        tests.append(
            f"test(nl_profile_validation) :-\n"
            f"    domain_priors:emerges_naturally({cid}),\n"
            f"    narrative_ontology:constraint_metric({cid}, accessibility_collapse, AC),\n"
            f"    narrative_ontology:constraint_metric({cid}, resistance, R),\n"
            f"    AC >= 0.85,\n"
            f"    R =< 0.15."
        )

    # Piton-specific tests
    if "piton" in types_seen:
        tests.append(
            f"test(piton_threshold) :-\n"
            f"    domain_priors:theater_ratio({cid}, TR),\n"
            f"    TR >= 0.70."
        )

    return tests


# ---------------------------------------------------------------------------
# Main generation
# ---------------------------------------------------------------------------

def generate_pl(data):
    """Generate the .pl file content from validated JSON data."""
    cid = data["header"]["constraint_id"]
    header = data["header"]
    bp = data["base_properties"]
    perspectives = data["perspectives"]
    interval = data["interval"]
    commentary = data.get("commentary", {})
    omegas = data.get("omegas", [])
    measurements = data.get("measurements", [])
    boltzmann = data.get("boltzmann", {})
    network = data.get("network", {})
    dir_overrides = data.get("directionality_overrides", [])

    module_name = header.get("module_name_override", f"constraint_{cid}")
    meas_prefix = _get_measurement_prefix(data)

    lines = []

    def emit(s=""):
        lines.append(s)

    # ------------------------------------------------------------------
    # 1. Header comment block
    # ------------------------------------------------------------------
    emit(f"% ============================================================================")
    emit(f"% CONSTRAINT STORY: {cid}")
    emit(f"% ============================================================================")
    emit(f"% Version: {header['version']} (Deferential Realism Core + Directionality + Boltzmann + Network)")
    emit(f"% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)")
    emit(f"% Generated: {header['generated_date']}")
    emit(f"% Status: [{header['status']}]")
    emit(f"% ============================================================================")
    emit()

    # ------------------------------------------------------------------
    # 2. Module declaration
    # ------------------------------------------------------------------
    emit(f":- module({module_name}, []).")
    emit()

    # ------------------------------------------------------------------
    # 3. use_module directives
    # ------------------------------------------------------------------
    emit(":- use_module(constraint_indexing).")
    emit(":- use_module(domain_priors).")
    emit(":- use_module(narrative_ontology).")
    emit()

    # ------------------------------------------------------------------
    # 4. DP-001 epsilon-invariance comment block
    # ------------------------------------------------------------------
    emit("% --- Constraint Identity Rule (DP-001: ε-Invariance) ---")
    emit("% Each constraint story must have a single, stable base extractiveness (ε).")
    emit("% If changing the observable used to evaluate this constraint would change ε,")
    emit("% you are looking at two distinct constraints. Write separate .pl files for")
    emit("% each, link them with affects_constraint/2, and document the relationship")
    emit("% in both files' narrative context sections.")
    emit("%")
    emit("% The context tuple is CLOSED at arity 4: (P, T, E, S).")
    emit("% Do not add measurement_basis, beneficiary/victim, or any other arguments.")
    emit("% Linter Rule 23 enforces context/4.")
    emit("%")
    emit("% See: epsilon_invariance_principle.md")
    emit()

    # ------------------------------------------------------------------
    # 5. Multifile block
    # ------------------------------------------------------------------
    decls = _build_multifile_declarations(data)
    emit("% --- Namespace Hooks (Required for loading) ---")
    emit(":- multifile")
    for i, d in enumerate(decls):
        comma = "," if i < len(decls) - 1 else "."
        emit(f"    {d}{comma}")
    emit()

    # ------------------------------------------------------------------
    # 6. Section 1: Narrative context
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   1. NARRATIVE CONTEXT")
    emit("   ========================================================================== */")
    emit()
    emit("/**")
    emit(" * CONSTRAINT IDENTIFICATION")
    emit(f" *   constraint_id: {cid}")
    emit(f" *   human_readable: {bp['human_readable']}")
    emit(f" *   domain: {bp['topic_domain']}")
    emit(" *")

    if commentary.get("narrative_context"):
        emit(" * SUMMARY:")
        # Word-wrap narrative context at ~72 chars per line
        words = commentary["narrative_context"].split()
        line = " *  "
        for w in words:
            if len(line) + len(w) + 1 > 78:
                emit(line)
                line = " *   " + w
            else:
                line += " " + w
        if line.strip(" *"):
            emit(line)
        emit(" *")

    if commentary.get("key_agents"):
        emit(" * KEY AGENTS:")
        for agent in commentary["key_agents"]:
            emit(f" *   - {agent}")

    emit(" */")
    emit()

    # ------------------------------------------------------------------
    # 7. Section 2: Base properties
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   2. BASE PROPERTIES (DOMAIN PRIORS)")
    emit("   ========================================================================== */")
    emit()

    # Numerical metrics — domain_priors facts
    emit("% --- Numerical metrics ---")
    emit(f"domain_priors:base_extractiveness({cid}, {bp['extractiveness']}).")
    emit(f"domain_priors:suppression_score({cid}, {bp['suppression']}).")
    emit(f"domain_priors:theater_ratio({cid}, {bp['theater_ratio']}).")
    emit()

    # constraint_metric facts (must mirror domain_priors)
    emit("% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---")
    emit(f"narrative_ontology:constraint_metric({cid}, extractiveness, {bp['extractiveness']}).")
    emit(f"narrative_ontology:constraint_metric({cid}, suppression_requirement, {bp['suppression']}).")
    emit(f"narrative_ontology:constraint_metric({cid}, theater_ratio, {bp['theater_ratio']}).")
    emit()

    # NL profile metrics (mountain)
    if bp.get("accessibility_collapse") is not None or bp.get("resistance") is not None:
        emit("% --- NL Profile Metrics (required for mountain constraints) ---")
        if bp.get("accessibility_collapse") is not None:
            emit(f"narrative_ontology:constraint_metric({cid}, accessibility_collapse, {bp['accessibility_collapse']}).")
        if bp.get("resistance") is not None:
            emit(f"narrative_ontology:constraint_metric({cid}, resistance, {bp['resistance']}).")
        emit()

    # Claim
    emit("% --- Constraint claim ---")
    emit(f"narrative_ontology:constraint_claim({cid}, {bp['claimed_type']}).")
    emit(f'narrative_ontology:human_readable({cid}, "{bp["human_readable"]}").')
    emit(f'narrative_ontology:topic_domain({cid}, "{bp["topic_domain"]}").')
    emit()

    # Binary flags
    if bp.get("requires_active_enforcement"):
        emit(f"domain_priors:requires_active_enforcement({cid}).")
    if bp.get("has_sunset_clause"):
        emit(f"narrative_ontology:has_sunset_clause({cid}).")
    if bp.get("emerges_naturally"):
        emit(f"domain_priors:emerges_naturally({cid}).")

    if bp.get("requires_active_enforcement") or bp.get("has_sunset_clause") or bp.get("emerges_naturally"):
        emit()

    # Beneficiaries and victims
    beneficiaries = bp.get("beneficiaries", [])
    victims = bp.get("victims", [])
    is_mountain_only = bp["claimed_type"] == "mountain"

    if beneficiaries or victims:
        emit("% --- Structural relationships ---")
        for b in beneficiaries:
            emit(f"narrative_ontology:constraint_beneficiary({cid}, {b}).")
        for v in victims:
            emit(f"narrative_ontology:constraint_victim({cid}, {v}).")
        emit()
    elif is_mountain_only:
        emit("% --- Structural relationships ---")
        emit("% No enrichment needed. As a Mountain (physical limit), this constraint does")
        emit("% not have beneficiaries or victims in the structural sense.")
        emit()

    # ------------------------------------------------------------------
    # 8. Section 3: Indexed classifications
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   3. INDEXED CLASSIFICATIONS (P, T, E, S)")
    emit("   ========================================================================== */")
    emit()

    for i, p in enumerate(perspectives):
        if p.get("comment"):
            emit(f"% {p['comment']}")
        emit(f"constraint_indexing:constraint_classification({cid}, {p['classification_type']},")
        emit(f"    context(agent_power({p['agent_power']}),")
        emit(f"            time_horizon({p['time_horizon']}),")
        emit(f"            exit_options({p['exit_options']}),")
        emit(f"            spatial_scope({p['spatial_scope']}))).")
        emit()

    # ------------------------------------------------------------------
    # 9. Section 4: Validation tests
    # ------------------------------------------------------------------
    tests = _generate_tests(data)
    emit("/* ==========================================================================")
    emit("   4. VALIDATION TESTS")
    emit("   ========================================================================== */")
    emit()

    if tests:
        emit(f":- begin_tests({cid}_tests).")
        emit()
        for t in tests:
            emit(t)
            emit()
        emit(f":- end_tests({cid}_tests).")
    else:
        emit(f":- begin_tests({cid}_tests).")
        emit(f":- end_tests({cid}_tests).")
    emit()

    # ------------------------------------------------------------------
    # 10. Section 5: Generative commentary
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   5. GENERATIVE COMMENTARY")
    emit("   ========================================================================== */")
    emit()

    has_commentary = any([
        commentary.get("logic_rationale"),
        commentary.get("perspectival_gap"),
        commentary.get("directionality_logic"),
        commentary.get("mandatrophy_analysis"),
    ])

    if has_commentary:
        emit("/**")
        if commentary.get("logic_rationale"):
            emit(" * LOGIC RATIONALE:")
            for line in commentary["logic_rationale"].split("\n"):
                emit(f" *   {line}")
            emit(" *")
        if commentary.get("perspectival_gap"):
            emit(" * PERSPECTIVAL GAP:")
            for line in commentary["perspectival_gap"].split("\n"):
                emit(f" *   {line}")
            emit(" *")
        if commentary.get("directionality_logic"):
            emit(" * DIRECTIONALITY LOGIC:")
            for line in commentary["directionality_logic"].split("\n"):
                emit(f" *   {line}")
            emit(" *")
        if commentary.get("mandatrophy_analysis"):
            emit(" * MANDATROPHY ANALYSIS:")
            for line in commentary["mandatrophy_analysis"].split("\n"):
                emit(f" *   {line}")
        emit(" */")
    emit()

    # ------------------------------------------------------------------
    # 11. Section 6: Omega variables
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   6. OMEGA VARIABLES (\u03a9) - IRREDUCIBLE UNCERTAINTIES")
    emit("   ========================================================================== */")
    emit()

    if omegas:
        for omega in omegas:
            # /5 form (bare)
            emit(f"omega_variable(")
            emit(f"    {omega['id']},")
            emit(f"    '{omega['question']}',")
            emit(f"    '{omega['resolution_mechanism']}',")
            emit(f"    '{omega['impact']}',")
            emit(f"    confidence_without_resolution({omega['confidence']})")
            emit(f").")
            emit()

            # /3 form (namespaced)
            emit(f"narrative_ontology:omega_variable({omega['id']}, {omega['type_class']}, '{omega['description']}').")
            emit()
    emit()

    # ------------------------------------------------------------------
    # 12. Section 7: Integration hooks
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   7. INTEGRATION HOOKS")
    emit("   ========================================================================== */")
    emit()
    emit(f"narrative_ontology:interval({cid}, {interval['start']}, {interval['end']}).")
    emit()

    # ------------------------------------------------------------------
    # 13. Section 8: Temporal measurements
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)")
    emit("   ========================================================================== */")
    emit()

    if measurements:
        # Group by metric for comments
        tr_measurements = [m for m in measurements if m["metric"] == "theater_ratio"]
        be_measurements = [m for m in measurements if m["metric"] == "base_extractiveness"]

        if tr_measurements:
            emit("% Theater ratio over time")
            for m in tr_measurements:
                mid = m.get("id_override", _measurement_id(meas_prefix, m["metric"], m["time_point"]))
                emit(f"narrative_ontology:measurement({mid}, {cid}, theater_ratio, {m['time_point']}, {m['value']}).")
            emit()

        if be_measurements:
            emit("% Extraction over time")
            for m in be_measurements:
                mid = m.get("id_override", _measurement_id(meas_prefix, m["metric"], m["time_point"]))
                emit(f"narrative_ontology:measurement({mid}, {cid}, base_extractiveness, {m['time_point']}, {m['value']}).")
            emit()
    emit()

    # ------------------------------------------------------------------
    # 14. Section 9: Boltzmann & network data
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   9. BOLTZMANN & NETWORK DATA")
    emit("   ========================================================================== */")
    emit()

    if boltzmann.get("coordination_type"):
        emit(f"narrative_ontology:coordination_type({cid}, {boltzmann['coordination_type']}).")
    if boltzmann.get("boltzmann_floor_override") is not None:
        emit(f"narrative_ontology:boltzmann_floor_override({cid}, {boltzmann['boltzmann_floor_override']}).")

    if network.get("affects_constraints"):
        for target in network["affects_constraints"]:
            emit(f"narrative_ontology:affects_constraint({cid}, {target}).")

    if network.get("dual_formulation_note"):
        emit()
        emit("% DUAL FORMULATION NOTE:")
        for line in network["dual_formulation_note"].split("\n"):
            emit(f"% {line}")

    emit()

    # ------------------------------------------------------------------
    # 15. Section 10: Directionality overrides
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)")
    emit("   ========================================================================== */")
    emit()

    if dir_overrides:
        for ovr in dir_overrides:
            emit(f"constraint_indexing:directionality_override({cid}, {ovr['power_atom']}, {ovr['d_value']}).")
        emit()

    # ------------------------------------------------------------------
    # 16. END marker
    # ------------------------------------------------------------------
    emit("/* ==========================================================================")
    emit("   END OF CONSTRAINT STORY")
    emit("   ========================================================================== */")

    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    import argparse
    parser = argparse.ArgumentParser(
        description="Generate a v6.0 constraint story .pl file from JSON"
    )
    parser.add_argument("input", help="Path to the JSON constraint story file")
    parser.add_argument("--validate-only", action="store_true",
                        help="Validate the JSON and exit (no .pl output)")
    parser.add_argument("--no-validate", action="store_true",
                        help="Skip schema validation before generating")
    args = parser.parse_args()

    with open(args.input, "r", encoding="utf-8") as f:
        data = json.load(f)

    if not args.no_validate:
        errors = validate_json(data)
        if errors:
            print("Validation errors:", file=sys.stderr)
            for e in errors:
                print(f"  - {e}", file=sys.stderr)
            sys.exit(1)
        if args.validate_only:
            print("Valid.", file=sys.stderr)
            sys.exit(0)
    elif args.validate_only:
        print("--validate-only and --no-validate are mutually exclusive", file=sys.stderr)
        sys.exit(1)

    output = generate_pl(data)
    sys.stdout.write(output)


if __name__ == "__main__":
    main()
