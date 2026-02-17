% ============================================================================
% CONSTRAINT STORY: silent_dependency_activation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_silent_dependency_activation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: silent_dependency_activation
 * human_readable: The Invisible Supply Chain Trap
 * domain: technological/economic
 * * SUMMARY:
 * This constraint occurs when a system relies on a hidden, low-level component
 * that remains invisible until a change in market conditions or policy
 * activates it as a critical bottleneck. Once activated, it transforms from
 * a background utility (Rope) into a high-extraction leverage point (Snare)
 * for whoever controls the dependency.
 * * KEY AGENTS:
 * - Downstream Manufacturer: Subject (Powerless)
 * - IP/Patent Holder: Beneficiary (Institutional)
 * - Supply Chain Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) because the silent nature of the dependency
% allows for the retroactive capture of entire industry margins.
domain_priors:base_extractiveness(silent_dependency_activation, 0.76).
domain_priors:suppression_score(silent_dependency_activation, 0.82). % Alternatives are often legally or technically impossible once locked in.
domain_priors:theater_ratio(silent_dependency_activation, 0.15).    % Low theater; the extraction is a direct mechanical/legal consequence.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(silent_dependency_activation, extractiveness, 0.76).
narrative_ontology:constraint_metric(silent_dependency_activation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(silent_dependency_activation, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The IP holder claims the dependency is for coordination and quality control.
narrative_ontology:constraint_claim(silent_dependency_activation, tangled_rope).
narrative_ontology:human_readable(silent_dependency_activation, "The Invisible Supply Chain Trap").

% Binary flags
% This is a Tangled Rope, requiring active enforcement (e.g., patent law).
domain_priors:requires_active_enforcement(silent_dependency_activation).

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(silent_dependency_activation, ip_patent_holder).
narrative_ontology:constraint_victim(silent_dependency_activation, downstream_manufacturer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the manufacturer, the activation of the dependency is a pure extraction trap.
constraint_indexing:constraint_classification(silent_dependency_activation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The IP holder views the dependency as a Rope for ensuring quality and coordination across the stack.
constraint_indexing:constraint_classification(silent_dependency_activation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of standard-setting coordination vs predatory rent-seeking.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silent_dependency_activation_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(silent_dependency_activation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silent_dependency_activation, ExtMetricName, E),
    (E =< 0.15 -> fail ; E >= 0.46). % This is a high-extraction constraint.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(silent_dependency_activation),
    narrative_ontology:constraint_beneficiary(silent_dependency_activation, _),
    narrative_ontology:constraint_victim(silent_dependency_activation, _).

:- end_tests(silent_dependency_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects the 'Mandatrophy' state where
 * the hidden nature of the dependency prevents agents from pricing in risk.
 * When the dependency is activated, the controller can capture immense value.
 * Suppression (0.82) is high due to legal (patent) and technical lock-in.
 * * PERSPECTIVAL GAP:
 * The Downstream Manufacturer feels a Snare because their sunk costs are
 * effectively held hostage. The IP Holder sees a Rope as their component
 * provides the "invisible" glue for the ecosystem.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This is resolved by the Tangled Rope classification.
 * The system avoids mislabeling this as a pure Snare because the component
 * performs a genuine coordination function (the system literally would not
 * work without it), which is captured by declaring a `constraint_beneficiary`.
 * However, the asymmetric extraction (captured by `constraint_victim`) and
 * the need for `requires_active_enforcement` confirm its hybrid, extractive nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_dependency_intent,
    'Was the dependency made silent by design (Snare) or by technological evolution (Mountain)?',
    'Forensic audit of technical standards documentation and private correspondence.',
    'If intentional: Snare. If evolutionary: Mountain of complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(silent_dependency_activation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint starts as a low-extraction utility and becomes highly
% extractive as its strategic importance is realized and monetized.
% Theater remains low as the extraction is direct and mechanical.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(sda_tr_t0, silent_dependency_activation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sda_tr_t5, silent_dependency_activation, theater_ratio, 5, 0.15).
narrative_ontology:measurement(sda_tr_t10, silent_dependency_activation, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(sda_ex_t0, silent_dependency_activation, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(sda_ex_t5, silent_dependency_activation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sda_ex_t10, silent_dependency_activation, base_extractiveness, 10, 0.76).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The dependency acts as a de facto technical standard.
narrative_ontology:coordination_type(silent_dependency_activation, information_standard).

% This dependency is a key component in a larger system, so its activation
% structurally affects the stability of the entire supply chain.
narrative_ontology:affects_constraint(silent_dependency_activation, semiconductor_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */