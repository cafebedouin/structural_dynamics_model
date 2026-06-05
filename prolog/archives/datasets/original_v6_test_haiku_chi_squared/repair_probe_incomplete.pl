% ============================================================================
% CONSTRAINT STORY: repair_probe_incomplete
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_repair_probe_incomplete, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: repair_probe_incomplete
 *   human_readable: Data Repair Bridge Probe (Deliberately Incomplete)
 *   domain: investigation/testing
 *
 * SUMMARY:
 *   The Data Repair Bridge Probe is a deliberately incomplete testset
 *   designed to trigger data_repair.pl bridge paths and validate their
 *   behavior under partial state specifications. The constraint exhibits
 *   tension between the diagnostic value of incomplete probe states
 *   (isolating specific bridge behavior) and the epistemic risk of leaving
 *   untested paths. The incompleteness is intentional — designed to force the
 *   data_repair system to handle gaps and interpolate missing state data —
 *   but this intentionality masks an extraction cost: unvalidated bridge
 *   paths remain untested, and the data integrity commons bears the cost of
 *   potential failures. The constraint's theater_ratio (0.55) reflects
 *   moderate performativity in the diagnostic process — the probe state looks
 *   like a complete test but is deliberately crippled, creating a gap between
 *   testing appearance and actual coverage.
 *
 * KEY AGENTS:
 *   - Testing Framework: Primary beneficiary (institutional/arbitrage) — gains diagnostic capability and bridge path isolation without maintaining complete state specifications
 *   - Data Integrity Commons: Primary victim (powerless/trapped) — bears cost of untested bridge paths and incomplete probe coverage
 *   - Analytical Observer: Observes structure (analytical/analytical) — assesses whether incomplete probe is intentional coordination or accidental extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(repair_probe_incomplete, 0.38).
domain_priors:suppression_score(repair_probe_incomplete, 0.42).
domain_priors:theater_ratio(repair_probe_incomplete, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(repair_probe_incomplete, extractiveness, 0.38).
narrative_ontology:constraint_metric(repair_probe_incomplete, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(repair_probe_incomplete, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(repair_probe_incomplete, tangled_rope).
narrative_ontology:human_readable(repair_probe_incomplete, "Data Repair Bridge Probe (Deliberately Incomplete)").
narrative_ontology:topic_domain(repair_probe_incomplete, "investigation/testing").

domain_priors:requires_active_enforcement(repair_probe_incomplete).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(repair_probe_incomplete, testing_framework).
narrative_ontology:constraint_victim(repair_probe_incomplete, data_integrity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA INTEGRITY COMMONS (SNARE) — Cannot exit incomplete probe states; bears full cost of bridge path failures. No self-correction mechanism. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.51.
constraint_indexing:constraint_classification(repair_probe_incomplete, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TESTING FRAMEWORK (ROPE) — Experiences incomplete probe as coordination mechanism for isolating bridge path behavior. Benefits from diagnostic capability. d≈0.10, f(d)≈0.02, σ=0.8 → χ≈0.003. Effective arbitrage position.
constraint_indexing:constraint_classification(repair_probe_incomplete, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the intentional incompleteness (coordination function: diagnostic isolation) and the extraction cost (data_repair.pl bridge paths remain unvalidated). d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.53.
constraint_indexing:constraint_classification(repair_probe_incomplete, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(repair_probe_incomplete_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(repair_probe_incomplete, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(repair_probe_incomplete, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(repair_probe_incomplete_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The incompleteness is partly intentional (diagnostic value) but creates unvalidated paths. The testing framework benefits from simplified probe states while the integrity commons bears hidden risks. Suppression (0.42): Moderate. No explicit prohibition against complete probes, but resource constraints and intentional design choices suppress comprehensive testing. Theater ratio (0.55): Moderate-high. Incomplete probes appear to be valid tests but deliberately omit coverage, creating performative testing appearance.
 *
 * PERSPECTIVAL GAP:
 *   The testing framework sees coordination — a mechanism for isolating bridge behavior. The data integrity commons sees extraction — untested paths that could fail. The analytical observer sees both: intentional incompleteness is coordination if it successfully isolates critical behavior; it is extraction if it leaves critical paths unvalidated.
 *
 * DIRECTIONALITY LOGIC:
 *   Testing framework: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Net beneficiary. Data integrity commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Observes both coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing intentional diagnostic incompleteness (coordination/tangled rope) from accidental coverage gaps (extraction/snare). The key discriminant is whether the incompleteness was designed to isolate bridge behavior or merely failed to complete implementation. If intentional, the constraint is Tangled Rope (coordination function + enforcement requirements). If accidental, it degrades to Snare (pure extraction without coordination value). The omegas capture this distinction: resolution of omega_intentional_degradation_scope determines final classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bridge_path_completeness,
    'Does the deliberately incomplete probe state expose all necessary bridge path behavior, or do hidden branches remain untested?',
    'Code coverage analysis of data_repair.pl bridge clauses; execution trace comparison against complete state specifications',
    'If complete: diagnostic isolation works as intended (Rope). If incomplete: critical failure paths remain untested (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridge_path_completeness, empirical, 'Whether incomplete probe state covers all bridge paths').

omega_variable(
    intentional_degradation_scope,
    'Is the incompleteness truly intentional (for diagnostic isolation) or accidental (unfinished implementation)?',
    'Code review, commit history, design documentation; comparison with test plan coverage matrix',
    'If intentional: classification is Tangled Rope (coordination + enforcement). If accidental: classification degrades to Snare (pure extraction without coordination value).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_degradation_scope, conceptual, 'Whether incompleteness is intentional diagnostic strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(repair_probe_incomplete, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repair_tr_t0, repair_probe_incomplete, theater_ratio, 0, 0.35).
narrative_ontology:measurement(repair_tr_t2, repair_probe_incomplete, theater_ratio, 2, 0.45).
narrative_ontology:measurement(repair_tr_t4, repair_probe_incomplete, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(repair_be_t0, repair_probe_incomplete, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(repair_be_t2, repair_probe_incomplete, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(repair_be_t4, repair_probe_incomplete, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(repair_probe_incomplete, enforcement_mechanism).
narrative_ontology:affects_constraint(repair_probe_incomplete, data_repair_bridge_execution).
narrative_ontology:affects_constraint(repair_probe_incomplete, incomplete_state_specification).

% DUAL FORMULATION NOTE:
% This probe is a deliberate testset component, not an independent constraint. It represents the interaction between incomplete state specifications and data_repair bridge path execution. The upstream constraints are the bridge clauses themselves; this probe tests their behavior under partial state conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
