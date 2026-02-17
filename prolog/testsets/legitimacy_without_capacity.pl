% ============================================================================
% CONSTRAINT STORY: legitimacy_without_capacity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_legitimacy_without_capacity, []).

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
 * * constraint_id: legitimacy_without_capacity
 * human_readable: The Sovereign Ghost
 * domain: political/organizational
 * * SUMMARY:
 * A scenario where an institution retains the social and legal "right to rule"
 * (legitimacy) but has lost the actual ability to provide services, security,
 * or order (capacity). This "Rope" of historical loyalty becomes a "Snare"
 * as the subject is legally bound to a defunct provider, liquidating their
 * safety while being barred from seeking alternative, non-state coordination.
 * * KEY AGENTS:
 * - Local Resident: Subject (Powerless)
 * - Legacy Bureaucrat: Beneficiary (Institutional)
 * - Institutional Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(legitimacy_without_capacity, 0.84). % High extraction because the institution continues to extract taxes and compliance without returning functional utility.
domain_priors:suppression_score(legitimacy_without_capacity, 0.71).   % Structural property (raw, unscaled). Alternatives are legally suppressed.
domain_priors:theater_ratio(legitimacy_without_capacity, 0.91).       % Extreme theater: symbols of statehood masking operational void.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(legitimacy_without_capacity, extractiveness, 0.84).
narrative_ontology:constraint_metric(legitimacy_without_capacity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimacy_without_capacity, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(legitimacy_without_capacity, tangled_rope).
narrative_ontology:human_readable(legitimacy_without_capacity, "The Sovereign Ghost").

% Binary flags
domain_priors:requires_active_enforcement(legitimacy_without_capacity). % Required for Tangled Rope. The state legally enforces its monopoly.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, legacy_bureaucracy). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(legitimacy_without_capacity, local_residents).       % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they are legally prohibited from forming
% self-defense or service groups because the "State" still claims a monopoly.
constraint_indexing:constraint_classification(legitimacy_without_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views its legitimacy as a Rope—the only coordination tool
% remaining to prevent total societal fragmentation, despite current failure.
constraint_indexing:constraint_classification(legitimacy_without_capacity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and suppression (0.71) masking as coordination.
% The presence of beneficiaries, victims, and active enforcement confirms Tangled Rope.
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Sovereignty" is a
% non-functional, performative artifact maintained by cultural inertia.
constraint_indexing:constraint_classification(legitimacy_without_capacity, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_capacity_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(legitimacy_without_capacity, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_capacity, rope,
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) triggers Piton classification.
    constraint_indexing:constraint_classification(legitimacy_without_capacity, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the necessary structural properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(legitimacy_without_capacity),
    narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, _),
    narrative_ontology:constraint_victim(legitimacy_without_capacity, _).

:- end_tests(legitimacy_without_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the
 * "coordination" is a parasitic liquidation of the subject's remaining capital.
 * The high suppression (0.71) represents the legal prohibition of alternatives.
 * The extreme theater ratio (0.91) shows that almost all state activity is
 * performative rather than functional.
 *
 * * PERSPECTIVAL GAP:
 * The Local Resident feels a Snare because they are taxed for security that
 * never arrives. The Bureaucrat sees a Rope because the "myth" of the state
 * is the only thing preventing an immediate descent into a Mountain of chaos.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical
 * observer, the "State" is no longer functional (Theater 0.91 -> Piton); it is an
 * inert spike siphoning 0.84 of the subject's agency to feed a legacy model.
 * The combination of a coordination claim, asymmetric extraction, and active
 * enforcement makes it a canonical Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_sovereign_reconstitution,
    'Can legitimacy be re-tethered to capacity, or is the divorce final (Snare vs Mountain)?',
    'Tracking the success rate of local "self-help" groups in regaining state recognition.',
    'If recognized: Tangled Rope/Snare of current policy. If crushed/ignored: Mountain of Institutional Death.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(legitimacy_without_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the state's decay: extraction remains high while theater
% replaces function. Required for base_extractiveness > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(lwc_tr_t0, legitimacy_without_capacity, theater_ratio, 0, 0.10).
narrative_ontology:measurement(lwc_tr_t5, legitimacy_without_capacity, theater_ratio, 5, 0.60).
narrative_ontology:measurement(lwc_tr_t10, legitimacy_without_capacity, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(lwc_ex_t0, legitimacy_without_capacity, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(lwc_ex_t5, legitimacy_without_capacity, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(lwc_ex_t10, legitimacy_without_capacity, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The state's claim to legitimacy is based on its role as the ultimate
% arbiter of order and security.
narrative_ontology:coordination_type(legitimacy_without_capacity, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */