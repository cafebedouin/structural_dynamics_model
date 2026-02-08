% ============================================================================
% CONSTRAINT STORY: capital_misallocation_spiral
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_capital_misallocation_spiral, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: capital_misallocation_spiral
 * human_readable: The Zombie Asset Loop
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where low interest rates or institutional mandates force capital
 * into increasingly unproductive "zombie" assets. To prevent a crash, even
 * more capital is allocated to prop up these failures, creating a recursive
 * siphon. It functions as a Rope for short-term market stability but becomes
 * a massive Snare for future generations who inherit a hollowed-out economy.
 * * KEY AGENTS:
 * - Emerging Innovator: Subject (Powerless)
 * - Central Financial Authority: Beneficiary (Institutional)
 * - Forensic Macroeconomist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) because the spiral siphons the species' real
% productive potential to maintain the "value" of non-functional spreadsheets.
domain_priors:base_extractiveness(capital_misallocation_spiral, 0.87).
domain_priors:suppression_score(capital_misallocation_spiral, 0.74).
domain_priors:theater_ratio(capital_misallocation_spiral, 0.89). % Extreme theater: the "market" is a performative clearinghouse.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(capital_misallocation_spiral, extractiveness, 0.87).
narrative_ontology:constraint_metric(capital_misallocation_spiral, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(capital_misallocation_spiral, theater_ratio, 0.89).

% The constraint claims to be a coordination mechanism for market stability.
narrative_ontology:constraint_claim(capital_misallocation_spiral, tangled_rope).

% Binary flags & Structural properties
domain_priors:requires_active_enforcement(capital_misallocation_spiral). % Required for Tangled Rope. Capital is forced via mandates.
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, central_financial_authority). % Derives has_coordination_function
narrative_ontology:constraint_victim(capital_misallocation_spiral, emerging_innovator). % Derives has_asymmetric_extraction

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the innovator, the spiral is a snare: capital is technically
% abundant, but economically trapped in legacy propping, leaving them starved.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the intervention as a vital Rope—the only way to
% coordinate market order and prevent systemic panic.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of high coordination intent (Rope)
% masking predatory, recursive extraction (Snare).
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: price signals have atrophied;
% the system is an inert spike of logic propped up by performative valuation.
constraint_indexing:constraint_classification(capital_misallocation_spiral, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_misallocation_spiral_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.89) triggers Piton classification.
    constraint_indexing:constraint_classification(capital_misallocation_spiral, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.87) is above the Snare threshold (>= 0.46).
    domain_priors:base_extractiveness(capital_misallocation_spiral, E),
    E >= 0.46.

:- end_tests(capital_misallocation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the
 * "market" provides zero price-signal coordination relative to real utility.
 * The system is both a Tangled Rope (it has a real coordination function for
 * beneficiaries and asymmetric extraction for victims) and a Piton (its
 * primary function has atrophied into theatrical price-setting). This dual
 * classification is valid and represents a late-stage, degraded constraint.
 *
 * * PERSPECTIVAL GAP:
 * The Emerging Innovator feels a Snare because they are effectively
 * de-capitalized by institutional "stability" measures. The Financial
 * Authority sees a Rope because the propping ensures no immediate collapse.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an
 * analytical observer, the coordination is no longer functional
 * (Theater 0.89); the system is an inert spike that siphons the
 * future's wealth to pay for the past's mistakes. The temporal data shows
 * this was not the initial state, but a degradation over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_signal_recovery,
    'Can price signals be restored without total systemic failure (Snare vs Mountain)?',
    'Tracking the delta between "shadow" market prices and institutional clearing prices.',
    'If signals recover: Snare of policy. If collapse is required: Mountain of Entropic Debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(capital_misallocation_spiral, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a stability mechanism (lower extraction) but
% degraded into a performative, extractive spiral.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cms_tr_t0, capital_misallocation_spiral, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cms_tr_t5, capital_misallocation_spiral, theater_ratio, 5, 0.60).
narrative_ontology:measurement(cms_tr_t10, capital_misallocation_spiral, theater_ratio, 10, 0.89).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cms_ex_t0, capital_misallocation_spiral, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cms_ex_t5, capital_misallocation_spiral, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(cms_ex_t10, capital_misallocation_spiral, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's function is to allocate capital, a core resource.
narrative_ontology:coordination_type(capital_misallocation_spiral, resource_allocation).

% This type of misallocation has downstream effects, particularly on sectors
% like housing that require long-term productive capital investment.
narrative_ontology:affects_constraint(capital_misallocation_spiral, housing_affordability_crisis).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */