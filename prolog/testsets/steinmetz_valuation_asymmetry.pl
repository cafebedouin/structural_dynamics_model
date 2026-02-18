% ============================================================================
% CONSTRAINT STORY: steinmetz_valuation_asymmetry
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_steinmetz_valuation_asymmetry, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: steinmetz_valuation_asymmetry
 * human_readable: The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)
 * domain: economic/technological
 * * SUMMARY:
 * This constraint models the extreme asymmetry between the value of physical labor and the value of specialized knowledge, as illustrated by the apocryphal story of Charles Proteus Steinmetz and Henry Ford. Steinmetz solves a critical problem with a "gigantic generator" by making a single chalk mark, then submits an invoice for $10,000: $1 for the chalk mark, and $9,999 for "knowing where to mark."
 * * KEY AGENTS:
 * - Ford's Engineers: Subject (Powerless), unable to solve the problem.
 * - Henry Ford: Payer (Institutional), trapped by operational failure.
 * - Charles Steinmetz: Beneficiary (Powerful), the expert with unique knowledge.
 * - Analytical Observer: Auditor (Analytical), assessing the entire system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(steinmetz_valuation_asymmetry, 0.90). % The $9,999 fee for knowledge is almost total extraction relative to the $1 physical act.
domain_priors:suppression_score(steinmetz_valuation_asymmetry, 0.50).   % Alternatives (internal engineers) existed but were ineffective, making Steinmetz's solution the only viable one.
domain_priors:theater_ratio(steinmetz_valuation_asymmetry, 0.05).       % The action is purely functional; there is no performative aspect.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, extractiveness, 0.90).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(steinmetz_valuation_asymmetry, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(steinmetz_valuation_asymmetry, tangled_rope).
narrative_ontology:human_readable(steinmetz_valuation_asymmetry, "The Steinmetz Chalk Mark (Knowledge Valuation Asymmetry)").
narrative_ontology:topic_domain(steinmetz_valuation_asymmetry, "economic/technological").

% Binary flags
domain_priors:requires_active_enforcement(steinmetz_valuation_asymmetry). % The value is only realized via the itemized invoice.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, expert_class). % Steinmetz and the value of expertise.
narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, capital_owner).   % Ford, who must pay to restore operations.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENGINEERS (SUBJECT)
% For the engineers unable to solve the problem, it was an immutable law of physics.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HENRY FORD (THE PAYER)
% For the institutional actor trapped by the failure, the non-negotiable, high-cost solution is a snare.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CHARLES STEINMETZ (THE BENEFICIARY)
% For the expert, the fee is a pure coordination mechanism to value unique knowledge fairly.
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The observer sees both the genuine coordination (generator fixed) and the asymmetric extraction (the fee).
constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(steinmetz_valuation_asymmetry_tests).

test(perspectival_gap_subject_vs_payer) :-
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == snare,
    TypePowerless \= TypeInstitutional.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(steinmetz_valuation_asymmetry, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(steinmetz_valuation_asymmetry, _),
    narrative_ontology:constraint_victim(steinmetz_valuation_asymmetry, _),
    domain_priors:requires_active_enforcement(steinmetz_valuation_asymmetry).

:- end_tests(steinmetz_valuation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Steinmetz Chalk Mark story is a canonical example of a perspectival gap in economic valuation.
 * The base extractiveness is set to 0.90, reflecting the 9,999% markup over the physical act of marking with chalk ($1).
 * Suppression is moderate (0.50) because while alternatives (Ford's internal engineers) existed, they were ineffective, making Steinmetz's expertise the only viable option.
 *
 * The perspectival classifications are stark:
 * - For the powerless engineers, the problem was an insurmountable 'Mountain'.
 * - For the institutional but trapped Henry Ford, the massive, non-negotiable invoice was a 'Snare'.
 * - For the powerful expert Steinmetz, the fee was a 'Rope', a fair coordination mechanism to price unique, high-value knowledge.
 * - The analytical observer, seeing all facets, classifies it as a 'Tangled Rope'. This acknowledges both the genuine coordination function (the generator is fixed, value is restored) and the highly asymmetric extraction required to achieve it.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The extremely high extraction (0.90) risks a misclassification of this event as pure predation (Snare). The Tangled Rope classification is the resolution mechanism. It correctly identifies that this is not a simple coercive trap but a hybrid constraint with a legitimate, if expensive, coordination component. The system avoids mandatrophy by recognizing that Ford *did* receive immense value, while also acknowledging the extractive nature of the price paid for that value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_steinmetz_valuation,
    'Was the $10,000 fee a calculated predatory price based on Ford''s desperation, or a standard valuation for unique expertise from GE at the time?',
    'Historical audit of GE''s consulting fees for similar high-stakes industrial problems in the 1920s.',
    'If predatory, it leans more towards Snare. If standard, it reinforces the Tangled Rope classification as a market-based coordination mechanism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(steinmetz_valuation_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a singular event, so the metrics are stable across the interval.
% Theater ratio over time:
narrative_ontology:measurement(sva_tr_t0, steinmetz_valuation_asymmetry, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sva_tr_t5, steinmetz_valuation_asymmetry, theater_ratio, 5, 0.05).
narrative_ontology:measurement(sva_tr_t10, steinmetz_valuation_asymmetry, theater_ratio, 10, 0.05).

% Extraction over time:
narrative_ontology:measurement(sva_ex_t0, steinmetz_valuation_asymmetry, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(sva_ex_t5, steinmetz_valuation_asymmetry, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(sva_ex_t10, steinmetz_valuation_asymmetry, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint allocates a scarce resource (Steinmetz's knowledge) to a critical failure point.
narrative_ontology:coordination_type(steinmetz_valuation_asymmetry, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */