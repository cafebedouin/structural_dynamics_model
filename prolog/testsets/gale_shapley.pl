% ============================================================================
% CONSTRAINT STORY: gale_shapley
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_gale_shapley, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: gale_shapley
 * human_readable: Gale-Shapley Stable Matching Algorithm (as applied in markets)
 * domain: economic/technological
 * * SUMMARY:
 * The Gale-Shapley algorithm is used to produce a "stable matching" between two sets of agents,
 * most famously in the National Resident Matching Program (NRMP) for doctors. While it solves
 * the coordination problem of "market unraveling," it does so by centralizing the process and
 * extracting significant agency from one side of the market (the applicants), who cannot
 * negotiate terms or engage in side-bargaining.
 * * KEY AGENTS:
 * - Medical Applicants: Subject (Powerless)
 * - Hospital Systems / NRMP: Beneficiary (Institutional)
 * - Market Design Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(gale_shapley, 0.80). % High extraction of individual agency and negotiation power.
domain_priors:suppression_score(gale_shapley, 0.80).   % Alternatives (side deals, direct negotiation) are strictly prohibited and punished.
domain_priors:theater_ratio(gale_shapley, 0.10).       % The mechanism is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(gale_shapley, extractiveness, 0.80).
narrative_ontology:constraint_metric(gale_shapley, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(gale_shapley, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Proponents frame it as a pure, fair coordination mechanism to prevent chaos.
narrative_ontology:constraint_claim(gale_shapley, tangled_rope).
narrative_ontology:human_readable(gale_shapley, "Gale-Shapley Stable Matching Algorithm (as applied in markets)").
narrative_ontology:topic_domain(gale_shapley, "economic/technological").

% Binary flags
domain_priors:requires_active_enforcement(gale_shapley). % The NRMP actively enforces its rules.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(gale_shapley, hospital_systems).
narrative_ontology:constraint_victim(gale_shapley, medical_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For an applicant, especially one with non-standard preferences (e.g., needing
% to be in a specific city for family reasons), the system is a trap. It removes
% their ability to signal these needs or negotiate, forcing them into a ranked lottery.
constraint_indexing:constraint_classification(gale_shapley, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the hospital system, the algorithm is a pure coordination mechanism (Rope).
% It provides a predictable, stable, and orderly supply of labor, preventing
% chaotic poaching wars and ensuring positions are filled efficiently.
constraint_indexing:constraint_classification(gale_shapley, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the asymmetric extraction.
% It solves a real coordination problem but does so by concentrating power and
% extracting agency. It requires active enforcement and has clear winners and
% losers, making it a canonical Tangled Rope.
constraint_indexing:constraint_classification(gale_shapley, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gale_shapley_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gale_shapley, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gale_shapley, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(gale_shapley, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_threshold) :-
    narrative_ontology:constraint_metric(gale_shapley, extractiveness, E),
    E >= 0.46.

:- end_tests(gale_shapley_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of the Gale-Shapley application. The high
 * extraction (0.8) and suppression (0.8) represent the total loss of bargaining
 * power for applicants and the strict prohibition of alternative matching channels.
 * The Perspectival Gap is stark: for institutions, it's a perfect coordination
 * Rope that solves a chaotic market problem. For applicants, it's a Snare that
 * removes their agency. The analytical view must therefore be Tangled Rope, as
 * the system possesses both a genuine coordination function (beneficiary exists)
 * and severe asymmetric extraction (victim exists), and requires active enforcement.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This is a severe Mandatrophic constraint (E=0.8). The system avoids being
 * misclassified as a pure Snare by acknowledging its genuine coordination role.
 * The Tangled Rope classification correctly identifies that the stability provided
 * to the market (the Rope function) is paid for by the agency extracted from
 * applicants (the Snare function). The resolution is in seeing that the two
 * functions are inseparable parts of the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gale_shapley,
    'Does the market stability benefit applicants enough to offset their loss of agency?',
    'Longitudinal studies comparing career satisfaction and outcomes for participants in matched markets vs. those in markets with decentralized negotiation.',
    'If stability benefit > agency loss, the system leans more towards Rope. If agency loss > stability benefit, it leans more towards Snare for all but the most powerful.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(gale_shapley, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the matching market becoming more entrenched and totalizing over time,
% increasing its extractive power as alternatives fade from memory and institutional
% reliance grows.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(gale_shapley_tr_t0, gale_shapley, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gale_shapley_tr_t5, gale_shapley, theater_ratio, 5, 0.08).
narrative_ontology:measurement(gale_shapley_tr_t10, gale_shapley, theater_ratio, 10, 0.10).

% Extraction over time (increases as mechanism becomes mandatory):
narrative_ontology:measurement(gale_shapley_ex_t0, gale_shapley, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(gale_shapley_ex_t5, gale_shapley, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(gale_shapley_ex_t10, gale_shapley, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic resource allocation problem: matching workers to positions.
narrative_ontology:coordination_type(gale_shapley, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */