% ============================================================================
% CONSTRAINT STORY: couples_residency_match
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_couples_residency_match, []).

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
 * * constraint_id: couples_residency_match
 * human_readable: The Medical Residency Couples Match Algorithm
 * domain: technological/economic
 * * SUMMARY:
 * The "Couples Match" is a specialized module of the National Resident Matching Program (NRMP) that allows two medical students to link their rank order lists (ROLs). To maintain stability, the algorithm must find a pair of programs that is stable for BOTH partners simultaneously. This significantly increases computational complexity and restricts the available "solution space," forcing couples to often sacrifice individual prestige for geographic proximity.
 * * KEY AGENTS:
 * - The Couple: Two agents acting as a single unit with linked preferences (Subject).
 * - Regional Hospital Systems: Institutions that benefit from attracting and retaining talent that might otherwise leave the area (Beneficiary).
 * - The NRMP/Algorithm Designers: The analytical observer defining the rules of the market.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(couples_residency_match, 0.5). % The system extracts a "choice penalty." To guarantee being together, couples often match at lower-ranked programs than they would individually.
domain_priors:suppression_score(couples_residency_match, 0.95).   % Structural property (raw, unscaled). Like the standard match, side-deals are strictly prohibited and algorithm participation is mandatory.
domain_priors:theater_ratio(couples_residency_match, 0.1).       % The system is highly functional and not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(couples_residency_match, extractiveness, 0.5).
narrative_ontology:constraint_metric(couples_residency_match, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(couples_residency_match, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a pure coordination tool to solve a social problem.
narrative_ontology:constraint_claim(couples_residency_match, tangled_rope).
narrative_ontology:human_readable(couples_residency_match, "The Medical Residency Couples Match Algorithm").
narrative_ontology:topic_domain(couples_residency_match, "technological/economic").

% Binary flags
domain_priors:requires_active_enforcement(couples_residency_match). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(couples_residency_match, regional_hospital_systems).
narrative_ontology:constraint_victim(couples_residency_match, high_achieving_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The couple experiences the constraint as a "Snare" because the number
% of valid stable matches drops exponentially when ROLs are linked.
% The binding nature of the contract means they might be forced into
% their 50th-ranked pair of programs simply because it's the only
% one that works for both, a "voluntary" trap.
constraint_indexing:constraint_classification(couples_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Program directors and regional hospital systems see the Couples Match as a "Rope."
% It helps them recruit and retain high-quality candidates who might otherwise
% go to different cities. It coordinates the social needs of residents with the
% labor needs of the hospital system.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system is a textbook Tangled Rope. It has a genuine coordination function
% (helping couples stay together), but this comes at the cost of asymmetric
% extraction (high-achieving partners sacrifice rank for joint stability) and
% requires high suppression (enforced matching rules) to function.
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(couples_residency_match_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(couples_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(couples_residency_match, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical view correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(couples_residency_match, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(couples_residency_match, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(couples_residency_match, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(couples_residency_match).

:- end_tests(couples_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the tension between its stated goal (coordination)
 * and its emergent effect (extraction). The base extractiveness of 0.5 reflects
 * the significant "prestige penalty" that couples pay to match together. The
 * suppression score of 0.95 is inherited from the NRMP itself, which is a
 * highly rigid, non-negotiable market mechanism.
 *
 * The Perspectival Gap is stark:
 * - For the couple (powerless, trapped), the algorithm is a Snare. They enter
 *   it voluntarily to solve a social problem, but the mathematical reality
 *   severely constrains their outcomes, extracting value (their top choices)
 *   in exchange for geographic stability.
 * - For the hospital system (institutional, mobile), it's a Rope. It's a tool
 *   that helps them solve a recruitment problem, attracting pairs of doctors
 *   who become stable, long-term employees.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * An early analysis might misclassify this as a Mountain, citing the NP-hard
 * complexity of the stable matching problem with couples. This is a classic
 * "false natural law" error. The complexity is a property of the *chosen solution*,
 * not an immutable law of nature. The system is a human-constructed market.
 * Classifying it as a Tangled Rope correctly identifies that it has both a
 * legitimate coordination function and a significant, asymmetrically applied
 * extractive component, preventing the mislabeling of its coercive aspects
 * as mere mathematical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_couples_residency_match,
    'To what extent is the "prestige penalty" a result of mathematical necessity versus a lack of geographic density in high-tier programs?',
    'Analysis of match data, comparing outcomes in dense urban areas (e.g., NYC, Boston) vs. sparse regions.',
    'If due to density, the extraction is a geographic Mountain. If due to math, the algorithm itself is the Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(couples_residency_match, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the increasing
% competitiveness and formalization of the match over time, leading to a
% gradual increase in the extractive penalty. Theater remains low as the
% system is purely functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(crm_tr_t0, couples_residency_match, theater_ratio, 0, 0.05).
narrative_ontology:measurement(crm_tr_t5, couples_residency_match, theater_ratio, 5, 0.08).
narrative_ontology:measurement(crm_tr_t10, couples_residency_match, theater_ratio, 10, 0.1).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(crm_ex_t0, couples_residency_match, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(crm_ex_t5, couples_residency_match, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(crm_ex_t10, couples_residency_match, base_extractiveness, 10, 0.5).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a classic resource allocation problem: assigning candidates to positions.
narrative_ontology:coordination_type(couples_residency_match, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */