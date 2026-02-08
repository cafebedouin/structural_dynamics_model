% ============================================================================
% CONSTRAINT STORY: scam_compound_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_scam_compound_2026, []).

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
 * * constraint_id: scam_compound_2026
 * human_readable: The Southeast Asian Scam Compound "Grey Zone"
 * domain: social/humanitarian/criminal
 * * SUMMARY:
 * In fortified enclaves, the distinction between victims of modern slavery
 * and criminal perpetrators has collapsed. Workers are trafficked (subjects)
 * but compelled to refine and execute online scams, creating a "victim-offender
 * overlap" that defies standard legal "Ideal Victim" categories. This structure
 * extracts value through coercion while suppressing escape or legal recourse.
 * * KEY AGENTS:
 * - Trafficked Migrant Labor: Subject (Powerless)
 * - Organized Crime Syndicates: Beneficiary (Institutional)
 * - Humanitarian/Legal Analysts: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(scam_compound_2026, 0.85). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(scam_compound_2026, 0.90).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(scam_compound_2026, 0.65).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(scam_compound_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(scam_compound_2026, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(scam_compound_2026, theater_ratio, 0.65).

% Constraint self-claim (what does the constraint claim to be?)
% The operators present it as a legitimate, if aggressive, business. The
% underlying mechanism is pure coercive enforcement.
narrative_ontology:constraint_claim(scam_compound_2026, snare).

% Binary flags
domain_priors:requires_active_enforcement(scam_compound_2026). % Physical confinement, threats, and violence are central.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(scam_compound_2026, organized_crime_syndicates).
narrative_ontology:constraint_victim(scam_compound_2026, trafficked_migrant_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE WORKER (SNARE)
% Experiences total coercion, high extraction of labor, and no viable exit.
% The structure is a pure trap.
constraint_indexing:constraint_classification(scam_compound_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE OPERATOR (ROPE)
% Views the compound as a highly efficient, coordinated business model for
% profit generation. The coercive elements are seen as necessary "management".
constraint_indexing:constraint_classification(scam_compound_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analytical view confirms the powerless perspective. The high extraction
% and suppression scores, combined with active enforcement, define a Snare.
% The "coordination" function serves only to enable extraction.
constraint_indexing:constraint_classification(scam_compound_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_compound_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the worker and the operator.
    constraint_indexing:constraint_classification(scam_compound_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_compound_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_snare) :-
    % Verify the analytical observer correctly identifies the structure as a snare.
    constraint_indexing:constraint_classification(scam_compound_2026, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == snare).

test(threshold_validation_snare) :-
    % Verify the base metrics meet the criteria for a Snare.
    domain_priors:base_extractiveness(scam_compound_2026, E),
    domain_priors:suppression_score(scam_compound_2026, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(scam_compound_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system of modern slavery.
 * - Base Extractiveness (0.85): Extremely high, representing the total capture
 *   of labor value from trafficked individuals under threat of violence.
 * - Suppression Score (0.90): Represents physical confinement in fortified
 *   compounds, passport confiscation, and the elimination of legal or physical
 *   escape routes. The inability for victims to organize is a key factor.
 * - Theater Ratio (0.65): High but below the Piton threshold. The "theater"
 *   involves creating fake online personas and elaborate scam narratives, which
 *   is functional to the goal of extraction, not just performative.
 *
 * The Perspectival Gap is stark: for the trapped worker, it is an inescapable
 * Snare. For the crime syndicate, it is a profitable coordination mechanism (Rope)
 * for deploying labor towards a financial goal. The analytical view sides with
 * the worker, classifying it as a Snare due to the overwhelming coercion.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.85) triggers mandatrophy analysis. This constraint
 * is not a Tangled Rope because its coordination function has no redeeming
 * social value or legitimate beneficiaries outside the extracting group. It is
 * pure coercion for asymmetric gain. Classifying it as a Snare correctly
 * identifies its primary nature and prevents the system from legitimizing
 * the operators' "coordination" claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scam_compound_2026,
    'To what extent does the "victim-offender overlap" create a stable internal hierarchy versus being a pure instrument of control?',
    'Analysis of escapee testimonies and internal communication logs to determine if coerced workers gain meaningful internal status or are uniformly exploited.',
    'If a stable hierarchy exists, it might suggest a more complex Tangled Rope structure. If not, it confirms the pure Snare model.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(scam_compound_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the evolution of these compounds from nascent, less
% efficient operations into highly refined systems of extraction.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (scam techniques become more sophisticated):
narrative_ontology:measurement(scam_compound_2026_tr_t0, scam_compound_2026, theater_ratio, 0, 0.20).
narrative_ontology:measurement(scam_compound_2026_tr_t5, scam_compound_2026, theater_ratio, 5, 0.45).
narrative_ontology:measurement(scam_compound_2026_tr_t10, scam_compound_2026, theater_ratio, 10, 0.65).

% Extraction over time (efficiency of exploitation increases):
narrative_ontology:measurement(scam_compound_2026_ex_t0, scam_compound_2026, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(scam_compound_2026_ex_t5, scam_compound_2026, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(scam_compound_2026_ex_t10, scam_compound_2026, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The internal structure is a perverse form of an enforcement mechanism,
% using rules and punishment to compel labor.
narrative_ontology:coordination_type(scam_compound_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% These compounds are often linked to global money laundering networks and
% illicit supply chains for technology.
% narrative_ontology:affects_constraint(scam_compound_2026, global_money_laundering_networks).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */