% ============================================================================
% CONSTRAINT STORY: portuguese_presidential_term_limits
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_portuguese_presidential_term_limits, []).

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
 * * constraint_id: portuguese_presidential_term_limits
 * human_readable: Portuguese Constitutional Term Limits (Article 123)
 * domain: political/legal
 * * SUMMARY:
 * Under the Portuguese Constitution, a President cannot serve a third
 * consecutive term. This creates an absolute "Mountain"
 * constraint for the 2026 election, as the highly popular incumbent,
 * Marcelo Rebelo de Sousa, is ineligible to run.
 * This structural limit forces a total reconfiguration of the political
 * field, transitioning from a stable incumbency to an open, multi-polar race.
 * * KEY AGENTS:
 * - Portuguese Electorate: Subject (Powerless) - Forbidden from voting for their most preferred candidate due to legal limits.
 * - Marcelo Rebelo de Sousa: Subject (Institutional) - The popular incumbent barred from re-election.
 * - Potential Presidential Candidates: Beneficiary (Organized) - Benefit from the open field created by the incumbent's ineligibility.
 * - Legal System: Auditor (Analytical) - Upholds the constitution as a fixed rule.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Mountain status: Extraction is effectively zero as it is a fixed constitutional rule.
domain_priors:base_extractiveness(portuguese_presidential_term_limits, 0.02).
% Suppression: Absolute. No legal "exit" exists for a third term.
domain_priors:suppression_score(portuguese_presidential_term_limits, 0.98).
% Theater: Low. The rule is strictly enforced by the Constitutional Court.
domain_priors:theater_ratio(portuguese_presidential_term_limits, 0.05).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, extractiveness, 0.02).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, theater_ratio, 0.05).

% Constraint self-claim: It claims to be a coordination mechanism for democratic turnover.
narrative_ontology:constraint_claim(portuguese_presidential_term_limits, mountain).

% Binary flags
domain_priors:requires_active_enforcement(portuguese_presidential_term_limits).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(portuguese_presidential_term_limits, potential_presidential_candidates).
narrative_ontology:constraint_victim(portuguese_presidential_term_limits, electorate_preferring_incumbent).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (POWERLESS VOTER) -> MOUNTAIN
% For a voter who wishes to re-elect the incumbent, the law is not a predatory
% snare (as extraction is near-zero), but an unchangeable, absolute limit
% on their democratic choice. The high suppression score is what they experience.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ORGANIZED CANDIDATE) -> ROPE
% For a new candidate, the term limit is a pure coordination mechanism (Rope)
% that guarantees an open field, prevents entrenched power, and ensures
% healthy democratic turnover.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER -> MOUNTAIN
% Viewed as an irreducible, physical-like limit of the democratic cycle.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INCUMBENT (INSTITUTIONAL) -> MOUNTAIN
% From the perspective of the incumbent president, the constitutional rule is
% an absolute, non-negotiable barrier to continuing in office. It is a Mountain.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_presidential_term_limits_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the powerless voter and the organized candidate.
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(portuguese_presidential_term_limits, mountain, context(agent_power(institutional), _, _, _)).

test(mountain_threshold_validation) :-
    % Verify the constraint meets the low-extraction, high-suppression signature of a Mountain.
    domain_priors:base_extractiveness(portuguese_presidential_term_limits, E),
    domain_priors:suppression_score(portuguese_presidential_term_limits, S),
    E =< 0.15,
    S >= 0.90.

:- end_tests(portuguese_presidential_term_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint demonstrates a key perspectival gap between a Mountain and a
 * Rope. The base metrics (ε=0.02, S=0.98) are a classic Mountain signature: an
 * unchangeable rule with very high suppression but negligible extraction.
 * For most agents (the incumbent, voters, analysts), it functions as this
 * immutable limit.
 *
 * However, for the set of agents who benefit from the turnover it enforces
 * (i.e., new presidential candidates), the rule is a pure coordination
 * mechanism (a Rope). It solves the collective action problem of how to
 * prevent permanent incumbency, creating opportunities for others. This
 * Mountain/Rope divergence is characteristic of foundational rules in stable
 * political systems. The original file's classification of 'Snare' for the
 * voter was incorrect, as the feeling of suppressed choice is captured by the
 * high suppression score, not by extraction.
 *
 * [RESOLVED MANDATROPHY]
 * Extraction is low (0.02), avoiding the high-extraction triggers. The
 * perspectival gap is explained by the dual function of the rule as both a
 * hard limit (Mountain) and a coordination device (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_portugal_nonpartisan,
    'Will the "Mountain" of term limits lead to a permanent shift toward non-partisan Presidents?',
    'Analyzing the performance and governance style of the 2026 election winner versus party-backed predecessors.',
    'Structural Shift (Rope evolves) vs Party Restoration (Return to status quo)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(portuguese_presidential_term_limits, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Term limits are a mechanism to enforce democratic turnover.
narrative_ontology:coordination_type(portuguese_presidential_term_limits, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */