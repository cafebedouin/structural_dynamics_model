% ============================================================================
% CONSTRAINT STORY: phase_a_contention_demo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_phase_a_contention_demo, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: phase_a_contention_demo
 *   human_readable: Phase-A mechanism demo: contention
 *   domain: technology/markets
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(phase_a_contention_demo, 0.55).
domain_priors:suppression_score(phase_a_contention_demo, 0.45).
domain_priors:theater_ratio(phase_a_contention_demo, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(phase_a_contention_demo, extractiveness, 0.55).
narrative_ontology:constraint_metric(phase_a_contention_demo, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(phase_a_contention_demo, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(phase_a_contention_demo, tangled_rope).
narrative_ontology:human_readable(phase_a_contention_demo, "Phase-A mechanism demo: contention").
narrative_ontology:topic_domain(phase_a_contention_demo, "technology/markets").

domain_priors:requires_active_enforcement(phase_a_contention_demo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(phase_a_contention_demo, platform_operator).
narrative_ontology:constraint_victim(phase_a_contention_demo, publisher_consortium).
narrative_ontology:constraint_victim(phase_a_contention_demo, independent_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the distribution terms; collects the margin.
narrative_ontology:constraint_stakeholder(phase_a_contention_demo, platform_operator, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(phase_a_contention_demo, platform_operator, beneficiary).

% Institutional counter-party paying the margin; same power atom, opposite side.
narrative_ontology:constraint_stakeholder(phase_a_contention_demo, publisher_consortium, payer,
    institutional, biographical, mobile, national).

% Bears the terms with no negotiating position.
narrative_ontology:constraint_stakeholder(phase_a_contention_demo, independent_creators, payer,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A common distribution rail is genuinely useful.
% TRANSFER_FUNCTION: Margin moves from publishers and creators to the operator.
% DISAPPEARANCE_RATIONALE: Distribution contracts would be renegotiated economy-wide.
% FOUNDING_PROBLEM: Fragmented distribution needed a common rail.
% FOUNDING_PROBLEM_CORROBORATION: Industry analyses outside the operator attest the founding problem.
narrative_ontology:disappearance_verdict(phase_a_contention_demo, world_rearranges).
narrative_ontology:founding_problem_status(phase_a_contention_demo, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(phase_a_contention_demo, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(phase_a_contention_demo, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(phase_a_contention_demo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(phase_a_contention_demo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(phase_a_contention_demo, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(phase_a_contention_demo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(phase_a_contention_demo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(phase_a_contention_demo, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
