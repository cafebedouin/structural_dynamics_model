% ============================================================================
% CONSTRAINT STORY: phase_a_noncontention_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_phase_a_noncontention_control, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: phase_a_noncontention_control
 *   human_readable: Phase-A mechanism demo: non-contention control
 *   domain: technology/markets
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(phase_a_noncontention_control, 0.55).
domain_priors:suppression_score(phase_a_noncontention_control, 0.45).
domain_priors:theater_ratio(phase_a_noncontention_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(phase_a_noncontention_control, extractiveness, 0.55).
narrative_ontology:constraint_metric(phase_a_noncontention_control, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(phase_a_noncontention_control, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(phase_a_noncontention_control, tangled_rope).
narrative_ontology:human_readable(phase_a_noncontention_control, "Phase-A mechanism demo: non-contention control").
narrative_ontology:topic_domain(phase_a_noncontention_control, "technology/markets").

domain_priors:requires_active_enforcement(phase_a_noncontention_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(phase_a_noncontention_control, standards_body).
narrative_ontology:constraint_victim(phase_a_noncontention_control, end_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(phase_a_noncontention_control, platform_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the standard; collects certification fees.
narrative_ontology:constraint_stakeholder(phase_a_noncontention_control, standards_body, beneficiary,
    institutional, biographical, mobile, national).

% Sells conforming implementations; same atom, same side.
narrative_ontology:constraint_stakeholder(phase_a_noncontention_control, platform_vendors, beneficiary,
    institutional, biographical, mobile, national).

% Pays the certification costs passed through.
narrative_ontology:constraint_stakeholder(phase_a_noncontention_control, end_users, payer,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% DISAPPEARANCE_RATIONALE: Certification market would reprice.
% FOUNDING_PROBLEM: Interoperability failures needed a standard.
% FOUNDING_PROBLEM_CORROBORATION: Regulator reports outside the standards body attest it.
narrative_ontology:disappearance_verdict(phase_a_noncontention_control, world_rearranges).
narrative_ontology:founding_problem_status(phase_a_noncontention_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(phase_a_noncontention_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(phase_a_noncontention_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(phase_a_noncontention_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(phase_a_noncontention_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(phase_a_noncontention_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(phase_a_noncontention_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(phase_a_noncontention_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(phase_a_noncontention_control, 0, 10).

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
