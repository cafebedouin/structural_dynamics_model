% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a novel claim is accepted. While it serves
 *   a vital coordination function—ensuring the reliability of the shared body
 *   of knowledge—it imposes significant costs on the original claimants,
 *   whose work is held in limbo and subjected to intense scrutiny. This
 *   creates a structural tension between ensuring quality and enabling
 *   innovation.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: The primary targets of the constraint, who bear the costs of verification (moderate/constrained).
 *   - The Scientific Community/Establishment: The primary beneficiaries and enforcers, who gain a reliable knowledge base (institutional/arbitrage).
 *   - The Public and Downstream Technologists: Secondary beneficiaries who rely on the vetted outputs of science (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r2, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r2, 0.8).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r2, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, the_scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, public_and_downstream_technologists).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, established_paradigm_holders).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r2, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r2, researchers_in_underfunded_fields).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the researcher with a new, disruptive finding, the process is a high-cost, high-coercion gauntlet they are constrained to run, where the benefits are delayed and may be captured by others.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% For the institutions of science (journals, funding bodies), this is a pure coordination mechanism essential for maintaining the integrity and reliability of the collective knowledge base.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function and the asymmetric extraction imposed on innovators, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% To the public, the outcome of this process appears as an unchangeable fact ('Science says...'). The internal extraction and coordination struggles are invisible, making it seem like a natural law.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) reflects the significant, asymmetric cost in time, resources, and career risk borne by innovators. The suppression score (0.80) is high because there are no socially accepted alternatives to this process within mainstream science; bypassing it leads to marginalization. The theater ratio (0.20) is low but non-zero, acknowledging that while the core work is functional, careerist incentives can add performative layers.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: innovators experience a Snare (a costly, coercive process they must endure), while the scientific establishment sees a Rope (a necessary tool for coordination and quality control). The public, unaware of the internal dynamics, perceives the outcome as a Mountain (an objective, unchangeable fact). The analytical perspective of Tangled Rope is required to hold both the coordination and extraction elements in view simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural flow of costs and benefits. 'Novel Claim Proposers' are declared as victims because they bear the direct costs of the verification delay and scrutiny. 'The Scientific Community' and 'Established Paradigm Holders' are beneficiaries because the system filters challenges and produces a stable, reliable knowledge base that reinforces their position and benefits the collective.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope is crucial for avoiding mandatrophy. A Rope classification would ignore the real, often career-altering extraction imposed on scientists with novel claims. A Snare classification would wrongly dismiss the undeniably critical coordination function that makes science a cumulative, reliable enterprise. The Tangled Rope classification correctly identifies it as a system with both a legitimate purpose and a significant, asymmetrically distributed cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_gatekeeping,
    'Is the high cost of verification a necessary filter for truth, or does it primarily serve to suppress disruptive innovation and entrench existing paradigms?',
    'Comparative analysis of verification times and success rates for paradigm-confirming vs. paradigm-shifting claims across multiple fields.',
    'If primarily a necessary filter, the constraint is closer to a Rope. If primarily gatekeeping, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_gatekeeping, empirical, 'Whether the verification cost is a necessary filter or a gatekeeping mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r2, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r2, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r2, technological_development_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
