% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_sed_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_sed_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_sed_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   before a claim is accepted. This serves a crucial coordination function,
 *   creating a shared, reliable body of knowledge. However, it also imposes
 *   significant costs on innovators, especially those challenging existing
 *   paradigms or working with limited resources, creating an extractive
 *   dynamic where the scientific establishment benefits from stability at the
 *   expense of disruptive but potentially valid new discoveries.
 *
 * KEY AGENTS:
 *   - Paradigm Challengers (powerless/trapped): Primary targets who bear the costs of verification.
 *   - Scientific Establishment / Gatekeepers (institutional/arbitrage): Primary beneficiaries who enforce the standard and benefit from epistemic stability.
 *   - Pragmatic Researchers (moderate/mobile): Participants who navigate the system, understanding both its benefits and its costs.
 *   - The General Public (powerless/trapped): Indirect beneficiary of a reliable scientific consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r2, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_sed_r2, 0.6).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_sed_r2, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r2, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r2, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r2, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_sed_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_sed_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_sed_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_sed_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r2, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r2, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r2, general_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r2, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r2, under_resourced_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r2, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel but difficult-to-replicate finding, the process feels like a snare designed to protect the status quo, potentially ending their career.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For funding bodies and journal editors, this is a pure coordination mechanism (rope) to ensure quality, filter noise, and maintain the integrity of the scientific record.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A mid-career scientist sees both the necessity for coordination and the extractive costs imposed on novelty, experiencing the system as a tangled rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the full structure: a vital coordination function for establishing reliable knowledge, achieved via an extractive process that suppresses novelty and burdens challengers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_sed_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_sed_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the high career, time, and funding costs imposed on researchers to get novel claims accepted. Suppression (0.60) represents the high barrier to entry for claims that contradict the consensus, enforced by peer review and funding bodies. The theater ratio (0.20) is low as the core activity is functional, though performative elements exist.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the institutional gatekeeper, verification is a pure Rope that coordinates the field towards truth. For the paradigm challenger, it is a Snare that extracts their resources and suppresses their work to protect the status quo. This difference arises directly from their structural relationship to the flow of epistemic authority and funding.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value (in the form of stability, reliability, and authority) towards the established scientific consensus and its institutional guardians. It extracts resources (time, funding, career opportunities) from those proposing novel, unverified, or challenging claims. The general public benefits from the output, but the direct costs are borne by a specific class of researchers.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly identifies the dual nature of the system. A pure Rope classification would ignore the immense, asymmetrically distributed costs and the active suppression of certain kinds of knowledge. A pure Snare classification would fail to recognize the indispensable coordination function that prevents science from descending into an epistemic free-for-all. The Tangled Rope captures the reality that a necessary coordination function is implemented via an extractive, coercive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_suppression_optimality,
    'Is the high cost of verification a necessary feature to ensure rigor, or an excessive barrier that primarily serves to protect existing paradigms?',
    'Retrospective analysis of the rate of rejected-then-vindicated scientific claims versus the severity of 'replication crises' in various fields.',
    'If the cost is shown to be optimally calibrated for rigor, the constraint is closer to a Rope. If it's excessively suppressive, it's closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_suppression_optimality, empirical, 'Whether the cost of verification is an optimal filter or an excessive barrier to progress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_sed_r2, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_sed_r2, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u2_sed_r2, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_sed_r2, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_sed_r2, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u2_sed_r2, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_sed_r2, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_sed_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_sed_r2, climate_model_consensus).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_sed_r2, pharmaceutical_approval_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
