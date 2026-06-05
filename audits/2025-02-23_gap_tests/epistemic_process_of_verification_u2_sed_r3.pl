% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_sed_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_sed_r3, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_sed_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification, centered on independent
 *   replication, is a foundational coordination mechanism. It aligns the
 *   global scientific community on a shared body of reliable knowledge.
 *   However, this process is not frictionless. It imposes significant costs
 *   in time, funding, and career risk, which are disproportionately borne by
 *   researchers proposing novel or paradigm-shifting claims. This creates an
 *   inherent tension between ensuring reliability (coordination) and enabling
 *   innovation (minimizing extraction).
 *
 * KEY AGENTS:
 *   - Proponents of Novel Claims: Primary targets (powerless/trapped) who bear the burden of proof.
 *   - Established Scientific Community: Primary beneficiaries (institutional/arbitrage) who define and enforce the standards.
 *   - Funding Agencies & Publishers: Institutional beneficiaries who allocate resources based on the verification standard.
 *   - Working Scientists: General participants (moderate/mobile) who both benefit from and pay into the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r3, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_sed_r3, 0.7).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_sed_r3, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r3, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r3, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r3, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_sed_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_sed_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_sed_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_sed_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r3, established_scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r3, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r3, journal_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r3, general_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r3, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r3, proponents_of_novel_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r3, underfunded_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel but difficult-to-replicate claim, the process is a coercive, high-cost barrier that can end a career. They are trapped within the system if they wish to be recognized.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the established community (e.g., journal editors, tenured faculty), the process is a necessary coordination tool to maintain standards, filter noise, and build a reliable body of knowledge. They benefit from and enforce the system.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A mid-career scientist experiences both the benefits of a stable knowledge base and the extractive costs of getting their own work verified. They can navigate the system but feel its friction.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical view recognizes the essential coordination function (preventing epistemic chaos) is inextricably linked with an extractive process that slows progress and disadvantages challengers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_sed_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_sed_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.55 reflects the high, non-recoverable costs (years of work, grant funding) required to push a novel claim through the verification gauntlet. The suppression score of 0.70 reflects the near-impossibility of having a claim accepted into the scientific canon without passing through this process; alternative paths to legitimacy are strongly suppressed.
 *
 * PERSPECTIVAL GAP:
 *   A stark gap exists between the 'Paradigm Challenger' who sees a career-threatening Snare and the 'Field's Gatekeeper' who sees a quality-control Rope. The former experiences the full extractive force of the system, while the latter primarily experiences its coordinating benefits and wields its power.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts resources (time, funding, intellectual energy) from individual claimants and converts it into a collective good: certainty and reliability for the entire field. The beneficiaries are the established community and society, who receive a vetted knowledge product. The victims are the innovators at the frontier, who must pay the high entry cost.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures its dual nature. A pure Rope classification would ignore the immense, often career-ending costs imposed on innovators. A pure Snare classification would wrongly dismiss the absolutely essential coordination function that prevents science from descending into an epistemic free-for-all. The Tangled Rope acknowledges that the mechanism for creating order is simultaneously a mechanism of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_friction_optimality,
    'Is the current level of skepticism and demand for replication an optimal filter for truth, or has it become an excessively conservative barrier that primarily serves to protect existing paradigms?',
    'Comparative analysis of innovation rates, retraction rates, and time-to-acceptance for paradigm-shifting claims across fields with different verification standards.',
    'If the friction is proven to be optimally calibrated for truth-finding, the constraint is closer to a Rope. If it's excessively conservative, it's closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_friction_optimality, empirical, 'Whether the verification process is an optimal filter or an overly conservative barrier to innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_sed_r3, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_sed_r3, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u2_sed_r3, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_sed_r3, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_sed_r3, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u2_sed_r3, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_sed_r3, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_sed_r3, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_sed_r3, public_trust_in_science).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
