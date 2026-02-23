% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r3, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The requirement for independent verification and replication is the
 *   bedrock of the scientific method. It serves a critical function by
 *   coordinating a global community of researchers toward a shared, reliable
 *   body of knowledge. However, this process is not frictionless. It imposes
 *   significant costs in time, resources, and career risk, and these costs
 *   are borne asymmetrically, primarily by early-career researchers and those
 *   proposing ideas that challenge the existing paradigm.
 *
 * KEY AGENTS:
 *   - Early-career researchers (Victim/Powerless/Trapped): Must pass through the verification gauntlet to establish a career.
 *   - Established scientific institutions (Beneficiary/Institutional/Arbitrage): Administer the process, benefiting from the stability and quality control it provides.
 *   - Proponents of heterodox claims (Victim/Powerless/Trapped): Face an amplified burden of proof and institutional resistance.
 *   - Senior academics / Peer reviewers (Beneficiary/Powerful/Mobile): Act as gatekeepers and validators within the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r3, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r3, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r3, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, established_scientific_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, senior_academics).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, proponents_of_heterodox_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, underfunded_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior researcher, the verification process is a high-stakes gatekeeper. The cost of failure is catastrophic to their career, and they lack the resources to easily navigate it. The coordination benefit is abstract compared to the immediate, high-extraction reality.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From an institutional viewpoint, the process is a pure coordination mechanism for quality control. The costs (extraction) are externalized onto the claimants and are seen as a necessary filter for maintaining the integrity of the scientific record.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A tenured academic in the field experiences both the coordination benefits (a stable paradigm to work within) and the extractive power (the ability to gatekeep competitors' work). They can choose which claims to engage with, making their position mobile.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical view recognizes the essential coordination function for building reliable knowledge while also seeing the clear, asymmetric extraction of resources and career potential from new or under-resourced entrants. This duality is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.48 reflects the immense cost of replication for complex experiments and the career-ending potential of a failed replication. The suppression score of 0.75 is high because there are virtually no legitimate alternatives within the scientific enterprise; bypassing verification is career suicide. The process is actively enforced by journal editors, peer reviewers, and funding committees, justifying the `requires_active_enforcement` flag.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the institutional beneficiary and the junior researcher. The institution perceives a pure Rope, a necessary tool for coordination and quality control, because it externalizes the costs. The junior researcher experiences it as a Snare, a high-risk, high-cost barrier where they are trapped and from which their career potential is extracted by an unforgiving system.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality flows from the claimants (victims) to the established system (beneficiaries). Junior and heterodox researchers provide the 'energy' for the system—their novel claims, time, and grant money—which is then filtered and consumed by the verification process. The beneficiaries are the established institutions and senior figures who gain a stable, reliable, and self-perpetuating field of knowledge without bearing the proportional risk.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope is crucial. A simple Rope classification would be naive, ignoring the severe, asymmetric costs borne by newcomers and challengers. A Snare classification would be cynical, ignoring the undeniably vital coordination function that produces reliable knowledge for society. The Tangled Rope classification correctly identifies that a system can have both a genuine, positive coordination function and a simultaneous, structurally embedded extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_necessity,
    'Is the high cost and slow pace of verification an irreducible property of discovering truth, or a socially constructed barrier that primarily serves to maintain existing power structures?',
    'Comparative analysis of scientific progress rates under different verification models, combined with a network analysis of funding and publication success for heterodox vs. orthodox claims.',
    'If the cost is proven to be an irreducible feature of rigor, the constraint would be closer to a Mountain. If it's primarily a gatekeeping tool, it's closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_necessity, conceptual, 'Whether the high cost of verification is an essential feature for rigor or a socially constructed gatekeeping mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r3, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1975, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(epis_tr_t2000, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1975, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(epis_be_t2000, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r3, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
