% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u1_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u1_exp_r4, []).

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
 *   constraint_id: epistemic_process_of_verification_u1_exp_r4
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a novel claim is accepted. This process
 *   coordinates the scientific community towards a shared, reliable
 *   understanding of reality. However, it imposes significant costs (time,
 *   funding, career risk) on the researchers proposing novel claims, creating
 *   a system with both a vital coordination function and a potent, asymmetric
 *   extractive component.
 *
 * KEY AGENTS:
 *   - Researchers with novel claims: Primary targets of the process (powerless/trapped).
 *   - Early-career researchers: A subset of targets who are particularly vulnerable to the costs of verification.
 *   - Scientific institutions (journals, funding bodies): Beneficiaries and enforcers of the process (institutional/arbitrage).
 *   - The established scientific community: Beneficiaries who rely on the shared standard (organized/mobile).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r4, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u1_exp_r4, 0.55).
domain_priors:theater_ratio(epistemic_process_of_verification_u1_exp_r4, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r4, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r4, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r4, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u1_exp_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u1_exp_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u1_exp_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u1_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r4, scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r4, established_researchers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r4, funding_agencies_and_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r4, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r4, researchers_with_novel_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a researcher with a new, unverified claim, the process is a high-stakes gauntlet that extracts immense resources (time, funding, reputation) with no guarantee of success. Exiting the process means abandoning the claim's entry into scientific canon.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the institutions that act as gatekeepers, the process is a pure coordination mechanism. It filters noise, ensures quality, and maintains the integrity of the field. The costs are externalized to the claimants, making the effective extraction for the institution negative.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes the dual function. The process is a Tangled Rope: it provides an essential coordination function for the entire scientific enterprise, but this function is achieved via an asymmetric extraction of resources from a specific class of agents (innovators).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For the community of established scientists, the process is a Rope that maintains the value of their own verified work and ensures a common ground of reliable facts to build upon. They both enforce and benefit from the standard.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u1_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u1_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.48 reflects the significant, non-trivial cost in resources and career risk required to pass a claim through the verification gauntlet. The suppression score of 0.55 reflects the process's core function: to actively filter and reject uncorroborated claims, thus suppressing alternatives to the established body of knowledge until they meet a high burden of proof.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the innovator who experiences the process as a costly, high-risk Snare, and the institution that wields it as a zero-cost (to them) Rope for quality control. The analytical classification of Tangled Rope is necessary to capture this dual-nature, where the coordination for the whole is paid for by a specific part.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is from the individual innovator towards the collective. The innovator (victim) invests their resources and reputation. The community (beneficiary) receives the benefit of a vetted, reliable new piece of knowledge, increasing the value and stability of the entire field. Institutions benefit by building prestige on the reliability this process creates.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the severe, asymmetric costs borne by innovators. Conversely, classifying it as a pure Snare would ignore its absolutely essential function in creating reliable, cumulative knowledge. The Tangled Rope classification correctly identifies that a socially vital coordination function is being powered by a highly extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_efficiency,
    'Is the high cost of verification an irreducible feature of discovering truth about a complex reality, or is it an artifact of inefficient and politicized human institutions (journals, universities)?',
    'Comparative analysis of verification costs and timescales across different fields and historical periods, and the development of novel, lower-cost verification methods (e.g., AI-driven experiment replication).',
    'If the cost is irreducible, the constraint is fundamentally a Rope with a high but necessary cost. If it is largely institutional, the extractive Snare-like component is larger and potentially reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_efficiency, empirical, 'Whether the high cost of verification is a necessary epistemic burden or an unnecessary institutional one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u1_exp_r4, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u1_exp_r4, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u1_exp_r4, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u1_exp_r4, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u1_exp_r4, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u1_exp_r4, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u1_exp_r4, base_extractiveness, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u1_exp_r4, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u1_exp_r4, academic_publishing_models).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u1_exp_r4, scientific_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
