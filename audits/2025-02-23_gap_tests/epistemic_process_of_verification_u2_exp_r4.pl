% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r4, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r4
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   of claims before they are accepted. This serves a vital coordination
 *   function, creating a shared and reliable body of knowledge. However, this
 *   process imposes significant, asymmetric costs in terms of time, funding,
 *   and career risk, primarily on early-career researchers and those
 *   proposing highly novel or difficult-to-replicate findings.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary targets of extraction (powerless/trapped)
 *   - Proposers of Novel Claims: Secondary targets of extraction (moderate/constrained)
 *   - Established Scientific Institutions (Universities, Journals, Funding Bodies): Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - Scientific Community at Large: Beneficiary of coordination (organized/mobile)
 *   - General Public: Indirect beneficiary (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r4, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r4, 0.6).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r4, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, established_scientific_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, general_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r4, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r4, proposers_of_novel_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher needing novel results for tenure, the high cost and long delay of verification acts as a Snare, extracting time and career capital with a high risk of failure.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For a well-funded lab or journal, the process is a Rope that coordinates the community, maintains quality standards, and reinforces their own position as arbiters of fact.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view sees both the essential coordination function (Rope) and the asymmetric costs imposed on new entrants (Snare), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The public experiences the reliable outputs of the process (technology, medicine) and perceives it as a pure coordination good (Rope), largely unaware of the internal extraction.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.55 reflects the immense cost of experimental replication, the years of researcher time invested, and the career-altering consequences of failed verification. The suppression score of 0.60 represents the high bar for acceptance and the active filtering of claims that cannot (yet) be independently corroborated, effectively suppressing them from canon.
 *
 * PERSPECTIVAL GAP:
 *   A stark gap exists between the early-career researcher, who experiences the system as a high-risk Snare that could end their career, and the established institution, which sees a necessary Rope for maintaining quality and order. The analytical perspective reconciles these by identifying the structure as a Tangled Rope, acknowledging both the valid coordination and the harsh, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts career potential, time, and resources from individual researchers (the victims) and transforms them into epistemic certainty and prestige for the community and its established institutions (the beneficiaries). The flow is from the precarious and novel to the stable and canonical.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a failure of Mandatrophy, as it would ignore the severe costs borne by a specific class of participants. Conversely, calling it a pure Snare would ignore its undeniably critical function in creating reliable knowledge. The Tangled Rope classification is essential to capture this duality of beneficial coordination built upon costly extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_necessity,
    'Is the high cost of verification an irreducible requirement for scientific rigor, or has it become an excessive form of gatekeeping by established players?',
    'Comparative analysis of scientific progress rates vs. funding concentration and replication costs over time. Analysis of retracted papers vs. the career status of their authors.',
    'If the cost is proven to be an irreducible feature of rigor, the constraint leans more towards Rope. If it's primarily gatekeeping, it's a more extractive Tangled Rope, bordering on Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_necessity, empirical, 'Whether the high cost of verification is necessary for rigor or a form of gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r4, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r4, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
