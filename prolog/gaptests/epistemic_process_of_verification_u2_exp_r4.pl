% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
 *   before a claim is accepted. This serves a vital coordination function,
 *   creating a shared, reliable body of knowledge. However, it also imposes
 *   significant costs (time, funding, reputation) on the proposers of novel
 *   claims, creating an extractive dynamic where innovators bear high risk
 *   for the collective benefit of the field's stability.
 *
 * KEY AGENTS:
 *   - Junior/Heterodox Researchers: Primary targets of extraction (powerless/trapped)
 *   - Established Scientific Community (Journals, Universities, Senior Faculty): Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - Funding Agencies: Beneficiaries seeking to de-risk investment (institutional/arbitrage)
 *   - The Public: Indirect beneficiary of reliable science
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r4, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r4, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r4, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r4, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, established_scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r4, general_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r4, junior_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r4, proposers_of_novel_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r4, heterodox_theorists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior researcher, the burden of proof and risk of being wrong or ignored can be career-ending. The process feels purely extractive and coercive.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the institution, this process is a pure coordination mechanism to ensure quality, stability, and efficient allocation of trust and resources. The extraction is seen as a necessary cost of doing business.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A tenured academic both benefits from the system's stability and bears the costs of verification for their own new ideas. They are aware of both the coordination and extraction.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical view recognizes the essential coordination function in building reliable knowledge while also seeing the clear, asymmetric extraction from innovators that can slow progress.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   The base extractiveness (ε=0.55) reflects the career-level stakes and significant resource investment required for a novel claim to be validated. Suppression (0.75) is high because alternative epistemic systems (e.g., authority-based claims) are actively and systematically excluded from mainstream science. The process is highly functional, hence the low theater ratio (0.30).
 *
 * PERSPECTIVAL GAP:
 *   The primary gap exists between the junior researcher, who experiences the process as a high-stakes, coercive Snare, and the institutional actor (e.g., a journal), which views it as a necessary, low-cost Rope for maintaining quality. The former feels the extraction personally, while the latter abstracts it as a system-level feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Value is extracted from the individual innovator, who must expend immense resources to meet the burden of proof. This value is transferred to the collective scientific field in the form of increased reliability and stability. The beneficiaries are those who depend on this stability—established labs, funding bodies, and downstream technology—while the costs are concentrated on those pushing the frontier.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a common error, ignoring the severe, asymmetric costs imposed on innovators. Conversely, calling it a pure Snare would ignore its undeniably critical function in coordinating the scientific enterprise. The Tangled Rope classification correctly identifies that it has both a genuine coordination function and a significant, non-trivial extractive component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_gatekeeping,
    'Is the high cost of verification a necessary feature for epistemic rigor, or a form of inertial gatekeeping that suppresses genuinely novel but disruptive ideas?',
    'Comparative analysis of time-to-acceptance for paradigm-shifting papers versus incremental ones, and funding outcomes for high-risk proposals.',
    'If primarily gatekeeping, the constraint's base extractiveness is higher and it functions more like a Snare. If primarily for rigor, it is a necessary Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_gatekeeping, empirical, 'Whether verification costs are for rigor or inertial gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r4, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u2_exp_r4, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u2_exp_r4, base_extractiveness, 2024, 0.55).


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
