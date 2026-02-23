% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u4_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u4_exp_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u4_exp_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the standard scientific method of requiring
 *   independent replication and corroboration before a novel claim is
 *   accepted as fact. While this process coordinates the scientific community
 *   towards a shared, reliable understanding of reality, it imposes
 *   significant costs in time, funding, and career progression, which are not
 *   borne equally by all participants.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary targets of extraction (powerless/trapped)
 *   - Established Institutions (Universities, Journals): Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - Proponents of Heterodox Claims: Secondary targets, often with more resources but still constrained (moderate/constrained)
 *   - The Scientific Community: Collective beneficiary of the coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r5, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u4_exp_r5, 0.7).
domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r5, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r5, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r5, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r5, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u4_exp_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u4_exp_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u4_exp_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u4_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r5, established_scientific_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r5, senior_principal_investigators).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r5, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r5, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r5, proponents_of_heterodox_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r5, underfunded_laboratories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For an early-career researcher, the demand for replication before acceptance can be a career-ending obstacle, consuming precious time and resources with no guarantee of success. It functions as a filter that extracts their innovative labor and discards them if they cannot meet the high burden of proof.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of a major university, funding body, or journal, the verification process is a pure coordination mechanism. It ensures the quality and reliability of the scientific record, which underpins their prestige and authority. The costs are externalized to individual researchers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analyst sees both the essential coordination function—creating a shared, reliable body of knowledge—and the asymmetric extraction. The process imposes severe costs on newcomers and outliers while reinforcing the authority of established players. This duality is the hallmark of a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A researcher with a genuinely novel but difficult-to-replicate finding experiences the system as a Snare. Despite being correct, their inability to meet the conventional standards of verification leads to their work being dismissed, funding withdrawn, and reputation damaged.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u4_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u4_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the high cost of failure (lost time, funding, career) imposed on individuals whose claims are not verified. The suppression score (0.70) is high because the system's explicit purpose is to filter out and reject uncorroborated claims, effectively suppressing alternative epistemic standards. The theater ratio is low (0.15) as the core work of replication is highly functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an established institution, the process is a low-cost Rope that maintains order and quality. For a junior researcher, it is a high-stakes Snare where a single failure to replicate can terminate a career. The former experiences the coordination benefit, while the latter bears the concentrated extraction cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the institutions and established PIs who control the resources for verification and whose authority is enhanced by a stable, vetted body of knowledge. Victims are the researchers at the frontier, particularly those with limited resources or unconventional ideas, who must pay the high entry cost of verification to have their work accepted.
 *
 * MANDATROPHY ANALYSIS:
 *   A Tangled Rope classification is crucial here. It prevents the mischaracterization of the scientific method as a pure Rope, which would ignore the immense career-level extraction it imposes on its most innovative but precarious members. It also avoids calling it a pure Snare, which would deny its undeniably powerful and necessary function in coordinating a global community to build reliable knowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_vs_stagnation,
    'Does the high cost of verification primarily serve to ensure reliability (a necessary cost of coordination), or does it primarily serve to enforce orthodoxy and stifle paradigm-shifting innovation (predatory extraction)?',
    'Comparative analysis of funding allocation and publication success rates for incremental vs. revolutionary scientific claims over a multi-decade period.',
    'If primarily for reliability, the constraint's base extractiveness could be re-evaluated as a functional cost, pushing it towards Rope. If primarily for orthodoxy, it is a deeply embedded Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_vs_stagnation, empirical, 'Distinguishing the functional cost of ensuring epistemic reliability from the extractive cost of enforcing orthodoxy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u4_exp_r5, 1660, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_process_of_verification_u4_exp_r5, theater_ratio, 0, 0.05).
narrative_ontology:measurement(epis_tr_t180, epistemic_process_of_verification_u4_exp_r5, theater_ratio, 180, 0.1).
narrative_ontology:measurement(epis_tr_t360, epistemic_process_of_verification_u4_exp_r5, theater_ratio, 360, 0.15).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_process_of_verification_u4_exp_r5, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(epis_be_t180, epistemic_process_of_verification_u4_exp_r5, base_extractiveness, 180, 0.4).
narrative_ontology:measurement(epis_be_t360, epistemic_process_of_verification_u4_exp_r5, base_extractiveness, 360, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u4_exp_r5, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r5, academic_tenure_system).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r5, journal_peer_review).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
