% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_exp_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_exp_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint models the scientific method's requirement for
 *   independent verification and replication. While this process is
 *   fundamental to coordinating the scientific community towards a shared,
 *   reliable body of knowledge, it imposes significant, asymmetric costs on
 *   early-career researchers and those with novel or heterodox findings,
 *   whose careers and funding are put at risk by the slow, expensive, and
 *   inherently conservative nature of verification.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary targets (powerless/trapped) who bear the costs of delay and skepticism.
 *   - Established Research Institutions: Primary beneficiaries (institutional/arbitrage) who act as gatekeepers and benefit from the stability and prestige the system provides.
 *   - Scientific Publishers: Institutional beneficiaries who leverage the peer review and verification process as a core part of their value proposition.
 *   - Labs with Heterodox Claims: Secondary targets (moderate/constrained) who face high barriers to acceptance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r5, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u3_exp_r5, 0.8).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_exp_r5, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r5, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r5, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r5, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_exp_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_exp_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_exp_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r5, established_research_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r5, scientific_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r5, society_at_large).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r5, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r5, labs_with_heterodox_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher whose career depends on novel publications, the slow, expensive, and skeptical verification process acts as a coercive filter that can terminate their career. The costs are immediate and personal, while the collective benefit is abstract.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For a well-funded institution, the verification process is a quality control mechanism that maintains the integrity of the field and its own reputation. It coordinates the community towards reliable knowledge, reinforcing the value of its own rigorous (and well-resourced) approach.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the indispensable coordination function (preventing an epistemic free-for-all) and the inherent, asymmetric extraction imposed on the most vulnerable and innovative actors in the system.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% From the perspective of a national funding agency, the process is a necessary evil. It ensures public money is spent on generating reliable knowledge (coordination), but it also consumes significant resources in 'redundant' replication and can stifle rapid innovation (extraction).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the high stakes for individual researchers, where failure to be replicated can be a career-ending event. The suppression score (0.80) is high because, within mainstream science, there is no viable alternative to this process; bypassing it results in being ostracized from the community. The active enforcement is carried out by peer reviewers, journal editors, and funding committees.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the pre-tenure researcher, who experiences the process as a high-stakes, coercive Snare, and the established institution, which views it as a necessary Rope for maintaining quality and order. The former is trapped by the 'publish or perish' imperative, while the latter uses the system to solidify its own authority and epistemic standards.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction flows from the new, precarious, and innovative to the established, stable, and conservative. Early-career researchers and proponents of novel theories provide the 'risk capital' of new ideas, but the verification process ensures that the 'dividends' of accepted knowledge primarily accrue to the established institutions and paradigms that control the means of replication.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope is crucial. A simple Rope classification would ignore the severe extraction imposed on junior scientists. A simple Snare classification would deny the process's absolutely essential function in coordinating scientific activity and producing reliable knowledge. The Tangled Rope correctly identifies that the mechanism for coordination is simultaneously the mechanism for extraction and gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_cost_origin,
    'Is the high cost of replication an irreducible feature of complex natural systems, or is it an artifact of the current institutional and funding structures of science?',
    'Comparative analysis of verification costs in fields with different funding models (e.g., public vs. private) or the development of AI-driven verification tools that lower the marginal cost of replication.',
    'If the cost is irreducible (a Mountain floor), the constraint is a fundamental Tangled Rope. If it is institutional, the system could be reformed into a more efficient Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_cost_origin, empirical, 'Whether the high cost of scientific replication is a natural limit or an institutional artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_exp_r5, 1950, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u3_exp_r5, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u3_exp_r5, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u3_exp_r5, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u3_exp_r5, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u3_exp_r5, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u3_exp_r5, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_exp_r5, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r5, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r5, technological_development_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
