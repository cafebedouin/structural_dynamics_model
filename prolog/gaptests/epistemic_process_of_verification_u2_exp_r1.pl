% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   before a claim is accepted. This serves a vital coordination function,
 *   creating a shared, reliable body of knowledge. However, it also imposes
 *   significant costs in time, funding, and career risk, which are
 *   disproportionately borne by early-career researchers and those proposing
 *   ideas that challenge the existing paradigm. This creates a system with
 *   both a genuine coordination function and a powerful, asymmetric
 *   extractive component.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary targets who bear the highest verification costs (powerless/trapped).
 *   - Established Institutions (Journals, Universities, Funding Bodies): Primary beneficiaries and enforcers who control the process (institutional/arbitrage).
 *   - Senior Researchers: Secondary beneficiaries who act as gatekeepers.
 *   - Society at Large: Diffuse beneficiary of reliable scientific knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r1, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r1, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r1, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, established_scientific_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, senior_researchers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, society_at_large).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r1, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r1, proponents_of_novel_paradigms).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r1, underfunded_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel but difficult-to-replicate finding, the process is a high-cost, coercive barrier where failure means career death. They cannot opt out.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of a gatekeeper, this is a pure coordination mechanism to ensure quality control and maintain the integrity of the scientific record. The costs are seen as a necessary price for rigor.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view sees both the essential coordination function and the asymmetric extraction. It is a system that produces reliable knowledge by imposing heavy costs on newcomers and challengers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A mid-career researcher both benefits from the system's stability and is constrained by it. They enforce it on junior members while still facing its verification demands for their own work.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score of 0.48 reflects the immense, non-recoverable investment of time, resources, and career capital required from researchers to get a novel claim verified. The suppression score of 0.75 reflects the near-total lack of alternative, accepted pathways to canonize scientific knowledge; one cannot simply opt-out of peer review and replication.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional gatekeepers, the process is a Rope—a necessary tool for quality control. For the innovator whose career hinges on getting a difficult result replicated, it feels like a Snare—a coercive, high-stakes filter. The analytical perspective of Tangled Rope acknowledges that both are true simultaneously; the system's function is inseparable from its extractive nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Extraction flows from individual researchers, who must expend resources to prove their claims, towards the established institutions that gain prestige and stability from the slow, rigorous process. The system filters for claims that are not only true but also replicable with available technology and palatable to the existing community, reinforcing the status of those who control the resources for replication.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a failure of mandatrophy, ignoring the severe extractive costs and gatekeeping effects that can stifle innovation. Classifying it as a pure Snare would also fail, ignoring its undeniably critical role in coordinating the scientific community to prevent epistemic chaos. The Tangled Rope classification correctly identifies the inherent tension between its coordination and extraction functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_gatekeeping,
    'To what extent are the high costs of verification a necessary filter for quality versus an emergent or intentional tool for gatekeeping by established paradigms?',
    'Analysis of rejection rates for novel vs. incremental claims at top journals, correlated with the prestige of the submitting institution and funding sources.',
    'If primarily a quality filter, the constraint leans more towards Rope. If primarily a gatekeeping tool, it is functionally closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_gatekeeping, empirical, 'Whether verification costs are a necessary quality filter or a paradigm-preserving gatekeeping tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r1, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r1, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
