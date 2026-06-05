% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u4_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u4_exp_r3, []).

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
 *   constraint_id: epistemic_process_of_verification_u4_exp_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a novel claim is accepted. It serves to
 *   coordinate the scientific community towards a shared, reliable
 *   understanding of reality, but imposes significant costs in time,
 *   resources, and career risk upon the individuals and labs tasked with
 *   proposing and verifying new discoveries.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Primary targets of extraction (powerless/trapped)
 *   - Replicating Laboratories: Bear costs of verification (organized/constrained)
 *   - The Scientific Establishment (Journals, Funders): Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - The Public / Future Researchers: Indirect beneficiaries of a reliable knowledge base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r3, 0.45).
domain_priors:suppression_score(epistemic_process_of_verification_u4_exp_r3, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r3, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r3, extractiveness, 0.45).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r3, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u4_exp_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u4_exp_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u4_exp_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u4_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r3, scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r3, future_researchers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r3, public_trust_in_science).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r3, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r3, replicating_laboratories).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r3, disproven_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a researcher with a groundbreaking but unverified claim, the process is a costly, high-risk, and coercive barrier to acceptance, extracting significant time and resources with career-defining stakes.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the institutions that govern science (journals, funding bodies), the process is a pure coordination mechanism to filter noise, maintain standards, and build a reliable body of knowledge. The costs are externalized to individual researchers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analyst sees both the essential coordination function that creates consensus reality and the asymmetric extraction of resources and career opportunities from those at the frontier of knowledge.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A policymaker needing to act on new findings perceives the process as slow and inert. From this view, its primary function (providing timely, actionable truth) appears atrophied, making it a Piton.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, piton,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u4_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r3, TR),
    TR >= 0.70.

:- end_tests(epistemic_process_of_verification_u4_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.45 reflects the substantial, non-trivial costs (funding, equipment, personnel hours, career risk) required to push a novel claim through the verification process. The high suppression score of 0.75 reflects the immense institutional and cultural weight that prevents unverified claims from entering the scientific canon; there are virtually no accepted alternatives to this process.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the individual researcher experiences the process as a Snare that can derail a career, while the institution that benefits from the resulting stability sees it as a pure Rope for coordination. The analytical view must account for both realities, hence the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Extraction flows from individual innovators and replicators towards the collective institution of science. The 'victims' (proposers, replicators) bear the direct, immediate costs, while the 'beneficiaries' (the community, the public) receive the diffuse, long-term benefit of a vetted body of knowledge. This structural asymmetry is the defining feature.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the coercive and extractive pressures placed on scientists. Conversely, classifying it as a pure Snare would ignore its undeniably critical and productive coordination function. The Tangled Rope classification correctly identifies that the system's valuable coordination function is achieved via a mechanism of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_efficiency_vs_gatekeeping,
    'Is the high cost and delay of verification an unavoidable friction of complex science, or an inflated barrier that serves to gatekeep and protect established paradigms?',
    'Comparative analysis of verification costs and paradigm-shift rates across fields with different institutional structures and funding models.',
    'If costs are unavoidable (Mountain-like), the constraint is closer to a Rope. If costs are artificially inflated for gatekeeping (Snare-like), the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_efficiency_vs_gatekeeping, empirical, 'Distinguishing between necessary epistemic friction and artificial institutional gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u4_exp_r3, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u4_exp_r3, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1987, epistemic_process_of_verification_u4_exp_r3, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u4_exp_r3, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u4_exp_r3, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1987, epistemic_process_of_verification_u4_exp_r3, base_extractiveness, 1987, 0.35).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u4_exp_r3, base_extractiveness, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u4_exp_r3, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r3, public_health_policy).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r3, climate_change_mitigation).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r3, technological_development_cycles).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
