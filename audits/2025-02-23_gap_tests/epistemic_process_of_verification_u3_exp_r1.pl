% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_exp_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_exp_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the standard scientific method of requiring
 *   independent replication and corroboration before a novel claim is
 *   accepted. While this process is fundamental to coordinating the
 *   scientific community towards a shared, reliable understanding of reality,
 *   it imposes significant costs in time, resources, and career risk, which
 *   are not borne equally by all participants.
 *
 * KEY AGENTS:
 *   - Proponents of Novel Claims (early-career, underfunded): Primary targets of extraction (powerless/trapped).
 *   - Scientific Establishment (funding agencies, journals, tenured committees): Primary beneficiaries and enforcers (institutional/arbitrage).
 *   - Established Research Labs: Participants who both benefit from and are constrained by the system (powerful/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r1, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u3_exp_r1, 0.65).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_exp_r1, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r1, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r1, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_exp_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_exp_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_exp_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r1, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r1, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r1, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r1, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r1, proponents_of_novel_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r1, underfunded_labs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a researcher with a groundbreaking but difficult-to-replicate finding, the process is a high-risk gauntlet that can end a career. The costs of verification are fully borne by them, while the system offers little support, appearing as a Snare.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the institutional view, this process is an essential Rope that coordinates the entire scientific enterprise, ensuring that only reliable, vetted claims become canon. The extraction is viewed as a necessary cost for maintaining epistemic quality and stability.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An established lab head experiences the constraint as a Tangled Rope. They benefit from the stability and prestige it confers, but are also subject to its high costs and slow pace, making them both a beneficiary and a constrained participant.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical view recognizes the dual function: an indispensable coordination mechanism for building reliable knowledge, coupled with a significant, asymmetrically-applied extractive process that filters innovators based on resource availability and conformity, not just correctness.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.55 reflects the immense cost (in funding, time, and career opportunity) required to push a novel claim through the verification process. The suppression score of 0.65 reflects the powerful filtering function of peer review and funding bodies, which actively reject claims that have not met this standard. The process requires active enforcement through these institutional mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the institutional beneficiary, who sees a necessary Rope for quality control, and the individual innovator, who experiences a career-threatening Snare. The former externalizes the cost of verification, while the latter internalizes it completely. This gap is the core tension of the Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is from individual innovators (who provide novel claims and labor) towards the established institutions (which gain stability, prestige, and control over the canon of accepted knowledge). Beneficiaries are those who manage the process, while victims are those who must subject themselves to it with limited resources.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the severe and asymmetric costs it imposes. Conversely, classifying it as a pure Snare would ignore its undeniably critical function in creating robust, collective knowledge. The Tangled Rope classification correctly identifies that the coordination function is real, but it is achieved via a highly extractive and suppressive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_origin,
    'Is the high cost and slow pace of verification an irreducible property of probing complex reality, or an artifact of institutional incentives and risk aversion?',
    'Comparative analysis of verification velocity and success rates in fields with different funding models and institutional structures (e.g., centralized pharmaceutical research vs. decentralized open-source software development).',
    'If the cost is found to be an irreducible feature of nature, the constraint would be re-classified closer to a Mountain. If it is primarily an artifact of institutional structure, its Snare-like properties are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_origin, empirical, 'Distinguishing between irreducible natural difficulty and contingent institutional friction in the scientific verification process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_exp_r1, 1850, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1850, epistemic_process_of_verification_u3_exp_r1, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u3_exp_r1, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u3_exp_r1, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1850, epistemic_process_of_verification_u3_exp_r1, base_extractiveness, 1850, 0.2).
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u3_exp_r1, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u3_exp_r1, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_exp_r1, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r1, academic_peer_review).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r1, research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
