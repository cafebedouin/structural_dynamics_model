% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u4_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u4_exp_r4, []).

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
 *   constraint_id: epistemic_process_of_verification_u4_exp_r4
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the standard scientific method of requiring
 *   independent replication and corroboration before a novel claim is
 *   accepted as fact. While this process is fundamental to coordinating the
 *   scientific community towards a shared, reliable understanding of reality,
 *   it imposes significant costs in terms of time, resources, and career
 *   risk, which are not borne equally by all participants.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary targets of extraction (powerless/trapped)
 *   - Labs with Novel/Difficult Claims: Secondary targets of extraction (moderate/constrained)
 *   - Established Labs & Tenured Faculty: Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - Scientific Publishers & Funding Agencies: Institutional beneficiaries who also manage the system (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r4, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u4_exp_r4, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r4, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r4, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u4_exp_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u4_exp_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u4_exp_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u4_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r4, established_labs).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r4, scientific_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r4, scientific_community_long_term).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r4, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r4, labs_with_novel_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior scientist whose career depends on publishing novel results quickly, the slow, expensive, and skeptical verification process acts as a high-stakes filter that can end their career. The costs are immediate and personal, while the benefits are diffuse and generational.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the perspective of a well-funded, tenured lab leader, the verification process is a crucial coordination mechanism that maintains the integrity and reliability of their field. They have the resources to act as verifiers, reinforcing their own authority and benefiting from the stability the system provides.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A funding body sees both sides. It relies on the verification process to ensure its investments lead to reliable knowledge (coordination), but is also acutely aware of the high costs, delays, and potential for the system to stifle truly disruptive innovation (extraction).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the essential coordination function of building a shared, reliable map of reality, while also seeing the clear, asymmetric extraction of resources, time, and career potential from new entrants to benefit established players and the system's long-term stability.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u4_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u4_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the substantial cost—in funding, time, and career viability—imposed on researchers attempting to validate novel claims. The high suppression (0.75) represents the lack of viable alternative paths to canonization within mainstream science; bypassing peer verification is institutionally impossible for most claims.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the early-career researcher, who experiences the process as a career-threatening Snare, and the established lab director, who views it as a necessary Rope for maintaining quality control. The former is trapped by the need to publish to survive, while the latter has the security and resources to benefit from the system's stability and gatekeeping function.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts from those with the most novel information but the least institutional power (junior researchers). It benefits those with the most institutional power and resources, who are positioned to act as gatekeepers (verifiers). This creates a flow of value (in the form of validated claims) from the periphery to the core of the scientific establishment.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope correctly identifies its dual nature. It avoids the error of calling it a pure Snare (which would ignore its vital coordination function in producing reliable knowledge) and the error of calling it a pure Rope (which would ignore the severe, asymmetric costs it imposes on its most vulnerable participants). The analysis highlights that the coordination function is bundled with a powerful system of extraction and gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_necessity,
    'Are the high career and resource costs of verification an irreducible feature of epistemology, or a contingent artifact of current institutional incentives (e.g., 'publish or perish')?',
    'Comparative analysis of alternative discovery/verification models, such as prize-based systems, registered reports, or decentralized science platforms, to measure if reliability can be maintained with lower extraction.',
    'If the costs are irreducible, the constraint has Mountain-like features. If they are contingent, the extractive elements constitute a Snare that could be engineered away, leaving a purer Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_necessity, empirical, 'Whether the high cost of verification is a necessary price for truth or a contingent institutional artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u4_exp_r4, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u4_exp_r4, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u4_exp_r4, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u4_exp_r4, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u4_exp_r4, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u4_exp_r4, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u4_exp_r4, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u4_exp_r4, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r4, academic_peer_review).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r4, university_tenure_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
