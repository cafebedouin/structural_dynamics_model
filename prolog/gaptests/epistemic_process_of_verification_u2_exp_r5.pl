% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a claim is accepted. While it serves a
 *   vital coordination function by creating a shared, reliable body of
 *   knowledge, it also imposes significant costs (time, funding, career risk)
 *   on those proposing novel or paradigm-shifting findings. The process is
 *   enforced by institutional gatekeepers like journal editors, peer
 *   reviewers, and funding agencies.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Researchers with new findings (Victim/Moderate/Constrained)
 *   - Scientific Establishment: Journal editors, funding bodies, senior academics (Beneficiary/Institutional/Arbitrage)
 *   - Early Career Researchers: Graduate students and postdocs (Victim/Powerless/Trapped)
 *   - Scientific Community & Public: Broader society that benefits from reliable science (Beneficiary)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r5, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r5, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r5, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, established_research_groups).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, downstream_technology_users).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r5, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r5, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a groundbreaking but difficult-to-replicate finding, the process feels like an extractive, career-threatening gauntlet.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% From the perspective of funding bodies, journal editors, and established labs, this is a necessary coordination mechanism to ensure the reliability and integrity of the scientific record.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analyst sees both the essential coordination function and the asymmetric extraction of resources and career potential from innovators at the frontier.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A graduate student or postdoc whose career depends on validating a novel result is trapped by the process, facing immense pressure with few resources or alternative paths.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (ε=0.48) reflects the significant investment of time, resources, and career capital demanded from innovators, which is not always recouped. The high suppression (0.75) indicates the near-impossibility of establishing a major scientific claim outside this institutional framework. The process is functional, not primarily performative, hence the low theater ratio (0.25).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the institutional beneficiary sees a pure Rope, a tool for maintaining quality control. The individual researcher at the frontier experiences it as a Snare, an extractive and often arbitrary barrier to progress. The analytical view must reconcile these by recognizing the system's dual nature as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs credibility, funding, and prestige towards claims that can be easily replicated within the existing paradigm and away from those that are novel, difficult, or challenging to the status quo. Established research groups, skilled in replication and navigating the system, are primary beneficiaries. Proposers of novel claims, especially those with limited resources, are the primary victims of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the immense coercive power and extractive costs it imposes on individuals. Conversely, calling it a pure Snare would ignore its demonstrably effective and necessary function in coordinating scientific consensus. The Tangled Rope classification correctly identifies that a genuine, valuable coordination function is intertwined with a powerful, asymmetric extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_origin,
    'Is the high cost of verification an unavoidable epistemic burden (Mountain-like) or an artifact of the current institutional structure and incentives (Snare-like)?',
    'Comparative analysis of fields with different verification standards, or development of new low-cost verification technologies (e.g., AI-driven experiment design).',
    'If the cost is fundamentally unavoidable, the constraint is closer to a pure Rope. If it's largely an institutional artifact, it's a reformable Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_origin, empirical, 'Whether the high cost of verification is an unavoidable epistemic burden or an artifact of institutional design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r5, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r5, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r5, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r5, pharmaceutical_drug_approval).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
