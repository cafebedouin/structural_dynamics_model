% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint models the scientific method's requirement for
 *   independent replication before a claim is accepted. While essential for
 *   coordinating the community towards reliable knowledge, this process
 *   imposes significant, asymmetric costs in terms of time, resources, and
 *   career risk. It functions as a filter but can also act as a gatekeeping
 *   mechanism that suppresses novel or disruptive findings.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Primary targets who bear the costs of verification (moderate/constrained).
 *   - Established Paradigms/Institutions: Primary beneficiaries who gain from a stable knowledge base (institutional/arbitrage).
 *   - Underfunded Labs: Victims excluded by the high resource costs of participation (powerless/trapped).
 *   - Scientific Community at Large: Secondary beneficiary of reliable knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r2, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r2, 0.62).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r2, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r2, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, established_paradigms).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r2, funding_agencies).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r2, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r2, underfunded_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r2, researchers_in_fringe_fields).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the researcher with a groundbreaking but difficult-to-replicate finding, the process is a high-stakes gauntlet with significant career risk. The costs of verification are borne almost entirely by them, making it feel like a Snare.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% From the perspective of a field's established institutions, the process is a pure coordination mechanism (Rope) that filters noise, ensures stability, and maintains the integrity of the shared knowledge base. They benefit from the high bar for entry.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A lab without the resources to perform costly replication experiments is effectively excluded from the verification process. For them, it is a Snare that enforces their marginalization.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% The analytical view recognizes both the essential coordination function and the asymmetric extraction. It is a Tangled Rope, a necessary process for building reliable knowledge that simultaneously imposes heavy, unevenly distributed costs.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.48) reflects the immense cost (funding, personnel, time) required to replicate a significant finding, a cost borne by the community but most acutely by the proposer. The suppression score (0.62) represents the powerful effect of peer review and the 'failure to replicate' in preventing claims from entering the canon. The process is actively enforced through funding and publication decisions.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the established institution, it's a Rope that ensures quality control. For the researcher trying to break new ground, it's a Snare that can end a career. The former experiences the system's stability, the latter its coercive filtering power.
 *
 * DIRECTIONALITY LOGIC:
 *   Benefits (reliable knowledge, paradigm stability) flow to the community and its established institutions. Costs (career risk, resource expenditure, risk of being unfairly dismissed) are concentrated on the individuals and labs proposing novel claims. This asymmetric distribution of costs and benefits is the core of the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would ignore the immense coercive power and career-destroying potential it holds. Classifying it as a pure Snare would deny its indispensable function in creating the reliable scientific knowledge on which society depends. The Tangled Rope classification correctly identifies it as a system with a genuine, vital coordination function that is structurally intertwined with high, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_cost_function,
    'Is the high cost of replication a necessary filter for quality (a feature) or a tool for incumbent gatekeeping (a bug)?',
    'Analysis of rejected-then-vindicated claims versus accepted-then-retracted claims, correlated with the initial cost and difficulty of replication.',
    'If primarily a necessary filter, the constraint's Rope-like properties are dominant. If primarily a gatekeeping tool, its Snare-like properties are dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_cost_function, empirical, 'Whether the high cost of replication is a functional filter or an incumbency-protection mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r2, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u2_exp_r2, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u2_exp_r2, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r2, public_trust_in_science).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
