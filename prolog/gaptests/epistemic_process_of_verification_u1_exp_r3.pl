% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u1_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u1_exp_r3, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_process_of_verification_u1_exp_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a novel claim is accepted. While essential
 *   for coordinating the scientific community towards a reliable, shared
 *   understanding of reality, it also functions as a powerful and costly
 *   filter. It imposes significant burdens on the originators of new claims,
 *   and its enforcement by established institutions creates an asymmetric
 *   power dynamic.
 *
 * KEY AGENTS:
 *   - Originating Lab/Scientist: The primary target of the constraint, who must bear the costs of verification (powerless/trapped).
 *   - Scientific Institutions (Journals, Funding Bodies): The primary beneficiaries and enforcers, who maintain the integrity of the scientific record (institutional/arbitrage).
 *   - The Scientific Community at Large: A distributed beneficiary that receives a more reliable knowledge base.
 *   - Proposers of Unreplicable Claims: The primary victims, whose work is filtered out by the process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r3, 0.38).
domain_priors:suppression_score(epistemic_process_of_verification_u1_exp_r3, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u1_exp_r3, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r3, extractiveness, 0.38).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r3, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u1_exp_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u1_exp_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u1_exp_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u1_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r3, scientific_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r3, the_scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r3, downstream_technology_users).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r3, proposers_of_unreplicable_claims).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r3, resource_constrained_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel, hard-to-replicate finding, the process is a high-stakes gauntlet that can end a career. They are trapped, as there is no other path to acceptance.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For institutions that curate scientific knowledge, this is a pure coordination mechanism to ensure quality control and maintain the integrity of the scientific record. The costs are externalized to the researchers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% For a peer lab, the process is both a coordinating standard for competition and a potential weapon. The ability to confirm or fail to replicate a competitor's result is a source of power.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical view sees both the essential coordination function that builds reliable knowledge and the asymmetric extraction of time, resources, and reputation from individual claimants, filtering out both error and premature discovery.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u1_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(epistemic_process_of_verification_u1_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.38) reflects the significant, non-reciprocal cost in time, resources, and career risk imposed on the claimant for the benefit of the entire community. Suppression (0.75) is high because, within mainstream science, there is no viable alternative to independent verification; one cannot simply opt out. The low theater ratio (0.15) indicates the process is highly functional, despite occasional performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the institution sees a pure coordination Rope, essential for its function. The individual claimant, facing immense pressure and career risk, experiences it as a Snare. The analytical observer recognizes the dual nature of the system, classifying it as a Tangled Rope where a vital coordination function is intertwined with coercive, extractive elements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the flow of costs and benefits. The scientific community and its institutions are the beneficiaries, gaining a high-integrity knowledge commons. The victims are the individual innovators who bear the direct costs of the verification process, and especially those whose claims are rejected, regardless of the reason (error, fraud, or simply being too far ahead of the curve for contemporary methods to replicate).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of a Tangled Rope that is often mis-claimed as a pure Rope. The narrative of 'science's self-correcting nature' obscures the coercive and extractive burden placed on individuals. By identifying the victims and the high suppression, the Tangled Rope classification prevents the mandatrophy of mislabeling a costly, high-stakes filtering mechanism as a purely benign coordination protocol.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u1_exp_r3, 1660, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u1_exp_r3, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u1_exp_r3, academic_peer_review).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u1_exp_r3, scientific_funding_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
