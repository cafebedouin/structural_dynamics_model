% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_exp_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_exp_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a claim is accepted. While this process
 *   coordinates the community towards a shared, reliable understanding of
 *   reality (a coordination function), it also imposes a significant,
 *   asymmetric burden of proof on individuals proposing novel, controversial,
 *   or difficult-to-replicate findings (an extraction function).
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers / Paradigm Challengers: Primary targets (powerless/trapped) who bear the cost of verification.
 *   - Scientific Community / Funding Agencies: Primary beneficiaries (institutional/arbitrage) who gain a reliable knowledge base.
 *   - Standard Researchers: Participants (moderate/constrained) who use the process for career progression and validation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r2, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u3_exp_r2, 0.65).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_exp_r2, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r2, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r2, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_exp_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_exp_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_exp_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r2, scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r2, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r2, society_at_large).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r2, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r2, paradigm_challengers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r2, underfunded_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a researcher with a novel, hard-to-replicate, or paradigm-shifting claim, the process is a Snare. It extracts immense resources (time, funding, reputation) and is enforced by an incumbent system that is structurally resistant to their claims. Exit is not an option if they wish to remain in the scientific community.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For an institutional body allocating capital, the verification process is a pure Rope. It is a low-overhead coordination mechanism to ensure that resources are directed towards reliable, cumulative knowledge, maximizing long-term return on investment and maintaining the integrity of the scientific enterprise.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical view recognizes the dual nature of the constraint. It is a Tangled Rope because its essential coordination function (Rope) is structurally coupled with asymmetric extraction (Snare) from innovators and those outside the established consensus. The system's stability is predicated on this extraction.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For a standard researcher working within the paradigm, the process acts as a Scaffold. It provides a structured pathway for career progression and contribution by verifying the work of others. It is a temporary support structure used to build the larger edifice of science, which they are a part of.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.48 reflects the immense cost in career capital, time, and resources demanded from innovators to overcome institutional inertia. The suppression of 0.65 is high because the explicit function of the process is to filter and suppress unverified claims from entering the canon, which is necessary for its coordination function to work.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Funding Agency, which sees a pure coordination Rope for efficient capital allocation, and the Paradigm Challenger, who experiences a career-threatening Snare. The analytical Tangled Rope classification captures the reality that the coordination is achieved *through* this asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs the benefits of epistemic certainty to the collective (the community, funders, society) while concentrating the costs of generating that certainty onto a small group of innovators and outsiders. The victims are those who must pay the high entry price to have their knowledge accepted, while the beneficiaries are those who consume the resulting validated knowledge.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope is crucial. A pure Rope classification would ignore the clear victims and the coercive power of peer review and funding denial. A pure Snare classification would fail to recognize the indispensable coordination function that produces reliable scientific knowledge. The Tangled Rope correctly identifies that the valuable coordination is structurally inseparable from the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_bias_vs_filter,
    'Is the high burden of proof on paradigm-challengers a necessary filter for quality, or a biased defense mechanism of the incumbent paradigm?',
    'Comparative historical analysis of the reception of ultimately successful vs. unsuccessful paradigm-shifting theories, controlling for experimental tractability.',
    'If primarily a necessary filter, the constraint is closer to a harsh Rope. If primarily a biased defense, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_bias_vs_filter, empirical, 'Distinguishing necessary epistemic rigor from incumbent paradigm defense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_exp_r2, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u3_exp_r2, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u3_exp_r2, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u3_exp_r2, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u3_exp_r2, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u3_exp_r2, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u3_exp_r2, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_exp_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r2, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r2, technological_development_cycles).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r2, pharmaceutical_approval_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
