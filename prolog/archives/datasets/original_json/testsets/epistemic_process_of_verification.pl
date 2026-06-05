% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification, []).

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
 *   constraint_id: epistemic_process_of_verification
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the standard scientific method of requiring
 *   independent replication and corroboration before a novel claim is
 *   accepted as fact. In fields like condensed matter physics, a
 *   groundbreaking claim (e.g., a new superconductivity mechanism or a
 *   spintronic effect like the inverse spin-valve in NbRe systems) is not
 *   canonized until other labs, often using different experimental setups,
 *   can reproduce the result. This process coordinates the entire scientific
 *   community towards a shared, reliable understanding of reality.
 *
 * KEY AGENTS:
 *   - The Scientific Community (institutional/beneficiary): Gains a reliable, self-correcting body of knowledge.
 *   - Researchers with Unreplicable Findings (powerless/victim): Their claims are invalidated, and their reputation is harmed.
 *   - Labs Performing Replication (organized/victim): Bear the direct financial and temporal costs of verification.
 *   - The Analytical Observer (analytical): Views the system as a whole, recognizing its function as a hybrid coordination/extraction mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification, 0.35).
domain_priors:suppression_score(epistemic_process_of_verification, 0.4).
domain_priors:theater_ratio(epistemic_process_of_verification, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification, extractiveness, 0.35).
narrative_ontology:constraint_metric(epistemic_process_of_verification, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(epistemic_process_of_verification, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification, general_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification, researchers_with_unreplicable_findings).
narrative_ontology:constraint_victim(epistemic_process_of_verification, individual_labs_funding_replication).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a high level, the process is a hybrid coordination/extraction mechanism. The costs (extraction) are a necessary, non-trivial, and asymmetrically applied component for the system to generate reliable knowledge.
constraint_indexing:constraint_classification(epistemic_process_of_verification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For the community as a whole, this process is the bedrock of its credibility. It is a foundational coordination mechanism (Rope) whose extractive costs are perceived as minimal relative to the benefit of a shared standard.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% For a lab with an incorrect, fraudulent, or unreplicable finding, the verification process is a Tangled Rope. It is not pure extraction (a Snare), but a costly filtering mechanism they are trapped within.
constraint_indexing:constraint_classification(epistemic_process_of_verification, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the lab with a correct but novel finding, the process is a Rope. It is a necessary coordination path to acceptance, and while it imposes costs, these are seen as part of the functional overhead of scientific progress, not as asymmetric extraction.
constraint_indexing:constraint_classification(epistemic_process_of_verification, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(epistemic_process_of_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.35) is significant, representing the real cost in time, funding, and labor required to perform replications. This cost is borne by the community but is particularly acute for the labs involved. Suppression (0.40) reflects the system's explicit function: to suppress the propagation of unverified claims. It is not a bug but a feature. The theater ratio is low because the process is overwhelmingly functional.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between the high-level analytical view and the powerless agent (both see a Tangled Rope) versus the institutional and organized actors who experience it as a functional Rope. For those whose claims are invalidated, it is a costly filtering mechanism (Tangled Rope). For the community and successful labs, it's a necessary coordination hurdle with manageable costs (Rope). The classification depends on whether an agent is primarily paying the cost of the filter or benefiting from the coordination it enables.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by who benefits from the shared standard of truth versus who pays the cost of upholding it. The 'scientific_community' is the clear beneficiary, gaining credibility and progress. The 'researchers_with_unreplicable_findings' are the primary victims, as the system is designed to extract and nullify their claims. Labs funding replication are secondary victims, paying the operational cost for the community's benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by correctly identifying the costs of coordination. Classifying it as a Tangled Rope acknowledges that even a beneficial system of coordination has coercive and extractive properties that cannot be ignored. A simple 'Rope' classification would obscure the real costs imposed on participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification, claim_inverse_spin_valve_nb_re).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
