% ============================================================================
% CONSTRAINT STORY: irreversible_policy_commitment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irreversible_policy_commitment, []).

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
 *   constraint_id: irreversible_policy_commitment
 *   human_readable: The Burned Bridge Protocol
 *   domain: political/economic
 *
 * SUMMARY:
 *   The Burned Bridge Protocol describes policy decisions intentionally
 *   designed to be irreversible, effectively 'burning bridges' to prevent
 *   future modifications. These policies often involve significant upfront
 *   investment, structural reorganizations, or constitutional amendments that
 *   make reversal politically or economically infeasible. The protocol
 *   benefits policy initiators by solidifying their agenda and creating
 *   lasting legacies, but it poses risks to future generations who may
 *   inherit unintended consequences or lack the flexibility to adapt to
 *   changing circumstances.
 *
 * KEY AGENTS:
 *   - Policy Initiators: The primary beneficiaries of the irreversible commitment.
 *   - Future Generations: The primary victims, bearing the long-term consequences.
 *   - Dissenting Minorities: Groups disadvantaged by the policy and unable to change it.
 *   - Legacy Institutions: Organizations that become responsible for enforcing or maintaining the policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irreversible_policy_commitment, 0.6).
domain_priors:suppression_score(irreversible_policy_commitment, 0.7).
domain_priors:theater_ratio(irreversible_policy_commitment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irreversible_policy_commitment, extractiveness, 0.6).
narrative_ontology:constraint_metric(irreversible_policy_commitment, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(irreversible_policy_commitment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irreversible_policy_commitment, snare).
narrative_ontology:human_readable(irreversible_policy_commitment, "The Burned Bridge Protocol").
narrative_ontology:topic_domain(irreversible_policy_commitment, "political/economic").

domain_priors:requires_active_enforcement(irreversible_policy_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, policy_initiators).
narrative_ontology:constraint_beneficiary(irreversible_policy_commitment, vested_interests).
narrative_ontology:constraint_victim(irreversible_policy_commitment, future_generations).
narrative_ontology:constraint_victim(irreversible_policy_commitment, dissenting_minorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations inherit the consequences of the irreversible policy, with no recourse for altering its course. They are trapped by the decisions of their predecessors.
constraint_indexing:constraint_classification(irreversible_policy_commitment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Dissenting minorities are constrained by the policy, but they may have some limited ability to influence its implementation or mitigate its effects.
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Policy initiators benefit from the entrenchment of their agenda, securing their legacy and power. They can arbitrage their position to ensure the policy's survival.
constraint_indexing:constraint_classification(irreversible_policy_commitment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Established institutions may find themselves maintaining the policy long after its original purpose has faded, becoming a piton in the institutional landscape.
constraint_indexing:constraint_classification(irreversible_policy_commitment, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, the observer sees the policy as a tangled rope, combining elements of coordination (establishing a framework) with extraction (limiting future choices).
constraint_indexing:constraint_classification(irreversible_policy_commitment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irreversible_policy_commitment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irreversible_policy_commitment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irreversible_policy_commitment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irreversible_policy_commitment, TR),
    TR >= 0.70.

:- end_tests(irreversible_policy_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The policy extracts from future generations by limiting their options. Suppression (0.70): High. The goal is to make reversal nearly impossible. Theater Ratio (0.30): Low. The emphasis is on real structural changes rather than performative displays.
 *
 * PERSPECTIVAL GAP:
 *   Future generations (snare) are trapped. Policy initiators (rope) benefit. Dissenting minorities (tangled rope) are constrained, but have some agency. Legacy institutions (piton) become relics maintaining the policy. The analytical observer sees both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (policy initiators) experience the constraint as coordination, enabling their long-term goals. Victims (future generations) experience it as a snare, limiting their choices and opportunities. Dissenting minorities experience it as a tangled rope, with some capacity to influence implementation but limited ability to reverse the policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversal_cost_threshold,
    'What is the true cost of reversing the policy, considering both economic and social factors?',
    'Cost-benefit analysis of potential reversal scenarios, factoring in public opinion and political will.',
    'If reversal cost is lower than perceived, the policy may be re-evaluated. If higher, the policy is effectively locked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_cost_threshold, empirical, 'Determining the true cost of reversing the policy.').

omega_variable(
    adaptability_of_institutions,
    'How adaptable are the institutions affected by the policy to changing circumstances?',
    'Analysis of institutional capacity for innovation and adaptation, considering factors such as funding, leadership, and culture.',
    'If institutions are highly adaptable, the policy may be less rigid and more responsive to future needs. If not, the policy may become a constraint on progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptability_of_institutions, empirical, 'Assessing the adaptability of affected institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irreversible_policy_commitment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irre_tr_t0, irreversible_policy_commitment, theater_ratio, 0, 0.1).
narrative_ontology:measurement(irre_tr_t5, irreversible_policy_commitment, theater_ratio, 5, 0.2).
narrative_ontology:measurement(irre_tr_t10, irreversible_policy_commitment, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(irre_be_t0, irreversible_policy_commitment, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(irre_be_t5, irreversible_policy_commitment, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(irre_be_t10, irreversible_policy_commitment, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irreversible_policy_commitment, enforcement_mechanism).
narrative_ontology:affects_constraint(irreversible_policy_commitment, policy_path_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
