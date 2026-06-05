% ============================================================================
% CONSTRAINT STORY: ibm_shield_contract_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ibm_shield_contract_2026, []).

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
 *   constraint_id: ibm_shield_contract_2026
 *   human_readable: IBM SHIELD IDIQ Program Contract
 *   domain: technological/political
 *
 * SUMMARY:
 *   IBM has secured a massive IDIQ (Indefinite Delivery, Indefinite Quantity)
 *   contract for the SHIELD program, intended to automate the military OODA
 *   loop (Observe, Orient, Decide, Act) via AI-enabled sensing and command.
 *   This contract creates a complex interplay of benefits and costs for
 *   different stakeholders. IBM benefits from guaranteed revenue, while the
 *   Pentagon aims to modernize its operations. However, competing contractors
 *   are disadvantaged, and public oversight faces challenges due to limited
 *   transparency.
 *
 * KEY AGENTS:
 *   - IBM: Primary beneficiary (institutional/arbitrage) - Secures substantial revenue and market position.
 *   - Pentagon Leadership: Secondary beneficiary (institutional/constrained) - Aims to modernize military operations, but becomes dependent on a single vendor.
 *   - Competing Contractors: Primary victim (powerless/trapped) - Shut out of significant modernization efforts.
 *   - Public Oversight: Secondary victim (moderate/constrained) - Faces challenges in scrutinizing the contract's implementation and impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ibm_shield_contract_2026, 0.55).
domain_priors:suppression_score(ibm_shield_contract_2026, 0.45).
domain_priors:theater_ratio(ibm_shield_contract_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ibm_shield_contract_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ibm_shield_contract_2026, tangled_rope).
narrative_ontology:human_readable(ibm_shield_contract_2026, "IBM SHIELD IDIQ Program Contract").
narrative_ontology:topic_domain(ibm_shield_contract_2026, "technological/political").

domain_priors:requires_active_enforcement(ibm_shield_contract_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, ibm).
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, pentagon_leadership).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, competing_contractors).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, public_oversight).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% IBM views the contract as a rope, enabling them to coordinate with the Pentagon and secure significant revenue streams. They benefit from the contract's size and scope, allowing them to allocate resources and expertise strategically. Their exit option is arbitrage, as they can leverage the contract to develop new technologies and expand their market presence.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% Pentagon leadership sees the contract as a tangled rope. It provides a means to modernize military operations through AI and automation (coordination), but also constrains them by creating dependence on a single vendor and potentially limiting competition (extraction). Their exit options are constrained due to the strategic importance of the program and the sunk costs associated with IBM's involvement. Generational time horizon due to the multi-year commitment and long-term impact on military strategy.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Competing contractors experience the contract as a snare. They are effectively shut out of a significant portion of military modernization efforts, limiting their ability to compete for future contracts and innovate in the AI-enabled defense space. Their exit options are trapped, as the size and scope of the IBM contract create a significant barrier to entry. Civilizational time horizon reflects the potential for long-term dominance by IBM in this sector.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% Public oversight experiences the contract as a tangled rope. On the one hand, the public benefits from a more modern and efficient military. On the other hand, the size and scope of the contract, along with limited transparency, can hinder effective public scrutiny and accountability. Constrained exit, as access to detailed contract information is often limited. Biographical time horizon as oversight groups focus on current contract performance and impacts.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Analytical observer views the contract as a tangled rope, balancing potential benefits of technological advancement with risks of vendor lock-in and reduced competition. The observer recognizes the long-term implications of this contract and its potential impact on the broader technological landscape. Civilizational time horizon due to the systemic consequences.
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_contract_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ibm_shield_contract_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ibm_shield_contract_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The contract provides IBM with a significant advantage, but also constrains competing contractors. The public bears indirect costs through potentially reduced competition and limited oversight. Suppression (0.45): Moderate. The contract's size and complexity, coupled with limited transparency, suppress opportunities for competing contractors and public scrutiny. Theater ratio (0.30): Relatively low. The SHIELD program has significant real-world implications for military modernization, reducing the 'theater' aspect.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ significantly based on the stakeholders' positions. IBM sees a rope, enabling coordination and revenue. Pentagon leadership sees a tangled rope, balancing modernization goals with vendor dependence. Competing contractors see a snare, limiting their opportunities. Public oversight sees a tangled rope, weighing the benefits of modernization against transparency concerns.
 *
 * DIRECTIONALITY LOGIC:
 *   IBM benefits directly from the contract, giving them a low directionality value. Competing contractors bear the costs of being excluded, resulting in a high directionality value. Pentagon leadership and public oversight experience a mix of benefits and costs, leading to intermediate directionality values. The derived directionality values align with the structural relationships among the stakeholders.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope resolves the mandatrophy by recognizing the dual nature of the contract. It is not purely extractive (snare) because it provides a coordination function for military modernization. However, it is not purely beneficial (rope) because it limits competition and public scrutiny. The tangled rope classification captures this complex interplay of benefits and costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oversight_transparency_balance,
    'What level of transparency is needed to ensure effective public oversight without compromising national security?',
    'Implementing reporting requirements and establishing independent review boards with appropriate security clearances.',
    'Determines whether the contract becomes a more transparent tool for modernization or an opaque source of rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_transparency_balance, preference, 'Balance between transparency and national security').

omega_variable(
    innovation_impact,
    'Does this contract genuinely accelerate innovation in AI and defense, or does it stifle competition and create vendor lock-in?',
    'Assessing the quality of IBM''s deliverables, benchmarking against other development efforts, and monitoring the impact on the broader defense technology ecosystem.',
    'Determines if the contract is a positive force for progress or an impediment to innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_impact, empirical, 'Impact of contract on AI and defense innovation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ibm_shield_contract_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ibm__tr_t0, ibm_shield_contract_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ibm__tr_t5, ibm_shield_contract_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ibm__tr_t10, ibm_shield_contract_2026, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ibm__be_t0, ibm_shield_contract_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ibm__be_t5, ibm_shield_contract_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ibm__be_t10, ibm_shield_contract_2026, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ibm_shield_contract_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
