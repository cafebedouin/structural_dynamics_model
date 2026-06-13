% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__council_communist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Workers' Councils as Direct Democratic Organs (Council Communist Reading)
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint describes the 'council communist' reading of
 *   revolutionary method, where workers' councils (soviets) serve as the
 *   primary organs of direct democracy, replacing both the capitalist state
 *   and the vanguard party. Power is decentralized and held by federated
 *   workplace assemblies. This reading emphasizes self-management and
 *   anti-authoritarianism, contrasting sharply with state-centric or
 *   party-led revolutionary models. The internal extractiveness within the
 *   council structure is low, reflecting its ideal of direct worker control,
 *   but it faces extremely high external suppression from both existing state
 *   power and rival revolutionary factions.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiary (organized/arbitrage) — exercise direct power
 *   - local_communities: Secondary beneficiary (organized/mobile) — benefit from decentralized control
 *   - state_bureaucrats: Primary victim (institutional/trapped) — lose power and function
 *   - vanguard_party_officials: Primary victim (institutional/trapped) — lose claim to leadership
 *   - capitalist_class: Primary victim (institutional/trapped) — expropriated and disempowered
 *   - democratic_gradualists: Excluded (organized/constrained) — their method is rejected
 *   - vanguard_party_adherents: Excluded (organized/constrained) — their method is rejected
 *   - council_communist_theorists: Observer (analytical/analytical) — analyze and advocate for this model
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.85).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils as Direct Democratic Organs (Council Communist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'b014141b-d8b7-4371-82f0-98acc4548781').
narrative_ontology:cs_kernel_codification('b014141b-d8b7-4371-82f0-98acc4548781', formalized).
narrative_ontology:cs_authority_grounding('b014141b-d8b7-4371-82f0-98acc4548781', practice).
narrative_ontology:cs_interpretation_layer_present('b014141b-d8b7-4371-82f0-98acc4548781').
narrative_ontology:cs_reading_relation('b014141b-d8b7-4371-82f0-98acc4548781', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('b014141b-d8b7-4371-82f0-98acc4548781', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('b014141b-d8b7-4371-82f0-98acc4548781', foundational, direct_worker_self_management_is_the_goal).
narrative_ontology:cs_axiom_status(direct_worker_self_management_is_the_goal, holdable).
narrative_ontology:cs_axiom_grounding('b014141b-d8b7-4371-82f0-98acc4548781', direct_worker_self_management_is_the_goal, deontological).
narrative_ontology:cs_axiom('b014141b-d8b7-4371-82f0-98acc4548781', foundational, state_and_party_are_instruments_of_class_rule).
narrative_ontology:cs_axiom_status(state_and_party_are_instruments_of_class_rule, holdable).
narrative_ontology:cs_axiom_grounding('b014141b-d8b7-4371-82f0-98acc4548781', state_and_party_are_instruments_of_class_rule, empirically_contingent).
narrative_ontology:cs_reference_frame('b014141b-d8b7-4371-82f0-98acc4548781', federated_council_democracy).
narrative_ontology:cs_drift_state('b014141b-d8b7-4371-82f0-98acc4548781', contemporary_political_discourse, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b014141b-d8b7-4371-82f0-98acc4548781', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, local_communities).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary agents of the council communist system, exercising direct democratic control over their workplaces and federating upwards. They benefit from self-management, direct allocation of resources, and the absence of external exploitation. Their exit options are high within the system, as they are the system.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, arbitrage, local).

% Benefit from decentralized decision-making, direct provision of social services, and democratic control over local resources. They are closely integrated with the worker collectives and participate in local council structures. Their benefits are direct and tangible.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, local_communities, beneficiary,
    organized, biographical, mobile, local).

% These agents represent the administrative and coercive apparatus of the capitalist state. The council communist reading aims to dismantle their power and render their roles obsolete. They face existential threat and loss of status, power, and livelihood. Their exit options are extremely limited, as their entire identity and function are tied to the state.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, generational, trapped, national).

% These agents represent the leadership of a centralized revolutionary party. The council communist reading rejects their claim to lead the revolution and replace the state with a party-state. They face loss of their ideological project, power, and organizational structure. Their exit options are limited, as their identity is fused with the party's role.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, generational, trapped, national).

% The owners of capital and means of production. The council communist reading advocates for their expropriation and the abolition of private property. They face total loss of their economic base, power, and social position. Their exit options are effectively zero within the proposed system.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_class, payer,
    institutional, generational, trapped, global).

% Advocate for achieving socialism through parliamentary means and gradual reforms within existing democratic structures. Their approach is fundamentally rejected by the council communist reading, which sees the state as an instrument of class rule to be overthrown, not reformed. They are excluded from the council communist's vision of revolutionary transformation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualists, excluded,
    organized, biographical, constrained, national).

% Believe that a disciplined, centralized vanguard party is necessary to lead the working class to revolution and establish a dictatorship of the proletariat. Their top-down, party-centric approach is explicitly rejected by the council communist reading, which prioritizes direct worker self-management. They are excluded from the council communist's vision of revolutionary organization.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_adherents, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable direct democratic control and self-management of production and social life by federated workplace and community assemblies, replacing hierarchical state and capitalist structures with decentralized, horizontal coordination.
% TRANSFER_FUNCTION: Transfers decision-making power and control over resources from state bureaucracies, vanguard parties, and the capitalist class directly to autonomous worker collectives and local communities. It also transfers surplus value from capital to labor.
% ABSENT_VOICES: State bureaucrats, vanguard party officials, and the capitalist class are actively disempowered and excluded from the new political and economic order. Democratic gradualists and vanguard party adherents are excluded ideologically, as their methods are deemed incompatible with direct council democracy.
% DISAPPEARANCE_RATIONALE: If the vision of workers' councils as direct democratic organs vanished, the entire revolutionary project it represents would collapse. Power would remain with the capitalist state or be seized by a vanguard party, and the fundamental structure of society would not be transformed in the way envisioned. The world would revert to, or remain in, a state of hierarchical control.
% FOUNDING_PROBLEM: The problem of alienated labor, capitalist exploitation, and the authoritarian nature of both the capitalist state and centralized vanguard parties, which prevent genuine working-class self-emancipation.
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists and historical accounts of workers' struggles (e.g., Kronstadt, Hungarian Revolution of 1956) corroborate the persistence of these problems and the recurring impulse for direct worker control, independent of state or party structures. Critics from both capitalist and vanguard party perspectives acknowledge the historical existence of these problems, though they dispute the council communist solution.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__council_communist_reading_tests).
:- end_tests(manifesto_revolutionary_method__council_communist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because, internally, it aims for genuine coordination and self-management with minimal extraction from its participants (autonomous worker collectives). The low extractiveness (0.25) reflects this ideal. However, its implementation faces immense suppression (0.85) from entrenched state power and competing revolutionary ideologies (vanguard parties, democratic gradualists). This external suppression is what makes its realization so difficult and contested. The theater ratio is low (0.1) because the model is fundamentally about direct action and genuine transformation, not performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of autonomous worker collectives, the council system is a pure coordination mechanism, empowering them directly. From the perspective of state bureaucrats and vanguard party officials, it is a direct threat to their power and legitimacy, requiring active suppression. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a Rope-like structure and victims experiencing a Snare-like structure due to high effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and local communities are direct beneficiaries (d=0.0-0.1) as they gain direct control and self-management. State bureaucrats, vanguard party officials, and the capitalist class are direct targets/victims (d=0.9-1.0) as the system aims to dismantle their power and expropriate their assets. The high external suppression ensures that these victims experience the constraint as highly extractive. Democratic gradualists and vanguard party adherents are excluded, as their methods are incompatible with the council communist approach, making them indirect targets of the constraint's ideological enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a proposed revolutionary method, so mandatrophy is not yet applicable in the traditional sense of an existing constraint losing its function. However, if a council system were established and then ossified into a new bureaucracy, it would signal mandatrophy, where the original mandate of direct democracy had atrophied into a new form of extraction. The low theater ratio and high resistance indicate it is far from such a state, being an actively contested and revolutionary proposal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_communist_vs_vanguard_party,
    'Is the direct democratic authority of federated workers'' councils compatible with the centralized authority of a vanguard party?',
    'Historical analysis of revolutionary movements where both forms emerged; theoretical reconciliation of direct democracy with centralized revolutionary leadership.',
    'If incompatible, this reading forecloses the vanguard_rupture_reading, highlighting a fundamental structural conflict in revolutionary method. If compatible, it suggests a potential for hybrid forms or a sequential transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(council_communist_vs_vanguard_party, conceptual, 'This constraint is the council_communist_reading of the manifesto_revolutionary_method kernel. It posits direct worker control, which fundamentally conflicts with the vanguard party''s claim to lead the revolution.').

omega_variable(
    council_communist_vs_democratic_gradualism,
    'Can the direct democratic power of workers'' councils be achieved through gradual, parliamentary means, or does it require a revolutionary rupture?',
    'Empirical observation of attempts to implement council-style democracy within existing parliamentary systems; theoretical analysis of the state''s capacity for self-transformation.',
    'If a revolutionary rupture is necessary, this reading forecloses the democratic_gradualism_reading, asserting the impossibility of achieving council power through existing state structures. If gradualism is possible, it suggests a less confrontational path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_communist_vs_democratic_gradualism, conceptual, 'This constraint is the council_communist_reading of the manifesto_revolutionary_method kernel. It emphasizes direct, extra-parliamentary power, contrasting with the democratic gradualism approach.').

omega_variable(
    internal_coordination_efficiency,
    'Can federated workplace assemblies effectively coordinate complex economic and social functions at scale without developing new forms of bureaucracy or hierarchy?',
    'Empirical study of large-scale federated direct democratic systems; theoretical modeling of information flow and decision-making in such structures.',
    'If new hierarchies inevitably emerge, the internal extractiveness of the council system would rise, potentially reclassifying it as a tangled_rope or even a snare from the perspective of individual workers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_coordination_efficiency, empirical, 'Assesses the practical viability and internal purity of the council system itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on workers' councils as direct democratic organs. It is structurally distinct from the vanguard_rupture_reading (party-led state power) and the democratic_gradualism_reading (parliamentary reform), which are modeled as separate constraints due to their differing structural claims and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
