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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Council Communist Model of Workers' Soviets
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint describes the council communist reading of revolutionary
 *   method, where workers' councils (soviets) serve as the direct democratic
 *   organs replacing both the capitalist state and the vanguard party. Power
 *   is held by federated workplace and community assemblies. Within the
 *   council structure, extraction is low (0.25) as it aims for
 *   self-management. However, external suppression is extremely high (0.88)
 *   due to active resistance from both capitalist states and rival vanguard
 *   parties, who see this model as a threat to their own power claims. The
 *   claimed type is 'rope' because, internally, it functions as a
 *   coordination mechanism for autonomous collectives, but its existence is
 *   heavily contested externally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.88).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.92).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Model of Workers' Soviets").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '12e79ce3-e2b3-46cf-845e-ba699e4a002d').
narrative_ontology:cs_kernel_codification('12e79ce3-e2b3-46cf-845e-ba699e4a002d', implicit).
narrative_ontology:cs_authority_grounding('12e79ce3-e2b3-46cf-845e-ba699e4a002d', practice).
narrative_ontology:cs_reading_relation('12e79ce3-e2b3-46cf-845e-ba699e4a002d', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('12e79ce3-e2b3-46cf-845e-ba699e4a002d', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('12e79ce3-e2b3-46cf-845e-ba699e4a002d', foundational, direct_worker_democracy_is_sole_legitimate_power).
narrative_ontology:cs_axiom_status(direct_worker_democracy_is_sole_legitimate_power, holdable).
narrative_ontology:cs_axiom_grounding('12e79ce3-e2b3-46cf-845e-ba699e4a002d', direct_worker_democracy_is_sole_legitimate_power, deontological).
narrative_ontology:cs_axiom('12e79ce3-e2b3-46cf-845e-ba699e4a002d', foundational, state_and_party_are_inherently_authoritarian).
narrative_ontology:cs_axiom_status(state_and_party_are_inherently_authoritarian, holdable).
narrative_ontology:cs_axiom_grounding('12e79ce3-e2b3-46cf-845e-ba699e4a002d', state_and_party_are_inherently_authoritarian, empirically_contingent).
narrative_ontology:cs_reference_frame('12e79ce3-e2b3-46cf-845e-ba699e4a002d', federated_worker_assemblies).
narrative_ontology:cs_drift_state('12e79ce3-e2b3-46cf-845e-ba699e4a002d', historical_repression_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('12e79ce3-e2b3-46cf-845e-ba699e4a002d', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, local_community_assemblies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, private_capital_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly control their workplaces and federate with other councils, exercising power through direct democracy. They benefit from self-management and the abolition of wage labor, but face external threats from rival political forces.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, local).

% Participate in federated council structures, coordinating social and economic life beyond the workplace. They benefit from direct democratic control over local resources and services.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, local_community_assemblies, beneficiary,
    organized, generational, mobile, local).

% Their power and positions are directly abolished by the council system. They represent the old order that the council communist reading seeks to dismantle, and would actively resist its establishment.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Their claim to lead the revolution and establish a 'dictatorship of the proletariat' is rejected. The council communist reading sees them as a new form of authoritarianism, and their power would be dissolved by direct worker control.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, biographical, trapped, national).

% Their ownership of the means of production is directly expropriated by the worker councils. They represent the economic system that the council communist reading seeks to overthrow.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, private_capital_owners, payer,
    powerful, biographical, trapped, global).

% Advocate for achieving socialism through existing parliamentary means. Their approach is seen as insufficient and ultimately co-opted by the capitalist state, and they are excluded from the revolutionary method proposed by council communists.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualists, excluded,
    organized, generational, constrained, national).

% Believe in the necessity of a disciplined vanguard party to lead the revolution. Their hierarchical and centralized model is fundamentally opposed to the direct democratic principles of council communism, leading to their exclusion from this model of revolutionary organization.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_adherents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To organize production and distribution directly through federated worker and community councils, ensuring democratic control over economic and social life without state or party mediation.
% TRANSFER_FUNCTION: Transfers control over the means of production and political decision-making power from capitalist owners, state bureaucrats, and vanguard party officials to directly elected and recallable delegates of worker and community councils.
% ABSENT_VOICES: Both democratic gradualists (who seek change through existing state structures) and vanguard party adherents (who advocate for party-led state power) are excluded from this model, as their methods are seen as antithetical to direct worker democracy. They would argue for their respective approaches as more 'realistic' or 'effective'.
% DISAPPEARANCE_RATIONALE: If the council communist model of direct worker power vanished, the vacuum would be filled by either the re-establishment of a capitalist state, a vanguard party seizing control, or a descent into uncoordinated chaos, as the fundamental structures of power and production would be unaddressed.
% FOUNDING_PROBLEM: The historical failure of both parliamentary socialism to achieve genuine worker emancipation and vanguard parties to avoid authoritarianism, leading to a search for a truly democratic and decentralized revolutionary method.
% FOUNDING_PROBLEM_CORROBORATION: Historians of revolutionary movements and critics of state socialism corroborate the persistent problem of authoritarianism and co-optation in both state-centric and party-centric models, supporting the council communist search for an alternative.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness reflects the ideal of direct worker control and the absence of a separate, extracting class or bureaucracy within the council system itself. The high suppression is a direct consequence of the historical context: the council communist model faced violent repression from both capitalist forces and Bolshevik-led states. Theater ratio is low because the model is a genuine attempt at direct democracy, not a performative cover. Accessibility collapse is high because the model fundamentally reconfigures social relations, making a return to previous forms difficult once established. Resistance is also high, reflecting the intense opposition it faced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the worker collectives, this is a pure coordination mechanism for liberation. From the perspective of the state and party officials, it is a direct threat to their existence, requiring maximum suppression. The engine's classification will reflect this internal 'rope' function versus the external 'snare' or 'tangled_rope' experience for those resisting it.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and local community assemblies are the primary beneficiaries, gaining direct control and self-management. Capitalist state bureaucrats, vanguard party officials, and private capital owners are the victims, as their power and property are directly abolished. Democratic gradualists and vanguard party adherents are excluded, as their methods are fundamentally incompatible with the council communist approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_coordination_vs_external_suppression,
    'Is the low internal extractiveness of the council model sustainable under conditions of high external suppression, or would it inevitably lead to internal consolidation and new forms of extraction?',
    'Historical analysis of attempts to implement council communism in isolation, or counterfactual modeling of a successful, globally federated council system.',
    'If internal extractiveness rises under external pressure, the model''s claimed ''rope'' nature would be reclassified as a ''tangled_rope'' or ''snare'' due to the necessity of internal coercion to survive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_coordination_vs_external_suppression, empirical, 'Sustainability of low internal extraction under high external threat.').

omega_variable(
    legitimacy_of_revolutionary_violence,
    'Is the use of revolutionary violence to establish the council system a legitimate act of self-defense against existing extractive structures, or does it inherently create a new form of suppression?',
    'Conceptual analysis of just war theory applied to revolutionary contexts, or empirical study of post-revolutionary societies that employed violence.',
    'If revolutionary violence is deemed inherently suppressive, the ''rope'' classification would be challenged by the implicit ''snare'' of its founding, leading to a re-evaluation of its base suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_revolutionary_violence, conceptual, 'Ethical and structural implications of revolutionary violence in establishing the council system.').

omega_variable(
    role_of_technology_in_decentralization,
    'To what extent would modern communication and coordination technologies alter the feasibility and internal dynamics of federated council systems, potentially reducing coordination costs or increasing vulnerability to external attack?',
    'Simulation and pilot projects of technologically-augmented direct democratic structures, or comparative analysis with historical attempts.',
    'If technology significantly reduces coordination costs, the ''rope'' classification would be strengthened. If it creates new vulnerabilities, the suppression metric might need adjustment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(role_of_technology_in_decentralization, empirical, 'Impact of modern technology on council communist feasibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1923).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(mani_tr_t1919, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(mani_tr_t1921, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1921, 0.05).
narrative_ontology:measurement(mani_tr_t1923, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1923, 0.05).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(mani_be_t1919, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement(mani_be_t1921, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1921, 0.24).
narrative_ontology:measurement(mani_be_t1923, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1923, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.7).
narrative_ontology:measurement(mani_su_t1919, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1919, 0.8).
narrative_ontology:measurement(mani_su_t1921, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1921, 0.85).
narrative_ontology:measurement(mani_su_t1923, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1923, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on the council communist model. It is structurally distinct from the vanguard party and democratic gradualist readings, which represent alternative approaches to revolutionary change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
