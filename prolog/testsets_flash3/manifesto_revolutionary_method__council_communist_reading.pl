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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Council Communist Model of Direct Democracy
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint describes the council communist reading of revolutionary
 *   method, advocating for workers' councils (soviets) as the primary organs
 *   of direct democracy, replacing both the capitalist state and any vanguard
 *   party. Power is held by federated workplace and community assemblies.
 *   This reading emphasizes decentralized coordination and self-emancipation,
 *   rejecting both parliamentary gradualism and party-led state socialism.
 *   The low internal extractiveness (0.25) reflects the ideal of direct
 *   democratic control within the council system, but the high external
 *   suppression (0.88) reflects the intense opposition from both capitalist
 *   forces and rival socialist factions (vanguard parties, democratic
 *   gradualists) that would seek to crush or co-opt such a system.
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
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Model of Direct Democracy").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '4078c36f-31fe-43e1-8cc6-7b9766f4b3e1').
narrative_ontology:cs_kernel_codification('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', implicit).
narrative_ontology:cs_authority_grounding('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', practice).
narrative_ontology:cs_interpretation_layer_present('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1').
narrative_ontology:cs_reading_relation('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', foundational, direct_democracy_is_the_only_true_proletarian_power).
narrative_ontology:cs_axiom_status(direct_democracy_is_the_only_true_proletarian_power, holdable).
narrative_ontology:cs_axiom_grounding('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', direct_democracy_is_the_only_true_proletarian_power, deontological).
narrative_ontology:cs_axiom('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', foundational, the_state_is_an_instrument_of_class_rule_to_be_smashed).
narrative_ontology:cs_axiom_status(the_state_is_an_instrument_of_class_rule_to_be_smashed, holdable).
narrative_ontology:cs_axiom_grounding('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', the_state_is_an_instrument_of_class_rule_to_be_smashed, conventional).
narrative_ontology:cs_reference_frame('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', workers_self_emancipation_through_councils).
narrative_ontology:cs_drift_state('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', post_russian_revolution_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4078c36f-31fe-43e1-8cc6-7b9766f4b3e1', '').
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

% Directly control their workplaces and federate with other councils, exercising power through assembly and delegation. They are the primary beneficiaries of the decentralized, non-hierarchical structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, local).

% Participate in local council structures, integrating workplace and residential democracy. They benefit from direct control over local resources and decision-making, free from centralized state or party control.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, local_communities, beneficiary,
    organized, generational, mobile, local).

% Their positions and authority are abolished by the council system. They are victims of the constraint's revolutionary overthrow of the state apparatus, losing power, status, and livelihood.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Their claim to lead the revolution and administer the 'dictatorship of the proletariat' is rejected. They are victims of the council system's anti-authoritarian stance, losing their hierarchical control and political power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, biographical, trapped, national).

% Their ownership of the means of production is expropriated, and their economic and political power is dismantled. They are the primary victims of the revolutionary transformation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_class, payer,
    powerful, biographical, trapped, global).

% Advocate for achieving socialism through existing parliamentary means. Their approach is rejected as reformist and insufficient by the council communist reading, which sees the state as an instrument of class rule to be smashed, not captured.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualists, excluded,
    organized, generational, constrained, national).

% Believe a disciplined, centralized party is essential for revolutionary success and post-revolutionary governance. Their model is seen as inherently authoritarian and a betrayal of working-class self-emancipation by the council communist reading.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_adherents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables direct, decentralized coordination of production and social life through federated worker and community councils, replacing hierarchical state and market mechanisms with democratic planning from below.
% TRANSFER_FUNCTION: Transfers political and economic power from centralized state and party apparatuses, and from the capitalist class, to federated assemblies of workers and local communities.
% ABSENT_VOICES: Both democratic gradualists and vanguard party adherents are excluded from the council communist framework, as their foundational premises (state reform or party dictatorship) are rejected as antithetical to direct workers' power. They would argue for their respective methods as more 'realistic' or 'effective' paths to socialism.
% DISAPPEARANCE_RATIONALE: If the council communist model were to disappear, the power vacuum would be filled by either a resurgent capitalist state, a vanguard party seizing control, or a descent into uncoordinated chaos, fundamentally altering the social and political landscape.
% FOUNDING_PROBLEM: The problem of capitalist exploitation and state oppression, compounded by the perceived authoritarian tendencies of vanguard parties and the reformist limitations of parliamentary democracy, preventing genuine working-class self-emancipation.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of workers' councils (e.g., Russian soviets before Bolshevik consolidation, German councils of 1918-19) and contemporary anarchist/autonomist movements attest to the ongoing relevance of direct democratic forms and the dangers of centralized power, corroborating the founding problem from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The claimed type is 'rope' because, internally, the council system is designed for genuine coordination among autonomous collectives with minimal extraction. However, its implementation requires revolutionary overthrow and faces extreme external suppression from established powers and rival political theories. The low theater ratio (0.05) reflects the direct, unmediated nature of council democracy, with little room for performative maintenance. Accessibility collapse (0.75) is high because, if implemented, it would fundamentally alter the political landscape, making previous alternatives (state, party) largely irrelevant. Resistance (0.92) is extremely high, as this model directly challenges the power of both the capitalist class and any centralized political authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of autonomous worker collectives, this is a pure coordination mechanism for self-emancipation. From the perspective of state bureaucrats or vanguard party officials, it is an existential threat that would dismantle their power structures. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and local communities are the primary beneficiaries, gaining direct control and self-management. State bureaucrats, vanguard party officials, and the capitalist class are the victims, losing their power, privilege, and property. Democratic gradualists and vanguard party adherents are 'excluded' as their foundational premises are incompatible with this model, and they would actively oppose its implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   The council communist model is a proposed alternative, not an existing constraint suffering mandatrophy. Its mandate is to achieve genuine working-class self-emancipation, a problem it considers very much 'live'. The classification prevents mislabeling its internal coordination as extraction, while accurately reflecting the external suppressive forces it would face.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    external_suppression_sustainability,
    'Given the high external suppression, could a council communist system realistically sustain itself against counter-revolutionary forces and rival political models?',
    'Historical analysis of actual council movements (e.g., Russian, German, Hungarian) and their suppression, or theoretical modeling of revolutionary defense strategies.',
    'If unsustainable, the model''s viability as a ''rope'' is compromised, potentially reclassifying it as a ''snare'' due to the overwhelming external forces, or a ''piton'' if it only persists in theory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_suppression_sustainability, empirical, 'The viability of council communism against external suppressive forces.').

omega_variable(
    internal_coordination_efficiency,
    'Would a fully decentralized, federated council system be efficient enough to manage a complex modern economy and society without reintroducing hierarchical structures?',
    'Detailed economic and organizational modeling of large-scale decentralized planning, or empirical observation of smaller-scale anarchist/autonomist experiments.',
    'If inefficient, internal pressures might lead to reintroduction of centralized elements, increasing internal extractiveness and potentially shifting the classification towards ''tangled_rope'' or even ''snare'' if new elites emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_coordination_efficiency, empirical, 'The efficiency and scalability of decentralized council coordination.').

omega_variable(
    kernel_framing_legitimacy,
    'Is the ''manifesto_revolutionary_method'' kernel best framed as a set of competing revolutionary strategies, or as a single historical trajectory with different phases?',
    'Conceptual analysis of historical materialism and revolutionary theory, examining whether different readings represent distinct, incompatible paths or sequential stages of a broader process.',
    'If framed as sequential stages, the ''council_communist_reading'' might be seen as an ideal endpoint rather than a distinct method, altering its relationship to other readings and potentially its classification as a ''rope'' if its internal viability is contingent on prior stages.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_legitimacy, conceptual, 'Framing of the ''manifesto_revolutionary_method'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1923).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(mani_tr_t1918, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1918, 0.05).
narrative_ontology:measurement(mani_tr_t1919, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(mani_tr_t1920, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(mani_tr_t1921, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1921, 0.05).
narrative_ontology:measurement(mani_tr_t1922, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1922, 0.05).
narrative_ontology:measurement(mani_tr_t1923, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1923, 0.05).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(mani_be_t1918, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1918, 0.22).
narrative_ontology:measurement(mani_be_t1919, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1919, 0.25).
narrative_ontology:measurement(mani_be_t1920, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1920, 0.23).
narrative_ontology:measurement(mani_be_t1921, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1921, 0.21).
narrative_ontology:measurement(mani_be_t1922, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1922, 0.24).
narrative_ontology:measurement(mani_be_t1923, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1923, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.7).
narrative_ontology:measurement(mani_su_t1918, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1918, 0.78).
narrative_ontology:measurement(mani_su_t1919, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1919, 0.83).
narrative_ontology:measurement(mani_su_t1920, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(mani_su_t1921, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1921, 0.87).
narrative_ontology:measurement(mani_su_t1922, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1922, 0.88).
narrative_ontology:measurement(mani_su_t1923, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1923, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'manifesto_revolutionary_method' kernel. This 'council_communist_reading' emphasizes direct democracy and decentralized power, contrasting with the 'vanguard_rupture_reading' (party-led state seizure) and the 'democratic_gradualism_reading' (parliamentary reform). Each reading represents a distinct approach to achieving socialism, with different beneficiaries, victims, and mechanisms of power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
