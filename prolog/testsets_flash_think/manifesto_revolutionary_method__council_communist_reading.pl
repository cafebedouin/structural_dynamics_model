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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__council_communist_reading
 *   human_readable: Federated Workers' Councils (Council Communist Reading)
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'council communist' reading of the
 *   'manifesto_revolutionary_method' kernel. It describes workers' councils
 *   (soviets) as direct democratic organs intended to replace both the
 *   capitalist state and the vanguard party, with power held by federated
 *   workplace assemblies. The reading posits a decentralized, self-managed
 *   society. While internally designed for low extraction and high
 *   coordination (claimed as a Rope), its historical and theoretical context
 *   involves immense external suppression from both capitalist and
 *   vanguard-party forces, leading to high overall suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.9).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Federated Workers' Councils (Council Communist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '83fed4ef-a4c4-429e-a98c-1f202147a1f3').
narrative_ontology:cs_kernel_codification('83fed4ef-a4c4-429e-a98c-1f202147a1f3', distributed).
narrative_ontology:cs_authority_grounding('83fed4ef-a4c4-429e-a98c-1f202147a1f3', practice).
narrative_ontology:cs_interpretation_layer_present('83fed4ef-a4c4-429e-a98c-1f202147a1f3').
narrative_ontology:cs_reading_relation('83fed4ef-a4c4-429e-a98c-1f202147a1f3', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('83fed4ef-a4c4-429e-a98c-1f202147a1f3', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('83fed4ef-a4c4-429e-a98c-1f202147a1f3', foundational, direct_democracy_is_supreme).
narrative_ontology:cs_axiom_status(direct_democracy_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('83fed4ef-a4c4-429e-a98c-1f202147a1f3', direct_democracy_is_supreme, deontological).
narrative_ontology:cs_axiom('83fed4ef-a4c4-429e-a98c-1f202147a1f3', foundational, state_and_party_are_alienating).
narrative_ontology:cs_axiom_status(state_and_party_are_alienating, holdable).
narrative_ontology:cs_axiom_grounding('83fed4ef-a4c4-429e-a98c-1f202147a1f3', state_and_party_are_alienating, empirically_contingent).
narrative_ontology:cs_reference_frame('83fed4ef-a4c4-429e-a98c-1f202147a1f3', direct_worker_self_management).
narrative_ontology:cs_drift_state('83fed4ef-a4c4-429e-a98c-1f202147a1f3', historical_suppression_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('83fed4ef-a4c4-429e-a98c-1f202147a1f3', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary organs of direct democracy, coordinating production and social life. They set the agenda for their respective workplaces and federate upwards. Their legitimacy is derived from direct participation, making exit from this form of governance a rejection of their own collective identity.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies, agenda_setter,
    institutional, generational, identity_locked, local).

% Workers organized in self-managed units, directly participating in decision-making and benefiting from the absence of capitalist exploitation and bureaucratic control. Their options are tied to the success of the council system.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, biographical, constrained, local).

% Officials of the capitalist state whose power and function are directly targeted for abolition by the council system. They would lose their positions, authority, and means of livelihood.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats, payer,
    institutional, immediate, trapped, national).

% Leaders and functionaries of a vanguard party whose claim to lead the revolution and establish a 'dictatorship of the proletariat' is rejected by the council communist reading. They would lose their organizational power and ideological justification.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, immediate, trapped, national).

% Owners of capital and means of production whose economic power and social status are directly expropriated and abolished by the council system. They would lose their property and ability to extract surplus value.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_class, payer,
    powerful, immediate, trapped, global).

% Advocates for achieving socialism through existing parliamentary democratic structures and gradual reforms. Their approach is seen as insufficient and ultimately co-opted by the council communist reading, excluding them from the proposed revolutionary method.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualists, excluded,
    organized, generational, constrained, national).

% Rank-and-file members of a vanguard party who believe in party leadership for revolutionary transformation. While potentially sympathetic to worker power, their commitment to party discipline and centralized guidance places them outside the direct democratic framework of council communism.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_members, excluded,
    moderate, biographical, identity_locked, national).

% Academics and theorists analyzing the historical attempts and theoretical viability of workers' councils as a form of social organization. They assess the internal dynamics and external challenges without direct participation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, diffuse).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable direct, decentralized coordination of production, distribution, and social life through federated workplace and community assemblies, replacing hierarchical state and party structures.
% TRANSFER_FUNCTION: Transfers decision-making power, control over economic resources, and social authority from centralized capitalist state and vanguard party apparatuses to local, federated worker assemblies.
% ABSENT_VOICES: Representatives of the capitalist state, vanguard party leadership, and proponents of parliamentary democracy or party-led revolution are structurally excluded. They would argue for the necessity of existing institutions or centralized party guidance.
% DISAPPEARANCE_RATIONALE: If the council system, once established, were to vanish overnight, the entire social, economic, and political organization would collapse. Production and distribution would cease to be coordinated, leading to chaos or a rapid reversion to prior forms of centralized control (state or party-led).
% FOUNDING_PROBLEM: The alienation, exploitation, and suppression of direct popular will inherent in both capitalist state structures and centralized vanguard party models of revolutionary transformation.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of workers' councils (e.g., Russian soviets 1905/1917, German councils 1918-1919), critiques of state socialism by council communists, and anarchist/autonomist political theory provide corroboration from outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness is low (0.25) because the council system, by its design, aims to eliminate exploitation and distribute power directly among workers, minimizing internal extraction. However, the suppression is very high (0.90) due to the revolutionary nature of the proposal, which directly challenges and seeks to abolish existing state and party structures. This necessitates active enforcement against powerful external adversaries. The theater ratio is low (0.10) as the councils are conceived as genuinely functional and directly democratic, with little room for performative maintenance. Resistance is high (0.85) from the entrenched powers it seeks to replace. Accessibility collapse is moderate (0.60) because while the existing alternatives (state, party) are actively suppressed, the theoretical possibility of direct democracy is not entirely collapsed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the worker collectives and assemblies, the system is a pure coordination mechanism for self-emancipation. From the perspective of the capitalist state and vanguard party, it is an existential threat requiring total suppression. The engine's computation of per-seat classification will reflect this divergence, with the internal 'rope' function being overshadowed by the external 'snare' dynamics from the perspective of the threatened powers.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and federated workplace assemblies are the primary beneficiaries, gaining direct control and eliminating exploitation. Capitalist state bureaucrats, vanguard party officials, and the capitalist class are the clear targets/victims, as their power and existence are directly challenged and abolished by this system. The directionality for the beneficiaries is low (near 0.0), while for the victims it is high (near 1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   As a theoretical proposal for a revolutionary transformation, this constraint is not subject to mandatrophy in the sense of an atrophied function. Its mandate (direct worker self-management) is considered live and urgent by its proponents. The question is not whether its function has outlived its purpose, but whether it can ever be successfully implemented and sustained against external opposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_communist_kernel_reading,
    'This constraint is one reading of the ''manifesto_revolutionary_method'' kernel. What would a sibling reading (e.g., vanguard_rupture_reading or democratic_gradualism_reading) change structurally?',
    'Analysis of the structural differences in power distribution, beneficiary/victim sets, and enforcement mechanisms proposed by each reading.',
    'Each sibling reading would instantiate a distinct constraint with different extractiveness, suppression, and stakeholder dynamics, leading to different classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(council_communist_kernel_reading, conceptual, 'This constraint is a specific reading of a broader kernel, with distinct structural implications.').

omega_variable(
    viability_under_external_pressure,
    'Can a decentralized, federated council system effectively defend itself and survive sustained external suppression from powerful state or party apparatuses?',
    'Historical analysis of attempts to establish such systems (e.g., Paris Commune, early Russian soviets, German councils) and their ultimate suppression, combined with theoretical models of resilience for decentralized networks.',
    'If such a system is inherently vulnerable to external suppression, its effective suppression and accessibility collapse would be higher, potentially reclassifying it as a Snare or Piton from an external observer''s perspective, despite its internal Rope-like design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_under_external_pressure, empirical, 'The long-term viability of the council system against external threats.').

omega_variable(
    internal_coordination_complexity,
    'Can federated workplace assemblies effectively coordinate a complex modern economy and society without eventually re-centralizing power or developing new forms of bureaucracy?',
    'Detailed theoretical modeling of complex resource allocation and decision-making in a fully decentralized system, or empirical observation of large-scale self-managed systems (if any were to emerge).',
    'If internal re-centralization is inevitable, the internal extractiveness would rise over time, potentially shifting the internal classification from Rope towards Tangled Rope or even Snare, as new forms of extraction emerge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_coordination_complexity, conceptual, 'The challenge of maintaining decentralized coordination in a complex society.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1967).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(mani_tr_t1927, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1927, 0.1).
narrative_ontology:measurement(mani_tr_t1937, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(mani_tr_t1947, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(mani_tr_t1957, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(mani_tr_t1967, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1967, 0.1).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.25).
narrative_ontology:measurement(mani_be_t1927, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1927, 0.24).
narrative_ontology:measurement(mani_be_t1937, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1937, 0.23).
narrative_ontology:measurement(mani_be_t1947, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1947, 0.22).
narrative_ontology:measurement(mani_be_t1957, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1957, 0.21).
narrative_ontology:measurement(mani_be_t1967, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1967, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.7).
narrative_ontology:measurement(mani_su_t1927, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1927, 0.8).
narrative_ontology:measurement(mani_su_t1937, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1937, 0.85).
narrative_ontology:measurement(mani_su_t1947, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1947, 0.88).
narrative_ontology:measurement(mani_su_t1957, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1957, 0.89).
narrative_ontology:measurement(mani_su_t1967, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1967, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'manifesto_revolutionary_method' kernel. It represents the council communist perspective, distinct from vanguard party and democratic gradualist approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
