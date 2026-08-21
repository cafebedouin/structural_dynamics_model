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
 *   human_readable: Council Communist Direct Democracy
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'council_communist_reading' of the
 *   'manifesto_revolutionary_method' kernel. It describes the theoretical and
 *   practical proposal of workers' councils (soviets) as direct democratic
 *   organs intended to replace both the capitalist state and the vanguard
 *   party, with power held by federated workplace assemblies. This reading
 *   emphasizes decentralized, self-managed coordination, contrasting sharply
 *   with other revolutionary theories.
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
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Council Communist Direct Democracy").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'c742c6eb-4780-4c74-af45-065c6d2597cb').
narrative_ontology:cs_kernel_codification('c742c6eb-4780-4c74-af45-065c6d2597cb', formalized).
narrative_ontology:cs_authority_grounding('c742c6eb-4780-4c74-af45-065c6d2597cb', practice).
narrative_ontology:cs_reading_relation('c742c6eb-4780-4c74-af45-065c6d2597cb', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('c742c6eb-4780-4c74-af45-065c6d2597cb', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('c742c6eb-4780-4c74-af45-065c6d2597cb', foundational, direct_democracy_over_representation).
narrative_ontology:cs_axiom_status(direct_democracy_over_representation, holdable).
narrative_ontology:cs_axiom_grounding('c742c6eb-4780-4c74-af45-065c6d2597cb', direct_democracy_over_representation, deontological).
narrative_ontology:cs_axiom('c742c6eb-4780-4c74-af45-065c6d2597cb', foundational, abolition_of_state_and_capital).
narrative_ontology:cs_axiom_status(abolition_of_state_and_capital, holdable).
narrative_ontology:cs_axiom_grounding('c742c6eb-4780-4c74-af45-065c6d2597cb', abolition_of_state_and_capital, instrumental).
narrative_ontology:cs_reference_frame('c742c6eb-4780-4c74-af45-065c6d2597cb', federated_worker_self_governance).
narrative_ontology:cs_drift_state('c742c6eb-4780-4c74-af45-065c6d2597cb', post_20th_century_revolutionary_failures, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c742c6eb-4780-4c74-af45-065c6d2597cb', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, individual_workers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, capitalist_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the federated workplace and community assemblies that directly control production and social life. They benefit from self-management and decentralized decision-making, free from state or capitalist control.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, global).

% Participate directly in the decision-making processes of their workplaces and communities, benefiting from increased autonomy, democratic control over their labor, and the abolition of exploitation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, individual_workers, beneficiary,
    moderate, biographical, mobile, local).

% Represent the administrative and coercive apparatus of the capitalist state. They are victims as the council system aims to dismantle their power, authority, and the state structures they inhabit.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Leaders and functionaries of a centralized revolutionary party. They are victims because the council communist reading rejects their claim to lead the revolution and control the transitional state, advocating for direct worker power instead.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_officials, payer,
    institutional, biographical, trapped, national).

% Owners of the means of production. They are victims as the council system entails the expropriation of private property and the abolition of their economic and political power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_class, payer,
    powerful, biographical, constrained, global).

% Advocate for achieving socialism through existing parliamentary democratic structures and gradual reforms. They are excluded from the council communist framework, which views the capitalist state as an instrument of class rule to be overthrown, not reformed.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, democratic_gradualists, excluded,
    organized, biographical, constrained, national).

% Adhere to the belief that a disciplined, centralized party is necessary to lead the working class to revolution and establish a socialist state. They are excluded from the council communist vision of spontaneous, decentralized worker self-organization.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party_members, excluded,
    organized, biographical, constrained, national).

% Academics and theorists who study the historical and theoretical implications of council communism, its successes, failures, and potential as a model for social organization.
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
% COORDINATION_FUNCTION: To enable direct democratic coordination of production and social life through federated workplace and community assemblies, ensuring immediate accountability and preventing bureaucratic centralism.
% TRANSFER_FUNCTION: Transfers decision-making power, control over the means of production, and allocation of resources from centralized capitalist and state entities to decentralized, self-managed worker and community assemblies.
% ABSENT_VOICES: Representatives of the capitalist state and vanguard party ideologues are structurally excluded; they would argue for the necessity of state power or party leadership, respectively, as indispensable for social order or revolutionary success.
% DISAPPEARANCE_RATIONALE: If the concept and practice of workers' councils as direct democratic organs vanished, the revolutionary project they represent would collapse. This would lead to the reassertion of either capitalist state power or the dominance of a vanguard party, fundamentally altering the political and economic landscape of any revolutionary movement.
% FOUNDING_PROBLEM: The historical failure of both parliamentary democracy to achieve genuine working-class liberation and vanguard parties to prevent state authoritarianism and bureaucratic centralism after successful revolutions.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of council communism (e.g., Anton Pannekoek, Otto Rühle, Cornelius Castoriadis) attest to the problem's persistence through historical analysis of failed revolutions and the rise of state capitalism. Opponents (e.g., Leninists, social democrats) dispute this premise, arguing for the necessity of state or party structures for revolutionary success or social welfare, respectively.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The base extractiveness is low (0.25) because, within the proposed council system, the goal is to eliminate exploitation and ensure direct worker control, minimizing internal extraction. However, the system faces extremely high suppression (0.90) from existing capitalist states and rival revolutionary parties, which actively oppose its implementation. The theater ratio is low (0.10) as the proposal is a genuine, functional model for direct democracy, not a performative facade. Resistance is high (0.95) due to the revolutionary nature of the proposal and the direct challenge it poses to established powers.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of council communism view this as a pure coordination mechanism for a liberated society, where power is genuinely distributed. Opponents, particularly vanguard parties, often dismiss it as utopian, anarchic, or strategically unviable, seeing it as a threat to their own claims to revolutionary leadership. Capitalist states view it as a fundamental threat to their existence.
 *
 * DIRECTIONALITY LOGIC:
 *   Autonomous worker collectives and individual workers are the primary beneficiaries, gaining direct control and eliminating exploitation. Capitalist state bureaucrats, vanguard party officials, and the capitalist class are the victims, as their power and existence are directly challenged and intended to be abolished by this system. The high external suppression reflects the existential threat this constraint poses to these victim groups.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a revolutionary proposal, not an existing system that has atrophied. Its 'mandate' is to replace existing structures, and its persistence is not due to inertia but to ongoing theoretical advocacy and sporadic attempts at implementation, despite severe external suppression. Mandatrophy is not resolved because the system has not been widely implemented and thus cannot 'outlive its function' in a steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''council_communist_reading'' of the ''manifesto_revolutionary_method'' kernel. What are the implications of its relationship to sibling readings?',
    'Analysis of historical revolutionary movements and theoretical debates comparing council communist outcomes with those of vanguard party states or gradualist reforms.',
    'Understanding the structural differences and points of contention with sibling readings (''vanguard_rupture_reading'', ''democratic_gradualism_reading'') clarifies the unique challenges and potential of this specific revolutionary method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading within a contested kernel of revolutionary methods.').

omega_variable(
    structural_delta_from_siblings,
    'How does this reading''s structural delta (decentralized coordination, specific beneficiaries/victims, low internal extraction, high external suppression) compare to the structural deltas of its sibling readings?',
    'Comparative historical analysis of revolutionary attempts and theoretical frameworks associated with each reading, focusing on power distribution, extraction mechanisms, and suppression dynamics.',
    'If the structural delta is consistently observed in historical instances or theoretical elaborations, it strengthens the distinctiveness of the council communist reading. If not, it suggests a convergence with other readings or a mischaracterization of its unique features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_from_siblings, empirical, 'Examines the distinct structural features of this reading compared to its revolutionary theory siblings.').

omega_variable(
    achievability_of_decentralized_power,
    'Is genuinely decentralized power through federated workplace assemblies achievable and sustainable at a societal scale without reverting to state or party control?',
    'Empirical observation of large-scale, self-managed systems (e.g., historical soviets, contemporary cooperatives, distributed autonomous organizations) and theoretical modeling of their scalability and resilience.',
    'If proven achievable, it validates the core premise of this reading. If not, it suggests an inherent limitation that might push towards more centralized (and potentially extractive) forms of organization, aligning with aspects of sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(achievability_of_decentralized_power, empirical, 'Assesses the practical viability of large-scale, stateless, partyless direct democracy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1989).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(mani_tr_t1930, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1930, 0.07).
narrative_ontology:measurement(mani_tr_t1950, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mani_tr_t1970, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(mani_tr_t1989, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1989, 0.1).

% Extraction over time
narrative_ontology:measurement(mani_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.2).
narrative_ontology:measurement(mani_be_t1930, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1930, 0.22).
narrative_ontology:measurement(mani_be_t1950, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1950, 0.23).
narrative_ontology:measurement(mani_be_t1970, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(mani_be_t1989, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1989, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.8).
narrative_ontology:measurement(mani_su_t1930, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(mani_su_t1950, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1950, 0.88).
narrative_ontology:measurement(mani_su_t1970, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1970, 0.89).
narrative_ontology:measurement(mani_su_t1989, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1989, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on direct worker democracy via councils, in contrast to vanguard party leadership or gradualist reform. It directly challenges and influences the viability and legitimacy claims of these sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
