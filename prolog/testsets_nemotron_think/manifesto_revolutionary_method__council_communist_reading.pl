% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Workers' Councils (Soviets) as Direct Democratic Organs
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   The council communist reading of the revolutionary method holds that
 *   workers' councils (soviets) are the genuine form of proletarian
 *   dictatorship: direct democratic assemblies in workplaces, federated
 *   upward by recallable delegates, replacing both the capitalist state and
 *   the vanguard party. This constraint story captures the council system as
 *   a coordination mechanism with low internal extraction (ε=0.25) but high
 *   external suppression from rival readings (Bolshevik party-state, social
 *   democracy). The claimed type is 'rope' — pure coordination — though the
 *   engine may compute a different per-seat classification given the victims
 *   (state bureaucrats, party officials) who bear the cost of displacement.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiary (organized/constrained) — gains direct democratic control
 *   - state_bureaucrats: Primary victim (institutional/trapped) — loses state positions and privileges
 *   - party_officials: Primary victim (institutional/trapped) — loses party leading role and career
 *   - vanguard_party: Excluded rival (organized/constrained) — denied political leadership
 *   - capitalist_class: Excluded ruling class (powerful/mobile) — expropriated
 *   - revolutionary_theorist: Analytical observer (analytical/analytical) — evaluates historical forms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.25).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.35).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils (Soviets) as Direct Democratic Organs").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__council_communist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, 'e92f4826-04b0-4519-972a-92504c486644').
narrative_ontology:cs_kernel_codification('e92f4826-04b0-4519-972a-92504c486644', fixed_text).
narrative_ontology:cs_authority_grounding('e92f4826-04b0-4519-972a-92504c486644', lineage).
narrative_ontology:cs_interpretation_layer_present('e92f4826-04b0-4519-972a-92504c486644').
narrative_ontology:cs_reading_relation('e92f4826-04b0-4519-972a-92504c486644', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('e92f4826-04b0-4519-972a-92504c486644', manifesto_revolutionary_method__democratic_gradualism_reading, coexists_with).
narrative_ontology:cs_axiom('e92f4826-04b0-4519-972a-92504c486644', foundational, workers_self_emancipation_requires_direct_democracy).
narrative_ontology:cs_axiom_status(workers_self_emancipation_requires_direct_democracy, holdable).
narrative_ontology:cs_axiom_grounding('e92f4826-04b0-4519-972a-92504c486644', workers_self_emancipation_requires_direct_democracy, deontological).
narrative_ontology:cs_axiom('e92f4826-04b0-4519-972a-92504c486644', secondary, councils_historically_emerge_in_revolution).
narrative_ontology:cs_axiom_status(councils_historically_emerge_in_revolution, holdable).
narrative_ontology:cs_axiom_grounding('e92f4826-04b0-4519-972a-92504c486644', councils_historically_emerge_in_revolution, empirically_contingent).
narrative_ontology:cs_reference_frame('e92f4826-04b0-4519-972a-92504c486644', council_power_framework).
narrative_ontology:cs_drift_state('e92f4826-04b0-4519-972a-92504c486644', post_1917_revolution, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e92f4826-04b0-4519-972a-92504c486644', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, party_officials).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, workers_self_emancipation).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, direct_democracy_as_proletarian_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers organized in workplace assemblies and federated councils directly manage production, distribution, and social decisions. They send recallable delegates to higher councils. Their power derives from collective control of the means of production. Exit means leaving the workplace or the council system, which undermines their livelihood and political agency.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, constrained, national).

% Officials of the capitalist state apparatus (ministries, police, judiciary, military). Under the council system their positions are abolished, their authority dissolved, and they lose material privileges. They cannot easily exit because their skills and status are tied to the state structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, state_bureaucrats, payer,
    institutional, biographical, trapped, national).

% Functionaries of the vanguard party (central committee, regional organizers, agitprop). The council system declares the party superfluous and dissolves its leading role. Party officials lose their political career, institutional base, and ideological rationale.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, party_officials, payer,
    institutional, biographical, trapped, national).

% The organized revolutionary party that seeks to seize state power and guide the transition. The council system excludes it from power by making workplace assemblies the sole sovereign bodies. The party can go underground or attempt to capture councils, but its official role is negated.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, vanguard_party, excluded,
    organized, generational, constrained, national).

% Owners of capital and means of production. The council system expropriates their property and removes their economic power. They can flee abroad or hide assets, but their structural position is eliminated.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, capitalist_class, excluded,
    powerful, biographical, mobile, global).

% Analyzes the council form as a historical development in the workers' movement, comparing it with parliamentarism, trade unionism, and party dictatorship. Does not participate in the councils but evaluates their theoretical coherence and historical efficacy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, revolutionary_theorist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of proletarian self-organization for revolutionary power and post-capitalist administration: workplace assemblies federate to coordinate production, distribution, defense, and social reproduction without a separate state or party hierarchy.
% TRANSFER_FUNCTION: Transfers political authority and control of the means of production from the capitalist state and vanguard party to federated worker councils. The old ruling strata (bureaucrats, party officials) lose their positions and privileges; workers gain direct decision-making power.
% ABSENT_VOICES: The vanguard party and social-democratic parties are structurally excluded from the council system; they would argue that spontaneous worker councils lack strategic coherence and require party leadership or parliamentary mediation. Anarchist currents might also object to any federated structure that resembles a state.
% DISAPPEARANCE_RATIONALE: If the council system vanished overnight, the capitalist state or a party dictatorship would reassert control; the workers would lose their direct democratic organs and be subjected to either bourgeois parliamentarism or a one-party regime. The world rearranges because the councils are the institutional form of workers' power.
% FOUNDING_PROBLEM: The problem of how the working class can exercise power directly without mediation by a parasitic bureaucracy (state or party) that reproduces class domination. The councils were built to make the proletariat the ruling class by making its self-organization the government.
% FOUNDING_PROBLEM_CORROBORATION: Council communists (Pannekoek, Gorter, Mattick) and the German-Dutch left attest the problem is live: every revolution produces councils that are then crushed by parties or states. Orthodox Marxists and Leninists attest the problem is dead: the party is the necessary mediator. The historical record of 1905, 1917, 1918-23, 1956, 1968 shows councils re-emerging, corroborating the live reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.25) because the councils operate on direct democracy with recallable delegates, minimizing skimming. Suppression is moderate (0.35) because the council system must actively suppress counter-revolutionary forces (old state, party, capital) but internally operates with minimal coercion. Theater ratio is low (0.15) because the councils are functional, not performative. Accessibility collapse is moderate (0.4): alternatives (parliament, party dictatorship) remain thinkable but are politically suppressed. Resistance is high (0.75) because the councils face violent opposition from the old order and rival socialist factions.
 *
 * PERSPECTIVAL GAP:
 *   From the worker collective seat, the councils are a rope: genuine coordination for self-emancipation. From the bureaucrat/party official seat, the councils are a snare: pure extraction of their power. The engine computes this divergence. The claimed type 'rope' reflects the council communist self-understanding; the metrics describe the operational reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker collectives are beneficiaries (d near 0.0) — they gain power and control. State bureaucrats and party officials are victims (d near 1.0) — they lose their structural position. The vanguard party and capitalist class are excluded (d not computed for them in the same way). The analytical observer sits at d=0.5. The engine will derive directionality from these structural positions and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The council form was built to solve the problem of workers' direct rule. That problem remains live (contested) because every revolutionary upsurge regenerates councils, and every counter-revolution destroys them. The constraint has not undergone mandatrophy; its founding problem persists. The high resistance and external suppression indicate the constraint is actively contested, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_form_naturalness,
    'Is the council form a necessary, recurrent structure of proletarian self-organization (a mountain-like historical invariant) or a contingent historical form that could be otherwise?',
    'Comparative historical analysis of all revolutionary uprisings: if councils appear in every case independently, they are a structural invariant; if they appear only in specific conditions, they are contingent.',
    'If necessary, the council constraint approaches a mountain (high naturalness, low extractiveness). If contingent, it remains a rope/tangled_rope with higher extractiveness potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_form_naturalness, empirical, 'Whether workers'' councils are a historical law or a tactical invention.').

omega_variable(
    suppression_source_ambiguity,
    'Is the measured suppression (0.35) primarily external (from rival readings) or internal (councils suppressing dissent within)?',
    'Case studies of council internal dynamics (e.g., Kronstadt 1921, Spanish Revolution 1936): measure internal coercion vs. external defense.',
    'If internal suppression is high, the constraint moves toward tangled_rope/snare. If purely external, it remains a rope under siege.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Attribution of suppression to internal vs. external sources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 1917, 1923).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1917, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1917, 0.05).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1918, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1918, 0.08).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1919, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1920, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1921, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1921, 0.15).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1922, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1922, 0.15).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_tr_t1923, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 1923, 0.15).

% Extraction over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1917, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1917, 0.15).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1918, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1918, 0.2).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1919, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1920, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1920, 0.25).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1921, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1921, 0.25).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1922, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1922, 0.25).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_be_t1923, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 1923, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1917, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1918, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1918, 0.3).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1919, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1919, 0.35).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1920, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1921, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1921, 0.35).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1922, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1922, 0.35).
narrative_ontology:measurement(manifesto_revolutionary_method__council_communist_reading_su_t1923, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 1923, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.08).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'manifesto_revolutionary_method'. The council reading claims workers' councils as the sole revolutionary form; the vanguard reading claims party dictatorship; the gradualist reading claims parliamentary reform. They form a constraint family linked by affects_constraints. The ε values differ: council reading ε=0.25 (low internal extraction), vanguard reading ε high (party extracts from workers), gradualist reading ε moderate (parliamentary mediation extracts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__council_communist_reading, institutional, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
