% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__council_communist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_council_communist_method, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Workers' Councils as Direct Democratic Organs (Council Communist Reading)
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint instantiates the council-communist reading of
 *   revolutionary method: workers' councils (soviets) as the primary organs
 *   of direct democratic power, replacing both the capitalist state and any
 *   centralized party apparatus. Authority is held by federated workplace
 *   assemblies where workers collectively decide production, compensation,
 *   and federation delegates. The reading asserts that revolutionary
 *   transformation requires no permanent mediating layer—councils themselves
 *   are the exercise of workers' power. This constraint is ONE READING of the
 *   contested kernel 'manifesto_revolutionary_method', coexisting with
 *   vanguard-party and electoral-gradualism readings. Each reading
 *   instantiates a different constraint with different beneficiaries,
 *   victims, and ε values. Within the council reading itself, extractiveness
 *   is low internally (0.28)—councils coordinate without central
 *   rent-collection—but the constraint's persistence requires high external
 *   suppression of rival readings (0.72), because the other readings'
 *   advocates retain organizational capacity and material interest in
 *   alternatives.
 *
 * KEY AGENTS:
 *   - autonomous_worker_collectives: Primary beneficiary and agenda-setter (organized, mobile exit despite revolutionary transformation context)
 *   - federated_workplace_assemblies: Coordinating layer (organized, constrained exit due to structural integration)
 *   - traditional_state_bureaucrats: Victim (powerful, trapped exit as institutional dissolution)
 *   - centralized_party_apparatus: Victim (powerful, trapped exit as representational function liquidated)
 *   - intellectual_vanguard_advocates: Observer and facilitator (moderate power, mobile exit)
 *   - rival_revolutionary_readings: Excluded (moderate power, constrained exit within this framework)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__council_communist_reading, 0.28).
domain_priors:suppression_score(manifesto_revolutionary_method__council_communist_reading, 0.72).
domain_priors:theater_ratio(manifesto_revolutionary_method__council_communist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__council_communist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__council_communist_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__council_communist_reading, "Workers' Councils as Direct Democratic Organs (Council Communist Reading)").
narrative_ontology:topic_domain(manifesto_revolutionary_method__council_communist_reading, "political_philosophy/revolutionary_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__council_communist_reading, '73b6e32d-efa9-4721-b3ae-1ee69a1530d6').
narrative_ontology:cs_kernel_codification('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', distributed).
narrative_ontology:cs_authority_grounding('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', practice).
narrative_ontology:cs_interpretation_layer_present('73b6e32d-efa9-4721-b3ae-1ee69a1530d6').
narrative_ontology:cs_reading_relation('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', manifesto_revolutionary_method__vanguard_rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_axiom('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', foundational, direct_workers_power_primacy).
narrative_ontology:cs_axiom_status(direct_workers_power_primacy, holdable).
narrative_ontology:cs_axiom_grounding('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', direct_workers_power_primacy, deontological).
narrative_ontology:cs_axiom('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', foundational, decentralized_federalism_necessity).
narrative_ontology:cs_axiom_status(decentralized_federalism_necessity, holdable).
narrative_ontology:cs_axiom_grounding('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', decentralized_federalism_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', workers_direct_collective_sovereignty).
narrative_ontology:cs_drift_state('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', contemporary_state_socialism_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73b6e32d-efa9-4721-b3ae-1ee69a1530d6', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, traditional_state_bureaucrats).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__council_communist_reading, centralized_party_apparatus).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, direct_democracy_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, decentralized_coordination_feasibility).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__council_communist_reading, worker_self_management_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual workplaces organized as democratic councils where all workers participate in production and allocation decisions. They hold authority over production methods, compensation distribution, and federation delegates. Their benefit is structural: they substitute their own assembly judgment for external state or party direction. Exit is possible through non-participation in revolutionary transformation, but organized workers perceive the coordinated system as liberation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives, agenda_setter).

% The nested coordinating structure: councils send delegates to regional federations, which send to national coordination bodies. Delegates remain accountable to their home assemblies and can be recalled at any time. The structure handles inter-workplace coordination (resource distribution, technical standards, conflict resolution) without forming a permanent bureaucratic apparatus. They benefit from horizontal coordination without centralized authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, federated_workplace_assemblies, beneficiary,
    organized, generational, constrained, national).

% Revolutionary theorists and organizers articulating the council system to workers. They operate as educators and facilitators but claim no formal power; the logic of their position rests on workers' voluntary adoption. Their capacity to influence depends on whether workers find their analysis compelling and whether material conditions support receptiveness.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, intellectual_vanguard_advocates, observer,
    moderate, biographical, mobile, national).

% Officials of the existing state apparatus whose authority and function would be displaced by the council system. They bear the dissolution of their institutional role and the transfer of decisional power to workplace assemblies. Their exit is institutional dissolution, not individual mobility—the constraint liquidates the structure they occupy.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, traditional_state_bureaucrats, payer,
    powerful, biographical, trapped, national).

% The organized vanguard party (if one exists transitionally) whose claim to mediate worker power is bypassed by direct council authority. Their position in this reading is that of an excluded layer: the councils system asserts workers need no party intermediary. The party bears the dissolution of its coordinating and representative functions to the councils.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, centralized_party_apparatus, payer,
    powerful, generational, trapped, national).

% Advocates of vanguard-party or electoral-gradualism readings who contest the council system's viability and necessity. They are excluded from THIS reading's framework—they have objections that would dissolve the council arrangement if their premises gained organizational force, but within this reading they remain outside, proposing alternatives.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, rival_revolutionary_readings, excluded,
    moderate, biographical, constrained, national).

% External analyst documenting the structural properties of the constraint: how power is distributed, what coordination costs it sustains, where it generates friction with rival readings, and what stability conditions it requires.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__council_communist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__council_communist_reading, autonomous_worker_collectives).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__council_communist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of aggregating decentralized production decisions and distributing resources without centralizing decisional authority into a state or party apparatus. Each workplace council makes production and compensation decisions internally; federation delegates coordinate inter-workplace resource flows, technical standards, and conflict resolution. The system treats coordination as an emergent property of horizontal negotiation rather than top-down command.
% TRANSFER_FUNCTION: Transfers decisional authority from centralized institutions (state and party) to federated workplace assemblies. Material transfers flow through councils' own allocation decisions rather than through bureaucratic hierarchies. No fixed extraction rate; councils retain full product of their labor, minus contributions to regional and national coordination infrastructure.
% ABSENT_VOICES: Agents excluded from this reading's assembly structure: the centralized party apparatus, the state bureaucracy, and rival revolutionary readings (vanguard and gradualist advocates). These groups would argue that councils lack coordination capacity for complex economies, that workers lack expertise for all production decisions, and that councils generate coordination failures that invite recentralization. They are structurally excluded by the reading's premises and would contest its viability if their organizational frameworks achieved force.
% DISAPPEARANCE_RATIONALE: If the council system dissolved overnight, workers would revert to either state employment with bureaucratic direction or to restoration of party-mediated authority—the production apparatus itself would reorganize around one of the rival authority structures. The disappearance of councils as a constraint means the disappearance of the decentralized decision structure; production would re-centralize.
% FOUNDING_PROBLEM: Production under capitalism concentrates control in capitalist hands and under state socialism concentrates it in bureaucratic or party hands. Neither structure allows workers to determine their own labor process, compensation, or collective priorities. The council reading emerged as a solution to this dual alienation: workers need an organizational form that places production decisions directly in their hands without requiring a permanent mediating apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Council communist theorists and participants in historical soviet experiments (Paris Commune, Russian workers' councils 1917, Hungarian councils 1956, Yugoslav self-management experiments) attest the founding problem is live and their reading addresses it. Vanguard and gradualist readings contest whether councils can achieve scale and whether the founding problem is best solved through their alternative methods. Historians outside revolutionary frameworks document council experiments but dispute whether they represent feasible steady-state systems or transitional phenomena. Independent economic analyses from non-affiliated researchers examine coordination costs in decentralized systems without endorsing or refuting the reading's premises.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__council_communist_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__council_communist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__council_communist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__council_communist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__council_communist_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured WITHIN the council system: once councils form, they retain the full product of their labor minus coordination infrastructure costs—low absolute extraction. The reading claims workers benefit from direct democratic control and absence of external rent-collection. Measurement series show extractiveness plateauing at 0.28 by t=15 and remaining stable through projection; this flatness reflects the reading's internal claim that councils sustain a low-extraction steady state. Suppression is HIGH (rising to 0.72) because the constraint's persistence depends entirely on organizational capacity to prevent rival readings from taking hold. The council reading exists only if workers reject or physically suppress the organizational forms of vanguard parties and state bureaucracies. Theater is low (0.18) within councils but this understates the theater required to MAINTAIN the reading: councils must conduct continuous public performance of self-governance to prevent reversion to state or party forms. The measurement trajectory shows suppression accumulating over the interval—a sign that maintaining the council structure requires increasing enforcement against rival alternatives, not a sign of natural stability.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is extreme: (1) Worker-collective seats compute councils as genuine coordination with low extraction and high beneficiary status. (2) State-bureaucrat and party-apparatus seats compute councils as confiscatory—the constraint liquidates their institutional position entirely, making them maximal-target seats (d→1.0). (3) Rival-reading advocates compute councils as utopian and unstable; they experience suppression of their alternative reading as the constraint's active cost. (4) The analytical seat observes that suppression is the constraint's primary budget: councils appear cheap internally but require expensive maintenance against alternatives. The engine computes these per-seat divergences from directionality; the authored claim (rope, based on genuine coordination among workers) and the authored metrics (high suppression) will diverge in the computed types, and that divergence IS the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Worker collectives are structural beneficiaries (d→0.0): they retain authority, receive no extraction, and have mobile exit (they could choose not to organize councils, though revolutionary context constrains choice). The federated assemblies are near-beneficiary (d~0.15-0.25): they coordinate and hold power but are constrained by the need for nested structures. State bureaucrats and party officials are full targets (d→1.0): their institutional positions are liquidated by the constraint. Rival reading advocates are excluded rather than directly extracted from, but they experience suppression of their reading as a cost (effective d→0.8 within this framework). The intellectual advocates are near-symmetric (d~0.5): they facilitate the structure but hold no formal power and could theoretically work within any reading. No overrides are necessary; the beneficiary/victim declarations and exit options yield the correct d profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims to solve a live founding problem (dual alienation under capitalism and state socialism) and asserts the council system is the permanent solution, not a transition. The measurement series show stable extractiveness and plateauing suppression—suggesting the claim is that councils reach a steady state once established. However, the high and rising suppression requirement (0.72) indicates the constraint is actively defended against alternatives, not passively maintained. This is a sign of CONTESTED mandatrophy: the constraint's mandate (direct worker democracy) is live to its beneficiaries and advocates, but its competitors (vanguard and gradualist readings) contest whether the founding problem persists or whether councils solve it. The engine's mandatrophy detection will flag the mismatch between the low internal extraction (suggesting natural coordination) and the high external suppression (suggesting constructed constraint defending against alternatives). This mismatch is exactly what falsifies the natural-law framing—councils are a constructed political form requiring continuous suppression of competitors, not an emergent structure from material conditions alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_capacity_at_scale,
    'Can federated councils genuinely coordinate complex production economies (energy, transportation, food systems, manufacturing) at national or global scale without functional recentralization?',
    'Historical analysis of council experiments (Yugoslavia, Hungary, Paris Commune) and contemporary cooperative networks; comparative analysis with other decentralized coordination systems (open-source software, supply-chain networks). If recentralization occurs within historical experiments, document the material conditions that drove it.',
    'If councils cannot sustain scale without recentralization, the constraint''s viability rests on counterfactual conditions (post-scarcity, ultra-simple economies). If they can, the reading gains empirical credibility. The engine flags this as a gate on the ''rope'' classification: genuine coordination requires demonstrable capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_capacity_at_scale, empirical, 'Whether the structural form can actually perform its claimed coordination function at scale.').

omega_variable(
    suppression_cost_unsustainability,
    'Is the high measured suppression (0.72) a temporary cost of establishing councils against entrenched alternatives, or a permanent structural requirement?',
    'Temporal extension of historical experiments: do council systems stabilize once rivals are suppressed (suppression drops), or does suppression remain continuously high to prevent reversion?',
    'If suppression is temporary, the constraint plateaus as genuine coordination with lower maintenance cost. If permanent, the constraint is a constructed political form requiring continuous coercion—reclassifying toward tangled_rope or snare depending on who bears the suppression cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_cost_unsustainability, empirical, 'Whether the constraint''s sustainability depends on continuous suppression of alternatives or on stable internal coordination.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the three readings logically foreclosed from coexisting (only one can be true), or do they represent rival frameworks that different actors can hold simultaneously?',
    'Examination of whether councils, vanguard party, and electoral mechanisms are mutually exclusive or whether they can operate in the same system (e.g., councils within a state, party advocacy within electoral politics). Historical analysis of mixed systems.',
    'If foreclosed, this is the central claim-space: actors must choose one reading, and suppression measures the enforcement of that choice. If coexistent, the three readings represent different seats'' perspectives on the same system, and the measured suppression reflects conflict between readings rather than structural incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s core premises logically exclude the sibling readings'' premises in any single framework.').

omega_variable(
    internal_extraction_within_councils,
    'Does the measured low extractiveness (0.28) reflect genuine absence of extraction within councils, or does it mask new forms of extraction (expertise-based gatekeeping, delegate capture, gender/skill-based stratification)?',
    'Ethnographic and participatory analysis of actual council operations: Do all workers meaningfully participate in decisions? Do delegates remain accountable or form a new bureaucratic layer? Are women, low-skilled workers, and minorities proportionally represented in decision-making and does their voice influence outcomes?',
    'If extraction is genuinely low, the reading''s claim of direct democracy gains support. If extraction is masked by formal procedures, councils function as a new extraction form—reclassifying toward tangled_rope (democratic appearance + asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_extraction_within_councils, empirical, 'Whether low measured extractiveness reflects actual power equality or concealed stratification within councils.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__council_communist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t0, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(mani_tr_t0, observed).
narrative_ontology:measurement(mani_tr_t5, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(mani_tr_t5, observed).
narrative_ontology:measurement(mani_tr_t10, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(mani_tr_t10, observed).
narrative_ontology:measurement(mani_tr_t15, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(mani_tr_t15, observed).
narrative_ontology:measurement(mani_tr_t20, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(mani_tr_t20, observed).
narrative_ontology:measurement(mani_tr_t25, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(mani_tr_t25, observed).
narrative_ontology:measurement(mani_tr_t30, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(mani_tr_t30, projected).
narrative_ontology:measurement(mani_tr_t40, manifesto_revolutionary_method__council_communist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(mani_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(mani_be_t0, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(mani_be_t0, observed).
narrative_ontology:measurement(mani_be_t5, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement_basis(mani_be_t5, observed).
narrative_ontology:measurement(mani_be_t10, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(mani_be_t10, observed).
narrative_ontology:measurement(mani_be_t15, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement_basis(mani_be_t15, observed).
narrative_ontology:measurement(mani_be_t20, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(mani_be_t20, observed).
narrative_ontology:measurement(mani_be_t25, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(mani_be_t25, observed).
narrative_ontology:measurement(mani_be_t30, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(mani_be_t30, projected).
narrative_ontology:measurement(mani_be_t40, manifesto_revolutionary_method__council_communist_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(mani_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t0, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(mani_su_t0, observed).
narrative_ontology:measurement(mani_su_t5, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(mani_su_t5, observed).
narrative_ontology:measurement(mani_su_t10, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(mani_su_t10, observed).
narrative_ontology:measurement(mani_su_t15, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(mani_su_t15, observed).
narrative_ontology:measurement(mani_su_t20, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(mani_su_t20, observed).
narrative_ontology:measurement(mani_su_t25, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(mani_su_t25, observed).
narrative_ontology:measurement(mani_su_t30, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(mani_su_t30, projected).
narrative_ontology:measurement(mani_su_t40, manifesto_revolutionary_method__council_communist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(mani_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__council_communist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__council_communist_reading, 0.12).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__council_communist_reading, manifesto_revolutionary_method__democratic_gradualism_reading).

% DUAL FORMULATION NOTE:
% This story and its siblings (vanguard_rupture_reading, democratic_gradualism_reading) decompose the contested kernel 'manifesto_revolutionary_method'. Each reading instantiates a structurally distinct constraint with different beneficiaries, victims, and ε values. The council reading asserts low internal extraction and decentralized power; the vanguard reading asserts high extraction during dictatorship phase and centralized power; the gradualist reading asserts minimal extraction and representative power. They are NOT perspectives on one constraint—they are three separate constraints that cannot coexist in a single framework. Each story must be generated separately with its own ε-invariant structure. All three stories link via network.affects_constraints to signal their kinship and rivalry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__council_communist_reading, institutional, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
