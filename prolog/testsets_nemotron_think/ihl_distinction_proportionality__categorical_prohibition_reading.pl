% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Autonomous Weapons under Martens Clause
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the categorical_prohibition_reading of
 *   the ihl_distinction_proportionality kernel. The reading asserts that the
 *   Martens Clause — the residual humanitarian floor in IHL — categorically
 *   prohibits autonomous weapons systems that cross the threshold of
 *   machine-decided killing, regardless of their technical performance. The
 *   constraint claims the status of a Mountain (principles of humanity and
 *   public conscience as natural law), but structurally operates with
 *   identifiable beneficiaries (anti-militarist civil society, states lacking
 *   LAWS capability) and victims (major military powers, defense industry),
 *   requires active enforcement (treaty verification), and shows rising
 *   extractiveness as LAWS capability advances. The claim/metric divergence
 *   is the measurement: a claimed Mountain with Snare/Tangled Rope metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.85).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.8).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Autonomous Weapons under Martens Clause").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).
domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '6bfc36b5-8489-4728-9c0b-9705f7d409b9').
narrative_ontology:cs_kernel_codification('6bfc36b5-8489-4728-9c0b-9705f7d409b9', formalized).
narrative_ontology:cs_authority_grounding('6bfc36b5-8489-4728-9c0b-9705f7d409b9', lineage).
narrative_ontology:cs_interpretation_layer_present('6bfc36b5-8489-4728-9c0b-9705f7d409b9').
narrative_ontology:cs_reading_relation('6bfc36b5-8489-4728-9c0b-9705f7d409b9', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('6bfc36b5-8489-4728-9c0b-9705f7d409b9', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('6bfc36b5-8489-4728-9c0b-9705f7d409b9', foundational, machine_lethal_decision_intrinsically_violates_dignity).
narrative_ontology:cs_axiom_status(machine_lethal_decision_intrinsically_violates_dignity, holdable).
narrative_ontology:cs_axiom_grounding('6bfc36b5-8489-4728-9c0b-9705f7d409b9', machine_lethal_decision_intrinsically_violates_dignity, deontological).
narrative_ontology:cs_axiom('6bfc36b5-8489-4728-9c0b-9705f7d409b9', foundational, martens_clause_categorically_binds_laws).
narrative_ontology:cs_axiom_status(martens_clause_categorically_binds_laws, holdable).
narrative_ontology:cs_axiom_grounding('6bfc36b5-8489-4728-9c0b-9705f7d409b9', martens_clause_categorically_binds_laws, conventional).
narrative_ontology:cs_reference_frame('6bfc36b5-8489-4728-9c0b-9705f7d409b9', categorical_human_dignity_threshold).
narrative_ontology:cs_drift_state('6bfc36b5-8489-4728-9c0b-9705f7d409b9', contemporary_laws_development_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6bfc36b5-8489-4728-9c0b-9705f7d409b9', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, major_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_industry_laws_developers).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, martens_clause_principles_of_humanity).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__categorical_prohibition_reading, human_dignity_per_se_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Campaigns for a preemptive treaty ban on LAWS (Campaign to Stop Killer Robots). Gains moral authority, funding, and policy influence from the categorical prohibition framing. Can shift to other disarmament causes if this constraint fails.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Non-nuclear, non-major military powers that would be disadvantaged in an LAWS arms race. The categorical ban preserves a legal floor that prevents technological domination. Their exit is constrained by dependence on great-power security guarantees.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    moderate, biographical, constrained, global).

% States with advanced autonomous weapons programs (US, China, Russia, Israel, etc.). Bear the cost of foregone military capability, verification burdens, and strategic disadvantage if adversaries cheat. Can exit via treaty withdrawal, non-participation, or dual-use development under civilian cover.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, major_military_powers, payer,
    institutional, generational, arbitrage, global).

% Corporate actors developing autonomous targeting, swarming, and decision-support systems. Lose R&D investment, market access, and revenue streams under a categorical ban. Exit options limited to pivoting to non-lethal autonomous systems or dual-use civilian markets.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_industry_laws_developers, payer,
    powerful, biographical, constrained, global).

% Civilians in conflict zones who would bear the consequences of LAWS use or the protection of a ban. Structurally excluded from CCW GGE negotiations and treaty-making; their voices enter only through civil society intermediation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, affected_civilian_populations, excluded,
    powerless, immediate, trapped, global).

% Academic experts interpreting Martens Clause, distinction, proportionality, and human dignity in light of autonomous weapons. Produce the legal arguments that all three readings draw on; their analyses shape the legitimacy conditions for each reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_scholars, observer,
    analytical, civilizational, analytical, universal).

% Guardian of IHL; issues authoritative interpretations, convenes expert meetings, and advises states. Holds the human_agency_reading as institutional position but engages all three readings in its legal analysis.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, icrc, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a destabilizing autonomous weapons arms race by establishing a clear legal red line; coordinates state behavior around the principle that human dignity requires human control over lethal decisions; maintains the humanitarian legal order's anchor in the Martens Clause.
% TRANSFER_FUNCTION: Transfers the option to develop and deploy lethal autonomous weapons systems from major military powers and defense industries to a prohibition regime; the cost is foregone military-technological advantage and R&D investment; the gain is humanitarian protection and crisis stability, distributed diffusely.
% ABSENT_VOICES: Affected civilian populations in current and future conflict zones are structurally excluded from CCW GGE negotiations; Global South states without Geneva representation lack direct voice; future generations who would live with LAWS proliferation have no seat.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, major powers would accelerate LAWS deployment within existing doctrinal frameworks; an arms race in autonomous targeting would likely follow; the Martens Clause would lose its operative force as a constraint on novel weapons; humanitarian law's claim to govern 'means' not just 'outcomes' would be substantially weakened.
% FOUNDING_PROBLEM: The Martens Clause (1899/1907/1949/1977) was adopted to ensure that in cases not covered by specific treaties, populations and belligerents remain under the protection of the principles of humanity and the dictates of public conscience — a residual humanitarian floor for novel weapons and unforeseen circumstances.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentaries, UN Special Rapporteurs on extrajudicial executions, Nobel Peace laureates (ICRC, ICAN, Jody Williams), and the Holy See attest the Martens Clause applies categorically to LAWS; major military powers (US, Russia, China, Israel) contest this application, arguing the Clause is interpretive not prohibitive, and that existing IHL suffices.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, ExtMetricName, E),
    domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.85 over the interval because the opportunity cost of the ban increases as LAWS technology matures — the constraint extracts more from major powers the more capable the banned systems become. Suppression requirement rises similarly because verification and compliance become harder as autonomous functions diffuse into dual-use systems. Theater ratio stays low (0.15) because the prohibition movement genuinely seeks elimination, not performance. Accessibility collapse is high (0.9) because the categorical claim leaves no room for compliant LAWS — the threshold is binary. Resistance is high (0.75) because the most powerful states actively oppose the ban in diplomatic forums.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (civil society, non-LAWS states), the constraint appears as a genuine Mountain — a moral absolute protecting humanity. From the payer seats (major powers, industry), it appears as a Snare — a coercive ban extracting strategic advantage under humanitarian cover. The engine computes this divergence from the declared structural positions; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are structural beneficiaries (d near 0.0) — they gain legal protection and political leverage without bearing enforcement costs. Major military powers and defense industry are structural targets (d near 1.0) — they bear the full cost of foregone capability and verification burden. Affected civilians are excluded (trapped, powerless) — they would benefit from the ban but have no voice in its negotiation. IHL scholars and ICRC are analytical observers (d=0.5) — they interpret but do not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Martens Clause as residual humanitarian floor) remains contested — not dead. The categorical reading argues the problem is LIVE because LAWS represent the precise novelty the Clause was designed for. The mandate has not atrophied; the contest is whether the Clause's principles extend to machine agency. Mislabeling this as mandatrophy would confuse ongoing normative contest with obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the categorical prohibition a genuine principle of natural law (Mountain) or a constructed constraint benefiting identifiable agents (Snare/Tangled Rope)?',
    'Cross-cultural and historical analysis of whether ''human dignity per se'' prohibitions on machine killing appear in non-Western legal traditions and pre-digital war ethics; if the prohibition is uniquely contemporary and Western, it is more likely constructed.',
    'If constructed, the FSM signature triggers reclassification to tangled_rope; if natural, the Mountain claim holds despite beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'False summit mountain ambiguity: natural law claim vs. identifiable beneficiary structure').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the categorical reading''s core premise (threshold crossing violates dignity per se) logically foreclose the human_agency_reading''s core premise (human judgment required at moment of force) within a single framework?',
    'Formal analysis of whether ''machine-decided killing'' in the categorical reading encompasses human-on-the-loop configurations permitted by the human_agency_reading; if yes, forecloses; if no, coexists_with or influences.',
    'Changes the reading_relations edge from influences to forecloses, altering the kernel''s constraint family topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between categorical and human_agency readings').

omega_variable(
    martens_clause_interpretive_authority,
    'Does the CCW GGE / IHL treaty system have interpretive authority to resolve the Martens Clause''s application to LAWS, or is the Clause''s meaning distributed across non-state actors?',
    'Track whether a CCW protocol on LAWS adopts categorical language, human_agency language, or outcomes_based language — the institutional outcome reveals where interpretive authority lies.',
    'If authority is institutional (CCW), cs_structure.authority_grounding = lineage; if distributed, authority_grounding = distributed. Affects whether interpretation_layer_present is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_interpretive_authority, empirical, 'Authority grounding of the Martens Clause in the LAWS debate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__categorical_prohibition_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the ihl_distinction_proportionality kernel into three structurally distinct readings. The categorical_prohibition_reading has the highest ε (bans entire technology class) and claims Mountain status. The human_agency_reading has moderate ε (allows human-supervised autonomy) and claims Rope/Scaffold. The outcomes_based_reading has near-zero ε (performance-based) and claims Mountain/Rope. They are linked via affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, institutional, 0.15).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, powerful, 0.85).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, moderate, 0.2).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, organized, 0.1).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, powerless, 0.9).
constraint_indexing:directionality_override(ihl_distinction_proportionality__categorical_prohibition_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
