% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_coordination, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: P5 Veto as Great-Power War Prevention (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   The P5 veto in Article 27 of the UN Charter grants each of the five
 *   permanent Security Council members (United States, Soviet Union/Russia,
 *   United Kingdom, France, China) the power to block any non-procedural
 *   Council resolution. This reading interprets the veto as a coordination
 *   mechanism solving a genuine collective-action problem: absent a unanimity
 *   requirement on military enforcement, each great power would face the risk
 *   of being bound by vote to military confrontation against another great
 *   power. The veto ensures that no binding war mandate can be imposed
 *   without all P5 members' consent, creating mutual security assurance and
 *   institutional stability. This reading treats the veto as a Rope—genuine
 *   coordination function, minimal extraction, beneficiary is the
 *   international system itself. The claim/metric gap reflects this: the veto
 *   is not presented as extractive (low ε, low suppression) because under
 *   this reading it is genuinely coordination, not a tool for any party to
 *   extract rents. The alternative readings (oligopoly_reading,
 *   sovereignty_reading) would author higher ε values and different
 *   beneficiary structures; they are separate constraints, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - P5 nuclear states (beneficiary): hold veto power ensuring mutual non-coercion; benefit symmetrically from the unanimity requirement
 *   - Non-P5 states (beneficiary): benefit from institutional stability that prevents great-power war, preserving Council authority in non-existential disputes
 *   - Great-power coordination mechanism (agenda setter, non-agent): the structural arrangement itself that solves the unanimity problem
 *   - International legal order (beneficiary, non-agent): persists because its enforcement mechanisms do not trigger great-power defection
 *   - Non-P5 regional powers (payer): experience veto blocks against their enforcement interests as necessary cost of P5 stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.12).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.08).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "P5 Veto as Great-Power War Prevention (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'e08e39f4-e91c-48ad-af91-37ac55e245bb').
narrative_ontology:cs_kernel_codification('e08e39f4-e91c-48ad-af91-37ac55e245bb', formalized).
narrative_ontology:cs_authority_grounding('e08e39f4-e91c-48ad-af91-37ac55e245bb', lineage).
narrative_ontology:cs_interpretation_layer_present('e08e39f4-e91c-48ad-af91-37ac55e245bb').
narrative_ontology:cs_reading_relation('e08e39f4-e91c-48ad-af91-37ac55e245bb', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('e08e39f4-e91c-48ad-af91-37ac55e245bb', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e08e39f4-e91c-48ad-af91-37ac55e245bb', foundational, great_power_war_prevention_via_institutional_veto).
narrative_ontology:cs_axiom_status(great_power_war_prevention_via_institutional_veto, holdable).
narrative_ontology:cs_axiom_grounding('e08e39f4-e91c-48ad-af91-37ac55e245bb', great_power_war_prevention_via_institutional_veto, instrumental).
narrative_ontology:cs_axiom('e08e39f4-e91c-48ad-af91-37ac55e245bb', foundational, collective_action_failure_without_unanimity_gate).
narrative_ontology:cs_axiom_status(collective_action_failure_without_unanimity_gate, holdable).
narrative_ontology:cs_axiom_grounding('e08e39f4-e91c-48ad-af91-37ac55e245bb', collective_action_failure_without_unanimity_gate, empirically_contingent).
narrative_ontology:cs_reference_frame('e08e39f4-e91c-48ad-af91-37ac55e245bb', charter_founding_unanimity_principle).
narrative_ontology:cs_drift_state('e08e39f4-e91c-48ad-af91-37ac55e245bb', contemporary_2026, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e08e39f4-e91c-48ad-af91-37ac55e245bb', '2026-06-19T14:32:18Z').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_nuclear_states).
narrative_ontology:constraint_victim(article_27_veto_power__coordination_reading, non_p5_regional_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power ensuring no Security Council resolution can compel them into military confrontation without their own consent. They benefit symmetrically from the unanimity requirement—each can block enforcement actions that threaten their vital interests while knowing the others have equal blocking power. Without the veto, they would face the risk of being bound by institutional majority vote to war against peer great powers. They actively defend the veto and have never ceded it despite institutional reform proposals.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_nuclear_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Benefit from a Security Council that remains effective as an international dispute-resolution forum because the P5 remain inside it. The veto prevents great-power wars from fragmenting the Council, which would leave non-P5 states without institutional recourse for conflict resolution. They also benefit indirectly from the deterrent effect of P5 mutual assurance—the veto makes great-power war less likely overall, stabilizing the international system within which they operate.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_states, beneficiary,
    organized, generational, constrained, global).

% An abstraction capturing the benefit of avoiding great-power wars. The veto contributes to international stability by preventing the Security Council itself from becoming a mechanism for coercing great powers into existential conflict, which would trigger institutional breakdown and power-balancing outside the Council (alliances, arms races, preemptive wars).
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Experience veto blocks on Security Council enforcement when a P5 member has aligned interests with their adversary. They may seek Council authorization for military action against a regional rival, only to have that action blocked by a P5 veto protecting the rival. Under the coordination reading, this veto is not extraction but necessary cost of institutional stability. They cannot unilaterally override the veto; their only recourse is diplomatic coalition-building among non-P5 states or appealing directly to the P5 member with veto power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_regional_powers, payer,
    powerful, generational, constrained, regional).

% The institutional structure itself that embodies and solves the unanimity problem in binding military enforcement. This is not an agent but the mechanism—the veto rule, the Council procedures, the Charter requirement for unanimous P5 consent. It sets the agenda by structuring what resolutions can be binding and who can block them.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, great_power_coordination_mechanism, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, great_power_coordination_mechanism).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of great-power mutual non-coercion into military confrontation: absent a unanimity requirement on binding military enforcement, each great power faces the risk that the other four could vote (with non-P5 support) to compel it into existential war. The veto restores mutual security confidence by making binding war mandates impossible without all P5 consent.
% TRANSFER_FUNCTION: Transfers decision-making authority from simple-majority rule (9+ of 15) to unanimity rule (all 5 P5) for military enforcement resolutions. The cost is borne by non-P5 states and P5 regional interests: a single veto blocks Council action even when 14 of 15 members would authorize enforcement. Coordination is purchased with institutional paralysis in non-existential disputes.
% ABSENT_VOICES: Smaller regional states that would benefit from Council enforcement against adversaries backed by a P5 member (e.g., weak states threatened by a P5 client state) cannot participate in the coordination conversation that generates the veto rule. They experience the veto as an external constraint, not as their own security guarantee. Their exclusion from great-power coordination is structural—intentional by design, not accidental.
% DISAPPEARANCE_RATIONALE: If the veto disappeared and the Security Council could bind resolutions by 9-0 vote without P5 unanimity, each P5 state would immediately face existential risk from the institution. They would reconstruct great-power coordination outside the Council (concert-of-powers agreements, military alliances, or separate enforcement mechanisms). The international legal order would reorganize around the de facto unanimity requirement the veto currently embeds, because the underlying collective-action problem does not disappear with the institutional rule.
% FOUNDING_PROBLEM: After World War II, the great powers recognized that a world order permitting institutional coercion of great powers into existential war would be unstable. If any great power could be bound by vote to military confrontation against other great powers, it would either exit the institution or launch preemptive war to prevent being cornered. The veto was the solution: no binding military mandate without all P5 consent.
% FOUNDING_PROBLEM_CORROBORATION: The great-power deterrence literature (Waltz, Mearsheimer, Gilpin) and actual P5 behavior (consistent veto defense, no movement to cede veto despite reform proposals) corroborate that the founding problem persists. Even critics of the veto acknowledge that removing it would destabilize great-power relationships. The risk of institutional coercion into existential war remains structurally present.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because this reading finds no actor systematically collecting rents from the arrangement. The P5 benefit symmetrically—each has the veto, and none extracts from the others by wielding it; the veto is exercised to protect vital interests, not to maximize extraction. Suppression is minimal (0.08) because the veto requires no active coercion to maintain; it is a standing institutional fact embedded in the Charter. Theater is near-zero (0.05) because the veto's function is direct—it prevents binding action—not performative. The constraint is so institutionally embedded that enforcement is automatic (any draft resolution that does not achieve unanimity fails on recorded vote). Accessibility to alternatives is very high (0.92 collapse) because the veto is a constitutional rule, not a negotiated agreement—no state can withdraw from it unilaterally, and no alternative institutional design that removes it is politically viable given P5 power. Resistance is minimal (0.06) because under this reading, the P5 actively defend the veto and non-P5 states understand its necessity for international stability. The measurement series is nearly flat because the veto's coordination function has been stable across the 80-year interval—no erosion of function, no accumulation of extraction, minimal performative drift. The slight upward drift in theater_ratio reflects increased rhetorical justification of veto use over time, but this does not indicate functional decay.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the beneficiary seats should compute identically under this reading—all P5 members and non-P5 states benefit from the same coordination function, and no party extracts from another through the veto. The engine should compute Rope from every seat. If the engine computes differently (e.g., Snare from a non-P5 seat), that divergence signals the reading's weakness or the data's fit. The committer frame is precisely to test whether this coordination reading is structurally defensible given the measured metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   All beneficiary seats (P5, non-P5) derive d-values near 0.0 (beneficiary end) because they all accrue genuine coordination benefit from the veto without bearing extraction costs. The veto prevents wars, stabilizes institutions, and holds great powers within the Council—all beneficiaries. There are no identified victims under this reading; the payer seats (non-P5 regional powers) pay a cost (blocked enforcement) that is structurally necessary for the collective benefit (no great-power war). This is the crux of the reading: the cost to regional enforcement is not extraction but coordination overhead—the price of institutional stability. Directionality overrides are unnecessary because the structural derivation (beneficiary + low-cost coordination + no victims) already produces the right d-profile.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is live and unambiguous: prevent great-power war through institutional unanimity. The founding problem (risk of great-power coercion into existential conflict) persists; the veto addresses it. Under the oligopoly_reading, by contrast, the founding mandate (coordination) would have decayed while the extraction mechanism (veto as power entrenchment) persists—that would be mandatrophy. Under the sovereignty_reading, the mandate (Westphalian consent principle) would have been superseded by international law evolution. This reading does NOT resolve mandatrophy because its founding problem is still live and the mechanism is still functional. The reading's weakness is not mandatrophy but explanatory fit: if the data shows rising theater_ratio or hidden asymmetric extraction among P5 members, the reading fails not because the mandate died but because the measured metrics contradict the coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_veto_designs_equivalence,
    'Could alternative institutional designs (weighted voting with great-power supermajority thresholds, enforcement authority delegation with great-power override, conditional veto triggered only on existential disputes) achieve the same great-power coordination benefit while permitting Council action on non-existential disputes?',
    'Comparative institutional analysis: model how each alternative design would affect great-power incentives to remain in the institution and to comply with enforcement decisions. Test whether any alternative eliminates the risk of institutional coercion into existential war without creating new defection incentives.',
    'If an alternative design could replicate the coordination benefit with less institutional paralysis, the veto would be over-inclusive mechanism (coordination + unnecessary restrictions), supporting the oligopoly_reading. If no alternative can eliminate the coercion risk, the veto''s coordination necessity is confirmed and the reading is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_veto_designs_equivalence, conceptual, 'Whether the specific veto design is the only institutional mechanism that solves the great-power coordination problem.').

omega_variable(
    extraction_disguised_as_coordination,
    'Does the veto systematically advance the interests of one or more P5 members at the expense of others, with the coordination narrative serving as cover for asymmetric power?',
    'Empirical analysis of veto usage: do all P5 members exercise veto at comparable rates and in comparable interest domains? Do some P5 members use veto strategically to block enforcement against allies while others exercise it mainly defensively? Does veto-blocking correlate with regional power consolidation or with great-power rivalry?',
    'High asymmetry in veto usage patterns (e.g., one P5 member vetoes 10x as often to protect regional allies while others veto only to protect existential interests) would indicate the veto has become an extraction mechanism disguised as coordination. This would support transition to oligopoly_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_disguised_as_coordination, empirical, 'Whether the veto operates symmetrically across P5 members or has become asymmetric in usage.').

omega_variable(
    collective_action_failure_without_veto_empirical,
    'Has any scenario since 1945 demonstrated that the great powers would have experienced institutional coercion into existential conflict absent the veto, or is the coordination benefit theoretical?',
    'Historical counterfactual: examine moments of Council disagreement on enforcement (Korea 1950, Vietnam, Iraq 2003) and assess whether the absence of veto would have created binding war mandates against any P5 member. Analyze whether any P5 member would have exited the institution had the veto not blocked enforcement against its interests.',
    'If every historical scenario shows that political coalitions would have prevented 9-0 votes against P5 interests anyway (e.g., because non-P5 states would have blocked enforcement), the veto''s coordination function would be hypothetical, not actual. This would weaken the reading''s empirical foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_failure_without_veto_empirical, empirical, 'Whether the veto''s coordination benefit is empirically necessary or theoretically over-insurance.').

omega_variable(
    kernel_reading_contest_asymmetry,
    'Is the coordination_reading''s claim that the veto solves a genuine collective-action problem logically compatible with the oligarchy_reading''s claim that the veto entrench geopolitical hierarchy, or do the readings foreclose each other?',
    'Logical analysis: a collective-action problem solved by asymmetric institutional design (some have veto, others do not) is inherently a mechanism that can serve both coordination AND entrenchment. The question is whether one reading''s core premise contradicts the other''s core premise (foreclose relation) or whether both can be true in different aspects (coexists_with relation). If the readings can both be true (veto solves coordination AND entrench hierarchy), they coexist; if coordination solution logically requires that the design NOT entrench, they foreclose.',
    'If the readings foreclose (they cannot both be true), the engine should flag this as a kernel-level contradiction, and one reading would be eliminated by the falsity of its core premise. If they coexist (both can be true), the readings remain live and empirical data distinguishes which is more explanatorily powerful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_asymmetry, conceptual, 'Whether the coordination and oligarchy readings are logically incompatible or structurally coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__coordination_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__coordination_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__coordination_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__coordination_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__coordination_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__coordination_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__coordination_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__coordination_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__coordination_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__coordination_reading, base_extractiveness, 80, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__coordination_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__coordination_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__coordination_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__coordination_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__coordination_reading, suppression_requirement, 60, 0.08).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__coordination_reading, suppression_requirement, 80, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.08).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The P5 veto kernel generates three structurally distinct constraints corresponding to three readings of Article 27: (1) coordination_reading (this file): veto as great-power mutual non-coercion mechanism, Rope classification, low extraction. (2) oligopoly_reading: veto as structural lock-in for geopolitical hierarchy, extracting power entrenchment rents, likely Tangled Rope or Snare. (3) sovereignty_reading: veto as instantiation of consent-to-binding-law principle for nuclear-armed great powers, Rope classification but grounded in deontological sovereignty axiom rather than collective-action failure. All three address the same institutional object (Article 27) but author different ε values, different beneficiary structures, and different cs_structure axioms. The readings are linked bidirectionally via network.affects_constraints: each reading makes claims about the same institution that the others contest. Empirical data distinguishing them (veto usage asymmetry, great-power exit risks, institutional evolution pressure) is designed to arbitrate between readings, not to adjudicate within a single reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
