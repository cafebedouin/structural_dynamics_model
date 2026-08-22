% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Amendment Threshold as Minoritarian Veto
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the minoritarian_veto reading of the
 *   supermajority_threshold kernel: the ratification threshold is read here
 *   as a mechanism whose original apportionment-based compromise has
 *   calcified into a permanent structural veto held by whichever coalition
 *   controls a blocking minority of ratifying units, regardless of
 *   contemporary population support for change. As population and power
 *   distributions have shifted since the threshold's design, the gap between
 *   what commands majority support and what can be ratified has widened,
 *   converting a historical negotiating concession into an entrenchment
 *   device. This is one of three readings of the same kernel; the
 *   consensus_safeguard_reading and adaptive_gradient_reading are separate
 *   constraint stories with their own ε and structural data, not alternate
 *   framings folded into this one.
 *
 * KEY AGENTS:
 *   - entrenched_rural_overrepresentation_beneficiaries: primary beneficiary (organized/arbitrage) — holds disproportionate blocking share
 *   - incumbent_property_interests: beneficiary/agenda_setter (powerful/arbitrage) — funds threshold maintenance
 *   - status_quo_political_coalitions: agenda_setter (institutional/mobile) — administers ratification procedure
 *   - contemporary_reform_majorities: primary target (moderate/trapped) — majority support, no ratification path
 *   - underrepresented_urban_populations: primary target (powerless/trapped) — harmed by the same malapportionment that blocks repair
 *   - constitutional_law_scholars: analytical observer — documents amendment rate collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.72).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Amendment Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'c317f92a-4c86-4c7b-ae56-5c3561c88031').
narrative_ontology:cs_kernel_codification('c317f92a-4c86-4c7b-ae56-5c3561c88031', formalized).
narrative_ontology:cs_authority_grounding('c317f92a-4c86-4c7b-ae56-5c3561c88031', lineage).
narrative_ontology:cs_interpretation_layer_present('c317f92a-4c86-4c7b-ae56-5c3561c88031').
narrative_ontology:cs_reading_relation('c317f92a-4c86-4c7b-ae56-5c3561c88031', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('c317f92a-4c86-4c7b-ae56-5c3561c88031', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('c317f92a-4c86-4c7b-ae56-5c3561c88031', foundational, supermajority_rule_is_captured_historical_privilege).
narrative_ontology:cs_axiom_status(supermajority_rule_is_captured_historical_privilege, holdable).
narrative_ontology:cs_axiom_grounding('c317f92a-4c86-4c7b-ae56-5c3561c88031', supermajority_rule_is_captured_historical_privilege, empirically_contingent).
narrative_ontology:cs_axiom('c317f92a-4c86-4c7b-ae56-5c3561c88031', foundational, contemporary_numerical_majority_carries_presumptive_legitimacy).
narrative_ontology:cs_axiom_status(contemporary_numerical_majority_carries_presumptive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c317f92a-4c86-4c7b-ae56-5c3561c88031', contemporary_numerical_majority_carries_presumptive_legitimacy, deontological).
narrative_ontology:cs_reference_frame('c317f92a-4c86-4c7b-ae56-5c3561c88031', founding_era_unit_bargain_compromise).
narrative_ontology:cs_drift_state('c317f92a-4c86-4c7b-ae56-5c3561c88031', contemporary_demographic_divergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c317f92a-4c86-4c7b-ae56-5c3561c88031', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_rural_overrepresentation_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, incumbent_property_interests).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_political_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, future_generations_bound_by_dead_hand_rules).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold disproportionate blocking power in the ratification process because the threshold's geographic or unit-based counting rules were fixed when their population share was larger. They need only hold a minority of ratifying units to block any amendment, and they exercise this repeatedly against redistricting, representation, and structural reforms that would dilute their advantage.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_rural_overrepresentation_beneficiaries, beneficiary,
    organized, generational, arbitrage, national).

% Benefit from the current constitutional property, taxation, and contract protections. They fund campaigns and legal challenges that keep the ratification threshold high and actively frame any lowering proposal as reckless. They do not need to win debates, only to hold the blocking share.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, incumbent_property_interests, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__minoritarian_veto_reading, incumbent_property_interests, agenda_setter).

% Control enough state legislatures or ratifying bodies to deny the threshold regardless of national polling. They administer the ratification process itself, scheduling votes and controlling procedural rules that determine whether a proposal even reaches a vote.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_political_coalitions, agenda_setter,
    institutional, generational, mobile, national).

% Repeatedly demonstrate majority or supermajority public support (in polling and popular vote) for specific reforms — voting rights expansion, campaign finance limits, representation fixes — that never reach the ratification threshold because a much smaller organized minority can block at the unit level. They have no exit from the jurisdiction that binds them and no legal path around the blocking minority.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_reform_majorities, payer,
    moderate, biographical, trapped, national).

% Live in the jurisdictions most affected by the malapportionment the amendment process would need to fix, but the same malapportionment that harms them also grants blocking power to the sparser jurisdictions that must ratify any change. The mechanism that causes the harm is the mechanism that prevents its own repair.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, underrepresented_urban_populations, payer,
    powerless, biographical, trapped, national).

% Inherit a ratification architecture calibrated to a historical population and power distribution they had no part in setting and cannot alter through ordinary democratic participation, since the threshold itself is what stands between them and revision.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, future_generations_bound_by_dead_hand_rules, payer,
    powerless, civilizational, trapped, national).

% Study ratification patterns, ratio of proposed to successful amendments, and the demographic profile of blocking coalitions across decades. Their work documents the gap between amendment proposals commanding broad public support and the near-total absence of successful ratification since the threshold's design era.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% Are treated in this reading primarily as the demographic vehicle for the veto rather than as a party with independent interests; the reading does not ask what these residents themselves would choose if their overrepresentation were corrected, leaving their actual preferences absent from the account.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, sparsely_populated_jurisdiction_residents, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates change so that constitutional amendments require broad-based agreement across constituent units rather than a bare simple majority, in principle preventing narrow transient majorities from rewriting foundational rules.
% TRANSFER_FUNCTION: Moves veto power from population share to unit share, transferring effective control over constitutional change from contemporary numerical majorities to whichever coalition can secure a blocking minority of ratifying units — predominantly the beneficiaries of the original apportionment.
% ABSENT_VOICES: Sparsely populated jurisdiction residents are treated as an undifferentiated blocking bloc rather than as people who might themselves support reform; contemporary reform majorities are heard in polls and popular votes but have no institutional forum where that support translates into ratification.
% DISAPPEARANCE_RATIONALE: If the supermajority ratification threshold were replaced with a simple-majority or population-proportional mechanism overnight, the backlog of majority-supported reforms — voting rights, representation corrections, campaign finance limits — would become ratifiable, and the organized minorities that currently hold veto power would lose their capacity to block; political coalition strategy, campaign spending, and legislative agenda-setting would reorganize around building simple majorities instead of denying supermajorities.
% FOUNDING_PROBLEM: The threshold was set to prevent transient majorities from rewriting foundational structural compromises made among founding constituent units, some of whom would not have joined the union without a guarantee against simple-majority override of their interests.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding period and dissenting framers' own writings attest the compromise was partly a negotiated concession to slaveholding and sparsely populated states protecting specific material interests, not a neutral consensus-protection device; contemporary comparative constitutional scholars outside any beneficiary group document that amendment rates have collapsed relative to public support for change, corroborating that the mechanism now functions as entrenchment rather than as protection of a still-living compromise.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.45 to 0.78) reflecting the growing divergence between population distribution and unit-based ratifying power as demographic shifts accumulate without corresponding threshold recalibration. Suppression is substantial (0.72) because the mechanism actively forecloses the ordinary majoritarian channel for its own revision — the veto protects itself. Theater ratio (0.40) reflects that a meaningful share of public defense of the threshold now invokes 'protecting minority interests' rhetoric disconnected from any specific interest being protected, functioning as legitimation cover for blocking power rather than substantive stabilization. Accessibility collapse (0.62) and resistance (0.71) are both substantial but not maximal: alternative amendment mechanisms (state constitutional conventions, judicial reinterpretation) remain nominally available, and organized reform movements do mount real resistance, but the practical path is extremely narrow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as the organized minority coalitions whose blocking power derives from unit-counting rather than population share, plus the property and political interests that fund and administer threshold maintenance — these get low d, benefiting from the constraint's persistence. Victims are declared as contemporary reform majorities and the populations most affected by the underlying malapportionment, whose numerical support cannot translate into ratification — these get high d, the trapped and powerless exit options confirming full-target positioning. Future generations are included as a distinct victim class because dead-hand calibration binds them without any participation in the original bargain.
 *
 * MANDATROPHY ANALYSIS:
 *   The minoritarian_veto reading treats the founding coordination story (protecting durable consensus) as obsolete cover: the founding_problem_status is authored as contested rather than flatly dead, because the reading acknowledges some structural compromise logic was real at founding while asserting that the specific interests it protected have shifted from legitimate minority protection to incumbent entrenchment. This prevents the reading from either whitewashing the mechanism as pure coordination or overclaiming it was always pure extraction — the corroboration trail (founding-era negotiators' own writings plus contemporary amendment-rate scholarship) is what licenses calling this mandatrophy rather than mere disagreement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_compromise_vs_entrenchment_boundary,
    'At what point, if any, did the threshold transition from a legitimate founding-era compromise necessary to secure union among disparate constituent units into an entrenchment mechanism serving incumbent interests unrelated to that original bargain?',
    'Historical analysis tracking whether the specific interests the threshold protects have remained continuous with the founding-era bargained interests, or whether the beneficiary class has shifted while the mechanism stayed fixed; comparative study of amendment success rates against population-support baselines over time.',
    'If continuity holds, this reading''s snare classification is weaker and the consensus_safeguard_reading''s account is closer to descriptively accurate for the same standing arrangement; if the beneficiary class has substantially shifted from founding parties to unrelated incumbent interests, this reading''s entrenchment account is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_compromise_vs_entrenchment_boundary, conceptual, 'Whether the veto beneficiary class is continuous with or divorced from the founding bargain.').

omega_variable(
    reading_selection_evidence,
    'What observable evidence would distinguish this reading (minoritarian veto) from the consensus_safeguard_reading of the same kernel, given that both readings describe the same textual threshold?',
    'Compare amendment proposal support levels (public opinion, legislative vote counts) against ratification outcomes across the historical record; a reading under which most blocked amendments commanded durable supermajority public support over multiple election cycles favors the minoritarian_veto account, while a reading under which blocked amendments consistently reflected transient or narrow majorities favors consensus_safeguard.',
    'Directly determines which reading better fits the empirical ratification record, though both readings can remain simultaneously held by different constitutional traditions regardless of this evidence (per coexists_with relation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidence, empirical, 'Empirical test separating this reading from its consensus_safeguard sibling.').

omega_variable(
    coalition_power_for_powerless_victims,
    'Can underrepresented urban populations and contemporary reform majorities build coalition power sufficient to overcome the blocking minority through means other than the amendment process itself (e.g., statutory workarounds, judicial reinterpretation, state-level reforms)?',
    'Track success rates of non-amendment reform channels pursued by reform coalitions over the interval; assess whether these channels have expanded or contracted as amendment-channel frustration has grown.',
    'If viable alternative channels exist and are used successfully, effective extraction is lower than the formal amendment-blocking picture suggests; if alternative channels are also captured or foreclosed, the snare classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_power_for_powerless_victims, empirical, 'Whether powerless victim classes retain meaningful alternative leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 80, 0.74).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(supe_su_t80, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(supe_su_t100, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the supermajority amendment threshold.' Each reading (minoritarian_veto, consensus_safeguard, adaptive_gradient) authors its own ε, beneficiaries, victims, and classification for the same standing ratification arrangement, assessed by that reading's own lights. They are linked via affects_constraints because a shift in legitimacy or public perception under one reading exerts structural pressure on the others' plausibility and political viability, without any reading being reducible to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
