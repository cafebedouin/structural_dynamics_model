% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty — Capacity-Calibrated Intervention Legitimacy
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story instantiates the 'graded sovereignty' reading of the
 *   Westphalian sovereignty kernel: sovereignty is not a binary attribute but
 *   a scalar capacity, measured against governance and institutional
 *   benchmarks, with intervention legitimacy calibrated to the measured
 *   deficit. This produces a de facto tiered state system in which the states
 *   that design and administer the capacity metrics occupy the top of the
 *   scale by construction, and the states measured against those metrics bear
 *   externally administered oversight, conditionality, and reduced practical
 *   sovereignty as a function of their score. This is a distinct constraint
 *   from the absolute_non_intervention reading (categorical inviolability, no
 *   scalar assessment) and from the conditional_responsibility reading
 *   (binary forfeiture triggered by atrocity, not continuous capacity
 *   grading) — the three are siblings in the same kernel contest, not the
 *   same constraint viewed differently. Only this reading's structure and
 *   metrics are asserted here.
 *
 * KEY AGENTS:
 *   - capacity_evaluation_authorities: institutional/arbitrage — design and administer the metrics
 *   - permanent_security_council_members: institutional/arbitrage — invoke the framework selectively
 *   - low_capacity_states: powerless/trapped — bear the oversight and conditionality
 *   - post_colonial_states_under_tutelage: powerless/trapped — experience continuity with earlier trusteeship regimes
 *   - international_law_scholars: analytical/analytical — document the correlation between capacity scores and geopolitical alignment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.6).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty — Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'e45a10b6-4b63-4f96-9ee8-f63464de4509').
narrative_ontology:cs_kernel_codification('e45a10b6-4b63-4f96-9ee8-f63464de4509', distributed).
narrative_ontology:cs_authority_grounding('e45a10b6-4b63-4f96-9ee8-f63464de4509', distributed).
narrative_ontology:cs_reading_relation('e45a10b6-4b63-4f96-9ee8-f63464de4509', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('e45a10b6-4b63-4f96-9ee8-f63464de4509', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('e45a10b6-4b63-4f96-9ee8-f63464de4509', foundational, sovereignty_is_continuous_capacity_variable).
narrative_ontology:cs_axiom_status(sovereignty_is_continuous_capacity_variable, holdable).
narrative_ontology:cs_axiom_grounding('e45a10b6-4b63-4f96-9ee8-f63464de4509', sovereignty_is_continuous_capacity_variable, empirically_contingent).
narrative_ontology:cs_axiom('e45a10b6-4b63-4f96-9ee8-f63464de4509', foundational, intervention_legitimacy_scales_with_measured_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_scales_with_measured_deficit, holdable).
narrative_ontology:cs_axiom_grounding('e45a10b6-4b63-4f96-9ee8-f63464de4509', intervention_legitimacy_scales_with_measured_deficit, instrumental).
narrative_ontology:cs_reference_frame('e45a10b6-4b63-4f96-9ee8-f63464de4509', post_westphalian_formal_equality_norm).
narrative_ontology:cs_drift_state('e45a10b6-4b63-4f96-9ee8-f63464de4509', post_cold_war_governance_conditionality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e45a10b6-4b63-4f96-9ee8-f63464de4509', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, post_colonial_states_under_tutelage).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_subject_to_trusteeship_arrangements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, western_democracies).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, state_capacity_as_measurable_variable).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, graduated_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the capacity metrics (governance indices, rule-of-law scores, fragile-states indices) that determine where a state sits on the sovereignty spectrum. Their own institutional standing is never subject to the same instruments they apply to others. They select which deficits trigger intervention discourse and which are tolerated, and they control the vocabulary in which capacity is discussed.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Invoke capacity deficits selectively to authorize or withhold intervention consistent with their own strategic interests. Their veto power means the graded framework is applied asymmetrically — allies with equivalent or worse capacity scores are not subjected to the same oversight as rivals or resource-rich weak states.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members, beneficiary,
    institutional, generational, arbitrage, global).

% Use capacity assessments to condition lending, debt relief, and reconstruction assistance on governance reforms. A low sovereignty score becomes leverage for policy conditionality that would be unacceptable if proposed against a fully sovereign borrower.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, agenda_setter).

% Are ranked, monitored, and subjected to external oversight mechanisms — trusteeship-style administration, conditional aid, peacekeeping mandates with governance components — justified by their position on the capacity scale. They have no comparable metric or forum in which to assess the capacity of the states doing the evaluating. Exiting the ranking system is not possible without exiting the international financial and security architecture entirely.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, low_capacity_states, payer,
    powerless, biographical, trapped, national).

% Experience the graded framework as continuous with earlier colonial trusteeship and mandate systems, now recoded in technocratic capacity language. Reduced treaty-making latitude, externally supervised elections, and conditioned recognition all track the same asymmetry that predated formal decolonization.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, post_colonial_states_under_tutelage, payer,
    powerless, generational, trapped, national).

% Live under governance structures partly administered by external actors invoking their state's capacity deficit. They bear the direct costs of institutional experimentation and shifting external priorities, with no direct voice in the capacity assessments that authorize the arrangement over their state.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_subject_to_trusteeship_arrangements, payer,
    powerless, immediate, trapped, local).

% Sit at the full-capacity end of the scale by construction of the metrics used, which weight institutional forms modeled on their own systems. They are treated as the baseline against which deficit is measured rather than as one point on a distribution, and are functionally exempt from the intervention logic they apply elsewhere.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, western_democracies, beneficiary,
    institutional, generational, arbitrage, global).

% Study whether capacity-based sovereignty grading is a coherent legal doctrine or a functional relabeling of great-power discretion. Document the correlation between low capacity scores and states outside the geopolitical core, and between high scores and states with veto power or alliance ties to it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and metric framework for the international community to identify states where governance failure poses risk to populations or regional stability, in principle enabling coordinated, proportionate assistance rather than binary intervention/non-intervention decisions.
% TRANSFER_FUNCTION: Moves discretionary authority over a weak state's internal governance, resource allocation conditions, and recognition status from that state's own institutions to external evaluators and the powerful states/institutions that act on their assessments; also moves reputational and financial costs of 'low capacity' labeling onto the ranked states.
% ABSENT_VOICES: Ranked states have no forum to evaluate the capacity of the evaluators, no vote proportional to their population in the institutions producing the metrics, and no mechanism to contest a low score outside appealing to the same institutions that assigned it.
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished as an operative doctrine, intervention would have to be justified on some other ground (conditional responsibility, consent, or the categorical non-intervention baseline) — the conditionality regimes attached to aid, the differentiated recognition practices, and the trusteeship-style peacekeeping mandates built on capacity-deficit reasoning would lose their legal-theoretical grounding and require re-justification or dismantlement.
% FOUNDING_PROBLEM: The absolute non-intervention norm left the international community without a legitimate vocabulary to respond to state collapse, mass atrocity, or governance failure that absolute inviolability seemed to require ignoring.
% FOUNDING_PROBLEM_CORROBORATION: Capacity-evaluation authorities and permanent members attest the framework remains necessary for calibrated, proportionate response to ongoing state fragility. Independent scholars from postcolonial and Global South legal traditions — writing outside the evaluating institutions — attest the capacity metrics reproduce colonial-era hierarchies under technocratic language and that no comparably rigorous metric is ever applied upward to the evaluators themselves.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high and rising (0.42→0.68) because the coordination story (proportionate, calibrated response to governance failure) is genuine at the margins but increasingly rides alongside conditionality regimes that extract policy concessions disproportionate to any risk the capacity deficit itself poses. Suppression is authored at a moderate-high, rising level (0.4→0.6) because ranked states face real institutional and financial consequences for contesting their score, and the mechanisms for contestation run through the same bodies that assign the score. Theater ratio rises over the interval (0.2→0.4) reflecting increasing performative invocation of 'capacity' language in cases better explained by geopolitical interest. Accessibility collapse is moderate (0.5): alternative sovereignty doctrines exist and are actively argued, so collapse is partial, not the near-total collapse of a genuine mountain. Resistance is meaningful (0.62): postcolonial states, scholars, and regional blocs actively contest the doctrine's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity-evaluation authority's seat, the framework is calibrated, humanitarian coordination — a rational improvement on binary inviolability. From the ranked state's seat, the same structure is asymmetric tutelage dressed in metric language, applied selectively according to strategic alignment rather than consistently according to the metrics themselves. The engine computes these as different seat-classifications from the same structural data; the divergence itself is the signal that the coordination story and the extraction story are riding the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluation authorities, permanent Security Council members, and international financial institutions sit near the beneficiary end: they design, invoke, and profit from the leverage the scalar framework creates, and their own institutional exit options are arbitrage-grade — they can decline to apply the framework to themselves or their allies. Low-capacity and post-colonial states sit near the full-target end: trapped exit, their governance decisions become externally reviewable, and they cannot exit the ranking system without exiting the international financial and security architecture. Western democracies benefit structurally by being the implicit benchmark against which deficit is measured, even without any single act of extraction — this is why they are listed as beneficiaries rather than neutral parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (absolute non-intervention leaving no legitimate vocabulary for responding to state collapse or atrocity) is genuinely contested as live: it has not disappeared, but the graded-capacity solution has drifted from proportionate emergency response toward a standing hierarchical apparatus that persists even where no atrocity or collapse is imminent — ordinary governance conditionality now cites capacity-scale reasoning routinely. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (some capacity assessment is informative and some intervention is proportionate) while still registering the asymmetric extraction (conditionality leverage, tutelage arrangements, unaccountable evaluators) that a pure-coordination 'rope' classification would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_construction_neutrality,
    'Are the governance/capacity metrics used to grade sovereignty neutral technical instruments, or are they constructed in the image of the states that design them, guaranteeing those states a permanent position at the top of the scale?',
    'Comparative analysis of whether capacity indices, if applied with equal rigor to evaluating-state institutions (campaign finance capture, incarceration disparities, colonial-era institutional path dependencies), would produce comparably low scores; and whether any evaluating state has ever been subject to the intervention logic it applies to others.',
    'If the metrics are self-referentially constructed to favor the evaluators, the ''scalar capacity'' framing is a naturalized hierarchy rather than a neutral measurement — this would support reclassification toward snare at the level of the evaluation apparatus, even while individual capacity-building coordination remains genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_construction_neutrality, conceptual, 'Whether capacity metrics are neutral or self-serving to the states that construct them.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does adopting graded sovereignty as the operative international-legal doctrine logically require abandoning absolute non-intervention as a live alternative, or can states selectively invoke either doctrine depending on the case?',
    'Examine state practice and ICJ/UN Security Council reasoning for cases where both doctrines are invoked by different parties to the same dispute — if states routinely argue absolute non-intervention against a graded-sovereignty claim without incoherence, the doctrines coexist in practice despite their premises'' apparent conflict.',
    'If graded sovereignty forecloses absolute non-intervention only in the analytical/legal-theory frame but not in practical diplomatic practice, this affects how strongly the ''forecloses'' relation in cs_structure should be read as descriptive of the international system versus normative-theoretical only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether the logical foreclosure between graded and absolute readings holds in practice or only in theory.').

omega_variable(
    diffuse_evaluator_accountability,
    'Is there any forum in which the capacity-evaluating states/institutions themselves are subject to an equivalent, binding capacity assessment with consequences for their own sovereignty?',
    'Survey of international institutional design for any reciprocal or symmetric capacity-review mechanism applicable to permanent Security Council members or major IFI shareholders.',
    'Absence of any such mechanism would corroborate the asymmetric-extraction reading and support treating the evaluator seat''s exit_options as durably arbitrage-grade rather than contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diffuse_evaluator_accountability, empirical, 'Whether reciprocal accountability exists for capacity evaluators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.24).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.29).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.32).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.35).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.38).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.1).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the westphalia_sovereignty kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: absolute_non_intervention (categorical inviolability, low authored extraction, closer to a rope/mountain-adjacent claim about non-interference as coordination baseline), conditional_responsibility (binary atrocity-triggered forfeiture, moderate authored extraction, tangled_rope at the R2P-invocation level), and this graded_sovereignty reading (continuous scalar capacity grading, the highest authored extraction of the three, tangled_rope with a rising extraction trajectory reflecting institutionalized tutelage). All three should link to each other via affects_constraints; ε values are NOT expected to match across the family — they are different constraints sharing a contested kernel, not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
