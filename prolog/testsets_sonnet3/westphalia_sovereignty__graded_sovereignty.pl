% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the graded_sovereignty reading of the
 *   westphalia_sovereignty kernel: sovereignty is treated not as a binary
 *   legal status but as a scalar quantity, measured by capacity indices, with
 *   intervention legitimacy scaled continuously to the measured deficit. This
 *   is structurally distinct from the absolute_non_intervention reading
 *   (which treats sovereignty as categorical and holds any external
 *   interference per se illegitimate) and from the conditional_responsibility
 *   reading (which treats sovereignty as an all-or-nothing status forfeited
 *   only by a discrete triggering atrocity). The graded reading's defining
 *   structural move is to convert a binary legal question into a continuous,
 *   technocratically-administered metric — which is precisely what generates
 *   its distinctive beneficiary class (the scorers and score-consumers) and
 *   its distinctive victim class (states permanently positioned in the lower
 *   bands regardless of improvement, since the metric itself and its
 *   historical correlates are sticky).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.61).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '82a1ab6f-64a0-4852-938d-03ce659b3238').
narrative_ontology:cs_kernel_codification('82a1ab6f-64a0-4852-938d-03ce659b3238', distributed).
narrative_ontology:cs_authority_grounding('82a1ab6f-64a0-4852-938d-03ce659b3238', extraction).
narrative_ontology:cs_interpretation_layer_present('82a1ab6f-64a0-4852-938d-03ce659b3238').
narrative_ontology:cs_reading_relation('82a1ab6f-64a0-4852-938d-03ce659b3238', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('82a1ab6f-64a0-4852-938d-03ce659b3238', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('82a1ab6f-64a0-4852-938d-03ce659b3238', foundational, sovereignty_is_continuous_measurable_capacity).
narrative_ontology:cs_axiom_status(sovereignty_is_continuous_measurable_capacity, holdable).
narrative_ontology:cs_axiom_grounding('82a1ab6f-64a0-4852-938d-03ce659b3238', sovereignty_is_continuous_measurable_capacity, empirically_contingent).
narrative_ontology:cs_axiom('82a1ab6f-64a0-4852-938d-03ce659b3238', secondary, intervention_legitimacy_scales_with_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_scales_with_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('82a1ab6f-64a0-4852-938d-03ce659b3238', intervention_legitimacy_scales_with_capacity_deficit, instrumental).
narrative_ontology:cs_reference_frame('82a1ab6f-64a0-4852-938d-03ce659b3238', westphalian_formal_equality_of_states).
narrative_ontology:cs_drift_state('82a1ab6f-64a0-4852-938d-03ce659b3238', post_cold_war_intervention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82a1ab6f-64a0-4852-938d-03ce659b3238', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, post_colonial_states_under_tiered_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bodies such as fragile-state indices, IFI governance-quality metrics, and UN institutional-capacity assessments produce the scores that determine where a state sits on the sovereignty spectrum. They set the criteria, administer the measurement, and revise the thresholds; their own governance is not subject to an equivalent external capacity audit.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Retain full, unquestioned sovereignty by construction (their institutional capacity metrics score at ceiling) while holding the votes that authorize intervention against states scored below the threshold. They benefit from a system that formalizes their own immunity from the same graded scrutiny they apply to others.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, permanent_security_council_members, beneficiary,
    institutional, civilizational, arbitrage, global).

% Use capacity and governance-quality scores as gatekeeping criteria for lending, debt relief, and program eligibility. The same graded-sovereignty logic that authorizes political intervention also authorizes their conditionality regimes, and the two systems cross-reference each other's assessments.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, agenda_setter).

% Score below the full-sovereignty threshold on indices they did not design and cannot easily contest. Their territorial authority becomes conditional and revisable; they face trusteeship arrangements, conditional aid, transitional administrations, or armed intervention justified by their own measured deficits. Exit from the measurement regime is not available while they remain dependent on the aid, credit, or diplomatic recognition the same authorities control.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, low_capacity_states, payer,
    powerless, biographical, trapped, national).

% Have functioning governments and formal UN membership but are persistently scored toward the lower end of the capacity spectrum, often on metrics correlated with colonial-era institutional disruption. They can contest specific interventions diplomatically and build coalitions, but cannot exit the graded system itself without withdrawing from the international financial and security architecture their economies depend on.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, post_colonial_states_under_tiered_recognition, payer,
    moderate, generational, constrained, national).

% The people living inside a state scored as low-capacity are the nominal justification for intervention but are not represented in the scoring process, the intervention authorization vote, or the design of remedial trusteeship arrangements. They bear the immediate consequences of both the deficits the score captures and the interventions the score licenses.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, domestic_populations_under_intervention, excluded,
    powerless, immediate, trapped, local).

% Study the correlation between capacity metrics, colonial history, and intervention patterns; document that the states most often scored as deficient cluster along predictable historical and geopolitical lines rather than tracking governance failure independent of those lines.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, comparative_political_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common metric for the international community to triage where governance failure, humanitarian crisis, or state collapse requires external assistance, so that scarce intervention and aid capacity can be allocated according to demonstrated need rather than ad hoc political judgment alone.
% TRANSFER_FUNCTION: Moves decision-making authority over a scored state's internal governance, security arrangements, and economic policy from that state's own institutions to the capacity-evaluators and the powerful states/institutions authorized to act on low scores; correspondingly moves reputational and material costs (conditionality, loss of full recognition, intervention exposure) onto the scored state and its population.
% ABSENT_VOICES: The populations of low-scoring states, and the states themselves, have no seat in designing the capacity indices, setting the intervention threshold, or auditing the scorers. Historians and scholars of the colonial origins of many 'capacity deficits' are cited selectively but rarely determine threshold design.
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished overnight, intervention would have to be justified on some other basis (categorical non-intervention, or atrocity-triggered forfeiture) rather than a continuous capacity score; IFI conditionality regimes that piggyback on the same scoring infrastructure would lose their justificatory scaffold; permanent security council members would lose a formalized, seemingly technocratic warrant for selective intervention and would have to argue each case on its political merits.
% FOUNDING_PROBLEM: The post-Cold War proliferation of state collapse, civil war, and humanitarian catastrophe (Somalia, Rwanda, Bosnia, later Libya and Syria) exposed the practical inadequacy of treating all UN member states as equally capable of exercising the sovereignty the Charter formally grants them, and created pressure for some principled basis to calibrate international response to actual governance capacity.
% FOUNDING_PROBLEM_CORROBORATION: Capacity-evaluation authorities and permanent Security Council members attest the founding problem remains live — ongoing state fragility and humanitarian crises are cited as evidence the graded framework is still functionally necessary. Comparative political scientists and scholars from post-colonial states, writing outside the benefiting institutions, attest that the graded framework has drifted from crisis-response triage into a standing hierarchy that tracks historical power distribution more than present governance performance, and that the 'capacity deficit' framing often naturalizes conditions the intervening powers' own histories helped produce.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored substantial (0.68 by interval end) because the scoring regime channels real material and political costs (conditionality, intervention exposure, diminished diplomatic standing) onto scored states while the scoring authorities bear none of the corresponding scrutiny. Suppression (0.61) reflects that alternatives to the graded framework — categorical non-intervention or atrocity-triggered forfeiture — are actively marginalized in policy discourse once the capacity-metric apparatus becomes institutionally embedded across security and financial institutions simultaneously. Theater ratio (0.42) is elevated but not dominant: the underlying humanitarian and governance-failure problems the metrics purport to track are often real, but a growing share of scoring activity functions as retrospective justification for interventions or conditionality decisions already made on other grounds.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity-evaluation authorities' seat, the framework reads as principled technocratic triage — allocating scarce intervention capacity to where governance failure is measurably worst. From the low-capacity states' seat, the identical structure operates as a standing hierarchy that converts historically-produced institutional weakness into a permanent warrant for external oversight. The engine computes these as different seat-level classifications from the same structural data; the divergence is the finding, not an inconsistency to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluation authorities and the permanent Security Council members sit near the full-beneficiary end: they design and administer the metric, and their own institutional capacity is exempted from equivalent graded scrutiny (a structural asymmetry, not an accident). Low-capacity states and post-colonial states under tiered recognition sit near the full-target end: trapped or constrained exit, no seat in threshold design, and material consequences (aid conditionality, intervention exposure, diminished recognition) that scale directly with their measured score. Domestic populations under intervention are the deepest target class but are structurally excluded from even nominal participation in the scoring or authorization process — hence their role is 'excluded' rather than 'payer,' though they bear the most immediate costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (principled response to 1990s state collapse and humanitarian catastrophe) was genuinely live at the framework's origin. Whether it remains live is exactly the contested R5 question here: the scoring apparatus has outlasted many of the acute crises that motivated it and increasingly functions as a standing administrative hierarchy (IFI conditionality, diplomatic tiering) rather than emergency triage — a classic mandatrophy signature where the mandate's original justification narrows while the apparatus built to serve it persists and expands into adjacent domains (lending, recognition, transitional administration).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graded_sovereignty_committer_structure,
    'Is the graded_sovereignty reading a genuinely distinct kernel-reading from conditional_responsibility, or is it a continuous re-description of the same discrete forfeiture logic dressed in metric language?',
    'Compare intervention case outcomes: does authorization track a discrete atrocity trigger (conditional_responsibility) or track continuous movement along a capacity index independent of any triggering event (graded_sovereignty)? Cases where intervention is authorized purely on declining capacity scores absent an atrocity trigger would corroborate the reading as structurally distinct.',
    'If the two readings converge empirically, the beneficiary/victim structure authored here (capacity-evaluation authorities vs. low-capacity states) may actually be describing the same underlying arrangement as conditional_responsibility''s beneficiary/victim structure, undermining the claim that these are two constraints rather than one relabeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graded_sovereignty_committer_structure, conceptual, 'Whether graded_sovereignty is structurally distinct from conditional_responsibility or a continuous restatement of it.').

omega_variable(
    capacity_metric_naturalness_vs_construction,
    'Are the capacity deficits the scoring regime measures genuine, observer-independent facts about institutional function, or are they substantially constructed by the choice of metric, weighting, and historical baseline set by the same authorities who benefit from administering the scale?',
    'Independent replication of capacity scores using metrics designed by scored states themselves or by disinterested third parties, compared against the incumbent indices for systematic divergence correlated with colonial history or geopolitical alignment.',
    'If scores are substantially construction-dependent, the graded_sovereignty framework functions closer to a snare wearing coordination language (capacity measurement as pretext) than to genuine triage; if scores are robust across independently-designed metrics, the coordination function is more credible and the extraction is better understood as a cost of otherwise-legitimate triage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_naturalness_vs_construction, empirical, 'Whether capacity deficits are natural facts or artifacts of the scoring authorities'' own metric design.').

omega_variable(
    self_exemption_of_scorers,
    'Why are permanent Security Council members and major IFI shareholders never scored on the same capacity indices used to justify intervention against low-scoring states, given that state capacity theoretically varies among them too?',
    'Apply the incumbent capacity indices symmetrically to all UN member states including P5 members and G7 states, and examine whether the framework''s own internal logic would place any of them below the intervention threshold under governance-quality or institutional-capacity sub-scores.',
    'A framework that cannot survive symmetric application to its own architects is diagnostic of tangled_rope rather than genuine mountain-style natural hierarchy; this bears directly on whether the claimed_type (tangled_rope) or a harsher snare classification is structurally correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_exemption_of_scorers, conceptual, 'Whether the asymmetric exemption of powerful states from their own capacity metric is decisive evidence of extraction rather than coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(west_tr_t7, westphalia_sovereignty__graded_sovereignty, theater_ratio, 7, 0.29).
narrative_ontology:measurement(west_tr_t14, westphalia_sovereignty__graded_sovereignty, theater_ratio, 14, 0.33).
narrative_ontology:measurement(west_tr_t21, westphalia_sovereignty__graded_sovereignty, theater_ratio, 21, 0.37).
narrative_ontology:measurement(west_tr_t28, westphalia_sovereignty__graded_sovereignty, theater_ratio, 28, 0.4).
narrative_ontology:measurement(west_tr_t35, westphalia_sovereignty__graded_sovereignty, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_be_t7, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 7, 0.49).
narrative_ontology:measurement(west_be_t14, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(west_be_t21, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 21, 0.61).
narrative_ontology:measurement(west_be_t28, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 28, 0.65).
narrative_ontology:measurement(west_be_t35, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(west_su_t7, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 7, 0.49).
narrative_ontology:measurement(west_su_t14, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 14, 0.53).
narrative_ontology:measurement(west_su_t21, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 21, 0.57).
narrative_ontology:measurement(west_su_t28, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 28, 0.59).
narrative_ontology:measurement(west_su_t35, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 35, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalia_sovereignty kernel. absolute_non_intervention treats sovereignty as categorical and non-scalar; conditional_responsibility treats it as binary-and-forfeitable on a discrete atrocity trigger; graded_sovereignty (this file) treats it as continuously measured capacity. Each reading carries its own ε, beneficiary/victim structure, and claimed_type — they are not the same constraint viewed three ways, per the ε-invariance principle. Network edges here record that this reading's institutionalization (permanent scoring infrastructure) creates downstream pressure on both siblings: it erodes absolute_non_intervention's practical viability by normalizing calibrated intervention, and it partially absorbs conditional_responsibility's atrocity-trigger cases into continuous scoring language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
