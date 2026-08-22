% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty (R2P): Sovereignty as Responsibility
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the conditional sovereignty reading of the
 *   Westphalian sovereignty kernel: the claim that sovereignty carries
 *   responsibility for population protection and that systematic human rights
 *   violations legitimate external override. It is the doctrinal core of the
 *   Responsibility to Protect (R2P) framework. Structurally, it transforms
 *   sovereign states from unconditional right-bearers into conditional
 *   duty-bearers subject to international enforcement triggers. The reading
 *   stands in contest with absolute sovereignty (unconditional domestic
 *   authority) and graduated sovereignty (capacity-determined spectrum).
 *
 * KEY AGENTS:
 *   - Intervention advocates (beneficiary/organized): Norm entrepreneurs and human rights NGOs who gain influence and resources from the doctrine's activation.
 *   - Target sovereign states (payer/moderate): Weak states whose sovereignty is overridden when thresholds are declared breached.
 *   - UN Security Council (agenda_setter/institutional): Gatekeeper that adjudicates threshold breaches and authorizes enforcement.
 *   - Sovereignty absolutist states (payer/powerful): Major powers resisting the norm's expansion and bearing the diffuse cost of eroded absolute sovereignty.
 *   - Humanitarian protection coalitions (beneficiary/organized): Operational actors receiving mandate and legal cover.
 *   - At-risk civilian populations (beneficiary/powerless): Intended coordination beneficiaries with no exit from the states that threaten them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.55).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty (R2P): Sovereignty as Responsibility").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'f620d375-cb6b-4c4f-8316-4d59a85a675a').
narrative_ontology:cs_kernel_codification('f620d375-cb6b-4c4f-8316-4d59a85a675a', fixed_text).
narrative_ontology:cs_authority_grounding('f620d375-cb6b-4c4f-8316-4d59a85a675a', lineage).
narrative_ontology:cs_interpretation_layer_present('f620d375-cb6b-4c4f-8316-4d59a85a675a').
narrative_ontology:cs_reading_relation('f620d375-cb6b-4c4f-8316-4d59a85a675a', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('f620d375-cb6b-4c4f-8316-4d59a85a675a', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('f620d375-cb6b-4c4f-8316-4d59a85a675a', foundational, sovereignty_conditional_on_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('f620d375-cb6b-4c4f-8316-4d59a85a675a', sovereignty_conditional_on_responsibility, deontological).
narrative_ontology:cs_axiom('f620d375-cb6b-4c4f-8316-4d59a85a675a', foundational, systematic_violation_triggers_intervention).
narrative_ontology:cs_axiom_status(systematic_violation_triggers_intervention, holdable).
narrative_ontology:cs_axiom_grounding('f620d375-cb6b-4c4f-8316-4d59a85a675a', systematic_violation_triggers_intervention, instrumental).
narrative_ontology:cs_reference_frame('f620d375-cb6b-4c4f-8316-4d59a85a675a', conditional_sovereignty_authority).
narrative_ontology:cs_drift_state('f620d375-cb6b-4c4f-8316-4d59a85a675a', post_libya_intervention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f620d375-cb6b-4c4f-8316-4d59a85a675a', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, humanitarian_protection_coalitions).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, target_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereignty_absolutist_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, un_charter_chapter_vii_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Human rights organizations, international NGOs, and norm entrepreneurs who promote the doctrine that sovereignty is conditional on responsibility. They gain institutional influence, funding, and legitimacy when the framework is invoked, and their advocacy shapes which crises receive international attention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, intervention_advocates, beneficiary,
    organized, generational, mobile, global).

% Operational actors including UN peacekeeping bodies and humanitarian intervention forces that receive mandate, resources, and legal cover under the conditional sovereignty framework to conduct protection activities in target states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, humanitarian_protection_coalitions, beneficiary,
    organized, biographical, mobile, global).

% States whose internal conduct is declared to breach responsibility thresholds and whose sovereignty is subsequently overridden through sanctions, authorized intervention, or legitimacy erosion. They cannot easily exit the UN Charter system or the international recognition order that subjects them to this conditionalization.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, target_sovereign_states, payer,
    moderate, generational, constrained, national).

% States that reject the conditional sovereignty reading and oppose its institutionalization. They bear the diffuse cost of a normative shift that erodes the absolute sovereignty framework they depend on for internal autonomy, and they expend diplomatic capital resisting it in UN fora.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereignty_absolutist_states, payer,
    powerful, civilizational, constrained, global).

% Holds the formal trigger mechanism for conditionalization through Chapter VII authorizations. Determines whether threshold violations have occurred and sanctions enforcement action. Its authority is amplified by the doctrine because it becomes the gatekeeper of legitimate sovereignty override.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% Populations facing mass atrocities who are the stated beneficiaries of the coordination function. They receive protection when the doctrine is activated and enforced, but suffer when thresholds are politicized or when intervention itself produces collateral harm. They have no exit from the state that threatens them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, at_risk_civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__conditional_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__conditional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international community to respond collectively to mass atrocities when a state fails to protect its population, replacing ad hoc unilateral intervention with a normative framework that authorizes unified action through institutional thresholds.
% TRANSFER_FUNCTION: Transfers sovereign autonomy and decisional authority from target states to the UN Security Council and intervening coalitions when threshold violations are declared, while transferring legitimacy and institutional mandate to intervention advocates.
% ABSENT_VOICES: Populations in states where intervention occurs who suffer collateral damage; small and weak states without UNSC voice; post-colonial critics who view the doctrine as neo-imperialism but are marginalized in the norm-setting institutions of the global North.
% DISAPPEARANCE_RATIONALE: Without the conditional sovereignty constraint, the UNSC would lose its primary post-Cold War legitimacy frame for overriding sovereignty; humanitarian intervention would revert to ad hoc coalitions or total non-intervention; sovereignty absolutism would resurge; and the architecture of international civilian protection would collapse into pre-1990s paralysis.
% FOUNDING_PROBLEM: Post-Cold War mass atrocities such as the Rwandan genocide and Balkan ethnic cleansing, which occurred while an international community paralyzed by sovereignty absolutism lacked a coordinated, legitimate mechanism to authorize protection.
% FOUNDING_PROBLEM_CORROBORATION: Intervention advocates and the ICISS commission attest the problem remains live. Sovereignty absolutist states and post-colonial scholars attest the founding problem was genuine but has been captured by geopolitical interests. Independent international relations scholars are split; no outside consensus exists, and the corroboration is itself politically segmented.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate because the constraint genuinely coordinates some civilian protection while simultaneously extracting autonomy from target states; it is not pure extraction. Suppression (0.55) reflects the structural coercion of UNSC-authorized sanctions and intervention, moderated by inconsistent application. Theater ratio (0.45) captures the substantial diplomatic performance around threshold adjudication, especially the post-Libya era where rhetorical commitment outruns operational follow-through. Resistance (0.70) is high because target states and sovereignty absolutists actively contest the norm in every UN forum. The temporal series show extraction peaking at the 2011 Libya intervention, then slightly receding as great-power polarization blocked subsequent enforcement, while theater rose as the gap between rhetoric and action widened.
 *
 * PERSPECTIVAL GAP:
 *   From the intervention advocate seat, the constraint is necessary coordination to prevent atrocities that sovereignty absolutism would permit. From the target state seat, the same mechanism is external coercion that strips self-determination under procedural cover. The UNSC seat experiences the constraint as authority amplification. The engine computes these divergences from the same structural data: the beneficiary/payer declarations plus the divergent exit options (mobile for advocates, constrained for target states).
 *
 * DIRECTIONALITY LOGIC:
 *   Intervention advocates and humanitarian coalitions are structural beneficiaries (low directionality): the constraint subsidizes their institutional existence and normative project. At-risk populations are near-symmetric: genuine benefit when protection is delivered, but no exit and potential harm from intervention itself. Target sovereign states are full targets (high directionality): they bear the sovereignty extraction directly. Sovereignty absolutist states are secondary targets: they do not always face direct intervention but bear the diffuse cost of a precedent that erodes their autonomy framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-Cold War atrocities under sovereignty paralysisâwas genuine and remains live in some regions. This prevents classification as pure snare: the coordination function is not merely cover. However, the doctrine has drifted from its protective reference frame toward selective great-power tool. The founding problem status is contested, and the temporal measurements show extraction peaking after formalization. This is a living tangled rope, not a scaffold (no sunset) and not a piton (the coordination function still operates in some cases).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_selectivity,
    'Are responsibility thresholds applied consistently across all states, or are they selectively enforced based on geopolitical alignment and power?',
    'Comparative case analysis of threshold invocation rates across regime types and alliances, controlling for atrocity severity, measured via humanitarian law databases and UNSC voting records.',
    'If thresholds are selectively applied, the coordination function is subordinate to extraction, shifting classification toward snare; if consistently applied, the genuine coordination component is stronger, supporting tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_selectivity, empirical, 'Whether enforcement selectivity undermines the norm''s universality.').

omega_variable(
    protection_vs_extraction_balance,
    'Does the doctrine primarily protect at-risk civilians, or does it primarily serve as legal infrastructure for powerful states to override weak state autonomy?',
    'Outcome measurement comparing civilian casualty rates pre- and post-intervention across R2P-invoked cases, paired with analysis of intervention motives and post-intervention state capacity.',
    'A dominance of protection outcomes would lower extractiveness and shift the constraint toward rope; dominance of geopolitical capture would raise extraction and push toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_vs_extraction_balance, conceptual, 'Whether the constraint''s core function is coordination or extraction.').

omega_variable(
    enforcement_capacity_decay,
    'Has the UNSC''s capacity to enforce conditional sovereignty decayed since the Libya intervention due to great-power polarization?',
    'Time-series analysis of UNSC resolutions invoking protection thresholds, passage rates, and subsequent enforcement actions from 2011 to present.',
    'If enforcement capacity has decayed while the norm persists rhetorically, theater_ratio rises and the constraint may be drifting toward piton; if enforcement remains active but selective, tangled rope persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_decay, empirical, 'Whether institutional enforcement has decayed into performative maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t9, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 9, 0.3).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 15, 0.35).
narrative_ontology:measurement(west_tr_t21, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 21, 0.4).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 25, 0.48).
narrative_ontology:measurement(west_tr_t34, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 34, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(west_be_t9, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 9, 0.22).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(west_be_t21, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 21, 0.42).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(west_be_t34, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 34, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(west_su_t9, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 9, 0.45).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(west_su_t21, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(west_su_t34, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 34, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
