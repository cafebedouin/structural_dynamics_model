% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Grievance-Threshold Reading of the Secession Legitimacy Boundary
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the grievance-threshold reading of the contested
 *   secession-legitimacy kernel: the claim that secession becomes legitimate
 *   once federal actions cross a threshold of structural injustice,
 *   independent of constitutional text. The reading is authored here as its
 *   own constraint with its own beneficiary/victim structure and its own
 *   epsilon, per the ε-invariance principle — the
 *   constitutional-impossibility, popular-sovereignty, and treaty-primacy
 *   readings are separate constraints, not alternate measurements of this
 *   one. As authored, the doctrine functions as a coordination device (it
 *   gives genuine federal overreach a channel other than unilateral rupture)
 *   riding alongside a real extraction dynamic (resource elites and
 *   nationalist parties control what counts as 'the threshold' and capture
 *   the gains if it is certified crossed, while minorities,
 *   transfer-dependent regions, and treaty nations bear the downstream cost
 *   without having set the criteria).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.47).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '064f835c-fb94-4d19-9010-6946e713c64c').
narrative_ontology:cs_kernel_codification('064f835c-fb94-4d19-9010-6946e713c64c', distributed).
narrative_ontology:cs_authority_grounding('064f835c-fb94-4d19-9010-6946e713c64c', distributed).
narrative_ontology:cs_reading_relation('064f835c-fb94-4d19-9010-6946e713c64c', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('064f835c-fb94-4d19-9010-6946e713c64c', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('064f835c-fb94-4d19-9010-6946e713c64c', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('064f835c-fb94-4d19-9010-6946e713c64c', foundational, structural_injustice_overrides_textual_silence).
narrative_ontology:cs_axiom_status(structural_injustice_overrides_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('064f835c-fb94-4d19-9010-6946e713c64c', structural_injustice_overrides_textual_silence, instrumental).
narrative_ontology:cs_axiom('064f835c-fb94-4d19-9010-6946e713c64c', secondary, objective_burden_of_proof_required_for_extraction_claims).
narrative_ontology:cs_axiom_status(objective_burden_of_proof_required_for_extraction_claims, holdable).
narrative_ontology:cs_axiom_grounding('064f835c-fb94-4d19-9010-6946e713c64c', objective_burden_of_proof_required_for_extraction_claims, conventional).
narrative_ontology:cs_reference_frame('064f835c-fb94-4d19-9010-6946e713c64c', post_confederation_fiscal_bargain).
narrative_ontology:cs_drift_state('064f835c-fb94-4d19-9010-6946e713c64c', resource_boom_era_provincial_assertiveness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('064f835c-fb94-4d19-9010-6946e713c64c', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, resource_rich_province_elites).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, provincial_nationalist_parties).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_populations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_transfer_dependent_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_nations_within_province).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, structural_injustice_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, extra_constitutional_remedy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control provincial resource revenue and political machinery. Frame federal equalization transfers, environmental review delays, and regulatory intervention in resource projects as structural extraction crossing an injustice threshold. Stand to retain a much larger share of resource rents if secession is legitimated on these grounds; they set the terms of what counts as 'the threshold' through provincial commissions and white papers they fund.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_rich_province_elites, beneficiary,
    powerful, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, resource_rich_province_elites, agenda_setter).

% Build electoral and organizational power by advancing the grievance-threshold doctrine as legal and moral justification for referendum campaigns. Author the burden-of-proof criteria used to certify that federal overreach has occurred, and control the political calendar for invoking them.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_nationalist_parties, agenda_setter,
    organized, biographical, mobile, regional).

% Bears the burden of disproving structural injustice claims once they are raised, faces permanent litigation and political exposure over any redistributive or regulatory policy applied to the province, and risks loss of resource revenue, defense infrastructure, and constitutional coherence if the threshold is deemed crossed. Cannot exit the relationship; can only contest the injustice claim on the claimants' chosen terrain.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Non-nationalist residents, recent immigrants, and federalist-identifying citizens within the province who did not choose the grievance framing and would be bound by a secession outcome regardless of their own vote share if the threshold is certified. Have no independent forum to contest the injustice claim before it becomes politically operative.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_populations, payer,
    powerless, biographical, trapped, regional).

% Other provinces and regions whose equalization transfers, shared infrastructure, and fiscal stability depend on the resource-rich province remaining inside the federation. Bear the downstream cost of a legitimated secession without having any voice in whether the injustice threshold was actually crossed.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_transfer_dependent_regions, payer,
    powerless, biographical, trapped, national).

% Hold treaty relationships with the Crown/federal government that predate the province's existence. The grievance-threshold framework treats them as incidental parties inside the seceding territory rather than as separate treaty holders whose consent the doctrine does not require, exposing them to a jurisdictional transfer they did not negotiate.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_nations_within_province, payer,
    moderate, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_nations_within_province, excluded).

% Asked to adjudicate whether the burden of proof for 'structural injustice' has been met, without a settled evidentiary standard, since the doctrine claims legitimacy independent of constitutional text. Their rulings either anchor the threshold in a reviewable process or leave it as a political claim resolved by relative power.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, resource_rich_province_elites).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a principled-sounding release valve for a province that believes ongoing federal policy has become structurally extractive, allowing grievance to be channeled into a legible, criteria-based claim rather than unbounded unilateral action.
% TRANSFER_FUNCTION: Moves the burden of proof onto the federal government and non-consenting minorities within the province to disprove a claimed injustice, and moves resource rents, transfer obligations, and treaty jurisdiction toward whichever party controls the threshold-certification process.
% ABSENT_VOICES: Provincial minorities who reject the nationalist framing, other-province taxpayers who fund transfers into the resource-rich province, and treaty nations whose consent the doctrine does not require are all bound by any threshold determination without a seat in setting it.
% DISAPPEARANCE_RATIONALE: If the grievance-threshold doctrine were withdrawn as a legitimacy basis, secession claims would have to run entirely through the constitutional amendment process or popular-sovereignty claims alone; resource-province elites and nationalist parties would lose their strongest extra-constitutional lever, federal negotiators would lose the pressure the ambiguous threshold currently exerts, and treaty nations would gain clearer footing to insist on separate consent.
% FOUNDING_PROBLEM: Federal policy — equalization formulas, resource royalty regimes, environmental review, defense posture — can genuinely become structurally lopsided against a province over decades, and the formal amendment process gives no province an exit if federal majorities simply refuse to renegotiate.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists and federalism scholars outside the province corroborate that resource-rent redistribution formulas can become structurally regressive over time, supporting a live founding problem in some cases; but federal officials and treaty nations attest that the doctrine, as actually invoked, is used opportunistically during resource-price upswings rather than during periods of demonstrable federal overreach, suggesting the founding problem's status is asserted rather than independently verified in most invocations.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine's practical operation skews toward legitimating secession during resource-price upswings rather than during periods of demonstrable federal overreach — the founding-problem corroboration section documents this gap directly. Suppression (0.47) is moderate rather than high: minorities and treaty nations are not physically coerced, but they have no independent forum to contest a threshold determination once nationalist-controlled provincial bodies certify it. Theater ratio (0.4) captures that a substantial share of 'injustice documentation' activity (provincial commissions, white papers) functions as political mobilization rather than genuinely evidentiary process. Accessibility collapse is comparatively low (0.35) because constitutional courts, federal negotiation, and international mediation remain live alternative channels — the doctrine has not fully foreclosed other paths, which is part of why resistance (0.72) runs high: federal government, minorities, and treaty nations all actively contest the doctrine's application in specific cases.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich province elites and nationalist parties sit near the beneficiary end: they define the burden-of-proof criteria, control the certification process, and capture the resource-rent upside if secession is legitimated. The federal government sits near the target end: it bears the burden of disproof and cannot exit the relationship. Provincial minorities and transfer-dependent regions are trapped targets — bound by outcomes they did not design and cannot contest through the doctrine's own machinery. Indigenous nations occupy a distinct position: their treaty relationship is structurally prior to the province, so the doctrine's silence on requiring their separate consent is itself an extraction vector, not merely an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal majorities entrenching structurally regressive resource-transfer arrangements with no negotiated exit available to the disadvantaged province — can be genuinely live in specific historical episodes. The tangled_rope classification (rather than snare) is deliberately preserved because the coordination function is real: some federal actions do cross real injustice thresholds, and a legible criterion-based doctrine is preferable to unbounded unilateralism. But the classification also requires naming that the actual pattern of invocation, per the corroboration record, tracks resource-price cycles more than injustice severity — the doctrine is at meaningful risk of being invoked opportunistically as a legitimating gloss on rent-maximization rather than as a genuine remedy, which is exactly the seat-divergence the engine should register between the agenda-setting elites and the payer seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_certification_authority,
    'Who has legitimate authority to certify that the structural-injustice threshold has actually been crossed, given the doctrine explicitly operates independent of constitutional text and therefore outside ordinary judicial review?',
    'Track whether an independent, non-provincially-controlled body (international arbitration panel, cross-partisan federal-provincial commission) emerges to adjudicate threshold claims, versus the criteria remaining self-certified by the claimant province.',
    'If certification remains self-administered by the seceding province''s own institutions, the doctrine functions closer to a snare (extraction dressed as remedy); if an independent adjudicative body with real teeth develops, the doctrine moves closer to a genuine rope-like coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_certification_authority, conceptual, 'Ambiguity over who legitimately certifies the injustice threshold under a doctrine that rejects constitutional-textual review.').

omega_variable(
    resource_cycle_confound,
    'Does invocation of the grievance-threshold doctrine correlate more strongly with genuine federal policy shifts (injustice) or with resource-price cycles (opportunism)?',
    'Historical event-study comparing timing of doctrine invocations against (a) documented federal policy changes affecting the province and (b) commodity price indices for the province''s dominant resource sector.',
    'A strong resource-price correlation with weak policy-change correlation would support reclassifying this reading''s real-world instances toward snare; a strong policy-change correlation independent of price cycles would support the coordination-function reading and move the constraint toward a genuine tangled_rope with lower extraction weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_cycle_confound, empirical, 'Whether doctrine invocation tracks genuine injustice or commodity price opportunism.').

omega_variable(
    treaty_consent_omission,
    'Is the doctrine''s silence on requiring separate treaty-nation consent a genuine oversight correctable within this reading, or a structural feature that the reading cannot absorb without collapsing into the treaty_primacy_reading?',
    'Examine whether grievance-threshold advocates, when pressed, amend the doctrine to require treaty-holder consent as a condition of threshold certification, or whether they resist this amendment as diluting provincial sovereignty claims.',
    'If the doctrine can absorb a treaty-consent requirement without contradiction, the victim status of indigenous_nations_within_province is remediable within this reading; if advocates structurally resist the amendment, the omission is load-bearing extraction, not an accident, and the two readings are more sharply in tension than ''coexists_with'' implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_consent_omission, conceptual, 'Whether the exclusion of treaty-holder consent is fixable within this reading or is structurally necessary to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 32, 0.45).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the secession_legitimacy_boundary kernel, each authored as a structurally distinct constraint with its own epsilon per the ε-invariance principle. The grievance_threshold_reading's core premise (secession legitimacy conditional on an objectively provable structural-injustice threshold) logically forecloses the constitutional_impossibility_reading's premise (unilateral secession is never legitimate regardless of grievance) within a single legal framework — a jurist cannot simultaneously hold that crossing an injustice threshold legitimates unilateral exit and that unilateral exit is categorically impermissible. It coexists with the popular_sovereignty_reading because a province could simultaneously argue both a referendum mandate and a grievance threshold without contradiction; different factions within the same secession movement typically deploy both arguments together. It influences (without foreclosing) the treaty_primacy_reading: if the grievance-threshold doctrine gains legal traction and is invoked, it changes the resource and jurisdictional stakes treaty nations face, creating downstream pressure on whether treaty consent is treated as a precondition or an afterthought, without logically requiring either outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
