% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This story instantiates the grievance-threshold reading of the secession
 *   legitimacy boundary kernel: the claim that unilateral secession becomes
 *   normatively legitimate once federal actions cross an objectively
 *   demonstrable threshold of structural injustice, independent of what
 *   constitutional text permits. A resource-exporting province's government
 *   and its aligned nationalist coalition build and deploy an 'extraction'
 *   case — fiscal transfers, regulatory constraints on resource development —
 *   as the evidentiary basis for that threshold. The reading requires an
 *   objective burden of proof (this distinguishes it from the
 *   popular_sovereignty_reading, where a referendum result is
 *   self-legitimating regardless of grievance content) and its victim set
 *   (recipient provinces, internal dissenters, treaty nations) exists only if
 *   and to the extent the threshold is judged crossed. As a tangled rope, the
 *   arrangement performs a genuine coordination function — articulating
 *   limits on federal overreach that ordinary constitutional politics may
 *   fail to check — while simultaneously serving as a vehicle for
 *   concentrating political power in provincial elites and externalizing
 *   costs onto parties outside the accounting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.58).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.44).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Grievance-Threshold Reading of the Secession Legitimacy Boundary").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'd7bb0a47-1746-4f3d-a034-df92acb2af47').
narrative_ontology:cs_kernel_codification('d7bb0a47-1746-4f3d-a034-df92acb2af47', distributed).
narrative_ontology:cs_authority_grounding('d7bb0a47-1746-4f3d-a034-df92acb2af47', distributed).
narrative_ontology:cs_reading_relation('d7bb0a47-1746-4f3d-a034-df92acb2af47', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('d7bb0a47-1746-4f3d-a034-df92acb2af47', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7bb0a47-1746-4f3d-a034-df92acb2af47', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('d7bb0a47-1746-4f3d-a034-df92acb2af47', foundational, structural_injustice_can_supersede_constitutional_text).
narrative_ontology:cs_axiom_status(structural_injustice_can_supersede_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('d7bb0a47-1746-4f3d-a034-df92acb2af47', structural_injustice_can_supersede_constitutional_text, deontological).
narrative_ontology:cs_axiom('d7bb0a47-1746-4f3d-a034-df92acb2af47', foundational, injustice_claims_require_objective_evidentiary_burden).
narrative_ontology:cs_axiom_status(injustice_claims_require_objective_evidentiary_burden, holdable).
narrative_ontology:cs_axiom_grounding('d7bb0a47-1746-4f3d-a034-df92acb2af47', injustice_claims_require_objective_evidentiary_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('d7bb0a47-1746-4f3d-a034-df92acb2af47', constitutional_text_as_exclusive_authority).
narrative_ontology:cs_drift_state('d7bb0a47-1746-4f3d-a034-df92acb2af47', contemporary_resource_federalism_dispute, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7bb0a47-1746-4f3d-a034-df92acb2af47', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, resource_exporting_province_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, provincial_nationalist_coalition).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_equalization_recipient_provinces).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_dissenters).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_nations_within_province).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, structural_injustice_thesis).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__grievance_threshold_reading, remedial_secession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the province's resource revenue and frames federal equalization transfers and regulatory limits on resource development as structural extraction that has crossed a legitimizing threshold. Sets the terms of the grievance narrative, commissions the studies that quantify 'net fiscal loss,' and controls whether a referendum is called. Cannot simply leave the federation without contesting constitutional text, so it invests heavily in building the injustice case as the legitimating alternative to negotiated exit.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_exporting_province_government, agenda_setter,
    institutional, generational, constrained, national).

% A political and cultural movement that gains standing, funding, and electoral power precisely because the grievance-threshold framing exists as a legitimating vocabulary. It benefits regardless of whether secession ever occurs, because the threshold claim itself concentrates political capital and marginalizes federalist voices within the province.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_nationalist_coalition, beneficiary,
    organized, generational, mobile, regional).

% Receive equalization transfers funded in part by the resource-exporting province's tax base under the current federal formula. If the grievance-threshold claim succeeds in delegitimizing the transfer regime, or if secession removes the exporting province's revenue from the federal pool, these provinces bear a direct fiscal loss without having any voice in the threshold determination — they are not party to the province's internal referendum or its injustice accounting.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_equalization_recipient_provinces, payer,
    moderate, biographical, trapped, national).

% Residents of the exporting province who reject the injustice framing, favor continued federation, or simply distrust unilateral secession. Their vote inside a nationalist-controlled referendum process can be structurally diluted if participation thresholds, question wording, or the injustice narrative itself are set by the agenda_setter; exiting the province to preserve federal citizenship means abandoning home, employment, and community.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, provincial_minority_dissenters, payer,
    powerless, biographical, constrained, regional).

% Hold treaty relationships predating both federal and provincial sovereignty claims. The grievance-threshold reading's injustice accounting is built entirely around federal-provincial fiscal and regulatory relations and does not treat treaty consent as a precondition for legitimacy; these nations are not consulted in the threshold determination even though a successful secession would radically reconfigure whose authority governs their territory.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, indigenous_nations_within_province, excluded,
    powerless, civilizational, trapped, regional).

% Bears the political and fiscal cost of contesting the injustice narrative, defending the equalization formula and regulatory regime as constitutionally legitimate exercises of federal jurisdiction. Cannot unilaterally suppress the grievance claim without appearing to validate it, and any concession made to address the province's grievances is read by the nationalist coalition as corroboration of the threshold having been crossed.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Evaluate whether the claimed threshold of structural injustice is objectively demonstrable or is a post-hoc legitimation of a pre-existing political preference for independence. Their analysis is cited by both sides but adjudicates nothing — no institution is bound by their findings.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, provincial_nationalist_coalition).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative vocabulary for adjudicating when a constituent unit's grievances against a federation are severe enough to justify exit outside ordinary constitutional amendment channels — a genuine question in political theory about the limits of federal obligation, distinct from mere preference for independence.
% TRANSFER_FUNCTION: Moves political legitimacy and negotiating leverage from the federal government and other constituent units toward the aggrieved province's leadership; if secession proceeds, it also moves fiscal capacity (resource revenue, tax base) out of the shared federal pool and away from recipient provinces.
% ABSENT_VOICES: Indigenous nations whose treaty relationships predate the federation are not parties to the injustice accounting despite being the group most structurally affected by any redrawing of sovereign boundaries. Provincial minority dissenters and other-province taxpayers who would bear the fiscal consequences of a successful threshold claim are likewise outside the room where the threshold is defined.
% DISAPPEARANCE_RATIONALE: If the grievance-threshold framing vanished, the underlying fiscal and regulatory disputes between the province and the federation would not disappear — they would have to be litigated or negotiated through ordinary constitutional channels instead. The nationalist coalition's political project would lose its most powerful legitimating vocabulary and would likely have to fall back to pure popular-sovereignty or negotiated-amendment arguments. Whether the world 'rearranges' or 'stays the same' depends on which of these two counterfactual channels the parties consider decisive — hence contested rather than settled.
% FOUNDING_PROBLEM: Federations sometimes impose fiscal formulas, resource regulations, or constitutional arrangements that a constituent unit experiences as a persistent, one-directional transfer of wealth or authority away from itself, with no realistic path to renegotiate the terms through ordinary federal politics.
% FOUNDING_PROBLEM_CORROBORATION: The province's own commissioned fiscal studies attest the problem is live and severe. Independent economists at national universities and the federal finance ministry's public accounts dispute the magnitude and framing of the 'net loss' figures, and note that resource revenue volatility, not federal extraction, explains much of the disputed gap — this is corroboration from outside the benefiting coalition that contests, rather than confirms, the founding-problem narrative.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that the threshold-claim mechanism captures real value (political legitimacy, negotiating leverage, potential fiscal capacity) for the coalition regardless of whether the underlying injustice is as severe as claimed; it is calibrated below the platform-commission benchmark because the 'threshold' requirement genuinely gates the claim on demonstrable overreach rather than being available on demand. Suppression (0.44) is moderate: dissent within the province is not criminalized but is structurally marginalized by the coalition's control of the injustice narrative and referendum mechanics. Theater ratio (0.40) captures that a meaningful share of the fiscal-injustice accounting is performative — commissioned studies calibrated to a predetermined conclusion — while some of it tracks real, contestable fiscal facts. Accessibility collapse is low (0.35) because federalist and treaty-primacy counter-framings remain politically live and are not foreclosed by the grievance-threshold reading alone. Resistance is high (0.72) because federal government, recipient provinces, and internal dissenters actively contest the threshold claim rather than acquiescing to it.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial government's seat, the threshold framing is a necessary corrective to entrenched federal overreach that ordinary politics cannot fix. From the federal government's and recipient provinces' seats, the same claim looks like a legitimating fiction constructed to justify a predetermined preference for secession. The engine's per-seat computation should reflect this divergence without either seat's framing being treated as authoritative.
 *
 * DIRECTIONALITY LOGIC:
 *   The provincial government sets the agenda and administers the injustice accounting — near the beneficiary end structurally, though it bears real political risk if the threshold claim fails. The nationalist coalition is a pure beneficiary: it gains standing and power from the framing's existence independent of secession's success. Recipient provinces and provincial minority dissenters are targets — they bear fiscal or political costs determined by a threshold-setting process in which they have no vote. Indigenous nations are targets whose treaty-holder status the reading does not even register as a precondition, making their directionality especially acute despite their formal exclusion from the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem check separates a live grievance from a captured vocabulary: if the fiscal formula genuinely imposes an objectively verifiable one-directional transfer with no renegotiation path, the coordination function (checking real federal overreach) remains live. If the 'net loss' accounting is contested by independent analysis and persists mainly because it now anchors an entrenched nationalist coalition's power, the arrangement has drifted from grievance-articulation toward legitimacy-manufacturing — this is exactly the tangled-rope signature: real coordination function at the founding, increasing extraction as the vocabulary outlives strict evidentiary support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objective_threshold_measurability,
    'Is there any observable, non-question-begging way to measure whether federal actions have crossed a ''threshold of structural injustice,'' or is the threshold inherently constructed after the fact to match a pre-existing secessionist preference?',
    'Independent, methodologically pre-registered fiscal and regulatory-burden analysis conducted by scholars with no stake in the outcome, compared against the province-commissioned studies; convergence would support objective measurability, divergence would support post-hoc construction.',
    'If no non-question-begging measure exists, the grievance-threshold reading collapses into a disguised version of popular_sovereignty_reading (preference dressed as injustice); if a genuine measure exists, the tangled-rope coordination function is real and separable from the extraction riding on it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(objective_threshold_measurability, conceptual, 'Whether the injustice threshold is objectively measurable or is definitionally self-serving.').

omega_variable(
    treaty_precondition_omission,
    'Does the grievance-threshold reading''s silence on treaty-holder consent reflect a genuine structural claim (that federal-provincial injustice and treaty rights are separate legal questions) or does it function to exclude indigenous nations from a determination that materially affects their territory and governance?',
    'Comparative analysis against jurisdictions where treaty consent has been formally required for boundary or sovereignty changes; also cross-reference against the treaty_primacy_reading''s own account of what the grievance-threshold reading forecloses or presupposes.',
    'If exclusion is structural rather than incidental, the tangled-rope''s victim set is undercounted in this reading and the true extraction is higher than the authored metrics capture from the province-facing accounting alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_precondition_omission, conceptual, 'Whether omitting treaty consent as a precondition is a defensible legal separation or an extraction-enabling exclusion.').

omega_variable(
    coalition_capture_vs_genuine_grievance,
    'Has the grievance-threshold vocabulary become primarily an instrument of the provincial nationalist coalition''s power (Piton/Tangled Rope drift) even if the founding fiscal grievance was once substantially real?',
    'Track whether the coalition''s political fortunes and the injustice narrative''s intensity move together independent of actual fiscal formula changes — if narrative intensity rises even as the federal government makes material concessions, this indicates capture rather than responsive grievance-articulation.',
    'Confirmed capture would support reclassifying the seat-level computation for the nationalist coalition toward snare-like extraction, while the founding coordination function would remain historically real but currently dormant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_capture_vs_genuine_grievance, empirical, 'Whether the threshold narrative now serves coalition power independent of the underlying fiscal facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(sece_tr_t4, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(sece_tr_t12, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sece_be_t4, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(sece_be_t12, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sece_su_t4, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(sece_su_t12, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the secession_legitimacy_boundary kernel. Each reading has its own ε, beneficiary/victim structure, and classification: constitutional_impossibility_reading treats unilateral exit as categorically impermissible (likely mountain-adjacent from the federal seat); popular_sovereignty_reading treats referendum results as self-legitimating without an injustice burden of proof; treaty_primacy_reading subordinates both federal and provincial claims to treaty-holder consent. This reading (grievance_threshold_reading) is distinguished by its objective-burden-of-proof requirement and its conditional (threshold-gated) victim set. All four are linked via affects_constraints rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
