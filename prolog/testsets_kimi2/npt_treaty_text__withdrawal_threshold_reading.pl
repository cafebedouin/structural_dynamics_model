% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Ambiguity (Sovereignty-Preservation Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the withdrawal-threshold reading of the NPT
 *   kernel. It treats Article X not as a minor escape clause but as the
 *   structural center of the treaty's legitimacy: the ambiguous threshold
 *   between 'extraordinary events' (high-threshold, regime-stability reading)
 *   and sovereign discretion (low-threshold, sovereignty-preservation
 *   reading) is the device that keeps threshold states inside the regime
 *   while allowing them latent exit credibility. The North Korea 2003
 *   withdrawal tested the clause and left a persistent precedent that neither
 *   the NWS high-threshold camp nor the review conference mechanism has been
 *   able to neutralize. The constraint is claimed as tangled_rope because it
 *   carries a genuine coordination function (universal membership in a
 *   non-proliferation regime) alongside asymmetric extraction (security
 *   certainty is transferred from regime-dependent NNWS to threshold states).
 *
 * KEY AGENTS:
 *   - threshold_states: Primary beneficiary (moderate/constrained) â retains exit option credibility and diplomatic leverage
 *   - regime_dependent_nnws: Primary payer (organized/constrained) â bears regime uncertainty and security degradation
 *   - nuclear_weapon_states: Agenda-setter (institutional/arbitrage) â enforces high-threshold interpretation, bears institutional costs of ambiguity
 *   - npt_review_conferences: Formal agenda-setter (institutional/constrained) â reproduces ambiguity via consensus paralysis
 *   - legal_clarity_advocates: Excluded voice (moderate/constrained) â structurally barred from clarifying the threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.55).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Ambiguity (Sovereignty-Preservation Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, 'fabc207b-e707-4914-8c77-346d6024df0a').
narrative_ontology:cs_kernel_codification('fabc207b-e707-4914-8c77-346d6024df0a', formalized).
narrative_ontology:cs_authority_grounding('fabc207b-e707-4914-8c77-346d6024df0a', lineage).
narrative_ontology:cs_interpretation_layer_present('fabc207b-e707-4914-8c77-346d6024df0a').
narrative_ontology:cs_reading_relation('fabc207b-e707-4914-8c77-346d6024df0a', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('fabc207b-e707-4914-8c77-346d6024df0a', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_axiom('fabc207b-e707-4914-8c77-346d6024df0a', foundational, article_x_sovereignty_core).
narrative_ontology:cs_axiom_status(article_x_sovereignty_core, holdable).
narrative_ontology:cs_axiom_grounding('fabc207b-e707-4914-8c77-346d6024df0a', article_x_sovereignty_core, conventional).
narrative_ontology:cs_axiom('fabc207b-e707-4914-8c77-346d6024df0a', secondary, threshold_ambiguity_maintains_universal_membership).
narrative_ontology:cs_axiom_status(threshold_ambiguity_maintains_universal_membership, holdable).
narrative_ontology:cs_axiom_grounding('fabc207b-e707-4914-8c77-346d6024df0a', threshold_ambiguity_maintains_universal_membership, instrumental).
narrative_ontology:cs_reference_frame('fabc207b-e707-4914-8c77-346d6024df0a', sovereignty_preservation_priority).
narrative_ontology:cs_drift_state('fabc207b-e707-4914-8c77-346d6024df0a', contemporary_nonproliferation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fabc207b-e707-4914-8c77-346d6024df0a', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, regime_dependent_nnws).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced nuclear latency (Iran, Japan, South Korea, Saudi Arabia) that retain credible capacity to pursue nuclear weapons if they exit the NPT. The ambiguous Article X threshold preserves their diplomatic leverage: they remain treaty members while the implicit low-threshold interpretation sustains their sovereignty-based exit option, extracting concessions without triggering formal withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, generational, constrained, global).

% Non-nuclear-weapon states that accepted permanent non-proliferation obligations in exchange for disarmament progress and security assurances. The ambiguous withdrawal pathway exposes them to sudden neighbor proliferation without warning or reciprocal compliance, degrading the security value of their own restraint. They bear the diffuse cost of regime uncertainty.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, regime_dependent_nnws, payer,
    organized, generational, constrained, global).

% The five recognized nuclear-weapon states under the NPT that administer the regime. They promote a high-threshold interpretation of Article X to prevent cascading withdrawals but face sovereignty-based resistance from threshold states. They bear institutional costs when ambiguity undermines enforcement credibility, while also benefiting from the treaty's near-universal membership.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The consensus-based review mechanism charged with interpreting the NPT's operation. It has repeatedly reproduced the threshold ambiguity rather than resolving it, because consensus requires accommodation of both regime-stability and sovereignty-preservation positions. Its output is interpretive declarations that lack enforcement specificity.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_review_conferences, agenda_setter,
    institutional, generational, constrained, global).

% Jurists and smaller states advocating for a formal protocol clarifying Article X withdrawal procedures. They are structurally excluded because both NWS (preferring flexible high-threshold enforcement) and threshold states (preferring ambiguous low-threshold sovereignty claims) benefit from maintaining interpretive ambiguity.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, legal_clarity_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a near-universal non-proliferation regime by accommodating threshold states' sovereignty concerns through an ambiguous exit clause, preventing their overt withdrawal or overt nuclearization while maintaining formal treaty cohesion.
% TRANSFER_FUNCTION: Transfers security certainty from regime-dependent NNWS to threshold states, who gain diplomatic leverage through retained exit credibility; also transfers interpretive authority from formal review mechanisms to powerful states' unilateral readings.
% ABSENT_VOICES: Legal clarity advocates and smaller states seeking binding withdrawal protocols are excluded from effective interpretation; their exclusion is structural because both dominant camps prefer ambiguity.
% DISAPPEARANCE_RATIONALE: If the Article X threshold ambiguity were resolved (either by clear high-threshold enforcement or unambiguous low-threshold sovereignty right), threshold states would lose leverage or exit en masse, the NPT's universal membership would fracture, and the security architecture of regime-dependent NNWS would require rapid rearrangement.
% FOUNDING_PROBLEM: The NPT required sovereign consent for an otherwise permanent non-proliferation commitment; states would not join an irrevocable treaty. Article X was drafted to assure an exit pathway for 'extraordinary events' jeopardizing 'supreme interests.'
% FOUNDING_PROBLEM_CORROBORATION: NWS and threshold states both attest that sovereign exit rights were necessary for treaty adhesion. However, legal historians and regime-dependent NNWS corroborate that the original intent was a high threshold (extraordinary events), while state practice (North Korea 2003) has shifted the operational threshold downward. No party outside the benefiting ambiguity-holders corroborates the current ambiguous equilibrium as the intended solution.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) is moderate because the transfer is real but diffuse: threshold states gain leverage without cash transfers, while NNWS lose security certainty. Suppression (0.45) reflects diplomatic coercion and structural exclusion of clarity advocates rather than physical enforcement. Theater ratio (0.25) is moderate-low: review conferences produce genuine legal contestation but increasingly performative consensus documents that avoid the threshold question. Accessibility collapse (0.40) is moderate because the NPT's near-universality makes exit costly, yet states can and do proliferate outside it (India, Pakistan, Israel, North Korea). Resistance (0.50) is significant and bidirectional: NWS resist low-threshold readings, threshold states resist high-threshold enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the threshold-state seat, the constraint is a sovereignty guarantee that makes permanent non-proliferation tolerable. From the regime-dependent NNWS seat, it is an asymmetric loophole that erodes the reciprocal bargain. From the NWS seat, it is a necessary but regrettable ambiguity that must be actively managed to prevent collapse. The engine will compute these divergent types from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold_states are the structural beneficiary (d near 0.0) because the ambiguity subsidizes their bargaining position. Regime_dependent_nnws are the structural target (d near 1.0) because they bear the security cost of others' retained exit options. Nuclear_weapon_states sit near the middle but agenda-setter side (d ~0.35): they both benefit from universal membership and pay for regime instability. The review conferences are effectively symmetric (d ~0.5) because they are constrained by consensus and do not capture the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids piton classification because its founding problem â securing sovereign consent for non-proliferation â is arguably still live for threshold states, even if the current ambiguous equilibrium is contested. However, if the ambiguity has drifted from a genuine consent mechanism to a pure leverage device for states with no real intention to comply, the constraint would approach snare. The temporal measurements show rising extractiveness and theater ratio, suggesting mandatrophy pressure, but not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_empirical_status,
    'Is the Article X threshold legally high or low in contemporary customary state practice?',
    'ICJ advisory opinion or subsequent treaty protocol clarifying the evidentiary and procedural requirements for Article X withdrawal.',
    'A clarified high threshold would shift the constraint toward rope (coordinated enforcement); a confirmed low threshold would shift it toward snare (extraction from regime-dependent states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_empirical_status, empirical, 'Whether Article X operates as a genuine barrier or a nominal formality.').

omega_variable(
    north_korea_precedent_bindingness,
    'Does North Korea''s 2003 withdrawal constitute binding precedent lowering the threshold for all parties, or was it a sui generis breach?',
    'Formal legal determination by an authoritative international tribunal or unanimous state practice explicitly rejecting the North Korea precedent.',
    'If the precedent is binding, effective extraction from regime-dependent NNWS is higher than the treaty text suggests; if sui generis, the ambiguity is narrower and more containable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_bindingness, conceptual, 'Status of the North Korea withdrawal as precedent or breach.').

omega_variable(
    npt_kernel_reading_indeterminacy,
    'Is the withdrawal-threshold reading structurally separable from the NWS and NNWS readings, or do the three readings form a single interdependent commitment system?',
    'Comprehensive NPT reform process addressing Articles VI, X, and non-proliferation obligations simultaneously.',
    'If interdependent, no single constraint story captures the treaty''s operation and the kernel should be modeled holistically; if separable, the decomposition is valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(npt_kernel_reading_indeterminacy, conceptual, 'Whether the three NPT readings are decomposable or form one commitment system.').

omega_variable(
    threshold_state_restraint_genuine,
    'Do threshold states actually restrain nuclear pursuit because of the ambiguous withdrawal pathway, or would they remain non-nuclear for independent strategic reasons?',
    'Comparative analysis of threshold state nuclear latency decisions against NPT membership status and bilateral security guarantees.',
    'If restraint is independent of the ambiguity, the coordination story is cover and the constraint leans snare; if genuine, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_restraint_genuine, empirical, 'Whether threshold state compliance is causally dependent on the ambiguous exit clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(npt__tr_t12, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(npt__tr_t25, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(npt__tr_t33, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 33, 0.22).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(npt__tr_t54, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 54, 0.3).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t12, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(npt__be_t25, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(npt__be_t33, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 33, 0.48).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(npt__be_t54, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 54, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(npt__su_t12, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement(npt__su_t25, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(npt__su_t33, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 33, 0.5).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(npt__su_t54, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 54, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nnws_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'NPT' conflates three structurally distinct claims: the NWS reading (non-proliferation as binding), the NNWS reading (disarmament as binding), and the withdrawal-threshold reading (sovereignty preservation as structural). Each has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family because amendments or authoritative interpretations of one provision would cascade to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
