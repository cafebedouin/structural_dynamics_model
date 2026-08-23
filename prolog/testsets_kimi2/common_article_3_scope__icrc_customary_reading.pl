% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: ICRC Customary Law Reading of Common Article 3 Scope
 *   domain: international_law/humanitarian
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions provides minimum
 *   protections in non-international armed conflicts, but its scope was left
 *   undefined. The ICRC customary reading holds that CA3's scope is
 *   determined by evolving state practice and opinio juris tracked through
 *   customary international law, allowing gradual expansion without formal
 *   treaty amendment. This reading is contested: a state-centric reading
 *   insists on strict intensity and organization thresholds, while an
 *   expansive human rights reading would apply CA3 to any organized armed
 *   violence regardless of classification. This story instantiates the ICRC
 *   customary reading as a procedural constraint on interpretation â a
 *   coordination mechanism that extracts from state autonomy by binding
 *   states to an evolving interpretive framework administered by the ICRC and
 *   applied by international tribunals. The constraint is claimed as
 *   tangled_rope because it carries a genuine coordination function (updating
 *   IHL across states without amendment) alongside asymmetric extraction
 *   (concentrated loss of classification autonomy for states, concentrated
 *   authority gains for the ICRC and tribunals).
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary payer (institutional/constrained) â bears the cost of customary constraints on conflict classification and military autonomy.
 *   - icrc: Agenda-setter (organized/analytical) â compiles and promotes the Customary IHL Study that operationalizes the reading.
 *   - international_criminal_tribunals: Beneficiary (institutional/analytical) â gains jurisdictional and substantive legal tools from the customary framework.
 *   - conflict_affected_populations: Beneficiary (powerless/trapped) â receives expanded protective coverage passively.
 *   - non_state_armed_groups: Secondary payer (moderate/trapped) â subject to customary obligations without standing to shape opinio juris.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.55).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.52).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "ICRC Customary Law Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_law/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '4ebe271c-9481-4405-8f50-84a0f3e7d562').
narrative_ontology:cs_kernel_codification('4ebe271c-9481-4405-8f50-84a0f3e7d562', fixed_text).
narrative_ontology:cs_authority_grounding('4ebe271c-9481-4405-8f50-84a0f3e7d562', expertise).
narrative_ontology:cs_interpretation_layer_present('4ebe271c-9481-4405-8f50-84a0f3e7d562').
narrative_ontology:cs_reading_relation('4ebe271c-9481-4405-8f50-84a0f3e7d562', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ebe271c-9481-4405-8f50-84a0f3e7d562', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_axiom('4ebe271c-9481-4405-8f50-84a0f3e7d562', foundational, scope_determined_by_cil_evolution).
narrative_ontology:cs_axiom_status(scope_determined_by_cil_evolution, holdable).
narrative_ontology:cs_axiom_grounding('4ebe271c-9481-4405-8f50-84a0f3e7d562', scope_determined_by_cil_evolution, conventional).
narrative_ontology:cs_axiom('4ebe271c-9481-4405-8f50-84a0f3e7d562', secondary, icrc_study_as_presumptive_customary_evidence).
narrative_ontology:cs_axiom_status(icrc_study_as_presumptive_customary_evidence, holdable).
narrative_ontology:cs_axiom_grounding('4ebe271c-9481-4405-8f50-84a0f3e7d562', icrc_study_as_presumptive_customary_evidence, instrumental).
narrative_ontology:cs_reference_frame('4ebe271c-9481-4405-8f50-84a0f3e7d562', geneva_mandate_customary_baseline).
narrative_ontology:cs_drift_state('4ebe271c-9481-4405-8f50-84a0f3e7d562', post_2005_customary_study_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ebe271c-9481-4405-8f50-84a0f3e7d562', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, conflict_affected_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, sovereign_states).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Generate state practice and opinio juris that shapes customary international law, but are bound by evolved customary rules even when they would prefer narrower Common Article 3 scope. Their diplomatic and military autonomy is constrained by the gradual expansion of IHL applicability through ICRC-tracked custom. Exit requires persistent objection before crystallization, which is legally uncertain and diplomatically costly.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, sovereign_states, payer,
    institutional, generational, constrained, global).

% Compiles, systematizes, and promotes the Customary IHL Study, functioning as the primary institutional interpreter tracking state practice and opinio juris. Its authority and institutional role are reinforced when courts and states reference its findings. It does not enforce directly but shapes the interpretive environment.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    organized, generational, analytical, global).

% Apply the ICRC customary reading to assert jurisdiction over non-international armed conflicts and to ground individual criminal responsibility. They benefit from a ready-made body of customary rules that does not require treaty ratification by the territorial state.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, analytical, global).

% Civilians and other protected persons in situations of violence who gain the protective floor of Common Article 3 because the customary reading expands or stabilizes its applicability beyond what a strict state-centric treaty reading would provide. They do not choose whether the constraint applies; they receive its protections passively.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, conflict_affected_populations, beneficiary,
    powerless, immediate, trapped, national).

% Subject to Common Article 3 obligations under the customary reading but lack recognized capacity to contribute state practice or opinio juris. Their conduct is judged against evolving standards they did not shape, while their entitlement to protections remains contested.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the gradual updating and harmonization of Common Article 3 scope across state and judicial actors without requiring universal treaty amendment, by establishing an interpretive procedure tied to evolving state practice and opinio juris.
% TRANSFER_FUNCTION: Transfers interpretive authority over the threshold and content of non-international armed conflict regulation from exclusive state treaty-party control to the ICRC customary-law tracking process and international criminal tribunals, while transferring protective obligations onto states and non-state parties in emerging conflict types.
% ABSENT_VOICES: Non-state armed groups lack standing to form opinio juris or contribute state practice in the ICRC framework; expansive human rights advocates argue the customary process is too deferential to state consent and too slow to protect victims in low-intensity violence.
% DISAPPEARANCE_RATIONALE: If the ICRC customary reading vanished, states would revert to strict treaty-text or unilateral classification of conflicts, tribunal jurisdiction over non-international armed conflicts would narrow, and the protective floor for victims in ambiguous conflicts would contract to the lowest common denominator of ratified text.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions fixed Common Article 3 text but left scope ambiguous; subsequent conflicts (wars of decolonization, internal strife, terrorism) did not fit the original assumptions, and formal treaty amendment was politically impossible.
% FOUNDING_PROBLEM_CORROBORATION: States and the ICRC attest the problem is live through the 2005 Customary IHL Study and ongoing state practice submissions. International criminal tribunals corroborate by repeatedly relying on customary law to fill gaps in treaty-based jurisdiction. No external party entirely outside the benefiting set disputes the original ambiguity; even state-centric readings acknowledge the gap but propose a different filler.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint meaningfully limits state autonomy to classify conflicts and exclude low-intensity violence from IHL, but it does not extract material rents. Suppression is moderate (0.52): the reading persists through active interpretive work by the ICRC and citations by international courts, which suppress competing state-centric textual readings in judicial fora. Theater is low (0.20): the ICRC study and customary analysis is substantive doctrinal work, not performative compliance. Accessibility collapse is moderate (0.45): alternatives (strict treaty textualism, pure state-centric thresholds) remain live arguments in state diplomatic practice and academic commentary. Resistance is moderate (0.50): several states have contested specific rules in the ICRC study, and the state-centric reading retains significant adherents.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign_states seat experiences the constraint as a loss of discretionary authority over when IHL applies, especially in counter-terrorism and law-enforcement operations. The ICRC and tribunal seats experience it as a necessary procedural mechanism to prevent legal obsolescence. The conflict-affected populations seat experiences it as a passive protective floor that may or may not materialize depending on judicial and diplomatic uptake. The engine should compute high directionality for states and non-state armed groups, low directionality for tribunals and protected populations, and a moderate fallback for the ICRC agenda-setter.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conflict_affected_populations, international_criminal_tribunals) are assigned low directionality: the constraint subsidizes their protective coverage and jurisdictional reach. Victims (sovereign_states, non_state_armed_groups) are assigned high directionality: the constraint extracts autonomy and imposes obligations. The ICRC, as agenda_setter with analytical exit, sits near the middle via canonical fallback. No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the procedural coordination function (flexible IHL updating via custom) from the asymmetric extraction (state autonomy loss). A purely state-centric reading would mislabel the coordination as non-existent, collapsing the constraint into a snare or mountain. A purely expansive human rights reading would mislabel the extraction as benign universalism, collapsing it into a rope. The ICRC reading sits between: genuine coordination plus asymmetric extraction, which the tangled_rope classification captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cil_crystallization_authority,
    'Who authoritatively determines when state practice and opinio juris have sufficiently evolved to expand Common Article 3 scope, and is that determination procedural or substantive?',
    'Comparative analysis of international judicial citations versus state objections to ICRC Study rules; tracking instances where courts adopted customary rules that specific states contested.',
    'If crystallization is effectively declared by ICRC and courts without genuine state consensus, extraction is higher than coordination; if it tracks real convergent practice, coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cil_crystallization_authority, conceptual, 'Authority and timing of customary crystallization for CA3 scope.').

omega_variable(
    kernel_reading_stability,
    'Is the ICRC customary reading structurally stable against its sibling readings, or does it function as a transitional compromise between state-centric sovereignty and expansive human rights universalism?',
    'Longitudinal analysis of state submissions to ICRC and judicial opinions: does the reading drift toward the expansive or state-centric pole over time?',
    'If unstable, the constraint may be a scaffold rather than a tangled rope; if stable, it represents a distinct equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Structural stability of the ICRC reading relative to kernel siblings.').

omega_variable(
    state_consent_erosion,
    'To what extent does the ICRC customary reading extract from state consent by treating non-ratified or contested rules as binding custom?',
    'Mapping state objections to individual rules in the ICRC Customary IHL Study against subsequent tribunal and UN practice.',
    'High erosion would indicate stronger extraction from sovereign_states; low erosion would support a rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_consent_erosion, empirical, 'Degree of state consent erosion under the ICRC customary reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_icrc_custom_tr_t0, common_article_3_scope__icrc_customary_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ca3_icrc_custom_tr_t5, common_article_3_scope__icrc_customary_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ca3_icrc_custom_tr_t10, common_article_3_scope__icrc_customary_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ca3_icrc_custom_tr_t15, common_article_3_scope__icrc_customary_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(ca3_icrc_custom_tr_t20, common_article_3_scope__icrc_customary_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(ca3_icrc_custom_tr_t25, common_article_3_scope__icrc_customary_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(ca3_icrc_custom_tr_t29, common_article_3_scope__icrc_customary_reading, theater_ratio, 29, 0.2).

% Extraction over time
narrative_ontology:measurement(ca3_icrc_custom_be_t0, common_article_3_scope__icrc_customary_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ca3_icrc_custom_be_t5, common_article_3_scope__icrc_customary_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ca3_icrc_custom_be_t10, common_article_3_scope__icrc_customary_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ca3_icrc_custom_be_t15, common_article_3_scope__icrc_customary_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(ca3_icrc_custom_be_t20, common_article_3_scope__icrc_customary_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ca3_icrc_custom_be_t25, common_article_3_scope__icrc_customary_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(ca3_icrc_custom_be_t29, common_article_3_scope__icrc_customary_reading, base_extractiveness, 29, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ca3_icrc_custom_su_t0, common_article_3_scope__icrc_customary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ca3_icrc_custom_su_t5, common_article_3_scope__icrc_customary_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ca3_icrc_custom_su_t10, common_article_3_scope__icrc_customary_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ca3_icrc_custom_su_t15, common_article_3_scope__icrc_customary_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(ca3_icrc_custom_su_t20, common_article_3_scope__icrc_customary_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(ca3_icrc_custom_su_t25, common_article_3_scope__icrc_customary_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(ca3_icrc_custom_su_t29, common_article_3_scope__icrc_customary_reading, suppression_requirement, 29, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, identity_coordination).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the common_article_3_scope kernel family, which decomposes the colloquial label 'CA3 scope' into three structurally distinct readings: state-centric (strict thresholds), ICRC customary (evolving custom), and expansive human rights (universal floor). Each reading has a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
