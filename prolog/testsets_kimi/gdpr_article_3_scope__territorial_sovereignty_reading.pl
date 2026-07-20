% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__territorial_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__territorial_sovereignty_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__territorial_sovereignty_reading
 *   human_readable: GDPR Article 3 Territorial Sovereignty Reading
 *   domain: technology governance / international law / privacy regulation
 *
 * SUMMARY:
 *   This constraint story instantiates the territorial sovereignty reading of
 *   GDPR Article 3 scope. Under this reading, the legitimate reach of the
 *   GDPR is bounded by territorial sovereignty, and Article 3(2)'s
 *   extraterritorial application to non-EU processors targeting or monitoring
 *   EU residents exceeds legitimate regulatory authority. The kernel is the
 *   contested scope of GDPR Article 3; this reading interprets the kernel as
 *   strictly territorial. Non-EU states benefit from preserved regulatory
 *   independence; EU data subjects and enforcement authorities bear the costs
 *   of lost extraterritorial protection. Sibling readings include the effects
 *   jurisdiction reading (extraterritoriality follows effects) and the market
 *   access reading (GDPR as conditional market standard).
 *
 * KEY AGENTS:
 *   - non_eu_states: Primary beneficiary and agenda-setter (institutional/constrained) â assert territorial sovereignty to resist GDPR extraterritoriality
 *   - eu_data_subjects: Primary payer (powerless/trapped) â lose GDPR protections for overseas processing
 *   - eu_data_protection_authorities: Secondary payer (institutional/constrained) â extraterritorial enforcement neutralized
 *   - international_legal_community: Analytical observer (analytical/arbitrage) â assesses legitimacy of extraterritorial data regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, 0.62).
domain_priors:suppression_score(gdpr_article_3_scope__territorial_sovereignty_reading, 0.72).
domain_priors:theater_ratio(gdpr_article_3_scope__territorial_sovereignty_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gdpr_article_3_scope__territorial_sovereignty_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__territorial_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__territorial_sovereignty_reading, "GDPR Article 3 Territorial Sovereignty Reading").
narrative_ontology:topic_domain(gdpr_article_3_scope__territorial_sovereignty_reading, "technology governance / international law / privacy regulation").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__territorial_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__territorial_sovereignty_reading, 'bc4a8683-eea5-440c-9c17-4977d14bb0a1').
narrative_ontology:cs_kernel_codification('bc4a8683-eea5-440c-9c17-4977d14bb0a1', formalized).
narrative_ontology:cs_authority_grounding('bc4a8683-eea5-440c-9c17-4977d14bb0a1', lineage).
narrative_ontology:cs_interpretation_layer_present('bc4a8683-eea5-440c-9c17-4977d14bb0a1').
narrative_ontology:cs_reading_relation('bc4a8683-eea5-440c-9c17-4977d14bb0a1', gdpr_article_3_scope__effects_jurisdiction_reading, forecloses).
narrative_ontology:cs_reading_relation('bc4a8683-eea5-440c-9c17-4977d14bb0a1', gdpr_article_3_scope__market_access_reading, influences).
narrative_ontology:cs_axiom('bc4a8683-eea5-440c-9c17-4977d14bb0a1', foundational, territorial_sovereignty_precludes_extraterritorial_data_regulation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_precludes_extraterritorial_data_regulation, holdable).
narrative_ontology:cs_axiom_grounding('bc4a8683-eea5-440c-9c17-4977d14bb0a1', territorial_sovereignty_precludes_extraterritorial_data_regulation, conventional).
narrative_ontology:cs_axiom('bc4a8683-eea5-440c-9c17-4977d14bb0a1', foundational, state_consent_required_for_cross_border_obligations).
narrative_ontology:cs_axiom_status(state_consent_required_for_cross_border_obligations, holdable).
narrative_ontology:cs_axiom_grounding('bc4a8683-eea5-440c-9c17-4977d14bb0a1', state_consent_required_for_cross_border_obligations, conventional).
narrative_ontology:cs_reference_frame('bc4a8683-eea5-440c-9c17-4977d14bb0a1', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('bc4a8683-eea5-440c-9c17-4977d14bb0a1', digital_globalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc4a8683-eea5-440c-9c17-4977d14bb0a1', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects).
narrative_ontology:constraint_victim(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert territorial sovereignty to resist the extraterritorial application of the GDPR under Article 3(2); enact data localization requirements and blocking statutes; preserve domestic regulatory autonomy over data processing occurring within their territory.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states, beneficiary).

% Lose GDPR protections when personal data is processed by non-EU actors lacking an EU establishment; cannot effectively seek remedy from EU data protection authorities for processing occurring outside territorial jurisdiction; bear the privacy cost of jurisdictional gaps.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_subjects, payer,
    powerless, biographical, trapped, continental).

% Attempt to enforce Article 3(2) extraterritorially but are blocked by non-EU state sovereignty claims and lack of local enforcement mechanisms; their regulatory mandate is fragmented by territorial limits, reducing effective protection for cross-border data flows.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, eu_data_protection_authorities, payer,
    institutional, generational, constrained, continental).

% Evaluates the legitimacy of extraterritorial data regulation under customary international law; documents the tension between human-rights-based jurisdiction and Westphalian sovereignty; produces competing doctrinal frameworks without enforcement authority.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__territorial_sovereignty_reading, international_legal_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__territorial_sovereignty_reading, non_eu_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral regulatory overreach by assigning exclusive data-protection authority to the territorial state; preserves a decentralized international order where jurisdiction is tied to physical borders rather than effects.
% TRANSFER_FUNCTION: Moves regulatory authority and enforcement capacity over offshore data processing from EU institutions to non-EU territorial states; transfers the cost of privacy protection away from non-EU actors and toward EU residents who lose coverage.
% ABSENT_VOICES: EU data subjects whose data is processed outside the Union are not represented in non-EU legislative or administrative processes; civil-society organizations advocating for universal data rights are marginalized in sovereignty-centric diplomatic forums.
% DISAPPEARANCE_RATIONALE: If the territorial sovereignty limit vanished, EU data protection authorities would extend enforcement to non-EU processors targeting EU residents, non-EU states would lose the primary legal shield protecting their regulatory autonomy, and data-localization regimes erected as resistance would likely collapse or be repurposed.
% FOUNDING_PROBLEM: Colonial and imperial powers historically imposed domestic laws on foreign territories without local consent; the Westphalian territorial-sovereignty framework was built to prevent such extraterritorial domination and preserve self-determination.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars from the Global South and non-EU jurisdictions attest the problem remains live, characterizing GDPR extraterritoriality as digital colonialism. EU institutions and data protection authorities assert the problem is dead, arguing that globalized data flows require effects-based jurisdiction; they corroborate the obsolescence reading from inside the benefiting framework. No neutral transnational authority attests either status without contestation.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__territorial_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__territorial_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__territorial_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__territorial_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__territorial_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__territorial_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial transfer of regulatory authority away from EU institutions. Suppression (0.72) captures the active enforcement required: data localization laws, blocking statutes, and refusal to cooperate with EU investigations. Theater ratio (0.50) acknowledges that sovereignty rhetoric often exceeds actual enforcement capacity, particularly in smaller states. Accessibility collapse (0.50) indicates that while territorial alternatives are conceptually available, global digital infrastructure makes them costly. Resistance (0.65) reflects ongoing EU pressure and rights-advocacy contestation. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Non-EU states experience the constraint as a necessary defense of sovereignty and self-determination; from their seat it is legitimate coordination. EU data subjects experience it as a rights gap; from their seat it is extraction of protective coverage. The engine computes this divergence from the structural data rather than adjudicating which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-EU states are declared beneficiaries and agenda-setters with constrained exit options; the structural derivation places them near the beneficiary end of the directionality spectrum because the constraint subsidizes their regulatory autonomy by blocking EU extraterritorial law. EU data subjects are declared victims with trapped exit; the derivation places them near the full-target end because they bear the cost of lost rights protection when data processing escapes territorial reach. EU data protection authorities are institutional victims with constrained exit; they sit toward the target end because their enforcement authority is truncated, though less severely than individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the coordination function (preventing imperial overreach) from the extraction function (creating data havens). If the coordination function were dead and only extraction remained, the constraint would degrade toward a snare. If sovereignty claims became purely performative while actual practice globalized, it would become a piton. Currently the coordination function is liveâgenuine resistance to unilateral extraterritorialityâbut asymmetric costs fall on individuals, yielding tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights,
    'Does the territorial sovereignty reading of GDPR Article 3 serve legitimate self-determination or primarily function to erode individual data rights?',
    'Comparative case law analysis: if territorial sovereignty consistently blocks effective remedies for data subjects while shielding non-compliant industries, rights-erosion dominates; if it empowers local democratic data governance, self-determination dominates.',
    'A rights-erosion resolution would push classification toward snare; a self-determination resolution would push toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights, conceptual, 'Ambiguity between legitimate sovereignty and rights-denial cover').

omega_variable(
    enforcement_capacity_gap,
    'Do non-EU states possess sufficient technical and legal capacity to enforce data localization and blocking statutes, or is sovereignty rhetoric largely performative?',
    'Empirical audit of enforcement actions: volume of GDPR-blocking penalties, actual data-localization compliance rates, and cross-border investigative cooperation refusals.',
    'If enforcement is largely theatrical, the theater_ratio is higher than authored and the constraint drifts toward piton; if enforcement is robust, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Theater gap between sovereignty rhetoric and enforcement capacity').

omega_variable(
    kernel_reading_boundary,
    'Is the territorial sovereignty reading a structurally distinct constraint or merely the negation of the effects jurisdiction reading?',
    'Examine whether territorial sovereignty generates positive institutional arrangements (data localization treaties, blocking statutes) independent of negating GDPR, or exists only as defensive litigation posture.',
    'If merely negation, it should be modeled as a dual formulation rather than an independent constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural independence of this reading from its negation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__territorial_sovereignty_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_territorial_tr_t0, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(gdpr_territorial_tr_t2, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gdpr_territorial_tr_t4, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 4, 0.44).
narrative_ontology:measurement(gdpr_territorial_tr_t6, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(gdpr_territorial_tr_t8, gdpr_article_3_scope__territorial_sovereignty_reading, theater_ratio, 8, 0.5).

% Extraction over time
narrative_ontology:measurement(gdpr_territorial_be_t0, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gdpr_territorial_be_t2, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(gdpr_territorial_be_t4, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(gdpr_territorial_be_t6, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(gdpr_territorial_be_t8, gdpr_article_3_scope__territorial_sovereignty_reading, base_extractiveness, 8, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_territorial_su_t0, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gdpr_territorial_su_t2, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(gdpr_territorial_su_t4, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(gdpr_territorial_su_t6, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(gdpr_territorial_su_t8, gdpr_article_3_scope__territorial_sovereignty_reading, suppression_requirement, 8, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__territorial_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__territorial_sovereignty_reading, gdpr_article_3_scope__market_access_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'GDPR Article 3 scope' decomposes into three structurally distinct constraints (readings): territorial_sovereignty_reading, effects_jurisdiction_reading, and market_access_reading. Each has a distinct epsilon, stakeholder structure, and classification. They are linked as a constraint family via network affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
