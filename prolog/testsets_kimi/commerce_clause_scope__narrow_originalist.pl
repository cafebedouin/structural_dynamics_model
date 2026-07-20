% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Narrow Originalist Commerce Clause Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the narrow originalist reading of the
 *   Commerce Clause: 'commerce among the several states' is limited to trade
 *   crossing state lines, 'regulate' means to make regular (facilitate)
 *   rather than prohibit, and federal power extends only to removing
 *   state-imposed barriers to interstate trade and ensuring uniform
 *   commercial rules. As a kernel reading, it claims fixed textual meaning
 *   but operates in a contested field with identifiable beneficiaries (state
 *   governments, local businesses shielded from federal regulation) and
 *   victims (federal regulators stripped of authority, civil rights claimants
 *   in recalcitrant states denied federal remedy). It functions as a
 *   commitment-system constraint grounded in fixed constitutional text,
 *   administered by an originalist judiciary, and requires active judicial
 *   enforcement to maintain against contrary federal statutes and the
 *   broad-effects alternative reading.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the narrow Commerce Clause scope
 *   - state_governments: Primary beneficiary (institutional/constrained) â retain regulatory autonomy over intrastate activity
 *   - local_intrastate_businesses: Secondary beneficiary (moderate/constrained) â avoid federal wage, environmental, and safety regulation
 *   - federal_regulators: Primary payer (institutional/constrained) â lose jurisdiction over local non-commercial activity
 *   - civil_rights_claimants_recalcitrant: Secondary payer (powerless/trapped) â denied federal commerce-power remedies in hostile states
 *   - constitutional_historians: Analytical observer â provide empirical evidence on original public meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.32).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.48).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.32).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Narrow Originalist Commerce Clause Reading").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, 'd39a9ebc-839b-4e16-af7d-aa0746ba6123').
narrative_ontology:cs_kernel_codification('d39a9ebc-839b-4e16-af7d-aa0746ba6123', fixed_text).
narrative_ontology:cs_authority_grounding('d39a9ebc-839b-4e16-af7d-aa0746ba6123', lineage).
narrative_ontology:cs_interpretation_layer_present('d39a9ebc-839b-4e16-af7d-aa0746ba6123').
narrative_ontology:cs_reading_relation('d39a9ebc-839b-4e16-af7d-aa0746ba6123', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('d39a9ebc-839b-4e16-af7d-aa0746ba6123', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('d39a9ebc-839b-4e16-af7d-aa0746ba6123', foundational, commerce_means_interstate_trade).
narrative_ontology:cs_axiom_status(commerce_means_interstate_trade, holdable).
narrative_ontology:cs_axiom_grounding('d39a9ebc-839b-4e16-af7d-aa0746ba6123', commerce_means_interstate_trade, empirically_contingent).
narrative_ontology:cs_axiom('d39a9ebc-839b-4e16-af7d-aa0746ba6123', foundational, regulate_means_make_regular).
narrative_ontology:cs_axiom_status(regulate_means_make_regular, holdable).
narrative_ontology:cs_axiom_grounding('d39a9ebc-839b-4e16-af7d-aa0746ba6123', regulate_means_make_regular, empirically_contingent).
narrative_ontology:cs_reference_frame('d39a9ebc-839b-4e16-af7d-aa0746ba6123', original_public_meaning_1787).
narrative_ontology:cs_drift_state('d39a9ebc-839b-4e16-af7d-aa0746ba6123', contemporary_post_new_deal, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d39a9ebc-839b-4e16-af7d-aa0746ba6123', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_intrastate_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, federal_regulators).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_claimants_recalcitrant).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, enumerated_powers_federalism).
narrative_ontology:constraint_vindicates(commerce_clause_scope__narrow_originalist, original_public_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Commerce Clause according to original public meaning, striking down federal statutes that regulate intrastate non-commercial activity. Derives institutional authority from the constitutional text and the interpretive obligation to preserve fixed meaning against legislative drift.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, originalist_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Retain police powers and regulatory autonomy over intrastate economic and non-commercial activity; shielded from federal commandeering and direct regulation of purely local conduct under the narrow reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Engaged in manufacturing, agriculture, or services that do not cross state lines; exempt from federal wage, hour, environmental, and safety regulations that would apply under a broad effects test.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, local_intrastate_businesses, beneficiary,
    moderate, biographical, constrained, regional).

% Administer federal statutes governing labor, environment, and civil rights; lose jurisdiction to regulate intrastate non-commercial activity when courts adopt the narrow reading, forcing regulatory retreat or statutory redesign.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, federal_regulators, payer,
    institutional, generational, constrained, national).

% Seek federal protection against local discrimination or violence in states resistant to civil rights enforcement; denied federal commerce-power remedies when the narrow reading forecloses federal jurisdiction over non-commercial local activity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, civil_rights_claimants_recalcitrant, payer,
    powerless, biographical, trapped, local).

% Argue for broad federal power to address national problems of discrimination, labor exploitation, and environmental harm; structurally disadvantaged in originalist judicial fora where historical linguistic evidence is privileged over policy consequences.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, progressive_legal_advocates, excluded,
    organized, generational, constrained, national).

% Produce empirical evidence about 18th-century word usage, ratification debates, and early federal practice; their findings are recruited by both sides but do not alone determine judicial outcomes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__narrow_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a structural boundary between federal and state regulatory authority, enabling policy experimentation, protecting local self-governance, and preventing a remote national legislature from absorbing all economic regulation.
% TRANSFER_FUNCTION: Transfers regulatory jurisdiction over intrastate non-commercial economic activity from federal legislative and administrative bodies to state governments and local actors; transfers to civil rights claimants the burden of seeking state rather than federal remedy in recalcitrant jurisdictions.
% ABSENT_VOICES: Progressive legal advocates and civil rights organizations who argue that federal power is necessary to protect minorities and workers from local majoritarian oppression; their historical-evidence claims are often discounted in originalist fora.
% DISAPPEARANCE_RATIONALE: Federal statutes regulating local non-commercial activity would survive judicial review; the national regulatory landscape would shift toward federal labor, environmental, and civil rights standards, and state regulatory autonomy would contract significantly.
% FOUNDING_PROBLEM: Fear that a remote federal legislature with enumerated powers would absorb all governance, destroying state sovereignty and local self-rule under the new Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and some state governments attest to the founding generation's concern with federal overreach. Progressive constitutional historians and civil rights organizations contest both the historical accuracy of the narrow scope and the contemporary relevance of an 18th-century economic conception of commerce to modern integrated markets.
narrative_ontology:disappearance_verdict(commerce_clause_scope__narrow_originalist, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__narrow_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__narrow_originalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__narrow_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__narrow_originalist, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).
:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.32) because the constraint primarily operates as a jurisdictional boundary rather than a direct resource transfer; its cost is the absence of federal protection or regulation rather than active taking. Suppression (0.48) reflects that the constraint depends on courts striking down federal statutes and excluding alternative statutory constructions. Theater ratio (0.35) captures that while the reading has genuine methodological adherents, its defense in contemporary constitutional politics carries significant performative dimension relative to its operational scope. Accessibility collapse (0.65) is high because once originalist methodology is accepted, the narrow linguistic evidence appears to foreclose broad alternatives, though political resistance keeps accessible alternatives live. Resistance (0.72) is high due to sustained opposition from the legal academy, federal government actors, and civil rights advocates.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judiciary and state governments experience this constraint as restoring proper constitutional order and federalism balance. Federal regulators and civil rights claimants experience the identical doctrinal structure as a barrier to necessary national action. The engine computes this divergence from the same structural facts: low directionality for beneficiaries (the constraint subsidizes their regulatory autonomy), high directionality for payers (the constraint extracts by denying federal jurisdiction).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and local businesses are structural beneficiaries: the constraint expands their regulatory freedom and contracts federal oversight, yielding low directionality (d near 0.0). Federal regulators and civil rights claimants are structural targets: the constraint directly removes federal remedies and jurisdiction they would otherwise access, yielding high directionality (d near 1.0). The originalist judiciary sits at moderate directionality: they administer the constraint and derive institutional authority from it, but are also bound by interpretive methodology.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by requiring both genuine coordination (federalism benefits, policy experimentation) and asymmetric extraction (denied federal remedies) for tangled_rope. A pure rope reading would ignore the civil rights claimants and national regulatory advocates who bear real costs. A pure snare reading would ignore the genuine coordination function of preserving state autonomy. The mandatrophy check asks whether the founding problem (fear of federal overreach) is live: it is contested, preventing automatic piton classification despite the reading's minority status across much of the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_empirical_status,
    'Does the historical record of the founding era substantiate that ''commerce'' was publicly understood to exclude all non-trading intrastate economic activity?',
    'Archival linguistics, corpus analysis of 18th-century usage, and systematic review of ratification debates and early federal practice.',
    'If the narrow linguistic claim fails, the foundational axioms collapse and the reading loses its empirical foundation, pushing toward conventional or instrumental grounding and potentially reclassifying the constraint toward pure coordination or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_empirical_status, empirical, 'Historical accuracy of the narrow originalist linguistic claims').

omega_variable(
    extraction_as_regulatory_absence,
    'Is the denial of federal regulatory jurisdiction properly classified as extraction by the constraint, or merely the absence of a federal subsidy?',
    'Comparative baseline analysis: whether the pre-constraint baseline included the federal power now denied (making the constraint a removal) or whether the constraint prevents a new extraction that would not have existed otherwise.',
    'If classified as mere absence rather than extraction, epsilon drops substantially and the constraint may compute as rope rather than tangled_rope, erasing the victim seat from the structural picture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_as_regulatory_absence, conceptual, 'Whether jurisdictional denial constitutes extraction or absence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comm_tr_t47, commerce_clause_scope__narrow_originalist, theater_ratio, 47, 0.18).
narrative_ontology:measurement(comm_tr_t94, commerce_clause_scope__narrow_originalist, theater_ratio, 94, 0.25).
narrative_ontology:measurement(comm_tr_t141, commerce_clause_scope__narrow_originalist, theater_ratio, 141, 0.58).
narrative_ontology:measurement(comm_tr_t188, commerce_clause_scope__narrow_originalist, theater_ratio, 188, 0.62).
narrative_ontology:measurement(comm_tr_t235, commerce_clause_scope__narrow_originalist, theater_ratio, 235, 0.38).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comm_be_t47, commerce_clause_scope__narrow_originalist, base_extractiveness, 47, 0.28).
narrative_ontology:measurement(comm_be_t94, commerce_clause_scope__narrow_originalist, base_extractiveness, 94, 0.2).
narrative_ontology:measurement(comm_be_t141, commerce_clause_scope__narrow_originalist, base_extractiveness, 141, 0.1).
narrative_ontology:measurement(comm_be_t188, commerce_clause_scope__narrow_originalist, base_extractiveness, 188, 0.15).
narrative_ontology:measurement(comm_be_t235, commerce_clause_scope__narrow_originalist, base_extractiveness, 235, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_scope__narrow_originalist, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the commerce_clause_scope kernel. It is structurally decomposed from the colloquial label 'Commerce Clause' because the label conflates three mutually incompatible interpretive frameworks with different epsilon values, beneficiary structures, and empirical statuses. Each reading instantiates a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
