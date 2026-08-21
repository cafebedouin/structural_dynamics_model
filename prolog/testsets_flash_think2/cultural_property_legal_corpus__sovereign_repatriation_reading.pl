% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation of Cultural Property
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereign repatriation' reading
 *   of the broader 'cultural_property_legal_corpus' kernel. From this
 *   perspective, cultural artifacts removed during colonial periods are
 *   considered the sovereign property of successor states. Their acquisition
 *   by colonial powers is deemed illegitimate extraction, and legitimate
 *   authority for these artifacts rests with states demonstrating historical
 *   continuity with the expropriated peoples. The constraint functions as a
 *   contested legal and normative framework, aiming to coordinate restitution
 *   but facing significant resistance and requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.75).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation of Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'd2022000-96c3-4190-8b9b-9f47244e3f20').
narrative_ontology:cs_kernel_codification('d2022000-96c3-4190-8b9b-9f47244e3f20', formalized).
narrative_ontology:cs_authority_grounding('d2022000-96c3-4190-8b9b-9f47244e3f20', lineage).
narrative_ontology:cs_interpretation_layer_present('d2022000-96c3-4190-8b9b-9f47244e3f20').
narrative_ontology:cs_reading_relation('d2022000-96c3-4190-8b9b-9f47244e3f20', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2022000-96c3-4190-8b9b-9f47244e3f20', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('d2022000-96c3-4190-8b9b-9f47244e3f20', foundational, cultural_property_is_state_sovereignty).
narrative_ontology:cs_axiom_status(cultural_property_is_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d2022000-96c3-4190-8b9b-9f47244e3f20', cultural_property_is_state_sovereignty, conventional).
narrative_ontology:cs_axiom('d2022000-96c3-4190-8b9b-9f47244e3f20', foundational, colonial_acquisition_is_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('d2022000-96c3-4190-8b9b-9f47244e3f20', colonial_acquisition_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('d2022000-96c3-4190-8b9b-9f47244e3f20', post_colonial_justice_framework).
narrative_ontology:cs_drift_state('d2022000-96c3-4190-8b9b-9f47244e3f20', contemporary_repatriation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d2022000-96c3-4190-8b9b-9f47244e3f20', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States claiming historical continuity with formerly colonized territories and peoples. They assert sovereign ownership over cultural artifacts removed during colonial periods and actively pursue their repatriation through diplomatic and legal channels. Their claims are often constrained by international legal frameworks and the power dynamics of former colonial powers.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary).

% Museums, galleries, and other cultural institutions in former colonial powers that currently hold artifacts acquired during the colonial era. They bear the costs of potential repatriation (loss of collection, financial expense) and actively defend their current holdings, often citing universal heritage principles or legal acquisition at the time.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_holding_institutions, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_holding_institutions, agenda_setter).

% The descendants of the original owners of cultural artifacts, whose heritage was removed during colonial rule. They experience cultural loss and discontinuity, and their claims for restitution are typically mediated through their successor states, making their direct agency limited.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples, beneficiary,
    powerless, generational, trapped, national).

% Individuals who own cultural artifacts acquired through colonial-era markets or inheritance. They face potential legal challenges and reputational damage from repatriation claims, but often have the mobility to move or sell their collections to avoid enforcement.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors, payer,
    powerful, biographical, mobile, global).

% Bodies like UNESCO that facilitate dialogue, develop international conventions, and set norms regarding cultural property. They lack direct enforcement power but play a crucial role in shaping the legal and ethical landscape for repatriation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_organizations, observer,
    institutional, generational, analytical, global).

% Groups and institutions that argue cultural artifacts are humanity's shared heritage, prioritizing preservation and universal access regardless of geographic origin. Their perspective often implicitly supports the status quo of current holding institutions, placing them outside the core framework of sovereign repatriation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear legal and normative framework for the ownership and transfer of cultural property, resolving historical injustices of colonial expropriation and ensuring cultural continuity for successor states.
% TRANSFER_FUNCTION: Transfers legal and, ideally, physical custody of cultural artifacts from colonial-era holding institutions and private collectors to successor states, along with the associated symbolic and identity capital.
% ABSENT_VOICES: Indigenous communities who may not recognize the successor state as their sole legitimate representative for cultural heritage; private collectors who view their acquisitions as legitimate property and resist state claims.
% DISAPPEARANCE_RATIONALE: If this legal framework vanished, the international system for cultural property would revert to a state of contested claims without a clear basis for repatriation. This would likely solidify current holdings, further entrenching colonial-era acquisitions, and severely diminish the capacity of successor states to reclaim their heritage.
% FOUNDING_PROBLEM: The historical injustice of colonial expropriation of cultural heritage, leading to cultural discontinuity, loss of identity, and economic disadvantage for colonized peoples, and ongoing international disputes over ownership.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, post-colonial studies academics, and numerous UN resolutions and declarations (e.g., UNDRIP) corroborate the ongoing nature and severity of this problem, independent of the direct beneficiaries.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it possesses a genuine coordination function (establishing clear ownership for repatriation and resolving disputes) but also involves asymmetric extraction (from holding institutions) and requires active enforcement to hold. Extractiveness is moderate (0.55) as the framework imposes costs on holding institutions and private collectors, but also involves significant diplomatic and legal friction for successor states. Suppression is high (0.75) due to the active resistance from holding institutions and the need for continuous diplomatic and legal pressure to compel returns. Theater ratio is moderate (0.45) as there is often performative engagement with repatriation requests without substantial actual returns, indicating a gap between stated intent and practical outcome. The temporal measurements show a gradual increase in extractiveness, suppression, and theater as the debate intensifies and holding institutions harden their stance, while successor states' claims become more formalized.
 *
 * PERSPECTIVAL GAP:
 *   The successor states perceive this framework as a necessary mechanism for historical justice and cultural restoration, a coordination of rightful ownership. Colonial holding institutions, however, often view it as an extractive demand that threatens their collections and universal access, leading to a fundamental divergence in how the constraint is experienced and interpreted.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states and expropriated peoples are the primary beneficiaries, as the constraint aims to restore their cultural heritage and sovereignty. Colonial holding institutions and private collectors are the primary targets/payers, as they bear the costs of potential repatriation and the loss of their collections. International organizations act as observers, while universal heritage advocates are structurally excluded from this reading's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the active resistance and extraction from holding institutions) or a pure Snare (ignoring the genuine coordination function of establishing a legal basis for restitution). It correctly identifies the hybrid nature where a legitimate coordination goal is intertwined with significant, contested transfers of value and power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_universal_heritage,
    'Is cultural property primarily the sovereign property of successor states, or is it humanity''s universal heritage?',
    'A shift in international legal consensus, or a landmark ruling by an international court that definitively prioritizes one framework over the other.',
    'If the universal heritage framework prevails, the claims of successor states are weakened, and the position of current holding institutions is strengthened. If sovereign property is prioritized, holding institutions face stronger legal and moral pressure for repatriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_universal_heritage, conceptual, 'Ambiguity regarding the primary legal/ethical framework for cultural property.').

omega_variable(
    state_vs_indigenous_authority,
    'Does legitimate authority over cultural property rest solely with successor states, or with specific indigenous communities within those states (or across borders)?',
    'The development of international legal frameworks that explicitly recognize indigenous self-determination over cultural heritage, or direct negotiations and agreements between indigenous communities and holding institutions.',
    'If indigenous authority is prioritized, the successor state''s role as the primary beneficiary and agenda-setter in repatriation claims is complicated or potentially superseded, leading to a different set of beneficiaries and targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_vs_indigenous_authority, conceptual, 'Ambiguity regarding the ultimate legitimate authority for cultural property claims.').

omega_variable(
    enforcement_efficacy_vs_theater,
    'Are current international legal and diplomatic mechanisms genuinely effective in compelling repatriation, or do they primarily serve to manage diplomatic friction and provide a veneer of engagement without compelling substantial returns?',
    'Empirical analysis of repatriation success rates for high-value artifacts, assessment of the actual enforcement mechanisms available, and independent evaluation of the gap between stated policy and practical outcomes.',
    'If mechanisms are found to be largely performative, the constraint''s effective suppression and theater ratio are higher than currently assessed, indicating a more extractive and less functional arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_efficacy_vs_theater, empirical, 'Uncertainty about the true efficacy of repatriation enforcement mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(cult_tr_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(cult_be_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(cult_su_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'cultural_property_legal_corpus' kernel. Its claims about state sovereignty and illegitimate colonial acquisition directly interact with and influence other readings, such as the 'universal_heritage_reading' and the 'indigenous_stewardship_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
