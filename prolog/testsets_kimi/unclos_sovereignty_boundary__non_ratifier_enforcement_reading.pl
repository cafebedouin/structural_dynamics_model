% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Customary Freedom of Navigation Independent of UNCLOS Ratification
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint instantiates the non_ratifier_enforcement_reading of the
 *   unclos_sovereignty_boundary kernel. It treats freedom of navigation as
 *   customary international law independent of UNCLOS ratification,
 *   enforceable through naval presence. Naval powers gain the ability to
 *   project maritime power and secure sea lanes without submitting to treaty
 *   obligations, while coastal states attempting to enforce EEZ exclusivity
 *   lose regulatory autonomy to unilateral naval operations. The constraint
 *   structure decouples maritime governance from the UNCLOS legal text,
 *   creating a bifurcated regime where treaty parties and non-party naval
 *   powers operate under different legitimating frameworks for the same
 *   waters.
 *
 * KEY AGENTS:
 *   - naval_powers: Primary beneficiary/agenda_setter (institutional/global/arbitrage) â enforces customary FON through naval presence without UNCLOS ratification
 *   - coastal_states_eez_claimants: Primary target (organized/regional/constrained) â lose EEZ exclusivity to naval operations
 *   - global_commercial_shipping: Secondary beneficiary (organized/global/mobile) â gains open routes without treaty fragmentation
 *   - coastal_fishing_industries: Secondary target (powerless/regional/trapped) â faces foreign naval presence in traditional fishing grounds
 *   - unclos_secretariat: Excluded analytical observer (institutional/global/analytical) â sees customary law bypassing treaty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.58).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Customary Freedom of Navigation Independent of UNCLOS Ratification").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '36ec3878-f8bb-4fed-9044-507678c01357').
narrative_ontology:cs_kernel_codification('36ec3878-f8bb-4fed-9044-507678c01357', fixed_text).
narrative_ontology:cs_authority_grounding('36ec3878-f8bb-4fed-9044-507678c01357', practice).
narrative_ontology:cs_interpretation_layer_present('36ec3878-f8bb-4fed-9044-507678c01357').
narrative_ontology:cs_reading_relation('36ec3878-f8bb-4fed-9044-507678c01357', unclos_sovereignty_boundary__strict_eez_reading, influences).
narrative_ontology:cs_reading_relation('36ec3878-f8bb-4fed-9044-507678c01357', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('36ec3878-f8bb-4fed-9044-507678c01357', foundational, customary_fon_independent_of_uncles_ratification).
narrative_ontology:cs_axiom_status(customary_fon_independent_of_uncles_ratification, holdable).
narrative_ontology:cs_axiom_grounding('36ec3878-f8bb-4fed-9044-507678c01357', customary_fon_independent_of_uncles_ratification, conventional).
narrative_ontology:cs_axiom('36ec3878-f8bb-4fed-9044-507678c01357', foundational, naval_presence_as_customary_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_as_customary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('36ec3878-f8bb-4fed-9044-507678c01357', naval_presence_as_customary_enforcement, conventional).
narrative_ontology:cs_reference_frame('36ec3878-f8bb-4fed-9044-507678c01357', customary_international_law_framework).
narrative_ontology:cs_drift_state('36ec3878-f8bb-4fed-9044-507678c01357', contemporary_maritime_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36ec3878-f8bb-4fed-9044-507678c01357', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_commercial_shipping).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, flag_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_fishing_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert freedom of navigation as customary international law independent of UNCLOS ratification, enforcing access through naval presence in contested EEZs. They set the legal frame, operate the enforcement, and benefit from unconstrained global mobility without treaty obligations.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Attempt to enforce exclusive economic zone control over resources and maritime activity near their coasts, but face naval operations that challenge their regulatory and resource authority without recourse to treaty enforcement mechanisms against non-ratifying powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_eez_claimants, payer,
    organized, generational, constrained, regional).

% Rely on open sea lanes and reduced regulatory fragmentation for international trade. They benefit from the constraint because it lowers the risk of coastal enclosure, though they do not control the enforcement apparatus.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_commercial_shipping, beneficiary,
    organized, biographical, mobile, global).

% Register commercial and private vessels that gain legal cover for passage through contested waters under the customary freedom of navigation framework, without needing to navigate a patchwork of coastal state treaty requirements.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, flag_states, beneficiary,
    moderate, biographical, mobile, global).

% Depend on local EEZ waters for livelihood but face foreign naval presence and competing commercial traffic that weakens coastal regulatory protection. They lack institutional voice in international legal forums where the constraint is asserted.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_fishing_industries, payer,
    powerless, biographical, trapped, regional).

% Administers the UNCLOS treaty framework and dispute resolution mechanisms, but is structurally sidelined when major naval powers assert customary law outside the treaty text, bypassing the institutions the secretariat maintains.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_secretariat, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining open sea lanes and preventing excessive coastal state territorial claims that would fragment the maritime commons, operating without requiring universal treaty ratification.
% TRANSFER_FUNCTION: Moves EEZ regulatory and resource control from coastal states to naval powers and international shipping, operationalized through naval presence and diplomatic assertion of customary rights.
% ABSENT_VOICES: UNCLOS dispute resolution tribunals and coastal fishing communities are structurally sidelined when naval powers assert customary law through unilateral naval operations rather than treaty mechanisms.
% DISAPPEARANCE_RATIONALE: If the customary FON principle independent of UNCLOS vanished, naval powers would lose legal justification for EEZ operations without ratification; coastal states would reassert exclusive resource control; global shipping would face fragmented regulatory regimes; the maritime order would shift toward treaty-based or bilateral arrangements.
% FOUNDING_PROBLEM: Preventing excessive maritime territorial claims that would close off international waters and impede global naval mobility and commercial shipping.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers attest the problem remains live, citing rising coastal state exclusivity. Coastal state alliances and UNCLOS parties attest the problem is addressed by treaty law and that unilateral naval enforcement undermines legal order. Independent international law scholars are divided; no consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the constraint systematically transfers EEZ regulatory authority from coastal states to naval powers without requiring treaty consent. Suppression (0.58) reflects the active naval presence and diplomatic pressure that sustain the constraint against coastal state resistance. Theater ratio (0.25) is relatively low because the coordination functionâopen sea lanesâis genuinely operational, though rhetoric about 'freedom' partially obscures the asymmetric enforcement structure. Accessibility collapse (0.45) indicates that alternatives such as UNCLOS dispute resolution or regional treaties exist but are partially collapsed by the naval powers' unilateral customary-law assertions. Resistance (0.55) captures ongoing diplomatic protests, maritime militia activity, and coastal state alliances pushing back against EEZ intrusions.
 *
 * PERSPECTIVAL GAP:
 *   From the naval power seat, the constraint is necessary public-order maintenance that prevents coastal enclosure of the global commons. From the coastal state seat, it is great-power prerogative that bypasses treaty consent frameworks and erodes sovereign resource rights. The engine computes this divergence from identical structural facts via directionality: naval powers have arbitrage-grade exit (they choose the legal frame), while coastal states are constrained within the customary-law assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers are the primary structural beneficiaries: the constraint subsidizes their global mobility by removing treaty ratification as a prerequisite for EEZ operations, yielding a low directionality value. Coastal states are the primary targets: the constraint extracts regulatory autonomy and resource control from their EEZs, yielding a high directionality value. Global commercial shipping and flag_states sit nearer the symmetric middle because they benefit from open routes without controlling the enforcement apparatus. The UNCLOS secretariat is analytical and does not participate in the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy, this might be misread as a pure rope (freedom of navigation is genuine coordination for global commerce) or a pure snare (naval hegemony extracting access from weaker coastal states). The tangled rope classification captures both truths: there is a real coordination function in maintaining sea lanes, but the enforcement mechanism asymmetrically benefits naval powers that have not ratified the treaty, extracting compliance from coastal states through naval presence rather than legal reciprocity. The founding problemâpreventing excessive territorial claimsâremains contested because treaty law already addresses it, and the unilateral customary-law mechanism functions partly as an alternative pathway that favors non-ratifiers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_status_authenticity,
    'Is freedom of navigation genuinely consolidated as customary international law, or is it a projection of naval state practice presented as universal legal obligation?',
    'Systematic review of non-naval state practice and opinio juris across the full UN membership; if a significant majority of states contest the practice or have persistently objected, customary status is weak.',
    'If not genuine custom, the constraint collapses toward snare (hegemonic extraction with coordination cover); if genuine, the coordination function is stronger and the classification edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_status_authenticity, empirical, 'Whether customary law status is empirically supported or power-projected').

omega_variable(
    military_vs_economic_applicability,
    'Does the customary FON principle apply only to military navigation, or also to resource extraction and economic activity in EEZs?',
    'Jurisprudential review of state practice and protest patterns distinguishing innocent military passage from resource exploitation assertions.',
    'If extended to economic activity, extractiveness increases substantially toward snare; if limited to military transit, coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_vs_economic_applicability, conceptual, 'Scope ambiguity of customary FON in economic vs military domains').

omega_variable(
    text_decoupling_legitimacy,
    'Does decoupling freedom of navigation from UNCLOS text strengthen international legal order by filling gaps, or fragment it by creating parallel enforcement regimes?',
    'Comparative stability analysis of maritime dispute resolution outcomes under treaty-based vs customary-law-based regimes.',
    'If fragmentation, the constraint''s coordination function is undermined and extraction dominates; if gap-filling, the tangled rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_decoupling_legitimacy, conceptual, 'Legitimacy of customary law decoupled from treaty text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 32, 0.23).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, historical_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the unclos_sovereignty_boundary kernel, which decomposes into three structurally distinct claims: strict EEZ textual limits, historical rights pre-dating UNCLOS, and customary FON independent of ratification. Each reading has a different beneficiary/victim structure and epsilon value. This reading decouples the constraint from legal text and makes naval powers beneficiaries while coastal states become victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
