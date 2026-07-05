% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order as Frozen Hegemonic Extraction Complex
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic-extraction reading of the RBIO norm
 *   complex kernel: the claim that the rules-based international order is
 *   formally revisable (Charter amendment procedures, treaty renegotiation,
 *   quota reviews exist on paper) but practically frozen because the P5 veto
 *   and weighted Bretton Woods voting structures give the founding victors
 *   permanent control over whether any revision proceeds. On this reading,
 *   the selectivity of enforcement — which sanctions get applied, which
 *   interventions get authorized, which debt crises trigger austerity
 *   conditionality versus quiet forbearance — is not incidental variation in
 *   a universal system but the observable signature of an extractive function
 *   operating beneath coordination language. This is ONE of three readings of
 *   the same kernel (rbio_practice_norm_complex); the
 *   liberal_institutional_reading treats the same norms as universal and
 *   consent-based with enforcement gaps as a capacity problem, and the
 *   sovereignty_maximalist_reading treats intervention itself as
 *   presumptively illegitimate absent target-state consent. Each reading is
 *   authored as its own ε-invariant constraint story with its own
 *   beneficiary/victim structure; this file does not average across them.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: agenda_setter (institutional/arbitrage) — set and block enforcement and amendment
 *   - us_and_european_capital: beneficiary (institutional/arbitrage) — receives structural adjustment outcomes without administering the norms directly
 *   - global_south_debtor_states: payer (moderate/constrained) — bear conditionality without amendment power
 *   - structural_adjustment_populations: payer (powerless/trapped) — bear direct austerity costs
 *   - third_world_legal_scholars: excluded (moderate/constrained) — documented critique without institutional uptake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.79).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order as Frozen Hegemonic Extraction Complex").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, 'a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0').
narrative_ontology:cs_kernel_codification('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', formalized).
narrative_ontology:cs_authority_grounding('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', extraction).
narrative_ontology:cs_interpretation_layer_present('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0').
narrative_ontology:cs_reading_relation('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', foundational, formal_revisability_without_practical_amendability_constitutes_frozen_extraction).
narrative_ontology:cs_axiom_status(formal_revisability_without_practical_amendability_constitutes_frozen_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', formal_revisability_without_practical_amendability_constitutes_frozen_extraction, empirically_contingent).
narrative_ontology:cs_axiom('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', foundational, enforcement_selectivity_is_diagnostic_of_extractive_intent_not_capacity_failure).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_diagnostic_of_extractive_intent_not_capacity_failure, holdable).
narrative_ontology:cs_axiom_grounding('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', enforcement_selectivity_is_diagnostic_of_extractive_intent_not_capacity_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', post_1945_victors_settlement).
narrative_ontology:cs_drift_state('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', post_cold_war_unipolar_and_multipolar_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a2a75ee4-72f7-4a32-9c07-09d0c8fff5d0', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, bretton_woods_headquartered_institutions).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, targeted_sanctioned_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__hegemonic_extraction_reading, rules_based_order_universality_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over UN Security Council authorization of intervention and sanction, and sit atop the weighted voting structures of the IMF and World Bank. They set which violations of RBIO norms trigger enforcement and which are ignored, and can block any formal amendment to the Charter or Bretton Woods governance structure that would dilute their own position. Their exit from the constraint is never forced — they can violate the same norms they enforce against others without consequence.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Financial institutions, bondholders, and multinational firms headquartered in creditor states benefit from conditionality regimes that open debtor economies to capital flows, privatize state assets, and enforce debt service priority over domestic spending. They do not administer the norms directly but receive the structural adjustment outcomes as market access and asset acquisition opportunities.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital, beneficiary,
    institutional, civilizational, arbitrage, global).

% Formally sovereign UN members with voting rights, but dependent on IMF/World Bank lending and vulnerable to selective sanction or intervention framed as norm enforcement. Exiting the RBIO system means forfeiting access to credit markets, development financing, and diplomatic standing; remaining means accepting conditionality terms set without their effective participation in amendment processes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_debtor_states, payer,
    moderate, generational, constrained, global).

% Bear the direct costs of austerity, currency devaluation, and privatization imposed as loan conditions — reduced subsidies, wage suppression, loss of public services. They have no seat at the negotiating table where conditionality terms are set and no meaningful exit short of emigration.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, biographical, trapped, national).

% Vote in the General Assembly and participate in treaty-formation processes, but any substantive reform to Security Council composition or Bretton Woods voting weights requires P5 or major-shareholder consent that has never been granted despite decades of formal reform proposals (e.g. G4 bid, IMF quota realignment). Their formal participation coexists with practical inability to amend the structures that bind them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, non_p5_un_member_states, excluded).

% Subject to sanctions or intervention justified under RBIO norms (human rights, non-proliferation, R2P) while comparably or more severe violations by P5 states or their allies go unsanctioned. The selectivity of enforcement is, on this reading, the observable signature of the extractive function beneath the coordination language.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, targeted_sanctioned_states, payer,
    powerless, biographical, trapped, national).

% TWAIL (Third World Approaches to International Law) scholars and Global South diplomats have long argued the RBIO's foundational rules were drafted in and for a colonial-era power distribution and that formal legal equality masks substantive hierarchy. Their critique is well documented in international law scholarship but has not altered Charter or Bretton Woods governance structures.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, third_world_legal_scholars, excluded,
    moderate, generational, constrained, global).

% The formal procedure (Article 108/109) by which the Charter could in principle be amended — included for completeness as the non-agent mechanism whose practical inertness (P5 ratification requirement) is the structural fact this reading identifies as freezing the order in place.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, un_charter_amendment_process, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(rbio_practice_norm_complex__hegemonic_extraction_reading, un_charter_amendment_process).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RBIO norm complex formally coordinates collective security authorization, sovereign debt workout, and baseline human-rights standards across a system of nominally equal states, replacing unilateral force and disorderly default with rule-governed process.
% TRANSFER_FUNCTION: Moves policy sovereignty, fiscal discretion, and downstream asset value from Global South debtor states and their populations to P5 states and Western creditor capital, mediated through conditionality lending, selective sanction, and veto-gated Charter enforcement.
% ABSENT_VOICES: Global South states collectively hold a majority of General Assembly votes and have repeatedly proposed Security Council and IMF/World Bank governance reform (G4 bid, Common African Position, IMF quota realignment); TWAIL scholars have documented the colonial lineage of the framework since the 1990s. Both are audible in academic and diplomatic fora but structurally unable to convert votes or arguments into binding amendment given P5 veto and weighted-voting requirements.
% DISAPPEARANCE_RATIONALE: P5 states and Western capital would say the world rearranges catastrophically without RBIO — collective security collapses into unrestrained great-power competition and sovereign lending dries up. Global South states and TWAIL scholars would say the underlying power asymmetry the order manages would persist regardless, and its removal would only strip the legitimating language, not the material relationship; the dispute over which is true is itself part of what this reading names as the frozen-hegemony problem.
% FOUNDING_PROBLEM: Post-1945 victors sought to prevent renewed great-power war and to stabilize a global financial architecture for reconstruction and decolonization-era development lending.
% FOUNDING_PROBLEM_CORROBORATION: P5 states and IMF/World Bank leadership attest the founding problem (great-power war prevention, orderly sovereign finance) remains live and the institutions still serve it. Independent corroboration from outside the P5/creditor set — UNCTAD reports, TWAIL scholarship, the G77's repeated formal reform submissions, and IMF's own Independent Evaluation Office findings on conditionality asymmetry — documents that enforcement and governance-weight allocation diverge from the stated universal function, supporting the contested status rather than resolving it.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.79 by 2025) reflecting the reading's core claim that conditionality lending and selective enforcement transfer real resources and policy discretion from debtor states to creditor capital over an 80-year arc. Suppression (0.72) is authored as substantial but not absolute — Global South states retain formal UN votes and can and do resist rhetorically and diplomatically, but cannot convert that resistance into binding structural change given the veto/quota architecture. Theater ratio rises from 0.30 to 0.61 across the interval: the coordination language (universal norms, rule of law, sovereign equality) increasingly diverges from the enforcement pattern the reading identifies, consistent with a Tangled Rope whose coordination cover has thinned relative to its extractive operation. Accessibility collapse (0.58) and resistance (0.69) reflect a constructed-but-entrenched constraint, not a natural law: alternatives (a reformed Security Council, reweighted IMF quotas) are conceivable and have been formally proposed, but resistance to the current arrangement is real and sustained rather than theoretical.
 *
 * PERSPECTIVAL GAP:
 *   From the P5/creditor agenda-setter seat, the arrangement is genuine coordination they built, maintain, and are entitled to administer given their post-1945 responsibilities. From the Global South debtor and structural-adjustment-population seats, the identical formal structure operates as enforced extraction with no meaningful path to renegotiate terms. The engine computes this divergence from the structural data (power, exit options, beneficiary/victim declarations) authored here; the claimed_type (tangled_rope) is authored independently of the metrics and is not tuned to force a particular seat outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states and Western capital sit near the full-beneficiary end: they set enforcement priorities, hold arbitrage-grade exit (they can violate the norms they enforce against others with limited consequence), and receive the transfer. Global South debtor states and structural adjustment populations sit near the full-target end: constrained-to-trapped exit, no effective amendment power, and they bear the conditionality costs. Non-P5 UN member states occupy an intermediate position — formal participation (voting, treaty processes) coexists with practical inability to bind the P5, which is the structural signature this reading treats as diagnostic of frozen hegemony rather than functioning multilateralism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power war, stabilizing postwar reconstruction finance) is authored as contested rather than flatly dead: P5 states and IMF leadership maintain the problem is live; TWAIL scholarship and G77 reform proposals argue the problem's original form has been substantially superseded by decolonization while the governance architecture built to manage it has not been correspondingly updated. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (collective security processes, sovereign debt workout mechanisms) that liberal_institutional_reading emphasizes, while still registering the asymmetric extraction and required active enforcement this reading identifies — avoiding both the mislabeling-as-pure-extraction error (ignoring real coordination value) and the mislabeling-as-pure-coordination error (ignoring the selectivity evidence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_inertia_vs_deliberate_design,
    'Is the practical un-amendability of RBIO governance structures (P5 veto, IMF/World Bank weighted voting) an unintended path-dependency artifact of 1945-era compromise, or a deliberately preserved feature that the beneficiary states actively maintain against known reform proposals?',
    'Archival and diplomatic-history analysis of P5 and major-shareholder voting behavior on the specific reform proposals (UN Security Council expansion bids, IMF quota realignment rounds) to determine whether blocking behavior is passive (default non-consent) or active (coordinated opposition with resource investment).',
    'If deliberate, the tangled_rope classification is strongly supported and edges toward snare; if primarily inertial with no active blocking investment, the constraint edges toward piton (degraded coordination maintained by institutional inertia rather than active extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_inertia_vs_deliberate_design, empirical, 'Whether Charter/Bretton Woods un-amendability is designed extraction or inertial drift.').

omega_variable(
    enforcement_selectivity_as_diagnostic,
    'Does the well-documented pattern of selective enforcement (sanctions applied asymmetrically relative to comparable violations) reliably indicate extractive intent, or can it be explained by capacity and coalition-formation constraints without invoking hegemonic design?',
    'Comparative case analysis matching violation severity against enforcement response across P5 and non-P5 target states, controlling for coalition cost and geopolitical alignment, to test whether power position or violation severity better predicts enforcement.',
    'If power position dominates the prediction, it corroborates this reading''s extraction claim; if severity and coalition feasibility dominate, it favors the liberal_institutional_reading''s capacity-problem framing instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_as_diagnostic, empirical, 'Whether enforcement selectivity is evidence of design or byproduct of collective-action constraints.').

omega_variable(
    kernel_framing_under_determination,
    'Is the RBIO norm complex better modeled as a single kernel with three contested readings (as done here), or does the coordination function (collective security process, debt workout mechanism) constitute a structurally separate, less-contested constraint from the enforcement/conditionality layer that carries the extraction claim?',
    'Apply the ε-invariance decomposition test directly to this reading: if the coordination-process component (e.g., UN peacekeeping mandate procedures) and the conditionality-enforcement component show durably different ε under independent measurement, they may warrant splitting into further sibling stories rather than remaining one hegemonic_extraction_reading file.',
    'A finer decomposition could isolate a genuine Rope-like coordination core from a more clearly Snare-like conditionality periphery, changing which parts of the RBIO complex this reading''s stakeholders and metrics should describe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether this reading itself is a further candidate for ε-invariance decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(rbio_tr_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1971, 0.35).
narrative_ontology:measurement(rbio_tr_t1989, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 1989, 0.42).
narrative_ontology:measurement(rbio_tr_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2001, 0.5).
narrative_ontology:measurement(rbio_tr_t2011, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2011, 0.56).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 2025, 0.61).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(rbio_be_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1971, 0.51).
narrative_ontology:measurement(rbio_be_t1989, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 1989, 0.61).
narrative_ontology:measurement(rbio_be_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(rbio_be_t2011, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2011, 0.73).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 2025, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(rbio_su_t1971, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1971, 0.54).
narrative_ontology:measurement(rbio_su_t1989, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(rbio_su_t2001, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(rbio_su_t2011, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2011, 0.69).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_conditionality_lending).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the rbio_practice_norm_complex kernel (hegemonic_extraction_reading; siblings: liberal_institutional_reading, sovereignty_maximalist_reading). Each reading is authored as an independently ε-invariant constraint with its own beneficiary/victim structure and classification; they are linked here rather than merged because the same textual/institutional kernel (UN Charter, Bretton Woods governance, customary RBIO norms) supports structurally incompatible legitimacy claims depending on which party's framework is applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
