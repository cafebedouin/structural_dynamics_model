% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'binding referee' reading of the WTO
 *   Dispute Settlement Body's (DSB) authority. Under this reading, DSB panels
 *   issue rulings that are legally binding on member states, requiring them
 *   to alter domestic policies found to violate WTO agreements. Member states
 *   are understood to have surrendered a degree of policy discretion
 *   (sovereignty) in WTO-covered domains in exchange for the benefits of a
 *   rules-based multilateral trading system. Non-compliance with rulings can
 *   lead to authorized trade retaliation, which acts as the primary
 *   enforcement mechanism. This reading emphasizes the judicial-like function
 *   of the DSB.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.65).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'f4ae17d5-80ea-454c-b8f7-cd753130327e').
narrative_ontology:cs_kernel_codification('f4ae17d5-80ea-454c-b8f7-cd753130327e', formalized).
narrative_ontology:cs_authority_grounding('f4ae17d5-80ea-454c-b8f7-cd753130327e', lineage).
narrative_ontology:cs_interpretation_layer_present('f4ae17d5-80ea-454c-b8f7-cd753130327e').
narrative_ontology:cs_reading_relation('f4ae17d5-80ea-454c-b8f7-cd753130327e', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_reading_relation('f4ae17d5-80ea-454c-b8f7-cd753130327e', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('f4ae17d5-80ea-454c-b8f7-cd753130327e', foundational, treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('f4ae17d5-80ea-454c-b8f7-cd753130327e', treaty_obligations_are_binding, conventional).
narrative_ontology:cs_axiom('f4ae17d5-80ea-454c-b8f7-cd753130327e', foundational, judicial_review_is_essential_for_rules_based_order).
narrative_ontology:cs_axiom_status(judicial_review_is_essential_for_rules_based_order, holdable).
narrative_ontology:cs_axiom_grounding('f4ae17d5-80ea-454c-b8f7-cd753130327e', judicial_review_is_essential_for_rules_based_order, instrumental).
narrative_ontology:cs_reference_frame('f4ae17d5-80ea-454c-b8f7-cd753130327e', rules_based_multilateralism).
narrative_ontology:cs_drift_state('f4ae17d5-80ea-454c-b8f7-cd753130327e', contemporary_appellate_body_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f4ae17d5-80ea-454c-b8f7-cd753130327e', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_states_exporting).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, wto_member_states_importing).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_industries_targeted_by_rulings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, international_trade_lawyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dispute settlement process, forms panels, adopts rulings, and authorizes retaliation. Its legitimacy rests on member states' treaty commitments. It benefits from the system's stability and its role as final arbiter.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from predictable market access and the ability to challenge trade barriers imposed by other members. They gain from rulings that open markets or prevent protectionist measures, even if they sometimes face adverse rulings themselves.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_exporting, beneficiary,
    institutional, biographical, mobile, global).

% Are bound by DSB rulings, which can require them to change domestic policies (e.g., subsidies, tariffs, regulations) that are found to violate WTO agreements. Non-compliance can lead to authorized trade retaliation, imposing economic costs.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_importing, payer,
    institutional, biographical, constrained, global).

% Bear the direct economic costs of DSB rulings, which may force them to reduce production, face increased competition, or lose government support. They have little direct recourse within the WTO system and rely on their national governments to defend their interests.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_industries_targeted_by_rulings, payer,
    organized, immediate, trapped, national).

% Benefit from the complexity and binding nature of WTO dispute settlement, as it creates demand for their specialized legal services in advising governments and industries on compliance and litigation strategy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, international_trade_lawyers, beneficiary,
    moderate, biographical, mobile, global).

% Are constrained by DSB rulings, which limit their policy space in areas covered by WTO agreements. They would prefer greater flexibility to implement policies tailored to domestic needs, but are bound by international treaty obligations.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_policy_makers, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rules-based mechanism for resolving trade disputes between member states, preventing unilateral protectionist actions and ensuring a stable, predictable global trading environment.
% TRANSFER_FUNCTION: Transfers policy discretion and sovereignty from individual member states to the multilateral WTO system, in exchange for market access and a stable trading order. It also transfers economic costs to non-compliant states and their industries.
% ABSENT_VOICES: Domestic policy-makers and civil society groups advocating for non-trade concerns (e.g., environmental protection, labor standards) often feel their policy space is constrained by WTO rulings, but have limited direct voice in the DSB process.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority vanished, member states would likely revert to unilateral trade measures, leading to increased protectionism, retaliatory cycles, and a breakdown of the rules-based trading system. Global trade flows would be significantly disrupted.
% FOUNDING_PROBLEM: The post-WWII international trading system needed a mechanism to prevent trade wars and ensure compliance with multilateral trade agreements, moving beyond the weaker GATT dispute resolution system.
% FOUNDING_PROBLEM_CORROBORATION: Most WTO member states, particularly those reliant on exports, corroborate that the problem of preventing trade wars and ensuring compliance remains live. International legal scholars and economists also attest to the ongoing need for a binding dispute resolution mechanism in global trade.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because member states must cede policy autonomy and potentially incur economic costs to comply with rulings, or face retaliation. Suppression (0.75) is high due to the threat of authorized retaliation, which makes non-compliance costly and difficult. Theater ratio (0.1) is low, as the DSB's function is largely effective and not performative; rulings generally lead to compliance or authorized countermeasures. Accessibility collapse (0.7) is high because once a ruling is issued, the policy options for the losing member state are severely constrained. Resistance (0.4) is moderate, as member states often challenge rulings but ultimately face strong pressure to comply.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of exporting member states, the DSB is a crucial mechanism for ensuring fair trade and market access. From the perspective of a member state facing an adverse ruling, or its affected domestic industries, the DSB's authority can feel highly extractive, forcing unwanted policy changes. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB itself and exporting member states are beneficiaries, gaining from a stable, rules-based system and market access. Importing member states and their domestic industries, when targeted by adverse rulings, are the primary payers, bearing the costs of compliance or retaliation. International trade lawyers also benefit from the system's complexity. Domestic policy-makers are excluded in the sense that their policy discretion is constrained by the DSB's authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_cession_ambiguity,
    'To what extent do member states genuinely perceive DSB rulings as a legitimate cession of sovereignty versus an external imposition?',
    'Analysis of national legislative debates and judicial interpretations of WTO law, particularly in cases of high-profile adverse rulings.',
    'If perceived as an imposition, the effective suppression and extractiveness are higher, indicating a more Snare-like dynamic. If perceived as legitimate cession, it supports the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_cession_ambiguity, conceptual, 'Ambiguity in member states'' perception of sovereignty cession to the WTO DSB.').

omega_variable(
    retaliation_effectiveness_ambiguity,
    'How effective is authorized retaliation as an enforcement mechanism, particularly against large economies or in cases where the complainant lacks economic leverage?',
    'Empirical studies on compliance rates and the economic impact of authorized retaliation across different types of disputes and member states.',
    'If retaliation is often ineffective, the actual suppression is lower than measured, and the constraint leans more towards a Rope (coordination without strong enforcement) or even Piton (inertial compliance) for some actors. If highly effective, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_effectiveness_ambiguity, empirical, 'Uncertainty regarding the true effectiveness of WTO authorized retaliation.').

omega_variable(
    reading_legitimacy_contest,
    'Is the ''binding referee'' reading of DSB authority the dominant and most widely accepted interpretation among WTO member states and legal scholars?',
    'Content analysis of official statements, legal scholarship, and voting patterns within the WTO General Council and DSB, comparing support for this reading against the ''advisory coordination'' and ''judicial activism'' readings.',
    'If this reading is widely contested or losing ground, the constraint''s stability and effective suppression are lower, indicating a more fragile or contested Tangled Rope, or even a shift towards a Piton if enforcement becomes purely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'The ''binding referee'' reading is one of several competing interpretations of DSB authority, and its dominance is subject to ongoing contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the WTO DSB's authority. Its siblings, 'advisory_coordination_reading' and 'judicial_activism_reading', represent alternative interpretations of the DSB's mandate and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
