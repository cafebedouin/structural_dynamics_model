% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Panels Exceeding Mandate (Judicial Activism Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint is the 'judicial activism' reading of the WTO Dispute
 *   Settlement Body's authority. It views DSB panels as exceeding their
 *   mandate by creating new obligations through interpretive drift, leading
 *   to illegitimate judicial legislation. Sibling readings include 'binding
 *   referee' (panels issue binding rulings within mandate) and 'advisory
 *   coordination' (panels provide expert advisory opinions). The claimed type
 *   is Tangled Rope, reflecting a coordination function (dispute settlement)
 *   that has become highly extractive due to perceived overreach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.8).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Panels Exceeding Mandate (Judicial Activism Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7').
narrative_ontology:cs_kernel_codification('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', formalized).
narrative_ontology:cs_authority_grounding('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', extraction).
narrative_ontology:cs_interpretation_layer_present('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7').
narrative_ontology:cs_reading_relation('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_axiom('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', foundational, judicial_restraint_principle).
narrative_ontology:cs_axiom_status(judicial_restraint_principle, holdable).
narrative_ontology:cs_axiom_grounding('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', judicial_restraint_principle, deontological).
narrative_ontology:cs_axiom('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', foundational, member_state_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(member_state_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', member_state_sovereignty_supremacy, conventional).
narrative_ontology:cs_reference_frame('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', original_treaty_mandate).
narrative_ontology:cs_drift_state('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', contemporary_era_of_judicial_activism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd4ccb8c-609d-4623-ad9d-2c9ed19ab7a7', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_dsb_panels).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, member_states_benefiting_from_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, sovereignty_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The adjudicative bodies within the WTO Dispute Settlement Body. From this reading's perspective, they are exceeding their original mandate by interpreting treaties in ways that create new obligations for member states, effectively engaging in judicial legislation. They benefit from expanded institutional power and scope.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_dsb_panels, agenda_setter,
    institutional, generational, constrained, global).

% Member states that are subject to DSB rulings which, from this reading's perspective, impose new obligations not explicitly agreed upon in the original treaties. They bear the costs of compliance, face authorized retaliation if they resist, and experience an erosion of national policy discretion.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings, payer,
    organized, biographical, constrained, national).

% The administrative body supporting the WTO, including the DSB. It benefits from the perceived strength and expanded role of the DSB, which enhances the institution's overall influence and stability, even if this comes at the cost of member state legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat, beneficiary,
    institutional, generational, constrained, global).

% Member states that benefit from specific DSB rulings, particularly those that align with their trade interests or expand their market access. While they may also be subject to DSB overreach in other cases, they currently gain from the expanded interpretive authority.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_benefiting_from_rulings, beneficiary,
    organized, biographical, mobile, national).

% Groups and political factions within member states who strongly oppose the erosion of national sovereignty to international bodies. They perceive the DSB's interpretive drift as a direct threat to democratic self-governance and bear the diffuse costs of perceived loss of control.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, sovereignty_advocates, payer,
    moderate, generational, identity_locked, national).

% Academics and legal experts who analyze the evolution of WTO jurisprudence. They observe and critique the DSB's interpretive practices, often highlighting instances of perceived judicial activism or overreach, but do not directly participate in the dispute settlement process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a rules-based, neutral mechanism for resolving trade disputes between member states, ensuring predictable application of WTO agreements and preventing unilateral trade actions.
% TRANSFER_FUNCTION: Transfers policy discretion and, from this reading's perspective, legislative authority from sovereign member states to the WTO DSB panels, imposing new obligations and potentially shifting economic advantages between states.
% ABSENT_VOICES: Member states and political factions who advocate for a strictly intergovernmental, less supranational WTO, and those who prioritize national legislative authority over international judicial interpretation. Their concerns about judicial overreach are often marginalized in the DSB's operational discourse.
% DISAPPEARANCE_RATIONALE: If the DSB's authority, even in its current form, vanished overnight, the multilateral trading system would lose its primary enforcement mechanism. This would likely lead to a resurgence of unilateral protectionist measures, increased trade disputes, and a significant reorganization of global trade governance towards bilateral or regional agreements.
% FOUNDING_PROBLEM: To prevent a return to unilateral trade retaliation and ensure a stable, rules-based multilateral trading system by providing a neutral, binding forum for dispute resolution based on agreed-upon treaties.
% FOUNDING_PROBLEM_CORROBORATION: The WTO DSB and its institutional supporters argue the founding problem of preventing trade wars and ensuring a rules-based system remains live. However, member states targeted by rulings and sovereignty advocates attest that the DSB's interpretive overreach has undermined its legitimacy, shifting the problem from dispute resolution to institutional overreach, as evidenced by increasing non-compliance and appeals to national sovereignty.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.8) because the DSB panels, from this reading's perspective, impose obligations beyond the explicit treaty text, effectively extracting new commitments from member states. Suppression is also high (0.75) as member states face authorized retaliation for non-compliance, limiting their exit options. Theater ratio is moderate (0.4) because while the panels still perform the legitimate function of dispute settlement, a significant portion of their activity is seen as performative interpretation that masks legislative overreach. Resistance is high (0.7) due to active contestation from member states and sovereignty advocates.
 *
 * PERSPECTIVAL GAP:
 *   The DSB panels and their institutional supporters would likely view their actions as legitimate interpretation necessary for the evolving trade landscape (a 'binding referee' reading), while targeted member states and sovereignty advocates perceive it as an illegitimate power grab (this 'judicial activism' reading). The engine computes this divergence from the structural data, not from the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB panels and Secretariat are beneficiaries, gaining institutional power and scope. Member states targeted by rulings and sovereignty advocates are victims, bearing the costs of new obligations and eroded sovereignty. Member states benefiting from specific rulings are also beneficiaries, as they gain from the expanded interpretive authority in certain cases. Trade law scholars are observers, analyzing the dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_treaty_interpretation,
    'What are the legitimate bounds of treaty interpretation before it constitutes ''judicial legislation'' in international law?',
    'Development of clearer international legal principles on judicial restraint in treaty bodies, or a consensus among member states on an interpretive protocol.',
    'If a narrow interpretation of judicial authority is adopted, the DSB''s actions would be more clearly classified as overreach, increasing its extractiveness. If a broad interpretation is accepted, the DSB''s actions might be reclassified as legitimate, reducing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_treaty_interpretation, conceptual, 'Ambiguity regarding the line between legitimate treaty interpretation and judicial overreach.').

omega_variable(
    impact_of_member_state_resistance,
    'What is the actual long-term impact of member states'' active resistance and non-compliance on the DSB''s effective authority and the stability of the multilateral trading system?',
    'Empirical study of compliance rates, retaliatory measures, and the evolution of trade disputes in the face of sustained member state resistance over a multi-year period.',
    'If resistance effectively curtails DSB overreach without collapsing the system, the constraint''s effective suppression might decrease. If resistance leads to systemic breakdown, the costs of the constraint (and its alternatives) would be higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_of_member_state_resistance, empirical, 'Uncertainty about the efficacy and systemic consequences of member state resistance to DSB rulings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wto__tr_t6, wto_dsb_authority__judicial_activism_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(wto__tr_t12, wto_dsb_authority__judicial_activism_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(wto__tr_t18, wto_dsb_authority__judicial_activism_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(wto__tr_t24, wto_dsb_authority__judicial_activism_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(wto__be_t6, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(wto__be_t12, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(wto__be_t18, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(wto__be_t24, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(wto__su_t6, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(wto__su_t12, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(wto__su_t18, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(wto__su_t24, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, national_trade_policy_autonomy).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, multilateral_trade_negotiation_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the WTO DSB's authority, each representing a distinct structural claim about its function and legitimacy. This 'judicial activism' reading focuses on perceived overreach and illegitimate legislation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
