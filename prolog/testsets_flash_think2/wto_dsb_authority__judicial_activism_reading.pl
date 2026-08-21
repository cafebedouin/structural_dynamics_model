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
 *   human_readable: WTO DSB Judicial Activism (Illegitimate Legislation Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'judicial activism' reading of the
 *   WTO Dispute Settlement Body's (DSB) authority. From this perspective, DSB
 *   panels have exceeded their treaty mandate by creating new obligations
 *   through interpretive drift, effectively engaging in illegitimate judicial
 *   legislation. This has led to active resistance from member states,
 *   contestation of the treaty interpretation itself, and a perception that
 *   retaliation authorizations are illegitimate. The constraint is classified
 *   as a Snare because the coordination story (neutral dispute resolution) is
 *   seen as a cover for the extraction of policy autonomy from member states,
 *   enforced through the threat of authorized retaliation.
 *
 * KEY AGENTS:
 *   - DSB Panels: Agenda-setter, institutional power, benefits from expanded authority.
 *   - WTO Secretariat: Beneficiary, institutional, supports DSB's expanded role.
 *   - Member States (Challenged by Rulings): Payer, organized, bear costs of new obligations, actively resist.
 *   - Member States (Advocating Limited Mandate): Payer, organized, bear systemic costs of eroded treaty fidelity.
 *   - Member States (Benefiting from Specific Rulings): Beneficiary, organized, may tacitly support expansive interpretation.
 *   - Academic Critics/Legal Scholars: Observer, analytical, highlight judicial overreach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.85).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.78).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism (Illegitimate Legislation Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '69f68549-a892-4329-9b97-ded028063cdd').
narrative_ontology:cs_kernel_codification('69f68549-a892-4329-9b97-ded028063cdd', fixed_text).
narrative_ontology:cs_authority_grounding('69f68549-a892-4329-9b97-ded028063cdd', extraction).
narrative_ontology:cs_interpretation_layer_present('69f68549-a892-4329-9b97-ded028063cdd').
narrative_ontology:cs_reading_relation('69f68549-a892-4329-9b97-ded028063cdd', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('69f68549-a892-4329-9b97-ded028063cdd', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_axiom('69f68549-a892-4329-9b97-ded028063cdd', foundational, dsb_exceeds_mandate_by_interpretation).
narrative_ontology:cs_axiom_status(dsb_exceeds_mandate_by_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('69f68549-a892-4329-9b97-ded028063cdd', dsb_exceeds_mandate_by_interpretation, conventional).
narrative_ontology:cs_axiom('69f68549-a892-4329-9b97-ded028063cdd', foundational, national_sovereignty_over_judicial_interpretation).
narrative_ontology:cs_axiom_status(national_sovereignty_over_judicial_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('69f68549-a892-4329-9b97-ded028063cdd', national_sovereignty_over_judicial_interpretation, deontological).
narrative_ontology:cs_reference_frame('69f68549-a892-4329-9b97-ded028063cdd', original_limited_dsb_mandate).
narrative_ontology:cs_drift_state('69f68549-a892-4329-9b97-ded028063cdd', contemporary_wto_disputes, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('69f68549-a892-4329-9b97-ded028063cdd', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_panels).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, member_states_benefiting_from_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_challenged_by_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_advocating_limited_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The adjudicative bodies that interpret WTO agreements and issue rulings. From this reading's perspective, they actively expand their mandate through interpretive drift, effectively creating new obligations and asserting legislative authority beyond the original treaty text. They benefit from increased institutional power and influence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_panels, agenda_setter,
    institutional, generational, constrained, global).

% Provides administrative and legal support to the DSB panels. Benefits from the expansion of the DSB's authority, as it enhances the WTO's overall institutional relevance and the secretariat's role within it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat, beneficiary,
    institutional, generational, constrained, global).

% These states are the targets of DSB rulings that, from this reading's perspective, impose new obligations or curtail policy space not explicitly agreed upon in the original treaties. They bear the costs of compliance or face authorized retaliation, and actively resist what they view as judicial overreach.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_challenged_by_rulings, payer,
    organized, biographical, constrained, global).

% A group of member states that consistently argue for a strict interpretation of the WTO agreements, emphasizing national sovereignty and the limited, intergovernmental nature of the DSB's mandate. They view the panels' actions as illegitimate judicial legislation and bear the systemic cost of eroded treaty fidelity.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_advocating_limited_mandate, payer,
    organized, generational, constrained, global).

% Certain member states may benefit from specific DSB rulings that align with their trade interests, even if those rulings are seen by others as exceeding the DSB's mandate. They may tacitly or explicitly support the DSB's expansive interpretation when it serves their immediate goals.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_benefiting_from_rulings, beneficiary,
    organized, biographical, mobile, global).

% Legal scholars and academics who analyze WTO jurisprudence and highlight instances where DSB panels have, in their view, engaged in judicial activism, creating new law rather than merely interpreting existing agreements. They provide critical analysis and contribute to the debate on the DSB's legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, academic_critics_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, to provide a neutral, rules-based mechanism for resolving trade disputes among member states, preventing unilateral retaliation and ensuring a stable multilateral trading system.
% TRANSFER_FUNCTION: Transfers policy autonomy and sovereign decision-making power from member states to the interpretive authority of the DSB panels, imposing new obligations and compliance costs on states found in violation of these expanded interpretations.
% ABSENT_VOICES: Member states that have withdrawn from the WTO's dispute settlement system or those who advocate for a return to purely diplomatic, negotiated dispute resolution would object. Their absence from the DSB's process means their views on its overreach are not directly heard within the system.
% DISAPPEARANCE_RATIONALE: If the DSB's expanded authority and its enforcement mechanisms vanished overnight, the international trade legal landscape would be fundamentally altered. Member states would likely revert to more bilateral dispute resolution, or seek to establish a new multilateral mechanism with a more explicitly defined and limited adjudicative mandate, leading to a significant reorganization of global trade governance.
% FOUNDING_PROBLEM: The founding problem was to prevent unilateral trade retaliation and ensure a rules-based, predictable multilateral trading system by providing a neutral forum for dispute resolution based on agreed-upon treaty texts.
% FOUNDING_PROBLEM_CORROBORATION: While proponents argue the DSB's role is still vital for maintaining the rules-based order, academic critics and several member states (especially those frequently targeted by rulings or advocating for a limited mandate) corroborate that the founding problem of preventing unilateralism has been overshadowed by the problem of judicial overreach, undermining the system's legitimacy. Legislative hearing testimony and independent legal analyses from outside the benefiting parties support this shifted-function reading.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the DSB's interpretive drift imposes new, unconsented obligations on member states, effectively extracting policy space and sovereignty. Suppression is also high (0.78) as compliance is enforced through authorized retaliation, and exit from the system is highly constrained. The theater ratio is moderate (0.45) because while the panels maintain the appearance of 'interpreting' existing agreements, a significant portion of their activity is perceived as legislative. Resistance is high (0.75) due to active pushback from member states, including non-compliance and calls for reform or withdrawal. The temporal measurements show a consistent increase in extractiveness, suppression, and theater over the DSB's operational history, reflecting the growing perception of judicial overreach.
 *
 * PERSPECTIVAL GAP:
 *   The DSB panels and WTO Secretariat would likely perceive their actions as legitimate interpretation necessary for the functioning of the multilateral trading system (a 'binding referee' or even 'rope' perspective). In contrast, the 'payer' member states and academic critics experience the same structure as an extractive snare, where their policy autonomy is curtailed by illegitimate judicial legislation. The engine's computation of per-seat classifications from the structural data will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The DSB Panels and WTO Secretariat are beneficiaries (low d) as they gain institutional power and relevance from the expanded authority. Member states challenged by rulings and those advocating a limited mandate are clear targets (high d) as they bear the direct and systemic costs of the DSB's overreach. Member states benefiting from specific rulings are beneficiaries (low d) in those instances, but still subject to the broader precedent. Academic critics are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the DSB's current operation as a legitimate 'binding referee' (a Rope or Tangled Rope) or a purely 'advisory coordination' mechanism (a Rope). By highlighting the high extractiveness, suppression, and resistance, it flags the constraint as one where the coordination story is cover for an extractive function, consistent with the 'illegitimate judicial legislation' reading. The contested status of the founding problem further supports this, indicating a potential drift from its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsb_mandate_interpretation_ambiguity,
    'Is the DSB''s interpretive practice a legitimate evolution of treaty law (binding_referee_reading) or an illegitimate overreach (judicial_activism_reading)?',
    'A global consensus among WTO member states on a revised mandate for the DSB, or a definitive ruling by an international court on the scope of treaty interpretation in international trade law.',
    'If resolved as legitimate evolution, the constraint would shift towards a Tangled Rope or Rope. If resolved as illegitimate overreach, the Snare classification would be strongly reinforced, potentially leading to calls for systemic reform or withdrawal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dsb_mandate_interpretation_ambiguity, conceptual, 'Ambiguity regarding the legitimate scope of DSB''s interpretive authority.').

omega_variable(
    impact_of_authorized_retaliation,
    'To what extent does the threat of authorized retaliation genuinely compel compliance with DSB rulings, versus merely escalating trade tensions?',
    'Empirical studies analyzing the correlation between authorized retaliation and subsequent compliance, disaggregated by the perceived legitimacy of the underlying ruling.',
    'If retaliation is found to be ineffective at compelling compliance with perceived illegitimate rulings, the ''suppression'' metric might be re-evaluated downwards, indicating a less effective coercive mechanism, though the ''resistance'' would remain high. If it is effective, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_authorized_retaliation, empirical, 'Effectiveness of authorized retaliation as an enforcement mechanism for DSB rulings.').

omega_variable(
    judicial_activism_vs_systemic_necessity,
    'Is the DSB''s expansive interpretation a form of judicial activism, or a necessary adaptation to fill gaps in an evolving and increasingly complex multilateral trading system?',
    'Analysis of the specific legal gaps addressed by DSB rulings and whether member states have failed to address these gaps through legislative means, alongside a review of the original negotiating history and intent.',
    'If deemed a necessary adaptation, the ''extractiveness'' might be re-evaluated downwards, as some ''extraction'' could be re-framed as essential coordination cost. If confirmed as activism, the Snare classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_systemic_necessity, conceptual, 'Distinguishing judicial activism from necessary systemic adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, multilateral_trade_negotiations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'wto_dsb_authority' kernel. This 'judicial activism' reading focuses on the perceived overreach and illegitimate legislative function of the DSB, contrasting with the 'binding referee' and 'advisory coordination' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
