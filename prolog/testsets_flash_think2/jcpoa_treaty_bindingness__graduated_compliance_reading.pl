% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA as Graduated Reciprocal Commitment
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) as a 'graduated reciprocal commitment' — a reading that
 *   emphasizes proportional compliance assessment and enforcement calibrated
 *   to violation severity. It views the agreement as a dynamic framework for
 *   managing nuclear proliferation through de-escalation and pragmatic
 *   diplomacy, rather than a rigid, legally binding treaty or a purely
 *   transactional, easily voidable deal. The metrics reflect the fluctuating
 *   nature of compliance and enforcement over the period 2015-2025, including
 *   periods of increased tension and partial re-engagement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA as Graduated Reciprocal Commitment").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '45b3c124-70d3-4013-9360-6da646a1971a').
narrative_ontology:cs_kernel_codification('45b3c124-70d3-4013-9360-6da646a1971a', formalized).
narrative_ontology:cs_authority_grounding('45b3c124-70d3-4013-9360-6da646a1971a', lineage).
narrative_ontology:cs_interpretation_layer_present('45b3c124-70d3-4013-9360-6da646a1971a').
narrative_ontology:cs_reading_relation('45b3c124-70d3-4013-9360-6da646a1971a', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('45b3c124-70d3-4013-9360-6da646a1971a', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('45b3c124-70d3-4013-9360-6da646a1971a', foundational, proportional_response_principle).
narrative_ontology:cs_axiom_status(proportional_response_principle, holdable).
narrative_ontology:cs_axiom_grounding('45b3c124-70d3-4013-9360-6da646a1971a', proportional_response_principle, conventional).
narrative_ontology:cs_axiom('45b3c124-70d3-4013-9360-6da646a1971a', foundational, de_escalation_priority).
narrative_ontology:cs_axiom_status(de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('45b3c124-70d3-4013-9360-6da646a1971a', de_escalation_priority, conventional).
narrative_ontology:cs_reference_frame('45b3c124-70d3-4013-9360-6da646a1971a', managed_de_escalation_framework).
narrative_ontology:cs_drift_state('45b3c124-70d3-4013-9360-6da646a1971a', contemporary_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('45b3c124-70d3-4013-9360-6da646a1971a', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_nations).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, global_non_proliferation_regime).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_in_us_and_iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commits to significant limitations on its nuclear program (enrichment levels, centrifuges, stockpiles) in exchange for sanctions relief. Bears the cost of these limitations but benefits from economic reintegration and reduced international isolation. Its exit options are constrained by the threat of renewed sanctions and potential military action.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iran, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iran, beneficiary).

% Negotiated and enforces the agreement, providing sanctions relief in exchange for nuclear limitations. Benefits from reduced proliferation risk and diplomatic engagement. Their exit options include reimposing sanctions or pursuing other diplomatic/military avenues.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_nations, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_nations, beneficiary).

% Verifies Iran's compliance with its nuclear commitments through extensive inspections and monitoring. Provides objective assessments of the constraint's operation, crucial for the graduated enforcement mechanism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea, observer,
    institutional, biographical, analytical, global).

% Benefit from the perceived success of diplomatic engagement and the de-escalation of nuclear tensions. Their influence rises when the agreement is seen as functional and effective.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the lifting of sanctions, allowing for renewed trade and investment opportunities with Iran. Their interests are directly tied to the stability and implementation of the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement, beneficiary,
    organized, immediate, mobile, global).

% Bear the cost of compromise, as they fundamentally oppose the agreement from their respective maximalist positions (either too lenient on Iran or an infringement on national sovereignty). They actively seek to undermine or withdraw from the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_factions_in_us_and_iran, payer,
    organized, generational, constrained, national).

% Benefits from the precedent of a diplomatic solution to a proliferation crisis and the strengthening of international non-proliferation norms. Its stability is enhanced by successful, verifiable agreements.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, global_non_proliferation_regime, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__graduated_compliance_reading, global_non_proliferation_regime).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To manage Iran's nuclear program, preventing proliferation through verifiable limitations, while providing reciprocal sanctions relief to Iran, thereby de-escalating regional tensions and avoiding military conflict.
% TRANSFER_FUNCTION: Transfers limitations on Iran's nuclear enrichment capacity and activities to the international community, in exchange for the transfer of economic sanctions relief from the P5+1 nations to Iran.
% ABSENT_VOICES: Hardline factions in both the US and Iran, as well as regional rivals of Iran (e.g., Israel, Saudi Arabia), are structurally excluded from the direct negotiation and implementation of this reading. They would advocate for more aggressive enforcement, full sanctions, or complete nuclear disarmament by Iran, viewing the current compromise as insufficient or dangerous.
% DISAPPEARANCE_RATIONALE: If the JCPOA vanished overnight, Iran would likely accelerate its nuclear program, leading to a severe international crisis, potential military intervention, and a collapse of diplomatic efforts. The global non-proliferation architecture would be significantly weakened, and regional security dynamics would fundamentally shift.
% FOUNDING_PROBLEM: Iran's accelerating nuclear program and the international community's desire to prevent nuclear proliferation without resorting to military conflict, coupled with Iran's desire for sanctions relief and recognition of its right to peaceful nuclear energy.
% FOUNDING_PROBLEM_CORROBORATION: IAEA reports consistently document Iran's nuclear activities and the ongoing proliferation risk. UN Security Council resolutions and statements from numerous international bodies and non-proliferation experts corroborate the persistent nature of the founding problem, even with the JCPOA in place.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'tangled_rope' classification reflects the dual nature of the JCPOA under this reading: it genuinely coordinates non-proliferation efforts (beneficiaries: P5+1, global non-proliferation regime) but also involves significant extraction from Iran (limits on nuclear program) and active enforcement (sanctions, monitoring). The extractiveness and suppression metrics fluctuate, reflecting periods of US withdrawal and Iranian counter-measures, followed by attempts at re-engagement. The theater ratio remains low, as the mechanism is intended to be functional and verifiable, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic diplomacy advocates, the JCPOA is a vital, if imperfect, coordination mechanism. From the perspective of hardline factions, it is a deeply flawed or illegitimate arrangement that extracts unacceptable concessions or fails to adequately address threats. This reading attempts to capture the operational reality of the 'graduated' approach, where the constraint's effectiveness is constantly being tested and recalibrated.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is both a payer (nuclear limitations) and a beneficiary (sanctions relief), leading to a complex directionality. The P5+1 nations are agenda-setters and beneficiaries (non-proliferation). Pragmatic diplomacy advocates and economic actors are clear beneficiaries. Hardline factions in both the US and Iran are victims, as the agreement represents a compromise they oppose. The IAEA acts as an analytical observer, providing crucial data for the graduated enforcement mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportional_compliance_ambiguity,
    'How is ''proportional compliance assessment'' objectively defined and applied, given the political nature of violations and enforcement actions?',
    'Analysis of Joint Commission decisions and IAEA reports over time, comparing stated proportionality principles with actual enforcement outcomes. Examination of the criteria used by P5+1 nations to determine ''proportional'' sanctions relief withdrawal.',
    'If proportionality is consistently applied, it strengthens the ''tangled_rope'' classification by validating the coordination function. If it''s found to be arbitrary or politically motivated, it shifts the constraint closer to a ''snare'' by revealing the extraction to be less justified by reciprocal action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportional_compliance_ambiguity, empirical, 'Ambiguity in the objective application of proportional compliance.').

omega_variable(
    de_escalation_priority_vs_legal_closure,
    'To what extent do dispute resolution mechanisms genuinely prioritize de-escalation and diplomatic solutions over formal legal closure or punitive measures?',
    'Case studies of past dispute resolution processes within the JCPOA framework, analyzing the outcomes and the stated rationales for decisions. Interviews with diplomats and legal experts involved in the process.',
    'If de-escalation is consistently prioritized, it reinforces the ''tangled_rope'' classification by highlighting the constraint''s coordination function. If formal legal closure or punitive measures dominate, it suggests a stronger extractive component, potentially pushing it towards a ''snare'' or a more rigid ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_escalation_priority_vs_legal_closure, empirical, 'The true priority of de-escalation in dispute resolution.').

omega_variable(
    reciprocal_commitment_naturalness,
    'Is the ''reciprocal commitment'' framing a genuine structural feature of the JCPOA, or a diplomatic cover for an inherently asymmetric power dynamic?',
    'Comparative analysis with other non-proliferation agreements and power-asymmetric international relations. Examination of the relative costs and benefits borne by Iran versus the P5+1 nations, and the actual leverage each party holds.',
    'If the reciprocity is found to be largely rhetorical, the constraint''s effective extraction from Iran would be higher, pushing it closer to a ''snare''. If genuine, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocal_commitment_naturalness, conceptual, 'The authenticity of the reciprocal commitment framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.12).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_program_limitations).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jcpoa_treaty_bindingness' kernel. This 'graduated_compliance_reading' emphasizes proportional, reciprocal enforcement, distinct from a 'binding_multilateral_reading' (rigid treaty) or a 'transactional_provisional_reading' (unilaterally voidable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
