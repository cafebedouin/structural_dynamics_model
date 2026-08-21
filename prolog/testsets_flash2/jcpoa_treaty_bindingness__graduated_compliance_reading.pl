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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA: Graduated Compliance Reading
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) through the lens of a 'graduated compliance' reading. In this
 *   interpretation, the JCPOA functions as a scaled reciprocal commitment,
 *   where enforcement actions (e.g., sanctions relief withdrawal) are
 *   calibrated proportionally to the severity of compliance deviations (e.g.,
 *   Iranian enrichment levels). The dispute resolution mechanism prioritizes
 *   de-escalation and pragmatic solutions over strict legalistic adherence,
 *   aiming to keep the diplomatic channel open. Beneficiaries are those who
 *   favor pragmatic diplomacy and limited economic engagement, while victims
 *   are those who prefer maximalist positions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA: Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'd96f5d9a-f762-4a75-a8c2-d8b8c7365cb3').
narrative_ontology:cs_kernel_codification('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', formalized).
narrative_ontology:cs_authority_grounding('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', lineage).
narrative_ontology:cs_interpretation_layer_present('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3').
narrative_ontology:cs_reading_relation('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', foundational, proportional_reciprocity_is_optimal_de_escalation).
narrative_ontology:cs_axiom_status(proportional_reciprocity_is_optimal_de_escalation, holdable).
narrative_ontology:cs_axiom_grounding('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', proportional_reciprocity_is_optimal_de_escalation, instrumental).
narrative_ontology:cs_axiom('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', foundational, diplomatic_channels_must_remain_open).
narrative_ontology:cs_axiom_status(diplomatic_channels_must_remain_open, holdable).
narrative_ontology:cs_axiom_grounding('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', diplomatic_channels_must_remain_open, deontological).
narrative_ontology:cs_reference_frame('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', adaptive_diplomacy_framework).
narrative_ontology:cs_drift_state('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d96f5d9a-f762-4a75-a8c2-d8b8c7365cb3', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_pragmatists).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, eu_economic_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, diplomatic_channels).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for continued engagement with the JCPOA to secure sanctions relief and economic integration. They benefit from the de-escalation mechanisms and the partial opening of trade, but are constrained by domestic hardliners and external pressure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_pragmatists, beneficiary,
    organized, biographical, constrained, national).

% Seek to re-establish trade and investment ties with Iran, benefiting from any sanctions relief. Their commitment is primarily economic, and they are mobile enough to shift focus if the deal becomes too unstable.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, eu_economic_actors, beneficiary,
    powerful, immediate, mobile, regional).

% Benefit from the existence of a framework for dialogue and de-escalation, even if imperfect. The constraint provides a structured mechanism for managing nuclear proliferation risks through negotiation rather than confrontation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, diplomatic_channels, beneficiary,
    institutional, generational, analytical, global).

% Oppose the JCPOA as an infringement on national sovereignty and a concession to Western powers. They bear the political cost of perceived compromise and seek to undermine the agreement, but are identity-locked into a nationalist narrative.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners, payer,
    organized, generational, identity_locked, national).

% Advocate for maximum pressure on Iran through comprehensive sanctions. They view any partial sanctions relief as a cost and a weakening of their policy goals, but are constrained by international diplomatic efforts.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_advocates, payer,
    institutional, biographical, constrained, national).

% Verify Iran's compliance with its nuclear commitments. Their technical assessments provide the empirical basis for graduated enforcement actions, operating independently of political pressures.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspectors, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for reciprocal commitments between Iran and P5+1 nations to prevent nuclear proliferation, allowing for calibrated responses to compliance deviations.
% TRANSFER_FUNCTION: Exchanges Iranian nuclear program limitations for sanctions relief, with both sides adjusting their commitments proportionally to the other's actions.
% ABSENT_VOICES: Hardline factions in both Iran and the US, who advocate for maximalist positions (full nuclear program or full sanctions) and reject the premise of graduated, reciprocal engagement, are marginalized by this diplomatic approach.
% DISAPPEARANCE_RATIONALE: If the graduated compliance framework vanished, the immediate consequence would be a rapid escalation of Iran's nuclear program and a return to full, unmitigated sanctions, leading to increased regional instability and a higher risk of military confrontation.
% FOUNDING_PROBLEM: The problem of Iran's accelerating nuclear program and the international community's desire to prevent nuclear proliferation through diplomatic means, avoiding military conflict.
% FOUNDING_PROBLEM_CORROBORATION: International diplomatic bodies, intelligence agencies, and non-proliferation experts outside the direct negotiating parties consistently corroborate that the threat of nuclear proliferation remains live, and that a diplomatic framework is essential for managing it.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the moderate costs imposed on Iran (nuclear limitations) and the P5+1 (sanctions relief), balanced by the benefits of non-proliferation. Suppression (0.55) is moderate, as both sides retain some agency to escalate or de-escalate, but are constrained by the reciprocal nature of the agreement. Theater ratio (0.20) is low, indicating that the core function of managing proliferation is largely genuine, though political posturing exists. The metrics reflect a dynamic, adaptive constraint rather than a rigid, highly extractive one.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic diplomacy advocates, the JCPOA is a functional Rope, providing essential coordination. From the perspective of hardliners on both sides, it is a Snare or Tangled Rope, imposing unacceptable costs or compromises. The engine's classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Iranian pragmatists and EU economic actors are beneficiaries, as they gain from the de-escalation and economic opportunities. Diplomatic channels themselves benefit from having a framework. Iranian hardliners and US sanctions advocates are payers, as they bear the costs of compromise or reduced leverage. IAEA inspectors act as observers, providing objective data for the graduated response.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_assessment_ambiguity,
    'How is ''proportionality'' of compliance deviations and enforcement actions objectively assessed, given differing national interests?',
    'Establishment of an independent, technically-expert arbitration body with binding authority on proportionality assessments, or a clear, pre-agreed matrix of violations-to-responses.',
    'If proportionality is consistently contested, the constraint drifts towards a Tangled Rope or Snare, as enforcement becomes arbitrary. If resolved, it strengthens the Rope classification by ensuring fairness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_ambiguity, conceptual, 'Ambiguity in defining and assessing proportional responses to compliance deviations.').

omega_variable(
    de_escalation_vs_punishment_priority,
    'Does the dispute resolution mechanism genuinely prioritize de-escalation, or is it primarily a mechanism for legitimizing punitive measures?',
    'Analysis of historical dispute resolution outcomes: frequency of de-escalatory compromises versus imposition of new sanctions or withdrawal of relief.',
    'If punitive measures consistently dominate, the constraint''s extractiveness and suppression are higher than currently assessed, potentially reclassifying it as a Tangled Rope or Snare. If de-escalation is primary, the Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_escalation_vs_punishment_priority, empirical, 'Whether the core function of dispute resolution is de-escalation or punishment.').

omega_variable(
    reading_naturalness_vs_construction,
    'Is this ''graduated compliance'' reading an inherent feature of the JCPOA text and its negotiating history, or a constructed interpretation driven by specific diplomatic preferences?',
    'Comprehensive textual analysis of the JCPOA and its annexes, combined with review of negotiating records and statements from all original parties.',
    'If it''s a constructed preference, its stability is lower, and it''s more vulnerable to shifts in political will. If inherent, it''s more robust to external pressures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_naturalness_vs_construction, conceptual, 'The degree to which the ''graduated compliance'' reading is textually inherent versus diplomatically constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
