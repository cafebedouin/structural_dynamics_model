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
 *   This constraint models the Joint Comprehensive Plan of Action (JCPOA)
 *   through the lens of a 'graduated compliance' reading. In this
 *   interpretation, the agreement functions as a scaled reciprocal commitment
 *   where enforcement actions (e.g., sanctions re-imposition) are
 *   proportional to the severity of Iranian non-compliance (e.g., enrichment
 *   levels). The primary goal is de-escalation and maintaining a diplomatic
 *   channel, even in the face of violations, rather than strict legal
 *   adherence or maximalist pressure. Beneficiaries include advocates for
 *   pragmatic diplomacy and economic actors seeking partial engagement with
 *   Iran, while the Iranian nuclear program and economy bear the costs of
 *   limitations and potential sanctions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.6).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA: Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '738d10da-9a6b-4b79-9889-b2d8ac7bf378').
narrative_ontology:cs_kernel_codification('738d10da-9a6b-4b79-9889-b2d8ac7bf378', formalized).
narrative_ontology:cs_authority_grounding('738d10da-9a6b-4b79-9889-b2d8ac7bf378', lineage).
narrative_ontology:cs_interpretation_layer_present('738d10da-9a6b-4b79-9889-b2d8ac7bf378').
narrative_ontology:cs_reading_relation('738d10da-9a6b-4b79-9889-b2d8ac7bf378', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('738d10da-9a6b-4b79-9889-b2d8ac7bf378', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('738d10da-9a6b-4b79-9889-b2d8ac7bf378', foundational, proportionality_in_response).
narrative_ontology:cs_axiom_status(proportionality_in_response, holdable).
narrative_ontology:cs_axiom_grounding('738d10da-9a6b-4b79-9889-b2d8ac7bf378', proportionality_in_response, conventional).
narrative_ontology:cs_axiom('738d10da-9a6b-4b79-9889-b2d8ac7bf378', foundational, de_escalation_priority).
narrative_ontology:cs_axiom_status(de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('738d10da-9a6b-4b79-9889-b2d8ac7bf378', de_escalation_priority, instrumental).
narrative_ontology:cs_reference_frame('738d10da-9a6b-4b79-9889-b2d8ac7bf378', managed_proliferation_risk_framework).
narrative_ontology:cs_drift_state('738d10da-9a6b-4b79-9889-b2d8ac7bf378', post_us_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('738d10da-9a6b-4b79-9889-b2d8ac7bf378', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_economy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors prioritize de-escalation and diplomatic solutions, viewing the JCPOA's graduated approach as a successful model for managing complex international disputes. They benefit from the stability and engagement fostered by this reading.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Businesses and investors who seek to re-enter or expand operations in Iran benefit from the partial sanctions relief and the predictable, albeit fragile, framework for engagement that this reading provides. They are sensitive to any escalation that might disrupt trade.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement, beneficiary,
    powerful, immediate, arbitrage, global).

% The program bears the costs of limitations on enrichment levels, centrifuge deployment, and intrusive inspections. While not a human agent, it represents the institutional and technical capacity that is constrained by the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program, payer,
    institutional, generational, constrained, national).

% The broader Iranian economy experiences the impact of sanctions, even with partial relief. It is constrained by the threat of re-imposed sanctions if compliance falters, and by the limited scope of economic engagement permitted under this reading.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_economy, payer,
    institutional, biographical, identity_locked, national).

% The signatory states (China, France, Germany, Russia, United Kingdom, United States) collectively administer the agreement, assess compliance, and determine proportional responses to violations. Their actions define the graduated enforcement mechanism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_states, agenda_setter,
    institutional, generational, constrained, global).

% The International Atomic Energy Agency provides the technical assessment of Iran's nuclear activities, verifying compliance with the agreement's provisions. Its reports are crucial for the graduated enforcement mechanism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea, agenda_setter,
    institutional, civilizational, analytical, universal).

% These factions view the JCPOA as an infringement on national sovereignty and a capitulation to Western demands. They would advocate for full nuclear development and rejection of external oversight, but their influence is contained by the current diplomatic framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_iranian_factions, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the reciprocal reduction of nuclear proliferation risk (Iran's enrichment capacity) with sanctions relief, through a mechanism that allows for proportional responses to deviations, aiming to prevent full collapse of the agreement.
% TRANSFER_FUNCTION: Transfers sanctions relief (economic benefits) to Iran in exchange for verifiable limitations on its nuclear program (security benefits for other states). The transfer is scaled and reciprocal.
% ABSENT_VOICES: Hardline factions in Iran and hawkish elements in the US and Israel are largely excluded from shaping the graduated compliance framework; they would advocate for maximalist positions (full nuclear program or full sanctions/regime change) that this reading seeks to avoid.
% DISAPPEARANCE_RATIONALE: If the JCPOA's graduated compliance framework vanished, Iran would likely accelerate its nuclear program, leading to a rapid re-imposition of maximal sanctions, increased regional instability, and a heightened risk of military confrontation. The international diplomatic landscape would be fundamentally altered.
% FOUNDING_PROBLEM: Iran's accelerating nuclear program posed a significant proliferation risk, leading to international sanctions and a diplomatic impasse, with a high risk of military conflict.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA continues to report on Iran's nuclear activities, and international diplomatic efforts consistently highlight the ongoing proliferation risk. While the immediate crisis of 2015 was averted, the underlying problem of nuclear proliferation in the region remains, corroborated by intelligence agencies and non-proliferation experts.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, reflecting the reciprocal nature where Iran gains sanctions relief but loses nuclear capabilities. Suppression (0.6) is significant, as the threat of re-imposed sanctions is a key enforcement mechanism. Theater ratio (0.2) is low, as the compliance assessment and graduated responses are generally genuine, though political posturing can introduce some performativity. The cyclical nature of measurements reflects periods of compliance and non-compliance, with corresponding adjustments in enforcement and sanctions relief.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the P5+1 states, this reading emphasizes a flexible, de-escalatory mechanism to manage proliferation risk. From Iran's perspective, it is a constraint on its sovereign right to nuclear development, albeit one that offers economic benefits. The 'iranian_nuclear_program' and 'iranian_economy' seats experience this as a tangible cost, while 'pragmatic_diplomacy_advocates' and 'economic_actors_seeking_engagement' perceive it as a beneficial framework for stability and opportunity.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'p5_plus_1_states' and 'iaea' act as agenda-setters, defining and enforcing the graduated compliance. 'pragmatic_diplomacy_advocates' and 'economic_actors_seeking_engagement' are beneficiaries, gaining from the stability and opportunities this framework provides. The 'iranian_nuclear_program' and 'iranian_economy' are the primary payers, bearing the costs of restrictions and sanctions. The 'hardline_iranian_factions' are excluded, as their maximalist stance is incompatible with this reading's de-escalatory approach.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the JCPOA as pure extraction by emphasizing the reciprocal nature of commitments and the genuine coordination function of de-escalation. It acknowledges the costs borne by Iran but frames them within a system designed to prevent a worse outcome (military conflict or unchecked proliferation). The graduated enforcement mechanism is intended to adapt to changing circumstances, preventing the constraint from becoming a 'piton' by allowing for dynamic adjustment rather than rigid adherence to an outdated mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_assessment_ambiguity,
    'What constitutes ''proportional'' non-compliance and ''proportional'' enforcement in practice, and is this assessment consistently applied by all P5+1 states?',
    'Analysis of historical responses to Iranian deviations, and formal statements from P5+1 members regarding their criteria for proportionality. Divergence in criteria would indicate a conceptual ambiguity.',
    'If proportionality is inconsistently applied, the constraint''s effective suppression and extractiveness could fluctuate unpredictably, leading to greater instability and potentially reclassifying it as a more extractive or less coordinated type from Iran''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_ambiguity, empirical, 'Ambiguity in the practical application of graduated enforcement.').

omega_variable(
    de_escalation_vs_deterrence_priority,
    'Does this reading prioritize de-escalation and diplomatic engagement over strict deterrence and maximal pressure, and is this prioritization shared by all key actors?',
    'Examination of diplomatic statements, policy decisions, and resource allocation by P5+1 states. If some states consistently prioritize deterrence, it indicates a divergence in the underlying normative framework.',
    'If deterrence is prioritized over de-escalation by key actors, the constraint''s ''tangled_rope'' nature could lean more towards ''snare'' due to increased coercive pressure and reduced coordination benefits for Iran.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_escalation_vs_deterrence_priority, preference, 'Underlying policy priority between de-escalation and deterrence.').

omega_variable(
    kernel_reading_distinction,
    'Is this ''graduated compliance'' reading truly distinct from the ''binding multilateral'' or ''transactional provisional'' readings, or does it merely represent a tactical approach within one of those frameworks?',
    'Conceptual analysis of the core premises of each reading: if the underlying assumptions about treaty nature, enforcement philosophy, and dispute resolution mechanisms are fundamentally different, the readings are distinct. If they share core premises and differ only in application, they are not distinct kernels.',
    'If not distinct, the classification of this constraint would collapse into one of the sibling readings, altering its claimed type and metric profile to match the dominant underlying framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinction between this reading and sibling readings of the JCPOA kernel.').


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
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_sanctions_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_proliferation_status).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the JCPOA treaty bindingness kernel. This 'graduated compliance' reading emphasizes proportional responses and de-escalation, distinct from a 'binding multilateral' or 'transactional provisional' interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
