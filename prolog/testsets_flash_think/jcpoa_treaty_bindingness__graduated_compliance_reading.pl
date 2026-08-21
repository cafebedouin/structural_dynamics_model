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
 *   human_readable: JCPOA Graduated Compliance Framework
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) through the lens of a 'graduated compliance' reading. In this
 *   interpretation, the JCPOA functions as a scaled reciprocal commitment
 *   where enforcement actions (e.g., sanctions snapback) are tied
 *   proportionally to the severity of Iranian non-compliance. Dispute
 *   resolution mechanisms prioritize de-escalation and preserving the
 *   framework over strict legalistic punishment. Beneficiaries are pragmatic
 *   diplomacy advocates and economic actors seeking partial engagement, while
 *   victims include hardline factions on all sides who oppose the deal's
 *   compromises.
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
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Framework").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '4ee6e989-69ac-43ad-acbd-d5f4e7217a23').
narrative_ontology:cs_kernel_codification('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', formalized).
narrative_ontology:cs_authority_grounding('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', lineage).
narrative_ontology:cs_interpretation_layer_present('4ee6e989-69ac-43ad-acbd-d5f4e7217a23').
narrative_ontology:cs_reading_relation('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', foundational, proportional_response_principle).
narrative_ontology:cs_axiom_status(proportional_response_principle, holdable).
narrative_ontology:cs_axiom_grounding('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', proportional_response_principle, conventional).
narrative_ontology:cs_axiom('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', foundational, de_escalation_priority).
narrative_ontology:cs_axiom_status(de_escalation_priority, holdable).
narrative_ontology:cs_axiom_grounding('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', de_escalation_priority, instrumental).
narrative_ontology:cs_reference_frame('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', reciprocal_compliance_framework).
narrative_ontology:cs_drift_state('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', post_us_withdrawal_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4ee6e989-69ac-43ad-acbd-d5f4e7217a23', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_moderates).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_hawks).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, israeli_security_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commits to limitations on its nuclear program in exchange for sanctions relief. Bears the cost of these limitations and faces graduated enforcement actions for non-compliance. Exit means abandoning sanctions relief and facing renewed international pressure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iran, payer,
    institutional, generational, constrained, national).

% The group of nations (China, France, Germany, Russia, United Kingdom, United States) that negotiated the deal. They administer the framework, assess compliance, and implement graduated enforcement or sanctions relief. Their exit options are constrained by geopolitical considerations and the desire to prevent proliferation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1_negotiators, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the framework's emphasis on de-escalation and diplomatic solutions to proliferation. They gain political capital and influence when the deal is seen as successfully managing tensions. Their exit options involve shifting advocacy to other diplomatic initiatives.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the partial lifting of sanctions, allowing for renewed trade and investment opportunities with Iran. They seek to maximize economic gains from engagement. Their exit options involve redirecting investment to other markets if the deal falters.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_engagement, beneficiary,
    powerful, immediate, arbitrage, global).

% Benefit from the economic relief and diplomatic engagement that the JCPOA provides, which strengthens their political position domestically. Their influence wanes if the deal collapses. Exit options are limited by their political context.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_moderates, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of nuclear limitations and the perceived compromise of national sovereignty. They resist the deal and seek its collapse, viewing it as a concession. Their identity is often tied to anti-Western sentiment and self-reliance, making exit from this stance difficult.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_hardliners, payer,
    powerful, generational, identity_locked, national).

% Bear the cost of sanctions relief and the perceived legitimization of the Iranian regime. They advocate for maximum pressure and view the deal as fundamentally flawed. Their political identity is often tied to a hardline stance against Iran, making exit from this position difficult.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_sanctions_hawks, payer,
    organized, generational, identity_locked, national).

% Bears the perceived security risk of a partially constrained Iranian nuclear program and the diplomatic engagement with Iran. They advocate for stricter measures or the deal's termination. Their exit options are constrained by national security imperatives.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, israeli_security_establishment, payer,
    institutional, generational, constrained, regional).

% The International Atomic Energy Agency monitors and verifies Iran's compliance with its nuclear commitments under the JCPOA. It provides technical assessments to the P5+1. Its role is to observe and report, not to enforce or benefit directly.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent Iran from developing nuclear weapons through a verifiable, reciprocal framework of nuclear limitations and sanctions relief, managed by graduated enforcement.
% TRANSFER_FUNCTION: Transfers sanctions relief and diplomatic engagement from the P5+1 to Iran, in exchange for limitations on Iran's nuclear enrichment capacity and intrusive inspections.
% ABSENT_VOICES: Regional adversaries (e.g., Saudi Arabia) who view the deal as insufficient to address Iran's regional behavior; hardline factions within Iran and the US who reject any compromise with the other side.
% DISAPPEARANCE_RATIONALE: If the JCPOA framework vanished overnight, Iran's nuclear program would likely accelerate without international oversight, leading to increased regional instability, potential military action, and a collapse of the non-proliferation regime. The global security landscape would fundamentally shift.
% FOUNDING_PROBLEM: Iran's accelerating nuclear program and the international community's concern about nuclear proliferation, coupled with the risk of military conflict.
% FOUNDING_PROBLEM_CORROBORATION: IAEA reports, UN Security Council resolutions, and intelligence assessments from multiple nations (including those not directly party to the deal) consistently attest to the ongoing proliferation risk and the deal's role in managing it. Independent academic analyses also corroborate the problem's persistence.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness (0.45) reflects the costs of compliance for Iran and the limitations on the P5+1's unilateral actions, balanced by the benefits of non-proliferation and economic engagement. It's not pure extraction due to the reciprocal nature and the goal of de-escalation. Suppression (0.60) is moderate, reflecting the active enforcement mechanisms (sanctions, inspections) but also the graduated, rather than absolute, nature of responses. The spike in extractiveness and suppression around 2019 reflects the period after the US withdrawal, where the framework was under severe stress, leading to increased Iranian non-compliance and heightened enforcement pressure from remaining parties. Theater ratio (0.20) is low, as this reading emphasizes pragmatic, functional compliance and enforcement over performative gestures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pragmatic diplomacy advocates, the JCPOA is a successful, albeit imperfect, coordination mechanism. From the perspective of hardline factions (e.g., Iranian hardliners, US sanctions hawks), the deal is either an unacceptable compromise or a dangerous legitimization of an adversary, making them net payers of its existence. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran is a primary target (payer) due to the nuclear limitations and the threat of sanctions. The P5+1 negotiators are agenda-setters, balancing enforcement with diplomatic goals. Pragmatic diplomacy advocates and economic actors are beneficiaries, gaining from stability and market access. Hardline factions on all sides are victims, as the deal constrains their preferred maximalist policies. The IAEA acts as an analytical observer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_assessment_ambiguity,
    'How is ''proportional compliance assessment'' objectively measured and agreed upon by all parties, especially when political interests diverge?',
    'Development of a universally accepted, independent technical body or a pre-agreed, transparent metric system for assessing compliance and proportional response.',
    'If proportionality is subjective, the constraint''s enforcement can be perceived as arbitrary, increasing extractiveness for targets and potentially leading to collapse. If objective, it strengthens the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_ambiguity, conceptual, 'Ambiguity in defining and assessing proportional compliance.').

omega_variable(
    de_escalation_vs_punishment_priority,
    'Does the dispute resolution mechanism genuinely prioritize de-escalation and preservation of the framework, or is it primarily a tool for imposing penalties?',
    'Analysis of historical dispute resolution outcomes: if outcomes consistently lead to compromise and framework preservation, de-escalation is prioritized. If they lead to escalation and breakdown, it''s punitive.',
    'If de-escalation is genuinely prioritized, the constraint functions more as a Rope/Tangled Rope. If punitive, it leans more towards a Snare, as the coordination story is cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_escalation_vs_punishment_priority, empirical, 'The true priority of the dispute resolution mechanism.').

omega_variable(
    kernel_reading_identity,
    'Is this ''graduated compliance'' interpretation the dominant or most effective reading of the JCPOA''s bindingness?',
    'Comparative analysis of policy outcomes and diplomatic stability under different interpretive frameworks, as well as expert consensus on the most functional reading.',
    'If a different reading (e.g., ''binding_multilateral_reading'') proves more effective or widely accepted, the overall stability and legitimacy of the JCPOA framework would shift, potentially altering its classification and impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'The relative efficacy and acceptance of this specific reading of the JCPOA kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2017, 0.18).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2023, 0.21).
narrative_ontology:measurement(jcpo_tr_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2021, 0.5).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2023, 0.48).
narrative_ontology:measurement(jcpo_be_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2017, 0.58).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2023, 0.62).
narrative_ontology:measurement(jcpo_su_t2025, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2025, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jcpoa_treaty_bindingness' kernel. Each reading represents a different structural interpretation of the agreement's nature and enforcement, leading to different ε values and classifications. This reading emphasizes graduated, proportional compliance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
