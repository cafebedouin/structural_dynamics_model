% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Bridge Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'bridge reading' of AI risk governance,
 *   which asserts that both present harms and existential risks are
 *   non-mutually-exclusive and structurally entangled, requiring unified
 *   frameworks. It aims to overcome the historical fragmentation of AI ethics
 *   and safety communities. The victim set includes both marginalized
 *   populations (bearing present harms) and future humanity (at risk from
 *   existential threats). Beneficiaries are institutions and researchers
 *   actively working on integrated approaches. This reading seeks to
 *   coordinate a broader, more holistic approach to AI governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.3).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "ai_governance/technology_ethics/risk_assessment").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f').
narrative_ontology:cs_kernel_codification('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', distributed).
narrative_ontology:cs_authority_grounding('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', expertise).
narrative_ontology:cs_interpretation_layer_present('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f').
narrative_ontology:cs_reading_relation('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', foundational, risk_spectrum_interdependence).
narrative_ontology:cs_axiom_status(risk_spectrum_interdependence, holdable).
narrative_ontology:cs_axiom_grounding('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', risk_spectrum_interdependence, empirically_contingent).
narrative_ontology:cs_axiom('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', foundational, unified_governance_necessity).
narrative_ontology:cs_axiom_status(unified_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', unified_governance_necessity, instrumental).
narrative_ontology:cs_reference_frame('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', fragmented_risk_discourse).
narrative_ontology:cs_drift_state('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('62f6bd16-3fc0-4d5d-8963-cd6f7cb6b51f', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_ai_safety_ethics_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the academic centers, think tanks, and policy groups that actively work to integrate near-term AI ethics with long-term AI safety. They benefit from funding and legitimacy when a unified approach is adopted, but their influence is fragile and depends on a few key broker actors.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary,
    organized, biographical, constrained, global).

% Researchers whose work spans both present AI harms and existential risks. They benefit from the intellectual coherence and funding opportunities that a unified framework provides, allowing their interdisciplinary work to be recognized and supported.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_ai_safety_ethics_researchers, beneficiary,
    moderate, biographical, constrained, global).

% These groups bear the brunt of present AI harms (bias, discrimination, surveillance, labor displacement). While the bridge reading acknowledges their concerns, the unified framework's implementation may still be slow or insufficient to mitigate immediate impacts, making them de facto payers of ongoing harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% The ultimate target of existential risk mitigation efforts. While the bridge reading aims to protect them, the long-term nature of the threat means they are a diffuse and unrepresented 'payer' of potential future catastrophe, with no agency to influence current governance.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Advocates primarily focused on immediate AI harms, who often view existential risk as a distraction or a 'long-termism' that diverts resources from urgent present issues. They are often excluded from unified framework discussions or feel their concerns are diluted.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_advocates, excluded,
    organized, biographical, constrained, national).

% Researchers primarily focused on preventing catastrophic AI risks, who may view near-term ethics as secondary or as potentially hindering rapid progress towards safety solutions. They may resist integrating their work with present harms, fearing a loss of focus or dilution of resources.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, long_term_safety_researchers, excluded,
    powerful, generational, mobile, global).

% Government officials and international bodies tasked with regulating AI. They are the primary agents who could implement unified frameworks, but they face pressure from both near-term and long-term camps, making their adoption of a truly integrated approach challenging.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate disparate research and policy communities (AI ethics, AI safety, human rights, futurism) that often operate in silos, ensuring that governance frameworks address the full spectrum of AI risks without false dichotomies.
% TRANSFER_FUNCTION: Transfers legitimacy, funding, and policy attention to integrated research and policy initiatives, aiming to shift resources from siloed approaches to unified frameworks. It also transfers the burden of addressing both types of risks onto a single, potentially overstretched, governance structure.
% ABSENT_VOICES: The most marginalized populations, who bear the brunt of present harms, often lack direct representation in high-level AI governance discussions. Future generations, by definition, have no voice. Both would demand more immediate and robust action on their respective concerns.
% DISAPPEARANCE_RATIONALE: If the 'bridge reading' vanished, the AI governance discourse would likely revert to a more polarized state, with near-term and long-term concerns competing for resources and attention, potentially leading to fragmented and less effective risk mitigation strategies. Bridging institutions would lose their mandate.
% FOUNDING_PROBLEM: The initial fragmentation of AI risk discourse into separate, often antagonistic, camps (near-term ethics vs. long-term safety), leading to inefficient resource allocation and incomplete governance solutions.
% FOUNDING_PROBLEM_CORROBORATION: Academic papers on interdisciplinary AI governance, reports from bridging institutions, and statements from international bodies corroborate the ongoing challenge of integrating these concerns. While progress has been made, the problem of fragmentation persists, as evidenced by continued debates and funding silos.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).
:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it aims for a more equitable distribution of attention and resources, the implementation of unified frameworks is often slow and imperfect, still leaving some harms unaddressed or future risks under-prioritized. Suppression (0.30) is relatively low as this reading primarily operates through persuasion and intellectual coordination rather than direct coercion, though it does implicitly suppress purely siloed approaches. Theater ratio (0.20) reflects some performative integration without full structural change, as the underlying disciplinary and funding silos can persist. The constraint is claimed as a 'rope' because its primary function is coordination, even if imperfectly realized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bridging institutions, this is a genuine coordination mechanism. From the perspective of purely near-term or long-term advocates, it might be seen as a 'tangled rope' that dilutes their focus or diverts resources. The engine's per-seat classification will capture this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and integrated researchers are beneficiaries, as this framework legitimizes and funds their work. Marginalized populations and future humanity are 'payers' in the sense that they bear the risks that the governance framework is meant to mitigate, and the framework's imperfections mean they continue to 'pay' the cost of unaddressed risks. Near-term and long-term advocates, while acknowledged, are often 'excluded' from the full integration, feeling their specific concerns are diluted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_gap_efficacy,
    'Does the adoption of unified frameworks actually lead to effective mitigation of both near-term harms and existential risks, or does it primarily serve to legitimize bridging institutions without sufficient impact on the ground?',
    'Empirical evaluation of policy outcomes: track resource allocation, regulatory changes, and measurable reductions in both types of risks following the implementation of unified frameworks.',
    'If the impact is low, the constraint''s effective extractiveness (from victims) and theater ratio would be higher, potentially reclassifying it as a ''tangled_rope'' or even ''piton'' if the coordination function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_gap_efficacy, empirical, 'Assesses the real-world efficacy of unified frameworks versus their symbolic value.').

omega_variable(
    resource_dilution_vs_synergy,
    'Does integrating near-term and long-term concerns lead to synergistic solutions, or does it dilute resources and attention, making both types of risks harder to address effectively?',
    'Comparative analysis of integrated vs. siloed approaches: evaluate the efficiency and effectiveness of resource deployment and problem-solving in contexts where integration is high versus low.',
    'If dilution occurs, the constraint''s coordination function is weaker, and its extractiveness (from victims) is higher due to unmet needs. If synergy is strong, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_dilution_vs_synergy, empirical, 'Examines whether integration creates synergy or dilutes efforts.').

omega_variable(
    broker_actor_fragility,
    'How dependent is the persistence of this ''bridge reading'' on a small number of key individuals or institutions that actively bridge the near-term/long-term divide?',
    'Network analysis of interdisciplinary collaborations and funding flows; scenario planning for the departure of key broker actors.',
    'High dependence indicates fragility. If these broker actors are removed, the constraint could collapse or revert to a more fragmented state, increasing extractiveness from victims as governance becomes less coherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(broker_actor_fragility, empirical, 'Assesses the structural fragility of the bridge reading''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
