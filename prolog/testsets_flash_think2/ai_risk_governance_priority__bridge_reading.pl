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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Integrated AI Risk Governance Framework (Bridge Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint describes the imperative within AI governance to address
 *   both present harms (e.g., bias, misinformation) and long-term existential
 *   risks as interconnected issues requiring unified frameworks. It
 *   represents a 'bridge reading' that seeks to overcome the historical
 *   polarization between 'AI safety' (often focused on x-risk) and 'AI
 *   ethics' (often focused on near-term harms). The constraint is claimed as
 *   a Tangled Rope because while it aims for beneficial coordination (unified
 *   frameworks), it involves active enforcement to integrate disparate views
 *   and reallocate resources, which can be perceived as extractive by those
 *   advocating for a singular focus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.55).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Integrated AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, '090f10e6-3fc2-4fcf-8a17-96b5731a2568').
narrative_ontology:cs_kernel_codification('090f10e6-3fc2-4fcf-8a17-96b5731a2568', distributed).
narrative_ontology:cs_authority_grounding('090f10e6-3fc2-4fcf-8a17-96b5731a2568', distributed).
narrative_ontology:cs_reading_relation('090f10e6-3fc2-4fcf-8a17-96b5731a2568', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('090f10e6-3fc2-4fcf-8a17-96b5731a2568', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('090f10e6-3fc2-4fcf-8a17-96b5731a2568', foundational, ai_risks_are_interconnected).
narrative_ontology:cs_axiom_status(ai_risks_are_interconnected, holdable).
narrative_ontology:cs_axiom_grounding('090f10e6-3fc2-4fcf-8a17-96b5731a2568', ai_risks_are_interconnected, empirically_contingent).
narrative_ontology:cs_axiom('090f10e6-3fc2-4fcf-8a17-96b5731a2568', foundational, unified_governance_is_optimal).
narrative_ontology:cs_axiom_status(unified_governance_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('090f10e6-3fc2-4fcf-8a17-96b5731a2568', unified_governance_is_optimal, instrumental).
narrative_ontology:cs_reference_frame('090f10e6-3fc2-4fcf-8a17-96b5731a2568', integrated_risk_paradigm).
narrative_ontology:cs_drift_state('090f10e6-3fc2-4fcf-8a17-96b5731a2568', contemporary_fragmentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('090f10e6-3fc2-4fcf-8a17-96b5731a2568', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_ai_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, marginalized_populations).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, future_generations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, pure_x_risk_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, pure_near_term_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the organizations (e.g., interdisciplinary research centers, multi-stakeholder forums) that actively promote and facilitate the integration of diverse AI risk perspectives. They benefit from the legitimacy and funding associated with this integrative approach.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Academics and practitioners whose work spans both near-term harms and existential risks. They benefit from increased funding, publication opportunities, and recognition within this integrated framework.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_ai_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Groups disproportionately affected by present AI harms (e.g., algorithmic bias, surveillance). This constraint aims to ensure their concerns are not sidelined by long-term speculative risks, making them beneficiaries of the integrated approach.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations, beneficiary,
    powerless, generational, trapped, global).

% The ultimate beneficiaries of preventing catastrophic AI risks, whether near-term or existential. This constraint seeks to ensure their long-term well-being is considered through a holistic risk lens.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Advocates and researchers primarily focused on preventing existential risks from advanced AI. They bear the cost of diluting their singular focus and resources into broader, integrated frameworks, which they may perceive as less effective for their primary goal.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, pure_x_risk_advocates, payer,
    organized, generational, constrained, global).

% Advocates and researchers primarily focused on mitigating present AI harms. They bear the cost of expanding their focus and resources to include long-term, speculative risks, which they may perceive as diverting attention from immediate suffering.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, pure_near_term_advocates, payer,
    organized, biographical, constrained, global).

% Companies and individuals developing AI systems. They face increased compliance burdens and broader ethical considerations under an integrated framework, potentially slowing development or increasing costs.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_developers, payer,
    powerful, immediate, constrained, global).

% Government bodies and international organizations tasked with regulating AI. They are pressured to adopt integrated frameworks, balancing competing advocacy groups and navigating complex technical and ethical landscapes.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Independent analysts and ethicists who study the dynamics of AI risk governance without direct involvement in advocacy or development. They provide critical assessment of the effectiveness and equity of integrated frameworks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate disparate risk perspectives (present harms and existential risks) into a coherent, actionable governance framework, preventing siloed and potentially conflicting approaches that fail to address the systemic nature of AI risk.
% TRANSFER_FUNCTION: Transfers resources (funding, attention, research effort) from single-focus advocacy/research to integrated, interdisciplinary approaches. It also transfers the burden of broader consideration onto all actors in the AI ecosystem.
% ABSENT_VOICES: Those who believe either existential risk or near-term harms are *exclusively* paramount and that any integration dilutes necessary focus. They are structurally excluded from the 'unified framework' conversation if they refuse to engage with the premise of entanglement.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the AI risk governance discourse would likely revert to a polarized, fragmented state. Separate communities would pursue their agendas in silos, potentially leading to less effective, contradictory, or even harmful policy outcomes, as systemic risks would be missed.
% FOUNDING_PROBLEM: The polarization and false dichotomy between near-term AI harms and long-term existential risks, leading to fragmented research, advocacy, and policy efforts that fail to address the systemic and interconnected nature of AI risk.
% FOUNDING_PROBLEM_CORROBORATION: Interdisciplinary researchers, independent ethics panels, and some forward-thinking policy bodies attest to the ongoing problem of fragmentation and the need for integrated approaches. Reports from organizations like the UN and OECD also highlight the need for holistic AI risk management.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.45) is moderate because while the integration aims for overall benefit, it imposes costs on specialized advocacy groups by diluting their focus and resources. Suppression (0.55) is moderate because active effort is required to counter the strong gravitational pull of single-focus narratives and to enforce interdisciplinary collaboration. Theater ratio (0.25) is low-moderate, reflecting genuine efforts towards integration, but also the risk of superficial 'unified' declarations that lack deep structural change. The temporal measurements show a gradual increase in extractiveness, theater, and suppression, indicating the growing pressure and complexity of maintaining this integrated approach over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bridging institutions, this constraint is a necessary coordination mechanism for effective AI governance. From the perspective of single-focus advocates, it can be seen as an extractive force that dilutes critical efforts and resources. The engine's computation of per-seat classifications will highlight this divergence, showing how the same structure is experienced differently based on an agent's position and preferred focus.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and integrated researchers are beneficiaries, as the constraint legitimizes and funds their interdisciplinary work. Marginalized populations and future generations are also beneficiaries, as their diverse interests are explicitly included. Pure x-risk and near-term advocates are payers/victims, as the constraint extracts from their preferred singular focus and forces them into a broader, potentially less impactful, framework. AI developers bear the cost of broader compliance. Policy makers are agenda-setters, navigating the implementation of these complex frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_superficial_integration,
    'Is the ''unified framework'' a genuine structural integration of concerns, or a superficial compromise that dilutes focus and resources without achieving true synergy?',
    'Empirical analysis of resource allocation patterns, research output, and policy outcomes: if resources are genuinely reallocated to interdisciplinary work and policy addresses both dimensions effectively, it''s genuine integration. If it''s mostly rhetorical, it''s superficial.',
    'If superficial, the constraint''s effective extractiveness for single-focus advocates is higher, as they are forced into a less effective framework. If genuine, the coordination function is stronger, reducing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_superficial_integration, empirical, 'Assessing the depth and effectiveness of integrated AI risk governance.').

omega_variable(
    resource_allocation_equity,
    'Does the integrated framework equitably distribute resources and attention to both near-term harms and existential risks, or does one dimension still implicitly dominate?',
    'Quantitative analysis of funding, research grants, and policy mandates over time, disaggregated by risk type. If one dimension consistently receives disproportionate resources, the equity claim is weakened.',
    'If inequitable, the constraint''s effective extractiveness is higher for the under-prioritized dimension''s advocates, as their concerns are still marginalized within the ''unified'' structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_equity, empirical, 'Equity of resource allocation within integrated AI risk governance.').

omega_variable(
    entanglement_premise_validity,
    'Are present harms and existential risks truly ''non-mutually-exclusive, structurally entangled concerns,'' or is this premise a conceptual framing to facilitate political compromise?',
    'Conceptual analysis and philosophical debate on the causal and ethical links between different AI risk categories. If strong, irreducible links are demonstrated, the premise is robust. If not, it''s a pragmatic framing.',
    'If the entanglement premise is weak, the justification for a unified framework is weakened, potentially re-legitimizing single-focus approaches and altering the perceived extractiveness of the integration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_premise_validity, conceptual, 'Validity of the core premise of entanglement between AI risks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 2018, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_governance_priority__bridge_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_governance_priority__bridge_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_governance_priority__bridge_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_governance_priority__bridge_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement(ai_r_tr_t2026, ai_risk_governance_priority__bridge_reading, theater_ratio, 2026, 0.24).
narrative_ontology:measurement(ai_r_tr_t2028, ai_risk_governance_priority__bridge_reading, theater_ratio, 2028, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2020, 0.38).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2022, 0.41).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2024, 0.43).
narrative_ontology:measurement(ai_r_be_t2026, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2026, 0.44).
narrative_ontology:measurement(ai_r_be_t2028, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2028, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2022, 0.51).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2024, 0.53).
narrative_ontology:measurement(ai_r_su_t2026, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2026, 0.54).
narrative_ontology:measurement(ai_r_su_t2028, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2028, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
