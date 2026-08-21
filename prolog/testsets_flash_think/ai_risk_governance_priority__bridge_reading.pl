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
    narrative_ontology:affects_constraint/2,
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
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint is the 'bridge_reading' of the
 *   'ai_risk_governance_priority' kernel, which seeks to unify the discourse
 *   around AI risk by addressing both present harms and existential risks as
 *   entangled concerns. It contrasts with the 'existential_risk_reading'
 *   (prioritizing long-term catastrophic risks) and the
 *   'near_term_harms_reading' (prioritizing immediate societal impacts). This
 *   reading posits that effective AI governance requires frameworks that
 *   integrate both perspectives, acknowledging their structural entanglement
 *   rather than treating them as mutually exclusive or hierarchically
 *   ordered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.55).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.6).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'be8013ce-8106-4d1f-aba4-689470c32724').
narrative_ontology:cs_kernel_codification('be8013ce-8106-4d1f-aba4-689470c32724', formalized).
narrative_ontology:cs_authority_grounding('be8013ce-8106-4d1f-aba4-689470c32724', expertise).
narrative_ontology:cs_interpretation_layer_present('be8013ce-8106-4d1f-aba4-689470c32724').
narrative_ontology:cs_reading_relation('be8013ce-8106-4d1f-aba4-689470c32724', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('be8013ce-8106-4d1f-aba4-689470c32724', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('be8013ce-8106-4d1f-aba4-689470c32724', foundational, interdependence_of_risks).
narrative_ontology:cs_axiom_status(interdependence_of_risks, holdable).
narrative_ontology:cs_axiom_grounding('be8013ce-8106-4d1f-aba4-689470c32724', interdependence_of_risks, empirically_contingent).
narrative_ontology:cs_axiom('be8013ce-8106-4d1f-aba4-689470c32724', foundational, unified_governance_efficacy).
narrative_ontology:cs_axiom_status(unified_governance_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('be8013ce-8106-4d1f-aba4-689470c32724', unified_governance_efficacy, instrumental).
narrative_ontology:cs_reference_frame('be8013ce-8106-4d1f-aba4-689470c32724', integrated_risk_management).
narrative_ontology:cs_drift_state('be8013ce-8106-4d1f-aba4-689470c32724', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('be8013ce-8106-4d1f-aba4-689470c32724', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_researchers).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, future_generations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, single_focus_x_risk_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, single_focus_near_term_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and consortia that actively promote and fund integrated research and policy frameworks for AI risk. They benefit from their role as conveners and thought leaders in a unified discourse, but their position is structurally fragile.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Academics and practitioners who receive funding and platforms for interdisciplinary work that spans both near-term harms and existential risks. They benefit from the legitimacy of integrated approaches but face pressure from both single-focus camps.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Advocates and researchers primarily focused on preventing catastrophic or existential risks from advanced AI. They perceive resources and attention being diverted from pure x-risk research and resist integration as potentially diluting their critical focus.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, single_focus_x_risk_advocates, payer,
    powerful, generational, constrained, global).

% Advocates and researchers primarily focused on mitigating demonstrated present harms of AI (e.g., bias, misinformation, labor displacement). They perceive resources and attention being diverted from immediate impact and resist integration as abstracting from real-world, current injustices.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, single_focus_near_term_advocates, payer,
    powerful, biographical, constrained, global).

% Populations disproportionately affected by present AI harms. They benefit from a framework that acknowledges and seeks to mitigate these harms, but their direct agency in shaping the governance framework is often limited.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_communities, beneficiary,
    powerless, immediate, trapped, local).

% The abstract beneficiaries of a framework that considers long-term, potentially existential risks. Their interests are represented through advocacy and ethical reasoning, but they have no direct voice.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, universal).

% Government officials and regulatory bodies tasked with developing AI policy. They seek comprehensive and politically viable solutions but face pressure from different advocacy groups and industry lobbies.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_makers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate disparate AI risk perspectives (existential vs. near-term harms) into a coherent, actionable governance strategy, preventing siloed efforts and fostering interdisciplinary collaboration.
% TRANSFER_FUNCTION: Transfers attention, funding, and legitimacy from purely single-focus AI risk approaches to integrated, interdisciplinary frameworks. It also transfers the burden of compromise onto advocates of single-focus approaches.
% ABSENT_VOICES: AI developers and industry leaders who might prefer less comprehensive or less restrictive frameworks, or those who believe current governance is sufficient. Also, truly global south perspectives on AI harms and risks, which are often underrepresented in dominant discourse.
% DISAPPEARANCE_RATIONALE: If the unified framework vanished, the AI risk discourse would likely revert to its prior fragmented state, with existential risk and near-term harms advocates competing for resources and attention, potentially leading to less effective or contradictory policy responses and a loss of bridging institutions' influence.
% FOUNDING_PROBLEM: The fragmentation of AI risk discourse into two largely separate and often antagonistic camps (existential risk vs. near-term harms), leading to inefficient resource allocation, missed interdependencies, and an inability to form a cohesive policy agenda.
% FOUNDING_PROBLEM_CORROBORATION: Academic analyses of AI ethics and safety literature, interdisciplinary workshops, and reports from bridging organizations (e.g., Partnership on AI, AI Now Institute) corroborate the persistent fragmentation and the need for integrated approaches. However, advocates from both single-focus camps often dispute the severity of the 'fragmentation problem' relative to their primary concern.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate a unified approach to AI risk, but this coordination inherently involves extraction from those who prefer a single-focus agenda. Extractiveness (0.55) is moderate, reflecting the costs imposed on siloed research and advocacy. Suppression (0.60) is also moderate, as active effort is required to maintain the unified framework against pressures to revert to fragmentation. Resistance (0.70) is high due to the challenge to established research silos and funding priorities. Theater ratio (0.25) is low-moderate, indicating that while the effort is genuine, some performative aspects exist to maintain a facade of consensus.
 *
 * PERSPECTIVAL GAP:
 *   Advocates of single-focus approaches (both x-risk and near-term harms) perceive this constraint as extractive, diverting resources and attention from their primary concerns. Bridging institutions and integrated researchers, however, view it as a necessary and beneficial coordination mechanism. The engine's per-seat classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and integrated researchers are beneficiaries, gaining legitimacy and resources for their interdisciplinary work. Marginalized communities and future generations are also beneficiaries, as their interests are explicitly included in the framework. Single-focus x-risk and near-term harms advocates are targets (payers), as the constraint extracts resources and influence from their siloed approaches. Policy makers act as agenda-setters, attempting to implement and enforce this unified vision.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint aims to prevent mandatrophy by ensuring that AI risk governance remains relevant to the full spectrum of risks, rather than allowing one set of concerns to atrophy while the other dominates. The 'live' status of the founding problem (fragmentation) suggests that the constraint's mandate is still active, though its effectiveness is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_sustainability,
    'How effective are bridging institutions at sustaining this integration against re-siloing pressures from single-focus advocacy groups and funding bodies?',
    'Longitudinal analysis of funding allocations, research collaborations, and policy outcomes in AI risk governance over a 5-10 year period.',
    'If integration proves unsustainable, the constraint''s effective extractiveness on single-focus groups might be lower than intended (as they successfully resist), and its coordination function might be weaker, potentially reclassifying it as a Piton or a weaker Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_sustainability, empirical, 'The long-term viability of integrated AI risk governance frameworks.').

omega_variable(
    true_balance_of_extraction,
    'Does the unified framework genuinely balance the costs and benefits across both near-term and existential risk concerns, or does it disproportionately burden one side more than intended?',
    'Detailed qualitative and quantitative analysis of resource allocation, policy implementation, and stakeholder satisfaction across both risk domains within the unified framework.',
    'If disproportionate burden is found, the constraint''s effective extraction for the more burdened group would be higher, potentially pushing it closer to a Snare for that specific seat, even if the overall classification remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_balance_of_extraction, empirical, 'Equity of burden distribution within the integrated framework.').

omega_variable(
    conceptual_coherence_of_entanglement,
    'Is the claim of ''structural entanglement'' between present harms and existential risks a robust conceptual truth, or a strategic framing to achieve political consensus?',
    'Philosophical and ethical analysis of the causal and normative links between the two risk categories, alongside discourse analysis of how the ''entanglement'' narrative is deployed by different actors.',
    'If primarily a strategic framing, the constraint''s coordination function might be weaker, and its persistence more dependent on political will than genuine structural necessity, potentially reclassifying it as a Piton or a weaker Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_coherence_of_entanglement, conceptual, 'The conceptual grounding of risk entanglement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__bridge_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__bridge_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__bridge_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__bridge_reading, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__bridge_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__bridge_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__bridge_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__bridge_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__bridge_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_safety_research_funding).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_ethics_guidelines).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
