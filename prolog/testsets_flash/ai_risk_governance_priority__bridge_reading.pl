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
 *   frameworks. It aims to overcome the fragmentation of the AI risk
 *   discourse. The claimed type is 'rope' because it genuinely seeks to
 *   coordinate diverse concerns for collective benefit, but its operation
 *   exhibits moderate extractiveness and suppression due to the difficulty of
 *   implementation and the inherent power asymmetries in addressing both
 *   types of risks simultaneously. The extractiveness is borne by both
 *   marginalized populations (who still experience present harms) and future
 *   generations (whose existential risks are still being addressed
 *   imperfectly).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.45).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.6).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'c4baed8c-8d60-41ab-b9a2-87b3644d5e91').
narrative_ontology:cs_kernel_codification('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', distributed).
narrative_ontology:cs_authority_grounding('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', expertise).
narrative_ontology:cs_interpretation_layer_present('c4baed8c-8d60-41ab-b9a2-87b3644d5e91').
narrative_ontology:cs_reading_relation('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', foundational, risk_interdependence_axiom).
narrative_ontology:cs_axiom_status(risk_interdependence_axiom, holdable).
narrative_ontology:cs_axiom_grounding('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', risk_interdependence_axiom, empirically_contingent).
narrative_ontology:cs_axiom('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', foundational, inclusive_governance_imperative).
narrative_ontology:cs_axiom_status(inclusive_governance_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', inclusive_governance_imperative, deontological).
narrative_ontology:cs_reference_frame('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', integrated_risk_management_paradigm).
narrative_ontology:cs_drift_state('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4baed8c-8d60-41ab-b9a2-87b3644d5e91', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions actively promote and develop unified frameworks for AI risk, seeking to integrate near-term and long-term concerns. They benefit from funding and influence directed towards interdisciplinary research and policy development, but face significant challenges in gaining widespread adoption.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    organized, biographical, constrained, global).

% Researchers whose work spans both present AI harms and existential risks. They benefit from the intellectual legitimacy and funding opportunities created by this unified approach, but their work is often under-resourced compared to more siloed efforts.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, integrated_safety_ethics_researchers, beneficiary,
    moderate, biographical, constrained, global).

% These groups bear the brunt of present AI harms (bias, discrimination, surveillance) and are often overlooked in discussions that prioritize abstract future risks. While this reading aims to include their concerns, the practical implementation of unified frameworks may still be slow to deliver tangible benefits, making them de facto payers of the status quo.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% These generations face the potential existential risks from advanced AI. While this reading acknowledges their stake, the abstract nature of these risks means their interests are often represented by proxies, and the immediate costs of governance are borne by present-day actors.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Advocates who believe that existential risks from AI should be the paramount concern, often viewing near-term harms as secondary or solvable within a long-term safety paradigm. They find their prioritization diluted by this unified approach, feeling their core message is not fully adopted.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_advocates, excluded,
    organized, generational, constrained, global).

% Advocates who prioritize immediate, demonstrable harms of AI, often viewing existential risks as speculative or a distraction from present injustices. They feel their urgent concerns are deprioritized or made abstract within a unified framework that also considers long-term, less tangible threats.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harms_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate diverse research, policy, and advocacy efforts across the spectrum of AI risks, from immediate societal impacts to long-term catastrophic scenarios, preventing fragmentation and ensuring comprehensive governance strategies.
% TRANSFER_FUNCTION: Transfers intellectual capital, funding, and policy attention towards integrated safety and ethics research and away from purely siloed approaches. It aims to transfer risk mitigation benefits to both present marginalized populations and future humanity.
% ABSENT_VOICES: While this reading attempts to bridge, the most extreme voices from both the 'existential risk only' and 'near-term harms only' camps feel their core priorities are not fully represented. They would argue for a more singular focus on their respective concerns.
% DISAPPEARANCE_RATIONALE: If this unified framework vanished, AI governance efforts would likely revert to more siloed, fragmented approaches, with separate communities focusing on either near-term harms or existential risks, potentially leading to critical gaps in overall risk mitigation.
% FOUNDING_PROBLEM: The fragmentation of AI risk discourse into two largely separate and often antagonistic camps (near-term harms vs. existential risks), leading to inefficient resource allocation, missed interdependencies, and incomplete policy solutions.
% FOUNDING_PROBLEM_CORROBORATION: Academic papers on interdisciplinary AI ethics, reports from bridging NGOs, and statements from international bodies (e.g., UNESCO, UN) corroborate the ongoing problem of fragmentation and the need for unified approaches. These sources are external to the direct beneficiaries of the bridging institutions.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while the framework aims for inclusivity, the practical costs of developing and implementing truly unified solutions are high, and some resources are diverted to maintaining the bridging institutions themselves. Suppression (0.60) is present because this reading actively suppresses purely siloed approaches, requiring advocates to frame their concerns within a broader, integrated context. Theater ratio (0.20) is low, indicating a genuine effort, but some performativity exists in the rhetoric of 'unity' that may not always translate to equitable resource allocation. The measurements show a slight increase in extractiveness and suppression as the complexity of bridging these concerns becomes more apparent, before stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bridging institutions, this is a necessary rope for collective action. From the perspective of siloed advocates, it can feel like a tangled rope or even a snare, as their specific priorities are diluted or suppressed in favor of a broader, less focused agenda. The engine's classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions and integrated researchers are beneficiaries, as the framework legitimizes and funds their work. Marginalized populations and future generations are payers, as they bear the risks and the costs of imperfect or slow implementation. Existential and near-term harms advocates are 'excluded' in the sense that their singular focus is not fully adopted, requiring them to adapt to the unified discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_equity_gap,
    'Does the implementation of unified AI risk frameworks equitably address the concerns of marginalized populations, or does it disproportionately favor abstract, long-term risks?',
    'Empirical assessment of resource allocation, policy outcomes, and impact reports from affected communities over a 5-year period.',
    'If implementation shows persistent inequity, the effective extractiveness on marginalized populations is higher, pushing the constraint towards a Tangled Rope or Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_equity_gap, empirical, 'Assesses whether unified frameworks deliver on their promise of equitable risk mitigation.').

omega_variable(
    structural_fragility_of_bridging,
    'Is the bridging function structurally robust, or does it depend on a fragile network of a few key institutions and individuals?',
    'Network analysis of funding flows, publication co-authorship, and policy influence over time. Assess resilience to the departure of key actors or institutions.',
    'If fragile, the constraint''s long-term viability as a Rope is questionable; it may degrade into a Piton if the bridging function atrophies, or revert to fragmented Snares if the underlying conflicts re-emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_fragility_of_bridging, empirical, 'Evaluates the resilience and distributed nature of the bridging mechanism.').

omega_variable(
    framing_under_determination_ai_risk,
    'Is the ''unified framework'' framing the only defensible approach, or do alternative framings (e.g., ''layered governance'' or ''context-specific risk assessment'') offer superior structural solutions?',
    'Comparative analysis of governance outcomes from different framing approaches in diverse jurisdictions. Conceptual analysis of the logical coherence and practical implications of alternative framings.',
    'If alternative framings prove more effective or less extractive, the current ''unified framework'' could be reclassified as a Tangled Rope or Snare, as its coordination story would be revealed as suboptimal or even extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination_ai_risk, conceptual, 'Examines whether the ''unified framework'' is the optimal structural solution or merely one among several, potentially less extractive, options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_governance_priority__bridge_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_governance_priority__bridge_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_governance_priority__bridge_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement(ai_r_tr_t2026, ai_risk_governance_priority__bridge_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement(ai_r_tr_t2028, ai_risk_governance_priority__bridge_reading, theater_ratio, 2028, 0.21).
narrative_ontology:measurement(ai_r_tr_t2030, ai_risk_governance_priority__bridge_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2024, 0.45).
narrative_ontology:measurement(ai_r_be_t2026, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2026, 0.47).
narrative_ontology:measurement(ai_r_be_t2028, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2028, 0.46).
narrative_ontology:measurement(ai_r_be_t2030, ai_risk_governance_priority__bridge_reading, base_extractiveness, 2030, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2022, 0.58).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(ai_r_su_t2026, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement(ai_r_su_t2028, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2028, 0.61).
narrative_ontology:measurement(ai_r_su_t2030, ai_risk_governance_priority__bridge_reading, suppression_requirement, 2030, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'bridge reading' of the 'AI risk governance priority' kernel, which seeks to integrate near-term and existential risks. It influences (and is influenced by) the 'existential_risk_reading' and 'near_term_harms_reading' by attempting to subsume their concerns into a broader framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
