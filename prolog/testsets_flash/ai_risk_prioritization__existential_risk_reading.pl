% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Reading
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of AI risk
 *   prioritization, where the primary concern is misaligned AGI leading to
 *   human extinction, and alignment research is paramount. This reading
 *   frames near-term harms as secondary or distractions. It is a reading of
 *   the 'ai_risk_prioritization' kernel, which also includes the
 *   'near_term_harms_reading' sibling.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.6).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'eb402296-82fd-4bc0-b1fb-3c4516bee650').
narrative_ontology:cs_kernel_codification('eb402296-82fd-4bc0-b1fb-3c4516bee650', distributed).
narrative_ontology:cs_authority_grounding('eb402296-82fd-4bc0-b1fb-3c4516bee650', expertise).
narrative_ontology:cs_interpretation_layer_present('eb402296-82fd-4bc0-b1fb-3c4516bee650').
narrative_ontology:cs_reading_relation('eb402296-82fd-4bc0-b1fb-3c4516bee650', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('eb402296-82fd-4bc0-b1fb-3c4516bee650', foundational, agi_poses_extinction_threat).
narrative_ontology:cs_axiom_status(agi_poses_extinction_threat, holdable).
narrative_ontology:cs_axiom_grounding('eb402296-82fd-4bc0-b1fb-3c4516bee650', agi_poses_extinction_threat, empirically_contingent).
narrative_ontology:cs_axiom('eb402296-82fd-4bc0-b1fb-3c4516bee650', foundational, alignment_research_is_paramount).
narrative_ontology:cs_axiom_status(alignment_research_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('eb402296-82fd-4bc0-b1fb-3c4516bee650', alignment_research_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('eb402296-82fd-4bc0-b1fb-3c4516bee650', humanity_at_risk_from_agi).
narrative_ontology:cs_drift_state('eb402296-82fd-4bc0-b1fb-3c4516bee650', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('eb402296-82fd-4bc0-b1fb-3c4516bee650', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_researchers).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a significant research effort (alignment research) but also involves asymmetric extraction. Resources (funding, attention, talent) are extracted from areas focused on near-term AI harms and directed towards existential risk. Active enforcement is present through funding mechanisms, academic prestige, and public advocacy that marginalizes alternative framings. Extractiveness is moderate (0.6) due to the significant redirection of resources. Suppression is high (0.7) because alternative risk framings are actively downplayed or dismissed as less important. Theater ratio is low (0.2) as the core research is genuine, but some advocacy may overstate the immediacy or certainty of the threat to maintain prioritization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions and longtermist funders, this is a crucial coordination mechanism for humanity's survival. From the perspective of near-term AI harms researchers, it is an extractive mechanism that diverts resources from urgent, observable problems to speculative, distant ones. Future humanity, as the ultimate victim, has no direct perspective but is represented by the x-risk community.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders are clear beneficiaries (d=0.0-0.2) as their agendas and funding priorities are amplified. Near-term AI harms researchers are payers (d=0.8-1.0) as their work is deprioritized and underfunded. Future humanity is a victim (d=1.0) as their very existence is at stake, and their interests are unilaterally defined by the beneficiaries. Policy makers and AI developers are complex, often acting as both beneficiaries (of a clear, albeit narrow, agenda) and payers (of resources or attention diverted from other areas).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing existential risk) is still considered 'live' by its proponents. However, the 'contested' status of the founding problem (is it truly the *primary* problem?) suggests a potential for mandatrophy if the existential threat is later deemed less immediate or if near-term harms become undeniably catastrophic. The classification as Tangled Rope, rather than a pure Rope, acknowledges the extractive and suppressive elements inherent in this prioritization, preventing it from being mislabeled as purely beneficial coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immediacy_of_threat,
    'Is the existential threat from AGI imminent (within 10-20 years) or a more distant, speculative risk (50+ years)?',
    'Empirical progress in AGI capabilities, expert consensus shifts, and the emergence of concrete, uncontained AGI prototypes.',
    'If the threat is more distant, the urgency of this prioritization framework diminishes, potentially reallocating resources to near-term harms. If imminent, it reinforces the current prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immediacy_of_threat, empirical, 'The timescale and certainty of AGI existential risk.').

omega_variable(
    resource_allocation_efficiency,
    'Is the current allocation of resources to existential risk research the most effective way to mitigate *all* AI-related risks, or does it create blind spots for other critical issues?',
    'Comparative analysis of risk mitigation outcomes across different funding models and research prioritization frameworks, including those focused on near-term harms.',
    'If inefficient, it would suggest the extractive component is higher than measured, as resources are not optimally deployed for overall risk reduction. If efficient, it would strengthen the coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for overall AI risk mitigation.').

omega_variable(
    framing_under_determination,
    'Is the ''existential risk'' framing the only defensible way to conceptualize the primary threat of AI, or is it a choice that serves specific institutional interests?',
    'Analysis of the historical development of AI risk discourse, the funding landscape, and the sociological dynamics of the AI safety community.',
    'If it''s primarily a choice serving interests, the constraint''s ''coordination'' function is weaker, and its ''extraction'' function is stronger, potentially reclassifying it closer to a Snare. If it''s the only defensible framing, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'The conceptual framing of AI risk and its potential for bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2025, 0.15).
narrative_ontology:measurement(ai_r_tr_t2030, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2030, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2025, 0.5).
narrative_ontology:measurement(ai_r_be_t2030, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2030, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement(ai_r_su_t2030, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2030, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_ethics_research_funding).

% DUAL FORMULATION NOTE:
% This constraint is the 'existential_risk_reading' of the 'ai_risk_prioritization' kernel. Its sibling, 'near_term_harms_reading', focuses on immediate, observable harms of AI systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
