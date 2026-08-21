% ============================================================================
% CONSTRAINT STORY: ai_alignment_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_priority__existential_risk_reading, []).

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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment Priority: Existential Risk Reading
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of AI
 *   alignment, where the primary goal is to prevent catastrophic loss of
 *   control over advanced AI systems, prioritizing existential safety above
 *   all other concerns. This framing directs significant resources and
 *   attention towards highly speculative future risks, often at the expense
 *   of addressing present-day harms from deployed AI. The constraint is
 *   claimed as a 'snare' because its coordination story (saving humanity)
 *   serves as cover for an extractive mechanism that reallocates resources
 *   and suppresses alternative framings of AI risk.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.85).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, snare).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment Priority: Existential Risk Reading").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'b546ae8f-15b7-43cf-8caa-141bf0a5cb21').
narrative_ontology:cs_kernel_codification('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', distributed).
narrative_ontology:cs_authority_grounding('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', expertise).
narrative_ontology:cs_interpretation_layer_present('b546ae8f-15b7-43cf-8caa-141bf0a5cb21').
narrative_ontology:cs_reading_relation('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', ai_alignment_priority__integrated_reading, influences).
narrative_ontology:cs_axiom('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', foundational, future_catastrophe_is_primary_risk).
narrative_ontology:cs_axiom_status(future_catastrophe_is_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', future_catastrophe_is_primary_risk, empirically_contingent).
narrative_ontology:cs_axiom('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', foundational, long_term_survival_trumps_present_harms).
narrative_ontology:cs_axiom_status(long_term_survival_trumps_present_harms, holdable).
narrative_ontology:cs_axiom_grounding('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', long_term_survival_trumps_present_harms, deontological).
narrative_ontology:cs_reference_frame('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', rational_risk_prioritization).
narrative_ontology:cs_drift_state('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', contemporary_ai_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b546ae8f-15b7-43cf-8caa-141bf0a5cb21', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_day_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, ai_developers_without_x_risk_focus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Focus on preventing catastrophic loss of control over advanced AI systems, prioritizing existential safety. They define the problem, propose solutions, and direct significant research funding towards this goal. Their professional identity is deeply tied to this framing of AI risk.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The ultimate beneficiary of preventing existential risks. This entity represents the abstract concept of humanity's continued existence, which is prioritized above present-day concerns.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).

% Bears the costs of diverting resources and attention from immediate AI harms (e.g., bias, job displacement, surveillance) to speculative future risks. This group is largely undifferentiated and lacks direct agency in the discourse.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_day_humanity, payer,
    powerless, immediate, trapped, global).

% Focus on preventing present discriminatory and extractive harms from deployed AI. Their work is often deprioritized or reframed as secondary to existential risk, leading to reduced funding and influence. They are structurally excluded from the primary agenda-setting bodies.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harms_researchers, excluded,
    moderate, biographical, constrained, global).

% Are pressured to adopt existential risk mitigation strategies, even if their immediate development goals or business models are not directly related to catastrophic scenarios. This can lead to increased compliance costs and a narrowing of research directions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_developers_without_x_risk_focus, payer,
    powerful, biographical, constrained, global).

% Are influenced by the existential risk narrative, leading to policy proposals that prioritize long-term, speculative risks over immediate, tangible harms. They often lack the technical expertise to critically evaluate the claims, relying on expert consensus.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, policy_makers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global research effort to identify and mitigate highly speculative, catastrophic risks from future advanced AI systems, aiming to ensure humanity's long-term survival.
% TRANSFER_FUNCTION: Transfers significant intellectual and financial resources from addressing present-day AI harms and developing beneficial AI applications to research focused on preventing hypothetical future existential risks.
% ABSENT_VOICES: Researchers focused on near-term AI harms, marginalized communities disproportionately affected by current AI systems, and those advocating for a more balanced approach to AI development are often sidelined or reframed as less urgent. Their concerns are not central to the existential risk discourse.
% DISAPPEARANCE_RATIONALE: If the existential risk framing vanished, the AI safety discourse would immediately reorient towards present-day harms, resource allocation would shift dramatically, and the focus of AI governance would become more concrete and immediate. The entire field of AI ethics and policy would reorganize.
% FOUNDING_PROBLEM: The potential for future advanced AI systems to develop uncontrollable capabilities that could lead to human extinction or irreversible civilizational collapse.
% FOUNDING_PROBLEM_CORROBORATION: The existential risk research community strongly attests that the problem is live and growing. Critics (e.g., near-term harms researchers, some AI ethicists) contest the problem's immediacy and tractability, arguing it distracts from more pressing issues; however, no external, disinterested party can definitively corroborate or refute a speculative future risk.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant diversion of resources and attention from tangible, immediate AI harms to speculative, long-term risks. Suppression (0.7) is high because alternative framings (e.g., near-term harms, justice-oriented AI) are actively marginalized or reframed as secondary. The theater ratio (0.4) indicates that while some genuine safety research occurs, a substantial portion of the activity is performative, reinforcing the narrative of extreme future risk to maintain resource flow and agenda control. Accessibility collapse (0.6) is moderate, as alternative framings are not entirely eliminated but are significantly harder to access and fund. Resistance (0.3) is present from near-term harms advocates but is often dismissed or deprioritized.
 *
 * PERSPECTIVAL GAP:
 *   The existential risk researchers perceive this as a 'rope' or even 'mountain' – a necessary, self-evident coordination to save humanity. However, from the perspective of near-term harms researchers and present-day humanity, it operates as a 'snare,' extracting resources and attention from their immediate concerns under the guise of a universal good.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers act as agenda-setters and primary beneficiaries, defining the problem and directing resources. 'Long-term future humanity' is an abstract beneficiary. 'Present-day humanity' and 'AI developers without x-risk focus' are payers, bearing the costs of diverted resources and narrowed research. Near-term harms researchers are excluded, their concerns suppressed. Policy makers are observers, influenced by the dominant narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing existential risk) is framed as perpetually live, even as the nature of the 'threat' remains highly speculative. This prevents a mandatrophy resolution, as the problem is always 'just over the horizon.' The classification as a snare highlights how this perpetual mandate can be used to justify ongoing extraction and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_risk_quantification,
    'Can the probability and impact of highly speculative, future AI existential risks be reliably quantified to justify the current resource allocation?',
    'Development of robust, independently verifiable methodologies for forecasting and quantifying risks from future AI capabilities, or a consensus among diverse experts on the inherent unquantifiability.',
    'If quantifiable and high, it would strengthen the ''rope'' framing. If unquantifiable or low, it would expose the ''snare'' aspect by revealing a mismatch between claimed risk and justified resource diversion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(speculative_risk_quantification, empirical, 'Uncertainty regarding the empirical basis for prioritizing speculative existential risks.').

omega_variable(
    resource_diversion_justification,
    'Is the current level of resource diversion from near-term AI harms to existential risk research justified by the relative urgency and tractability of these problems?',
    'Independent, multi-stakeholder assessment of the societal impact of present AI harms versus the projected impact of future existential risks, including cost-benefit analysis of interventions.',
    'If the diversion is found to be disproportionate, it would reinforce the ''snare'' classification by highlighting the extractive nature of the resource reallocation. If proportionate, it would lend credence to the ''rope'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_justification, preference, 'Ambiguity regarding the ethical justification for prioritizing future over present harms.').

omega_variable(
    framing_underdetermination,
    'Is the ''existential_risk_reading'' the only defensible framing of AI alignment, or are there equally coherent alternative framings (e.g., near-term harms, integrated approach) that produce different classifications?',
    'A conceptual analysis demonstrating the logical coherence and practical implications of alternative framings, leading to a recognition that the choice of framing is a conceptual, not purely empirical, decision.',
    'If alternative framings are recognized as equally valid, it would highlight the ''conceptual'' nature of the constraint and the ''snare'' aspect of suppressing these alternatives. If this reading is shown to be uniquely coherent, it would strengthen its ''rope'' claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'The choice of framing for AI alignment is underdetermined, leading to different structural classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_governance_funding_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_ethics_research_priorities).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_alignment_priority' kernel. This 'existential_risk_reading' focuses on catastrophic loss of control. It influences, but does not foreclose, the 'nearterm_harms_reading' and 'integrated_reading' by shaping the discourse and resource landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
