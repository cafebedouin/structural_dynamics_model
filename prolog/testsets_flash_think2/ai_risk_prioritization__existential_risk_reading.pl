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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk: Existential Threat Prioritization (Existential Risk Reading)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the reading of AI risk that prioritizes
 *   existential threats from misaligned AGI, asserting that alignment
 *   research is paramount. It frames AI safety primarily as a long-term,
 *   species-level survival problem. This reading drives significant resource
 *   allocation and shapes policy discourse, often at the expense of other AI
 *   risk considerations. The constraint is classified as a Tangled Rope
 *   because it claims to coordinate for a collective good (humanity's
 *   survival) but involves substantial extraction (resource diversion,
 *   suppression of alternative priorities) and requires active enforcement to
 *   maintain its dominance.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda_setter/beneficiary (institutional/arbitrage)
 *   - longtermist_funders: Primary beneficiary (institutional/arbitrage)
 *   - future_humanity: Primary payer/victim (powerless/trapped)
 *   - near_term_ai_harms_advocates: Primary payer/excluded (organized/constrained)
 *   - general_public_resources: Payer (powerless/trapped)
 *   - ai_developers: Payer (moderate/constrained)
 *   - policy_makers: Agenda_setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.78).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.85).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk: Existential Threat Prioritization (Existential Risk Reading)").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, 'ebbcda0e-058e-44c1-811d-5f69f241374c').
narrative_ontology:cs_kernel_codification('ebbcda0e-058e-44c1-811d-5f69f241374c', formalized).
narrative_ontology:cs_authority_grounding('ebbcda0e-058e-44c1-811d-5f69f241374c', expertise).
narrative_ontology:cs_interpretation_layer_present('ebbcda0e-058e-44c1-811d-5f69f241374c').
narrative_ontology:cs_reading_relation('ebbcda0e-058e-44c1-811d-5f69f241374c', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('ebbcda0e-058e-44c1-811d-5f69f241374c', foundational, agi_poses_extinction_threat).
narrative_ontology:cs_axiom_status(agi_poses_extinction_threat, holdable).
narrative_ontology:cs_axiom_grounding('ebbcda0e-058e-44c1-811d-5f69f241374c', agi_poses_extinction_threat, empirically_contingent).
narrative_ontology:cs_axiom('ebbcda0e-058e-44c1-811d-5f69f241374c', foundational, alignment_is_paramount_solution).
narrative_ontology:cs_axiom_status(alignment_is_paramount_solution, holdable).
narrative_ontology:cs_axiom_grounding('ebbcda0e-058e-44c1-811d-5f69f241374c', alignment_is_paramount_solution, instrumental).
narrative_ontology:cs_reference_frame('ebbcda0e-058e-44c1-811d-5f69f241374c', longterm_survival_imperative).
narrative_ontology:cs_drift_state('ebbcda0e-058e-44c1-811d-5f69f241374c', contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebbcda0e-058e-44c1-811d-5f69f241374c', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, general_public_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the problem, set the research agenda for AGI alignment and capability control, and receive significant funding based on the prioritization of existential risk. They actively shape the discourse and policy recommendations.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Philanthropic organizations and high-net-worth individuals who allocate substantial capital to research and advocacy aligned with the existential risk framing. They benefit from the legitimacy and focus this framing provides to their investment thesis.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    institutional, generational, arbitrage, global).

% The ultimate target of the existential threat, they bear the conceptual cost of potentially misallocated resources if the prioritization is flawed, as their future well-being depends on effective risk mitigation. Their 'payment' is the opportunity cost of not addressing other risks.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Researchers, activists, and policymakers who focus on immediate, tangible harms of AI (e.g., algorithmic bias, job displacement, surveillance). They bear the cost of having their priorities deprioritized, their research underfunded, and their voices marginalized in the dominant discourse.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, excluded).

% Represents the societal resources (public funding, policy attention, talent) that are diverted towards existential risk mitigation, potentially at the expense of other pressing societal issues or more certain, near-term AI risks. This is an opportunity cost borne by the public.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, general_public_resources, payer,
    powerless, biographical, trapped, global).

% Engineers and researchers building AI systems who are increasingly pressured to incorporate 'alignment' principles, sometimes at the expense of immediate utility, safety for current deployments, or ethical considerations not directly related to existential risk. They bear the cost of shifting research priorities.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_developers, payer,
    moderate, biographical, constrained, global).

% Government officials and regulatory bodies who are influenced by the existential risk narrative, leading to policy decisions that prioritize AGI alignment research and capability controls, potentially diverting resources from other regulatory needs or social programs.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_prioritization__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global attention, research efforts, and resource allocation towards mitigating the perceived paramount threat of misaligned AGI, aiming to ensure humanity's long-term survival.
% TRANSFER_FUNCTION: Transfers significant research funding, policy attention, and public discourse away from near-term AI harms and other societal issues towards AGI alignment and capability control research. It also transfers legitimacy and influence to institutions and individuals aligned with this prioritization.
% ABSENT_VOICES: Advocates for immediate, tangible harms of AI (e.g., algorithmic bias, job displacement, surveillance, environmental impact) are often marginalized or dismissed as distracting from the 'real' problem. Their concerns are framed as secondary or less urgent.
% DISAPPEARANCE_RATIONALE: If the prioritization of existential AI risk vanished overnight, funding and policy would immediately reorient towards a more diverse set of AI risks, including near-term harms, and other societal challenges. Research agendas would broaden, and public discourse would shift away from a singular focus on AGI alignment.
% FOUNDING_PROBLEM: The perceived existential threat of misaligned superintelligent AI, which could lead to human extinction, requiring a focused, long-term effort to ensure humanity's survival.
% FOUNDING_PROBLEM_CORROBORATION: The problem is primarily attested by x-risk researchers, longtermist philosophers, and some prominent figures in AI development. Skeptics, including near-term harms advocates and some ethicists, contest the immediacy, certainty, and exclusivity of this threat, arguing it is speculative or overblown compared to present-day harms. Corroboration from outside the benefiting parties is contested.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) due to the significant diversion of resources (funding, talent, policy attention) towards a highly speculative, long-term threat, potentially at the expense of more immediate and certain AI-related harms. Suppression (0.85) is very high because alternative framings of AI risk, particularly those focusing on near-term societal harms, are actively marginalized, dismissed, or reframed as distractions. The persistence of this prioritization relies heavily on controlling the narrative and resource flows. Theater ratio is moderate (0.40): while genuine research and concern exist, a portion of the activity serves to maintain the dominance of this specific framing and its associated funding streams, rather than solely addressing the core problem. The measurement series show increasing extractiveness and suppression over time, reflecting the hardening of this discourse and its institutionalization.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setters (x_risk_research_institutions, longtermist_funders) perceive this as a vital coordination mechanism for humanity's survival, a 'Rope' that secures the future. However, from the perspective of payers (near_term_ai_harms_advocates, future_humanity, general_public_resources), the same structure operates as a 'Snare' or 'Tangled Rope', extracting resources and suppressing alternative, potentially more urgent, concerns. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders are clear beneficiaries, receiving funding and influence. Future humanity and general public resources are victims/payers, bearing the opportunity costs of diverted attention and resources. Near-term AI harms advocates are also victims/payers, as their priorities are suppressed and their work is de-legitimized within the dominant discourse. AI developers and policymakers are also payers, as their work is shaped by this prioritization, potentially at the expense of other considerations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the prioritization as pure coordination (a Rope) by highlighting the significant, asymmetric extraction and suppression involved. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, albeit contested, coordination function of addressing a perceived existential threat. The 'live' status of the founding problem is contested, which is a key indicator for potential mandatrophy, but the active enforcement and concentrated beneficiaries suggest it's not yet a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_threat_probability,
    'What is the actual probability and timescale of an extinction-level threat from misaligned AGI, and how does it compare to other global catastrophic risks?',
    'Development of robust, interdisciplinary methodologies for forecasting highly uncertain technological risks, coupled with empirical progress in AI capabilities and safety research that either validates or refutes current models.',
    'If the probability is significantly lower or the timescale much longer than currently asserted, the extractiveness of this prioritization would be re-evaluated as higher, potentially shifting the classification towards a Snare. Conversely, strong corroboration would reinforce the coordination aspect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_threat_probability, empirical, 'Uncertainty regarding the likelihood and timing of AGI existential risk.').

omega_variable(
    resource_allocation_efficiency,
    'Are the resources currently allocated to AGI alignment research and capability controls the most effective way to mitigate overall AI risk, considering both existential and near-term harms?',
    'Comprehensive, independent cost-benefit analyses comparing the impact of investments in existential risk mitigation versus near-term AI safety, ethics, and governance, accounting for opportunity costs.',
    'If current allocations are found to be inefficient or counterproductive for overall AI risk reduction, the extraction from ''general_public_resources'' and ''near_term_ai_harms_advocates'' would be seen as more severe, strengthening the Snare-like aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Efficiency of resource allocation for AI risk mitigation.').

omega_variable(
    framing_underdetermination,
    'Is the ''existential risk'' framing the only defensible way to conceptualize paramount AI risk, or does the ''near-term harms'' framing offer an equally coherent, yet structurally distinct, approach?',
    'Analysis of the logical coherence and empirical grounding of both framings, and their respective implications for policy and resource allocation, acknowledging that different framings can lead to different optimal strategies.',
    'If the ''near-term harms'' framing is recognized as equally coherent and valid, it would highlight the conceptual suppression inherent in the ''existential risk'' reading, potentially increasing its effective suppression score and reinforcing its Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination, conceptual, 'Alternative framings of paramount AI risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_prioritization__existential_risk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t6, ai_risk_prioritization__existential_risk_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_prioritization__existential_risk_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ai_r_tr_t18, ai_risk_prioritization__existential_risk_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(ai_r_tr_t24, ai_risk_prioritization__existential_risk_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(ai_r_tr_t30, ai_risk_prioritization__existential_risk_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(ai_r_be_t6, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 6, 0.69).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(ai_r_be_t18, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(ai_r_be_t24, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(ai_r_be_t30, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(ai_r_su_t6, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(ai_r_su_t18, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(ai_r_su_t24, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(ai_r_su_t30, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel, focusing on existential threats. A sibling reading, 'ai_risk_prioritization__near_term_harms_reading', focuses on immediate societal impacts, and is structurally influenced by this prioritization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
