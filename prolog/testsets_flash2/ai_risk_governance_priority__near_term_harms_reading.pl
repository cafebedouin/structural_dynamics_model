% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance: Prioritizing Near-Term Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI risk
 *   governance, which prioritizes mitigating demonstrated present harms
 *   (bias, misinformation, labor displacement, surveillance) affecting
 *   marginalized populations now. It is one of three readings of the
 *   'ai_risk_governance_priority' kernel, alongside
 *   'existential_risk_reading' and 'bridge_reading'. This reading asserts
 *   that focusing on current, tangible harms is the most ethical and
 *   effective approach to AI governance, and that diverting resources to
 *   speculative future risks is a form of extraction from those currently
 *   suffering.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.7).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Prioritizing Near-Term Harms").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '7488545f-f666-481c-b997-37976f80d277').
narrative_ontology:cs_kernel_codification('7488545f-f666-481c-b997-37976f80d277', distributed).
narrative_ontology:cs_authority_grounding('7488545f-f666-481c-b997-37976f80d277', distributed).
narrative_ontology:cs_reading_relation('7488545f-f666-481c-b997-37976f80d277', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7488545f-f666-481c-b997-37976f80d277', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('7488545f-f666-481c-b997-37976f80d277', foundational, present_suffering_demands_priority).
narrative_ontology:cs_axiom_status(present_suffering_demands_priority, holdable).
narrative_ontology:cs_axiom_grounding('7488545f-f666-481c-b997-37976f80d277', present_suffering_demands_priority, deontological).
narrative_ontology:cs_axiom('7488545f-f666-481c-b997-37976f80d277', foundational, speculative_risk_diverts_accountability).
narrative_ontology:cs_axiom_status(speculative_risk_diverts_accountability, holdable).
narrative_ontology:cs_axiom_grounding('7488545f-f666-481c-b997-37976f80d277', speculative_risk_diverts_accountability, empirically_contingent).
narrative_ontology:cs_reference_frame('7488545f-f666-481c-b997-37976f80d277', justice_oriented_ai_governance).
narrative_ontology:cs_drift_state('7488545f-f666-481c-b997-37976f80d277', contemporary_ai_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7488545f-f666-481c-b997-37976f80d277', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, near_term_harm_advocates).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, impacted_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, x_risk_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for policies and research focused on immediate, demonstrable harms of AI. They push for regulatory frameworks, fairness audits, and accountability mechanisms for deployed systems. Their influence is growing but faces significant opposition from industry and x-risk focused groups.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, near_term_harm_advocates, agenda_setter,
    organized, biographical, constrained, global).

% Marginalized populations, workers, and communities disproportionately affected by algorithmic bias, surveillance, and automation-driven labor displacement. They stand to benefit directly from policies prioritizing near-term harm mitigation, but have limited direct power in governance debates.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, impacted_communities, beneficiary,
    powerless, immediate, trapped, local).

% Bear the costs of implementing bias mitigation, undergoing audits, and complying with regulations aimed at present harms. They often prefer a focus on long-term, speculative risks, which diverts regulatory attention and resources away from their current deployment practices.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, payer,
    institutional, biographical, constrained, global).

% Their research and policy proposals, focused on existential risks from advanced AI, receive less funding and attention when near-term harms are prioritized. They view this as a misallocation of resources given the perceived catastrophic potential of future AI systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, x_risk_researchers, payer,
    powerful, civilizational, constrained, global).

% Navigate competing demands from various AI risk communities. This reading pushes them to allocate resources towards immediate regulatory action, social impact assessments, and enforcement against discriminatory AI systems, potentially at the expense of funding for speculative risk research.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, policymakers, agenda_setter,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate the tangible, observable harms of AI systems currently in deployment, ensuring resources are directed to affected populations and robust regulatory frameworks are developed.
% TRANSFER_FUNCTION: Transfers regulatory attention, research funding, and policy implementation resources from speculative, long-term AI risks to immediate, demonstrated harms. It also transfers accountability and compliance costs to technology companies.
% ABSENT_VOICES: Future generations, whose interests are central to existential risk framings, are implicitly de-prioritized. Their 'voice' is represented by x-risk researchers, who are themselves marginalized by this reading.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, regulatory efforts would likely shift back towards speculative, long-term risks, reducing accountability for current harms and leaving marginalized populations more vulnerable to algorithmic discrimination and displacement. Resource allocation for AI safety would fundamentally reorient.
% FOUNDING_PROBLEM: The rapid deployment of AI systems without adequate ethical oversight led to demonstrable harms such as algorithmic bias, job displacement, and privacy violations, disproportionately affecting vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from civil society organizations and affected communities corroborate the ongoing nature and severity of these harms. This is attested by independent researchers and advocacy groups, not just the direct beneficiaries.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) reflects the cost imposed on technology companies through increased regulation and accountability for current AI deployments, and the opportunity cost for x-risk researchers whose agenda is de-prioritized. Suppression (0.65) is moderate, as this reading actively seeks to suppress alternative framings that divert attention from present harms. Theater ratio (0.2) is low, as this reading emphasizes concrete, verifiable actions and outcomes over performative gestures. The metrics reflect the active contestation and resource reallocation inherent in this prioritization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of impacted communities, this prioritization is a necessary corrective to historical neglect. From the perspective of technology companies, it represents an undue burden on innovation. From the perspective of x-risk researchers, it is a dangerous distraction from truly catastrophic risks. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Near-term harm advocates and impacted communities are beneficiaries, as this reading directs resources and regulatory power towards their concerns. Technology companies and x-risk researchers are victims, as they bear the costs of compliance or the opportunity cost of de-prioritized agendas. Policymakers are agenda-setters, influenced by this reading to shape governance accordingly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_efficiency,
    'Is prioritizing near-term harms the most efficient allocation of limited AI safety resources to maximize overall human well-being and minimize risk?',
    'Comprehensive, independent cost-benefit analysis comparing the societal impact of mitigating present harms versus preventing speculative future harms, accounting for uncertainty and discount rates.',
    'If found inefficient, this reading''s claim to optimal resource allocation would be weakened, potentially shifting support to a ''bridge_reading'' or ''existential_risk_reading''. If efficient, it strengthens the case for this prioritization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Evaluates the efficiency of resource allocation under this prioritization.').

omega_variable(
    causal_link_to_x_risk,
    'Does effective mitigation of near-term AI harms (e.g., robust alignment, fairness, interpretability) inherently contribute to reducing long-term existential risks, or are these distinct problem sets?',
    'Theoretical and empirical research demonstrating a strong, consistent causal link between progress on near-term safety and reduction of x-risk factors, or conversely, demonstrating their independence.',
    'If a strong link is proven, this reading gains legitimacy as a ''bridge'' to x-risk mitigation, potentially leading to a reclassification towards a ''bridge_reading''. If no link, the two risk categories remain distinct, reinforcing the prioritization choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_link_to_x_risk, empirical, 'Examines the causal relationship between near-term harm mitigation and existential risk reduction.').

omega_variable(
    framing_as_diversion,
    'Is the ''existential risk'' framing primarily a genuine concern for future catastrophe, or does it function as a strategic diversion of regulatory attention from present-day corporate accountability?',
    'Sociological and political economy analysis of funding flows, lobbying efforts, and public discourse patterns, particularly examining the beneficiaries of each framing.',
    'If found to be a diversion, the ''existential_risk_reading'' would be reclassified as a ''snare'' from the perspective of impacted communities, and this ''near_term_harms_reading'' would gain significant moral and political force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_as_diversion, conceptual, 'Assesses whether x-risk framing serves as a diversion from present accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_ethics_research_funding).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_governance_priority' kernel. It is linked to 'existential_risk_reading' and 'bridge_reading' as part of a constraint family addressing the same core problem from different normative standpoints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
