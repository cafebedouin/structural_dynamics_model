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
 *   human_readable: AI Risk Governance: Prioritizing Present Harms
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the reading that AI risk governance must
 *   prioritize mitigating demonstrated present harms (bias, misinformation,
 *   labor displacement, surveillance) affecting marginalized populations now.
 *   It is one reading of the broader 'AI risk governance priority' kernel.
 *   From this perspective, the current state of AI deployment, without this
 *   prioritization, is highly extractive and suppressive, with identifiable
 *   victims and beneficiaries. The metrics reflect the ongoing and
 *   intensifying nature of these harms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.85).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.78).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, snare).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Prioritizing Present Harms").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '9537122e-695a-40c3-963f-eaebb79d37ea').
narrative_ontology:cs_kernel_codification('9537122e-695a-40c3-963f-eaebb79d37ea', distributed).
narrative_ontology:cs_authority_grounding('9537122e-695a-40c3-963f-eaebb79d37ea', distributed).
narrative_ontology:cs_reading_relation('9537122e-695a-40c3-963f-eaebb79d37ea', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('9537122e-695a-40c3-963f-eaebb79d37ea', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('9537122e-695a-40c3-963f-eaebb79d37ea', foundational, present_suffering_demands_immediate_action).
narrative_ontology:cs_axiom_status(present_suffering_demands_immediate_action, holdable).
narrative_ontology:cs_axiom_grounding('9537122e-695a-40c3-963f-eaebb79d37ea', present_suffering_demands_immediate_action, deontological).
narrative_ontology:cs_axiom('9537122e-695a-40c3-963f-eaebb79d37ea', foundational, algorithmic_systems_reproduce_and_amplify_social_inequalities).
narrative_ontology:cs_axiom_status(algorithmic_systems_reproduce_and_amplify_social_inequalities, holdable).
narrative_ontology:cs_axiom_grounding('9537122e-695a-40c3-963f-eaebb79d37ea', algorithmic_systems_reproduce_and_amplify_social_inequalities, empirically_contingent).
narrative_ontology:cs_reference_frame('9537122e-695a-40c3-963f-eaebb79d37ea', human_rights_and_social_justice_framework).
narrative_ontology:cs_drift_state('9537122e-695a-40c3-963f-eaebb79d37ea', contemporary_ai_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9537122e-695a-40c3-963f-eaebb79d37ea', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_developers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct and disproportionate harms of algorithmic bias, surveillance, and lack of access to redress, often with no viable alternatives to using problematic AI systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Face job displacement and deskilling due to AI-driven automation, often without adequate retraining or social safety nets, leading to economic precarity.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, workers_displaced_by_automation, payer,
    powerless, biographical, constrained, regional).

% Are disproportionately affected by data exploitation, resource extraction for AI development, and the deployment of surveillance technologies, often lacking regulatory protections or recourse.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, global_south_populations, payer,
    powerless, generational, trapped, global).

% Actively research, document, and advocate against present AI harms, expending significant resources to counter powerful industry narratives and influence policy, often facing an uphill battle.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, civil_society_advocates, payer,
    organized, biographical, constrained, national).

% Benefit from less stringent regulation and oversight on the present harms of their AI products, allowing for faster deployment and greater profit. They often shape the discourse around AI risk, diverting attention from current issues.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, technology_companies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, technology_companies, agenda_setter).

% Benefit from a regulatory environment that prioritizes innovation and future risks over immediate, demonstrated harms, enabling rapid development and deployment without extensive accountability for social impact.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% Hold the power to implement policies prioritizing present harms but are often influenced by lobbying from technology companies or distracted by other risk framings, leading to slow or insufficient action.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, governments_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__near_term_harms_reading, governments_regulators, observer).

% Advocate for a different prioritization of AI risk (long-term, catastrophic scenarios), which, from the perspective of this reading, diverts attention and resources away from mitigating present harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint (prioritizing near-term harms) aims to coordinate global and local efforts to identify, measure, and mitigate the demonstrated negative impacts of AI systems on human rights, social equity, and labor, ensuring accountability for developers and deployers.
% TRANSFER_FUNCTION: The current lack of this prioritization transfers the social, economic, and psychological costs of AI harms from technology companies and developers to marginalized populations. Implementing this prioritization would transfer regulatory attention, resources, and accountability towards mitigating these harms.
% ABSENT_VOICES: The voices of directly affected communities – including indigenous groups, disability advocates, low-wage workers, and those experiencing algorithmic discrimination – are often absent or marginalized in dominant AI risk governance discussions, which tend to be shaped by industry and academic elites.
% DISAPPEARANCE_RATIONALE: If the call for prioritizing near-term harms vanished overnight, the current trajectory of AI development and deployment would largely continue, with harms accumulating on marginalized populations, while resources and attention would continue to flow towards other risk framings. The status quo would persist.
% FOUNDING_PROBLEM: The historical and ongoing pattern of technological development disproportionately harming vulnerable populations, exacerbated by the rapid, unregulated deployment of AI systems that embed and amplify existing social inequalities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous reports from civil society organizations (e.g., AI Now Institute, Algorithmic Justice League), academic studies, investigative journalism, and direct testimony from affected communities consistently corroborate the existence and intensification of these harms, from outside the benefiting parties.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__near_term_harms_reading, world_unchanged).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_governance_priority__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__near_term_harms_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because the costs of AI deployment (social, economic, psychological) are largely externalized onto vulnerable groups, while benefits accrue to tech companies. Suppression (0.78) is high due to the power imbalance, lack of regulatory enforcement, and limited exit options for affected populations. Theater ratio (0.45) is moderate, reflecting that while some efforts are made to address bias or misinformation, many are performative or insufficient to alter the underlying extractive structures. The increasing trends in extractiveness, suppression, and theater reflect the expanding reach of AI and the growing gap between its harms and effective mitigation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of technology companies and some policymakers, the current approach might be framed as a 'rope' (coordinating innovation and economic growth) or even a 'mountain' (inevitable technological progress). However, from the perspective of affected communities, the same structure operates as a 'snare,' actively extracting value and imposing harms while suppressing resistance. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations, displaced workers, and Global South communities are clear targets (payers) of the current system, bearing the brunt of the harms. Civil society advocates also bear costs through their efforts to resist. Technology companies and AI developers are the primary beneficiaries, profiting from a regulatory environment that allows rapid deployment with less accountability for present harms. Governments and regulators are agenda-setters who could shift priorities but are often constrained by competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_divergence,
    'What is the actual proportion of resources (funding, research, policy attention) allocated to mitigating demonstrated present AI harms versus speculative existential risks?',
    'Comprehensive audit of public and private funding streams, research grants, and policy initiatives related to AI safety and ethics.',
    'If resource allocation heavily favors existential risks, it would strengthen the claim that the ''near-term harms'' reading is being actively suppressed or sidelined, reinforcing its ''snare'' classification. If allocation is balanced, it would suggest a more ''contested'' or ''tangled rope'' dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_divergence, empirical, 'Empirical measurement of resource distribution across different AI risk framings.').

omega_variable(
    causal_link_between_framings,
    'To what extent does the prioritization of existential risks actively divert resources and attention from present harms, or vice-versa, creating a zero-sum dynamic?',
    'Qualitative and quantitative analysis of policy debates, funding decisions, and public discourse to identify instances of direct trade-offs or rhetorical displacement.',
    'If a strong diversionary effect is demonstrated, it would highlight the active suppression mechanism within the ''snare'' classification. If the framings are found to be largely independent or complementary, it would weaken the ''snare'' aspect related to resource competition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_link_between_framings, conceptual, 'Whether different AI risk framings are in direct competition for resources and attention.').

omega_variable(
    efficacy_of_mitigation_efforts,
    'Are current efforts to mitigate present AI harms (e.g., bias audits, ethical guidelines) genuinely effective in reducing extractiveness and suppression, or are they primarily performative?',
    'Independent, longitudinal studies tracking the real-world impact of mitigation strategies on affected communities and the power dynamics between tech companies and users.',
    'If efforts are largely performative, it would increase the ''theater_ratio'' and reinforce the ''snare'' classification by demonstrating that the coordination story is cover. If genuinely effective, it would suggest a shift towards a ''tangled rope'' or even ''rope'' over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_mitigation_efforts, empirical, 'Assessment of the real-world impact of AI harm mitigation efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 2018, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2018, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(ai_r_tr_t2022, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(ai_r_tr_t2024, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2024, 0.43).
narrative_ontology:measurement(ai_r_tr_t2026, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement(ai_r_tr_t2028, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 2028, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2018, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(ai_r_be_t2022, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2022, 0.8).
narrative_ontology:measurement(ai_r_be_t2024, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2024, 0.83).
narrative_ontology:measurement(ai_r_be_t2026, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2026, 0.84).
narrative_ontology:measurement(ai_r_be_t2028, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 2028, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2018, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(ai_r_su_t2022, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement(ai_r_su_t2024, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2024, 0.76).
narrative_ontology:measurement(ai_r_su_t2026, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2026, 0.77).
narrative_ontology:measurement(ai_r_su_t2028, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 2028, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
