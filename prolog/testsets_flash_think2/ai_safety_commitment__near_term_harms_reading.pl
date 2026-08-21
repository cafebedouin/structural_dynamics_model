% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Preventing Present-Day Harms
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the reading of 'AI safety' that prioritizes
 *   preventing and mitigating documented, present-day harms from deployed AI
 *   systems, such as algorithmic bias, discrimination, labor exploitation,
 *   and misinformation. It stands in contrast to readings that focus
 *   primarily on speculative, long-term existential risks. While ostensibly a
 *   coordination effort to make AI 'safe,' its implementation often involves
 *   asymmetric extraction, where tech companies benefit from a narrow
 *   definition of safety that avoids more stringent regulation, while
 *   marginalized communities continue to bear the brunt of AI's negative
 *   impacts.
 *
 * KEY AGENTS:
 *   - tech_companies: Agenda setter/Beneficiary (institutional/arbitrage)
 *   - ai_developers: Beneficiary (powerful/mobile)
 *   - marginalized_populations: Payer (powerless/trapped)
 *   - gig_workers: Payer (powerless/identity_locked)
 *   - communities_facing_algorithmic_discrimination: Payer (organized/constrained)
 *   - ai_safety_advocates_near_term: Observer/Agenda setter (moderate/mobile)
 *   - regulators: Observer/Agenda setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Preventing Present-Day Harms").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '10049a21-ace7-47b6-a72d-0ab7b8fae0c3').
narrative_ontology:cs_kernel_codification('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', distributed).
narrative_ontology:cs_authority_grounding('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', extraction).
narrative_ontology:cs_interpretation_layer_present('10049a21-ace7-47b6-a72d-0ab7b8fae0c3').
narrative_ontology:cs_reading_relation('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', foundational, present_harms_are_primary_concern).
narrative_ontology:cs_axiom_status(present_harms_are_primary_concern, holdable).
narrative_ontology:cs_axiom_grounding('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', present_harms_are_primary_concern, empirically_contingent).
narrative_ontology:cs_axiom('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', foundational, accountability_for_deployed_systems).
narrative_ontology:cs_axiom_status(accountability_for_deployed_systems, holdable).
narrative_ontology:cs_axiom_grounding('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', accountability_for_deployed_systems, deontological).
narrative_ontology:cs_reference_frame('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', human_rights_and_social_justice_framework).
narrative_ontology:cs_drift_state('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', contemporary_ai_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10049a21-ace7-47b6-a72d-0ab7b8fae0c3', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_developers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These companies develop and deploy AI systems. They benefit from a definition of AI safety that focuses on easily quantifiable or remediable harms, allowing them to avoid more fundamental structural changes or costly regulations. They often control the narrative and resources for 'AI safety' initiatives.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, tech_companies, beneficiary).

% Individual developers and teams within companies who benefit from clearer, albeit sometimes narrow, guidelines for 'safe' AI development. They may face pressure to implement superficial fixes rather than deep systemic changes, but also benefit from the perception of safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% These communities disproportionately experience harms from biased algorithms, discriminatory systems, and surveillance technologies. They bear the direct costs of 'unsafe' AI systems, often with limited recourse or ability to opt out.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, local).

% Workers whose livelihoods are managed by algorithmic systems, leading to exploitation, wage theft, and lack of transparency. Their identity is often tied to the platforms, making exit difficult despite adverse conditions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    powerless, immediate, identity_locked, local).

% Groups organizing to resist and mitigate the impact of discriminatory AI systems in areas like housing, credit, and policing. They bear the costs of systemic bias and face an uphill battle for accountability and change.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    organized, generational, constrained, regional).

% Researchers, activists, and policy experts who champion the focus on present-day harms. They work to expose issues, propose regulations, and shift the discourse, but often struggle for funding and influence against more powerful actors.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_safety_advocates_near_term, observer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_safety_advocates_near_term, agenda_setter).

% Government bodies tasked with overseeing technology and protecting citizens. They are often reactive, playing catch-up with rapidly evolving AI, and face lobbying pressure from tech companies. Their actions can enforce or mitigate the constraint.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate efforts and resources towards making AI systems 'safe' by identifying, measuring, and mitigating documented harms such as bias, discrimination, and labor exploitation, thereby fostering public trust and responsible innovation.
% TRANSFER_FUNCTION: Transfers the burden of addressing AI-related harms from tech companies (who might otherwise face stricter regulation or liability) to affected communities (who continue to bear the harms) and to advocates (who expend resources pushing for change). It also transfers attention and resources away from more fundamental systemic changes.
% ABSENT_VOICES: The voices of those most directly impacted by AI harms (e.g., individuals wrongly identified by facial recognition, workers subjected to unfair algorithmic management) are often marginalized in policy discussions, replaced by industry-led 'ethics' initiatives or academic debates. Their direct experiences would highlight the urgency and severity of the extraction.
% DISAPPEARANCE_RATIONALE: If the commitment to addressing near-term AI harms vanished, the existing harms would likely intensify without even the current, often insufficient, mitigation efforts. Public trust in AI would erode completely, potentially leading to widespread resistance or calls for outright bans, fundamentally reorganizing the AI development and deployment landscape.
% FOUNDING_PROBLEM: The rapid deployment of AI systems led to documented instances of bias, discrimination, privacy violations, and labor exploitation, creating a need for mechanisms to ensure these systems do not cause undue harm.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities consistently corroborate the ongoing existence and severity of these harms. While tech companies acknowledge some issues, their framing often downplays the systemic nature of the problem, making independent corroboration crucial.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects that despite calls for 'safety,' the actual burden of harms continues to fall disproportionately on vulnerable populations, while tech companies largely avoid accountability for systemic issues. Suppression (0.70) is significant due to the power imbalance, the technical opacity of AI systems, and the difficulty for affected individuals to organize or exit. The rising theater ratio (0.45) indicates a growing gap between performative 'ethics' initiatives and genuine, impactful harm reduction, as resources are often diverted to less challenging or more PR-friendly efforts. Resistance (0.68) is high from affected communities and advocates, but often outmatched by institutional power.
 *
 * PERSPECTIVAL GAP:
 *   Tech companies and AI developers perceive this constraint as a necessary, albeit sometimes burdensome, coordination mechanism for responsible innovation. They emphasize the technical challenges and incremental progress. Conversely, marginalized populations and gig workers experience it as a system that perpetuates and legitimizes their exploitation, with 'safety' efforts often failing to address their lived realities. Regulators and near-term advocates often see a gap between stated intentions and actual outcomes, struggling to enforce meaningful change.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech companies and AI developers are beneficiaries as the current framing of 'near-term harms' often allows them to continue deploying systems with significant negative impacts, while appearing to address safety concerns. Marginalized populations, gig workers, and communities facing algorithmic discrimination are clear targets, bearing the direct costs of these harms. Near-term AI safety advocates and regulators are observers, attempting to shift the directionality but often constrained by the power of the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent harms is still live, but its effectiveness is contested. The classification as a Tangled Rope highlights that while there is a genuine coordination function (the desire for 'safe' AI), it is intertwined with asymmetric extraction. The 'safety' narrative can serve as a cover for maintaining the status quo, where the costs of AI deployment are externalized onto vulnerable groups, preventing the constraint from being a true Rope. The rising theater ratio suggests a drift towards performative compliance rather than substantive resolution of the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_safety_scope,
    'Does ''AI safety'' in this context encompass only technical fixes for bias and discrimination, or does it extend to broader socio-economic impacts like labor displacement and power asymmetries?',
    'Analysis of funding allocations for ''AI safety'' research and policy initiatives: if funding disproportionately targets technical mitigation over socio-economic impact studies and policy, the scope is narrower.',
    'If the scope is narrow, the effective extractiveness is higher, as many harms remain unaddressed by the ''safety'' framework. If broad, the constraint moves closer to a genuine coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_safety_scope, conceptual, 'Ambiguity in the scope of ''AI safety'' for near-term harms.').

omega_variable(
    effectiveness_of_mitigation,
    'Are current industry-led ''AI ethics'' and ''responsible AI'' initiatives genuinely mitigating harms, or are they primarily serving as public relations and regulatory capture mechanisms?',
    'Independent, longitudinal audits of deployed AI systems'' impact on affected communities, measuring actual harm reduction versus reported compliance metrics.',
    'If initiatives are largely performative, the theater_ratio is higher, and the constraint functions more as a Snare. If genuinely effective, it moves closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_mitigation, empirical, 'Effectiveness of current AI harm mitigation efforts.').

omega_variable(
    resource_allocation_bias,
    'Is the allocation of resources (funding, talent, attention) within the broader AI safety field disproportionately skewed towards speculative long-term risks at the expense of documented near-term harms?',
    'Quantitative analysis of research grants, academic publications, and industry investment across different AI safety sub-fields over time.',
    'If resources are skewed, the ''near_term_harms_reading'' is structurally undermined, increasing its effective extractiveness by diverting capacity from its core mandate. This would strengthen the ''influences'' relation to the ''existential_risk_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_bias, empirical, 'Bias in resource allocation within AI safety discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2015, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ai_s_tr_t2017, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(ai_s_tr_t2019, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(ai_s_tr_t2021, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2021, 0.4).
narrative_ontology:measurement(ai_s_tr_t2023, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2023, 0.43).
narrative_ontology:measurement(ai_s_tr_t2025, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2015, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_s_be_t2017, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(ai_s_be_t2019, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement(ai_s_be_t2021, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2021, 0.74).
narrative_ontology:measurement(ai_s_be_t2023, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2023, 0.76).
narrative_ontology:measurement(ai_s_be_t2025, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2015, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(ai_s_su_t2017, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(ai_s_su_t2019, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(ai_s_su_t2021, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(ai_s_su_t2023, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2023, 0.69).
narrative_ontology:measurement(ai_s_su_t2025, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
