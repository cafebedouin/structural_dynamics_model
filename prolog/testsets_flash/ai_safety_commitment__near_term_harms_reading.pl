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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Preventing Near-Term Harms
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint defines AI safety as the prevention of documented,
 *   present-day harms from deployed AI systems, such as algorithmic bias,
 *   discrimination, labor exploitation, and misinformation. It emphasizes
 *   accountability for current impacts over speculative future risks. This
 *   constraint is one reading of the broader 'AI safety commitment' kernel,
 *   focusing on the immediate, tangible consequences for affected
 *   populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Preventing Near-Term Harms").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '4d1729b2-64ae-490b-89d4-02f2890251f0').
narrative_ontology:cs_kernel_codification('4d1729b2-64ae-490b-89d4-02f2890251f0', distributed).
narrative_ontology:cs_authority_grounding('4d1729b2-64ae-490b-89d4-02f2890251f0', practice).
narrative_ontology:cs_interpretation_layer_present('4d1729b2-64ae-490b-89d4-02f2890251f0').
narrative_ontology:cs_reading_relation('4d1729b2-64ae-490b-89d4-02f2890251f0', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d1729b2-64ae-490b-89d4-02f2890251f0', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('4d1729b2-64ae-490b-89d4-02f2890251f0', foundational, present_day_harms_are_primary_concern).
narrative_ontology:cs_axiom_status(present_day_harms_are_primary_concern, holdable).
narrative_ontology:cs_axiom_grounding('4d1729b2-64ae-490b-89d4-02f2890251f0', present_day_harms_are_primary_concern, deontological).
narrative_ontology:cs_axiom('4d1729b2-64ae-490b-89d4-02f2890251f0', foundational, accountability_for_deployed_systems_is_paramount).
narrative_ontology:cs_axiom_status(accountability_for_deployed_systems_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('4d1729b2-64ae-490b-89d4-02f2890251f0', accountability_for_deployed_systems_is_paramount, conventional).
narrative_ontology:cs_reference_frame('4d1729b2-64ae-490b-89d4-02f2890251f0', human_rights_and_social_justice_framework).
narrative_ontology:cs_drift_state('4d1729b2-64ae-490b-89d4-02f2890251f0', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4d1729b2-64ae-490b-89d4-02f2890251f0', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies_avoiding_regulation).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, ai_developers_with_limited_accountability).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct and indirect costs of algorithmic bias, discrimination, and surveillance in areas like credit, employment, and policing. They often lack recourse or the technical means to challenge these systems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, immediate, trapped, global).

% Subject to algorithmic management systems that dictate tasks, pay, and performance metrics, often leading to exploitation, wage theft, and lack of transparency. Their livelihoods are tied to platforms, limiting exit options.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    powerless, immediate, identity_locked, global).

% Experience systemic harms from AI systems in areas like predictive policing, resource allocation, and social scoring, leading to disproportionate negative impacts. They organize to resist but face powerful institutional actors.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    moderate, biographical, constrained, local).

% Are exposed to and influenced by AI-generated or amplified misinformation, leading to social polarization, erosion of trust, and real-world harm. Their ability to discern truth is constrained by the scale and sophistication of AI-driven content.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, misinformation_targets, payer,
    powerless, immediate, constrained, global).

% Benefit from a focus on speculative future risks, which diverts attention and resources from regulating their current deployed systems and the harms they cause. They can continue to operate with limited accountability for present-day impacts.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies_avoiding_regulation, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from a narrative that prioritizes abstract future risks, allowing them to avoid stringent requirements for transparency, auditing, and ethical deployment of their current products. They face less pressure to implement costly safety measures for present-day harms.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_developers_with_limited_accountability, beneficiary,
    powerful, biographical, mobile, global).

% Actively push for policies and practices that address present-day AI harms, advocating for transparency, accountability, and justice. They set the agenda for this reading of AI safety but face significant resistance from industry and other AI safety factions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, near_term_ai_ethicists_and_advocates, agenda_setter,
    organized, biographical, constrained, national).

% Focus on preventing catastrophic future risks from advanced AI. From the perspective of this reading, their concerns are seen as a distraction from urgent present-day harms, and their proposed solutions are often deemed irrelevant or counterproductive to addressing immediate issues.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate documented harms from deployed AI systems, ensuring that ethical considerations and social justice are central to AI development and deployment.
% TRANSFER_FUNCTION: Transfers attention, resources, and regulatory pressure from speculative future risks to present-day harms, shifting accountability for negative impacts onto AI developers and deployers, and potentially transferring power to affected communities.
% ABSENT_VOICES: Existential risk researchers and proponents of 'long-term' AI safety are often excluded or marginalized in discussions focused solely on near-term harms, as their frameworks and proposed solutions are deemed irrelevant or a diversion from immediate justice issues.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, the focus on present-day harms would dissipate, leading to increased unchecked algorithmic bias, discrimination, and labor exploitation. Tech companies would face even less pressure to address these issues, and affected communities would lose a critical framework for advocacy.
% FOUNDING_PROBLEM: The rapid deployment of AI systems without adequate ethical oversight led to documented harms like algorithmic bias, discrimination, and labor exploitation, disproportionately affecting marginalized communities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from affected communities and civil society organizations consistently corroborate the ongoing nature and severity of these harms. This corroboration comes from sources independent of the direct beneficiaries of this reading.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because the failure to prevent these harms imposes significant costs on victims, while the beneficiaries (tech companies) avoid the costs of robust regulation and ethical development. Suppression (0.70) is also high, as affected communities often lack the power or mechanisms to resist or exit these systems. The theater ratio (0.20) is relatively low, indicating that while some 'safety' efforts might be performative, there is a genuine, albeit often insufficient, push to address these harms. The rising extractiveness and suppression over time reflect the increasing deployment of AI systems and the growing awareness of their negative impacts, coupled with slow regulatory response.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of affected populations, this constraint is a critical, albeit often insufficient, framework for justice. From the perspective of tech companies, it represents a potential threat to their operational freedom and profit margins, which they may seek to mitigate by shifting the safety narrative towards more abstract, long-term concerns.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations, gig workers, and communities facing algorithmic discrimination are clear targets (high d) as they bear the direct costs. Tech companies avoiding regulation and AI developers with limited accountability are beneficiaries (low d) as they profit from the lack of stringent oversight. Near-term AI ethicists and advocates act as agenda-setters, pushing for this framing of AI safety.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_allocation_divergence,
    'Does the focus on near-term harms divert resources and attention away from addressing long-term, potentially catastrophic AI risks, or are these efforts complementary?',
    'Empirical analysis of funding flows and policy priorities in AI safety research and regulation over time. If resources are zero-sum, then a trade-off exists.',
    'If resources are diverted, this reading, while addressing real harms, might inadvertently increase the risk of other types of AI safety failures. If complementary, the focus on near-term harms strengthens the overall safety ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_divergence, empirical, 'Whether near-term and long-term AI safety efforts compete for resources.').

omega_variable(
    scope_of_accountability,
    'To what extent does this reading''s emphasis on ''documented harms'' limit accountability for emerging or less quantifiable harms that are not yet fully ''documented''?',
    'Longitudinal study of AI harm identification: track the lag between the emergence of a new harm and its formal documentation and inclusion in policy frameworks.',
    'If the documentation process is slow, this reading might create a blind spot for novel or subtle harms, allowing them to proliferate before being addressed. If documentation is agile, the framework remains responsive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_accountability, conceptual, 'The potential for ''documented harms'' to create blind spots for emerging issues.').

omega_variable(
    natural_vs_constructed_harms,
    'Are the identified harms (bias, discrimination) inherent to AI technology or are they reflections of existing societal biases amplified by design choices?',
    'Comparative analysis of AI systems designed with explicit bias mitigation strategies versus those without, across different cultural contexts.',
    'If harms are primarily societal biases amplified, then interventions must target both AI design and societal structures. If inherent to AI, then technological solutions are paramount. This affects the scope of ''AI safety'' itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_harms, empirical, 'Distinguishing inherent AI harms from amplified societal biases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t2015, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement_basis(ai_s_tr_t2015, observed).
narrative_ontology:measurement(ai_s_tr_t2018, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement_basis(ai_s_tr_t2018, observed).
narrative_ontology:measurement(ai_s_tr_t2021, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement_basis(ai_s_tr_t2021, observed).
narrative_ontology:measurement(ai_s_tr_t2024, ai_safety_commitment__near_term_harms_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(ai_s_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t2015, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement_basis(ai_s_be_t2015, observed).
narrative_ontology:measurement(ai_s_be_t2018, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(ai_s_be_t2018, observed).
narrative_ontology:measurement(ai_s_be_t2021, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(ai_s_be_t2021, observed).
narrative_ontology:measurement(ai_s_be_t2024, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 2024, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t2015, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement_basis(ai_s_su_t2015, observed).
narrative_ontology:measurement(ai_s_su_t2018, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement_basis(ai_s_su_t2018, observed).
narrative_ontology:measurement(ai_s_su_t2021, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2021, 0.65).
narrative_ontology:measurement_basis(ai_s_su_t2021, observed).
narrative_ontology:measurement(ai_s_su_t2024, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 2024, 0.7).
narrative_ontology:measurement_basis(ai_s_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, algorithmic_auditing_standards).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, data_privacy_regulations).

% DUAL FORMULATION NOTE:
% This constraint is the 'near_term_harms_reading' of the 'ai_safety_commitment' kernel. It focuses on present-day, documented harms from deployed AI systems, distinguishing itself from readings that prioritize existential risks or attempt to balance both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
