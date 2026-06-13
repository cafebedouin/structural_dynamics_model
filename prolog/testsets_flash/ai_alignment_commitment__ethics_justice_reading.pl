% ============================================================================
% CONSTRAINT STORY: ai_alignment_commitment__ethics_justice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_alignment_commitment__ethics_justice_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_alignment_commitment__ethics_justice_reading
 *   human_readable: AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics & Justice Reading)
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'ethics and justice' reading of AI
 *   alignment, which prioritizes preventing social bias and present-day harm
 *   in AI systems. It is one of several competing interpretations of the
 *   broader 'AI alignment commitment' kernel. This reading focuses on
 *   tangible, observable harms to marginalized communities, advocating for
 *   immediate intervention and accountability. The constraint is claimed as a
 *   Tangled Rope because it genuinely coordinates efforts to address real
 *   harms (beneficiaries: marginalized communities) but also extracts
 *   resources and attention from other AI development priorities (victims:
 *   performance-focused developers, long-term safety researchers) through
 *   active enforcement by ethics researchers and regulatory bodies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, 0.65).
domain_priors:suppression_score(ai_alignment_commitment__ethics_justice_reading, 0.4).
domain_priors:theater_ratio(ai_alignment_commitment__ethics_justice_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_alignment_commitment__ethics_justice_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_commitment__ethics_justice_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_commitment__ethics_justice_reading, "AI Alignment: Preventing Social Bias and Present-Day Harm (Ethics & Justice Reading)").
narrative_ontology:topic_domain(ai_alignment_commitment__ethics_justice_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_alignment_commitment__ethics_justice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_commitment__ethics_justice_reading, '2056a190-3e83-48af-a470-9154279729f0').
narrative_ontology:cs_kernel_codification('2056a190-3e83-48af-a470-9154279729f0', distributed).
narrative_ontology:cs_authority_grounding('2056a190-3e83-48af-a470-9154279729f0', expertise).
narrative_ontology:cs_interpretation_layer_present('2056a190-3e83-48af-a470-9154279729f0').
narrative_ontology:cs_reading_relation('2056a190-3e83-48af-a470-9154279729f0', ai_alignment_commitment__safety_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('2056a190-3e83-48af-a470-9154279729f0', ai_alignment_commitment__integrated_reading, influences).
narrative_ontology:cs_axiom('2056a190-3e83-48af-a470-9154279729f0', foundational, present_day_harm_is_primary_concern).
narrative_ontology:cs_axiom_status(present_day_harm_is_primary_concern, holdable).
narrative_ontology:cs_axiom_grounding('2056a190-3e83-48af-a470-9154279729f0', present_day_harm_is_primary_concern, deontological).
narrative_ontology:cs_axiom('2056a190-3e83-48af-a470-9154279729f0', foundational, ai_systems_must_be_fair_and_equitable).
narrative_ontology:cs_axiom_status(ai_systems_must_be_fair_and_equitable, holdable).
narrative_ontology:cs_axiom_grounding('2056a190-3e83-48af-a470-9154279729f0', ai_systems_must_be_fair_and_equitable, deontological).
narrative_ontology:cs_reference_frame('2056a190-3e83-48af-a470-9154279729f0', ai_for_social_good_framework).
narrative_ontology:cs_drift_state('2056a190-3e83-48af-a470-9154279729f0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2056a190-3e83-48af-a470-9154279729f0', '').
narrative_ontology:cs_kernel_id(ai_alignment_commitment__ethics_justice_reading, ai_alignment_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, ethics_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_commitment__ethics_justice_reading, social_justice_advocates).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, ai_developers_prioritizing_performance).
narrative_ontology:constraint_victim(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must invest significant resources in bias detection, mitigation, and fairness audits, often delaying product launches or compromising on raw performance metrics. They face reputational and regulatory risks if they fail to address present-day harms, but also market pressure to deploy quickly.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ai_developers_prioritizing_performance, payer,
    powerful, immediate, constrained, global).

% Are the primary beneficiaries of efforts to prevent social bias and present-day harm, as they are disproportionately affected by discriminatory AI systems. Their well-being and rights are protected by this alignment focus, though they often lack direct agency in its implementation.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, marginalized_communities, beneficiary,
    powerless, generational, trapped, global).

% Define the frameworks, metrics, and methodologies for identifying and mitigating social bias and harm. They gain influence, funding, and academic recognition from this focus, shaping the discourse and priorities of AI alignment.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, ethics_researchers, agenda_setter,
    organized, biographical, mobile, global).

% Actively push for the integration of ethical and justice considerations into AI development and deployment. They benefit from the increased attention and resources directed towards addressing systemic inequalities in AI, but face an uphill battle against entrenched power structures.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, social_justice_advocates, agenda_setter,
    organized, generational, constrained, national).

% Experience a diversion of resources, attention, and institutional priority towards present-day harm mitigation, often at the expense of funding and focus on existential or catastrophic risks from advanced AI. They argue this short-term focus may neglect more profound future threats.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, long_term_safety_researchers, payer,
    moderate, civilizational, constrained, global).

% Are increasingly tasked with developing and enforcing regulations that mandate fairness, transparency, and accountability in AI systems. They benefit from a clear mandate and public support for addressing tangible harms, but face challenges in technical implementation and enforcement.
narrative_ontology:constraint_stakeholder(ai_alignment_commitment__ethics_justice_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates AI development efforts to systematically identify, measure, and mitigate social biases and present-day harms, ensuring that AI systems are built and deployed in a manner consistent with ethical principles and justice.
% TRANSFER_FUNCTION: Transfers resources (time, money, talent) from purely performance-driven AI development and long-term safety research towards ethics, fairness, and bias mitigation research and implementation. It also transfers risk from marginalized communities to AI developers and deployers.
% ABSENT_VOICES: AI developers focused solely on rapid deployment and performance optimization, who would argue that excessive focus on present-day harms stifles innovation and that some biases are inherent in data. Also, some long-term safety advocates who believe this focus distracts from more critical existential risks.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, AI development would likely revert to prioritizing performance and profit without robust ethical safeguards. Marginalized communities would face increased exposure to biased and harmful AI systems, and the field of AI ethics would lose significant institutional support and funding.
% FOUNDING_PROBLEM: The rapid development and deployment of AI systems were found to exacerbate existing social inequalities and introduce new forms of discrimination, leading to tangible harm for vulnerable populations through biased algorithms in areas like hiring, lending, and criminal justice.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and testimonies from civil society organizations and affected individuals consistently corroborate the ongoing nature of AI-driven social bias and harm. Regulatory bodies are also increasingly acknowledging and legislating against these issues, providing external validation beyond the direct beneficiaries.
narrative_ontology:disappearance_verdict(ai_alignment_commitment__ethics_justice_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_commitment__ethics_justice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_commitment__ethics_justice_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_alignment_commitment__ethics_justice_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_alignment_commitment__ethics_justice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_alignment_commitment__ethics_justice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_alignment_commitment__ethics_justice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it mandates significant re-prioritization and resource allocation from AI developers and other research areas. Suppression (0.40) is moderate but growing, as regulatory pressure and public scrutiny increasingly compel compliance. Theater ratio (0.20) is relatively low, indicating that while some 'ethics washing' occurs, a significant portion of the activity genuinely addresses the stated harms. Accessibility collapse (0.30) is low, as alternative approaches to AI development (e.g., pure performance, long-term safety) still exist but face increasing pressure. Resistance (0.70) is high, reflecting ongoing debates and pushback from those whose priorities are de-emphasized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized communities and ethics advocates, this constraint is a necessary coordination mechanism to ensure equitable and just AI development. From the perspective of long-term safety researchers, it is an extractive diversion of critical resources from potentially catastrophic future risks. AI developers see it as a necessary but costly regulatory burden. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities and social justice advocates are clear beneficiaries, as the constraint directly addresses harms they experience or champion. Ethics researchers and regulatory bodies act as agenda-setters, gaining influence and resources. AI developers prioritizing performance and long-term safety researchers are the primary payers/victims, as they bear the costs of re-prioritization and resource diversion. The directionality for these groups is derived from their structural positions as either receiving protection/resources or bearing costs/diversions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_diversion_impact,
    'To what extent does the prioritization of present-day harm mitigation genuinely divert resources from long-term AI safety research, and what is the net impact on overall AI risk reduction?',
    'Longitudinal studies tracking funding allocations, research output, and expert consensus shifts in both present-day ethics and long-term safety domains.',
    'If diversion is substantial and detrimental to long-term safety, the overall societal benefit of this reading might be lower than perceived, potentially reclassifying it as more extractive from a broader societal perspective. If the fields are complementary or the diversion is minor, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Assessing the true cost of resource diversion from long-term AI safety.').

omega_variable(
    scope_of_harm_definition,
    'Is the definition of ''present-day harm'' sufficiently comprehensive to capture all relevant ethical and justice concerns, or does it inadvertently exclude certain forms of systemic or emergent harm?',
    'Ongoing qualitative research, community engagement, and interdisciplinary analysis to identify gaps in current harm taxonomies and mitigation strategies.',
    'If the definition is too narrow, the constraint''s effectiveness in achieving its stated goals is reduced, potentially increasing its ''theater_ratio'' and reducing its genuine coordination function. A broader definition might increase extractiveness on developers but improve overall ethical outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, conceptual, 'Evaluating the completeness of ''present-day harm'' definition.').

omega_variable(
    ethics_vs_safety_framing_contest,
    'Is the contest between present-day ethics/justice and long-term safety a genuine structural tension, or a conceptual framing that could be resolved through an integrated approach?',
    'Development and adoption of robust integrated frameworks (e.g., ''integrated_reading'' sibling) that demonstrate practical, non-zero-sum solutions to both problem sets.',
    'If the tension is primarily conceptual, the ''ethics_justice_reading'' might be seen as unnecessarily extractive from the ''safety_control_reading'', and an integrated approach would be a more efficient coordination mechanism. If the tension is structural, the current trade-offs are unavoidable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethics_vs_safety_framing_contest, conceptual, 'Structural vs. conceptual nature of the ethics-safety tension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_commitment__ethics_justice_reading, 2018, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2018, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(ai_a_tr_t2020, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(ai_a_tr_t2022, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement(ai_a_tr_t2024, ai_alignment_commitment__ethics_justice_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2018, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(ai_a_be_t2020, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(ai_a_be_t2022, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(ai_a_be_t2024, ai_alignment_commitment__ethics_justice_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2018, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(ai_a_su_t2020, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement(ai_a_su_t2022, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2022, 0.35).
narrative_ontology:measurement(ai_a_su_t2024, ai_alignment_commitment__ethics_justice_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_commitment__ethics_justice_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_development_funding_priorities).
narrative_ontology:affects_constraint(ai_alignment_commitment__ethics_justice_reading, ai_regulatory_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is the 'ethics_justice_reading' of the 'ai_alignment_commitment' kernel. It is linked to 'safety_control_reading' and 'integrated_reading' as sibling interpretations of the same core commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
