% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__existential_risk_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: Prioritization of AI Existential Risk
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint story instantiates the 'existential risk' reading of AI
 *   risk governance, which prioritizes preventing superintelligence scenarios
 *   that could annihilate or permanently curtail humanity's potential. From
 *   this reading's perspective, this prioritization is a necessary
 *   coordination effort for humanity's survival. The metrics, however,
 *   reflect the substantial extraction of resources and suppression of
 *   alternative framings that this prioritization entails, especially for
 *   those focused on present-day AI harms.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda_setter and beneficiary (institutional/generational)
 *   - ai_labs_claiming_safety_leadership: Beneficiary (institutional/biographical)
 *   - future_humanity: Conceptual payer/victim (powerless/civilizational)
 *   - present_day_marginalized_communities: Payer/victim (powerless/immediate)
 *   - near_term_ai_harm_advocates: Payer/victim (organized/biographical)
 *   - governments_and_regulators: Agenda_setter (institutional/biographical)
 *   - bridge_framework_advocates: Excluded (organized/biographical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.85).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.75).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Prioritization of AI Existential Risk").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, 'f9dc5386-0b4a-405e-8a78-1ef487c4a0a1').
narrative_ontology:cs_kernel_codification('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', formalized).
narrative_ontology:cs_authority_grounding('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', expertise).
narrative_ontology:cs_interpretation_layer_present('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1').
narrative_ontology:cs_reading_relation('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', ai_risk_governance_priority__bridge_reading, forecloses).
narrative_ontology:cs_axiom('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', foundational, existential_catastrophe_is_primary_risk).
narrative_ontology:cs_axiom_status(existential_catastrophe_is_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', existential_catastrophe_is_primary_risk, empirically_contingent).
narrative_ontology:cs_axiom('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', foundational, future_potential_maximization_is_moral_imperative).
narrative_ontology:cs_axiom_status(future_potential_maximization_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', future_potential_maximization_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', humanity_first_longterm_survival_framework).
narrative_ontology:cs_drift_state('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', contemporary_ai_acceleration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f9dc5386-0b4a-405e-8a78-1ef487c4a0a1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, present_day_marginalized_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_ai_harm_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, general_public).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, longtermism_doctrine).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, precautionary_principle_extreme_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define, research, and advocate for the prioritization of existential risks from advanced AI. They receive significant funding and influence policy direction, framing their work as essential for humanity's survival.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Major AI development labs that publicly align with existential risk prioritization. This stance enhances their legitimacy, attracts talent, and potentially deflects scrutiny from present-day harms or monopolistic practices, while also genuinely investing in safety research.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    institutional, biographical, arbitrage, global).

% The conceptual beneficiary of preventing existential risk, but also the ultimate 'payer' in the sense that their potential existence is at stake. Their interests are represented by current advocates, but they have no direct agency.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% These communities disproportionately experience the present-day harms of AI (e.g., algorithmic bias in policing, surveillance, labor displacement). Their immediate concerns are often deprioritized or reframed as secondary to speculative future risks.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, present_day_marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Advocates, researchers, and policymakers focused on mitigating current, demonstrable harms of AI. They find their concerns marginalized in policy discussions and resource allocation when existential risk takes precedence.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_ai_harm_advocates, payer,
    organized, biographical, constrained, global).

% National and international bodies tasked with AI governance. They are influenced by the existential risk narrative, leading to policy initiatives and funding allocations that reflect this prioritization, sometimes at the expense of other regulatory needs.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, governments_and_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from the perceived safety and long-term security promised by existential risk mitigation efforts. However, they also bear the indirect costs of deprioritized present harms and potentially misallocated resources.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, general_public, beneficiary,
    moderate, biographical, constrained, global).

% Advocates for integrated approaches that address both near-term harms and long-term risks as interconnected issues. Their calls for unified frameworks are often sidelined by the dominant prioritization of existential risk.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, bridge_framework_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global efforts, resources, and policy attention towards preventing catastrophic outcomes from advanced AI, thereby safeguarding humanity's long-term survival and potential.
% TRANSFER_FUNCTION: Transfers significant financial, intellectual, and political capital from addressing present-day AI harms and other societal priorities towards speculative, long-term AI safety and alignment research, and the development of governance frameworks for future superintelligence.
% ABSENT_VOICES: Advocates for present-day marginalized communities, who bear the brunt of existing AI harms (bias, surveillance, labor displacement), are often sidelined or their concerns reframed as secondary to existential risk. Their voices are present but often deprioritized in the dominant discourse.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished overnight, resources would immediately reallocate towards mitigating present AI harms, developing more equitable AI systems, and addressing other pressing societal issues. The focus of AI research and policy would shift dramatically, and the legitimacy of many x-risk institutions would erode.
% FOUNDING_PROBLEM: The perceived potential for advanced AI systems to develop capabilities beyond human control, leading to unintended catastrophic outcomes, human extinction, or permanent disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: The problem is primarily attested by x-risk research institutions and some AI lab leaders. Independent corroboration from outside these benefiting parties is limited and often contested by other scientific and ethical communities, who point to the speculative nature of superintelligence and the urgency of present harms.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant diversion of resources and attention towards speculative, long-term risks, often at the expense of addressing immediate, demonstrable harms. Suppression (0.75) is high because alternative risk framings and resource allocations are actively marginalized or dismissed as distractions. Theater ratio (0.40) indicates that while genuine research occurs, there's also performative 'safety leadership' that may serve to enhance legitimacy or deflect criticism. Resistance (0.70) is substantial due to ongoing pushback from advocates for near-term harms. The claimed type is 'rope' from the perspective of the reading itself, as it frames the prioritization as a collective action problem for humanity's survival, but the metrics suggest a more extractive reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions and AI labs, this prioritization is a vital, coordinating 'rope' for humanity's future. However, from the perspective of present-day marginalized communities and near-term harm advocates, the same structure operates as a 'snare' or 'tangled_rope', extracting resources and attention from their urgent concerns and suppressing alternative policy agendas.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs are beneficiaries (low d) as they gain funding, influence, and legitimacy. Future humanity is a conceptual victim (high d) as their potential is at stake. Present-day marginalized communities and near-term harm advocates are direct victims/payers (high d) as their concerns are deprioritized and resources diverted. Governments and the general public are complex, with some benefits from perceived safety but also costs from misallocated resources.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling a potentially extractive prioritization as pure coordination. While the stated mandate is to protect humanity, the high extractiveness and suppression metrics, coupled with the deprioritization of present harms, suggest that the mechanism of prioritization itself may be generating asymmetric costs. The 'live' status of the founding problem, combined with the 'world_rearranges' verdict, indicates that the constraint is still perceived as functional, but the contested corroboration points to a potential for mandatrophy if the problem's nature is re-evaluated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'This constraint is the ''existential_risk_reading'' of the ''ai_risk_governance_priority'' kernel. How do its structural properties compare to sibling readings?',
    'Comparative analysis with ''near_term_harms_reading'' and ''bridge_reading'' constraints, focusing on differences in beneficiary/victim sets, extractiveness, and suppression.',
    'Understanding the structural deltas between readings clarifies the nature of the contestation within AI risk governance and identifies which specific structural elements are being contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Contextualizes this reading within the broader kernel of AI risk governance.').

omega_variable(
    empirical_basis_of_x_risk,
    'Is the threat of superintelligence an empirically contingent risk, or is its prioritization driven by conceptual/deontological arguments?',
    'Scientific consensus on AI capabilities trajectories, independent empirical assessment of alignment research progress, and philosophical analysis of the underlying ethical arguments.',
    'If primarily conceptual, the high extractiveness and suppression may be less justifiable empirically. If empirically contingent, a lack of progress in mitigation could lead to re-evaluation of the prioritization''s effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_basis_of_x_risk, empirical, 'Ambiguity regarding the empirical grounding of existential risk claims.').

omega_variable(
    resource_allocation_efficiency,
    'Is prioritizing existential risk the most efficient and equitable way to secure humanity''s long-term future, or does it divert resources from more immediate, solvable problems that also impact long-term potential?',
    'Comprehensive cost-benefit analysis comparing the impact of x-risk mitigation vs. present-harm mitigation on long-term human potential, including intergenerational equity considerations.',
    'If found inefficient or inequitable, the constraint''s extractiveness would be re-evaluated as less justified, potentially shifting its classification towards a Snare or Tangled Rope from more seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, preference, 'Debate over optimal resource allocation for AI risk mitigation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of near-term harm concerns structural (resource diversion, institutional gatekeeping) or internalized (belief in x-risk''s absolute primacy by advocates)?',
    'Post-policy-shift trajectory: if concerns for near-term harms persist and re-emerge strongly after x-risk prioritization is relaxed, it suggests structural suppression. If the belief in x-risk''s primacy remains, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would make the constraint more resilient to external policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ai_r_tr_t2017, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2017, 0.25).
narrative_ontology:measurement(ai_r_tr_t2019, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(ai_r_tr_t2021, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement(ai_r_tr_t2023, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_r_be_t2017, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2017, 0.68).
narrative_ontology:measurement(ai_r_be_t2019, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(ai_r_be_t2021, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2021, 0.8).
narrative_ontology:measurement(ai_r_be_t2023, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2023, 0.83).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(ai_r_su_t2017, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(ai_r_su_t2019, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(ai_r_su_t2021, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(ai_r_su_t2023, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2023, 0.73).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_ethics_research_funding).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_development_regulation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, bridge_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
