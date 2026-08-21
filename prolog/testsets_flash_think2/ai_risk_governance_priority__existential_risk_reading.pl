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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Prioritization of AI Existential Risk in Governance
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential_risk_reading' of the broader
 *   'ai_risk_governance_priority' kernel. It asserts that preventing
 *   superintelligence scenarios that could annihilate or permanently curtail
 *   humanity's potential must be the paramount concern in AI risk governance.
 *   This framing, while presented as a collective good (a 'rope'),
 *   structurally diverts resources and attention from present, tangible AI
 *   harms to speculative, future ones, leading to high effective extraction
 *   from those affected by current AI systems.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda_setter/beneficiary (institutional/arbitrage)
 *   - ai_labs_claiming_safety_leadership: Beneficiary (powerful/constrained)
 *   - advocates_for_near_term_harms: Payer/excluded (organized/constrained)
 *   - marginalized_populations_affected_by_ai_harms: Payer/victim (powerless/trapped)
 *   - policy_makers: Agenda_setter (institutional/constrained)
 *   - future_humanity: Conceptual observer/ultimate beneficiary/victim (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.75).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "Prioritization of AI Existential Risk in Governance").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '5ab5870e-4077-4bf2-b58c-108387f63944').
narrative_ontology:cs_kernel_codification('5ab5870e-4077-4bf2-b58c-108387f63944', formalized).
narrative_ontology:cs_authority_grounding('5ab5870e-4077-4bf2-b58c-108387f63944', expertise).
narrative_ontology:cs_interpretation_layer_present('5ab5870e-4077-4bf2-b58c-108387f63944').
narrative_ontology:cs_reading_relation('5ab5870e-4077-4bf2-b58c-108387f63944', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('5ab5870e-4077-4bf2-b58c-108387f63944', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('5ab5870e-4077-4bf2-b58c-108387f63944', foundational, existential_risk_is_primary).
narrative_ontology:cs_axiom_status(existential_risk_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('5ab5870e-4077-4bf2-b58c-108387f63944', existential_risk_is_primary, deontological).
narrative_ontology:cs_axiom('5ab5870e-4077-4bf2-b58c-108387f63944', secondary, superintelligence_is_imminent_or_inevitable).
narrative_ontology:cs_axiom_status(superintelligence_is_imminent_or_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('5ab5870e-4077-4bf2-b58c-108387f63944', superintelligence_is_imminent_or_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('5ab5870e-4077-4bf2-b58c-108387f63944', humanity_long_term_flourishing).
narrative_ontology:cs_drift_state('5ab5870e-4077-4bf2-b58c-108387f63944', contemporary_ai_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5ab5870e-4077-4bf2-b58c-108387f63944', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, advocates_for_near_term_harms).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, marginalized_populations_affected_by_ai_harms).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, longtermism_doctrine).
narrative_ontology:constraint_vindicates(ai_risk_governance_priority__existential_risk_reading, effective_altruism_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define, research, and advocate for the prioritization of existential AI risks. They receive significant funding and influence policy agendas, positioning themselves as essential for humanity's long-term survival.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Major AI development labs benefit from this framing by diverting scrutiny from present-day harms, attracting talent interested in 'solving' grand challenges, and positioning themselves as responsible actors leading on safety, often receiving public funding or goodwill for their efforts.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    powerful, biographical, constrained, global).

% These groups advocate for addressing immediate, tangible harms of AI (e.g., bias, surveillance, labor displacement). They bear the cost of diverted resources, attention, and policy focus, finding their concerns marginalized in high-level governance discussions.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, advocates_for_near_term_harms, payer,
    organized, biographical, constrained, global).

% These communities experience the direct, negative impacts of current AI systems. They are victims of the constraint's prioritization as resources that could mitigate their present suffering are instead allocated to speculative future risks, with little to no agency in the governance discourse.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, marginalized_populations_affected_by_ai_harms, payer,
    powerless, immediate, trapped, local).

% The conceptual entity whose potential is to be preserved or curtailed. While not an active agent, its long-term flourishing is the stated ultimate beneficiary of this prioritization, and its annihilation or curtailment is the ultimate victim.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% Government officials and international bodies who are influenced by the existential risk narrative, allocating public funds and legislative attention to this area, often at the expense of other AI governance concerns.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts and resources towards a singular, long-term goal of preventing catastrophic AI outcomes, ensuring humanity's long-term survival and flourishing.
% TRANSFER_FUNCTION: Transfers significant financial, intellectual, and political capital from addressing present-day AI harms and other societal challenges towards speculative research and governance frameworks focused on future superintelligence risks.
% ABSENT_VOICES: Advocates for near-term harms, marginalized communities disproportionately affected by current AI systems, and researchers focused on immediate, tangible ethical issues are often marginalized or excluded from high-level discussions dominated by existential risk framing.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, resources and attention would immediately shift towards mitigating present AI harms, developing beneficial AI applications with immediate impact, and addressing other pressing global challenges. The AI safety research landscape would fundamentally reorient.
% FOUNDING_PROBLEM: The perceived threat of advanced AI systems (superintelligence, AGI) developing capabilities beyond human control, leading to unintended catastrophic outcomes or the permanent curtailment of human potential.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (x-risk institutions, some AI labs) attest the problem is live and urgent, citing theoretical arguments and extrapolation from current AI progress. Critics (near-term harm advocates, some ethicists) argue the problem is speculative, unproven, and distracts from real, present dangers; independent social scientists and ethicists often corroborate the latter.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.75) is high because the prioritization diverts substantial resources (funding, talent, policy focus) from addressing immediate, demonstrable AI harms to speculative, future risks. Suppression (0.7) is significant, as alternative framings and advocacy for near-term harms are often marginalized in high-level discourse. The theater ratio (0.4) reflects that while genuine research occurs, the 'safety leadership' narrative also serves to deflect criticism and consolidate power for certain actors. The claimed type is 'rope' as this reading frames itself as coordinating humanity against a common threat, but the metrics indicate a strong extractive component.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk institutions and AI labs, this is a necessary 'rope' for humanity's survival. From the perspective of advocates for near-term harms and affected populations, it functions as a 'snare' or 'tangled_rope' that extracts resources and attention from their urgent needs, using a distant, speculative threat as justification. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs are beneficiaries, as they gain funding, influence, and legitimacy. Advocates for near-term harms and marginalized populations are payers/victims, as their concerns are deprioritized and resources diverted. Policy makers are agenda-setters, influenced by the dominant narrative. Future humanity is the conceptual object of concern, not an active agent in the present.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure coordination by highlighting the high extractiveness and suppression. While the mandate (preventing existential risk) is framed as live, the 'contested' status of the founding problem and the high extraction suggest a potential for mandatrophy, where the original coordination function (if genuine) is overshadowed by resource capture and agenda-setting power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''ai_risk_governance_priority'' kernel, or is it merely a difference in emphasis within a single, unified governance approach?',
    'Analysis of resource allocation patterns and policy outcomes: if resources are consistently diverted and alternative framings actively suppressed, it confirms a distinct, competing reading.',
    'If confirmed as a distinct reading, it strengthens the case for analyzing it as a separate constraint with its own structural properties. If not, its extractive elements might be re-attributed to a broader, less specific ''ai_risk_governance'' constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as the ''existential_risk_reading'' of the ''ai_risk_governance_priority'' kernel.').

omega_variable(
    existential_risk_speculation_level,
    'To what extent is the ''existential risk'' from superintelligence a scientifically grounded, imminent threat versus a speculative, distant, or even hypothetical concern?',
    'Consensus among independent AI safety researchers (not affiliated with x-risk institutions) on timelines and mechanisms of AGI development, and empirical evidence of scaling laws leading to unmanageable capabilities.',
    'If the risk is highly speculative, the justification for high extraction and suppression weakens, potentially reclassifying the constraint towards a ''snare'' or ''tangled_rope'' due to the lack of a genuine coordination problem. If the risk is confirmed as imminent, it would strengthen the ''rope'' claim, though the extractive elements would still need justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_risk_speculation_level, empirical, 'Assesses the empirical grounding of the existential risk claim.').

omega_variable(
    resource_diversion_impact,
    'What is the quantifiable impact of resource diversion (funding, talent, policy attention) from near-term AI harms to existential risk on the well-being of marginalized populations?',
    'Socio-economic impact assessments, public health data, and ethnographic studies in communities affected by AI bias, surveillance, and automation, comparing outcomes with counterfactual scenarios of increased resource allocation to near-term harms.',
    'A high, demonstrable negative impact would strengthen the ''victim'' status of affected populations and increase the effective extraction (chi) for those seats, pushing the classification further from ''rope'' towards ''snare'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'Quantifies the real-world cost of prioritization on affected communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_risk_governance_priority' kernel. This 'existential_risk_reading' prioritizes future, speculative risks, while 'near_term_harms_reading' focuses on present, demonstrable harms, and 'bridge_reading' attempts to unify both. Each reading constitutes a distinct constraint due to differing ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
