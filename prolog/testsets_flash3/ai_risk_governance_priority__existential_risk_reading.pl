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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_risk_governance_priority__existential_risk_reading
 *   human_readable: AI Risk Governance: Existential Risk Prioritization
 *   domain: ai_governance/technology_ethics/risk_assessment
 *
 * SUMMARY:
 *   This constraint describes the prioritization of existential risk (x-risk)
 *   from advanced AI in global AI governance. It frames the primary goal of
 *   AI safety as preventing superintelligence scenarios that could annihilate
 *   or permanently curtail humanity's potential. This reading directs
 *   significant resources and policy attention towards highly speculative
 *   future threats, often at the expense of addressing immediate,
 *   demonstrable harms of AI systems. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates a global effort around a complex
 *   problem, but simultaneously extracts resources and attention from other
 *   critical areas, creating identifiable victims.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Primary agenda-setter (institutional/mobile) — defines the problem, directs resources.
 *   - ai_labs_claiming_safety_leadership: Primary beneficiary (powerful/arbitrage) — benefits from framing their work as x-risk mitigation.
 *   - near_term_harms_advocates: Primary payer (organized/constrained) — their concerns are sidelined, receive less funding.
 *   - marginalized_communities_affected_by_ai: Primary payer/victim (powerless/trapped) — bear current harms, receive less redress.
 *   - policy_makers: Agenda-setter (institutional/constrained) — influenced by x-risk narrative, shape policy.
 *   - future_humanity: Abstract beneficiary (analytical/universal) — moral justification for prioritization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_governance_priority__existential_risk_reading, 0.4).
domain_priors:theater_ratio(ai_risk_governance_priority__existential_risk_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ai_risk_governance_priority__existential_risk_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__existential_risk_reading, "AI Risk Governance: Existential Risk Prioritization").
narrative_ontology:topic_domain(ai_risk_governance_priority__existential_risk_reading, "ai_governance/technology_ethics/risk_assessment").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__existential_risk_reading, '99fae774-3a15-4623-a830-40c195ce5c48').
narrative_ontology:cs_kernel_codification('99fae774-3a15-4623-a830-40c195ce5c48', distributed).
narrative_ontology:cs_authority_grounding('99fae774-3a15-4623-a830-40c195ce5c48', extraction).
narrative_ontology:cs_interpretation_layer_present('99fae774-3a15-4623-a830-40c195ce5c48').
narrative_ontology:cs_reading_relation('99fae774-3a15-4623-a830-40c195ce5c48', ai_risk_governance_priority__near_term_harms_reading, influences).
narrative_ontology:cs_reading_relation('99fae774-3a15-4623-a830-40c195ce5c48', ai_risk_governance_priority__bridge_reading, coexists_with).
narrative_ontology:cs_axiom('99fae774-3a15-4623-a830-40c195ce5c48', foundational, superintelligence_is_existential_threat).
narrative_ontology:cs_axiom_status(superintelligence_is_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('99fae774-3a15-4623-a830-40c195ce5c48', superintelligence_is_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('99fae774-3a15-4623-a830-40c195ce5c48', foundational, long_term_safety_precedes_short_term_harms).
narrative_ontology:cs_axiom_status(long_term_safety_precedes_short_term_harms, holdable).
narrative_ontology:cs_axiom_grounding('99fae774-3a15-4623-a830-40c195ce5c48', long_term_safety_precedes_short_term_harms, deontological).
narrative_ontology:cs_reference_frame('99fae774-3a15-4623-a830-40c195ce5c48', humanity_at_risk_from_agi).
narrative_ontology:cs_drift_state('99fae774-3a15-4623-a830-40c195ce5c48', contemporary_ai_development_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99fae774-3a15-4623-a830-40c195ce5c48', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates).
narrative_ontology:constraint_victim(ai_risk_governance_priority__existential_risk_reading, marginalized_communities_affected_by_ai).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the scope of 'AI risk' primarily around long-term, catastrophic scenarios. They receive significant funding and influence policy discussions, directing resources towards alignment-as-control and AGI governance frameworks. Their focus on speculative future threats often de-emphasizes present-day harms.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, civilizational, mobile, global).

% Major AI development labs benefit from this prioritization by framing their advanced research as 'safety-critical' and positioning themselves as essential actors in preventing existential risks. This narrative can deflect scrutiny from current product harms and consolidate power in the hands of a few large developers.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, ai_labs_claiming_safety_leadership, beneficiary,
    powerful, generational, arbitrage, global).

% These groups advocate for addressing immediate, demonstrable harms of AI such as bias, discrimination, and labor displacement. Under an existential risk prioritization, their concerns often receive less funding, policy attention, and institutional support, effectively paying a cost in neglected issues.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, near_term_harms_advocates, payer,
    organized, biographical, constrained, national).

% These communities disproportionately bear the brunt of current AI harms (e.g., algorithmic bias in policing, surveillance, welfare systems). Their immediate suffering is often framed as secondary to hypothetical future risks, leading to a lack of redress and continued exposure to harm.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, marginalized_communities_affected_by_ai, payer,
    powerless, immediate, trapped, local).

% Government bodies and international organizations tasked with AI governance. They are influenced by powerful lobbying from x-risk institutions and major AI labs, leading to policy frameworks that emphasize long-term, high-impact scenarios over immediate regulatory needs for current AI systems.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% The theoretical beneficiary of preventing existential risks. This abstract entity serves as a moral anchor for the prioritization, justifying current resource allocation and policy focus on long-term safety, even if the benefits are distant and speculative.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__existential_risk_reading, future_humanity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_risk_governance_priority__existential_risk_reading, future_humanity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global research and policy efforts around a shared, high-stakes goal: preventing catastrophic outcomes from advanced AI. It mobilizes resources and talent towards specific technical and governance challenges related to superintelligence.
% TRANSFER_FUNCTION: Transfers significant financial, intellectual, and political capital from addressing present-day AI harms and broader societal needs towards speculative, long-term AI safety and alignment research, primarily benefiting institutions and labs focused on existential risk.
% ABSENT_VOICES: Advocates for AI ethics, digital rights, and social justice who focus on the immediate, tangible impacts of AI on vulnerable populations are often marginalized in discussions dominated by existential risk. Their perspectives are crucial for a balanced risk assessment but are frequently sidelined.
% DISAPPEARANCE_RATIONALE: If this prioritization vanished, the landscape of AI governance would dramatically shift. Funding would likely reallocate towards mitigating current harms, regulatory frameworks would focus on existing systems, and the public discourse around AI would become less dominated by speculative future threats, leading to a reorientation of research and policy agendas.
% FOUNDING_PROBLEM: The potential for advanced artificial intelligence to develop capabilities beyond human control, leading to unforeseen and potentially catastrophic outcomes for humanity.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of existential risk prioritization, including prominent AI researchers and philosophers, attest that the problem is live and urgent, citing theoretical arguments and extrapolations of current AI progress. Critics, including many ethicists and social scientists, contest the urgency and even the coherence of the problem, arguing it distracts from more pressing issues; however, the core concern of potential loss of control remains a live, albeit contested, problem for many outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_governance_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__existential_risk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the focus on speculative future risks diverts substantial resources from addressing present-day harms, creating a net cost for those affected by current AI systems. Suppression (0.40) is moderate; while not overtly coercive, the dominance of the x-risk narrative in funding and policy discussions effectively suppresses alternative framings of AI risk. Theater ratio (0.55) is high because a significant portion of 'safety' work under this paradigm is performative, aimed at legitimizing advanced AI development rather than genuinely mitigating immediate, observable risks. The increasing trend in extractiveness and theater ratio over time reflects the growing institutionalization and financialization of the x-risk agenda.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk institutions and major AI labs, this prioritization is a necessary, high-minded coordination effort to secure humanity's future. From the perspective of near-term harms advocates and marginalized communities, it is an extractive mechanism that diverts attention and resources from their immediate suffering, effectively making them pay for a speculative future. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and AI labs are beneficiaries (low d) as they gain funding, influence, and a legitimizing narrative. Near-term harms advocates and marginalized communities are targets (high d) as their issues are deprioritized and resources diverted. Policy makers are agenda-setters, influenced by the dominant narrative. Future humanity is an abstract beneficiary, serving as the moral justification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the dual nature of the constraint. While it genuinely coordinates a response to a perceived threat (preventing superintelligence), its high extractiveness and theater ratio, coupled with identifiable victims, reveal it as a Tangled Rope rather than a pure Rope. The 'mandate' of preventing existential risk is used to justify an arrangement that also serves to concentrate power and resources, and deflect attention from present harms. The 'live' status of the founding problem is contested, indicating that the mandate's function is not universally accepted as primary or unproblematic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_basis_of_x_risk,
    'What is the empirical basis for the probability and timeline of superintelligence scenarios, and how robust are these predictions?',
    'Development of more rigorous, falsifiable methodologies for forecasting AI capabilities and risks, coupled with independent audits of AI progress and safety claims.',
    'If the empirical basis is weak, the extractiveness of this prioritization would be reclassified as higher, as resources are diverted based on less substantiated claims. If strong, the coordination function would be more robustly justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_basis_of_x_risk, empirical, 'Uncertainty regarding the scientific grounding of superintelligence risk assessments.').

omega_variable(
    distraction_vs_necessity,
    'To what extent does the prioritization of existential risk genuinely address the most pressing AI-related threats, versus serving as a distraction from current harms or a legitimizing narrative for powerful AI developers?',
    'Comparative analysis of resource allocation and policy outcomes in jurisdictions with different risk prioritization frameworks, assessing impact on both long-term safety and near-term harm mitigation.',
    'If primarily a distraction, the constraint would lean more towards a Snare, with the coordination story serving as cover for extraction. If genuinely necessary, its Tangled Rope nature would be more balanced, with higher coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distraction_vs_necessity, conceptual, 'Ambiguity regarding the true function of existential risk prioritization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harms advocacy structural (e.g., funding mechanisms, institutional power) or internalized (e.g., self-censorship, belief in x-risk narrative)?',
    'Post-funding-shift advocacy trajectory: if advocacy for near-term harms persists and gains traction after a reallocation of funding away from x-risk, reclassify as primarily structural. If it remains muted, partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — advocates carry the suppression with them. If structural, removing the institutional barriers would more directly empower alternative framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ai_r_tr_t5, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ai_r_tr_t10, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(ai_r_tr_t15, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__existential_risk_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_r_be_t5, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_r_be_t10, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ai_r_be_t15, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__existential_risk_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ai_r_su_t5, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(ai_r_su_t10, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(ai_r_su_t15, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__existential_risk_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__existential_risk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__existential_risk_reading, 0.1).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__near_term_harms_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_risk_governance_priority__bridge_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_safety_research_funding_allocation).
narrative_ontology:affects_constraint(ai_risk_governance_priority__existential_risk_reading, ai_regulatory_framework_development).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'AI risk governance priority' kernel. This 'existential_risk_reading' prioritizes long-term, catastrophic AI scenarios. It is structurally distinct from the 'near_term_harms_reading' (focus on current harms) and the 'bridge_reading' (unified approach), each with different beneficiaries, victims, and extractiveness profiles. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
