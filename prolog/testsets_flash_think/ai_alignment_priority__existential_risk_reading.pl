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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment: Existential Risk Priority
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint represents the 'existential risk' reading of the broader
 *   AI alignment priority kernel. It posits that preventing catastrophic loss
 *   of control over advanced AI systems is the paramount concern,
 *   prioritizing existential safety above all else. This framing drives
 *   significant resource allocation and shapes policy discourse, often at the
 *   expense of addressing more immediate, tangible harms from AI. The high
 *   extractiveness reflects the diversion of resources and attention from
 *   other pressing issues, while suppression reflects the active defense of
 *   this priority against competing ethical frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.8).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment: Existential Risk Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, 'ea68389b-2542-4c60-9f54-c594e053fdd1').
narrative_ontology:cs_kernel_codification('ea68389b-2542-4c60-9f54-c594e053fdd1', formalized).
narrative_ontology:cs_authority_grounding('ea68389b-2542-4c60-9f54-c594e053fdd1', expertise).
narrative_ontology:cs_interpretation_layer_present('ea68389b-2542-4c60-9f54-c594e053fdd1').
narrative_ontology:cs_reading_relation('ea68389b-2542-4c60-9f54-c594e053fdd1', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('ea68389b-2542-4c60-9f54-c594e053fdd1', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('ea68389b-2542-4c60-9f54-c594e053fdd1', foundational, catastrophic_loss_of_control_is_primary_risk).
narrative_ontology:cs_axiom_status(catastrophic_loss_of_control_is_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('ea68389b-2542-4c60-9f54-c594e053fdd1', catastrophic_loss_of_control_is_primary_risk, empirically_contingent).
narrative_ontology:cs_axiom('ea68389b-2542-4c60-9f54-c594e053fdd1', secondary, long_term_future_humanity_is_undifferentiated_victim).
narrative_ontology:cs_axiom_status(long_term_future_humanity_is_undifferentiated_victim, holdable).
narrative_ontology:cs_axiom_grounding('ea68389b-2542-4c60-9f54-c594e053fdd1', long_term_future_humanity_is_undifferentiated_victim, deontological).
narrative_ontology:cs_reference_frame('ea68389b-2542-4c60-9f54-c594e053fdd1', human_extinction_avoidance_imperative).
narrative_ontology:cs_drift_state('ea68389b-2542-4c60-9f54-c594e053fdd1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ea68389b-2542-4c60-9f54-c594e053fdd1', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_day_humanity).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, near_term_harm_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, ai_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These researchers define the problem of catastrophic AI risk, propose solutions, and advocate for resources and policy focus. They benefit from the prioritization of their research agenda and the associated funding and influence.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    institutional, generational, analytical, global).

% The ultimate beneficiary of preventing existential catastrophe, this abstract entity represents all future generations whose existence is posited to depend on successful AI alignment. They have no agency in the present.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Bears the costs of diverted resources, talent, and attention from addressing immediate societal problems (e.g., climate change, poverty, present-day AI harms) towards speculative future risks. Their agency is diffuse and constrained by the dominant narrative.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_day_humanity, payer,
    moderate, biographical, constrained, global).

% Advocate for addressing immediate harms from AI, such as bias, discrimination, and job displacement. They find their concerns deprioritized and resources diverted by the dominant existential risk narrative, often feeling excluded from central policy discussions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, near_term_harm_advocates, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_alignment_priority__existential_risk_reading, near_term_harm_advocates, excluded).

% Face increasing pressure and regulation to incorporate alignment and safety measures, potentially slowing innovation or increasing development costs, driven by the existential risk narrative. Their ability to pursue capability-focused research without safety constraints is reduced.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_developers, payer,
    powerful, biographical, constrained, global).

% Seek to bridge the gap between existential risk and near-term harms, advocating for a holistic approach. They observe the tension and attempt to integrate different perspectives, but often struggle to shift the dominant prioritization.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_approach_advocates, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:fixing_cost_class(ai_alignment_priority__existential_risk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global research, policy, and development efforts to prevent catastrophic loss of control over advanced AI systems, thereby ensuring the long-term survival of humanity.
% TRANSFER_FUNCTION: Transfers significant intellectual, financial, and political resources from addressing present-day AI harms and other societal issues towards speculative, long-term AI safety research and governance.
% ABSENT_VOICES: Marginalized communities disproportionately affected by current AI systems, advocates for immediate social justice, and those who prioritize tangible present-day well-being over speculative future risks are often sidelined or unheard in the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the priority on existential AI risk vanished, research funding, policy focus, and public discourse around AI would dramatically shift towards present-day applications, ethical concerns, and economic impacts. This would leave long-term, speculative risks largely unaddressed, fundamentally reorganizing the AI governance landscape.
% FOUNDING_PROBLEM: The hypothetical future development of superintelligent AI systems that, if misaligned with human values, could autonomously cause an irreversible, existential catastrophe for humanity.
% FOUNDING_PROBLEM_CORROBORATION: Primarily attested by existential risk researchers, prominent AI figures, and affiliated institutions. However, this problem's 'liveness' is contested by near-term harm advocates and some ethicists, who argue it is speculative, distracts from present issues, or is a form of 'longtermism' that devalues current suffering. Corroboration from outside the benefiting parties is limited and often framed as skepticism.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.80) stems from the significant resources (funding, talent, policy attention) diverted towards a highly speculative, long-term problem, often at the expense of addressing present-day AI harms or other societal challenges. Suppression (0.70) is evident in the way alternative ethical frameworks or research priorities are marginalized or dismissed as less urgent. The theater ratio is low (0.10) because the concern, while speculative, is genuinely held by its proponents, and the activities undertaken are considered functional towards its stated goal. The claimed type of 'tangled_rope' reflects the genuine coordination function (preventing global catastrophe) coupled with the asymmetric extraction from present-day concerns and the active enforcement of this specific priority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existential risk researchers, this constraint is a vital 'rope' for humanity's survival, coordinating essential efforts. However, from the perspective of near-term harm advocates or present-day humanity, it operates as a 'snare' or 'tangled_rope', extracting resources and attention from immediate, tangible problems based on speculative future scenarios. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers are clear beneficiaries, gaining influence, funding, and status by defining and leading this agenda. Long-term future humanity is the abstract beneficiary. Present-day humanity and near-term harm advocates are victims, as resources and attention are extracted from their immediate concerns. AI developers are also payers, facing constraints and regulations driven by this priority. The directionality for these groups reflects their structural position relative to the flow of benefits and costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_imminent_risk_priority,
    'Is the existential risk from advanced AI sufficiently imminent and certain to warrant the current level of resource diversion from present-day harms?',
    'Development of more robust, empirically grounded timelines for advanced AI capabilities and a clearer, less speculative causal chain for catastrophic outcomes, or a re-evaluation of the ethical weight given to speculative future harms versus certain present harms.',
    'If the risk is found to be less imminent or more speculative than currently asserted, the extractiveness and suppression metrics would decrease, potentially reclassifying the constraint towards a ''rope'' or ''piton'' as its justification weakens. If found to be more imminent, the current metrics would be further validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_vs_imminent_risk_priority, empirical, 'Uncertainty regarding the timeline and certainty of existential AI risk.').

omega_variable(
    resource_allocation_efficiency_for_existential_risk,
    'Are the resources currently allocated to existential AI risk research and governance genuinely effective in mitigating the hypothesized threat, or are they being inefficiently deployed?',
    'Independent audits of research efficacy, impact assessments of policy interventions, and comparative analysis with other high-stakes, long-term risk mitigation efforts (e.g., climate change, pandemic preparedness).',
    'If resources are found to be inefficiently deployed, the ''theater_ratio'' would increase, and the ''extractiveness'' might be re-evaluated as less functional, pushing the constraint towards a ''piton'' or ''snare'' if the coordination function is deemed performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency_for_existential_risk, empirical, 'Effectiveness of current resource allocation in addressing existential AI risk.').

omega_variable(
    framing_underdetermination_ai_alignment,
    'Is the ''existential_risk_reading'' the most defensible framing for AI alignment, or does the ''integrated_reading'' or ''nearterm_harms_reading'' offer a more comprehensive and ethically sound approach?',
    'A shift in the dominant discourse and funding priorities towards an integrated or near-term harms approach, or a consensus among diverse ethical and technical communities on the optimal balance of priorities.',
    'If an alternative framing gains dominance, this constraint would be reclassified, likely with lower extractiveness and suppression, as its core premise would be superseded or integrated into a broader framework. This would fundamentally alter the beneficiary/victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_ai_alignment, conceptual, 'Ambiguity in the foundational framing of AI alignment priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_alignment_priority__existential_risk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_alignment_priority__existential_risk_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(ai_a_tr_t10, ai_alignment_priority__existential_risk_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ai_a_tr_t15, ai_alignment_priority__existential_risk_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ai_a_tr_t20, ai_alignment_priority__existential_risk_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_alignment_priority__existential_risk_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(ai_a_be_t5, ai_alignment_priority__existential_risk_reading, base_extractiveness, 5, 0.77).
narrative_ontology:measurement(ai_a_be_t10, ai_alignment_priority__existential_risk_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(ai_a_be_t15, ai_alignment_priority__existential_risk_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(ai_a_be_t20, ai_alignment_priority__existential_risk_reading, base_extractiveness, 20, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t0, ai_alignment_priority__existential_risk_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(ai_a_su_t5, ai_alignment_priority__existential_risk_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(ai_a_su_t10, ai_alignment_priority__existential_risk_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(ai_a_su_t15, ai_alignment_priority__existential_risk_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_a_su_t20, ai_alignment_priority__existential_risk_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, global_infrastructure).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_research_funding_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_ethics_framework_development).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_safety_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_alignment_priority' kernel, focusing on existential risk. It is structurally distinct from the 'nearterm_harms_reading' and 'integrated_reading' due to differing ε values and stakeholder impacts, but all are part of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
