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
 *   constraint_id: ai_alignment_priority__existential_risk_reading
 *   human_readable: AI Alignment: Existential Risk Priority
 *   domain: AI Governance / Technology Ethics / Risk Assessment
 *
 * SUMMARY:
 *   This constraint represents the framing of AI alignment as primarily a
 *   problem of preventing catastrophic loss of control over advanced AI
 *   systems, prioritizing existential safety above other concerns. It is one
 *   reading of the broader 'AI alignment priority' kernel. This reading
 *   emphasizes the speculative, long-term risks of AGI/ASI, often leading to
 *   a diversion of resources and attention from more immediate, demonstrable
 *   harms of AI. The high extractiveness reflects the opportunity cost of
 *   this singular focus, while suppression indicates the marginalization of
 *   alternative framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_alignment_priority__existential_risk_reading, 0.82).
domain_priors:suppression_score(ai_alignment_priority__existential_risk_reading, 0.75).
domain_priors:theater_ratio(ai_alignment_priority__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(ai_alignment_priority__existential_risk_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_alignment_priority__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_alignment_priority__existential_risk_reading, "AI Alignment: Existential Risk Priority").
narrative_ontology:topic_domain(ai_alignment_priority__existential_risk_reading, "AI Governance / Technology Ethics / Risk Assessment").

domain_priors:requires_active_enforcement(ai_alignment_priority__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_alignment_priority__existential_risk_reading, '17cfc94f-b5cc-4996-8946-163d2953cead').
narrative_ontology:cs_kernel_codification('17cfc94f-b5cc-4996-8946-163d2953cead', implicit).
narrative_ontology:cs_authority_grounding('17cfc94f-b5cc-4996-8946-163d2953cead', expertise).
narrative_ontology:cs_interpretation_layer_present('17cfc94f-b5cc-4996-8946-163d2953cead').
narrative_ontology:cs_reading_relation('17cfc94f-b5cc-4996-8946-163d2953cead', ai_alignment_priority__nearterm_harms_reading, influences).
narrative_ontology:cs_reading_relation('17cfc94f-b5cc-4996-8946-163d2953cead', ai_alignment_priority__integrated_reading, coexists_with).
narrative_ontology:cs_axiom('17cfc94f-b5cc-4996-8946-163d2953cead', foundational, existential_risk_is_primary).
narrative_ontology:cs_axiom_status(existential_risk_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('17cfc94f-b5cc-4996-8946-163d2953cead', existential_risk_is_primary, deontological).
narrative_ontology:cs_axiom('17cfc94f-b5cc-4996-8946-163d2953cead', foundational, loss_of_control_is_catastrophic).
narrative_ontology:cs_axiom_status(loss_of_control_is_catastrophic, holdable).
narrative_ontology:cs_axiom_grounding('17cfc94f-b5cc-4996-8946-163d2953cead', loss_of_control_is_catastrophic, empirically_contingent).
narrative_ontology:cs_reference_frame('17cfc94f-b5cc-4996-8946-163d2953cead', pre_agi_existential_threat_awareness).
narrative_ontology:cs_drift_state('17cfc94f-b5cc-4996-8946-163d2953cead', contemporary_agi_development, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('17cfc94f-b5cc-4996-8946-163d2953cead', '').
narrative_ontology:cs_kernel_id(ai_alignment_priority__existential_risk_reading, ai_alignment_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, existential_risk_researchers).
narrative_ontology:constraint_beneficiary(ai_alignment_priority__existential_risk_reading, ai_safety_funders).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, nearterm_harms_advocates).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, present_day_marginalized_communities).
narrative_ontology:constraint_victim(ai_alignment_priority__existential_risk_reading, ai_developers_without_alignment_focus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These researchers define the problem of existential AI risk, develop mitigation strategies, and advocate for this priority in policy and funding. They benefit from dedicated funding streams and influence over the discourse, but are constrained by the speculative nature of their field.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, existential_risk_researchers, agenda_setter,
    organized, generational, constrained, global).

% Philanthropic organizations and government agencies that direct significant resources towards AI existential risk research. They shape the research agenda and benefit from the perceived urgency of the problem, with high flexibility in their funding decisions.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_safety_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% The abstract beneficiary of preventing existential catastrophe. This entity has no agency but is the ultimate object of protection, represented by current advocates.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, long_term_future_humanity, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ai_alignment_priority__existential_risk_reading, long_term_future_humanity).

% Advocates for addressing present-day harms of AI (e.g., bias, discrimination, labor displacement). Their concerns are often deprioritized or framed as secondary to existential risk, leading to reduced funding and influence, making their exit from the dominant discourse difficult.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, nearterm_harms_advocates, excluded,
    organized, biographical, constrained, global).

% Indirectly bear the costs of resource diversion from near-term AI harms. They experience the direct negative impacts of deployed AI systems without sufficient attention or funding directed towards their mitigation, with no viable exit from these systems.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, present_day_marginalized_communities, payer,
    powerless, immediate, trapped, local).

% Developers focused on general AI capabilities or applications without a primary focus on existential alignment. They face increasing pressure to integrate alignment considerations, potential funding biases, and reputational risks if not seen as contributing to 'safe' AI, limiting their independent development paths.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, ai_developers_without_alignment_focus, payer,
    moderate, biographical, constrained, global).

% Advocates who seek to bridge the gap between existential and near-term AI safety concerns, promoting a holistic approach. They observe the tensions and resource allocation dynamics, attempting to influence the discourse towards a more balanced perspective.
narrative_ontology:constraint_stakeholder(ai_alignment_priority__existential_risk_reading, integrated_alignment_advocates, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global research agenda and public discourse around the singular, long-term, high-impact risk of advanced AI systems (AGI/ASI) posing an existential threat to humanity, aiming to prevent catastrophic loss of control.
% TRANSFER_FUNCTION: Transfers significant intellectual, financial, and political resources from addressing present-day AI harms and general AI development to speculative, long-term existential risk mitigation research and advocacy.
% ABSENT_VOICES: Advocates for near-term AI harms, marginalized communities disproportionately affected by current AI systems, and those who believe AI risk is primarily a social/political problem rather than a purely technical one. Their concerns are often framed as secondary or distractions from the 'real' problem of existential safety.
% DISAPPEARANCE_RATIONALE: If this priority vanished overnight, the AI safety discourse would immediately reorient, funding would shift towards near-term harms or general capability development, and the focus on AGI control would diminish, leading to a fundamentally different trajectory for AI development and governance.
% FOUNDING_PROBLEM: The perceived inevitability of advanced AI systems (AGI/ASI) posing an existential threat to humanity if not properly aligned with human values, leading to loss of control, unintended consequences, or human disempowerment.
% FOUNDING_PROBLEM_CORROBORATION: Primarily attested by a subset of AI researchers and public intellectuals who have popularized the concept. Critics (e.g., social scientists, ethicists focused on present harms) dispute the immediacy and scope of the problem, arguing it is speculative, a distraction from present issues, or misrepresents the nature of AI risk. Corroboration from outside the benefiting parties is limited and often critical.
narrative_ontology:disappearance_verdict(ai_alignment_priority__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_alignment_priority__existential_risk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_alignment_priority__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_alignment_priority__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_alignment_priority__existential_risk_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.82) because this framing effectively captures significant resources (funding, talent, public attention) for a specific, speculative future problem, at the expense of other pressing AI-related issues. Suppression (0.75) is also high, as alternative framings (e.g., near-term harms, social justice in AI) are often dismissed or deprioritized within the dominant discourse, limiting their ability to gain traction or funding. The theater ratio is low (0.20) because the concern for existential risk is genuinely held by its proponents, though some activities might be performative in their urgency. The increasing extractiveness and suppression over time reflect the growing dominance of this framing in the AI safety discourse.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading perceive it as a necessary, urgent coordination effort to save humanity. Critics, however, see it as an extractive mechanism that diverts attention and resources from real, present harms, and suppresses alternative, more equitable approaches to AI governance. The engine's classification as a Tangled Rope captures this dual nature: a genuine coordination function (focusing on a specific future risk) coupled with asymmetric extraction (from other concerns and communities).
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers and funders are clear beneficiaries, as this framing directs resources and legitimacy towards their work. Long-term future humanity is an abstract beneficiary. Near-term harms advocates and present-day marginalized communities are victims, as their concerns are sidelined and resources diverted. AI developers without an alignment focus are payers, facing pressure to conform to this priority. The structural asymmetry is clear: a specific group benefits from defining the problem in a particular way, while others bear the costs of that definition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_demonstrable_harms,
    'Is the prioritization of speculative, future existential risk over demonstrable, present-day harms justified by the probability and magnitude of the former?',
    'Development of robust, empirically-grounded risk assessment methodologies for advanced AI, and comparative analysis of the social and economic costs of present harms versus projected future catastrophes.',
    'If future risks are found to be less probable or manageable than currently asserted, the extractiveness of this constraint would decrease, and its suppression of near-term concerns would be less justified, potentially reclassifying it towards a Snare if the coordination function is deemed insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_vs_demonstrable_harms, empirical, 'The balance between speculative future risks and demonstrable present harms.').

omega_variable(
    framing_as_coordination_vs_extraction,
    'Is the ''existential safety'' framing a genuine coordination mechanism for a shared human future, or primarily a rhetorical device to extract resources and attention for a specific research agenda?',
    'Analysis of resource allocation patterns, stakeholder influence, and the responsiveness of the discourse to critiques from marginalized groups. If resource capture and exclusion persist despite evidence of present harms, the extractive component is dominant.',
    'If primarily extractive, the constraint''s classification would shift closer to a Snare, highlighting the coercive nature of its persistence. If genuinely coordinative, its Rope-like qualities would be emphasized, despite the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_as_coordination_vs_extraction, conceptual, 'The true function of the existential risk framing.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative AI safety framings structural (e.g., funding mechanisms, academic gatekeeping) or internalized (e.g., researchers self-censoring due to perceived urgency)?',
    'Post-funding-shift discourse trajectory: if alternative framings gain traction and resources after structural barriers are removed, suppression was primarily structural. If the community continues to prioritize existential risk even with open funding, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the community carries the suppression with them, making it harder to shift priorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_alignment_priority__existential_risk_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2015, ai_alignment_priority__existential_risk_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(ai_a_tr_t2017, ai_alignment_priority__existential_risk_reading, theater_ratio, 2017, 0.17).
narrative_ontology:measurement(ai_a_tr_t2019, ai_alignment_priority__existential_risk_reading, theater_ratio, 2019, 0.19).
narrative_ontology:measurement(ai_a_tr_t2021, ai_alignment_priority__existential_risk_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(ai_a_tr_t2023, ai_alignment_priority__existential_risk_reading, theater_ratio, 2023, 0.2).
narrative_ontology:measurement(ai_a_tr_t2025, ai_alignment_priority__existential_risk_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2015, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(ai_a_be_t2017, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2017, 0.68).
narrative_ontology:measurement(ai_a_be_t2019, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(ai_a_be_t2021, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2021, 0.79).
narrative_ontology:measurement(ai_a_be_t2023, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2023, 0.81).
narrative_ontology:measurement(ai_a_be_t2025, ai_alignment_priority__existential_risk_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ai_a_su_t2015, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(ai_a_su_t2017, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2017, 0.62).
narrative_ontology:measurement(ai_a_su_t2019, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(ai_a_su_t2021, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement(ai_a_su_t2023, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2023, 0.74).
narrative_ontology:measurement(ai_a_su_t2025, ai_alignment_priority__existential_risk_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_alignment_priority__existential_risk_reading, identity_coordination).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_research_funding_allocation).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_ethics_discourse_framing).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__nearterm_harms_reading).
narrative_ontology:affects_constraint(ai_alignment_priority__existential_risk_reading, ai_alignment_priority__integrated_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'existential_risk_reading' of the 'ai_alignment_priority' kernel. It focuses on preventing catastrophic loss of control over advanced AI systems, prioritizing existential safety. Its ε value is high due to the opportunity cost and resource diversion from other AI safety concerns. It is linked to sibling readings that offer alternative framings of AI alignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
