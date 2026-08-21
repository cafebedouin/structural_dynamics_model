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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Preventing Near-Term Harms
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents the 'near-term harms' reading of AI safety,
 *   which defines safety primarily as preventing documented present-day harms
 *   from deployed systems (e.g., bias, discrimination, labor exploitation,
 *   misinformation). This reading emphasizes accountability for existing
 *   systems and tangible, measurable impacts on human populations. It is one
 *   of several competing interpretations of 'AI safety' (the kernel), each
 *   with different beneficiaries, victims, and policy implications.
 *
 * KEY AGENTS:
 *   - marginalized_populations: Primary victims (powerless/trapped) — bear direct harms.
 *   - gig_workers: Victims (moderate/constrained) — subject to algorithmic exploitation.
 *   - communities_facing_algorithmic_discrimination: Victims (organized/constrained) — organize to resist systemic harms.
 *   - tech_companies_avoiding_speculative_regulation: Primary beneficiaries (institutional/arbitrage) — benefit from focus on remediable harms.
 *   - researchers_focused_on_applied_ethics: Beneficiaries (powerful/mobile) — gain resources and influence.
 *   - policy_makers_and_regulators: Agenda-setters (institutional/constrained) — develop and enforce regulations.
 *   - existential_risk_advocates: Excluded (organized/constrained) — marginalized in policy discussions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.78).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Preventing Near-Term Harms").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'a137bfb2-1af6-4ca8-bc42-8b96461e0ecb').
narrative_ontology:cs_kernel_codification('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', distributed).
narrative_ontology:cs_authority_grounding('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', practice).
narrative_ontology:cs_interpretation_layer_present('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb').
narrative_ontology:cs_reading_relation('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', foundational, safety_is_empirical_harm_prevention).
narrative_ontology:cs_axiom_status(safety_is_empirical_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', safety_is_empirical_harm_prevention, empirically_contingent).
narrative_ontology:cs_axiom('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', foundational, accountability_for_deployed_systems).
narrative_ontology:cs_axiom_status(accountability_for_deployed_systems, holdable).
narrative_ontology:cs_axiom_grounding('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', accountability_for_deployed_systems, deontological).
narrative_ontology:cs_reference_frame('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', applied_ethics_and_human_rights_framework).
narrative_ontology:cs_drift_state('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a137bfb2-1af6-4ca8-bc42-8b96461e0ecb', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies_avoiding_speculative_regulation).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, researchers_focused_on_applied_ethics).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of algorithmic bias, discrimination, and surveillance. Their ability to exit or resist is severely constrained by systemic inequalities and the pervasive nature of AI systems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Subject to algorithmic management, wage suppression, and precarious employment conditions mediated by AI systems. Their exit options are limited by economic necessity and lack of alternative employment.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    moderate, immediate, constrained, local).

% Experience systemic harms from AI in areas like credit, housing, and criminal justice. They organize to resist but face powerful institutional actors and complex technical barriers to redress.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, communities_facing_algorithmic_discrimination, payer,
    organized, generational, constrained, national).

% Benefit from a focus on present-day, remediable harms, which often allows them to avoid more stringent, speculative regulations related to advanced AI capabilities. They can frame their existing compliance efforts as 'AI safety'.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies_avoiding_speculative_regulation, beneficiary,
    institutional, generational, arbitrage, global).

% Their work on bias detection, fairness metrics, and explainable AI is directly relevant and funded under this framing of AI safety. They gain influence and resources by addressing concrete, observable problems.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, researchers_focused_on_applied_ethics, beneficiary,
    powerful, biographical, mobile, global).

% Are tasked with developing and enforcing regulations to mitigate AI harms. They navigate political pressures from industry and advocacy groups, often prioritizing tangible, measurable harms over speculative risks.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, policy_makers_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Argue that focusing solely on near-term harms distracts from catastrophic, long-term risks. They are often marginalized in policy discussions dominated by immediate concerns, finding their priorities deprioritized or unfunded.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_advocates, excluded,
    organized, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates efforts to identify, measure, and mitigate specific, documented harms arising from deployed AI systems, providing a common framework for accountability and intervention.
% TRANSFER_FUNCTION: Transfers resources (funding, regulatory attention, research focus) towards addressing present-day harms, and implicitly transfers the burden of speculative future risks away from current regulatory frameworks.
% ABSENT_VOICES: Advocates for existential risk are often excluded from the primary policy-making tables, finding their concerns dismissed as speculative or distracting from 'real' problems. Their perspective would shift the focus to long-term alignment and control.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, the existing (albeit imperfect) mechanisms for addressing algorithmic bias, discrimination, and labor exploitation would lose their primary framing and funding. Tech companies would face less pressure to remediate these issues, and affected communities would lose a key lever for advocacy, leading to a significant rearrangement of accountability structures.
% FOUNDING_PROBLEM: The rapid deployment of AI systems led to documented instances of algorithmic bias, discrimination, privacy violations, and labor exploitation, causing real-world harm to individuals and communities.
% FOUNDING_PROBLEM_CORROBORATION: Numerous academic studies, investigative journalism reports, and civil society organizations (e.g., AI Now Institute, Algorithmic Justice League) consistently document the ongoing nature of these harms, corroborating the problem's live status from outside the direct beneficiaries of this framing.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.78) is high because the focus on 'near-term harms' often leads to regulatory frameworks that are reactive and allow tech companies to continue deploying systems with known issues, only addressing them post-hoc or with minimal penalties. Suppression (0.65) is present as affected communities face significant barriers to legal recourse and technical understanding, while industry lobbying can suppress more stringent regulations. Theater ratio (0.40) reflects that while some efforts are genuine, a substantial portion of 'AI safety' initiatives under this framing serve to manage public perception and preempt broader, more impactful regulation. The metrics show a trend of increasing extractiveness and theatricality over time, suggesting a drift towards performative compliance rather than fundamental change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of affected communities, this framing is a Tangled Rope, offering some coordination (e.g., standards for bias auditing) but primarily extracting by allowing harmful systems to persist with insufficient accountability. From the perspective of tech companies, it's a Rope, providing a manageable framework for 'safety' that avoids more disruptive interventions. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized populations, gig workers, and communities facing algorithmic discrimination are clear targets (high d) as they bear the direct costs. Tech companies and applied ethics researchers are beneficiaries (low d) as this framing aligns with their interests (avoiding more radical regulation, funding for their specific research). Policy makers are agenda-setters, balancing competing interests. Existential risk advocates are excluded, as their concerns are not prioritized by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This framing prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of identifying and mitigating specific harms. However, it risks mislabeling extraction as coordination by allowing tech companies to define the scope of 'safety' in a way that minimizes their accountability for systemic issues, potentially leading to a Piton if the 'safety' efforts become purely performative without addressing root causes. The 'contested' status of the founding problem (solved vs. live) is key here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_harm_definition,
    'Is the definition of ''near-term harms'' sufficiently broad to capture all relevant present-day harms, or does it implicitly exclude systemic or diffuse harms that are harder to quantify?',
    'Longitudinal studies tracking the emergence of new harm categories not covered by existing definitions, or comparative analysis with broader human rights frameworks.',
    'If the definition is too narrow, the constraint''s effective extractiveness is higher than measured, as significant harms remain unaddressed. If it''s sufficiently broad, the measured extractiveness is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_harm_definition, conceptual, 'Ambiguity in the scope of ''near-term harms'' and its impact on unaddressed harms.').

omega_variable(
    regulatory_capture_by_framing,
    'To what extent does the focus on ''near-term harms'' serve as a form of regulatory capture, allowing tech companies to shape the safety agenda to their advantage by diverting attention from more fundamental structural issues or speculative risks?',
    'Analysis of lobbying expenditures, policy outcomes, and the alignment of ''safety'' initiatives with industry interests versus public interest advocacy.',
    'If significant capture is present, the constraint functions more as a Snare for affected populations, as the coordination narrative is primarily cover for industry-benefiting extraction. If capture is minimal, it functions more as a genuine Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_framing, empirical, 'Whether the ''near-term harms'' framing is a form of regulatory capture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of legal avenues, technical complexity) or internalized (fatalism among affected communities, belief in technological inevitability)?',
    'Post-intervention suppression trajectory: if suppression persists after legal/technical barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — affected communities carry the suppression with them after barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for affected communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__near_term_harms_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, algorithmic_auditing_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_safety_commitment' kernel, focusing on present-day harms. It is linked to other readings (existential_risk_reading, dual_priority_reading) which offer alternative definitions of AI safety.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
