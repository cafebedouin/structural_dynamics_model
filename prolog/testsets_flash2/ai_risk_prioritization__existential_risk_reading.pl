% ============================================================================
% CONSTRAINT STORY: ai_risk_prioritization__existential_risk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_prioritization__existential_risk_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_prioritization__existential_risk_reading
 *   human_readable: AI Risk Prioritization: Existential Risk Reading
 *   domain: AI Safety/Technology Governance/Risk Assessment
 *
 * SUMMARY:
 *   This constraint story represents the 'existential risk' reading of AI
 *   risk prioritization, where the primary concern is misaligned AGI posing
 *   an extinction-level threat, and alignment research is paramount. This
 *   reading emphasizes long-term, speculative risks over immediate, tangible
 *   harms. It is one reading of the 'ai_risk_prioritization' kernel, distinct
 *   from the 'near_term_harms_reading'.
 *
 * KEY AGENTS:
 *   - x_risk_research_institutions: Agenda setter (institutional/constrained)
 *   - longtermist_funders: Beneficiary (powerful/mobile)
 *   - future_humanity: Payer (powerless/trapped)
 *   - near_term_ai_harms_advocates: Payer (organized/constrained)
 *   - ai_developers_agi_focused: Beneficiary (powerful/constrained)
 *   - policy_makers: Agenda setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, 0.65).
domain_priors:suppression_score(ai_risk_prioritization__existential_risk_reading, 0.7).
domain_priors:theater_ratio(ai_risk_prioritization__existential_risk_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_risk_prioritization__existential_risk_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_prioritization__existential_risk_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_prioritization__existential_risk_reading, "AI Risk Prioritization: Existential Risk Reading").
narrative_ontology:topic_domain(ai_risk_prioritization__existential_risk_reading, "AI Safety/Technology Governance/Risk Assessment").

domain_priors:requires_active_enforcement(ai_risk_prioritization__existential_risk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_prioritization__existential_risk_reading, '00b29c26-4063-453d-b395-ed539f92107c').
narrative_ontology:cs_kernel_codification('00b29c26-4063-453d-b395-ed539f92107c', distributed).
narrative_ontology:cs_authority_grounding('00b29c26-4063-453d-b395-ed539f92107c', expertise).
narrative_ontology:cs_interpretation_layer_present('00b29c26-4063-453d-b395-ed539f92107c').
narrative_ontology:cs_reading_relation('00b29c26-4063-453d-b395-ed539f92107c', ai_risk_prioritization__near_term_harms_reading, influences).
narrative_ontology:cs_axiom('00b29c26-4063-453d-b395-ed539f92107c', foundational, agi_poses_existential_threat).
narrative_ontology:cs_axiom_status(agi_poses_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('00b29c26-4063-453d-b395-ed539f92107c', agi_poses_existential_threat, empirically_contingent).
narrative_ontology:cs_axiom('00b29c26-4063-453d-b395-ed539f92107c', foundational, longterm_future_value_maximization).
narrative_ontology:cs_axiom_status(longterm_future_value_maximization, holdable).
narrative_ontology:cs_axiom_grounding('00b29c26-4063-453d-b395-ed539f92107c', longterm_future_value_maximization, deontological).
narrative_ontology:cs_reference_frame('00b29c26-4063-453d-b395-ed539f92107c', agi_x_risk_as_primary_concern).
narrative_ontology:cs_drift_state('00b29c26-4063-453d-b395-ed539f92107c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('00b29c26-4063-453d-b395-ed539f92107c', '').
narrative_ontology:cs_kernel_id(ai_risk_prioritization__existential_risk_reading, ai_risk_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, longtermist_funders).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, future_humanity).
narrative_ontology:constraint_victim(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_prioritization__existential_risk_reading, ai_developers_agi_focused).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the problem of AI existential risk, conduct alignment research, and advocate for policy interventions focused on AGI safety. They receive significant funding and influence policy discourse based on this prioritization.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, x_risk_research_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Philanthropic organizations and individuals who prioritize long-term future welfare, directing substantial resources towards AGI alignment and x-risk mitigation, thereby reinforcing the focus on existential threats.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, longtermist_funders, beneficiary,
    powerful, civilizational, mobile, global).

% The ultimate 'victim' of unaligned AGI, bearing the potential extinction-level consequences. Their interests are represented by x-risk advocates, but they have no direct agency or exit options.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Advocates for addressing immediate, tangible harms from AI systems (e.g., bias, job displacement, surveillance). Their concerns are often framed as secondary or distracting by the existential risk narrative, leading to resource diversion from their priorities.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, near_term_ai_harms_advocates, payer,
    organized, biographical, constrained, global).

% Developers of advanced AI systems, particularly those pursuing AGI. While they face scrutiny regarding safety, the existential risk narrative often justifies their pursuit of powerful systems, provided they also invest in alignment research.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, ai_developers_agi_focused, beneficiary,
    powerful, immediate, constrained, global).

% Government officials and international bodies tasked with regulating AI. They are influenced by the existential risk narrative to prioritize AGI safety and capability controls, potentially at the expense of addressing immediate societal impacts.
narrative_ontology:constraint_stakeholder(ai_risk_prioritization__existential_risk_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts and resources towards a singular, long-term goal of preventing human extinction from misaligned AGI, by focusing research and policy on alignment and control.
% TRANSFER_FUNCTION: Transfers intellectual and financial resources from addressing near-term, tangible AI harms to speculative, long-term existential risks, primarily from general artificial intelligence.
% ABSENT_VOICES: Future generations, who are the primary 'victims' but cannot speak for themselves. Their interests are mediated through current advocates, whose interpretations may be contested. Also, those directly experiencing current AI harms, whose voices are often marginalized in this prioritization framework.
% DISAPPEARANCE_RATIONALE: If the existential risk prioritization vanished, the AI safety field would fragment, resources would likely shift towards addressing immediate harms, and the long-term trajectory of AI development might proceed with less emphasis on AGI alignment, leading to a different set of risks and opportunities.
% FOUNDING_PROBLEM: The theoretical possibility of superintelligent AI systems developing goals misaligned with human values, leading to an uncontrollable and potentially catastrophic outcome for humanity.
% FOUNDING_PROBLEM_CORROBORATION: The problem's status is attested as 'live' by a significant portion of the AI research community, prominent public intellectuals, and a growing number of policymakers, who cite theoretical arguments and accelerating AI capabilities. Critics, however, argue the problem is speculative and distracts from present dangers.
narrative_ontology:disappearance_verdict(ai_risk_prioritization__existential_risk_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_prioritization__existential_risk_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_prioritization__existential_risk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ai_risk_prioritization__existential_risk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_prioritization__existential_risk_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_prioritization__existential_risk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_prioritization__existential_risk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_prioritization__existential_risk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because resources are diverted from addressing present, measurable harms to a speculative, future threat, benefiting a specific research and funding ecosystem. Suppression (0.70) is significant as alternative risk framings (e.g., near-term harms) are actively marginalized or dismissed as distractions. The theater ratio (0.20) is moderate; while genuine research is conducted, some activities may serve to maintain the narrative's dominance and funding streams. The claimed type is 'tangled_rope' because it genuinely coordinates a global research effort (benefiting future humanity and AGI developers) but does so with significant asymmetric extraction from those concerned with near-term harms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of x-risk research institutions and longtermist funders, this is a crucial 'rope' coordinating humanity's defense against an existential threat. From the perspective of near-term harms advocates, it functions as a 'snare' that extracts resources and attention from pressing issues, while 'future humanity' is a diffuse victim whose interests are interpreted by the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   X-risk research institutions and longtermist funders are clear beneficiaries (low d) as they define the agenda and receive resources. AGI developers also benefit from the focus on long-term safety, which can legitimize their pursuit of powerful systems. Future humanity is the ultimate target/victim (high d), as they bear the risk. Near-term harms advocates are also targets (high d) because their priorities are suppressed and resources diverted.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling a potentially extractive prioritization as pure coordination. By identifying beneficiaries and victims, and tracking the rising extractiveness and suppression, it highlights how a legitimate concern (AI safety) can become a 'tangled rope' if the coordination function is coupled with asymmetric resource allocation and suppression of alternative perspectives. The 'live' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, suggests the constraint is not yet a piton, but the rising extractiveness indicates a drift towards a more extractive form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculative_vs_empirical_risk,
    'Is the existential threat from AGI a sufficiently well-defined and empirically grounded risk to justify its current prioritization over demonstrable near-term harms?',
    'Development of more robust, falsifiable models of AGI risk, or a shift in AI capabilities that makes the threat more immediate and less speculative. Conversely, if near-term harms escalate dramatically without AGI emergence, the empirical balance shifts.',
    'If the existential threat remains highly speculative, the extractiveness of this prioritization (diverting resources) would be reclassified higher, potentially shifting the constraint towards a Snare. If the threat becomes more concrete, the coordination function would be more strongly validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_vs_empirical_risk, empirical, 'Uncertainty regarding the empirical basis and immediacy of AGI existential risk.').

omega_variable(
    resource_allocation_efficiency,
    'Are the resources allocated to AGI alignment research genuinely effective in mitigating existential risk, or are they primarily serving to sustain a particular research ecosystem?',
    'Independent audits of alignment research efficacy, clear metrics for progress towards alignment, and comparative analysis of impact per dollar spent across different AI safety approaches.',
    'If resources are found to be inefficiently used or primarily self-serving, the theater_ratio would increase, and the extractiveness would be re-evaluated upwards, potentially pushing the constraint closer to a Piton or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Uncertainty about the effectiveness and true beneficiaries of current alignment research funding.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of near-term harms advocacy structural (e.g., funding mechanisms, media gatekeeping) or internalized (e.g., self-censorship by researchers fearing being seen as ''distracting'')?',
    'Post-exit suppression trajectory: if near-term harms advocates gain significant traction and funding after the existential risk narrative is de-emphasized, it suggests structural suppression. If marginalization persists, it points to internalized factors.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external challenge. If purely structural, removing the external barriers would have a more immediate impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative AI risk framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_prioritization__existential_risk_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t2015, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement_basis(ai_r_tr_t2015, observed).
narrative_ontology:measurement(ai_r_tr_t2020, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement_basis(ai_r_tr_t2020, observed).
narrative_ontology:measurement(ai_r_tr_t2025, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement_basis(ai_r_tr_t2025, observed).
narrative_ontology:measurement(ai_r_tr_t2030, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2030, 0.25).
narrative_ontology:measurement_basis(ai_r_tr_t2030, projected).
narrative_ontology:measurement(ai_r_tr_t2035, ai_risk_prioritization__existential_risk_reading, theater_ratio, 2035, 0.28).
narrative_ontology:measurement_basis(ai_r_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t2015, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement_basis(ai_r_be_t2015, observed).
narrative_ontology:measurement(ai_r_be_t2020, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(ai_r_be_t2020, observed).
narrative_ontology:measurement(ai_r_be_t2025, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(ai_r_be_t2025, observed).
narrative_ontology:measurement(ai_r_be_t2030, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2030, 0.7).
narrative_ontology:measurement_basis(ai_r_be_t2030, projected).
narrative_ontology:measurement(ai_r_be_t2035, ai_risk_prioritization__existential_risk_reading, base_extractiveness, 2035, 0.72).
narrative_ontology:measurement_basis(ai_r_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t2015, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement_basis(ai_r_su_t2015, observed).
narrative_ontology:measurement(ai_r_su_t2020, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(ai_r_su_t2020, observed).
narrative_ontology:measurement(ai_r_su_t2025, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(ai_r_su_t2025, observed).
narrative_ontology:measurement(ai_r_su_t2030, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement_basis(ai_r_su_t2030, projected).
narrative_ontology:measurement(ai_r_su_t2035, ai_risk_prioritization__existential_risk_reading, suppression_requirement, 2035, 0.78).
narrative_ontology:measurement_basis(ai_r_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_prioritization__existential_risk_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_governance_frameworks).
narrative_ontology:affects_constraint(ai_risk_prioritization__existential_risk_reading, ai_research_funding_priorities).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ai_risk_prioritization' kernel. Its sibling, 'near_term_harms_reading', focuses on immediate AI impacts. Both are distinct but related constraints within the broader AI governance domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
