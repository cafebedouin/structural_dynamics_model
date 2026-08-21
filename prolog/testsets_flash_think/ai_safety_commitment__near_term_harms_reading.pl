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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: AI Safety Defined as Preventing Present-Day Harms (Limiting Scope)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents a specific reading of 'AI safety' that defines
 *   it primarily as preventing documented present-day harms from deployed
 *   systems (e.g., bias, discrimination, labor exploitation, misinformation).
 *   While ostensibly focused on protection, this framing functions as a
 *   constraint by limiting the scope of AI safety discourse and resource
 *   allocation, thereby benefiting tech companies who avoid broader, more
 *   costly, or speculative safety investments. The constraint is the
 *   *dominant framing* itself, which coordinates action around immediate
 *   harms but extracts by limiting overall responsibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.7).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety Defined as Preventing Present-Day Harms (Limiting Scope)").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, 'afcfb4e4-0364-4369-b874-c559026d537c').
narrative_ontology:cs_kernel_codification('afcfb4e4-0364-4369-b874-c559026d537c', distributed).
narrative_ontology:cs_authority_grounding('afcfb4e4-0364-4369-b874-c559026d537c', distributed).
narrative_ontology:cs_reading_relation('afcfb4e4-0364-4369-b874-c559026d537c', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('afcfb4e4-0364-4369-b874-c559026d537c', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('afcfb4e4-0364-4369-b874-c559026d537c', foundational, present_day_harms_are_primary_concern).
narrative_ontology:cs_axiom_status(present_day_harms_are_primary_concern, holdable).
narrative_ontology:cs_axiom_grounding('afcfb4e4-0364-4369-b874-c559026d537c', present_day_harms_are_primary_concern, empirically_contingent).
narrative_ontology:cs_axiom('afcfb4e4-0364-4369-b874-c559026d537c', foundational, accountability_for_deployed_systems).
narrative_ontology:cs_axiom_status(accountability_for_deployed_systems, holdable).
narrative_ontology:cs_axiom_grounding('afcfb4e4-0364-4369-b874-c559026d537c', accountability_for_deployed_systems, deontological).
narrative_ontology:cs_reference_frame('afcfb4e4-0364-4369-b874-c559026d537c', human_rights_and_social_justice_framework).
narrative_ontology:cs_drift_state('afcfb4e4-0364-4369-b874-c559026d537c', contemporary_ai_deployment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afcfb4e4-0364-4369-b874-c559026d537c', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, policymakers_seeking_tangible_wins).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, future_generations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_communities_affected_by_ai).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, ai_ethics_advocates).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and benefit from a definition of AI safety focused exclusively on documented present-day harms, as it allows them to avoid more speculative, costly, or fundamental safety investments and broader regulatory oversight. They shape the discourse and direct resources towards these limited concerns.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, tech_companies, beneficiary).

% Benefit from this framing as it allows them to focus on politically expedient, measurable problems with clear, immediate solutions, rather than complex, long-term, or speculative risks that are harder to legislate or demonstrate impact on. This provides a narrative of action and progress.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, policymakers_seeking_tangible_wins, beneficiary,
    organized, immediate, mobile, national).

% Bear the cost of their concerns about extinction-level outcomes from misaligned superintelligent systems being marginalized, underfunded, and dismissed as 'speculative' or 'long-term' by the dominant framing. Their research and advocacy efforts struggle for legitimacy and resources.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, payer,
    moderate, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded).

% Are the ultimate bearers of unaddressed long-term and systemic risks that are neglected by a narrow focus on present-day harms. They have no voice or agency in the current framing of AI safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, future_generations, payer,
    powerless, generational, trapped, universal).

% While this framing purports to address their harms (bias, discrimination, labor exploitation), they remain victims of these harms when the framing is used to limit broader systemic changes or to avoid accountability from tech companies. The focus on 'documented' harms can also exclude emerging or less visible forms of harm.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_communities_affected_by_ai, payer,
    powerless, biographical, trapped, local).

% Work to highlight and mitigate present-day harms, but also bear the cost of fighting against the narrowness of the dominant framing, which often prevents deeper, systemic interventions. They are caught between addressing immediate issues and pushing for a more comprehensive safety agenda.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, ai_ethics_advocates, observer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, ai_ethics_advocates, payer).

% Benefits from any genuine mitigation of present-day harms but pays the cost of unaddressed systemic risks and the potential for future, more severe harms that are not prioritized by this limited definition of AI safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, general_public, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates research, policy, and industry efforts around tangible, measurable, and documented harms from AI systems, providing a common language and focus for immediate action.
% TRANSFER_FUNCTION: Transfers regulatory leniency and reduced long-term responsibility to tech companies by diverting attention and resources away from broader, more speculative, or fundamental safety concerns. It also transfers the burden of unaddressed systemic risks to future generations and the general public.
% ABSENT_VOICES: Advocates for broader AI safety, including those focused on systemic risks, long-term societal impacts, and existential risks, are often sidelined or dismissed. Future generations, who would bear the brunt of unaddressed long-term risks, are entirely absent from the conversation.
% DISAPPEARANCE_RATIONALE: If this framing vanished overnight, the discourse around AI safety would immediately broaden, leading to a re-evaluation of priorities, funding, and regulatory approaches. Tech companies would face pressure for more comprehensive safety investments, and research into systemic and long-term risks would gain prominence. The current allocation of resources and responsibilities would be fundamentally reorganized.
% FOUNDING_PROBLEM: The initial problem was the emergence of demonstrable harms from deployed AI systems, such as algorithmic bias, discrimination, and the spread of misinformation, which required immediate attention and mitigation strategies.
% FOUNDING_PROBLEM_CORROBORATION: While tech companies and some policymakers attest that the problem is live and their efforts are addressing it, independent researchers and civil society organizations corroborate the existence of these harms but argue that the current framing is insufficient to address them systemically, often serving to limit accountability rather than genuinely solve the problem.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because this framing diverts attention and resources from other critical safety concerns, effectively granting regulatory leniency to tech companies. Suppression is high as it actively marginalizes alternative, broader definitions of AI safety. The theater ratio is moderate because some initiatives under this framing may be performative, addressing symptoms without tackling systemic issues. The claimed type is Tangled Rope because it genuinely coordinates efforts to address visible harms, but simultaneously extracts by limiting the scope of responsibility and suppressing alternative safety agendas.
 *
 * PERSPECTIVAL GAP:
 *   Tech companies and some policymakers perceive this framing as a necessary and effective coordination mechanism for managing AI risks, seeing themselves as beneficiaries of a pragmatic approach. In contrast, existential risk researchers, future generations, and many AI ethics advocates experience this same framing as extractive, as it sidelines their concerns and leaves systemic risks unaddressed, making them payers of the constraint's limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   Tech companies and policymakers seeking tangible wins are beneficiaries, as this framing reduces their overall regulatory burden and allows for politically expedient actions. Existential risk researchers and future generations are victims, as their concerns are deprioritized. Marginalized communities, while ostensibly protected, are also victims if the limited scope of this framing prevents comprehensive solutions to the harms they face. The constraint's active enforcement ensures this narrow framing dominates the discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framing_as_extraction_ambiguity,
    'Is the focus on near-term harms a genuine coordination effort to protect vulnerable populations, or is it primarily a strategic framing that extracts regulatory leniency from tech companies by diverting attention from broader risks?',
    'Analysis of resource allocation: if funding for systemic risk research and comprehensive regulatory frameworks remains disproportionately low compared to ''near-term harm'' initiatives, it supports the extraction hypothesis. Also, examination of policy outcomes: if documented harms persist or evolve despite ''near-term'' interventions, it suggests the framing is insufficient.',
    'If primarily extraction, the constraint''s effective extractiveness is higher, and its classification shifts closer to a Snare. If genuine coordination, extractiveness is lower, supporting a Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_as_extraction_ambiguity, conceptual, 'Ambiguity regarding the primary function of the ''near-term harms'' framing in AI safety.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative AI safety framings structural (e.g., funding mechanisms, institutional inertia) or internalized (e.g., researchers self-censoring due to perceived lack of funding/legitimacy for ''speculative'' work)?',
    'Post-funding-shift trajectory: if alternative framings gain traction and resources rapidly after a shift in major funding priorities, it suggests structural suppression was dominant. If resistance persists even with new funding, internalized suppression plays a larger role.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the suppressed ideas persist even after external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative AI safety framings.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''near_term_harms_reading'' of the ''ai_safety_commitment'' kernel, or does it conflate elements of other readings or external critiques?',
    'Expert review by proponents of the ''near_term_harms_reading'' to validate its core tenets and boundaries against the authored constraint. Comparison with canonical texts and advocacy positions of this specific school of thought.',
    'If misidentified, the classification of this constraint would be inaccurate, and its relationships to sibling readings in the ''ai_safety_commitment'' kernel would be distorted, leading to incorrect network analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies that the constraint accurately instantiates the intended kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__near_term_harms_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__near_term_harms_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__near_term_harms_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__near_term_harms_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_governance_regulatory_frameworks).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_ethics_guidelines).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ai_safety_commitment' kernel, focusing on present-day harms. Its structural properties are distinct from the 'existential_risk_reading' and 'dual_priority_reading', which address different aspects of AI safety.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
