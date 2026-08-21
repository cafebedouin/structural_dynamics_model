% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness and Institutional Memory (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint describes a dual-track system of disaster preparedness
 *   where core technical competence is concentrated and retained within
 *   specialized institutions (like Rijkswaterstaat or water boards), while
 *   broader societal memory and active participation in preparedness become
 *   increasingly ceremonial or performative. This 'hybrid reading'
 *   acknowledges the genuine technical coordination function of the
 *   specialized bodies but also identifies the asymmetric extraction of
 *   distributed resilience from local communities and the broader public.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.65).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.7).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Disaster Preparedness and Institutional Memory (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '6449971c-ce81-4642-b9b8-004a8e4a47e2').
narrative_ontology:cs_kernel_codification('6449971c-ce81-4642-b9b8-004a8e4a47e2', formalized).
narrative_ontology:cs_authority_grounding('6449971c-ce81-4642-b9b8-004a8e4a47e2', expertise).
narrative_ontology:cs_interpretation_layer_present('6449971c-ce81-4642-b9b8-004a8e4a47e2').
narrative_ontology:cs_reading_relation('6449971c-ce81-4642-b9b8-004a8e4a47e2', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6449971c-ce81-4642-b9b8-004a8e4a47e2', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('6449971c-ce81-4642-b9b8-004a8e4a47e2', foundational, technical_expertise_requires_centralization).
narrative_ontology:cs_axiom_status(technical_expertise_requires_centralization, holdable).
narrative_ontology:cs_axiom_grounding('6449971c-ce81-4642-b9b8-004a8e4a47e2', technical_expertise_requires_centralization, empirically_contingent).
narrative_ontology:cs_axiom('6449971c-ce81-4642-b9b8-004a8e4a47e2', secondary, societal_memory_can_be_passive).
narrative_ontology:cs_axiom_status(societal_memory_can_be_passive, holdable).
narrative_ontology:cs_axiom_grounding('6449971c-ce81-4642-b9b8-004a8e4a47e2', societal_memory_can_be_passive, empirically_contingent).
narrative_ontology:cs_reference_frame('6449971c-ce81-4642-b9b8-004a8e4a47e2', efficient_centralized_expertise).
narrative_ontology:cs_drift_state('6449971c-ce81-4642-b9b8-004a8e4a47e2', contemporary_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6449971c-ce81-4642-b9b8-004a8e4a47e2', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, central_government).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, broader_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, disaster_response_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These bodies (e.g., Rijkswaterstaat, water boards) are mandated to retain and exercise technical competence for complex infrastructure. They benefit from concentrated authority, stable funding, and a clear mandate, but are also responsible for large-scale outcomes.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the perceived efficiency and reliability of centralized expertise, which simplifies governance and provides a clear point of accountability for disaster management. It relies on these institutions to manage complex risks.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, central_government, beneficiary,
    institutional, civilizational, analytical, national).

% Bear the costs of reduced local agency and distributed resilience. Their active role in preparedness is often reduced to ceremonial participation, leaving them vulnerable to the single point of failure inherent in centralized systems.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Experiences a general sense of security from the existence of specialized institutions, but their own practical knowledge and memory of preparedness practices atrophy. They pay through taxes and through increased vulnerability to systemic failures.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, broader_public, payer,
    powerless, biographical, trapped, national).

% Local emergency services and first responders often find themselves operating with limited local knowledge and resources, relying heavily on directives from centralized bodies. They bear the immediate operational costs of the stratified system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, disaster_response_practitioners, payer,
    moderate, immediate, constrained, local).

% Academics, NGOs, and independent researchers who analyze the effectiveness of disaster preparedness, often highlighting the trade-offs between centralized expertise and distributed resilience. They can influence policy but do not directly participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, critical_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To centralize and maintain highly specialized technical competence for managing complex, large-scale infrastructure and disaster risks that exceed local capacity.
% TRANSFER_FUNCTION: Transfers active responsibility for preparedness and risk management from distributed societal actors to specialized, centralized institutions, in exchange for a perceived (and often real) increase in technical capacity and efficiency.
% ABSENT_VOICES: Advocates for distributed, adaptive resilience, local knowledge holders, and community-based disaster management initiatives are often marginalized or their contributions ceremonialized, rather than integrated into core planning.
% DISAPPEARANCE_RATIONALE: If the specialized institutions and their mandates vanished overnight, the capacity to manage large-scale disasters (e.g., major floods, infrastructure failures) would collapse, leading to catastrophic consequences and a complete reorganization of national risk management.
% FOUNDING_PROBLEM: The need for highly specialized, long-term technical expertise and institutional memory to manage complex, large-scale infrastructure (like water management in the Netherlands) and mitigate risks that are beyond the scope of local, ad-hoc responses.
% FOUNDING_PROBLEM_CORROBORATION: Technical experts, historical records of major infrastructure projects, and ongoing risk assessments from independent bodies corroborate the continued need for specialized expertise. However, the extent to which this necessitates the ceremonialization of broader societal memory is contested by social scientists and community organizers.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates complex technical expertise for large-scale risks (benefiting specialized institutions and central government) while simultaneously extracting distributed resilience and local agency (victimizing local communities and the public). Extractiveness (0.65) reflects the cost of lost local capacity and the single point of failure risk. Suppression (0.70) arises from the institutionalization of expertise and the ceremonialization of alternatives. The theater ratio (0.45) captures the significant, but not total, performative aspect of broader societal engagement with preparedness. Metrics show a gradual increase in extraction, suppression, and theater over time as the stratification deepens.
 *
 * PERSPECTIVAL GAP:
 *   Specialized institutions and central government tend to view this arrangement as an efficient and necessary coordination mechanism for complex risks, emphasizing the technical competence it preserves. Local communities and disaster response practitioners, however, experience it as a disempowering structure that extracts their agency and leaves them reliant on distant expertise, often through ceremonial participation that lacks genuine impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and central government are beneficiaries, gaining efficiency, authority, and simplified governance (low directionality). Local communities and the broader public are victims, bearing the costs of lost distributed resilience and increased vulnerability (high directionality). Local disaster response practitioners are also payers, as they operate within the constraints of the centralized system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_centralization_decentralization_balance,
    'What is the optimal balance between centralized technical expertise and distributed societal resilience for effective disaster preparedness?',
    'Comparative studies of disaster outcomes in systems with varying degrees of centralization, and empirical analysis of the cost-effectiveness of investing in local vs. central capacity.',
    'If a more distributed model proves more effective or cost-efficient, the current stratification would be reclassified as more extractive and suppressive than currently assessed, leading to policy recommendations for decentralization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_centralization_decentralization_balance, empirical, 'Determining the ideal mix of centralized and distributed preparedness.').

omega_variable(
    ceremonial_function_necessity,
    'Is the ceremonial aspect of broader societal memory a necessary component for maintaining public trust and engagement, or is it a cover for the atrophy of genuine distributed competence?',
    'Qualitative sociological studies on public perception and engagement with preparedness drills, combined with quantitative analysis of actual behavioral changes and knowledge retention post-ceremony.',
    'If purely ceremonial, the theater_ratio and extractiveness would be higher, indicating a greater degree of performative maintenance and disempowerment. If it serves a genuine, albeit indirect, function, the current metrics are appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_function_necessity, conceptual, 'Assessing the true function of ceremonial preparedness activities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1950, preparedness_retention__hybrid_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(prep_tr_t1965, preparedness_retention__hybrid_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(prep_tr_t1980, preparedness_retention__hybrid_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(prep_tr_t1995, preparedness_retention__hybrid_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(prep_tr_t2010, preparedness_retention__hybrid_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(prep_tr_t2025, preparedness_retention__hybrid_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(prep_be_t1950, preparedness_retention__hybrid_reading, base_extractiveness, 1950, 0.45).
narrative_ontology:measurement(prep_be_t1965, preparedness_retention__hybrid_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(prep_be_t1980, preparedness_retention__hybrid_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(prep_be_t1995, preparedness_retention__hybrid_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(prep_be_t2010, preparedness_retention__hybrid_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(prep_be_t2025, preparedness_retention__hybrid_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1950, preparedness_retention__hybrid_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(prep_su_t1965, preparedness_retention__hybrid_reading, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement(prep_su_t1980, preparedness_retention__hybrid_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement(prep_su_t1995, preparedness_retention__hybrid_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(prep_su_t2010, preparedness_retention__hybrid_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(prep_su_t2025, preparedness_retention__hybrid_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'preparedness_retention' family, which decomposes the natural-language concept of 'preparedness' into distinct structural claims. This 'hybrid_reading' focuses on the stratification of competence and ceremonialization of memory, distinct from readings that emphasize pure competence or pure performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
