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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint describes a 'hybrid' reading of disaster preparedness,
 *   where technical competence is concentrated in specialized institutions
 *   (e.g., Rijkswaterstaat, water boards in the Netherlands), while broader
 *   societal memory and participation in preparedness activities become
 *   increasingly ceremonial or ritualistic. This creates a dual-track system:
 *   core technical staff maintain genuine operational capacity, but
 *   peripheral actors perform rituals that feel like preparedness without
 *   building deep, distributed competence. The constraint is claimed as a
 *   Tangled Rope because it offers a genuine coordination function
 *   (centralized expertise for complex problems) but also involves asymmetric
 *   extraction, where the general public pays for a level of preparedness
 *   that is not fully realized at the distributed level, and political
 *   leaders benefit from the appearance of competence without bearing the
 *   full costs of its absence. This reading acknowledges both the functional
 *   and performative aspects.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.6).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.4).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '60820723-3825-46f4-81ed-1f47af494b4c').
narrative_ontology:cs_kernel_codification('60820723-3825-46f4-81ed-1f47af494b4c', formalized).
narrative_ontology:cs_authority_grounding('60820723-3825-46f4-81ed-1f47af494b4c', expertise).
narrative_ontology:cs_interpretation_layer_present('60820723-3825-46f4-81ed-1f47af494b4c').
narrative_ontology:cs_reading_relation('60820723-3825-46f4-81ed-1f47af494b4c', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('60820723-3825-46f4-81ed-1f47af494b4c', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_axiom('60820723-3825-46f4-81ed-1f47af494b4c', foundational, stratified_competence_is_necessary).
narrative_ontology:cs_axiom_status(stratified_competence_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('60820723-3825-46f4-81ed-1f47af494b4c', stratified_competence_is_necessary, empirically_contingent).
narrative_ontology:cs_axiom('60820723-3825-46f4-81ed-1f47af494b4c', secondary, public_reassurance_is_a_preparedness_function).
narrative_ontology:cs_axiom_status(public_reassurance_is_a_preparedness_function, holdable).
narrative_ontology:cs_axiom_grounding('60820723-3825-46f4-81ed-1f47af494b4c', public_reassurance_is_a_preparedness_function, instrumental).
narrative_ontology:cs_reference_frame('60820723-3825-46f4-81ed-1f47af494b4c', centralized_technical_excellence).
narrative_ontology:cs_drift_state('60820723-3825-46f4-81ed-1f47af494b4c', contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60820723-3825-46f4-81ed-1f47af494b4c', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, emergency_responders).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., Rijkswaterstaat, water boards) are the primary holders of technical competence for disaster preparedness. They define and execute drills, maintain infrastructure, and advise policy. They benefit from concentrated funding and authority, but are constrained by political mandates and public expectations.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the appearance of preparedness and the ability to delegate complex technical issues. They can claim credit for successful responses while deflecting blame for failures onto technical bodies. Their focus is often on short-term electoral cycles.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, political_leadership, beneficiary,
    powerful, immediate, mobile, national).

% Pays taxes to fund preparedness efforts and bears the primary risk when disasters strike. Their role in preparedness is largely ceremonial, participating in drills that do not build deep competence. They are trapped by the necessity of collective action for large-scale disaster response.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, general_public, payer,
    powerless, biographical, trapped, national).

% Expected to implement local preparedness plans, but often lack the deep technical expertise or resources of national institutions. Their memory of past disasters fades, and their participation in drills becomes ritualistic, leaving them vulnerable when actual competence is needed.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    moderate, generational, constrained, local).

% Benefit from the existence of specialized institutions that provide high-level planning and resources. However, they bear the direct costs of responding to events where societal preparedness is low, often facing under-resourcing and burnout due to the gap between ceremonial and actual competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, emergency_responders, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates disaster response by centralizing technical expertise and planning within specialized institutions, providing a single point of authority and knowledge for complex challenges like flood control.
% TRANSFER_FUNCTION: Transfers the burden of active, distributed preparedness from the general public and local communities to specialized institutions, in exchange for a perceived (but often ceremonial) sense of security.
% ABSENT_VOICES: Advocates for distributed resilience and genuine societal competence would argue for shifting resources and training to empower local communities and individuals, rather than concentrating it in a few institutions. Their voices are often drowned out by the perceived efficiency of centralized expertise and the political appeal of 'expert-led' solutions.
% DISAPPEARANCE_RATIONALE: If this stratified system vanished, the immediate consequence would be a collapse in large-scale disaster response capacity, as the specialized institutions holding technical competence would no longer exist. Society would be forced to rapidly re-decentralize or rebuild preparedness, likely at great cost and with significant initial failures.
% FOUNDING_PROBLEM: The problem of managing complex, large-scale natural disasters (e.g., floods, earthquakes) that require highly specialized technical knowledge and coordinated infrastructure, which cannot be effectively managed by diffuse, untrained populations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, as large-scale natural disasters continue to pose significant threats. Specialized institutions and independent scientific bodies corroborate the ongoing need for technical expertise. However, critics (e.g., disaster sociologists, community organizers) contest whether the current stratified solution is the most effective way to address it, arguing it creates new vulnerabilities.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.6) because resources are drawn from the public for preparedness, but the actual benefit of distributed resilience is diminished by the ceremonial nature of broader engagement. Suppression is moderate (0.4) as alternatives (e.g., fully decentralized, community-led preparedness) are not actively crushed but are implicitly discouraged by the centralized model. Theater ratio is high (0.55) because a significant portion of 'preparedness' activities (e.g., public drills, awareness campaigns) serve more to reassure and maintain the legitimacy of the centralized system than to build genuine, distributed competence. The increasing trend in extractiveness and theater ratio over time reflects the gradual shift from a more balanced system to one where the ceremonial aspect grows relative to actual distributed capacity.
 *
 * PERSPECTIVAL GAP:
 *   Specialized institutions and political leaders perceive this as an efficient and necessary coordination mechanism for complex problems. The general public and local communities, however, experience it as a system that extracts resources and participation without delivering commensurate, tangible improvements in their own preparedness or safety. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and political leadership are beneficiaries: institutions gain concentrated authority and funding, and leaders gain political capital from perceived competence. The general public and local communities are victims: they bear the costs (taxes, risk) but receive diminished actual resilience due to the ceremonial nature of their engagement. Emergency responders are dual-positioned: they benefit from centralized planning but pay with increased workload and risk due to the gaps in distributed preparedness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balance_of_competence_distribution,
    'What is the optimal balance between centralized technical competence and distributed societal preparedness for effective disaster response?',
    'Comparative studies of disaster outcomes in regions with different preparedness models, coupled with cost-benefit analysis of investing in centralized vs. distributed capacity.',
    'If a more distributed model proves superior, the current hybrid system would be reclassified as more extractive and less coordinative, as its ''coordination'' function would be revealed as suboptimal and self-serving. If the current balance is optimal, the extraction would be re-evaluated as a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_competence_distribution, empirical, 'Determining the ideal distribution of preparedness competence.').

omega_variable(
    ceremonial_vs_functional_threshold,
    'At what point does ''ceremonial'' participation in preparedness activities become actively detrimental to actual societal resilience, rather than merely ineffective?',
    'Empirical studies correlating the level of ceremonial engagement with actual response times, casualty rates, and recovery metrics in real-world disaster scenarios.',
    'If ceremonial activities are found to actively degrade resilience (e.g., by fostering false confidence or diverting resources), the ''theater_ratio'' would be re-evaluated as having a more severe impact, potentially shifting the constraint towards a Snare classification for the public.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_vs_functional_threshold, empirical, 'Threshold for ceremonial activities becoming harmful.').

omega_variable(
    framing_of_preparedness_kernel,
    'Is the ''preparedness_retention'' kernel fundamentally about technical capacity, or about societal resilience?',
    'Conceptual analysis of policy documents, public discourse, and expert testimony, identifying the dominant framing. This is a conceptual choice, not an empirical one.',
    'If framed as purely technical capacity, the hybrid reading''s extractiveness might be seen as a necessary cost. If framed as societal resilience, the current stratification would be seen as a failure of the coordination function, increasing its perceived extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_preparedness_kernel, conceptual, 'Conceptual framing of the preparedness kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1950, preparedness_retention__hybrid_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(prep_tr_t1970, preparedness_retention__hybrid_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(prep_tr_t1990, preparedness_retention__hybrid_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(prep_tr_t2010, preparedness_retention__hybrid_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(prep_tr_t2024, preparedness_retention__hybrid_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_be_t1950, preparedness_retention__hybrid_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(prep_be_t1970, preparedness_retention__hybrid_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(prep_be_t1990, preparedness_retention__hybrid_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(prep_be_t2010, preparedness_retention__hybrid_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(prep_be_t2024, preparedness_retention__hybrid_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1950, preparedness_retention__hybrid_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(prep_su_t1970, preparedness_retention__hybrid_reading, suppression_requirement, 1970, 0.32).
narrative_ontology:measurement(prep_su_t1990, preparedness_retention__hybrid_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(prep_su_t2010, preparedness_retention__hybrid_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(prep_su_t2024, preparedness_retention__hybrid_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'preparedness_retention' kernel, which describes how disaster preparedness is maintained. This 'hybrid_reading' acknowledges both functional competence and ceremonial aspects, contrasting with the 'competence_reading' (purely functional) and 'husk_reading' (purely ceremonial).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
