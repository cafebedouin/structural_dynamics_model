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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint describes a hybrid system of disaster preparedness where
 *   deep technical competence is concentrated in specialized institutions,
 *   while broader societal engagement in preparedness becomes largely
 *   ceremonial. This reading acknowledges the necessity of specialized
 *   expertise for complex risks but highlights the resulting vulnerability
 *   from a loss of distributed resilience. The system is claimed as a Tangled
 *   Rope because it genuinely coordinates complex technical functions but
 *   also extracts agency and practical knowledge from the broader public,
 *   creating an asymmetric dependency.
 *
 * KEY AGENTS:
 *   - specialized_institutions: Primary agenda-setter (institutional/constrained) — retains technical competence.
 *   - political_leadership: Primary beneficiary (powerful/mobile) — benefits from perceived preparedness.
 *   - broader_society: Primary payer (powerless/trapped) — loses practical knowledge and agency.
 *   - local_communities: Payer (moderate/constrained) — bears direct costs of distributed failure.
 *   - emergency_responders: Payer/Beneficiary (organized/constrained) — operates at the interface of competence and vulnerability.
 *   - critical_infrastructure_operators: Beneficiary (institutional/constrained) — relies on centralized expertise.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.6).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.4).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '39428429-75f8-4511-8cb8-4c507d3faa98').
narrative_ontology:cs_kernel_codification('39428429-75f8-4511-8cb8-4c507d3faa98', formalized).
narrative_ontology:cs_authority_grounding('39428429-75f8-4511-8cb8-4c507d3faa98', expertise).
narrative_ontology:cs_interpretation_layer_present('39428429-75f8-4511-8cb8-4c507d3faa98').
narrative_ontology:cs_reading_relation('39428429-75f8-4511-8cb8-4c507d3faa98', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('39428429-75f8-4511-8cb8-4c507d3faa98', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('39428429-75f8-4511-8cb8-4c507d3faa98', foundational, specialized_expertise_is_paramount).
narrative_ontology:cs_axiom_status(specialized_expertise_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('39428429-75f8-4511-8cb8-4c507d3faa98', specialized_expertise_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('39428429-75f8-4511-8cb8-4c507d3faa98', foundational, societal_engagement_is_symbolic).
narrative_ontology:cs_axiom_status(societal_engagement_is_symbolic, holdable).
narrative_ontology:cs_axiom_grounding('39428429-75f8-4511-8cb8-4c507d3faa98', societal_engagement_is_symbolic, conventional).
narrative_ontology:cs_reference_frame('39428429-75f8-4511-8cb8-4c507d3faa98', centralized_technical_competence_with_public_support).
narrative_ontology:cs_drift_state('39428429-75f8-4511-8cb8-4c507d3faa98', contemporary_risk_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('39428429-75f8-4511-8cb8-4c507d3faa98', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, broader_society).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, emergency_responders).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, critical_infrastructure_operators).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., Rijkswaterstaat, water boards) are tasked with maintaining technical competence for disaster preparedness. They benefit from concentrated resources and authority, but also bear the burden of being the primary point of failure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the appearance of robust preparedness without needing to invest in broad societal resilience. Can point to the specialized institutions as evidence of competence, deflecting responsibility for distributed failures.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, political_leadership, beneficiary,
    powerful, immediate, mobile, national).

% Pays taxes to fund specialized institutions but loses practical knowledge and agency in disaster response. Becomes dependent on centralized expertise, making them vulnerable to systemic failures.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, broader_society, payer,
    powerless, biographical, trapped, national).

% Experience the direct consequences of preparedness failures. Their local knowledge and capacity for self-organization are undervalued, leading to a loss of distributed resilience. They bear the costs of centralized expertise's limitations.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    moderate, biographical, constrained, local).

% Operate at the interface of specialized knowledge and societal vulnerability. They benefit from the technical competence of specialized institutions but are often overwhelmed by the lack of distributed resilience in the broader society.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, emergency_responders, payer,
    organized, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, emergency_responders, beneficiary).

% Rely on the specialized institutions for large-scale protective measures (e.g., flood defenses). They benefit from this centralized expertise but are exposed to risks if the specialized institutions fail or if their competence does not translate to local contexts.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, critical_infrastructure_operators, beneficiary,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates large-scale disaster preparedness by centralizing technical expertise and resources in specialized institutions, aiming for efficient, high-level protective measures.
% TRANSFER_FUNCTION: Transfers responsibility and practical competence for disaster preparedness from broader society and local communities to specialized institutions and central government, in exchange for perceived safety and efficiency.
% ABSENT_VOICES: Advocates for distributed resilience, community-led preparedness initiatives, and local knowledge integration are often marginalized. They would argue for a more decentralized, participatory approach to preparedness, emphasizing societal agency over centralized control.
% DISAPPEARANCE_RATIONALE: If this stratified system vanished, there would be an immediate crisis of competence and coordination for large-scale disasters. However, it would also create an imperative for communities to rebuild local resilience and for a more distributed, adaptive preparedness model to emerge, fundamentally reorganizing how society approaches risk.
% FOUNDING_PROBLEM: The need for highly specialized, long-term technical expertise to manage complex, large-scale environmental risks (e.g., flood control in low-lying deltas) that exceed local capacity.
% FOUNDING_PROBLEM_CORROBORATION: Specialized institutions and many scientific bodies attest that the core problem of managing complex environmental risks remains live and requires deep technical expertise. Critics acknowledge the technical need but argue the current stratification has created new vulnerabilities, as evidenced by post-disaster reviews and academic studies on resilience from outside the benefiting parties.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.6) because while specialized institutions provide a real service, the centralization of competence creates a single point of failure and extracts distributed resilience from society. Suppression (0.4) is present as societal participation is channeled into symbolic acts rather than genuine capacity building. Theater ratio (0.5) reflects the balance between genuine technical work and the performative aspects of public preparedness campaigns that mask underlying vulnerabilities. The increasing extractiveness and theater over time reflect a drift towards greater centralization and less distributed capacity.
 *
 * PERSPECTIVAL GAP:
 *   Specialized institutions and political leadership perceive this as an efficient, necessary coordination mechanism for complex risks. Broader society and local communities experience it as a loss of agency and a source of vulnerability, where their role is reduced to ceremonial compliance. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and political leadership are beneficiaries, as they gain authority, resources, or political capital from this arrangement. Broader society and local communities are victims, as they lose practical knowledge, agency, and bear the costs of centralized failure. Emergency responders and critical infrastructure operators have a dual role, benefiting from technical competence but also bearing the costs of systemic vulnerability.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction of distributed resilience) or a pure Snare (ignoring the genuine coordination function of specialized expertise). It highlights the hybrid nature where a necessary coordination function has become entangled with an asymmetric transfer of agency and risk, leading to a form of Mandatrophy where the original mandate of comprehensive preparedness is undermined by its own structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_resilience_measurement,
    'How can distributed societal resilience be quantitatively measured to assess its loss under this stratified system?',
    'Development of new metrics for community self-organization, local knowledge retention, and adaptive capacity, tested through post-disaster recovery studies.',
    'If distributed resilience is found to be severely degraded, the extractiveness of the hybrid system would be re-evaluated as higher, pushing it closer to a Snare. If it''s found to be robust, the extractiveness would be lower, closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_resilience_measurement, empirical, 'Uncertainty in measuring the loss of distributed societal resilience.').

omega_variable(
    competence_vs_ceremony_threshold,
    'At what point does the ceremonial aspect of broader societal preparedness outweigh the functional competence of specialized institutions, creating a net vulnerability?',
    'Comparative case studies of disaster outcomes in systems with varying degrees of stratification, correlating with the balance of technical competence vs. societal ceremony.',
    'Identifying this threshold would clarify when the ''hybrid'' system effectively collapses into a ''husk_reading'' (pure ceremony) from a societal perspective, leading to a reclassification towards Snare for the broader public.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_vs_ceremony_threshold, conceptual, 'Defining the tipping point where ceremonial preparedness becomes a net vulnerability.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''preparedness_retention'' kernel. What specific structural elements would change if a ''husk_reading'' or ''competence_reading'' were adopted?',
    'Analysis of policy documents, institutional budgets, and public communication strategies under each reading. A ''husk_reading'' would show minimal investment in actual competence outside specialized institutions, while a ''competence_reading'' would show active investment in distributed knowledge.',
    'If the ''husk_reading'' were adopted, the constraint''s extractiveness and theater_ratio would be significantly higher, and suppression would be more explicit. If the ''competence_reading'' were adopted, extractiveness and theater_ratio would be lower, and beneficiaries would be more broadly distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between the ''hybrid_reading'' and its sibling readings of the preparedness_retention kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__hybrid_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
