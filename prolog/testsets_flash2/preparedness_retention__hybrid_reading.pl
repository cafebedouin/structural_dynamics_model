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
 *   core technical competence is retained within specialized institutions
 *   (e.g., Rijkswaterstaat, water boards), while broader societal memory and
 *   engagement in preparedness become largely ceremonial. This reading
 *   acknowledges the genuine technical function of specialized bodies but
 *   highlights the cost of this centralization in terms of reduced
 *   distributed resilience and a performative, rather than substantive,
 *   societal role in preparedness. The claimed type is 'tangled_rope' because
 *   it offers a genuine coordination function (centralized expertise) but
 *   also involves asymmetric extraction (reduced societal resilience,
 *   concentrated risk) and requires active enforcement to maintain the
 *   stratification.
 *
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
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '64c1f33d-4c23-4c4a-9a03-98fe4a1440db').
narrative_ontology:cs_kernel_codification('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', formalized).
narrative_ontology:cs_authority_grounding('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', expertise).
narrative_ontology:cs_interpretation_layer_present('64c1f33d-4c23-4c4a-9a03-98fe4a1440db').
narrative_ontology:cs_reading_relation('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', preparedness_retention__competence_reading, influences).
narrative_ontology:cs_reading_relation('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', preparedness_retention__husk_reading, influences).
narrative_ontology:cs_axiom('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', foundational, technical_competence_centralization_is_necessary).
narrative_ontology:cs_axiom_status(technical_competence_centralization_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', technical_competence_centralization_is_necessary, empirically_contingent).
narrative_ontology:cs_axiom('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', foundational, distributed_resilience_is_sacrificed_for_centralization).
narrative_ontology:cs_axiom_status(distributed_resilience_is_sacrificed_for_centralization, holdable).
narrative_ontology:cs_axiom_grounding('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', distributed_resilience_is_sacrificed_for_centralization, empirically_contingent).
narrative_ontology:cs_reference_frame('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', centralized_technical_excellence_with_diffuse_risk).
narrative_ontology:cs_drift_state('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', contemporary_era_of_complex_risks, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('64c1f33d-4c23-4c4a-9a03-98fe4a1440db', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, broader_society).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., Rijkswaterstaat, water boards) are the primary holders of technical competence and operational memory for disaster preparedness. They benefit from concentrated resources and authority, but also bear the burden of maintaining complex systems.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the appearance of robust preparedness and the ability to delegate complex technical issues to specialized bodies, avoiding direct accountability for day-to-day operational readiness. Can shift blame if failures occur.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, political_leadership, beneficiary,
    powerful, immediate, mobile, national).

% Pays for preparedness through taxes and diffuse risks, but lacks direct access to or understanding of the technical competence. Its role in preparedness becomes largely ceremonial, leading to a false sense of security and reduced adaptive capacity.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, broader_society, payer,
    powerless, biographical, trapped, national).

% Bear the immediate impact of disaster failures due to reduced local resilience and over-reliance on centralized expertise. Their own memory and capacity for self-organization in preparedness atrophy, making them victims of the stratified system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    moderate, biographical, constrained, local).

% Operate at the interface of specialized competence and societal memory. They rely on the technical expertise of institutions but often face the consequences of a less prepared public and fragmented local knowledge.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, emergency_responders, payer,
    organized, immediate, constrained, local).

% Analyze the effectiveness of preparedness strategies, often highlighting the risks of stratified competence and the need for distributed resilience. Their insights may challenge the existing institutional arrangements.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, independent_experts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates disaster preparedness by centralizing technical expertise and operational capacity within specialized institutions, aiming for efficient, large-scale response and infrastructure management.
% TRANSFER_FUNCTION: Transfers the burden of active preparedness from broader society and local communities to specialized institutions, in exchange for a perceived (but often ceremonial) sense of security and institutional continuity.
% ABSENT_VOICES: Future generations and those who will bear the brunt of systemic failures due to atrophied societal memory are absent. They would argue for a more distributed, resilient, and transparent preparedness model.
% DISAPPEARANCE_RATIONALE: If this stratified system vanished overnight, there would be an immediate crisis of technical competence and operational capacity, as specialized institutions would no longer hold exclusive knowledge. However, it would also force a rapid re-localization and re-distribution of preparedness efforts, potentially leading to more resilient, if initially chaotic, outcomes.
% FOUNDING_PROBLEM: The need for highly specialized, long-term technical expertise to manage complex infrastructure (e.g., water management, large-scale flood defenses) and coordinate national-level disaster response.
% FOUNDING_PROBLEM_CORROBORATION: Specialized institutions and political leadership attest the problem is live, citing the ongoing complexity of infrastructure and threats. Independent experts corroborate the need for technical competence but question the stratification, arguing it creates new vulnerabilities.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.6) is moderate-high because the system extracts distributed resilience from society, concentrating risk and decision-making power. Suppression (0.4) is present but not extreme; it's more about the 'soft' suppression of alternatives through institutional inertia and the perceived competence of experts, rather than overt coercion. Theater ratio (0.5) is significant, reflecting the ceremonial nature of broader societal preparedness activities (drills, public information campaigns) that often lack genuine competence-building for the general public. Accessibility collapse (0.3) is low because alternatives (distributed resilience, local knowledge) are not entirely collapsed but are significantly marginalized. Resistance (0.2) is low because the system is largely accepted due to the perceived necessity of specialized expertise.
 *
 * PERSPECTIVAL GAP:
 *   Specialized institutions perceive this as an efficient, necessary coordination mechanism for complex problems. Broader society experiences it as a distant, opaque system that demands compliance but offers little genuine empowerment. The engine's classification as 'tangled_rope' captures this dual nature, diverging from a purely 'rope' (coordination) or 'snare' (pure extraction) view.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and political leadership are beneficiaries, gaining authority, resources, and reduced direct accountability. Broader society and local communities are victims, losing agency and resilience while bearing diffuse risks. Emergency responders are payers, caught between the centralized expertise and the unprepared public. Independent experts act as observers, analyzing the system's dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (managing complex disaster risks) is still live, but its *mode* of operation has drifted. The stratification prevents mislabeling it as a pure rope (ignoring the extraction of resilience) or a pure snare (ignoring the genuine technical coordination). It's a tangled rope because the coordination function is real, but its implementation creates an extractive asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    societal_memory_decay_rate,
    'At what rate does broader societal memory and practical competence for preparedness decay under this stratified system?',
    'Longitudinal studies tracking public knowledge, skill retention, and adaptive capacity in communities over decades, correlated with the degree of institutional centralization.',
    'A high decay rate would increase the effective extractiveness and suppression for broader society, pushing the constraint closer to a ''snare'' for those seats. A low decay rate would suggest the ceremonial aspects are less detrimental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(societal_memory_decay_rate, empirical, 'Quantifying the loss of distributed resilience due to stratification.').

omega_variable(
    centralization_efficiency_tradeoff,
    'Is the efficiency gained by centralizing technical competence truly offset by the loss of distributed resilience, or is the centralization itself a net benefit?',
    'Comparative analysis of disaster outcomes in highly centralized vs. more distributed preparedness systems, controlling for other variables. This is a conceptual question about the optimal balance.',
    'If the net benefit of centralization is high, the ''tangled_rope'' classification might lean more towards ''rope'' for the system as a whole. If the costs of lost resilience outweigh benefits, it leans towards ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centralization_efficiency_tradeoff, conceptual, 'Evaluating the overall systemic benefit of stratified preparedness.').

omega_variable(
    reading_framing_impact,
    'Is this ''hybrid_reading'' the most accurate framing, or does the ''competence_reading'' or ''husk_reading'' better capture the structural reality?',
    'Empirical evidence on the actual efficacy of drills and public engagement (competence_reading) vs. their purely performative nature (husk_reading), combined with analysis of institutional power dynamics.',
    'If ''competence_reading'' is more accurate, the extractiveness and theater ratio would be lower. If ''husk_reading'' is more accurate, extractiveness and theater ratio would be higher, pushing towards a ''snare'' or ''piton''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Ambiguity in the primary structural characterization of preparedness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__hybrid_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'preparedness_retention' kernel. This 'hybrid_reading' acknowledges both genuine technical competence and ceremonial societal memory, influencing how the 'competence_reading' (more optimistic) and 'husk_reading' (more cynical) are understood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
