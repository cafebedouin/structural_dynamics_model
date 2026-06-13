% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission as Hollow Ritual
 *   domain: institutional/disaster_management
 *
 * SUMMARY:
 *   After severe floods in the 1970s–1980s, disaster management agencies
 *   established standardized protocols and mandatory drills to ensure
 *   preparedness knowledge survived generational transitions. For decades,
 *   drills validated actual competence — engineers and coordinators exercised
 *   procedures that reflected current settlement patterns and infrastructure.
 *   By the 2000s, the founding problem (knowledge loss across transitions)
 *   had been solved by modern documentation, institutional structures, and
 *   communication tools. Yet the drilling regimen persisted unchanged, now as
 *   theater: ritual performance with low correspondence to actual operational
 *   capability. This is the HUSK READING — organizational memory persists in
 *   written form, but the knowledge embedded in practice has decayed. High
 *   compliance with protocol form masks low adaptive capacity under novel
 *   scenarios. The constraint persists because institutional legitimacy,
 *   career incentives, and the absence of a triggering failure event sustain
 *   it.
 *
 * KEY AGENTS:
 *   - Emergency management agency: agenda-setter; administers rituals; cannot exit without institutional legitimacy collapse
 *   - Disaster management career cohort: beneficiary; careers depend on preparedness infrastructure remaining salient
 *   - Community flood coordinators: payer, identity-locked; know protocols are obsolete but face career ruin if they say so
 *   - Civilian population: payer, trapped; participates in drills based on outdated knowledge
 *   - Disaster research community: observer; documents the gap but has no seat in agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.71).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission as Hollow Ritual").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "institutional/disaster_management").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, '69da9d7c-6fcb-4ea3-932a-056d0045ff67').
narrative_ontology:cs_kernel_codification('69da9d7c-6fcb-4ea3-932a-056d0045ff67', formalized).
narrative_ontology:cs_authority_grounding('69da9d7c-6fcb-4ea3-932a-056d0045ff67', extraction).
narrative_ontology:cs_interpretation_layer_present('69da9d7c-6fcb-4ea3-932a-056d0045ff67').
narrative_ontology:cs_reading_relation('69da9d7c-6fcb-4ea3-932a-056d0045ff67', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('69da9d7c-6fcb-4ea3-932a-056d0045ff67', preparedness_transmission__hybrid_reading, influences).
narrative_ontology:cs_axiom('69da9d7c-6fcb-4ea3-932a-056d0045ff67', foundational, institutional_form_persists_without_knowledge_substance).
narrative_ontology:cs_axiom_status(institutional_form_persists_without_knowledge_substance, holdable).
narrative_ontology:cs_axiom_grounding('69da9d7c-6fcb-4ea3-932a-056d0045ff67', institutional_form_persists_without_knowledge_substance, empirically_contingent).
narrative_ontology:cs_axiom('69da9d7c-6fcb-4ea3-932a-056d0045ff67', secondary, suppression_preserves_hollow_ritual_against_adaptation).
narrative_ontology:cs_axiom_status(suppression_preserves_hollow_ritual_against_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('69da9d7c-6fcb-4ea3-932a-056d0045ff67', suppression_preserves_hollow_ritual_against_adaptation, conventional).
narrative_ontology:cs_reference_frame('69da9d7c-6fcb-4ea3-932a-056d0045ff67', generational_preparedness_continuity).
narrative_ontology:cs_drift_state('69da9d7c-6fcb-4ea3-932a-056d0045ff67', contemporary_post_2020_settlement_change, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69da9d7c-6fcb-4ea3-932a-056d0045ff67', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, institutional_continuity_administrators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, disaster_management_career_cohort).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_transmission__husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.62 over the interval as the founding problem solved and protocols became increasingly decoupled from operational reality. The cost of ritual performance (budget, personnel time, opportunity cost) remains constant, but the benefit (actual preparedness) decays as demographics, infrastructure, and communication tools change. Theater ratio rises from 0.22 to 0.68: by 2025, the majority of drilling activity is performance for institutional continuity rather than functional knowledge validation. Suppression is high (0.71) because the constraint persists through active enforcement of compliance and through the career penalties on anyone who publicly declares the knowledge hollow. Community coordinators are suppressed via identity-lock and professional reputational risk; engineers are suppressed via organizational culture (the agency does not reward gap-raising); the civilian population is suppressed through information asymmetry (they do not know the protocols are obsolete). The metric series captures the trajectory of institutional decay: a constraint that began as genuine coordination (in 1975, when protocols reflected lived experience) has degraded into performative theater maintained by inertia. This is the defining Piton signature: the cost to fix the constraint (rebuild the knowledge base, update protocols, retrain personnel, admit public gap) exceeds what any single party bears from its persistence.
 *
 * PERSPECTIVAL GAP:
 *   The agency and career cohort compute the constraint as coordination (we maintain shared frameworks, we ensure institutional continuity) — from their seats, the drilling ritual is successful institutional memory. The community coordinators and engineers compute it as extraction (we perform labor validating procedures we know are inadequate; the cost is our credibility and the community's actual preparedness; the benefit accrues to the institutional regime). The civilian population computes it as structural inevitability (we are governed by procedures we cannot evaluate, trust the regime, hope it works). The engine should compute DIFFERENT types from these different seats, reflecting the structural asymmetry: the agenda-setter and beneficiary seats experience coordination or inertial rope; the payer seats experience snare (the procedures suppress their voice on obsolescence and trap them in participation). The husk reading asserts that this divergence reflects genuine structural differences in how each seat relates to the constraint's actual function.
 *
 * DIRECTIONALITY LOGIC:
 *   The agency (agenda_setter) has low d — it benefits from the constraint's persistence (legitimacy, budget, career advancement for compliant staff). Community coordinators and civilian population have high d — they are trapped in participation and bear the cost (labor, opportunity, trust) without control. The constraint extracts from the low-power many (civic time, deferred adaptation) to maintain the mid-power few (careers, institutional budgets). Identity-lock on the community coordinators is the critical asymmetry: they have information (the protocols are obsolete) but cannot act on it without destroying their identity within the community leadership structure. Exit would mean abandonment of the role itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss of preparedness across generational transitions) was live in 1975 and has been solved by 2025 through institutional structures and documentation. The constraint persists in terminal mandatrophy — the institutional regime that solved the problem now maintains it as theater because the cost of admitting the problem is solved exceeds the benefit of reorganizing. The piton classification captures this: the theater ratio rises as extraction dominates functional benefit. A Rope would show continuous re-validation of function and updates reflecting changing conditions — the measurement series shows decay, not renewal. A Tangled Rope would show asymmetric extraction WITH coordination — there is no coordination here, only the form of it. The Piton classification reflects the honest dynamics: inertial maintenance of a regime whose primary function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hollow_form_vs_functional_decay,
    'Is the constraint''s persistence maintained primarily by the institutional form (career incentives, budget structures, organizational continuity), or by actual ongoing knowledge transmission and validation?',
    'Detailed audit of drill scenario design compared to contemporary flood modeling; interviews with engineers and coordinators on the correspondence between protocol and actual operational capacity; post-disaster performance against novel scenarios.',
    'If form dominates (the husk reading), the constraint is piton and extractive. If function dominates (the competence reading), the constraint is rope or tangled rope and primarily coordinating. The reading classification depends on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hollow_form_vs_functional_decay, empirical, 'Whether institutional form masks functional decay or whether validation is genuine.').

omega_variable(
    identity_lock_mechanism_in_coordinators,
    'Is the suppression experienced by community coordinators structural (they are legally prohibited from changing protocols) or internalized (they believe they lack authority and fear career consequences)?',
    'Natural experiment: removal of career-reputational consequences (e.g., legislation protecting whistleblowers who identify obsolete protocols) and observation of whether coordinators begin advocating for updates.',
    'If structural, the constraint''s suppression could be reduced by legal reform. If internalized, the constraint''s suppression persists even after barriers are removed because identity fusion is unbroken — the coordinator''s self-concept remains bound to regime compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_coordinators, empirical, 'Whether identity-lock is structural or internalized in the coordinator role.').

omega_variable(
    novel_flood_scenario_triggering,
    'If a novel-pattern flood (e.g., compound flooding with storm surge + inland runoff, or multi-day inundation outside historical ranges) occurred within the next 5 years, would the regime''s protocols enable effective response or would improvisation during crisis succeed by accident?',
    'Structured scenario analysis by disaster research community comparing protocol-based response to what actual novel flood dynamics would require; expert elicitation from engineers and coordinators on gap assessment.',
    'A large gap would validate the husk reading and support reclassification from rope to piton or snare (if the gap is discovered post-failure). Success in a novel scenario despite protocol gaps would suggest resilience through improvisation rather than through protocol correctness — still extractive (the constraint''s extraction is not earning its keep), but with an accidental backup.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(novel_flood_scenario_triggering, empirical, 'Whether novel flood scenarios would expose the gap between protocol form and operational capacity.').

omega_variable(
    committer_axis_husk_vs_competence,
    'Does the kernel preparedness_transmission instantiate as hollow ritual (husk reading) or as living competence re-validated through exercise (competence reading)?',
    'This omega captures the reading contest itself. The husk reading claims high form compliance + low adaptive capacity + pre-specified-failure-mode detection. The competence reading claims ongoing validation and adaptive mechanisms. Both instantiate the same kernel commitment (transmit preparedness). The structural evidence is the trajectory of theater_ratio, accessibility_collapse, resistance, and field data on protocol-scenario correspondence.',
    'If the husk reading''s structural claims are supported, the constraint''s type should be piton (inertial, theatrical). If the competence reading''s claims are supported, the type should be rope or tangled rope (coordinating, adaptive). The engine does not compute the reading itself — it computes the type from structural metrics. A high theater_ratio (0.68) supports the husk reading; a low theater_ratio would support the competence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axis_husk_vs_competence, conceptual, 'Committer axis: the reading contest between husk and competence instantiations of the preparedness_transmission kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1975, preparedness_transmission__husk_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__husk_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__husk_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__husk_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__husk_reading, theater_ratio, 2020, 0.65).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__husk_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(prep_be_t1975, preparedness_transmission__husk_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__husk_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__husk_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__husk_reading, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__husk_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__husk_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1975, preparedness_transmission__husk_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__husk_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__husk_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__husk_reading, suppression_requirement, 2010, 0.63).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__husk_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__husk_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_transmission__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel is instantiated by three structurally distinct constraint stories: competence_reading (drills re-validate capability), husk_reading (this constraint: ritual form without function), and hybrid_reading (infrastructure competence maintained, civilian coordination decayed). The readings coexist as live positions held by different parties (agency leadership claims competence; disaster researchers and coordinators document hollowing; engineers observe partial degradation). The three constraints form a family linked by their instantiation of the same kernel commitment. The husk reading argues for piton classification due to high theater_ratio, decoupling of form and function, and persistence through institutional inertia rather than functional validation. The competence reading would argue for rope classification based on ongoing validation mechanisms. The network edges enable contamination propagation: if the husk reading's structural claims are validated, the competence reading's classification comes under pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__husk_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
