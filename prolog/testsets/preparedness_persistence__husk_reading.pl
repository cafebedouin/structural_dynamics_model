% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Preparedness Husk: Ritualized Drills and Inspections as Memorial Performance
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   A jurisdiction subject to recurring floods maintains a formal
 *   preparedness system: annual evacuation drills, annual structural
 *   inspections, compliance reports sent to supervisory authorities. The form
 *   persists: drills are conducted, inspections are completed, documents are
 *   filed. But institutional memory practitioners and independent
 *   emergency-response researchers have documented that actual evacuation
 *   coordination capability has atrophied over decades — the people
 *   conducting the drills lack hands-on experience with the coordination
 *   machinery, responders lack cross-training, critical information is no
 *   longer transmitted between senior and junior personnel during drill-time,
 *   and command structures exist in writing but not in muscle memory or
 *   real-time rehearsal. The constraint persists because the administrative
 *   hierarchy benefits from the appearance of preparedness (legitimacy with
 *   supervisory authority, public confidence, career advancement through
 *   visible compliance) while bearing none of the cost of failure. The
 *   flood-exposed population depends on evacuation capacity but has no seat
 *   in defining what preparedness means or evaluating whether it is real.
 *   This reading asserts the constraint is Piton: atrophied institutional
 *   function maintained theatrically because the cost of fixing it exceeds
 *   what any administrator bears alone, but the cost of not fixing it
 *   (population risk in a flood event) falls on the powerless and trapped.
 *
 * KEY AGENTS:
 *   - administrative_hierarchy: agenda_setter with institutional power; defines and enforces the drill/inspection regime to satisfy upward accountability
 *   - emergency_management_personnel: moderate power, constrained exit; careers advance through ritual compliance, creating incentive alignment with the form over substance
 *   - flood_exposed_population: powerless, trapped; bear the ultimate cost if the constraint fails and evacuation is needed
 *   - supervisory_authority: institutional observer; accepts formal compliance reports without independent verification of actual readiness
 *   - institutional_memory_practitioners: excluded voices; would testify to atrophy but are not seated in preparedness evaluation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Preparedness Husk: Ritualized Drills and Inspections as Memorial Performance").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '19f049c2-8712-476e-8a31-6392e7e82480').
narrative_ontology:cs_kernel_codification('19f049c2-8712-476e-8a31-6392e7e82480', fixed_text).
narrative_ontology:cs_authority_grounding('19f049c2-8712-476e-8a31-6392e7e82480', extraction).
narrative_ontology:cs_interpretation_layer_present('19f049c2-8712-476e-8a31-6392e7e82480').
narrative_ontology:cs_reading_relation('19f049c2-8712-476e-8a31-6392e7e82480', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('19f049c2-8712-476e-8a31-6392e7e82480', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('19f049c2-8712-476e-8a31-6392e7e82480', foundational, atrophy_is_real_and_measurable).
narrative_ontology:cs_axiom_status(atrophy_is_real_and_measurable, holdable).
narrative_ontology:cs_axiom_grounding('19f049c2-8712-476e-8a31-6392e7e82480', atrophy_is_real_and_measurable, empirically_contingent).
narrative_ontology:cs_axiom('19f049c2-8712-476e-8a31-6392e7e82480', foundational, form_decoupled_from_function).
narrative_ontology:cs_axiom_status(form_decoupled_from_function, holdable).
narrative_ontology:cs_axiom_grounding('19f049c2-8712-476e-8a31-6392e7e82480', form_decoupled_from_function, empirically_contingent).
narrative_ontology:cs_reference_frame('19f049c2-8712-476e-8a31-6392e7e82480', post_disaster_knowledge_preservation).
narrative_ontology:cs_drift_state('19f049c2-8712-476e-8a31-6392e7e82480', contemporary_generational_atrophy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19f049c2-8712-476e-8a31-6392e7e82480', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, administrative_hierarchy).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, flood_exposed_population).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio of 0.71 at interval end (rising from 0.48) captures the constraint's defining feature: the constraint persists increasingly as performance rather than function. Extractiveness is high (0.62) because the administrative hierarchy extracts institutional legitimacy and political safety from the appearance of preparedness without bearing the cost of actual readiness — the extracted value is purely positional (being seen as prepared by supervisory authority and public) not operational (having an evacuation system that works). Suppression is moderate (0.58) because the constraint is maintained not through external coercion but through the structural architecture of incentives: career advancement through compliance, supervisory authority's acceptance of formal reports, and public confidence built on visible drills and signed inspection forms. Resistance is low (0.42) because the powerless flood-exposed population has no organized counter-position and institutional memory practitioners are excluded from the deliberative space where alternatives would be voiced. The measurements span 40 years and track the trajectory: extractiveness rises as administrative overhead grows and real competence investment stalls; theater_ratio rises as the functional link between drills and actual readiness weakens.
 *
 * PERSPECTIVAL GAP:
 *   From the administrative hierarchy's seat, the constraint is coordination: maintain public order, ensure disaster readiness within budget constraints, satisfy regulatory compliance. From the flood-exposed population's seat, the constraint is pure extraction: they bear the risk (trapped in flood zone, relying on evacuation capacity) and receive theater (drills that don't prepare responders, inspections that don't verify actual capability). From supervisory authority's seat, the constraint appears as genuine coordination (they receive reports of compliance and certify preparedness). The engine computes this divergence from the structural data: the hierarchy's beneficiary status, the population's victim status, and their divergent power and exit options drive d-values toward opposite ends of the spectrum, yielding different type-classifications at each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative hierarchy: beneficiary role (collects legitimacy) + institutional power + arbitrage exit (can change the system at will) → d near 0.1 (full beneficiary end). Flood-exposed population: payer role (bears the readiness risk) + powerless + trapped exit (geographic and economic immobility) → d near 0.95 (full target end). Emergency management personnel: mixed (benefit from career advancement through compliance, pay by being bound to a non-functional framework) + moderate power + constrained exit → d near 0.55 (symmetric). Supervisory authority: observer role, analytical power, analytical exit → d not computed (external seat). Institutional memory practitioners: excluded role, would be targets if seated, powerless + trapped → d would be near 0.90 if they had formal standing (the exclusion itself is structural suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: after the prior flood, knowledge preservation through coordinated drills and training prevented generational loss of evacuation expertise. That problem is now dead — the current system no longer addresses knowledge preservation and instead addresses the problem of demonstrating compliance with a formalized schedule to supervisory authority. The constraint meets all Piton criteria: (1) it persists through inertia and theatrical maintenance rather than genuine coordination or extraction of rents by a capturer; (2) no party benefits enough to maintain it if challenged (the hierarchy only benefits from continuity, not from the system's actual function); (3) no party bears enough concentrated cost to fix it alone (the population bears the risk but has no power; the hierarchy bears only legitimacy cost, not operational cost); (4) the cost to alter the system (retraining, immersive drill design, mentorship infrastructure) exceeds what any single administrator will invest. The divergence between claimed founding problem (knowledge preservation) and actual institutional function (compliance demonstration) is the canonical Piton signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_natural_drift,
    'Is the measured decline in actual evacuation readiness the result of deliberate organizational non-investment in genuine competence, or a natural attrition of institutional knowledge over generational turnover?',
    'Compare organizations where the founding problem (knowledge preservation after disaster) is actively resourced — mentorship programs, immersive training, simulation with real-scenario complexity — to those where drills are performed as scheduled ritual. If actively-resourced organizations maintain readiness while ritual-only organizations show uniform atrophy, atrophy is deliberate structure choice, not drift.',
    'If deliberate: the constraint''s victim and beneficiary roles are clear (hierarchy benefits from low-cost legitimacy theater; population bears the readiness risk). If natural drift: the constraint might be tragic accident rather than structural extraction, shifting the type toward Piton-of-institutional-inertia rather than Piton-of-maintenance-capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_natural_drift, empirical, 'Whether atrophy is deliberate organizational choice or emergent knowledge loss.').

omega_variable(
    competing_readings_empirical_test,
    'What would falsify each reading? If an actual flood event occurs and evacuation succeeds, does that support competence_reading? If it fails and reveals atrophy, does that support husk_reading?',
    'Post-disaster assessment: measure actual evacuation outcomes against pre-disaster drill protocols and inspection certifications. Conduct forensic interviews with responders about their real-time knowledge, decision-making, and coordination under stress. Identify specific points where the drills did or did not prepare them.',
    'A successful evacuation with revealed atrophy in non-critical subsystems would support hybrid_reading (stratified competence). A failed evacuation traceable to missing knowledge tested in no drill would support husk_reading. Successful evacuation with drill-to-reality correspondence would support competence_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_readings_empirical_test, empirical, 'Empirical test distinguishing readings through actual disaster response outcomes.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is preparedness a single unified constraint (one kernel, three readings of the same thing) or three distinct constraints (competence, husk, hybrid are separate things evaluated under different observables)?',
    'Test ε-invariance: does the measure of extractiveness change when the observable shifts from ''does the drill demonstrate current knowledge'' to ''does the protocol match actual organizational capability'' to ''are critical subsystems tested, others not''? If ε changes fundamentally with the observable choice, the readings instantiate different constraints, not different readings of one kernel.',
    'If different constraints: decompose into three JSON files per the ε-invariance principle, linked via network.affects_constraints, not treated as readings of a single kernel. If one kernel: the reading-relations structure below is correct and the three readings coexist as competing legitimate frames of the same institutional commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether preparedness is one contested kernel or three distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__husk_reading, theater_ratio, 8, 0.55).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__husk_reading, theater_ratio, 16, 0.63).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.68).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__husk_reading, theater_ratio, 32, 0.7).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__husk_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__husk_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__husk_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__husk_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__husk_reading, base_extractiveness, 40, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three constraint stories representing three legitimate but contested readings of how to understand flood-disaster preparedness systems. Husk_reading asserts that form has decoupled from functional capacity and the system now persists as Piton (atrophied institutional function maintained theatrically). Competence_reading asserts that drills and inspections maintain real operational readiness (Rope or Tangled_Rope depending on how enforcement is measured). Hybrid_reading asserts that some subsystems remain genuinely competent while others have ritualized. Each reading instantiates a different constraint with potentially different ε, beneficiary/victim structures, and classifications. They are linked via network.affects_constraints to signal that they are readings of the same contested kernel, not independent constraints. The contest between them is not resolvable in a single constraint story; the corpus carries all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__husk_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
