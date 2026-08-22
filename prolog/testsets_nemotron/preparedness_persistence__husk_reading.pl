% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Memorial Drill and Inspection Regime (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   The flood preparedness drill and inspection regime originated as a
 *   genuine coordination mechanism after the 1955 basin flood. Over seven
 *   decades it has atrophied into memorial performance: exercise scripts are
 *   recycled, inspection checklists audit paperwork compliance rather than
 *   functional capacity, and 'readiness' certification decouples from the
 *   instrumental capabilities that would actually protect populations. The
 *   constraint is claimed as Mountain (immutable preparedness doctrine) by
 *   the administering agencies, but operates as Piton — the form persists
 *   because dismantling it would expose the legitimacy vacuum, not because it
 *   coordinates anything real. Beneficiaries are institutional legitimacy and
 *   budget continuity; victims are the populations who trust the
 *   certification. This is the husk_reading of the preparedness_persistence
 *   kernel: it asserts the regime's coordination function is dead and its
 *   persistence is extractive theater.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.45).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Memorial Drill and Inspection Regime (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_persistence__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'b442f6c4-e781-4258-a08d-e445d15ab36c').
narrative_ontology:cs_kernel_codification('b442f6c4-e781-4258-a08d-e445d15ab36c', formalized).
narrative_ontology:cs_authority_grounding('b442f6c4-e781-4258-a08d-e445d15ab36c', extraction).
narrative_ontology:cs_interpretation_layer_present('b442f6c4-e781-4258-a08d-e445d15ab36c').
narrative_ontology:cs_reading_relation('b442f6c4-e781-4258-a08d-e445d15ab36c', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('b442f6c4-e781-4258-a08d-e445d15ab36c', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('b442f6c4-e781-4258-a08d-e445d15ab36c', foundational, drill_regime_coordination_function_dead).
narrative_ontology:cs_axiom_status(drill_regime_coordination_function_dead, holdable).
narrative_ontology:cs_axiom_grounding('b442f6c4-e781-4258-a08d-e445d15ab36c', drill_regime_coordination_function_dead, empirically_contingent).
narrative_ontology:cs_axiom('b442f6c4-e781-4258-a08d-e445d15ab36c', secondary, institutional_legitimacy_requires_ritual_performance).
narrative_ontology:cs_axiom_status(institutional_legitimacy_requires_ritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('b442f6c4-e781-4258-a08d-e445d15ab36c', institutional_legitimacy_requires_ritual_performance, conventional).
narrative_ontology:cs_reference_frame('b442f6c4-e781-4258-a08d-e445d15ab36c', post_1955_coordination_mandate).
narrative_ontology:cs_drift_state('b442f6c4-e781-4258-a08d-e445d15ab36c', contemporary_instrumental_readiness_gap, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b442f6c4-e781-4258-a08d-e445d15ab36c', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, flood_management_agency).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, regulatory_inspectorate).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, municipal_executive_offices).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, downstream_communities).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, emergency_response_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_response_personnel).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, institutional_legitimacy_through_ritual_compliance).
narrative_ontology:constraint_vindicates(preparedness_persistence__husk_reading, bureaucratic_continuity_as_public_trust).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the drill and inspection regime, sets exercise schedules, certifies compliance, and reports readiness to political leadership. Collects institutional legitimacy and budget stability from the regime's visible activity. Can shift enforcement priorities or redefine metrics when actual capability gaps become visible, but does not bear the cost of operational failure.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, flood_management_agency, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, flood_management_agency, beneficiary).

% Conducts compliance audits of drill execution and inspection records. Their authority and resource justification depend on the regime's formal continuity — finding violations sustains their mandate, but deep structural atrophy is not in their detection remit. They benefit from the regime's persistence as a stable inspection object.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, regulatory_inspectorate, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, regulatory_inspectorate, beneficiary).

% Receive political credit for 'preparedness' metrics without investing in the costly capability maintenance that would make drills meaningful. Can point to completed exercises and clean inspection reports during campaigns. Exit is mobile — they rotate out before atrophy consequences materialize.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, municipal_executive_offices, beneficiary,
    organized, biographical, mobile, local).

% Live in the hazard zone trusting the regime's certified readiness. Bear the full cost of operational failure — evacuation delays, inadequate shelter, failed warning systems — while having no influence over drill design or resource allocation. Exit is constrained: relocation is economically prohibitive and socially disruptive.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, floodplain_residents, payer,
    powerless, biographical, constrained, local).

% Face compound risk from upstream regime failure (dam overtopping, levee breach cascades). Their vulnerability is structurally coupled to the upstream agency's atrophy but they have no representation in that agency's governance. Exit options are similarly constrained by geography and economics.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, downstream_communities, payer,
    powerless, biographical, constrained, regional).

% Execute the drills and know the gap between script and reality. Their professional identity fuses with the regime — they cannot credibly denounce it without undermining their own role. They bear operational risk when atrophy meets event, and gain professional standing from participation. Exit is identity-locked: leaving the role means abandoning the self-concept of 'protector.'
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_response_personnel, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_response_personnel, beneficiary).

% Model actual system capacity vs. certified readiness using instrumental data (sensor networks, historical failure rates, resource audits). They see the full structure but have no enforcement lever. Their analyses are cited in post-event inquiries but do not alter the regime's internal logic.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, independent_hazard_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates inter-agency communication protocols, resource pre-positioning triggers, and public warning chains — the drill script is the only shared reference that aligns disparate responders before an event.
% TRANSFER_FUNCTION: Transfers institutional legitimacy, budget authority, and political credit from the performance of readiness to the agencies that administer the regime; transfers operational risk (evacuation failure, shelter collapse, warning gaps) to residents and responders who cannot audit the regime's depth.
% ABSENT_VOICES: Future flood victims — the people who will occupy the floodplain when atrophy meets event — are structurally absent. Also absent: infrastructure maintainers (levee crews, pump station operators) whose practical knowledge contradicts drill outcomes but who are not consulted in exercise design.
% DISAPPEARANCE_RATIONALE: If the drill regime vanished overnight, agencies would lose their primary legitimacy artifact and coordination scaffold. Residents would lose the (false) assurance of managed response. Responders would revert to ad-hoc coordination. The flood management apparatus would reorganize around instrumental capacity metrics or collapse into jurisdictional fragmentation — the world rearranges because the regime is the load-bearing fiction holding the institutional structure together.
% FOUNDING_PROBLEM: After the 1955 basin flood, the regime was built to solve: fragmented warning systems, uncoordinated evacuation routes, and no shared exercise doctrine across municipal, state, and federal responders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by the 1998 National Research Council review (independent body) which documented that communication interoperability, standardized incident command, and GIS-based evacuation modeling had rendered the original coordination gaps obsolete. The agencies' own after-action reports from 2000–2010 show drill scenarios unchanged while actual capability requirements shifted to cyber-physical infrastructure resilience — a gap the regime does not address.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the widening gap between the regime's resource consumption (personnel hours, exercise budgets, inspection apparatus) and its marginal contribution to actual flood survival probability. Theater ratio (0.72) is high because the majority of drill activity performs compliance rather than stress-testing capability — scripts avoid failure modes, inspections check documentation not hardware, after-action reports emphasize narrative over corrective action. Suppression (0.45) is moderate: the regime does not actively prevent alternatives (communities can self-organize, analysts can publish), but it monopolizes the legitimacy space so alternatives cannot achieve institutional recognition. Accessibility collapse (0.58) is partial: instrumental alternatives exist (sensor-driven early warning, distributed evacuation modeling) but are structurally excluded from the certified readiness framework. Resistance (0.28) is low because the regime's beneficiaries control the metrics and the victims lack leverage — resistance manifests as post-event inquiries, not regime change.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent types: from the agency seats, the regime reads as Scaffold (transitional coordination with declared but ignored sunset); from resident/responder seats, it reads as Tangled Rope (real coordination vestige + asymmetric extraction); from the analytical seat, it reads as Piton (atrophied function maintained by institutional inertia). The claim (Piton) is the analytical reading; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda-setters (flood_management_agency, regulatory_inspectorate) sit near the beneficiary pole (d ~0.15): they collect legitimacy, budget, and authority from the regime's visible operation. Municipal executives are beneficiaries with mobile exit (d ~0.2). Floodplain residents and downstream communities are targets with constrained exit (d ~0.85): they bear the full risk of atrophy with no influence on the regime. Emergency responders are identity-locked payers who also collect professional standing (d ~0.55): their fused identity prevents clean exit but their operational knowledge makes them aware of the extraction. Independent analysts are analytical observers (d ~0.5) with no stake in the regime's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1955 coordination vacuum) is dead — solved by technological and doctrinal evolution the regime did not drive. The regime persists because its form IS the legitimacy artifact for the agencies that administer it. Mandatrophy is resolved: the mandate has outlived its function, but the constraint remains because no beneficiary bears the cost of fixing it and no victim can force the fix. Theater ratio rise over the interval (0.12→0.72) tracks the mandatrophy progression: as the coordination function atrophied, performative maintenance intensified to preserve the legitimacy stream.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_boundary,
    'Is the regime''s coordination function completely dead (husk) or partially alive in technical subdomains (hybrid)?',
    'Component-level capability audit: test engineering inspection outcomes against actual infrastructure performance; test evacuation drill outcomes against modeled vs. actual egress times. If any subdomain shows drill-to-outcome correlation, hybrid_reading gains ground; if none do, husk_reading is structurally verified.',
    'If hybrid, extractiveness is overestimated in this reading (some theater is real coordination cost); if pure husk, the full extractiveness is rent. Changes classification from Piton (husk) toward Tangled Rope (hybrid) for the technical subdomains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_boundary, empirical, 'Whether any drill/inspection component retains instrumental coordination value').

omega_variable(
    identity_lock_mechanism,
    'Is emergency responder identity-lock professional (career path dependence), relational (team cohesion), ideological (protector self-concept), or institutional (organization has become its function)?',
    'Longitudinal cohort study of responders who rotate out of flood role: track whether drill-cynicism predicts departure, role change, or internalization. Compare with responders in domains without ritualized drill regimes.',
    'If professional/relational, identity_lock is reversible with career restructuring; if ideological/institutional, it persists post-exit and amplifies effective suppression beyond structural measures. Determines whether responder seat is a potential reform lever or a locked-in extraction surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Mechanism binding emergency responders to the atrophied regime').

omega_variable(
    kernel_framing_ambiguity,
    'Does the preparedness_persistence kernel refer to the drill regime itself, the broader flood management architecture, or the legitimacy claim that ''preparedness is managed''?',
    'Trace citation networks in agency authorizations, legislative mandates, and post-event inquiries: which object is treated as the immutable commitment? If the kernel is the regime, readings differ on its state; if the kernel is the architecture, readings differ on its boundaries; if the kernel is the legitimacy claim, readings differ on what sustains it.',
    'Changes the structural delta between readings: regime-level kernel means readings are state-variants of one constraint; architecture-level means they partition the constraint; legitimacy-claim means they are competing authority-groundings for the same extraction stream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'What the contested kernel actually names — regime, architecture, or legitimacy claim').

omega_variable(
    suppression_internalization,
    'Is the moderate suppression (0.45) structural (regulatory barriers to alternative readiness certification) or internalized (residents believe drills equal safety, responders believe critique undermines mission)?',
    'Post-exit suppression trajectory: survey residents who relocated from floodplains — do they retain the ''managed preparedness'' belief? Track responders who transfer to non-drill domains — does drill-cynicism persist? If suppression persists after structural removal, internalized component is significant.',
    'If substantially internalized, effective suppression is higher than 0.45 — the constraint carries its own enforcement into the cognitive layer. This would push classification toward Snare from Piton (theatrical maintenance with internalized capture).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in the drill regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t1955, preparedness_persistence__husk_reading, theater_ratio, 1955, 0.12).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t1975, preparedness_persistence__husk_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t1995, preparedness_persistence__husk_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t2005, preparedness_persistence__husk_reading, theater_ratio, 2005, 0.58).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t2015, preparedness_persistence__husk_reading, theater_ratio, 2015, 0.67).
narrative_ontology:measurement(preparedness_persistence__husk_reading_tr_t2025, preparedness_persistence__husk_reading, theater_ratio, 2025, 0.72).

% Extraction over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t1955, preparedness_persistence__husk_reading, base_extractiveness, 1955, 0.15).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t1975, preparedness_persistence__husk_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t1995, preparedness_persistence__husk_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t2005, preparedness_persistence__husk_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t2015, preparedness_persistence__husk_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(preparedness_persistence__husk_reading_be_t2025, preparedness_persistence__husk_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t1955, preparedness_persistence__husk_reading, suppression_requirement, 1955, 0.18).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t1975, preparedness_persistence__husk_reading, suppression_requirement, 1975, 0.28).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t1995, preparedness_persistence__husk_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t2005, preparedness_persistence__husk_reading, suppression_requirement, 2005, 0.41).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t2015, preparedness_persistence__husk_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(preparedness_persistence__husk_reading_su_t2025, preparedness_persistence__husk_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, flood_early_warning_system).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, levee_certification_regime).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, evacuation_route_planning).

% DUAL FORMULATION NOTE:
% This is the husk_reading of the preparedness_persistence kernel. The competence_reading (live exercised knowledge) and hybrid_reading (stratified competence/ritual) are sibling constraints with distinct ε, stakeholder structures, and types. This reading asserts the coordination function is dead; competence_reading asserts it is live; hybrid_reading partitions it. All three share the same institutional referent but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__husk_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
