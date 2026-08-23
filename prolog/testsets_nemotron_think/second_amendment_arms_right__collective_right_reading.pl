% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   This constraint story represents the collective right reading of the
 *   Second Amendment: the Amendment protects the authority of states to
 *   maintain organized militias, not an individual right to possess arms
 *   outside militia service. Under this reading, the constraint is a
 *   structural feature of the federal design — a fixed limit on federal power
 *   over state military capacity. The reading claims mountain status
 *   (emerges_naturally: true) as the original constitutional settlement.
 *   However, identifiable beneficiaries (state governments, state militias)
 *   and victims (federal government, individuals outside militia) exist,
 *   creating a false-summit-mountain candidate. The low authored
 *   extractiveness (0.25) reflects the reading's view that the constraint
 *   itself does not extract; it merely allocates authority. The victims
 *   experience extraction via the plenary state regulation this reading
 *   permits, but that extraction is attributed to state police power, not the
 *   Second Amendment constraint itself.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary and agenda-setter (institutional/arbitrage) — holds the protected militia authority
 *   - state_militias: Direct beneficiary (organized/constrained) — exercises the guaranteed force
 *   - federal_government: Primary payer (institutional/constrained) — constitutionally barred from disarming state militias
 *   - individuals_outside_militia: Payer (powerless/trapped) — subject to plenary regulation with no constitutional exit
 *   - legal_scholars_courts: Observer (institutional/analytical) — interprets and applies the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.25).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, mountain).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:emerges_naturally(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, 'a97f8499-4067-4228-a31e-370e6b6c2df6').
narrative_ontology:cs_kernel_codification('a97f8499-4067-4228-a31e-370e6b6c2df6', fixed_text).
narrative_ontology:cs_authority_grounding('a97f8499-4067-4228-a31e-370e6b6c2df6', lineage).
narrative_ontology:cs_interpretation_layer_present('a97f8499-4067-4228-a31e-370e6b6c2df6').
narrative_ontology:cs_reading_relation('a97f8499-4067-4228-a31e-370e6b6c2df6', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('a97f8499-4067-4228-a31e-370e6b6c2df6', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('a97f8499-4067-4228-a31e-370e6b6c2df6', foundational, second_amendment_protects_state_militia_only).
narrative_ontology:cs_axiom_status(second_amendment_protects_state_militia_only, holdable).
narrative_ontology:cs_axiom_grounding('a97f8499-4067-4228-a31e-370e6b6c2df6', second_amendment_protects_state_militia_only, conventional).
narrative_ontology:cs_axiom('a97f8499-4067-4228-a31e-370e6b6c2df6', secondary, no_individual_right_outside_militia_service).
narrative_ontology:cs_axiom_status(no_individual_right_outside_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('a97f8499-4067-4228-a31e-370e6b6c2df6', no_individual_right_outside_militia_service, conventional).
narrative_ontology:cs_reference_frame('a97f8499-4067-4228-a31e-370e6b6c2df6', original_meaning_framework).
narrative_ontology:cs_drift_state('a97f8499-4067-4228-a31e-370e6b6c2df6', contemporary_individual_right_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a97f8499-4067-4228-a31e-370e6b6c2df6', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_militias).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individuals_outside_militia).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, original_meaning_second_amendment).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_militia_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the constitutional right to maintain organized militias free from federal disarmament. They set militia policy, appoint officers, and benefit from the structural guarantee that the federal government cannot abolish their militia authority. They can influence the constraint through political appointments and federalism litigation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary).

% The organized militia forces (National Guard) that directly exercise the protected authority. They receive federal funding and equipment but remain under state command unless federalized. Their existence and state-control are the operational realization of the collective right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_militias, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of being constitutionally barred from disarming or abolishing state militias. This limits federal military centralization and emergency powers. The constraint is experienced as a structural limit on federal sovereignty over domestic force.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, payer,
    institutional, generational, constrained, national).

% Individuals who wish to own or carry arms outside of organized militia service. Under this reading they have no constitutional claim against state or federal regulation. They are subject to plenary police-power regulation with no exit from the constitutional interpretation short of amendment or judicial reversal. Their situation is defined by the absence of a protected individual right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individuals_outside_militia, payer,
    powerless, biographical, trapped, national).

% The interpretive community (judges, academics, advocates) that debates and applies the collective right reading. They do not directly collect or pay from the constraint but determine its operational meaning through precedent and scholarship. Their exit is analytical — they can change interpretive frameworks.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, legal_scholars_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the federal-state military balance by guaranteeing states a reserved force (militia) that the federal government cannot disarm, preventing total federal monopoly on organized violence.
% TRANSFER_FUNCTION: Transfers regulatory authority over non-militia firearm possession from the constitutional level to the state police power, enabling plenary state regulation of individual ownership without federal constitutional obstacle.
% ABSENT_VOICES: Founding-era Anti-Federalists who feared standing armies but also wanted individual arms guarantees; contemporary originalist scholars who argue the text protects individual ownership; individuals in jurisdictions with strict gun laws who would claim a personal right if the reading were different.
% DISAPPEARANCE_RATIONALE: If the collective right reading vanished overnight, the constitutional barrier to federal disarmament of state militias would disappear, altering the federal-state military balance. Simultaneously, the textual basis for denying an individual right would be removed, likely triggering a shift to an individual right framework (as occurred in Heller) and invalidating many state regulations.
% FOUNDING_PROBLEM: The founding problem was the Anti-Federalist fear that the new Constitution's grant of power to raise a federal army and control the militia would leave states defenseless against federal tyranny. The Second Amendment was adopted to guarantee that states could maintain their own militias as a check on federal military power.
% FOUNDING_PROBLEM_CORROBORATION: Historical records (Madison's correspondence, state ratification debates, Militia Acts of 1792) corroborate the state-militia purpose. However, the individual right reading cites the same sources plus English Bill of Rights antecedents and early commentaries (Tucker, Rawle) to argue the founding problem also included individual self-defense. No consensus exists outside the benefiting parties (state governments and gun-control advocates).
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, ExtMetricName, E),
    domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(second_amendment_arms_right__collective_right_reading),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint operates as a limit on federal power rather than an active extraction mechanism. Suppression is low because the constraint does not actively coerce individuals; it merely fails to shield them from state regulation. Theater ratio is minimal because the collective right reading was the dominant judicial interpretation for most of U.S. history with little performative maintenance. Accessibility collapse is high (0.85) because the constitutional text, once interpreted as collective, leaves no alternative individual right claim within the framework. Resistance is low (0.2) historically until the late 20th century individual rights movement. The measurement series show slight increases in extractiveness and theater ratio after 1900 as the individual right reading gained scholarly traction, requiring more interpretive work to maintain the collective reading.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat classifications: for state_governments (d near 0.0) the constraint appears as a protective mountain; for federal_government (d near 0.5) a symmetric coordination limit; for individuals_outside_militia (d near 1.0) the constraint's denial of protection enables high effective extraction by states, potentially computing as snare from their seat. The claimed_type (mountain) reflects the reading's self-understanding; the computed per-seat types will reveal the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state_governments, state_militias) collect the constitutional guarantee — directionality damped toward subsidy. Victims (federal_government, individuals_outside_militia) bear costs: federal loses militia control, individuals lose constitutional shield. Exit options differentiate: federal_government has constrained exit (amendment, appointment power); individuals_outside_militia are trapped (no constitutional exit, amendment practically impossible). This produces high effective extraction for individuals despite low base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal military tyranny) is contested as live or dead. If dead, the constraint persists as a vestigial structure (piton candidate). If live, it remains coordination (rope/mountain). The collective right reading's proponents argue the problem is live (federal overreach risk); opponents argue it is dead (federalism protected by other means, militia obsolete). The mandatrophy_resolved flag is not set because the status is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_ambiguity,
    'Does the Second Amendment''s original public meaning protect only state militia authority, or also an individual right to arms?',
    'Converging historical evidence from founding-era sources (ratification debates, state constitutions, legal commentaries, linguistic corpus analysis) that could establish a consensus original meaning.',
    'If individual right is confirmed as original meaning, the collective right reading is a constructed false summit (FSM triggers). If collective right is confirmed, the individual right reading is the constructed overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_meaning_ambiguity, empirical, 'Whether the collective right reading reflects genuine original meaning or a later doctrinal construction').

omega_variable(
    militia_obsolescence,
    'Has the founding problem (state militia as check on federal power) become obsolete given the National Guard''s dual state-federal status and modern military reality?',
    'Political science analysis of whether state military forces retain any independent checking function against federal tyranny, or whether the National Guard''s federalization pipeline has eliminated the structural guarantee.',
    'If obsolete, the constraint is a piton (degraded coordination maintained by inertia). If still functional, it remains live coordination (rope/mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_obsolescence, conceptual, 'Whether the coordination function the constraint was built for still operates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 0, 233).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t0, second_amendment_arms_right__collective_right_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t50, second_amendment_arms_right__collective_right_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t100, second_amendment_arms_right__collective_right_reading, theater_ratio, 100, 0.05).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t150, second_amendment_arms_right__collective_right_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t200, second_amendment_arms_right__collective_right_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_tr_t233, second_amendment_arms_right__collective_right_reading, theater_ratio, 233, 0.1).

% Extraction over time
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t0, second_amendment_arms_right__collective_right_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t50, second_amendment_arms_right__collective_right_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t100, second_amendment_arms_right__collective_right_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t150, second_amendment_arms_right__collective_right_reading, base_extractiveness, 150, 0.22).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t200, second_amendment_arms_right__collective_right_reading, base_extractiveness, 200, 0.25).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_be_t233, second_amendment_arms_right__collective_right_reading, base_extractiveness, 233, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t0, second_amendment_arms_right__collective_right_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t50, second_amendment_arms_right__collective_right_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t100, second_amendment_arms_right__collective_right_reading, suppression_requirement, 100, 0.1).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t150, second_amendment_arms_right__collective_right_reading, suppression_requirement, 150, 0.12).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t200, second_amendment_arms_right__collective_right_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement(second_amendment_arms_right__collective_right_reading_su_t233, second_amendment_arms_right__collective_right_reading, suppression_requirement, 233, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% BGS-style decomposition: the Second Amendment kernel splits into three readings with distinct ε and stakeholder structures. This reading (collective) has low base extractiveness but high effective extraction for trapped individuals. The individual right reading has higher base extractiveness (blocks regulations) but different beneficiary/victim structure. The civic republican reading sits between. All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__collective_right_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
