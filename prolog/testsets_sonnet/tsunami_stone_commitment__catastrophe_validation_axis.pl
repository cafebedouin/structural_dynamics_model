% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: The 2011 Tsunami as Decisive Empirical Test of the Stone Markers' Warning
 *   domain: disaster_anthropology/commitment_system_analysis/institutional_memory
 *
 * SUMMARY:
 *   Along the Sanriku coast of northeastern Japan, dozens of centuries-old
 *   stone markers ('tsunami stones') were erected after prior devastating
 *   tsunamis (notably 1896 and 1933), inscribed with warnings such as 'do not
 *   build homes below this point.' The 2011 Tohoku earthquake and tsunami
 *   provided a rare, sharp, binary empirical test: villages that built above
 *   the stone markers survived largely unscathed; villages that built below
 *   them, or that had allowed settlement to creep below the marked line over
 *   the intervening decades, suffered catastrophic losses. This constraint
 *   isolates the tsunami event itself — not the stone's inscribed content,
 *   not the social practice of heeding it — as the physical, once-only
 *   adjudicating mechanism that produced legible, low-noise validation data
 *   about whether the warning had been retained as living practice or had
 *   decayed into ornament. The event is a geological/oceanographic occurrence
 *   indifferent to human institutions; its evidentiary value is what feeds
 *   the sibling readings (behavioral_competence_reading and
 *   commemorative_husk_reading), which classify what the stones' social
 *   function actually was. This story is the adjudication device, not the
 *   adjudicated claim.
 *
 * KEY AGENTS:
 *   - tsunami_stone_preservation_advocates: Primary beneficiary (organized/mobile) — use the validated event to argue for renewed marker maintenance and disaster education funding
 *   - disaster_risk_reduction_researchers: Primary beneficiary (institutional/analytical) — use the event as a natural experiment for intergenerational risk-communication theory
 *   - surviving_coastal_villages_above_marker: Analytical observer with direct stake (moderate/trapped-to-constrained) — experienced the binary outcome directly as survival
 *   - destroyed_coastal_villages_below_marker: Analytical observer with direct stake (powerless/trapped) — experienced the binary outcome directly as loss
 *   - seismological_and_oceanographic_record: Analytical observer — the underlying geophysical process that produced the test, indifferent to any human reading of it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.03).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.02).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.03).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "The 2011 Tsunami as Decisive Empirical Test of the Stone Markers' Warning").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system_analysis/institutional_memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'c032aa82-816f-45d4-a293-169248b25659').
narrative_ontology:cs_kernel_codification('c032aa82-816f-45d4-a293-169248b25659', fixed_text).
narrative_ontology:cs_authority_grounding('c032aa82-816f-45d4-a293-169248b25659', practice).
narrative_ontology:cs_reading_relation('c032aa82-816f-45d4-a293-169248b25659', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('c032aa82-816f-45d4-a293-169248b25659', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('c032aa82-816f-45d4-a293-169248b25659', foundational, physical_event_is_observer_independent_arbiter).
narrative_ontology:cs_axiom_status(physical_event_is_observer_independent_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('c032aa82-816f-45d4-a293-169248b25659', physical_event_is_observer_independent_arbiter, empirically_contingent).
narrative_ontology:cs_axiom('c032aa82-816f-45d4-a293-169248b25659', secondary, evidentiary_value_separable_from_social_function_claim).
narrative_ontology:cs_axiom_status(evidentiary_value_separable_from_social_function_claim, holdable).
narrative_ontology:cs_axiom_grounding('c032aa82-816f-45d4-a293-169248b25659', evidentiary_value_separable_from_social_function_claim, conventional).
narrative_ontology:cs_reference_frame('c032aa82-816f-45d4-a293-169248b25659', physical_inundation_record_as_arbiter).
narrative_ontology:cs_drift_state('c032aa82-816f-45d4-a293-169248b25659', post_2011_event, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c032aa82-816f-45d4-a293-169248b25659', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_preservation_advocates).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, disaster_risk_reduction_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, surviving_coastal_villages_above_marker).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_warning_transmission_hypothesis).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, stone_marker_line_as_predictive_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, local historical societies, and municipal cultural-heritage offices point to the 2011 outcomes to argue for funding to maintain, restore, and erect new tsunami stones. They did not cause the tsunami and derive no extraction from its occurrence, but the sharp binary evidence it produced is the strongest argument in their advocacy toolkit; without it, their case for marker preservation would rest on weaker, pre-2011 anecdote.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_preservation_advocates, beneficiary,
    organized, generational, mobile, regional).

% Academic and international-agency researchers (UNDRR-adjacent, university disaster-studies departments) use the 2011 stone-marker outcomes as a widely cited natural experiment in intergenerational risk communication. They gain publications, grant citations, and policy influence from the event's evidentiary clarity, but did not produce or control the tsunami itself.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_risk_reduction_researchers, beneficiary,
    institutional, civilizational, analytical, global).

% Communities such as Aneyoshi, built above the inscribed line, experienced direct survival as the outcome of the physical wave failing to reach their elevation. They are living evidence in the natural experiment but had no ability to alter whether the tsunami occurred or how high it rose; their situation illustrates the test's outcome rather than participating in producing it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, surviving_coastal_villages_above_marker, observer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__catastrophe_validation_axis, surviving_coastal_villages_above_marker, beneficiary).

% Communities that had settled below historical marker lines, often because economic and land-use pressures over decades pushed development seaward despite the inscribed warnings, suffered catastrophic loss of life and property in 2011. They bore the outcome of the physical event directly; this constraint (the event itself) did not extract from them any more than it benefited the villages above the line — the wave's elevation was indifferent to settlement patterns. Their loss is the evidentiary data point, not a consequence of this constraint's operation.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, destroyed_coastal_villages_below_marker, observer,
    powerless, biographical, trapped, local).

% The physical record of plate subduction, wave generation, and inundation height is not an actor but the substrate the constraint is made of; it is included for completeness as the non-agent entity whose behavior is the constraint itself.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, seismological_and_oceanographic_record, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, seismological_and_oceanographic_record).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__catastrophe_validation_axis, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — a tsunami is not a coordination mechanism. Its function within this analysis is purely evidentiary: it produced a legible, hard-to-dispute natural experiment separating villages that retained the warning line from those that did not.
% TRANSFER_FUNCTION: The physical event transfers nothing between parties in the extractive sense; it transferred destructive kinetic and hydraulic energy onto the coastline according to elevation, indifferent to any human institution. What IS transferred, downstream, is evidentiary capital: the clarity of the 2011 outcome flows to preservation advocates and researchers as usable proof in their respective arguments about the stones' social function.
% ABSENT_VOICES: The destroyed villages below the marker line cannot testify to why the marker was not heeded (whether from ignorance, economic pressure, or eroded transmission) — their absence from the historical record on THAT specific question is what makes this reading (isolating the physical event as adjudicator) necessary rather than presuming an answer. Their voice would matter enormously to the sibling readings but is largely unavailable except through survivor accounts and municipal records.
% DISAPPEARANCE_RATIONALE: If this constraint 'disappeared' — meaning if the 2011 tsunami had simply not happened, or if we strike the event from consideration as an evidentiary object — the underlying geophysical hazard along the Sanriku coast would remain unchanged, the stone markers would remain in place, and the sibling readings' dispute over the markers' social function would simply lack this particular piece of dramatic corroborating evidence. No arrangement of human institutions was built around the tsunami's occurrence as such; the world's physical and social structures do not depend on this event having happened for their own coherence, only for evidentiary support in an unrelated debate.
% FOUNDING_PROBLEM: This is not an arrangement 'built to solve' a problem in the way an institution is — it is a natural hazard event. Read charitably, the closest analog to a 'founding problem' is the geophysical process of subduction-zone stress accumulation and release, which exists entirely independent of any human commitment, warning system, or institutional design.
% FOUNDING_PROBLEM_CORROBORATION: Independent seismological and plate-tectonic science (JAMSTEC, USGS, and international seismology consortia with no stake in either the preservation-advocacy or academic-DRR beneficiary groups) corroborates that the Japan Trench subduction zone remains an active, live hazard-generating system; this corroboration comes from parties entirely outside any group that benefits from the tsunami's evidentiary value.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both authored near zero because the event itself — plate subduction, tsunami generation, wave inundation up a coastal profile — is a physical process that would occur identically regardless of any human commitment system, any inscription, or any observer. No party collects rent from the tsunami occurring; no party enforces its occurrence. Accessibility collapse is high (0.9): once a coastline's elevation profile and marker placement are known, there is no alternative account of where the water reached — the physical record is close to unambiguous. Resistance is near-zero because no faction disputes that the wave struck to a measurable elevation; what is contested (in the sibling readings) is what that fact IMPLIES about the stones' social function, not the fact itself. The measurements show near-flat extraction and theater trajectories across the full interval (1896-2011) because the geophysical constraint's operation did not change; the tiny uptick reflects growing scientific/preservation institutional activity layered atop the physical event as its evidentiary value became recognized, not a change in the event's own structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries here are analytically distinct from the event's own operation: preservation advocates and DRR researchers benefit from the EXISTENCE of a clean natural experiment, not from the tsunami itself. This is precisely the FSM-candidate pattern the schema flags — a mountain with declared beneficiaries — and is documented via omega below. No victims are declared for THIS reading: the villages destroyed below the marker line are casualties of the tsunami as a physical event, not of this commitment-system constraint (the tsunami-as-mountain does not extract from them; it simply occurred). Assigning them as 'victims' would conflate the physical event with the social commitment systems (the sibling readings) that determined whether the warning was heeded. That assignment belongs to the sibling stories, not here.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading cannot become mandatrophic because it names no ongoing mandate — it names a singular, non-repeating historical event serving as a measurement instrument. The 'mandate' (if any) belongs to the memorial/warning practice itself, which is adjudicated by the sibling readings. Decomposing the kernel into three stories prevents the classic collapse-into-one-verdict error: without decomposition, a single 'tsunami stone constraint' story would either dilute the crisp physical mountain with contested social claims, or contaminate the physical record's zero-extraction profile with the extractive/theatrical possibilities present in the commemorative-husk reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_event_vs_constructed_evidentiary_frame,
    'Is the ''2011 tsunami as decisive empirical test'' a pure natural-law mountain (the wave occurred and reached the elevation it reached, full stop), or is the framing of this event AS a ''test'' itself a constructed narrative that benefits identifiable parties (preservation advocates, DRR researchers) who gain funding, legitimacy, or career capital from casting it as an adjudication device?',
    'Compare independent geophysical/inundation-mapping records (produced without reference to the stones or any warning-validation narrative) against the narrative framing used in preservation and DRR literature. If the physical inundation data stands entirely apart from the framing literature and would exist identically absent any ''test'' narrative, the mountain reading is clean; if funding or publication incentives visibly shaped which sites were selected as exemplary ''validations,'' the framing carries constructed elements.',
    'If the framing is substantially constructed by beneficiary narrative-selection (e.g., selective citation of villages that confirm the stones'' efficacy while underreporting villages where the marker/outcome relationship was ambiguous or reversed), the false_summit_mountain signature would be appropriately triggered and this constraint would warrant reclassification toward tangled_rope at the level of ''the test as academic/advocacy object'' rather than at the level of ''the wave that struck the coast.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_event_vs_constructed_evidentiary_frame, conceptual, 'Whether the tsunami''s status as a ''validation test'' is a natural fact or a beneficiary-shaped narrative construction layered on a natural fact.').

omega_variable(
    sibling_reading_dependency,
    'Given that this reading exists specifically to serve as an adjudication device feeding the behavioral_competence_reading and commemorative_husk_reading, does its classification as an independent Mountain hold if one or both sibling readings are later shown to depend on cherry-picked instances of this event rather than the full population of marked villages?',
    'A comprehensive census of all extant tsunami stone locations along Sanriku, cross-referenced against 2011 inundation maps and settlement-elevation data at each site, rather than the commonly cited handful of dramatic examples (e.g., Aneyoshi).',
    'If the full census shows a much noisier, less binary relationship between marker line and survival than the popular narrative suggests, the sibling readings'' evidentiary reliance on this constraint weakens, though this reading''s own status as a physical mountain (the wave occurred and reached specific elevations) remains unaffected — only the CLARITY of the binary signal it supplies would be revised downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_dependency, empirical, 'Whether the popularly-cited binary validation instances represent the full population of marked sites or a selected subset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 1896, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1896, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1896, 0.02).
narrative_ontology:measurement(tsun_tr_t1933, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1933, 0.03).
narrative_ontology:measurement(tsun_tr_t1960, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(tsun_tr_t1990, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(tsun_tr_t2005, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1896, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1896, 0.02).
narrative_ontology:measurement(tsun_be_t1933, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1933, 0.02).
narrative_ontology:measurement(tsun_be_t1960, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1960, 0.02).
narrative_ontology:measurement(tsun_be_t1990, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1990, 0.02).
narrative_ontology:measurement(tsun_be_t2005, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2005, 0.03).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
