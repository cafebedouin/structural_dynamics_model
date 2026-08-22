% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive: Behavioral Competence Reading
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone marker, placed at an unknown date (estimated pre-1945
 *   based on local memory) in Japan's Iwate Prefecture, reads: 'High tides
 *   have risen to here.' The stone's placement at a particular elevation
 *   established a de facto land-use boundary: settlement and agriculture were
 *   historically confined below the marked line. For 78 years (from the
 *   study's reference point through the 2011 Tōhoku event and beyond), the
 *   community maintained the directive — avoiding building above the stone's
 *   elevation — without any institutional validation of whether the boundary
 *   still tracked the actual hazard or reflected the original
 *   geomorphological reasoning. This reading asserts that the directive's
 *   behavioral force rests on the claim that the stone marks a physical
 *   threshold (maximum reach of historical tsunami, landslide run-out, or
 *   flood crest). The reading's core premise: the constraint persists because
 *   it embeds accurate information about physical hazard geography, not
 *   because it has become purely ceremonial or because beneficiary extraction
 *   sustains it.
 *
 * KEY AGENTS:
 *   - aneyoshi_community_members: local inhabitants who maintain the land-use boundary out of inherited practice (powerless/constrained exit)
 *   - oral_tradition_keepers: moderate-power agents transmitting the directive across generations without necessarily validating its reasoning
 *   - geomorphological_stability_process: the non-agent physical reality (tsunami reach, landslide slope, flood crest) the directive asserts to mark
 *   - external_validation_institution: excluded institutional seat that could in principle verify the boundary but has not been consulted (national land-use planners, geologists)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Stone Directive: Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '08e2814e-9845-4f32-8469-08aa93ce50a0').
narrative_ontology:cs_kernel_codification('08e2814e-9845-4f32-8469-08aa93ce50a0', fixed_text).
narrative_ontology:cs_authority_grounding('08e2814e-9845-4f32-8469-08aa93ce50a0', practice).
narrative_ontology:cs_interpretation_layer_present('08e2814e-9845-4f32-8469-08aa93ce50a0').
narrative_ontology:cs_reading_relation('08e2814e-9845-4f32-8469-08aa93ce50a0', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('08e2814e-9845-4f32-8469-08aa93ce50a0', foundational, stone_marks_verified_hazard_threshold).
narrative_ontology:cs_axiom_status(stone_marks_verified_hazard_threshold, holdable).
narrative_ontology:cs_axiom_grounding('08e2814e-9845-4f32-8469-08aa93ce50a0', stone_marks_verified_hazard_threshold, empirically_contingent).
narrative_ontology:cs_axiom('08e2814e-9845-4f32-8469-08aa93ce50a0', secondary, behavioral_compliance_tracks_physical_safety).
narrative_ontology:cs_axiom_status(behavioral_compliance_tracks_physical_safety, holdable).
narrative_ontology:cs_axiom_grounding('08e2814e-9845-4f32-8469-08aa93ce50a0', behavioral_compliance_tracks_physical_safety, instrumental).
narrative_ontology:cs_reference_frame('08e2814e-9845-4f32-8469-08aa93ce50a0', post_founding_event_hazard_memory).
narrative_ontology:cs_drift_state('08e2814e-9845-4f32-8469-08aa93ce50a0', contemporary_pre_validation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08e2814e-9845-4f32-8469-08aa93ce50a0', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, community_continuity_across_disaster_cycles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the settlement established downslope from the stone marker. Comply with the directive's boundary without necessarily understanding its origin or rationale. They inherit the constraint as part of the landscape's normal order.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_community_members, observer,
    powerless, generational, constrained, local).

% Transmit the directive's content and its authority across generations. They may or may not understand the directive's behavioral mechanism; their role is maintenance of the injunction itself, not validation of its reasoning.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, oral_tradition_keepers, agenda_setter,
    moderate, generational, mobile, local).

% The physical process that makes the stone's placement and the directive's land-use boundary materially consequential. The directive's behavioral force depends on continued correlation between the marked boundary and actual geological hazard.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, geomorphological_stability_process, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive__behavioral_competence_reading, geomorphological_stability_process).

% Modern land-use planners, geologists, and governmental agencies that could in principle verify the stone's original reasoning or update the directive's spatial scope. They are absent from the constraint's operation and have not been consulted to validate or revise the boundary.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, external_validation_institution, excluded,
    institutional, biographical, mobile, national).

% The physical constraint that makes the directive structurally consequential: landslide, tsunami, or flood hazard at a particular elevation or distance. The directive's claim rests on the assertion that this physical fact grounds the boundary.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, geological_hazard_process, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive__behavioral_competence_reading, geological_hazard_process).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination problem is solved by the directive itself; the directive asserts a boundary that correlates with hazard avoidance. The coordination, if any, is implicit: the community organizes settlement and land use around an observed or transmitted boundary, which may or may not preserve functional hazard avoidance.
% TRANSFER_FUNCTION: No material transfer occurs. The directive is a prohibition on settlement or intensive use above a marked line. Compliance is costless if the boundary matches hazard; compliance is foregone opportunity if the boundary is more restrictive than hazard or if hazard has shifted.
% ABSENT_VOICES: Geoscientific institution — land-use planners, seismic surveys, hazard modelers — are absent. If present, they would question whether the 78-year-old boundary reflects current hazard understanding or has drifted from the original geomorphological referent.
% DISAPPEARANCE_RATIONALE: If the directive and its enforcement vanished, land use in Aneyoshi would revert to unconstrained settlement. Whether this would increase disaster risk depends on whether the directive's boundary tracks the actual hazard boundary; if it does, disappearance would result in settlement in hazard zones and increased casualties in the next major event. If the boundary has drifted from the hazard referent, disappearance might have no observable effect on risk (the constraint was already inert) or might enable development in safe zones and reduce regional economic constriction.
% FOUNDING_PROBLEM: A major landslide, tsunami, or flood hazard occurred at a location near Aneyoshi; survivors determined that settlement above a particular elevation or distance threshold faced repeated catastrophic risk. The stone directive was established to mark this threshold and bind future generations to avoid it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (the original hazard event and the identification of risk) is attested by oral tradition within Aneyoshi and by the existence of the stone itself — the most reliable external corroboration. Modern geological surveys (not conducted until decades after the stone's placement) would have independently assessed the hazard, but no such survey's findings are cited as validation of the directive's original boundary. The problem's status as 'dead' is based on: (1) no catastrophic event has occurred within the 78-year interval in the study period, (2) no institutional re-assessment has confirmed that the directive's boundary still tracks current hazard, (3) external geological knowledge exists to test the claim but has not been applied.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the directive, if behaviorally true to its physical referent, imposes no net cost on the community — avoiding hazardous land is costless in a utility sense (all else equal). Suppression is very low (0.12) because the directive operates as a natural-law inference, not active coercion; residents follow it because it is embedded in the landscape itself and in transmitted instruction about where safety lies. Theater ratio is high and rising (0.45→0.65 over the interval) because the directive's functional mechanism (marking a physical hazard boundary) has never been validated by external geoscientific measurement. Over time, as the last eyewitnesses of the original event pass away and the memory shifts from 'we saw the water reach here' to 'the stone says not to build here,' the performative element — the ritual maintenance of the injunction — increases relative to the verification of its physical claim. By the interval's end, the directive persists as a named rule rather than as a tested empirical statement. The measurement grid is one shared series for all metrics at every examined time point; each time point represents a generational cohort's relationship to the directive (t=0 first post-event generation, t=78 generation present at 2011 validation opportunity). Theater tracks the ratio of 'we obey because it is tradition' to 'we obey because we have measured the hazard'; as measurement is never conducted, theater rises toward 1.0.
 *
 * PERSPECTIVAL GAP:
 *   The local community members perceive the directive as natural law — 'this is where the water/stone came' — and comply costlessly. External geoscientific observers (if present) would perceive a claim that has never been tested against current hazard models, elevation surveys, and geological evidence. The constraint computes the same physically at both seats — it is a mountain if and only if the stone's elevation still marks an actual hazard threshold — but the seats differ in their access to verification. This is not a perspectival gap in the DR sense (different extractiveness at different seats); it is an epistemic gap: the seats differ in whether they have measured what the directive asserts.
 *
 * DIRECTIONALITY LOGIC:
 *   The directive has no target and no beneficiary in the extraction sense. It is claimed as a mountain: a physical constraint that emerges from the correlation between a marked boundary and an actual hazard threshold. All agents — the community members, the tradition keepers, the excluded validators — have d ≈ 0.5 (symmetric): they all benefit from the boundary if it is accurate (hazard avoidance is universal good) and all pay equally if it is inaccurate or outdated (opportunity cost of forgone safe development, or exposure if the actual hazard has shifted upslope). The constraint's claim is that no one extracts from it; the directive is a shared fact about geography, not an extraction mechanism. The 'beneficiary' listed (community_continuity_across_disaster_cycles) is the non-agent collective good the constraint putatively protects, not an actor that collects rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (the original hazard event and its identification) is dead: no corroborating event has occurred in the 78-year interval, and no institutional re-examination has confirmed the boundary's validity against modern hazard assessment. The disappearance_verdict is world_rearranges: if the directive vanished, settlement would expand upslope, and the outcome would depend entirely on whether the boundary was behaviorally true (expansion into hazard = increased risk) or ceremonial (expansion into safe zone = development proceeds unchanged). The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges is the mandatrophy signal: the constraint persists after its original problem has left the observable world, yet its absence would produce large-scale reorganization. This indicates either (1) the constraint is a true natural law whose founding event happened long ago and whose hazard remains unobservable until the next rare event, or (2) the constraint has become ceremonial theater and its collapse would expose the absence of any real underlying hazard. The theater_ratio's upward trajectory (0.45→0.65) supports the second interpretation: as direct memory of the founding event fades, the behavioral mechanism shifts from 'we know this because we observed it' to 'we do this because it is the tradition.' The mandatrophy is unresolved pending validation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_physical_correspondence,
    'Does the stone''s elevation correspond to the actual maximum reach of the historical hazard (tsunami run-up, landslide toe, or flood crest) that prompted its placement?',
    'Geoscientific survey: tsunami inundation modeling using topographic data, sediment core analysis for paleo-tsunami deposits, landslide scarps and deposits, flood stage markers. Comparison of historical event reach (from geological evidence or historical records) to the stone''s elevation.',
    'If correspondence is confirmed, the constraint is a true mountain: a natural-law statement about physical geography, costlessly maintained by the community as compliance-with-fact. If correspondence is absent or has drifted (the actual hazard threshold has shifted due to subsidence, erosion, or changed coastal bathymetry), the constraint''s behavioral force is severed from its physical referent and it becomes an inherited fiction — reclassification to piton or degraded snare (compliance maintained by inertia alone, with theater_ratio → 1.0).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_physical_correspondence, empirical, 'Whether the stone marks an actual hazard threshold or is decoupled from physical reality.').

omega_variable(
    inter_reading_kernel_contest,
    'Is the stone''s behavioral force resting on verified hazard avoidance (behavioral_competence_reading) or on ceremonial maintenance of a memorial whose original meaning has been lost (commemorative_husk_reading)?',
    'Ethnographic documentation of compliance motivation: why do contemporary Aneyoshi residents obey the directive? Because they understand it marks a hazard boundary (supports behavioral_competence_reading) or because it is an inherited obligation whose original purpose they do not verify or revisit (supports commemorative_husk_reading)? Post-2011 observation: did the community''s response to the actual Tōhoku tsunami validate the stone''s boundary (the stone correctly predicted reach, supporting competence), or did the tsunami exceed the stone''s marker, contradicting it (supporting husk reading)?',
    'The two readings have opposite ε values and opposite computed types: behavioral_competence_reading claims mountain (ε ≈ 0.08, natural-law constraint); commemorative_husk_reading claims piton (ε ≈ 0.65, theatrical maintenance of a dead function). The empirical resolution (did the 2011 tsunami validate or contradict the stone?) determines which reading''s structural claim is supported. If the tsunami validated the stone (reached to or below its elevation), behavioral_competence_reading is confirmed and the constraint is a mountain. If the tsunami exceeded the stone (reached above it), the stone''s boundary is falsified as a hazard threshold and commemorative_husk_reading''s piton classification is supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inter_reading_kernel_contest, empirical, 'Which reading''s core premise is supported by the 2011 Tōhoku tsunami outcome and by contemporary ethnographic evidence of compliance motivation.').

omega_variable(
    epistemic_validation_gap,
    'Why has no institutional geoscientific validation been conducted or consulted in 78 years, despite the existence of methods (tsunami modeling, elevation survey, hazard mapping) that could test the stone''s claim?',
    'Historical analysis: interviews with Iwate Prefecture land-use planners, geological survey agencies, and community leaders about why the stone''s boundary was never formally assessed. Archival review of land-use policy documents and hazard mapping efforts to determine whether Aneyoshi''s constraint was ever integrated into regional risk assessment or deliberately excluded.',
    'If validation was deliberately avoided (the boundary is treated as customary law not subject to geological testing), the constraint is maintained by institutional deference to tradition rather than by verification of its claim. This supports the piton reading (theater_ratio interpretation: compliance is performative, not functional). If validation was simply not attempted because the community''s compliance was perceived as adequate and no institutional incentive existed to verify, the gap reflects institutional neglect, not deliberate inertia — a different mandatrophy vector (lack of coupling between local practice and regional hazard governance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_validation_gap, empirical, 'Whether the 78-year absence of validation reflects deliberate institutional deference or institutional neglect of local hazard governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(aney_tr_t0, observed).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 13, 0.52).
narrative_ontology:measurement_basis(aney_tr_t13, observed).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 26, 0.58).
narrative_ontology:measurement_basis(aney_tr_t26, observed).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 39, 0.62).
narrative_ontology:measurement_basis(aney_tr_t39, observed).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 52, 0.64).
narrative_ontology:measurement_basis(aney_tr_t52, observed).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 78, 0.65).
narrative_ontology:measurement_basis(aney_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(aney_be_t0, observed).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 13, 0.08).
narrative_ontology:measurement_basis(aney_be_t13, observed).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 26, 0.08).
narrative_ontology:measurement_basis(aney_be_t26, observed).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 39, 0.08).
narrative_ontology:measurement_basis(aney_be_t39, observed).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 52, 0.08).
narrative_ontology:measurement_basis(aney_be_t52, observed).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 78, 0.08).
narrative_ontology:measurement_basis(aney_be_t78, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel decomposes into two structurally distinct constraints corresponding to two readings of the stone's functional status. BEHAVIORAL_COMPETENCE_READING (this story) asserts the stone marks a physical hazard threshold and the community's 78-year compliance preserves this behavioral function; ε ≈ 0.08, claimed mountain. COMMEMORATIVE_HUSK_READING (sibling) asserts the stone is a memorial whose original behavioral content has been lost and is maintained by theatrical tradition; ε ≈ 0.65, claimed piton. The two readings have opposite directionality structures: behavioral_competence treats the boundary as a universal good (no beneficiary/victim, all seats symmetric); commemorative_husk treats the boundary as an inert tradition maintained by inertia with diffuse compliance costs and no captured gains (diffuse victims, no beneficiary). The empirical resolution (whether the 2011 Tōhoku tsunami validated or contradicted the stone's elevation) determines which reading's ε and type are correct. Both stories are true as descriptions of the constraint-as-read from Aneyoshi's perspective; they are false as claims about the constraint's structural essence — the essence is settled only by the hazard validation omega's resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
