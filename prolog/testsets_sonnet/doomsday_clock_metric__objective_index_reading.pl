% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Existential-Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Science and Security Board sets
 *   the Doomsday Clock's minute hand annually, presenting it publicly as a
 *   synthesis of measurable indicators (warhead counts, emissions
 *   trajectories, biosecurity incidents, disruptive technology risk) into a
 *   single figure representing 'closeness to global catastrophe.' The
 *   objective_index_reading takes this framing at face value: the Clock
 *   measures a real quantity via expert aggregation, and the setting is a
 *   scientific output, not a rhetorical or strategic choice. This framing is
 *   structurally significant because it suppresses the normative and
 *   strategic dimensions of the choice (what counts as a risk indicator, how
 *   indicators are weighted, when incremental changes cross a minute-hand
 *   threshold) and locates final interpretive authority in a closed expert
 *   board rather than in any auditable public process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.71).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Existential-Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'e5932c5f-05bf-41fb-82cb-86b6054fa658').
narrative_ontology:cs_kernel_codification('e5932c5f-05bf-41fb-82cb-86b6054fa658', formalized).
narrative_ontology:cs_authority_grounding('e5932c5f-05bf-41fb-82cb-86b6054fa658', expertise).
narrative_ontology:cs_interpretation_layer_present('e5932c5f-05bf-41fb-82cb-86b6054fa658').
narrative_ontology:cs_reading_relation('e5932c5f-05bf-41fb-82cb-86b6054fa658', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5932c5f-05bf-41fb-82cb-86b6054fa658', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('e5932c5f-05bf-41fb-82cb-86b6054fa658', foundational, existential_risk_is_measurable_via_indicator_synthesis).
narrative_ontology:cs_axiom_status(existential_risk_is_measurable_via_indicator_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('e5932c5f-05bf-41fb-82cb-86b6054fa658', existential_risk_is_measurable_via_indicator_synthesis, empirically_contingent).
narrative_ontology:cs_axiom('e5932c5f-05bf-41fb-82cb-86b6054fa658', foundational, expert_weighting_choices_are_normatively_neutral).
narrative_ontology:cs_axiom_status(expert_weighting_choices_are_normatively_neutral, holdable).
narrative_ontology:cs_axiom_grounding('e5932c5f-05bf-41fb-82cb-86b6054fa658', expert_weighting_choices_are_normatively_neutral, conventional).
narrative_ontology:cs_reference_frame('e5932c5f-05bf-41fb-82cb-86b6054fa658', postwar_scientific_communication_mandate).
narrative_ontology:cs_drift_state('e5932c5f-05bf-41fb-82cb-86b6054fa658', contemporary_multipolar_risk_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5932c5f-05bf-41fb-82cb-86b6054fa658', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, credentialed_risk_expert_class).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, elected_policymakers_outside_expert_circle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the minute hand annually by closed-door synthesis of nuclear, climate, and biosecurity indicators plus disruptive-technology assessments. Presents the result as a measured reading of an underlying risk quantity rather than a chosen framing, and controls both the indicator selection and the weighting that produces the final setting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains professional authority and citation currency from being the interpretive layer standing between raw indicator data and the public setting. Their disciplinary standing is reinforced each time the Clock is treated as a scientific reading rather than a normative judgment call.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, credentialed_risk_expert_class, beneficiary,
    organized, generational, mobile, global).

% Receive the Clock setting as an authoritative fact about the state of the world, with no visibility into the weighting choices that produced it and no mechanism to contest the indicator selection. Cannot verify the underlying synthesis and cannot vote on or revise the indicator set.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_publics, payer,
    powerless, biographical, trapped, global).

% Face public and media pressure keyed to the Clock's annual setting without having participated in or being able to audit the indicator synthesis. Must respond politically to a number whose construction they did not control and cannot formally revise, even though it shapes constituent perception of their governance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, elected_policymakers_outside_expert_circle, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, elected_policymakers_outside_expert_circle, excluded).

% Produce alternative existential-risk frameworks (e.g. differently weighted indices, probabilistic forecasting models) that receive far less public salience because the Clock's decades of brand recognition and claimed objectivity crowd out competing framings from the same public attention space.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, rival_risk_assessment_bodies, excluded,
    organized, biographical, constrained, global).

% Study the gap between the Clock's claimed measurement status and its actual construction process, publishing critiques of the indicator weighting and the absence of published uncertainty bounds, without power to alter how the setting is produced or received.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, independent_science_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizes disparate technical indicators (nuclear posture, emissions trajectories, biosecurity incidents, AI capability signals) into a single, communicable annual figure that lets non-specialists track relative existential risk trend without independently monitoring each domain.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as 'the' risk level from distributed public and political deliberation to a small, self-perpetuating expert board; moves public trust and media attention toward that board's annual pronouncement rather than toward pluralistic risk debate.
% ABSENT_VOICES: Rival risk-assessment methodologies, affected populations in the Global South disproportionately exposed to climate and conflict risk, and elected officials who must answer for the number publicly are not part of the indicator-weighting process; they would object to being bound by a synthesis method they cannot inspect or revise.
% DISAPPEARANCE_RATIONALE: The Board and much of the expert commentariat would say the world loses its clearest existential-risk communication device and default coordination point for science journalism and policy rhetoric. Critics would say public discourse would simply route through more explicitly normative and pluralistic risk communication, with no loss of underlying knowledge — only loss of a monopoly framing device. Which is true is exactly the contested claim this reading takes a side on.
% FOUNDING_PROBLEM: In 1947, physicists who had worked on the atomic bomb needed a way to communicate accelerating nuclear danger to a lay public and policymakers in a single, memorable, recurring signal that did not require technical literacy to parse.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin's own board attests the founding problem — communicating civilizational risk trend to a broad public — remains fully live and treats the annual setting as continuous with that original scientific-communication mission. Independent science-policy researchers (outside the Bulletin) and several former contributing scientists have published critiques arguing the mission has shifted from communicating measured risk to manufacturing a normatively loaded advocacy signal dressed as measurement; no fully independent replication of the indicator-weighting methodology has been published by a body outside the Bulletin's own circle.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.71) are authored high because the objective-index framing suppresses visibility into the actual weighting choices — presenting a normatively loaded construction as a neutral measurement is itself an act of interpretive foreclosure that concentrates authority. Theater ratio (0.42) reflects that a real synthesis function occurs (the Board does track real indicators) alongside a growing performative layer (media ritual around the annual announcement has outpaced the indicator-tracking function's marginal informational value). Resistance is moderate (0.48): critique exists in science-policy literature but has not meaningfully altered the Board's methodology or its public framing.
 *
 * PERSPECTIVAL GAP:
 *   From the Board's seat, this is a Rope: real coordination problem (communicating diffuse technical risk to laypeople), solved by credentialed synthesis, broadly beneficial. From the excluded-public and excluded-policymaker seats, the same structure functions closer to Tangled Rope or Snare: the coordination function is real, but it comes bundled with an extraction of interpretive authority that these seats can never buy back, because the framing itself (measurement, not judgment) forecloses the avenue by which they might contest the synthesis. The engine's per-seat computation should register this divergence directly from the beneficiary/victim and exit-option data authored above.
 *
 * DIRECTIONALITY LOGIC:
 *   The Science and Security Board and the broader credentialed risk-expert class are the structural beneficiaries: their interpretive monopoly is reinforced every time the setting is received as objective fact rather than judgment call, which increases the authority-value of Board membership and adjacent expert credentials. Democratic publics and elected officials outside the expert circle are targets: they receive a number they cannot contest or audit, yet must organize political and personal risk perception around it. Rival risk-assessment bodies are excluded rather than coordinated — the objective-index framing crowds out competing framings by claiming a unique measurement status that alternative indices cannot match rhetorically even when methodologically comparable.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_vs_construction_ambiguity,
    'Is the Doomsday Clock setting best modeled as a measurement of a mind-independent existential-risk quantity, or as a constructed synthesis whose weighting choices are themselves normative and contestable?',
    'Publication of the full indicator-weighting methodology with sensitivity analysis showing how alternative reasonable weightings would change the setting; comparison against independently constructed risk indices using disclosed, auditable methods.',
    'If the setting is robust across a wide range of reasonable weighting schemes, the objective_index_reading gains support and the constraint moves toward genuine Rope; if small weighting changes produce large swings in the setting, the reading''s core premise weakens and the constraint''s actual operation looks more like the performative_tool_reading or the hybrid_legitimacy_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_vs_construction_ambiguity, conceptual, 'Whether the Clock''s claimed objectivity survives methodological audit.').

omega_variable(
    expert_monopoly_democratic_deficit,
    'Does routing existential-risk interpretation exclusively through a closed expert board constitute a legitimate epistemic division of labor, or a democratic accountability deficit that concentrates unearned interpretive power?',
    'Comparative study of public risk perception and policy responsiveness in jurisdictions/contexts where risk communication is produced through more participatory or pluralistic processes versus the Bulletin''s closed-board model.',
    'If participatory alternatives produce comparable calibration with greater public buy-in and contestability, the expert-monopoly structure looks more like extraction than efficient coordination; if participatory models degrade calibration substantially, the closed-expert model''s coordination value is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expert_monopoly_democratic_deficit, preference, 'Whether expert-exclusive interpretation of existential risk is a legitimate coordination mechanism or a democratic-accountability extraction.').

omega_variable(
    sibling_reading_foreclosure_question,
    'Does adopting the objective_index_reading (measurement claim) logically require rejecting the hybrid_legitimacy_reading''s claim that scientific and normative judgment are irreducibly entangled in this domain?',
    'Philosophical/methodological analysis of whether existential-risk indicator selection can, even in principle, be normatively neutral — i.e. whether ''risk of catastrophe'' is a natural kind measurable independent of value-laden threshold and weighting choices.',
    'If indicator selection is shown to be irreducibly value-laden, the objective_index_reading cannot be coherently held alongside the hybrid reading in the same framework, which would argue for reclassifying this reading''s relation to hybrid_legitimacy_reading from coexists_with to forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_question, conceptual, 'Whether the objective-measurement claim and the hybrid-entanglement claim can both be held, or logically exclude each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement(doom_tr_t1970, doomsday_clock_metric__objective_index_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(doom_tr_t1991, doomsday_clock_metric__objective_index_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(doom_tr_t2007, doomsday_clock_metric__objective_index_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement(doom_tr_t2018, doomsday_clock_metric__objective_index_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(doom_tr_t2025, doomsday_clock_metric__objective_index_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.28).
narrative_ontology:measurement(doom_be_t1970, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(doom_be_t1991, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1991, 0.4).
narrative_ontology:measurement(doom_be_t2007, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2007, 0.48).
narrative_ontology:measurement(doom_be_t2018, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2018, 0.53).
narrative_ontology:measurement(doom_be_t2025, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.45).
narrative_ontology:measurement(doom_su_t1970, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(doom_su_t1991, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1991, 0.58).
narrative_ontology:measurement(doom_su_t2007, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2007, 0.64).
narrative_ontology:measurement(doom_su_t2018, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(doom_su_t2025, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.05).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the doomsday_clock_metric kernel. objective_index_reading (this file) claims the setting measures a mind-independent risk quantity via expert synthesis. performative_tool_reading claims the setting is strategically chosen for policy mobilization. hybrid_legitimacy_reading claims the scientific and normative components are irreducibly entangled and cannot be cleanly separated into either of the other two framings. Each reading has its own epsilon and stakeholder structure; they are linked here rather than merged because measuring 'the Clock' under each framing yields materially different extraction and suppression profiles (ε-invariance principle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
