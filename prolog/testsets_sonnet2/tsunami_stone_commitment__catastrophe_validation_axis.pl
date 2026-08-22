% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: The 2011 Tsunami as Decisive Empirical Test of Stone-Marker Commitment
 *   domain: disaster anthropology / commitment system analysis / institutional memory
 *
 * SUMMARY:
 *   Centuries-old stone markers on the Japanese coast carry inscriptions
 *   warning descendants not to build below a given line, on the grounds that
 *   tsunamis reach that height. Whether these markers represented a living,
 *   actively-enforced behavioral norm (the behavioral_competence_reading) or
 *   had degraded into a symbolic, largely-ignored artifact (the
 *   commemorative_husk_reading) is a genuinely contested question about the
 *   same kernel object — the stone commitment. This story is neither of those
 *   readings. It isolates the 2011 Tohoku tsunami itself as a physical event
 *   that functioned, independent of anyone's intent, as a decisive empirical
 *   test distinguishing the two readings for any given marker and village:
 *   did the water cross the line or not, and did the settlement pattern
 *   above/below the line predict the outcome. That test mechanism is a
 *   Mountain — a natural, non-negotiable physical occurrence that neither
 *   reading's advocates control, staged, or can suppress. It feeds
 *   evidentiary weight into both sibling readings without itself being either
 *   of them.
 *
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
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "The 2011 Tsunami as Decisive Empirical Test of Stone-Marker Commitment").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster anthropology / commitment system analysis / institutional memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '5f2a2145-423f-4ce7-bca8-de86e7cf9fac').
narrative_ontology:cs_kernel_codification('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', fixed_text).
narrative_ontology:cs_authority_grounding('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', practice).
narrative_ontology:cs_interpretation_layer_present('5f2a2145-423f-4ce7-bca8-de86e7cf9fac').
narrative_ontology:cs_reading_relation('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', foundational, physical_event_outcome_is_interpretation_independent).
narrative_ontology:cs_axiom_status(physical_event_outcome_is_interpretation_independent, holdable).
narrative_ontology:cs_axiom_grounding('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', physical_event_outcome_is_interpretation_independent, empirically_contingent).
narrative_ontology:cs_axiom('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', secondary, binary_test_evidence_underdetermines_mechanism).
narrative_ontology:cs_axiom_status(binary_test_evidence_underdetermines_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', binary_test_evidence_underdetermines_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', inscribed_warning_as_literal_hazard_boundary).
narrative_ontology:cs_drift_state('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', post_2011_tsunami, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5f2a2145-423f-4ce7-bca8-de86e7cf9fac', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_villages_above_marker_line).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, disaster_preparedness_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_villages_below_marker_line).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, stone_marker_predictive_validity).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_hazard_memory_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that built or maintained settlement above the stone markers experienced negligible loss of life and structure when the 2011 wave arrived; the physical fact of the water line stopping at or below the marker gave them, after the fact, an unambiguous outcome that required no interpretation or testimony to establish. They did nothing to produce this test — the wave simply arrived and either crossed the line or did not.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_villages_above_marker_line, beneficiary,
    powerless, generational, trapped, local).

% Seismologists, anthropologists, and disaster-policy scholars treat the 2011 wave's arrival as a naturally occurring controlled comparison: settlements above vs. below hundreds of centuries-old markers, with outcome data legible independent of any village's own narrative about why the markers were respected or ignored. They did not design the test; they read it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_preparedness_researchers, observer,
    institutional, generational, analytical, global).

% Communities that had settled, over generations, below the stone lines — whether from land pressure, forgotten markers, or a belief the markers no longer applied — suffered the losses the markers had warned against. The wave's arrival did not create their vulnerability; it revealed it, all at once, without appeal.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_villages_below_marker_line, payer,
    powerless, immediate, trapped, local).

% The tsunami itself is the mechanism that administers the test — it arrives without regard to interpretation, belief, or institutional politics, and its run-up line is the same physical fact for every observer who later measures it. It is listed as agenda_setter only in the structural sense that it sets the terms of the test; it is not an actor with interests and collects nothing.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, the_pacific_ocean, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, the_pacific_ocean).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__catastrophe_validation_axis, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__catastrophe_validation_axis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, in the ordinary sense — a tsunami does not coordinate anyone. What this constraint captures is the physical event's role as an involuntary, binary adjudication mechanism between two live interpretive claims (behavioral-competence vs. commemorative-husk) about what the stone markers actually were.
% TRANSFER_FUNCTION: The event transfers nothing between agents in the economic sense; it transfers epistemic status between competing readings of the kernel — the wave's run-up line moved evidentiary weight from whichever reading it failed to corroborate toward whichever reading it corroborated, for a given marker and village.
% ABSENT_VOICES: The communities below the marker line who did not survive to give testimony are structurally absent from the resulting scholarly record in a strong sense; their absence is asymmetric with the survivor villages, who can narrate why they respected the marker. This asymmetry risks a survivorship bias in how corroborating vs. disconfirming instances get weighted afterward.
% DISAPPEARANCE_RATIONALE: The tsunami event itself cannot 'disappear' retroactively — it already happened once as physical fact. Read as a constraint, its removal would mean simply that no such binary test had occurred; the underlying dispute between the behavioral-competence and commemorative-husk readings would remain unresolved by direct evidence, reverting to inference from indirect sources (oral history, compliance records). The world of stone-marker scholarship would not rearrange because of the test's absence — it would merely be poorer in decisive evidence.
% FOUNDING_PROBLEM: The stone markers themselves were built to solve intergenerational transmission failure — the concern that hazard knowledge fades faster than the multi-century recurrence interval of the hazard itself. The 2011 wave, as an event, was not built to solve anything; it functions here only as an unplanned mechanism that tested whether that transmission problem had actually been solved.
% FOUNDING_PROBLEM_CORROBORATION: Independent seismological and tsunami-hazard researchers (e.g., post-2011 field surveys correlating marker locations with inundation extent) attest to the test's occurrence and its outcome pattern from outside any village's own account of its compliance culture; this corroboration is external to both benefiting readings and to the villages themselves.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction and suppression are authored near zero because the wave's arrival and run-up extent are physical facts nobody administers, enforces, or profits from directly — the tsunami does not extract rents, and no party can suppress or manufacture the outcome after the fact. Accessibility collapse is high (0.9) because once the event occurred, the counterfactual (what would the marker have predicted, had the wave not come) collapsed into a single observed outcome — there is no remaining ambiguity about what the 2011 wave actually did at any given marker. Resistance is low because no faction can resist a completed physical event, though interpretation of what the event MEANS for the competing readings remains contested (that contest belongs to the sibling stories, not this one).
 *
 * DIRECTIONALITY LOGIC:
 *   Villages above the marker line are coded as beneficiaries not because the tsunami favored them but because the outcome vindicated the ancestral warning that shaped their settlement pattern — the constraint (the test) operated to their structural advantage after the fact. Villages below the line are coded as payers because the same test operated to reveal and realize their vulnerability. Neither directionality reflects the tsunami 'choosing' anyone; both reflect the pre-existing settlement pattern's exposure to a physical fact that arrived indifferently. Researchers are pure observers with analytical exit — they bear no risk from the event's outcome and simply read the resulting data.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the event itself as Mountain (rather than folding it into either the behavioral-competence or commemorative-husk reading) prevents two mislabeling errors: (1) treating the tsunami as if it were an enforcement mechanism serving one reading's interests — it is not, it is indifferent; and (2) treating the resulting evidentiary pattern as if it settles the interpretive dispute on its own — a single binary test at many sites still requires the interpretive work the sibling readings do to decide what compliance and non-compliance actually mean at each site (economic pressure, forgotten inscriptions, coincidental building patterns). The Mountain classification isolates the adjudication device from the adjudicated claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    test_specificity_across_markers,
    'Was the tsunami a uniformly decisive test across all marker sites, or did local factors (wave height variance, local topography, marker placement error, seawall interference) make some sites poor tests despite superficially binary outcomes?',
    'Site-by-site inundation modeling compared against each marker''s stated warning height and the settlement pattern''s actual elevation, controlling for local bathymetry and coastal defenses.',
    'If test quality varies substantially by site, aggregating ''the 2011 tsunami'' into a single validation event overstates its evidentiary weight for the behavioral_competence_reading versus the commemorative_husk_reading — some apparent confirmations or disconfirmations would be artifacts of local geography rather than of marker efficacy or neglect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(test_specificity_across_markers, empirical, 'Whether the tsunami functioned as a uniform test or a heterogeneous one across marker sites.').

omega_variable(
    survivorship_asymmetry_in_evidence,
    'Does the scholarly record of ''markers that worked'' versus ''markers that were ignored'' suffer from survivorship bias, since below-line communities that suffered severe losses are less able to supply post-hoc testimony about their compliance reasoning than above-line survivors?',
    'Cross-reference pre-2011 land-use records, aerial/satellite imagery, and municipal archives (rather than survivor testimony alone) to reconstruct settlement patterns and stated reasons for building location independent of who survived to narrate them.',
    'If survivorship bias is substantial, the apparent decisiveness of the 2011 test as validation evidence for either sibling reading is overstated, and the true evidentiary strength of this Mountain-classified test event is lower than a naive above/below tally suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_asymmetry_in_evidence, empirical, 'Whether asymmetric survival of testimony biases how the test''s evidentiary output gets read.').

omega_variable(
    committer_framing_alternative,
    'Could the 2011 tsunami instead be authored as a shared evidentiary input residing INSIDE each sibling reading (i.e., part of the behavioral_competence_reading''s and commemorative_husk_reading''s own constraint definitions) rather than as an independent third constraint?',
    'Compare classification outcomes under both framings: (a) tsunami-as-independent-Mountain feeding both siblings via network edges, versus (b) tsunami-evidence folded directly into each sibling''s own extractiveness/suppression metrics as an embedded fact.',
    'Framing (a), adopted here, keeps ε invariant and clean for all three constraints and lets each sibling interpret the same physical fact independently without contaminating their metrics with a shared observable; framing (b) would risk exactly the kind of observer-relative ε drift the ε-invariance principle prohibits, since the two sibling readings would then be forced to encode the same physical event with different implied extraction values depending on how each treats it as confirming or disconfirming. Framing (a) was chosen because it isolates the physical mechanism from the contested interpretive claims cleanly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the tsunami belongs as an independent Mountain constraint or as embedded evidence within each sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t1, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1, 0.05).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(tsun_be_t1, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is the adjudication-mechanism member of a three-story kernel decomposition of 'the tsunami stone commitment.' behavioral_competence_reading and commemorative_husk_reading are the two contested interpretive claims about what the stone markers' inscriptions sociologically accomplished before 2011; this story characterizes the physical test event that both sibling readings must now interpret as evidence. ε for this story is near zero (a natural, non-extractive physical occurrence); ε for the sibling readings differs substantially and is authored independently in their own files, per the ε-invariance principle — this story does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
