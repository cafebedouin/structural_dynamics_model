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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: The 2011 Tsunami as Decisive Empirical Test of Tsunami-Stone Warnings
 *   domain: disaster_anthropology/commitment_system_analysis
 *
 * SUMMARY:
 *   This story isolates one component of the 'tsunami stone' kernel: the 2011
 *   Tōhoku tsunami itself, treated strictly as a physical event that produced
 *   binary, checkable evidence (which settlements above/below historic
 *   inscribed hazard markers were inundated). This is NOT the claim that the
 *   stones retained live behavioral force (that is the sibling
 *   behavioral_competence_reading) nor the claim that the stones had decayed
 *   to symbolic status (the sibling commemorative_husk_reading). Those are
 *   readings that argue FROM this event's evidence toward opposite
 *   conclusions about the stones' function. This story's referent is narrower
 *   and structurally prior: the tsunami as a natural, non-negotiable
 *   adjudication device — a mountain in the sense that no party controls
 *   whether the water reached a given elevation, no party collects rent from
 *   the inundation line's location, and the physical fact would be exactly
 *   what it is regardless of which cultural reading eventually prevails.
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
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "The 2011 Tsunami as Decisive Empirical Test of Tsunami-Stone Warnings").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system_analysis").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '3a14aff6-bf54-42ab-b4d7-62747fcf2c44').
narrative_ontology:cs_kernel_codification('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', fixed_text).
narrative_ontology:cs_authority_grounding('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', practice).
narrative_ontology:cs_reading_relation('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', foundational, physical_inundation_evidence_is_reading_independent).
narrative_ontology:cs_axiom_status(physical_inundation_evidence_is_reading_independent, holdable).
narrative_ontology:cs_axiom_grounding('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', physical_inundation_evidence_is_reading_independent, empirically_contingent).
narrative_ontology:cs_axiom('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', secondary, adjudication_instrument_status_does_not_settle_functional_dispute).
narrative_ontology:cs_axiom_status(adjudication_instrument_status_does_not_settle_functional_dispute, holdable).
narrative_ontology:cs_axiom_grounding('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', adjudication_instrument_status_does_not_settle_functional_dispute, conventional).
narrative_ontology:cs_reference_frame('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', physical_inundation_record_as_neutral_arbiter).
narrative_ontology:cs_drift_state('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3a14aff6-bf54-42ab-b4d7-62747fcf2c44', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, behavioral_competence_reading_advocates).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, disaster_preparedness_researchers).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_warning_had_binary_testable_content).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, physical_inundation_line_constitutes_objective_adjudication_evidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers and commentators who argue the stone markers retained live behavioral force cite the 2011 event as their strongest evidence: villages that stayed above the marked line were spared, villages that settled below were devastated. The tsunami's physical record is the fact they lean on to argue the inscription was not merely commemorative.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, behavioral_competence_reading_advocates, beneficiary,
    analytical, generational, analytical, regional).

% Use the tsunami as a natural experiment: settlement location relative to centuries-old inscribed boundary markers correlates with survival outcomes in a way that is independently checkable against tide gauge and inundation-mapping data. They gain a rare, hard, non-negotiable data point for models of intergenerational risk transmission.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_preparedness_researchers, beneficiary,
    institutional, generational, analytical, global).

% Argue the stones had decayed into symbolic artifacts and that any correlation between marker position and survival is coincidental (elevation and marker placement both track terrain features that independently reduce flood risk). They contest the inferential leap from the physical event to a behavioral-force claim, but the physical facts of the 2011 event itself are not something they dispute — only what those facts are evidence FOR.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, commemorative_husk_reading_advocates, excluded,
    analytical, generational, analytical, regional).

% Lived through the event the adjudication is built on. Some households above the markers survived; some below did not. They did not choose to be the natural experiment, and the physical record of who lived and who died sits independently of how academics later characterize the stones' cultural function.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, coastal_survivor_communities, observer,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this is not a coordination mechanism. The 2011 tsunami is a physical event that functions as an evidentiary instrument: it produced a fixed, independently observable outcome (inundation line vs. settlement line) that different interpretive communities use to adjudicate a prior, unresolved question about the stones' function.
% TRANSFER_FUNCTION: No resource, labor, or status is transferred by the tsunami itself. What moves is evidentiary weight: the event shifts the burden of proof in the ongoing dispute between the behavioral-competence and commemorative-husk readings, in favor of whichever reading the inundation data supports.
% ABSENT_VOICES: The households who died below the marker line have no voice in how their deaths are subsequently used as evidence in an academic and cultural dispute about the stones' meaning. Their outcome is treated as a data point rather than as an irreversible loss with its own standing.
% DISAPPEARANCE_RATIONALE: The tsunami already happened; it cannot un-happen, and its physical record (which settlements were inundated, which were not) is now fixed regardless of any future interpretive dispute. If this constraint (the event's status as adjudication evidence) were somehow erased, the physical facts would remain recoverable from independent seismological, tidal, and settlement records — the world's causal structure does not depend on this being recognized as an adjudication mechanism.
% FOUNDING_PROBLEM: The stones were originally erected to solve a problem of intergenerational risk communication: how to transmit a rare, high-consequence hazard boundary across generations that would not personally witness a confirming event. The 2011 tsunami tests whether that transmission mechanism actually worked.
% FOUNDING_PROBLEM_CORROBORATION: Independent seismological and tsunami-inundation researchers (NOAA, Japanese government inundation surveys, and academic disaster-anthropology fieldwork conducted after 2011, none of whom hold a stake in either the behavioral-competence or commemorative-husk reading being correct) have published settlement-line versus inundation-line correlation data. This corroboration exists outside both advocacy camps, though its interpretation remains contested between them.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction, suppression, and theater are all authored near zero because the tsunami's physical trace (inundation extent, tide-gauge records, settlement-survival correlation) is not a human-administered arrangement — no one enforces it, no one profits from its having happened at a particular elevation, and nothing about its evidentiary status is performative. Accessibility collapse is high (0.9): once the physical record is established, there is no alternative interpretation of WHERE the water reached — that specific empirical fact forecloses easily. Resistance is low (0.05): no faction disputes the raw inundation-line data itself; the entire live dispute is about what the data is evidence FOR (the two sibling readings), not about the data's existence.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the behavioral-competence and commemorative-husk camps benefit from having a hard adjudication instrument to argue from, even though they draw opposite conclusions from it — hence both associated groups are listed as beneficiaries of the evidentiary event, not as targets. There are no victims of the tsunami-as-evidence construct; the actual tsunami had victims (the survivor communities), but they are victims of the disaster, not of this constraint (the evidentiary status of the disaster), which is why they are coded as observers rather than payers. This is a subtle but important distinction: the constraint under analysis is the event's role as a test, not the event's harm.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy question for this reading — the founding problem (intergenerational risk transmission) is not this constraint's own founding problem; this constraint (the tsunami-as-test) has no mandate to outlive, since it is not an institution or arrangement that persists and is administered. It occurred once and its evidentiary content is now fixed. The mandatrophy-relevant questions belong to the sibling readings, which describe living arrangements (a still-standing stone, still-recited or still-forgotten warnings) that can genuinely drift from their founding function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evidentiary_instrument_vs_cultural_claim,
    'Is the 2011 tsunami''s role as ''decisive empirical test'' itself a natural fact (the water reached where it reached) or is the FRAMING of it as a ''test'' of the stones already a culturally loaded interpretive act that smuggles in one of the sibling readings?',
    'Compare independent seismological/inundation mapping (produced without reference to the stones at all) against settlement-boundary records; if the correlation is derivable purely from terrain/elevation data without needing the stones'' existence as a variable, the ''test'' framing is a post-hoc interpretive overlay rather than an inherent property of the event.',
    'If the framing is interpretive rather than natural, this story''s mountain classification would need re-examination — it might actually be a thin analytical construct serving the interests of whichever reading invokes it, rather than a pure physical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidentiary_instrument_vs_cultural_claim, conceptual, 'Whether treating the tsunami as a ''test'' of the stones is itself a neutral physical description or an interpretive move already favoring one sibling reading.').

omega_variable(
    selection_and_survivorship_bias,
    'Are the surviving stone markers and the settlements near them a representative sample, or does the very fact that a stone AND a story about it survived to 2011 already select for communities where the warning system had some efficacy (survivorship bias in which markers get studied)?',
    'Systematic survey of ALL known historic hazard markers along the affected coastline (including ones with no associated surviving settlement or oral tradition), compared against inundation outcomes, to check whether the ''validating'' cases were cherry-picked by which markers happened to be well-documented.',
    'If selection bias is substantial, the ''decisive'' validation is less decisive than claimed, weakening the evidentiary force this constraint provides to the behavioral_competence_reading and strengthening skepticism aligned with commemorative_husk_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_and_survivorship_bias, empirical, 'Whether the sample of documented stones with confirmed 2011 outcomes is representative or self-selected.').

omega_variable(
    kernel_framing_underdetermination,
    'Two coherent framings exist for what ''the tsunami stone commitment'' even names: (a) an institution of intergenerational risk-transmission whose EFFICACY is being tested by 2011, versus (b) a raw physical hazard-boundary that independently exists whether or not any stone or story refers to it. This story adopts framing (a)''s evidentiary apparatus while classifying as framing (b)''s mountain.',
    'Would the classification change under strict framing (b) alone? Under (b), there is no ''commitment'' at all to test — just topography and a flood. The ''test'' concept only makes sense under framing (a), which already presupposes a commitment system exists to be validated or falsified.',
    'If framing (a) is adopted fully, this constraint arguably belongs to the CS structure family rather than to pure physical mountain status, since ''serving as adjudication device'' is itself a role assigned by a commitment-system observer, not an intrinsic property of tidal hydrodynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether classifying the event as an ''adjudication device'' already imports commitment-system framing rather than describing a pure physical fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story is the third member of a three-story constraint family decomposing the natural-language label 'tsunami stone commitment' per the ε-invariance principle. behavioral_competence_reading and commemorative_husk_reading are the two contested cultural/institutional readings of the stones' function (both likely classify away from mountain, toward rope/tangled_rope/piton depending on their own metrics). This story, catastrophe_validation_axis, isolates the physical 2011 event itself as a mountain-type evidentiary instrument that both sibling readings draw upon but which is not itself a claim about the stones' cultural function. It has negligible ε because no party administers, enforces, or profits from where the tsunami's water line fell. All three stories share the kernel_id tsunami_stone_commitment and are linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
