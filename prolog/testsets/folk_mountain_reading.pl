% ============================================================================
% CONSTRAINT STORY: folk_mountain_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_folk_mountain_reading, []).

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
 *   constraint_id: folk_mountain_reading
 *   human_readable: 'You Can't Beat the Races' as Near-Physical Law
 *   domain: quantitative_finance/gambling_theory/market_microstructure
 *
 * SUMMARY:
 *   This story instantiates the 'folk mountain' reading of the
 *   beatability-of-the-take kernel: the widely-held belief that horse racing
 *   wagering is unbeatable in the long run, not because of any specific
 *   bettor's skill deficit, but because the parimutuel take combined with the
 *   field's genuine unpredictability constitutes something close to a law of
 *   nature. Under this reading, the question 'can I build an edge here' is
 *   never seriously entertained by the reading's holders — it is treated as
 *   settled, the way one does not ask whether a rock will fall. Crucially,
 *   this reading authors no beneficiaries and no victims: unlike the
 *   flow_extraction_reading (which reads the take as extraction routed
 *   through track operators and totes) or the meta_prediction_reading (which
 *   reads persistent-edge claims by professional syndicates as the live
 *   empirical question), this reading deletes the action from the space
 *   entirely for its holders. Nobody is coordinated, nobody pays, because
 *   nobody bets seriously enough to generate a payer or a beneficiary
 *   relation — the reading forecloses the game before the game starts.
 *
 * KEY AGENTS:
 *   - casual_bettors_holding_the_folk_belief: treat the take/unpredictability combination as settled fact and do not seriously search for edge
 *   - track_operators_and_tote_systems: administer the wagering pools and collect the take, but are not declared as beneficiaries under THIS reading because this reading's holders never test the claim against them
 *   - professional_handicappers_and_syndicates: exist in the world but are outside this reading's frame — they appear in sibling readings, not here
 *   - analytical_observer: notes that this reading functions as a null-action heuristic rather than a tested physical claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(folk_mountain_reading, 0.05).
domain_priors:suppression_score(folk_mountain_reading, 0.1).
domain_priors:theater_ratio(folk_mountain_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(folk_mountain_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(folk_mountain_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(folk_mountain_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(folk_mountain_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(folk_mountain_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(folk_mountain_reading, mountain).
narrative_ontology:human_readable(folk_mountain_reading, "'You Can't Beat the Races' as Near-Physical Law").
narrative_ontology:topic_domain(folk_mountain_reading, "quantitative_finance/gambling_theory/market_microstructure").

domain_priors:emerges_naturally(folk_mountain_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(folk_mountain_reading, 'd36d2318-4aef-4d4a-a4dd-95a04df05f61').
narrative_ontology:cs_kernel_codification('d36d2318-4aef-4d4a-a4dd-95a04df05f61', implicit).
narrative_ontology:cs_authority_grounding('d36d2318-4aef-4d4a-a4dd-95a04df05f61', diffuse_epistemic).
narrative_ontology:cs_reading_relation('d36d2318-4aef-4d4a-a4dd-95a04df05f61', folk_mountain_reading__flow_extraction_reading, influences).
narrative_ontology:cs_reading_relation('d36d2318-4aef-4d4a-a4dd-95a04df05f61', folk_mountain_reading__public_risk_reading, influences).
narrative_ontology:cs_reading_relation('d36d2318-4aef-4d4a-a4dd-95a04df05f61', folk_mountain_reading__meta_prediction_reading, coexists_with).
narrative_ontology:cs_axiom('d36d2318-4aef-4d4a-a4dd-95a04df05f61', foundational, take_plus_variance_forecloses_all_edge).
narrative_ontology:cs_axiom_status(take_plus_variance_forecloses_all_edge, holdable).
narrative_ontology:cs_axiom_grounding('d36d2318-4aef-4d4a-a4dd-95a04df05f61', take_plus_variance_forecloses_all_edge, empirically_contingent).
narrative_ontology:cs_axiom('d36d2318-4aef-4d4a-a4dd-95a04df05f61', foundational, beatability_question_is_settled_not_actor_relative).
narrative_ontology:cs_axiom_status(beatability_question_is_settled_not_actor_relative, holdable).
narrative_ontology:cs_axiom_grounding('d36d2318-4aef-4d4a-a4dd-95a04df05f61', beatability_question_is_settled_not_actor_relative, conventional).
narrative_ontology:cs_reference_frame('d36d2318-4aef-4d4a-a4dd-95a04df05f61', folk_consensus_unbeatability).
narrative_ontology:cs_drift_state('d36d2318-4aef-4d4a-a4dd-95a04df05f61', contemporary_quant_handicapping_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d36d2318-4aef-4d4a-a4dd-95a04df05f61', '').
narrative_ontology:cs_kernel_id(folk_mountain_reading, beatability_of_the_take).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, by this reading's own structure: the folk belief coordinates nothing between parties because it authors no parties. Its only function is epistemic — it closes off a line of inquiry for its holders before any transfer, coordination, or extraction relationship can form.
% TRANSFER_FUNCTION: No transfer occurs under this reading. This is the structural point: unlike the sibling flow_extraction_reading, where the take moves money from bettors to track operators, this reading's holders never engage seriously enough with the wagering market to generate a transfer worth naming.
% ABSENT_VOICES: Professional handicappers and quantitative syndicates who hold the meta_prediction_reading would object that the folk law is empirically false for their population; they are not present in the folk reading's frame because the folk reading, by construction, does not engage with or test their counter-evidence.
% DISAPPEARANCE_RATIONALE: If the folk belief vanished overnight, the underlying take and the underlying unpredictability of races would remain exactly as they are; only the population that currently declines to seriously bet might begin to search for edge, converting some of them into participants of the flow_extraction_reading or meta_prediction_reading populations instead. The world governed by this reading's own terms — a population that treats the question as closed — rearranges only insofar as the belief itself changes; the physical/financial substrate is untouched.
% FOUNDING_PROBLEM: The folk belief functions as a heuristic shortcut protecting casual bettors from the cognitively costly and empirically difficult task of determining whether an edge exists, given the complexity of odds, take structures, and race dynamics.
% FOUNDING_PROBLEM_CORROBORATION: Behavioral-economics literature on gambling heuristics (outside any party that benefits from the belief) corroborates that such folk 'unbeatable' framings function as protective, effort-saving heuristics for casual populations. Professional handicapping literature and syndicate track records, also outside the folk-belief-holding population, corroborate that the founding problem the heuristic addresses (avoiding costly search) may be dead for sophisticated actors even if still live for the casual population the reading actually describes.
narrative_ontology:disappearance_verdict(folk_mountain_reading, world_unchanged).
narrative_ontology:founding_problem_status(folk_mountain_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(folk_mountain_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(folk_mountain_reading, 'none', 1).
narrative_ontology:epsilon_provenance(folk_mountain_reading, 0.05, 'claude-sonnet-5', 'benter_hkjc_parimutuel_2026_20260825_125025', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(folk_mountain_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(folk_mountain_reading, ExtMetricName, E),
    domain_priors:suppression_score(folk_mountain_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(folk_mountain_reading),
    narrative_ontology:constraint_metric(folk_mountain_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(folk_mountain_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(folk_mountain_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction and suppression are authored low because, under this reading's own terms, there is no active enforcement, no coercion, and no identifiable extraction mechanism being defended — the reading simply is a belief that closes off inquiry. Accessibility collapse is authored high (0.88) because the reading's entire function is to make the alternative (seriously modeling and betting the races for edge) feel unavailable or irrational to its holders; this is a collapse of perceived alternatives, not a collapse imposed by active suppression. Resistance is authored low because nobody within this reading is fighting the claim — resistance would come from holders of the sibling readings, who are outside this constraint's population by construction.
 *
 * PERSPECTIVAL GAP:
 *   There is no payer/beneficiary seat divergence to explain here, which is itself the notable structural fact: this reading is mountain-shaped precisely because it authors no parties. Compare this to the flow_extraction_reading, where the same underlying take generates a clear track-operator-beneficiary / bettor-payer structure. The gap between the readings is not a disagreement about facts on the ground — it is a difference in whether the question of parties is asked at all.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared under this reading, so no directionality derivation applies and no stakeholders are authored. This is the intended structural delta: the folk mountain reading is mountain-as-foreclosure, not mountain-as-physics-with-hidden-winners. Declaring beneficiaries here would misrepresent the reading; that structure belongs to the flow_extraction_reading sibling story.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply cleanly here because there is no mandate being defended and no institution whose founding function could have atrophied — the reading is a piece of folk epistemology, not an administered arrangement. The interesting question this reading raises for the corpus is whether a belief with zero authored parties can still be doing extractive work by proxy, by discouraging the population from ever discovering the flow_extraction_reading's beneficiary structure. That question is routed to the omega about mountain-vs-foreclosure rather than resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_foreclosure,
    'Is ''you can''t beat the races'' a genuine near-physical limit imposed by the take plus intrinsic unpredictability, or is it a foreclosure heuristic that deletes the wagering-as-investable-action question before it can be asked, thereby serving whoever benefits from bettors never testing their models?',
    'Compare this reading''s population (recreational bettors, casual observers, most media commentary) against populations holding the flow_extraction_reading or meta_prediction_reading (professional syndicates, quant handicappers) on whether sustained edge exists net of take for skilled minorities. If a persistent, non-degenerate population of positive-EV bettors exists across jurisdictions and eras, the near-physical-law framing is empirically false for the population, even if true in expectation for an average or naive bettor.',
    'If resolved toward foreclosure, this reading is a false summit: no beneficiary of a market-clearing arrangement is declared, but track operators, tote systems, and the takeout structure benefit from the folk-law framing suppressing the search for edge among the mass of bettors, even though no explicit beneficiary/victim structure is authored here by design (per the reading''s own structural delta).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_foreclosure, conceptual, 'Whether the folk ''unbeatable races'' claim is genuine natural law or a constructed foreclosure that happens to look like one from inside this reading.').

omega_variable(
    generalization_scope_of_the_law,
    'Does ''you can''t beat the races'' generalize to ALL bettors uniformly (true near-physical law reading) or is it actually a claim about the median/naive bettor that gets mis-stated as universal?',
    'Statistical decomposition of long-run ROI by bettor sophistication tier, if such data could be obtained from tote systems or syndicate records.',
    'If the law is actually bettor-tier-specific, this reading''s claim to universality (its core distinguishing feature from the sibling readings) is undermined, and the mountain classification becomes harder to sustain even within this reading''s own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generalization_scope_of_the_law, empirical, 'Whether the folk law''s claimed universality survives disaggregation by bettor type.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(folk_mountain_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(folk_tr_t0, folk_mountain_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(folk_tr_t8, folk_mountain_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(folk_tr_t16, folk_mountain_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(folk_tr_t24, folk_mountain_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(folk_tr_t32, folk_mountain_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement(folk_tr_t40, folk_mountain_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(folk_be_t0, folk_mountain_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(folk_be_t8, folk_mountain_reading, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(folk_be_t16, folk_mountain_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(folk_be_t24, folk_mountain_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(folk_be_t32, folk_mountain_reading, base_extractiveness, 32, 0.05).
narrative_ontology:measurement(folk_be_t40, folk_mountain_reading, base_extractiveness, 40, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(folk_mountain_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(folk_mountain_reading, information_standard).
narrative_ontology:boltzmann_floor_override(folk_mountain_reading, 0.02).
narrative_ontology:affects_constraint(folk_mountain_reading, flow_extraction_reading).
narrative_ontology:affects_constraint(folk_mountain_reading, public_risk_reading).
narrative_ontology:affects_constraint(folk_mountain_reading, meta_prediction_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the beatability_of_the_take kernel. folk_mountain_reading (this file) authors zero parties and reads the claim as a near-universal, actor-independent law that forecloses inquiry. flow_extraction_reading authors track operators/totes as beneficiaries and bettors as victims of the take. public_risk_reading treats aggregate wagering losses as a diffuse public-cost concern independent of any specific extractive party. meta_prediction_reading treats beatability as a live, contested empirical claim about whether skilled minorities sustain edge net of take. All four share the same underlying kernel (does the take make racing unbeatable) but instantiate structurally distinct constraints with different ε, different parties, and different classifications, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
