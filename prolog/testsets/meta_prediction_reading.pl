% ============================================================================
% CONSTRAINT STORY: meta_prediction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_prediction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: meta_prediction_reading
 *   human_readable: Meta-Prediction Edge: Betting the Public's Prediction Error, Not the Race
 *   domain: quantitative_finance/gambling_theory/market_microstructure
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel 'beatability
 *   of the take' in pari-mutuel wagering. At the level of predicting
 *   individual race outcomes, the take is a mathematically fixed drag that
 *   makes the average bettor's expectation negative no matter how skillfully
 *   they pick horses — this is the ground the folk-mountain reading and the
 *   flow-extraction reading each stand on in their own ways. But this reading
 *   claims a different, narrower, and verifiably real action exists one level
 *   up: the public's tote-implied probabilities are themselves a noisy,
 *   biased estimate of true win probability, and a sufficiently sophisticated
 *   modeler (the historical Benter/Woods syndicate is the canonical case) can
 *   profitably bet not on 'who wins' directly but on 'where does the crowd's
 *   implied probability diverge from true probability by more than the
 *   takeout.' That divergence, not the race itself, is the actionable object.
 *   This is a Rope from the syndicate's own operating logic — a genuine
 *   coordination/information function (aggregating decentralized public
 *   belief into odds) that a sophisticated analytical actor can arbitrage
 *   without suppressing anyone's ability to bet, without excluding the public
 *   from the pool, and without requiring coercion. The 'extraction' the
 *   metrics register is not extraction-by-suppression; it is
 *   extraction-by-superior-modeling, drawn net of takeout from bettors whose
 *   individual bets were, in the pool-aggregate sense, mispriced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_prediction_reading, 0.62).
domain_priors:suppression_score(meta_prediction_reading, 0.15).
domain_priors:theater_ratio(meta_prediction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_prediction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(meta_prediction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(meta_prediction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(meta_prediction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(meta_prediction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_prediction_reading, rope).
narrative_ontology:human_readable(meta_prediction_reading, "Meta-Prediction Edge: Betting the Public's Prediction Error, Not the Race").
narrative_ontology:topic_domain(meta_prediction_reading, "quantitative_finance/gambling_theory/market_microstructure").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meta_prediction_reading, '86c6cbe6-e1f8-4b74-ad67-6af32f993b91').
narrative_ontology:cs_kernel_codification('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', distributed).
narrative_ontology:cs_authority_grounding('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', distributed).
narrative_ontology:cs_reading_relation('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', meta_prediction_reading__folk_mountain_reading, influences).
narrative_ontology:cs_reading_relation('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', meta_prediction_reading__flow_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', meta_prediction_reading__public_risk_reading, coexists_with).
narrative_ontology:cs_axiom('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', foundational, actionable_target_is_prediction_error_not_outcome).
narrative_ontology:cs_axiom_status(actionable_target_is_prediction_error_not_outcome, holdable).
narrative_ontology:cs_axiom_grounding('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', actionable_target_is_prediction_error_not_outcome, empirically_contingent).
narrative_ontology:cs_axiom('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', secondary, take_unbeatability_is_level_relative_not_absolute).
narrative_ontology:cs_axiom_status(take_unbeatability_is_level_relative_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', take_unbeatability_is_level_relative_not_absolute, empirically_contingent).
narrative_ontology:cs_reference_frame('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', individual_outcome_prediction_floor).
narrative_ontology:cs_drift_state('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', post_benter_syndicate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86c6cbe6-e1f8-4b74-ad67-6af32f993b91', '').
narrative_ontology:cs_kernel_id(meta_prediction_reading, beatability_of_the_take).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_prediction_reading, syndicate_modelers).
narrative_ontology:constraint_victim(meta_prediction_reading, public_wagering_pool).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build statistical models of race outcomes (Benter/Woods-style) and, crucially, models of how the public's tote-derived implied probabilities diverge from those outcome models. They do not try to beat the take at the level of picking winners in isolation — they bet only where the pool's implied probability on a given horse diverges from the model's true probability by more than the track's takeout. This narrows their action set sharply but yields a genuine, repeatable statistical edge across a large volume of bets, funded by continuous data collection, model refinement, and computing infrastructure most bettors cannot replicate.
narrative_ontology:constraint_stakeholder(meta_prediction_reading, syndicate_modelers, beneficiary,
    organized, biographical, arbitrage, national).

% Ordinary bettors collectively set the tote odds through their aggregate wagers; the pool's implied probabilities are simply the sum of everyone's beliefs (and biases — favoring visible jockeys, recent winners, post-position superstitions). Individually none of them experience an 'extraction' event; they lose or win on ordinary variance. But in aggregate, their systematic biases are exactly the signal the syndicate mines, and the syndicate's stake is drawn dollar-for-dollar from the same pari-mutuel pool. No individual bettor can exit the pool's aggregate bias; the pool's composition is what generates the exploitable divergence in the first place.
narrative_ontology:constraint_stakeholder(meta_prediction_reading, public_wagering_pool, payer,
    powerless, immediate, trapped, national).

% Sets the takeout rate and administers the pari-mutuel pool mechanically; collects the takeout regardless of who wins the internal contest between the public's implied probabilities and the syndicate's model. Structurally indifferent to whether the public or the syndicate 'wins' the meta-prediction game, since the take is levied on total pool volume either way. Has no stake in the specific reading of the kernel being litigated here — profits under all four sibling readings.
narrative_ontology:constraint_stakeholder(meta_prediction_reading, track_operator, agenda_setter,
    institutional, generational, analytical, national).

% Would object that the game is being described in a way that renders their individual losses structurally invisible — from their seat there is no 'meta-prediction' layer, just a bet on a horse that did or didn't win. They are not consulted in, and largely unaware of, the syndicate's read of the situation; their aggregate behavior is the raw material the syndicate's model consumes, but they have no seat in the analysis that names them as the source of the exploited signal.
narrative_ontology:constraint_stakeholder(meta_prediction_reading, casual_bettors, excluded,
    powerless, immediate, trapped, local).

% Studies the structure of pari-mutuel markets to identify where and why professional syndicates achieve durable positive expected value despite the take being, in the individual-outcome sense, mathematically unbeatable in expectation for the average bettor. Recognizes that the actionable target is the *divergence* between public implied probability and true probability, not the outcome itself — this is the reading that recovers the action the other readings cannot see.
narrative_ontology:constraint_stakeholder(meta_prediction_reading, quantitative_analyst_observer, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The pari-mutuel pool coordinates a large, decentralized set of bettors' private beliefs into a single public odds line without requiring any bettor to consult any other; the syndicate's meta-prediction layer coordinates its own internal statistical apparatus (data collection, handicapping models, bias models of the public) into a single systematic edge-detection process. Neither coordination function requires the other's cooperation.
% TRANSFER_FUNCTION: Wagered dollars move from the collective public pool into the track's takeout (fixed cut) and, net of that takeout, redistribute among winning bettors in proportion to correctly-priced risk. The syndicate's edge is a further redistribution within that redistribution: because its model prices the true win probability more accurately than the pool's implied probability, it systematically captures a share of the pool disproportionate to its wagering volume, drawn from bettors whose bets were mispriced relative to true probability.
% ABSENT_VOICES: Casual bettors, whose aggregate biases constitute the very signal being mined, are never in the room where this reading is articulated — they experience the game as a contest with a horse, not as an ongoing statistical relationship with a syndicate's model of their own collective error. If present, they would likely object that describing their losses as 'harvested prediction error' recasts ordinary recreational risk-taking as an extraction event they never consented to in those terms.
% DISAPPEARANCE_RATIONALE: If this reading's actionable structure disappeared — i.e., if the public's implied probabilities converged perfectly to true probabilities, or if syndicates lost the capacity to model the divergence — the syndicate's positive-EV wagering would vanish; their capital and analytical apparatus would exit racing markets and redeploy into other imperfectly-efficient prediction markets (sports betting exchanges, financial microstructure venues). The track's take-per-dollar-wagered would be unaffected, but the composition of who wins net of take would shift back toward pure variance for all remaining bettors.
% FOUNDING_PROBLEM: Pari-mutuel wagering was designed to let a large public collectively price uncertain outcomes without requiring a bookmaker to set individualized odds, while guaranteeing the operator a fixed cut regardless of outcome. It was not designed with the expectation that a sub-population would build superior statistical models of the pool's own aggregate bias and systematically harvest the gap.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic work (e.g., studies of the Hong Kong Jockey Club syndicates, published quantitative-finance retrospectives on Benter's model) corroborates from outside the syndicate itself that this divergence-harvesting structure is real and durable, not a self-serving syndicate narrative; track operators and racing regulators have separately acknowledged the phenomenon in public commentary on why some bettors show multi-decade positive returns despite takeout rates that make the average bettor's expectation strongly negative.
narrative_ontology:disappearance_verdict(meta_prediction_reading, world_rearranges).
narrative_ontology:founding_problem_status(meta_prediction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(meta_prediction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(meta_prediction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(meta_prediction_reading, 0.62, 'claude-sonnet-5', 'benter_hkjc_parimutuel_2026_20260825_125025', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_prediction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_prediction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(meta_prediction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-moderate 0.62 by interval end: real money moves systematically from the mispriced side of the pool to the syndicate, and this is not a trivial edge — multi-decade documented outperformance exists. But suppression is low (0.15) and accessibility_collapse is only moderate (0.35): no bettor is barred from building their own model, no alternative wagering structure is suppressed, and the public pool continues operating exactly as designed. Resistance is moderate (0.45) reflecting periodic track/regulatory scrutiny of large syndicate bets, not because any structural barrier blocks entry. Theater ratio stays near zero throughout — there is essentially no performative layer here; the syndicate's operation is almost entirely functional (data collection, modeling, execution).
 *
 * DIRECTIONALITY LOGIC:
 *   The syndicate is the clean structural beneficiary: it collects a durable statistical edge and bears no imposed cost from the constraint's operation (arbitrage-grade exit — it can redeploy capital to any sufficiently inefficient market). The public pool is the structural payer: in aggregate its collective mispricing is the exploited resource, and no individual bettor can exit the pool's aggregate bias (trapped exit) even though each individual bettor exits any single race freely. This is the directionality signature of a Rope read from the top (a real information-aggregation function exploited by a sophisticated analytical arbitrageur) rather than a Snare (no suppression of alternatives) or a Tangled Rope (no active enforcement is required to maintain the syndicate's edge — it is unenforced structural).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is not mandatrophic: the founding problem (aggregate pricing of uncertain outcomes via decentralized wagering) is fully live, and the syndicate's meta-prediction layer is not a vestigial or captured version of that function — it is a novel, additional layer riding on top of a live coordination mechanism. The classification as Rope-at-this-reading should not be mistaken for a claim that the average bettor's experience is unextractive; that is precisely what the sibling readings (flow_extraction_reading, public_risk_reading) are for. Keeping this reading narrow and clean prevents conflating 'the take is unbeatable for average outcome-prediction' (true, and the ground of folk_mountain_reading) with 'the take is unbeatable at every level of analysis' (false, and the premise this reading exists to falsify).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition_location,
    'Is the disagreement among the four readings of beatability_of_the_take located in what each reading treats as the actionable prediction target, or in a genuine empirical dispute about whether syndicate edges are real and durable?',
    'Independent replication of syndicate-style meta-prediction strategies across multiple pari-mutuel jurisdictions and eras, holding takeout rates constant, would show whether the edge tracks the divergence-mining mechanism this reading claims or is better explained by information asymmetries the flow_extraction_reading would attribute to track-level structure.',
    'If the edge is confirmed as divergence-mining specifically, this reading''s foreclosure of the folk_mountain_reading''s stronger claim (unbeatable at every analytical level) is empirically vindicated. If the edge instead traces to privileged information flows (e.g. insider tips, exchange rebates), the flow_extraction_reading would be the better home for the phenomenon and this reading''s beneficiary/victim structure would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition_location, empirical, 'Whether the syndicate edge is genuinely meta-predictive or is a mislabeled instance of a different extractive mechanism.').

omega_variable(
    public_pool_as_aggregate_victim_coherence,
    'Does it make coherent sense to name ''the public wagering pool'' as a victim group when no individual bettor experiences the extraction as such, and the pool''s composition (not any individual''s choice) generates the exploited signal?',
    'Philosophical/structural analysis of aggregate versus distributive harm: compare to other cases where a population''s collective statistical bias, rather than any individual''s decision, is the resource being mined (e.g. insurance risk pools, prediction markets generally).',
    'If aggregate-level victimhood without individual-level awareness is judged incoherent or too diffuse to ground a victim declaration, this reading''s classification would drift toward Rope-with-no-victim (closer to a pure coordination/arbitrage story) rather than the current Rope-with-declared-victim structure authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_pool_as_aggregate_victim_coherence, conceptual, 'Whether aggregate statistical bias constitutes a coherent victim class distinct from any individual bettor''s experience.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does this reading''s claim that action exists one level above outcome-prediction actually foreclose the folk_mountain_reading''s core premise, or merely operate alongside it at a different level of description?',
    'Careful logical analysis of whether ''the take is unbeatable for individual outcome prediction'' (folk_mountain_reading''s premise) and ''the take is beatable for meta-prediction of the public''s error'' (this reading''s premise) can both be true simultaneously within one bettor''s operating framework.',
    'If both premises can coexist (a bettor can accept both), the relation to folk_mountain_reading should be coexists_with or influences rather than forecloses, and the cs_structure declaration below should be revisited in light of this omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether this reading logically forecloses folk_mountain_reading or merely operates at an orthogonal level of description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_prediction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t0, meta_prediction_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(meta_tr_t5, meta_prediction_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement(meta_tr_t10, meta_prediction_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(meta_tr_t15, meta_prediction_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(meta_tr_t20, meta_prediction_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(meta_tr_t25, meta_prediction_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(meta_tr_t30, meta_prediction_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(meta_be_t0, meta_prediction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(meta_be_t5, meta_prediction_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(meta_be_t10, meta_prediction_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(meta_be_t15, meta_prediction_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(meta_be_t20, meta_prediction_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(meta_be_t25, meta_prediction_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(meta_be_t30, meta_prediction_reading, base_extractiveness, 30, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(meta_prediction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_prediction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(meta_prediction_reading, 0.15).
narrative_ontology:affects_constraint(meta_prediction_reading, flow_extraction_reading).
narrative_ontology:affects_constraint(meta_prediction_reading, public_risk_reading).
narrative_ontology:affects_constraint(meta_prediction_reading, folk_mountain_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the shared kernel beatability_of_the_take, decomposed per the epsilon-invariance principle: folk_mountain_reading treats the take as a Mountain-like floor on individual outcome prediction (near-zero contested extraction from that seat); flow_extraction_reading treats the track's takeout mechanism itself as the primary extractive structure with the public as direct victim; public_risk_reading treats the pool's aggregate risk-bearing as the analytical object, largely orthogonal to any specific actor's edge; this meta_prediction_reading claims a distinct actionable layer — predicting the divergence between the public's implied probability and true probability — that the other three readings' framings structurally cannot see, because each of them takes a different object as the relevant prediction target. All four share the same underlying pari-mutuel kernel and must remain linked via affects_constraints; none averages or hedges across the others' epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
