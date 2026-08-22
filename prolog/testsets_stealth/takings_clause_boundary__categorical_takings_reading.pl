% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Poles with Penn Central Balancing Middle (Takings Clause Boundary)
 *   domain: constitutional/legal/institutional
 *
 * SUMMARY:
 *   Since Penn Central (1978), consolidated by Loretto (1982) and Lucas
 *   (1992), United States takings doctrine has run on a two-track
 *   architecture: permanent physical occupations and total economic
 *   eliminations trigger compensation automatically, while every other
 *   regulation is weighed under three multi-factor balancing inquiries that
 *   almost never conclude in the owner's favor. The arrangement solves a real
 *   boundary problem — it tells regulators what is categorically forbidden
 *   and gives everyone a common vocabulary for the rest — while
 *   simultaneously transferring the ordinary costs of land-use regulation
 *   onto owners without payment. This story instantiates the
 *   categorical_takings_reading of the takings_clause_boundary kernel; the
 *   physical_appropriation_reading and regulatory_takings_reading siblings
 *   are separate constraints with their own epsilon values, beneficiary sets,
 *   and classifications (see network.dual_formulation_note). Claim/metric
 *   independence is deliberate: the claimed type is what I judge structurally
 *   true of the hybrid (both a working coordination device and a systematic
 *   uncompensated transfer), and the metrics describe its observed operation
 *   independently of that claim.
 *
 * KEY AGENTS:
 *   - - federal_judiciary: Agenda setter (institutional/analytical) — announces the per se categories, administers the balancing inquiry, controls the docket
 *   - - government_regulators: Primary beneficiary (institutional/arbitrage) — regulates without treasury outlay everywhere except the poles
 *   - - small_property_owners: Primary target (moderate/constrained) — absorbs uncompensated diminution in the middle zone
 *   - - large_developers_institutional_landowners: Dual-positioned payer/beneficiary (powerful/mobile) — funds the litigation, gains pole predictability, carries the largest absolute middle-zone losses
 *   - - public_beneficiaries_of_regulation: Incidental beneficiary (organized/constrained) — receives regulatory goods without funding compensation
 *   - - dismissed_claimant_owners: Excluded voice (powerless/trapped) — final losers with no further hearing
 *   - - constitutional_scholars: Analytical observer (moderate/analytical) — maps the gap between announced method and disposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.58).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.52).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Poles with Penn Central Balancing Middle (Takings Clause Boundary)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/legal/institutional").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'db362de4-cac7-4f92-90a4-ff564aca2057').
narrative_ontology:cs_kernel_codification('db362de4-cac7-4f92-90a4-ff564aca2057', fixed_text).
narrative_ontology:cs_authority_grounding('db362de4-cac7-4f92-90a4-ff564aca2057', lineage).
narrative_ontology:cs_interpretation_layer_present('db362de4-cac7-4f92-90a4-ff564aca2057').
narrative_ontology:cs_reading_relation('db362de4-cac7-4f92-90a4-ff564aca2057', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('db362de4-cac7-4f92-90a4-ff564aca2057', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('db362de4-cac7-4f92-90a4-ff564aca2057', foundational, extreme_states_compensated_by_rule_not_weighting).
narrative_ontology:cs_axiom_status(extreme_states_compensated_by_rule_not_weighting, holdable).
narrative_ontology:cs_axiom_grounding('db362de4-cac7-4f92-90a4-ff564aca2057', extreme_states_compensated_by_rule_not_weighting, conventional).
narrative_ontology:cs_axiom('db362de4-cac7-4f92-90a4-ff564aca2057', foundational, ordinary_regulation_presumptively_uncompensated).
narrative_ontology:cs_axiom_status(ordinary_regulation_presumptively_uncompensated, holdable).
narrative_ontology:cs_axiom_grounding('db362de4-cac7-4f92-90a4-ff564aca2057', ordinary_regulation_presumptively_uncompensated, instrumental).
narrative_ontology:cs_reference_frame('db362de4-cac7-4f92-90a4-ff564aca2057', categorical_poles_balanced_middle).
narrative_ontology:cs_drift_state('db362de4-cac7-4f92-90a4-ff564aca2057', contemporary_post_cedar_point, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db362de4-cac7-4f92-90a4-ff564aca2057', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, public_beneficiaries_of_regulation).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, small_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, large_developers_institutional_landowners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, dismissed_claimant_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, large_developers_institutional_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares which government actions require payment to owners and which do not. Announces the per se categories, administers the multi-factor inquiry for everything else, and chooses which disputes to hear. Members' reputations ride on the framework's perceived principledness; leaving the bench ends their participation but not the doctrine's operation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Land-use, environmental, and preservation agencies impose setbacks, density caps, permit conditions, and use restrictions. Provided they avoid permanent occupations and total value elimination, they place burdens on owners without treasury outlay. They select among many available regulatory instruments and can steer toward those that stay clear of the payment-triggering categories.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, beneficiary,
    institutional, generational, arbitrage, national).

% Hold homes and small parcels subject to zoning, environmental, and preservation restrictions that cut value substantially short of total loss. The restriction runs with the land, so selling does not escape it. Challenging it means years of litigation against well-resourced public counsel under factors that seldom produce payment; most absorb the loss.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, small_property_owners, payer,
    moderate, biographical, constrained, regional).

% Build and hold large portfolios. They fund nearly all serious litigation and win the occasional landmark ruling at the categories' edges, gaining usable predictability there. They can structure projects around known limits and spread risk across jurisdictions, yet they still carry the largest absolute share of uncompensated restriction costs in the middle range.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, large_developers_institutional_landowners, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, large_developers_institutional_landowners, beneficiary).

% Receive environmental protection, preserved streetscapes, orderly development, and hazard avoidance delivered not through treasury payments to owners but through restrictions placed on them. They bear none of the framework's direct costs and have little incentive to support compensation expansions; their electoral weight anchors the middle zone's political durability.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, public_beneficiaries_of_regulation, beneficiary,
    organized, generational, constrained, national).

% Former claimants whose compensation petitions were rejected under the multi-factor inquiry. Their losses are final, their parcels remain restricted, and the doctrine affords no further hearing. They would testify to the distance between the framework's promise of case-specific fairness and their own outcomes, but they hold no seat in its administration.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, dismissed_claimant_owners, excluded,
    powerless, biographical, trapped, regional).

% Document the doctrine's evolution, measure the gap between the announced factors and actual dispositions, and propose rival architectures. They shape the law's long-run direction through citation networks, clerkship pipelines, and nomination debates, but decide nothing themselves.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, constitutional_scholars, observer,
    moderate, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the standing boundary dispute between the compensation guarantee and the police power: it tells regulators which regulatory forms are categorically off-limits (permanent occupation, total value elimination) and supplies a shared evaluative vocabulary for the remaining run of land-use disputes, so regulation can proceed without case-by-case constitutional crisis.
% TRANSFER_FUNCTION: Moves the economic burden of land-use regulation from government budgets to affected owners throughout the middle zone (uncompensated diminution), while routing extreme burdens at the poles back to government as payable claims; it also moves litigation cost and risk onto claimants, who must finance challenges under factors that rarely conclude in their favor.
% ABSENT_VOICES: Owners whose claims were dismissed under the balancing inquiry absorb the losses the framework assigns but have no voice in its formation; future owners inherit regulated parcels with pre-diminished expectations they never consented to. State legislatures experimenting with statutory compensation schemes sit partly outside the doctrinal conversation that governs their constituents.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, every land-use agency's operating assumptions would collapse simultaneously: under the physical-appropriation sibling, regulators could destroy value through paper without paying, triggering political upheaval and defensive legislation; under the regulatory sibling, treasuries would face mass compensation liability for ordinary zoning. Either replacement rearranges the fiscal and regulatory landscape of every developed jurisdiction.
% FOUNDING_PROBLEM: Reconciling two commitments in collision since the repudiation of the Lochner era: protecting owners against government destroying property value through regulation, while preserving the police power to regulate land use for health, safety, and welfare without bankrupting public treasuries. Penn Central (1978) was constructed expressly as the middle course after half a century of doctrinal oscillation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: property-rights litigators and academic critics who oppose the framework concede the underlying tension is real; state legislatures enacting statutory compensation regimes attest the problem persists beneath the doctrine; government amici defending the framework acknowledge the fiscal stakes openly. Both camps affirm the problem is live while disputing the solution — corroboration of the problem, not of the answer.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the middle zone covers the overwhelming run of land-use disputes and resolves them without payment, a large recurring transfer, but the poles do return extreme cases to the treasury side, bounding the take. Suppression 0.52 is authored as a raw structural property — unscaled by power or scope; the engine owns any scaling. It reflects closure of alternatives: due-process challenges to land-use regulation fail routinely, state constitutional routes are weak, and the compensation claim itself is only available through the framework being evaluated. Theater 0.45: the per se lines do real work, but the balancing factors are notoriously conclusory — announced in every opinion, predictive in few — so a substantial minority of the framework's operative activity is performative announcement. Accessibility collapse 0.60: once an owner understands the architecture, rival theories largely fold into it. Resistance 0.62: a sustained property-rights bar, organized scholarship, state statutory compensation experiments, and periodic Supreme Court reversals show the arrangement must be actively defended. Coalition note: individually moderate owners hold latent coalition power — ballot measures and state legislatures have produced real statutory compensation regimes — which is why suppression is authored below the level of a locked system. The suppression_requirement series tracks genuine enforcement-capacity change: the Court's takings docket intensified from near-zero salience in 1978 to routine category-policing by the 2010s, then plateaued; this is an enforcement-buildup story, so the series is authored on the shared nine-point grid alongside the other metrics.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the regulator seat the arrangement is a legible map of permissible action — bright lines to avoid, a forgiving interior. From the small-owner seat the same interior is a confiscation without remedy: value taken, factors recited, claim dismissed. From the judiciary seat it is principled moderation between two failed extremes. Nothing in the authored claim adjudicates among these; the engine derives each seat's classification from power, exit, and directional position, and the divergence between the regulator's and the owner's computed experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators sit near the beneficiary end: the arrangement subsidizes their instrument choice, and their arbitrage-grade exit (selecting among regulatory tools) places them furthest from the target pole. Public beneficiaries of regulation are diffuse incidental gainers — they receive the goods regulation produces without bearing its costs. Small property owners sit near the full-target end: they bear the transfer with constrained exit (restrictions run with the land). Large developers occupy a genuinely dual position — targets of the middle-zone transfer, beneficiaries of pole-level predictability — captured by paired roles rather than a forced single d. Dismissed claimants are the purest targets: trapped, finalized losses. The federal judiciary collects a mild benefit the structural arrays do not encode — doctrinal discretion and agenda control accrue to the seat that administers the framework — but directionality overrides are keyed at power-atom granularity, and regulators share the judiciary's institutional atom with a very different derived d, so the correction is documented here rather than forced through an override that would misprice the regulator seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling the compensation guarantee with a workable police power — remains live: climate adaptation, housing scarcity, and preservation conflicts press on the boundary daily, corroborated by partisans on both sides. Nothing here is vestigial; the framework performs its coordination function continuously even as it extracts through the middle zone. Classifying it as tangled_rope prevents two symmetrical errors: reading it as pure coordination (rope) would erase the identifiable owners who pay uncompensated for everyone else's regulatory goods; reading it as pure extraction (snare) would erase the genuine notice function that bounds regulatory predation at the poles. The live founding problem plus a world_rearranges disappearance verdict yields no mismatch flag — the arrangement persists because the problem persists, not because anyone is maintaining a corpse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the takings_clause_boundary kernel (the fixed Takings Clause text). What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling files plus engine foreclosure computation across the reading set; the disagreement is located at the position of the compensation trigger — physical invasion versus economic effect versus categorical-plus-balancing hybrid.',
    'If the physical_appropriation_reading prevailed, owners lose the total-wipeout per se category and regulator seats gain; if the regulatory_takings_reading prevailed, treasuries become liable for substantial diminutions and effective extraction on government seats rises sharply. This reading''s epsilon is valid only for the hybrid arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, where the readings diverge.').

omega_variable(
    penn_central_predictive_validity,
    'Do the three announced factors (economic impact, investment-backed expectations, character of government action) actually predict outcomes, or do they rationalize conclusions reached on other grounds?',
    'Systematic coding of the reported case universe: regress dispositions on factor profiles and test whether the factors carry predictive weight independent of result-oriented language.',
    'If the factors lack predictive power, the middle zone operates as close to unreviewable discretion and the authored theater_ratio understates the performative share; if they predict, the balancing is a genuine decision procedure and the hybrid''s coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictive_validity, empirical, 'Whether the balancing inquiry is a working test or conclusory rationalization.').

omega_variable(
    middle_zone_burden_share,
    'What share of the aggregate regulatory burden imposed on owners falls in the uncompensated middle zone versus the compensated poles?',
    'Econometric aggregation of parcel-level value effects of land-use restrictions against the population of compensation awards and settlements.',
    'Determines whether the framework''s net transfer is modest coordination overhead or the dominant wealth flow in American land use; a dominant-flow finding pushes the arrangement toward extraction-dominant classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middle_zone_burden_share, empirical, 'Magnitude of the uncompensated middle-zone transfer relative to pole-level compensation.').

omega_variable(
    expectation_stabilization_efficacy,
    'Does the hybrid actually stabilize owner expectations — its claimed justification — or does middle-zone uncertainty negate stabilization for the majority of owners who never reach the poles?',
    'Panel studies comparing investment behavior under known per se lines versus middle-zone exposure; survey of developer and owner expectations before and after landmark rulings.',
    'If stabilization fails for most owners, the coordination half of the hybrid weakens and the arrangement reads as extraction riding a thin coordination shell; if it succeeds, the tangled characterization is confirmed from the coordination side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expectation_stabilization_efficacy, conceptual, 'Whether the predictability benefit is real for median owners or confined to repeat players at the poles.').

omega_variable(
    purchase_price_offset_question,
    'To what extent do owners recover the regulatory burden through lower purchase prices — i.e., is the middle-zone loss capitalized at acquisition such that later owners are not net victims?',
    'Hedonic pricing studies of regulated versus comparable unregulated parcels, controlling for acquisition date relative to restriction enactment.',
    'If capitalization offsets most of the burden, the victim set shrinks to pre-enactment owners and measured extraction falls; if restrictions post-date acquisition for most claimants, the victim set stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purchase_price_offset_question, empirical, 'Whether market capitalization neutralizes the middle-zone transfer for successor owners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcb_categorical_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement(tcb_categorical_tr_t1984, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1984, 0.33).
narrative_ontology:measurement(tcb_categorical_tr_t1990, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(tcb_categorical_tr_t1996, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1996, 0.38).
narrative_ontology:measurement(tcb_categorical_tr_t2002, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2002, 0.4).
narrative_ontology:measurement(tcb_categorical_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.42).
narrative_ontology:measurement(tcb_categorical_tr_t2014, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2014, 0.43).
narrative_ontology:measurement(tcb_categorical_tr_t2019, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2019, 0.44).
narrative_ontology:measurement(tcb_categorical_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(tcb_categorical_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.44).
narrative_ontology:measurement(tcb_categorical_be_t1984, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1984, 0.47).
narrative_ontology:measurement(tcb_categorical_be_t1990, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(tcb_categorical_be_t1996, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement(tcb_categorical_be_t2002, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2002, 0.54).
narrative_ontology:measurement(tcb_categorical_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement(tcb_categorical_be_t2014, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(tcb_categorical_be_t2019, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(tcb_categorical_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcb_categorical_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(tcb_categorical_su_t1984, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1984, 0.42).
narrative_ontology:measurement(tcb_categorical_su_t1990, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(tcb_categorical_su_t1996, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1996, 0.47).
narrative_ontology:measurement(tcb_categorical_su_t2002, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2002, 0.49).
narrative_ontology:measurement(tcb_categorical_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.51).
narrative_ontology:measurement(tcb_categorical_su_t2014, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement(tcb_categorical_su_t2019, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2019, 0.52).
narrative_ontology:measurement(tcb_categorical_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'takings law' conflates three structurally distinct compensation-trigger arrangements instantiated by one fixed kernel text. This file authors the categorical hybrid reading: bright per se triggers at the extremes, multi-factor balancing elsewhere. The physical_appropriation_reading sibling authors a narrower arrangement whose victim set includes owners suffering total regulatory wipeout (unprotected there); the regulatory_takings_reading sibling authors a broader arrangement whose payer set includes every treasury exposed to substantial-diminution claims. Epsilon differs across the family because the beneficiary/victim sets differ, not because any single arrangement is measured inconsistently. Upstream/downstream: the categorical reading absorbs the physical pole and channels the regulatory impulse, so it structurally influences both siblings' operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
