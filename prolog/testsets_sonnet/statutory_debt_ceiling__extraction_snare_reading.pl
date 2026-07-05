% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Legislative-Minority Hostage Mechanism
 *   domain: constitutional/political economy/fiscal governance
 *
 * SUMMARY:
 *   This story instantiates the extraction_snare_reading of the
 *   statutory_debt_ceiling kernel: the debt ceiling as a boundary that, once
 *   decoupled from any genuine operational necessity, functions as a
 *   recurring hostage mechanism. A cohesive legislative minority (whether a
 *   faction within a majority conference or a chamber majority representing
 *   an electoral minority) withholds routine reauthorization, manufacturing a
 *   deadline (the Treasury 'X-date') against which it extracts policy
 *   concessions unrelated to the debt itself, while risk and reputational
 *   costs fall on bondholders, program beneficiaries, federal workers, and
 *   the general public. The 1995-96, 2011, 2013, and 2023 episodes are read
 *   here as instances of the same extraction pattern intensifying over time,
 *   not as isolated fiscal disputes. This is one of three readings of the
 *   debt-ceiling kernel; the coordination_scaffold_reading treats the same
 *   statute as legitimate Treasury-operations coordination, and the
 *   constitutional_nullity_reading treats it as void under the 14th
 *   Amendment. Each reading has a different beneficiary/victim structure and
 *   a different epsilon; they are not the same constraint measured
 *   differently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.81).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Legislative-Minority Hostage Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political economy/fiscal governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '3d3c1064-92da-4d80-a93e-5ac85c91fc56').
narrative_ontology:cs_kernel_codification('3d3c1064-92da-4d80-a93e-5ac85c91fc56', formalized).
narrative_ontology:cs_authority_grounding('3d3c1064-92da-4d80-a93e-5ac85c91fc56', extraction).
narrative_ontology:cs_interpretation_layer_present('3d3c1064-92da-4d80-a93e-5ac85c91fc56').
narrative_ontology:cs_reading_relation('3d3c1064-92da-4d80-a93e-5ac85c91fc56', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d3c1064-92da-4d80-a93e-5ac85c91fc56', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('3d3c1064-92da-4d80-a93e-5ac85c91fc56', foundational, power_of_purse_licenses_hostage_leverage).
narrative_ontology:cs_axiom_status(power_of_purse_licenses_hostage_leverage, holdable).
narrative_ontology:cs_axiom_grounding('3d3c1064-92da-4d80-a93e-5ac85c91fc56', power_of_purse_licenses_hostage_leverage, conventional).
narrative_ontology:cs_axiom('3d3c1064-92da-4d80-a93e-5ac85c91fc56', foundational, original_coordination_rationale_is_defunct).
narrative_ontology:cs_axiom_status(original_coordination_rationale_is_defunct, holdable).
narrative_ontology:cs_axiom_grounding('3d3c1064-92da-4d80-a93e-5ac85c91fc56', original_coordination_rationale_is_defunct, empirically_contingent).
narrative_ontology:cs_reference_frame('3d3c1064-92da-4d80-a93e-5ac85c91fc56', wartime_operational_flexibility_framework).
narrative_ontology:cs_drift_state('3d3c1064-92da-4d80-a93e-5ac85c91fc56', post_2011_downgrade_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3d3c1064-92da-4d80-a93e-5ac85c91fc56', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, brinksmanship_faction_leadership).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, opposition_party_negotiators).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiaries_dependent_on_payments).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_civilian_workforce).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, state_and_local_governments_receiving_federal_transfers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_department).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, congressional_power_of_the_purse_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cohesive minority bloc within the majority conference (or a chamber majority representing a national minority of the electorate) withholds routine ceiling-raise votes, timing the impasse against the Treasury's projected 'X-date' to maximize leverage. They face essentially no personal financial exposure to default and can defect from a deal at low cost, while extracting spending cuts, policy riders, or symbolic concessions unrelated to the debt itself as the price of their votes.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, brinksmanship_faction_leadership, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, brinksmanship_faction_leadership, beneficiary).

% Party leadership uses the manufactured crisis to extract policy wins it could not obtain through ordinary appropriations, trading votes for concessions while publicly performing reluctant statesmanship. They benefit from the theater of crisis resolution regardless of which side 'wins' the underlying fiscal dispute.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, opposition_party_negotiators, beneficiary,
    organized, biographical, mobile, national).

% Domestic and foreign holders of U.S. Treasury securities bear repricing risk, technical default risk on specific instruments near the X-date, and permanent credit-rating and risk-premium effects (as in 2011 and 2023) that persist long after each episode resolves. They cannot exit U.S. sovereign debt exposure without systemic cost, since it underlies global collateral and reserve systems.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    powerful, immediate, constrained, global).

% Social Security recipients, veterans, Medicaid providers, and other program beneficiaries face the real threat of delayed or suspended payments if the Treasury exhausts extraordinary measures. They have no ability to hedge, no seat in the negotiation, and no alternative income source during an impasse.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiaries_dependent_on_payments, payer,
    powerless, immediate, trapped, national).

% Furloughs and delayed pay during adjacent shutdown/ceiling crises fall directly on federal employees and contractors. Their unions can lobby but cannot force resolution; changing employer means leaving federal service entirely.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_civilian_workforce, payer,
    moderate, biographical, constrained, national).

% Medicaid matching funds, education grants, and infrastructure disbursements are threatened by prioritization schemes that would follow a breach. States must plan contingency budgets for a crisis they did not cause and cannot influence.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, state_and_local_governments_receiving_federal_transfers, payer,
    moderate, biographical, constrained, national).

% Deploys extraordinary measures, projects the X-date, and absorbs the operational and reputational burden of managing a crisis it did not create and has no statutory tool to end unilaterally under this reading. It administers the mechanism but cannot fix it — Congress alone can raise, suspend, or repeal the ceiling.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_department, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, treasury_department, payer).

% Monitor the recurring brinkmanship pattern itself, not merely fiscal fundamentals, when setting sovereign ratings and outlooks — as in the 2011 S&P downgrade and 2023 Fitch downgrade, both citing governance dysfunction around the ceiling rather than debt levels alone.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, generational, analytical, global).

% Bear the diffuse long-run costs of elevated risk premia and reputational damage to U.S. credit but have no seat at the negotiating table and no mechanism to register objection between elections, by which point the crisis has typically already been resolved or deferred.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, general_public_taxpayers, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, brinksmanship_faction_leadership).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a single aggregate borrowing authority could let Treasury finance already-enacted spending and tax law without a separate vote for every issuance — but under this reading that coordination function is a pretext layered over a mechanism whose real operative use is to create a recurring, engineered crisis point with a hard deadline.
% TRANSFER_FUNCTION: Moves policy concessions (spending cuts, program riders, unrelated legislative wins) from the negotiating counterparty to the brinksmanship faction, and moves risk-premium costs, payment-delay risk, and reputational damage from that faction onto bondholders, federal payment recipients, federal workers, and the general public.
% ABSENT_VOICES: Social Security recipients, federal contractors, and foreign bondholders — the parties who actually bear default or near-default risk — have no representation in the closed-door negotiations that resolve each episode; state and local governments planning contingency budgets are informed only after leadership deals are struck.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling were repealed or permanently suspended, Congress would still control spending and taxation through ordinary appropriations, but the recurring hostage-crisis cycle — extraordinary measures, X-date brinkmanship, last-minute deals trading unrelated policy concessions for a vote — would end entirely; credit-rating volatility tied specifically to ceiling episodes would disappear, and the brinksmanship faction would lose a leverage point it currently uses for extraction unavailable through normal legislative process.
% FOUNDING_PROBLEM: The 1917 Second Liberty Bond Act consolidated piecemeal congressional authorization of individual bond issuances into a single aggregate ceiling, intended to give Treasury operational flexibility to finance wartime borrowing without a separate vote for every issuance.
% FOUNDING_PROBLEM_CORROBORATION: Treasury officials across multiple administrations (Democratic and Republican) have testified that the ceiling serves no operational financing function once Congress has already enacted the spending and tax law generating the borrowing need; the Bipartisan Policy Center and Government Accountability Office, both outside the brinksmanship faction's coalition, have documented that the mechanism no longer performs an authorization function distinct from appropriations and instead functions as a stand-alone leverage point, corroborating that the original coordination problem is dead even though the statute persists.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply at 2011 (S&P downgrade year) and again at 2023 (Fitch downgrade year), the two episodes where negotiations extracted the largest unrelated policy concessions (the Budget Control Act sequester in 2011; spending caps and permitting provisions in 2023) under the most acute default-proximity theater. Suppression tracks the same pattern: it measures how thoroughly alternative resolutions (clean raises, bipartisan pre-commitment, statutory reform) were foreclosed by the manufactured deadline dynamic, not a scaled function of extraction. Theater ratio is moderate throughout (0.3-0.5) because genuine market and payment risk is real, not purely performed, even though the crisis itself is engineered; it does not dominate the profile the way it would in a pure piton.
 *
 * PERSPECTIVAL GAP:
 *   From the brinksmanship faction's seat, each episode is a legitimate exercise of the power of the purse forcing fiscal restraint the ordinary appropriations process would not produce. From the bondholder, federal-beneficiary, and civilian-workforce seats, the identical structure is an engineered emergency used to extract concessions unconnected to appropriations, at a cost (rating downgrades, payment risk, furlough pay) they did not choose and cannot decline. The engine computing divergent per-seat types from the same structural data is exactly the phenomenon this reading is written to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The brinksmanship faction and the opposition negotiators sit at the beneficiary end: they extract concessions with near-zero personal exposure to the downside (arbitrage/mobile exit) and set the crisis's timing and terms (agenda_setter role). Bondholders, program beneficiaries, and civilian employees sit at the target end: they are trapped or constrained, cannot exit exposure to U.S. sovereign risk or federal payment dependency, and bear costs (rate spikes, payment delays, furloughs) generated entirely by a negotiation they do not participate in. Treasury occupies an unusual dual seat: institutionally powerful but structurally constrained — it administers the crisis but under this reading has no unilateral statutory escape (distinguishing it sharply from the constitutional_nullity_reading, where Treasury or the President could act unilaterally under 14th Amendment authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (operational flexibility for wartime bond issuance under the 1917 Act) is dead by Treasury's and GAO's own testimony, yet the statute persists and has been repurposed as an extraction lever precisely because its original coordination rationale gives the brinkmanship faction cover to claim it is merely enforcing fiscal discipline. Classifying this reading as snare (rather than tangled_rope) reflects the judgment that no live coordination function remains to weigh against the extraction — distinguishing it from the coordination_scaffold_reading, which asserts the coordination function is still live and legitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_function_liveness,
    'Is the ceiling''s original Treasury-operations coordination function genuinely dead, or does it retain some residual disciplining value that the extraction reading undercounts?',
    'Compare fiscal outcomes (deficit trajectories, spending growth) in the small number of jurisdictions and historical periods without a debt ceiling equivalent against the U.S. record; if outcomes are statistically indistinguishable, the disciplining function is not doing real work and the snare reading is strengthened.',
    'If a genuine residual coordination function is found, this constraint would more properly be read as tangled_rope (coordination plus extraction) rather than pure snare, and the beneficiary/victim asymmetry would need to be weighed against a live coordination benefit rather than a dead one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_liveness, empirical, 'Whether the ceiling still performs any genuine fiscal-discipline coordination function.').

omega_variable(
    faction_composition_stability,
    'Is the extraction beneficiary a stable, identifiable minority faction across episodes, or does the identity of the extracting party shift with which party holds the relevant chamber majority, making ''beneficiary'' a rotating rather than fixed role?',
    'Track which party/faction initiated brinkmanship in each of the 1995, 2011, 2013, and 2023 episodes and whether the same actors would object to the mechanism when in the minority themselves versus wielding it as leverage.',
    'If the beneficiary role rotates opportunistically with whichever faction holds leverage, this supports reading the mechanism as a structural feature exploitable by any minority coalition rather than a fixed capture by one political faction — reinforcing the snare classification''s structural (not partisan) character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(faction_composition_stability, empirical, 'Whether the extracting faction is fixed or rotates with legislative control.').

omega_variable(
    reading_selection_ambiguity,
    'Given that the same statutory text supports the extraction_snare, coordination_scaffold, and constitutional_nullity readings, what determines which reading a given political actor adopts, and is that choice itself strategic?',
    'Examine whether actors switch which reading they invoke (nullity vs. scaffold vs. snare) depending on which chamber or branch they currently control — a consistent switch pattern would indicate the reading choice tracks momentary institutional advantage rather than settled constitutional or economic analysis.',
    'If reading-switching correlates with control of Congress or the Presidency, it suggests all three readings are live strategic postures rather than purely analytical positions, which should inform how much weight the corpus places on any single reading''s claimed_type as a settled fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Whether the choice among the three kernel readings tracks institutional interest rather than fixed structural analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 1995, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1995, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(stat_tr_t2001, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement(stat_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(stat_tr_t2013, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2013, 0.35).
narrative_ontology:measurement(stat_tr_t2019, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(stat_tr_t2023, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2023, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t1995, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(stat_be_t2001, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(stat_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(stat_be_t2013, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement(stat_be_t2019, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2019, 0.6).
narrative_ontology:measurement(stat_be_t2023, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2023, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1995, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(stat_su_t2001, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(stat_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(stat_su_t2013, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2013, 0.65).
narrative_ontology:measurement(stat_su_t2019, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement(stat_su_t2023, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2023, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.05).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the natural-language 'debt ceiling' concept per the kernel-reading protocol: this story (extraction_snare_reading, high epsilon, snare), statutory_debt_ceiling__coordination_scaffold_reading (lower epsilon, scaffold/rope, treats the ceiling as legitimate operational coordination), and statutory_debt_ceiling__constitutional_nullity_reading (treats the entire mechanism as void under the 14th Amendment, a distinct constitutional-legitimacy claim rather than a political-economy extraction claim). All three share the same statutory kernel but diverge in claimed type, beneficiary/victim structure, and epsilon; they are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
