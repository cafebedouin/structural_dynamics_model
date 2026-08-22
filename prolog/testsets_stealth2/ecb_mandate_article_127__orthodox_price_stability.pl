% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate Article 127 - Orthodox Exclusive Price-Stability Reading
 *   domain: monetary policy/economic governance
 *
 * SUMMARY:
 *   Article 127 TFEU fixes price stability as the primary objective of the
 *   ESCB and appends secondary clauses (support for general Union policies,
 *   sustainable growth, employment) qualified by 'without prejudice.' The
 *   orthodox reading operationalizes this as exclusive focus on a 2 percent
 *   inflation target, rendering the secondary objectives subordinate and
 *   non-operational. This file generates THAT reading as one clean,
 *   epsilon-invariant constraint in a three-reading family: the siblings
 *   (expansive_secondary_objectives, climate_incorporation) are separate
 *   constraints with their own epsilon values, linked via
 *   network.affects_constraints. The epsilon referent is the standing
 *   exclusive-focus arrangement as it actually operates from 1999 to 2025 -
 *   not the expansive or climate-integrated arrangements this reading
 *   rejects. Claim and metrics are independent authored facts: the constraint
 *   is CLAIMED as tangled_rope (genuine anchor coordination plus asymmetric,
 *   actively enforced transfer), and the metrics are authored from the
 *   descriptive record without tuning toward any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - ecb_governing_council_orthodox_majority: Agenda setter (institutional/identity_locked) - sets rates and interprets the mandate; professionally fused with the stability tradition
 *   - ecb_legal_services: Enforcement arm (institutional/constrained) - drafts legal bases and screens instruments against the narrow reading
 *   - global_bond_investors: Primary beneficiary (powerful/arbitrage) - captures the concentrated gains of a surprise-free anchor
 *   - savers_and_net_creditors: Beneficiary (organized/mobile) - real balances preserved by the anchor
 *   - ordoliberal_policy_community: Beneficiary (institutional/identity_locked) - supplies doctrinal justification; careers ride on the hierarchy of objectives
 *   - indebted_peripheral_member_states: Payer (organized/trapped) - absorb real debt-burden shifts in tightening episodes
 *   - eurozone_workers: Payer (organized/constrained) - bear unemployment and wage suppression on the path to target
 *   - eurozone_mortgage_debtors: Payer (moderate/trapped) - payment shocks transmit within months of each hiking cycle
 *   - climate_exposed_regions_and_sectors: Payer (moderate/trapped) - risks unpriced in the collateral framework
 *   - mandate_expansion_litigants: Excluded (moderate/constrained) - reach the process only through courts and campaigns after positions harden
 *   - european_parliament_econ_committee: Observer (institutional/analytical) - questions on the record, no vote on instrument design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.54).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.7).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.54).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate Article 127 - Orthodox Exclusive Price-Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary policy/economic governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '4a9b016c-a051-4cc3-8af8-00bbc087db74').
narrative_ontology:cs_kernel_codification('4a9b016c-a051-4cc3-8af8-00bbc087db74', fixed_text).
narrative_ontology:cs_authority_grounding('4a9b016c-a051-4cc3-8af8-00bbc087db74', lineage).
narrative_ontology:cs_interpretation_layer_present('4a9b016c-a051-4cc3-8af8-00bbc087db74').
narrative_ontology:cs_reading_relation('4a9b016c-a051-4cc3-8af8-00bbc087db74', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('4a9b016c-a051-4cc3-8af8-00bbc087db74', ecb_mandate_article_127__climate_incorporation, forecloses).
narrative_ontology:cs_axiom('4a9b016c-a051-4cc3-8af8-00bbc087db74', foundational, price_stability_sole_operational_objective).
narrative_ontology:cs_axiom_status(price_stability_sole_operational_objective, holdable).
narrative_ontology:cs_axiom_grounding('4a9b016c-a051-4cc3-8af8-00bbc087db74', price_stability_sole_operational_objective, conventional).
narrative_ontology:cs_axiom('4a9b016c-a051-4cc3-8af8-00bbc087db74', secondary, hard_money_credibility_maximizes_long_run_welfare).
narrative_ontology:cs_axiom_status(hard_money_credibility_maximizes_long_run_welfare, holdable).
narrative_ontology:cs_axiom_grounding('4a9b016c-a051-4cc3-8af8-00bbc087db74', hard_money_credibility_maximizes_long_run_welfare, instrumental).
narrative_ontology:cs_reference_frame('4a9b016c-a051-4cc3-8af8-00bbc087db74', bundesbank_ordoliberal_stability_framework).
narrative_ontology:cs_drift_state('4a9b016c-a051-4cc3-8af8-00bbc087db74', post_2021_strategy_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a9b016c-a051-4cc3-8af8-00bbc087db74', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_net_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, global_bond_investors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, ordoliberal_policy_community).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, indebted_peripheral_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, eurozone_mortgage_debtors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, eurozone_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_regions_and_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the key interest rates and interprets the treaty mandate through Council statements, projections, and legal positions. Members serve eight-year non-renewable terms after careers built inside stability-oriented institutions; treating employment or climate goals as co-equal objectives would repudiate the professional tradition that selected them. Leaving means resignation into irrelevance or public reversal of long-held convictions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council_orthodox_majority, agenda_setter,
    institutional, generational, identity_locked, continental).

% Drafts the legal basis for every instrument and screens proposed collateral criteria and asset purchases against a narrow reading of the price-stability assignment. Resisted climate-linked collateral differentiation for years before the 2021 strategy review; its opinions are the working surface that keeps secondary objectives out of operations. Advancement runs through producing opinions the Court of Justice will uphold.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_legal_services, agenda_setter,
    institutional, generational, constrained, continental).

% Hold deposits, bonds, and pension claims denominated in euros; unexpected inflation taxes these balances, so a credibly maintained 2 percent anchor preserves their real value. Organized through consumer and pensioner associations in creditor countries. Assets can move into foreign currency or indexed instruments if the anchor weakens, so their stake is protected twice over.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_net_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Price euro-denominated debt against the anchor; a disciplined target strips inflation-surprise risk out of term premia and guarantees collateral predictability. Portfolios reprice across jurisdictions in milliseconds and no jurisdictional tie binds them to the euro area. They capture the largest concentrated gains from the arrangement while bearing none of its unemployment costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, global_bond_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Academics, former central bankers, and editorial writers in the stability tradition supply the intellectual case for the narrow reading and staff councils, courts, and finance ministries. Chairs, citation networks, and advisory standing depend on the hierarchy-of-objectives doctrine remaining operative. Embracing the expansive reading would devalue a lifetime of doctrinal capital.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ordoliberal_policy_community, beneficiary,
    institutional, generational, identity_locked, continental).

% Governments in southern Europe service euro-denominated debt they cannot inflate away and cannot repudiate without leaving the currency. Tightening episodes raise their funding costs exactly as growth slows, forcing austerity that deepens the slump. Exiting the euro would trigger banking collapse and default; staying means absorbing the real burden shift.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, indebted_peripheral_member_states, payer,
    organized, biographical, trapped, national).

% Households carrying variable-rate mortgages see payments rise within months of each hiking cycle, and disinflation raises the real weight of outstanding principal. No individual hedge is available at comparable cost and they hold no vote in the Council; their adjustment channel is cutting consumption.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_mortgage_debtors, payer,
    moderate, biographical, trapped, continental).

% Carry the unemployment and wage-suppression side of disinflationary episodes; the 2011 tightening and the 2022-23 cycle both cooled labor markets on the path to target. Unions bargain nationally and hold no seat in monetary decisions; moving between member states is limited by language, credential recognition, and housing costs.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_workers, payer,
    organized, biographical, constrained, continental).

% Farms, coastal industries, carbon-intensive firms in transition, and the regions that depend on them face physical and transition risks that the collateral framework historically declined to price, so their funding costs ignore their exposure. They cannot relocate out of the climate or out of the currency; their recourse is litigation and pressure aimed at other institutions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_exposed_regions_and_sectors, payer,
    moderate, generational, trapped, continental).

% NGOs, research institutes, and some member-state governments argue for operational weight on employment and climate. They reach the process only through amicus briefs, court actions, and public campaigns after positions have hardened; no formal channel lets them propose or block instrument design.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, mandate_expansion_litigants, excluded,
    moderate, generational, constrained, continental).

% Holds quarterly monetary dialogues with the President and publishes assessments of mandate fulfillment. Its questions shape the public record but carry no vote on instrument design; it watches the enforcement of the narrow reading without taking part in it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_parliament_econ_committee, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, global_bond_investors).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the time-inconsistency problem of discretionary monetary policy: a single credible nominal anchor at 2 percent coordinates the inflation expectations of roughly 350 million people, national wage bargains, and financial contracts across twenty member states, removing the temptation to engineer surprise inflation.
% TRANSFER_FUNCTION: Moves real purchasing power indirectly and episodically: tightening episodes shift real debt burdens from debtors toward creditors and output costs onto labor, while the anchor continuously protects savers' real balances from the inflation tax. It also moves agenda control: climate-allocation decisions are kept off the central bank balance sheet and left to fiscal actors.
% ABSENT_VOICES: Indebted households, workers displaced by tightening, climate-vulnerable regions, and future generations hold no seat in Governing Council deliberations. Mandate-expansion advocates enter only as litigants and commentators after decisions are made; unions appear in listening events with no decision rights. The unanimity around the narrow reading is produced in a room these parties never entered.
% DISAPPEARANCE_RATIONALE: If the exclusive-focus constraint vanished overnight, euro-area expectations would lose their anchor: wage negotiations, bond pricing, and the fiscal-monetary division of labor would all renegotiate around uncertainty, and climate-allocation fights would migrate onto the balance sheet immediately. The currency union's institutional architecture visibly depends on the arrangement continuing.
% FOUNDING_PROBLEM: The Great Inflation of the 1970s and the longer record of politically captured monetary policy: discretionary central banks engineered surprise inflation that became entrenched, imposing arbitrary losses on savers and wage earners. Maastricht's drafters built a legally insulated, hierarchically mandated central bank to make price stability constitutionally prior to every other goal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the time-inconsistency literature (Kydland-Prescott, Barro-Gordon) independently establishes the problem the hierarchy answers; the 1970s inflation record is cited by debtor-side economists who otherwise oppose the narrow reading; and even mandate-expansion litigants dispute the remedy, not the existence of the anchoring problem. No serious participant attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-substantial (0.54 at interval end) because the arrangement delivers a real public good - anchored expectations - while episodically transferring real resources to creditors: the 2011 hikes into the sovereign-debt crisis and the 2022-23 cycle both cooled labor markets and raised debtor burdens ahead of any symmetric benefit. Suppression is high (0.70) because keeping the secondary clauses non-operational requires continuous work: legal-services screening of collateral criteria, interpretive defense of every unconventional tool as mandate-consistent, and dilution of the 2021-25 climate action plans under political and judicial pressure. Theater is moderate (0.40) and rising: the targeting machinery is functional, but a growing share of activity is accountability performance - listening events, dialogue sessions, published strategies - that substitutes for operational change. The temporal series run on one shared eight-point grid (every tracked metric authored at every examined year). The dynamics are cyclical rather than monotonic: crisis forces pragmatic flexibility (2008-12, 2020), then orthodoxy reasserts and claws back flexibility while creditor protections persist - an asymmetric ratchet in which the oscillation itself is partly the mechanism, since episodic flexibility legitimizes the framework while steady-state operation reverts to the narrow mode. Base properties are measured at interval end (2025), mid-retrenchment phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the global bond investor's seat the arrangement is nearly pure coordination - a surprise-free anchor with arbitrage-grade exit if it fails. From the trapped payer seats (peripheral governments, mortgage debtors, workers, climate-exposed regions) the same structure operates as enforced transfer with no exit. From the agenda-setter seats it is experienced as fiduciary duty: the council majority and legal services do not collect listed rents, they administer and identify with the arrangement. The engine derives these per-seat classifications from power, exit options, and directionality; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: bond investors and savers receive preserved real value; the ordoliberal community collects authority and doctrinal capital. Victim declarations map to concentrated costs: debt-service burdens, unemployment spells, unpriced climate exposure. Trapped or constrained exit pins the payer seats near the full-target end; arbitrage-grade exit pins bond investors nearest the beneficiary pole; savers' mobile exit damps their effective burden despite genuine benefit. Two overrides are declared because the derivation cannot place the administering seats: the Governing Council majority and legal services hold no entry in the beneficiary arrays, so structural derivation would leave them at the power-atom fallback, yet both administer and identify with the arrangement (d 0.15 and 0.22 respectively - near-beneficiary, not rent-collecting). No overrides were needed for the payer seats: victim status plus trapped/constrained exit already derives high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - the Great Inflation and the time-inconsistency of discretionary monetary policy - is live, not dead: inflation returned violently in 2021-23 and the anchor had to be re-defended. No mandatrophy is declared. The hybrid classification guards against both mislabels: calling the arrangement pure extraction erases the anchor value delivered to every holder of euro balances, including wage earners' purchasing power; calling it pure coordination erases the documented asymmetric ratchet by which flexibility granted in crises is withdrawn afterward while creditor protections persist. Keeping both halves on the books routes the live dispute to measurable quantities - transfer magnitude and anchoring-equivalence - rather than to labels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the orthodox_price_stability reading of the ecb_mandate_article_127 kernel; would the sibling readings (expansive_secondary_objectives, climate_incorporation) produce a structurally different beneficiary set, victim set, and extraction profile from the same treaty text?',
    'Generate the sibling readings as separate constraint files and run a cross-reading classification diff on the shared kernel text.',
    'If the siblings compute broader beneficiary sets and lower suppression, the exclusive-focus reading is the load-bearing extractor in the family; if all three compute similar profiles, the extraction sits in the treaty assignment itself rather than in any one reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position marker: one reading of a contested kernel, siblings are separate constraints.').

omega_variable(
    anchor_necessity_vs_creditor_capture,
    'Is exclusive focus on a 2 percent point target a technical necessity for anchoring expectations across a twenty-state currency union, or a constructed hierarchy that concentrates gains on creditors?',
    'Cross-country comparison of flexible-mandate central banks (Federal Reserve, RBNZ, Bank of England) on expectation-anchoring quality and distributional outcomes at equivalent credibility.',
    'If flexible-mandate banks anchor expectations equally well, the exclusivity component is extractive overlay on a genuine coordination core; if they anchor worse, part of the measured burden is the price of the anchor itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anchor_necessity_vs_creditor_capture, empirical, 'Whether mandate exclusivity is technically required or distributionally motivated.').

omega_variable(
    distributional_transfer_magnitude,
    'How large are the real transfers from debtors and labor to creditors during exclusive-focus tightening episodes (2011, 2022-23) relative to a counterfactual balanced mandate?',
    'Event-study estimation of rate-hike incidence on mortgage servicing burdens, unemployment duration, and real bond returns, benchmarked against dual-mandate tightening cycles.',
    'Large measured transfers confirm the asymmetric half of the hybrid structure; negligible transfers collapse the classification toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_transfer_magnitude, empirical, 'Size of the debtor-to-creditor and labor-to-capital transfers under the narrow reading.').

omega_variable(
    climate_neutrality_claim_validity,
    'Does excluding climate risk from collateral valuation protect the anchor under the market-neutrality doctrine, or does it misprice the very assets the anchor depends on?',
    'Compare transmission and balance-sheet risk outcomes before and after the partial post-2021 climate integration; stress-test collateral pools with and without carbon tilts.',
    'If market neutrality is empirically unfounded, the externalization is pure cost-shifting onto exposed regions and future balance sheets; if founded, part of the exclusion is defensible coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_neutrality_claim_validity, empirical, 'Validity of the market-neutrality justification for climate externalization.').

omega_variable(
    council_identity_lock_durability,
    'Is the narrow reading sustained by accumulating evidence or by identity fusion with the Bundesbank lineage among appointed council members?',
    'Track voting patterns and dissent across successive appointment cohorts; convergence of younger non-German cohorts on broader readings without new evidence indicates identity rather than data sustains the arrangement.',
    'Identity-sustained enforcement decays with personnel turnover and predicts abrupt rather than gradual liberalization; evidence-sustained enforcement persists regardless of composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(council_identity_lock_durability, conceptual, 'Source of persistence: evidentiary or institutional-identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.18).
narrative_ontology:measurement(ecb__tr_t2003, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2003, 0.21).
narrative_ontology:measurement(ecb__tr_t2008, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(ecb__tr_t2011, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2011, 0.29).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2015, 0.34).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2021, 0.34).
narrative_ontology:measurement(ecb__tr_t2025, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.38).
narrative_ontology:measurement(ecb__be_t2003, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2003, 0.41).
narrative_ontology:measurement(ecb__be_t2008, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement(ecb__be_t2011, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2011, 0.59).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2021, 0.49).
narrative_ontology:measurement(ecb__be_t2025, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2025, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.46).
narrative_ontology:measurement(ecb__su_t2003, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2003, 0.49).
narrative_ontology:measurement(ecb__su_t2008, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(ecb__su_t2011, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2011, 0.61).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2020, 0.67).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2021, 0.64).
narrative_ontology:measurement(ecb__su_t2025, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, information_standard).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the ECB mandate.' The single treaty clause decomposes into three structurally distinct constraints distinguished by the operative status assigned to the secondary objectives: exclusive-and-non-operational (this file), discretionary-balancing (expansive reading), and binding-integration (climate reading). Each gets its own epsilon, beneficiary/victim structure, and classification; the upstream orthodox reading influences the downstream siblings because its dominance is what expansion advocates litigate against. Epsilon differs across the family because the referent arrangement differs: this file assesses the exclusive-focus regime; the siblings assess the regimes their readings would instantiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
