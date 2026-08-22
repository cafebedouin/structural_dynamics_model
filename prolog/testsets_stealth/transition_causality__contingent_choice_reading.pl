% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Post-Bretton Woods Fiat Dollar Standard (Contingent-Choice Reading)
 *   domain: economic/political/international_finance
 *
 * SUMMARY:
 *   On August 15, 1971 the United States suspended dollar convertibility into
 *   gold, ending the Bretton Woods par-value commitment. This story
 *   instantiates ONE reading of the resulting arrangement: the
 *   contingent-choice reading, under which the fiat dollar standard is a
 *   maintained artifact of repeated U.S. policy decisions, not an inevitable
 *   outcome. The standing arrangement under contest — the post-1971 regime in
 *   which the U.S. issues the world's reserve asset without external anchor —
 *   is scored by this reading's own lights: it delivers genuine global
 *   coordination (a single deep reserve asset, crisis lender-of-last-resort
 *   swap lines) while transferring seigniorage and adjustment costs outward,
 *   and it persists because each administration has chosen to preserve it
 *   from an open menu that included restoring an anchor. The claim and the
 *   metrics are independent authored facts: claimed_type is tangled_rope
 *   because the reading sees real coordination fused with asymmetric
 *   extraction under active enforcement; the metrics describe the regime's
 *   actual operation without being tuned to that claim. Family membership:
 *   this file is one of three readings of the transition_causality kernel;
 *   the sibling stories carry their own epsilon values and beneficiary
 *   structures, and the epsilon difference across members follows from each
 *   reading assessing the same referent arrangement by its own lights.
 *
 * KEY AGENTS:
 *   - us_federal_government: Agenda-setter and primary beneficiary (institutional/arbitrage) — administers the fiat arrangement, collects seigniorage and deficit-financing capacity, and demonstrated reserve-freeze capability in 2022
 *   - us_financial_sector: Secondary beneficiary (powerful/arbitrage) — collects dollar-centrality rents (funding franchise, Treasury intermediation, invoicing dominance) without administering the regime
 *   - dollar_reserve_accumulators: Primary payer (powerful/constrained) — export-led economies absorbing reserve-accumulation costs with slow, self-penalizing diversification as the only exit
 *   - commodity_importing_debtors: Primary payer (moderate/trapped) — dollar debtors bearing imported inflation and every tightening cycle's adjustment burden
 *   - gold_convertibility_creditors: Historical payer (powerful/mobile) — European governments that lost redemption rights in 1971 and subsequently diversified out of concentrated exposure
 *   - alternative_reserve_issuers: Excluded challenger (institutional/trapped) — euro and renminbi issuers kept from reserve primacy by network incumbency rather than formal rule
 *   - international_monetary_historians: Analytical observer — adjudicates the causal contest from released archives with no material stake in the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.72).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.68).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Post-Bretton Woods Fiat Dollar Standard (Contingent-Choice Reading)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "economic/political/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '4e736a90-c7b9-4a43-8d31-6dd10607b244').
narrative_ontology:cs_kernel_codification('4e736a90-c7b9-4a43-8d31-6dd10607b244', distributed).
narrative_ontology:cs_authority_grounding('4e736a90-c7b9-4a43-8d31-6dd10607b244', distributed).
narrative_ontology:cs_reading_relation('4e736a90-c7b9-4a43-8d31-6dd10607b244', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('4e736a90-c7b9-4a43-8d31-6dd10607b244', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('4e736a90-c7b9-4a43-8d31-6dd10607b244', foundational, counterfactual_alternatives_were_viable).
narrative_ontology:cs_axiom_status(counterfactual_alternatives_were_viable, holdable).
narrative_ontology:cs_axiom_grounding('4e736a90-c7b9-4a43-8d31-6dd10607b244', counterfactual_alternatives_were_viable, empirically_contingent).
narrative_ontology:cs_axiom('4e736a90-c7b9-4a43-8d31-6dd10607b244', foundational, us_policy_autonomy_was_decisive_gain).
narrative_ontology:cs_axiom_status(us_policy_autonomy_was_decisive_gain, holdable).
narrative_ontology:cs_axiom_grounding('4e736a90-c7b9-4a43-8d31-6dd10607b244', us_policy_autonomy_was_decisive_gain, empirically_contingent).
narrative_ontology:cs_reference_frame('4e736a90-c7b9-4a43-8d31-6dd10607b244', regime_as_deliberate_policy_choice).
narrative_ontology:cs_drift_state('4e736a90-c7b9-4a43-8d31-6dd10607b244', post_archival_release_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4e736a90-c7b9-4a43-8d31-6dd10607b244', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_federal_government).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_financial_sector).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, dollar_reserve_accumulators).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, commodity_importing_debtors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_convertibility_creditors).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, fiat_money_global_reserve_viability).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, national_currency_internationalization_strategy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Since August 1971 each administration has faced the same menu — restore an external anchor, negotiate shared adjustment, or preserve unilateral discretion — and has repeatedly chosen preservation. It collects seigniorage, borrows in its own currency at a scale no other state can match, and in 2022 demonstrated the ability to freeze a rival central bank's reserves. Abandoning the role would forfeit the financing advantage; maintaining it requires continuous enforcement of dollar-network centrality.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_federal_government, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, us_federal_government, beneficiary).

% Dollar centrality supplies the global dollar-funding franchise, Treasury-market intermediation fees, and invoicing-and-settlement dominance. It collects these rents without administering the regime and lobbies for continuity, but bears little of the arrangement's cost and could relocate its book if the network migrated.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_financial_sector, beneficiary,
    powerful, biographical, arbitrage, global).

% Export-led economies (Japan and Germany historically, China later) accumulate Treasury claims as the byproduct of exchange-rate management. Holdings grow too large to liquidate without self-inflicted losses, so diversification proceeds slowly and at the margin. The arrangement taxes their savers through dollar depreciation while subsidizing their exporters through suppressed currencies — a mix their own policy choices helped produce.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, dollar_reserve_accumulators, payer,
    powerful, generational, constrained, global).

% They borrow in dollars they cannot issue. The 1970s delivered imported inflation; the 1980s Volcker tightening converted it into debt crises and lost decades of development; every subsequent tightening cycle repeats the pattern. There is no alternative credit source of comparable scale, and default means losing market access entirely.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, commodity_importing_debtors, payer,
    moderate, biographical, trapped, global).

% European governments, notably France, held dollar reserves against the treaty promise of gold redemption at $35 per ounce. The 1971 suspension extinguished that right overnight, converting their claims to paper. Survivors diversified into gold and deutsche marks; their concentrated exposure to the fiat arrangement is now largely a historical loss rather than an ongoing flow.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_convertibility_creditors, payer,
    powerful, biographical, mobile, continental).

% Euro-area and Chinese authorities could offer reserve alternatives but face dollar network lock-in, shallower safe-asset markets, and distrust of their capital controls or legal systems. Their exclusion from reserve primacy is maintained by incumbency advantages the arrangement protects rather than by any formal rule they could petition to change.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, alternative_reserve_issuers, excluded,
    institutional, generational, trapped, continental).

% They reconstruct the 1969-1973 decision record from released Treasury, Federal Reserve, and foreign central-bank archives, publish counterfactual analyses, and adjudicate among competing causal accounts. They hold no material stake in the arrangement and their assessments feed no enforcement mechanism.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_historians, observer,
    analytical, civilizational, analytical, global).

% It inherited a mandate built for par-value surveillance; after 1973 it rebranded as overseer of floating rates. Its governance is dominated by the U.S. quota share, so it documents adjustment asymmetries in Article IV reports but cannot compel the issuer, and its institutional continuity depends on the arrangement it monitors.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, imf_surveillance_mission, observer,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_federal_government).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the world a single deep, liquid reserve asset and settlement medium, solving the n-country problem of settling trade imbalances without a commodity anchor; dollar swap lines function as a global lender of last resort in crises.
% TRANSFER_FUNCTION: Moves real goods, resources, and policy discipline from reserve-accumulating and debtor countries to the United States in exchange for dollar claims whose purchasing power the issuer alone controls; seigniorage and adjustment costs flow outward, financing capacity flows inward.
% ABSENT_VOICES: Surplus-country households whose savings absorbed U.S. deficits, developing-country debtor publics who bore 1980s austerity, and gold-holding creditor legislatures were absent from the August 1971 decision, which was announced without prior consultation of allies or the IMF. Alternative-reserve advocates remain outside the governance core today; the arrangement's periodic reform reviews seat them as witnesses, not principals.
% DISAPPEARANCE_RATIONALE: If the fiat dollar standard vanished overnight, trade invoicing, reserve portfolios, cross-border bank funding, and the multi-trillion-dollar Treasury market would lose their anchor simultaneously; settlement would seize until a successor anchor emerged, and the distribution of seigniorage and adjustment burdens would be renegotiated from scratch — the world rearranges around whatever replaces it.
% FOUNDING_PROBLEM: By 1971 the United States faced the Triffin bind — supplying world reserves through deficits while defending $35-per-ounce gold convertibility — compounded by Vietnam-era inflation and an accelerating run on the gold stock; external convertibility and domestic policy autonomy had become mutually unsustainable.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians outside the beneficiary set (Eichengreen, Bordo, James) and IMF-independent scholarship attest that the Triffin tension was real and remains unresolved rather than solved. Contemporaneous records — Treasury memoranda from Connally and Volcker, Bundesbank and Bank of France correspondence, Congressional hearing transcripts — corroborate both the acute 1971 bind and the unilateral character of the response. No source outside the benefiting parties attests that the deeper anchoring problem was solved; the U.S. Treasury's own attestations are inside the beneficiary set and are discounted accordingly.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the arrangement's core transfer — reserve accumulation against claims whose purchasing power the issuer alone controls — is decoupled from any service the issuer renders at the margin, and because sanctions leverage monetizes the network directly. Suppression (0.68) reflects the maturing enforcement machinery: exclusion from dollar clearing is existential for banks and states, FATCA and secondary sanctions extend reach extraterritorially, and the 2022 reserve freeze demonstrated outright confiscation capacity. Theater (0.46) is moderate: monetary operations and crisis swap lines are real work, but a growing share of activity is multilateral reform ritual (G20 communiques, IMF surveillance reports, 'strong dollar' rhetoric) that substitutes for adjustment. Accessibility_collapse is 0.5: alternatives (gold, euro, renminbi, SDR, commodity baskets) are well understood and each collapses under scrutiny of depth, convertibility, or rule-of-law risk, yet none is impossible — this is not a natural law. Resistance (0.48) is real but ineffective so far: record central-bank gold purchases, BRICS settlement experiments, and persistent de-dollarization rhetoric with minimal actual displacement. The measurement series run on one shared time grid (t=0,10,20,30,40,50) with every tracked metric authored at every point; the suppression_requirement series is authored because the story specifically traces enforcement-capacity build-up, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute different types from identical structural data. From the issuer's position the arrangement is stewardship it performs credibly: it supplies the safe asset the world demands, lends freely in crises, and defends the network everyone uses. From the trapped debtor seat the same structure operates as enforced extraction: adjustment is always outward, discipline is always for others, and exit means default. The powerful-but-constrained accumulator seat sits between — it profits from undervalued exchange rates while its savers absorb the reserve tax. The observer seat sees none of the operation directly, only the contested causal record. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The issuer declares beneficiary status and holds arbitrage-grade exit (it defines the unit of account), placing it near the beneficiary end where effective extraction inverts toward subsidy. The financial sector, a pure beneficiary with arbitrage positioning, derives similarly low d. Commodity-importing debtors declare victim status with trapped exit, deriving near-full-target d — the arrangement extracts from them at close to full strength. Reserve accumulators declare victim status but hold powerful power atoms with constrained (not trapped) exit, and their accumulation was partly a chosen development strategy; the derivation tempers their d below the full-target end accordingly. Gold-convertibility creditors declare victim status for a discrete 1971 loss but hold mobile exit — they diversified — so their derived d sits mid-range, reflecting largely extinguished ongoing exposure. Alternative reserve issuers are excluded rather than coordinated: high d without receipt. No directionality overrides are authored because exit_options already differentiate the same-power seats (arbitrage vs constrained vs mobile), which is exactly what the derivation chain reads.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two opposite mislabels. Against mountain mislabeling: this reading's entire content is that the arrangement was chosen and is re-chosen, so emerges_naturally is false and the FSM naturalness profile is withheld — the regime is not presented as natural law even though its beneficiaries sometimes describe it that way ('there is no alternative to the dollar'). Against pure-snare mislabeling: the arrangement genuinely solves the n-country reserve-asset problem, and its coordination function would survive a more symmetric distribution of adjustment costs, so the extraction rides on coordination rather than replacing it. Mandatrophy residue exists inside the machinery but not in the core constraint: the IMF's original par-value surveillance mandate died with the system in 1973 and the institution persists under a rebranded mandate — that is a candidate piton story for a separate file — while the fiat dollar arrangement itself still performs its coordinating function and therefore carries no mandatrophy_resolved declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_depth,
    'Were the 1971 alternatives (coordinated revaluation, an SDR substitution account, staged gold-price revision) actually implementable, or did fiscal-political constraints render them unavailable?',
    'Archival reconstruction of the 1969-1971 option papers (Treasury, Federal Reserve, OEP) combined with counterfactual modeling of each proposal''s macroeconomic path.',
    'If the alternatives were non-viable, this reading loses its foundational premise and the standing arrangement migrates toward the hybrid or overdetermined sibling classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_depth, empirical, 'Depth of viable counterfactuals at the 1971 decision point.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the contingent_choice_reading of kernel transition_causality; what would adopting a sibling reading change structurally?',
    'Compare the three sibling stories'' epsilon values, beneficiary declarations, and computed types: the overdetermined sibling attributes the arrangement to structural necessity (lowering choice-responsibility and beneficiary salience); the hybrid sibling splits causation between accumulated contradiction and trigger event.',
    'The classification of the standing arrangement shifts with the adopted reading: this reading keeps the arrangement choice-maintained and actively enforced; the overdetermined reading pushes toward naturalized, mountain-like framing; the hybrid reading sits between. Disagreement is located at the counterfactual-viability premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel.').

omega_variable(
    strategic_vs_coerced_accumulation,
    'Are the costs borne by dollar_reserve_accumulators coerced by the arrangement, or voluntarily incurred as exchange-rate-management and export-led-growth strategy?',
    'Welfare comparison of surplus economies against counterfactual appreciation paths, plus archival evidence on the accumulators'' own stated policy objectives.',
    'If accumulation was largely strategic, effective extraction from that seat drops and the arrangement reads closer to pure coordination for those actors; if coerced, the extraction asymmetry sharpens toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_coerced_accumulation, empirical, 'Coerced versus chosen nature of reserve-accumulation costs.').

omega_variable(
    suppression_network_vs_enforcement,
    'Is the measured suppression of exits primarily network lock-in (structural) or active enforcement (sanctions, clearing access, reserve freezes)?',
    'Decompose failed de-dollarization episodes into network-friction causes versus explicit coercive intervention.',
    'If mostly structural, suppression would persist under benign U.S. policy and the rising enforcement series overstates discretionary coercion; if enforcement-driven, the rising suppression series tracks policy choice, which reinforces this reading''s contingency claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_network_vs_enforcement, empirical, 'Structural versus enforced component of dollar-system suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tran_tr_t0, observed).
narrative_ontology:measurement(tran_tr_t10, transition_causality__contingent_choice_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(tran_tr_t10, observed).
narrative_ontology:measurement(tran_tr_t20, transition_causality__contingent_choice_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tran_tr_t20, observed).
narrative_ontology:measurement(tran_tr_t30, transition_causality__contingent_choice_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(tran_tr_t30, observed).
narrative_ontology:measurement(tran_tr_t40, transition_causality__contingent_choice_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(tran_tr_t40, observed).
narrative_ontology:measurement(tran_tr_t50, transition_causality__contingent_choice_reading, theater_ratio, 50, 0.46).
narrative_ontology:measurement_basis(tran_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(tran_be_t0, observed).
narrative_ontology:measurement(tran_be_t10, transition_causality__contingent_choice_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(tran_be_t10, observed).
narrative_ontology:measurement(tran_be_t20, transition_causality__contingent_choice_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(tran_be_t20, observed).
narrative_ontology:measurement(tran_be_t30, transition_causality__contingent_choice_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(tran_be_t30, observed).
narrative_ontology:measurement(tran_be_t40, transition_causality__contingent_choice_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(tran_be_t40, observed).
narrative_ontology:measurement(tran_be_t50, transition_causality__contingent_choice_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(tran_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(tran_su_t0, observed).
narrative_ontology:measurement(tran_su_t10, transition_causality__contingent_choice_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(tran_su_t10, observed).
narrative_ontology:measurement(tran_su_t20, transition_causality__contingent_choice_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(tran_su_t20, observed).
narrative_ontology:measurement(tran_su_t30, transition_causality__contingent_choice_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(tran_su_t30, observed).
narrative_ontology:measurement(tran_su_t40, transition_causality__contingent_choice_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(tran_su_t40, observed).
narrative_ontology:measurement(tran_su_t50, transition_causality__contingent_choice_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(tran_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the single historical event 'end of Bretton Woods gold convertibility' decomposes into three reading-stories of the transition_causality kernel, linked by affects_constraints. This member (contingent_choice_reading) assumes high counterfactual viability and locates causation in the decision itself; the overdetermined sibling assumes near-zero counterfactual viability; the hybrid sibling interpolates. Epsilon differs across members because each reading assesses the same referent arrangement by its own lights: this reading scores the fiat arrangement as maintained-by-choice and actively enforced (epsilon 0.72); the overdetermined sibling scores the same arrangement as structurally compelled, which lowers attributed choice and reshapes the beneficiary structure. The archival record feeds all three members; no member is evidentially upstream of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
