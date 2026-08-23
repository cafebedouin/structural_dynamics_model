% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Post-Bretton-Woods Fiat Monetary Order (Composite Overdetermination Reading)
 *   domain: monetary economics/political economy/history of economic thought
 *
 * SUMMARY:
 *   The standing arrangement under contest is the post-Bretton-Woods fiat
 *   monetary order, assessed by the composite_overdetermination_reading's
 *   lights. On this reading the order emerged from four independent
 *   structural changes converging over roughly two decades -
 *   telecommunications enabling instant cross-border capital flows, the
 *   collapse of the Bretton Woods pegs, the shift in organized labor's
 *   bargaining power, and the maturation of legal-tender enforcement - rather
 *   than from a single swap executed in August 1971. The Nixon Shock marks
 *   the sequence symbolically; the causal work was distributed. Consequences
 *   for structure: no single beneficiary exists (each strand redistributed
 *   differently), the burden is moderate and heterogeneous, and the kernel's
 *   premise of a unified transition object is itself challenged. The
 *   claim/metric split is deliberate: the reading is CLAIMED as tangled_rope
 *   (hybrid coordination and asymmetric burden under active enforcement),
 *   while the metrics describe what the order's operation actually looks like
 *   - moderate burden, substantial enforcement machinery, alternatives left
 *   partly open. The engine measures any divergence; nothing here reconciles
 *   claim to metrics.
 *
 * KEY AGENTS:
 *   - - sovereign_issuing_governments: agenda-setter and partial beneficiary (institutional/constrained) - enforces acceptance, collects only its own currency's seigniorage
 *   - - united_states_monetary_authorities: dominant partial beneficiary (institutional/arbitrage) - reserve-currency privilege, weakest external discipline
 *   - - globally_mobile_capital: primary beneficiary of the telecom strand (powerful/arbitrage) - escapes rather than collects
 *   - - commercial_banking_sector: dual-positioned beneficiary and cost-bearer (organized/constrained)
 *   - - cash_wage_earners: principal bearer of price-level erosion (powerless/trapped)
 *   - - fixed_income_savers: bearer with partial hedge access (moderate/constrained)
 *   - - organized_labor_unions: bearer of the bargaining-power shift (organized/constrained)
 *   - - debtor_developing_economies: dual-positioned bearer and incidental gainer (moderate/trapped)
 *   - - monetary_history_analysts: analytical observer attesting the causal genealogy from outside every benefiting seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.52).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Post-Bretton-Woods Fiat Monetary Order (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary economics/political economy/history of economic thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'bd5de4dd-8f95-4aeb-bf64-f40d01598d93').
narrative_ontology:cs_kernel_codification('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', distributed).
narrative_ontology:cs_authority_grounding('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', expertise).
narrative_ontology:cs_interpretation_layer_present('bd5de4dd-8f95-4aeb-bf64-f40d01598d93').
narrative_ontology:cs_reading_relation('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', foundational, transition_was_overdetermined_convergence).
narrative_ontology:cs_axiom_status(transition_was_overdetermined_convergence, holdable).
narrative_ontology:cs_axiom_grounding('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', transition_was_overdetermined_convergence, empirically_contingent).
narrative_ontology:cs_axiom('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', foundational, nixon_shock_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', nixon_shock_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_axiom('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', secondary, heterogeneous_strand_distribution_no_single_beneficiary).
narrative_ontology:cs_axiom_status(heterogeneous_strand_distribution_no_single_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', heterogeneous_strand_distribution_no_single_beneficiary, empirically_contingent).
narrative_ontology:cs_reference_frame('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', overdetermined_convergence_frame).
narrative_ontology:cs_drift_state('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', contemporary_revisionist_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bd5de4dd-8f95-4aeb-bf64-f40d01598d93', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, sovereign_issuing_governments).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, united_states_monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, globally_mobile_capital).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, cash_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor_unions).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_developing_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_developing_economies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, discretionary_monetary_policy_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, policy_trilemma_resolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the central bank, enacts legal-tender statutes, and accepts taxes only in its own currency. Receives seigniorage through central-bank remittances and gains crisis-financing capacity no metallic standard allowed. Maintaining acceptance is an ongoing administrative duty, and leaving its own currency means redenomination trauma, so each government stays and maintains - while capturing only the seigniorage of the currency it issues, never the ensemble's total gains.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, sovereign_issuing_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, sovereign_issuing_governments, beneficiary).

% Issues the dominant reserve currency. Foreign demand for dollars delivers seigniorage well beyond what a closed economy would yield, and policy decisions transmit worldwide while external discipline stays weaker than for smaller issuers. The privilege is large but partial - it followed from the peg collapse and telecom-driven dollar demand rather than from a designed plan, and it is one strand among several rather than the whole story.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, united_states_monetary_authorities, beneficiary,
    institutional, generational, arbitrage, global).

% Moves money across borders in seconds over telecom networks, rebalancing denominations and jurisdictions faster than any regulator can respond. Escapes national price erosion by repositioning rather than by collecting anything, and earns returns on the volatility that floating rates introduced. Pays little of what the system costs its slower participants.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, globally_mobile_capital, beneficiary,
    powerful, immediate, arbitrage, global).

% Creates credit on fractional reserves against central-bank money and earns intermediation spreads on the spread between deposit and lending rates. Depends on the lender-of-last-resort backstop the same institutions provide, and absorbs prudential capital requirements, supervision costs, and deposit-insurance premiums. Cannot operate outside the system whose benefits it collects.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector, payer).

% Receives wages and holds balances denominated exclusively in legal-tender fiat. Prices renegotiate upward faster than wages, and taxes are payable only in the state currency, so stepping outside means losing participation in the formal economy. Collective action through unions or indexed contracting is the main lever available, and it has thinned as bargaining conditions shifted.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, cash_wage_earners, payer,
    powerless, biographical, trapped, national).

% Holds deposits, bonds, annuities, and pension claims denominated in fiat, losing purchasing power whenever inflation outruns nominal yields. Partial hedges exist - equities, property, gold, foreign currency - but they demand knowledge, access, and taxable conversion friction that smaller savers disproportionately lack.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers, payer,
    moderate, biographical, constrained, national).

% Held wage-setting leverage under tighter postwar labor markets and watched it erode as floating rates, capital mobility, and disinflationary policy episodes raised the credible threat of relocation and unemployment. Membership institutions persist, but their position in wage negotiations worsened steadily across the interval.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor_unions, payer,
    organized, generational, constrained, regional).

% Gained freedom from metal-redemption discipline - deficits became financeable and exchange rates adjustable - but borrows in dollars it cannot print. Currency depreciation inflates local-currency debt burdens, rollovers arrive priced in hard currency, and adjustment programs impose austerity as the condition of continued access.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_developing_economies, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, debtor_developing_economies, beneficiary).

% Reconstructs the transition's causal structure from archives, capital-flow data, and wage-series records. Hosts the unity-of-causation dispute between single-node and convergence accounts of 1968-1990, publishes from outside every benefiting seat, and supplies the genealogical record on which the founding-problem interview relies.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an elastic, universally accepted medium of account and settlement: money creation decoupled from metal supply, cross-border clearing at telecom speed, and liquidity expandable in crises - services a commodity standard could not deliver at current transaction volumes.
% TRANSFER_FUNCTION: Moves purchasing power from holders of state-denominated balances to issuing governments (seigniorage) and to first receivers of new credit; moves wage-setting power from organized labor to employers through capital mobility and disinflationary slack; moves portfolio risk onto participants least equipped to reposition.
% ABSENT_VOICES: Cash-dependent households, small savers without hedging access, future taxpayers carrying monetized deficits, and hard-money dissenters - audible in elections but excluded from technical monetary design. Debtor publics meet adjustment conditions through creditor-board proxies rather than at the table where terms are set.
% DISAPPEARANCE_RATIONALE: Tax systems payable only in state fiat, trillion-scale derivative books priced off policy rates, trade invoicing conventions, and bank balance sheets all presuppose the arrangement. Overnight disappearance would freeze payments, void units of account mid-contract, and force emergency re-monetization - the world rearranges immediately, whichever causal story about 1971 turns out to be true.
% FOUNDING_PROBLEM: Each component answered a distinct pressure: the pegs strained against capital mobility that the telecom layer was enabling; governments wanted crisis-financing autonomy the gold window denied; employers sought relief from wage-push inflation; treasuries and courts needed enforceable non-metallic tender. On this reading there was no single founding problem to solve - the premise of one is part of what the reading disputes.
% FOUNDING_PROBLEM_CORROBORATION: Monetary-history scholarship sitting outside the beneficiary set - Eichengreen's Globalizing Capital, the Bordo-Eichengreen retrospective volumes, and contemporaneous Federal Reserve and Treasury memoranda in published annual reports - attests that peg stress, communications technology, and wage dynamics moved on partly independent schedules. No beneficiary-party attestation is relied upon. Whether those pressures constituted one founding problem remains the live dispute this reading embodies.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Burden is moderate (0.52 at interval end) because genuine coordination goods - elastic settlement, crisis liquidity, a universal unit of account - are delivered alongside real transfer streams (seigniorage, price-level erosion on trapped balances, risk dumped on slow repricers), none dominant. Suppression is high (0.72) and is a raw structural property, unscaled by power or scope: acceptance of fiat is compelled by statute (legal tender), by the tax system's denomination requirement, and increasingly by surveillance of alternative media (KYC/AML/FATCA), not by participant preference. Theater ratio 0.38: the functional core (payments, lender-of-last-resort) is real, while a growing share of activity is communicative ritual (forward-guidance ceremonies, inflation-targeting theatre) rather than operational. Accessibility_collapse 0.42: alternatives (foreign currency, gold, later crypto) persist but remain second-class because taxes clear only in state fiat. Resistance 0.55: recurring hard-money politics, gold-bug movements, and inflation-backlash elections. The measurement series share one grid (t=0..50 indexing 1970..2020 in decade steps) with every tracked metric authored at every point. The burden series is non-monotonic - peaking during the 1970s-80s price surges, dipping under disinflation, migrating to asset-channel transfers thereafter - reflecting a political cycle (tolerance, crackdown, quiet accumulation) driven by electoral rhythms rather than intermittent reinforcement. Gain_flow='diffuse' is an affirmative, checked claim: seigniorage splits across roughly 160 issuing sovereigns (the United States capturing the largest single share yet still a partial one), banking spreads are offset by backstop dependence and prudential costs, and mobile capital avoids the system's costs rather than receiving its gains - so no named seat captures the ensemble's gains. Fixing is prohibitive: re-anchoring would require treaty-level coordination and a deflationary transition no major economy will attempt.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data encodes why. The sovereign seat experiences governance capacity and an administrative duty it chose; the United States seat experiences privilege with attenuated discipline; mobile capital experiences near-costless optionality; the banking seat experiences profitable dependence it cannot leave; wage-earner and saver seats experience erosion with no usable exit; the debtor-state seat experiences flexibility priced in a currency it does not issue. Same-level divergence is sharpest between globally_mobile_capital and cash_wage_earners - nominally both 'users of money' - where the differentiating factors are denomination access and repricing speed, both specific to this arrangement rather than to general wealth. Inter-institutionally, sovereign_issuing_governments and united_states_monetary_authorities hold adjacent institutional rank yet face different exit surfaces (redenomination trauma versus exporting adjustment), so equal-seeming power produces unequal directional pull.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation and no overrides are needed because the exit atoms already encode the asymmetries. Sovereigns sit low on the target axis (subsidy side) with their enforcement duty tempering the subsidy; United States authorities sit lowest among sovereigns via arbitrage-grade exit; mobile capital sits nearest the beneficiary extreme because arbitrage exit converts would-be costs into avoided costs; banks sit moderately low, their dual position expressed through the secondary role rather than an override. Cash wage-earners (trapped) and fixed-income savers (constrained) sit near the full-target end; organized labor sits high despite organized power because exit is structural - jobs and contracts are denominated locally regardless of union strength; debtor economies sit elevated but tempered by their secondary flexibility gain. Coalition power for the powerless seats is noted: wage-earner coalitions were historically the countervailing force, which is precisely why the labor strand matters to this reading's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading sharpens rather than resolves mandatrophy: the operative question is not whether a mandate outlived its function but whether there was ever a single mandate. Reading the order as snare would ignore the settlement-and-liquidity core that no participant, including its bearers, would surrender; reading it as rope would ignore statutorily compelled acceptance and the incidence of price-level erosion on those least able to reprice. The hybrid classification preserves both halves. On the R5 consumer path: founding_problem_status is 'contested' (parties dispute whether one founding problem existed), paired with disappearance_verdict 'world_rearranges', so the dead-mandate-plus-dependence mismatch flag does not fire - the arrangement's functions are demonstrably live even while its origin story is disputed. No mandatrophy_resolved declaration is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_contest,
    'Does a unified ''gold-fiat transition'' exist as a single structural object, or is the label a narrative bundle over heterogeneous changes with distinct timings, actors, and constituencies?',
    'Comparative component chronology: test whether peg stress (1968-1973), telecom capital-flow growth (telex to SWIFT, 1960s-1977), wage-share inflection (late 1970s), and legal-tender enforcement milestones share timing, actors, and feedback sufficient to constitute one process.',
    'If no unified transition exists, the automatic_constraint_reading and the creditor_discipline_reading misattribute causality to a non-existent swap; classification attaches to an ensemble whose burden is the weighted sum of strands, not to a node event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_contest, conceptual, 'This story is the composite_overdetermination_reading of kernel gold_fiat_transition_mechanism; this omega routes the reading''s core contest with its siblings - whether the kernel denotes one transition at all.').

omega_variable(
    component_causal_weights,
    'How much of the observed regime change does each strand carry - could the pegs have held without telecom-speed capital flows, or would telecom connectivity have mattered without peg stress?',
    'Peg-survival hazard models against capital-flow elasticity; difference-in-differences on economies exposed to SWIFT connectivity before and after 1973; wage-share decomposition against monetary-policy shock series.',
    'Redistributes burden attribution across strands; if the telecommunications layer dominates, the arrangement''s profile reads closer to a technology-driven market outcome than a policy construction, lowering constructed-regime assessments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_causal_weights, empirical, 'Relative causal contribution of the four convergent strands.').

omega_variable(
    nixon_shock_causal_status,
    'Was the August 1971 suspension purely a symbolic marker laid over already-moving components, or did closing the gold window exert independent causal force?',
    'Event-study controls: capital-flow, price, and wage series detrended for pre-1971 trajectories, tested for a structural break attributable to the announcement itself.',
    'Marker-only confirmation shifts the arrangement''s effective birthdate earlier and dates any type transitions accordingly; an independent-force finding restores a partial causal node and softens the overdetermination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_causal_status, empirical, 'Symbolic-marker versus causal-node status of the 1971 suspension.').

omega_variable(
    epsilon_aggregation_weighting,
    'Is a single scalar burden measure meaningful for an ensemble whose strands benefit and harm different seats - whose losses count in the aggregate?',
    'Seat-level per-strand burden estimates aggregated under explicit declared weighting schemes, with a sensitivity report across egalitarian, incidence-based, and welfare-based weights.',
    'The aggregate ranges materially with weights: heavier saver-and-labor incidence pushes toward snare-side computations, heavier crediting of liquidity and settlement benefits pushes toward rope-side, changing computed type at the margins without altering the underlying heterogeneity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_aggregation_weighting, preference, 'Normative weighting ambiguity in aggregating heterogeneous strand effects.').

omega_variable(
    labor_shift_endogeneity_direction,
    'Was organized labor''s bargaining-power loss a driver of the monetary regime or a consequence of it (disinflationary unemployment crushing union density)?',
    'Sequencing tests on strike volume, wage share, and policy-rate paths; cross-country comparison of union-density decline against early capital-account liberalization.',
    'Driver status adds labor to the composite''s causal roster and raises the arrangement''s constructedness; consequence status removes a strand and shrinks the claimed convergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_shift_endogeneity_direction, empirical, 'Causal direction of the labor-bargaining strand within the composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_composite_tr_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(gftm_composite_tr_t0, observed).
narrative_ontology:measurement(gftm_composite_tr_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(gftm_composite_tr_t10, observed).
narrative_ontology:measurement(gftm_composite_tr_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(gftm_composite_tr_t20, observed).
narrative_ontology:measurement(gftm_composite_tr_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(gftm_composite_tr_t30, observed).
narrative_ontology:measurement(gftm_composite_tr_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(gftm_composite_tr_t40, observed).
narrative_ontology:measurement(gftm_composite_tr_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(gftm_composite_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gftm_composite_be_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(gftm_composite_be_t0, observed).
narrative_ontology:measurement(gftm_composite_be_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(gftm_composite_be_t10, observed).
narrative_ontology:measurement(gftm_composite_be_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(gftm_composite_be_t20, observed).
narrative_ontology:measurement(gftm_composite_be_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(gftm_composite_be_t30, observed).
narrative_ontology:measurement(gftm_composite_be_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(gftm_composite_be_t40, observed).
narrative_ontology:measurement(gftm_composite_be_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(gftm_composite_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gftm_composite_su_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(gftm_composite_su_t0, observed).
narrative_ontology:measurement(gftm_composite_su_t10, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(gftm_composite_su_t10, observed).
narrative_ontology:measurement(gftm_composite_su_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(gftm_composite_su_t20, observed).
narrative_ontology:measurement(gftm_composite_su_t30, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(gftm_composite_su_t30, observed).
narrative_ontology:measurement(gftm_composite_su_t40, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(gftm_composite_su_t40, observed).
narrative_ontology:measurement(gftm_composite_su_t50, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(gftm_composite_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% Kernel gold_fiat_transition_mechanism decomposes into three readings per the epsilon-invariance principle: automatic_constraint_reading (material limit swapped for institutional discretion), creditor_discipline_reading (redemption-threat veto removed), and this composite_overdetermination_reading (convergent independent strands, no unified swap). All three assess the SAME standing referent - the post-transition fiat monetary order - under their own lights, yielding different epsilon and different victim sets. This file links both siblings via affects_constraints; upstream/downstream pressure runs from this reading toward the others because its component chronology constrains which causal attributions their accounts can sustain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
