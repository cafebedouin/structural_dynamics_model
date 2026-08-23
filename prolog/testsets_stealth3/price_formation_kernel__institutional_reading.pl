% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__institutional_reading, []).

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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutional Construction of Housing Prices (Zoning-Lending-Tax-Platform Regime)
 *   domain: economic/political
 *
 * SUMMARY:
 *   In high-demand metropolitan housing markets, the price of shelter is set
 *   less by open exchange than by an interlocking institutional arrangement:
 *   zoning caps what can be built and where, lending standards gate who can
 *   borrow and at what leverage, tax treatment subsidizes holding and
 *   penalizes mobility, and intermediary platforms tax every transaction
 *   through customary commissions. Each pillar solves a real coordination
 *   problem, and the same rules distribute the gains of scarcity to incumbent
 *   owners, lenders, and brokers while renters, first-time buyers, and
 *   would-be residents pay. The arrangement is administered across four
 *   institutional seats - planning, financial regulation, tax legislation,
 *   platform governance - each bound to different principals and each
 *   structurally coupled to the beneficiary class. Assumptions stated: the
 *   referent is the standing US-style metropolitan arrangement over
 *   1970-2025; authored scalars describe the regime where it binds hardest
 *   (supply-constrained high-demand metros) and vary by jurisdiction. Claim
 *   and metrics are independent: claimed_type is tangled_rope because both a
 *   coordination function and an asymmetric-incidence transfer are
 *   structurally present and enforcement exists to hold both; the metric
 *   values describe observed operation.
 *
 * KEY AGENTS:
 *   - incumbent_homeowners: primary beneficiary seat (organized/mobile) - collects scarcity rents, appreciation, and tax preferences; simultaneously the dominant agenda-setting constituency in local land-use politics
 *   - mortgage_lenders: beneficiary seat (institutional/arbitrage) - revenue base scales with the price level the standards they help write support
 *   - real_estate_intermediaries: beneficiary seat (organized/mobile) - collects commissions on every transaction and writes the platform rules governing market access
 *   - renters: primary target seat (powerless/constrained) - pays scarcity through rents with thin collective voice and high churn
 *   - first_time_buyers: target seat (moderate/constrained) - pays the entry price; held in the arrangement by promised future beneficiary status
 *   - prospective_in_migrants: excluded seat (powerless/trapped) - bears the misallocation as foregone mobility and income with no standing anywhere
 *   - local_planning_authorities: agenda-setter seat (institutional/constrained) - administers zoning; electorally and fiscally bound to the beneficiary class
 *   - financial_regulators: agenda-setter seat (institutional/constrained) - set the underwriting and guarantee parameters that gate access
 *   - tax_legislatures: agenda-setter seat (institutional/constrained) - write the tax treatment; donor-coupled to real estate interests
 *   - housing_economists: analytical observer (analytical/analytical) - measures supply elasticities, incidence, and commission structure from outside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.64).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Prices (Zoning-Lending-Tax-Platform Regime)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "economic/political").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '20d36dcf-ec68-43a3-aefe-079b0b3b8c5e').
narrative_ontology:cs_kernel_codification('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', distributed).
narrative_ontology:cs_authority_grounding('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', expertise).
narrative_ontology:cs_interpretation_layer_present('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e').
narrative_ontology:cs_reading_relation('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', foundational, housing_prices_are_policy_outcomes).
narrative_ontology:cs_axiom_status(housing_prices_are_policy_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', housing_prices_are_policy_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', secondary, regulatory_scarcity_rent_is_reclaimable).
narrative_ontology:cs_axiom_status(regulatory_scarcity_rent_is_reclaimable, holdable).
narrative_ontology:cs_axiom_grounding('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', regulatory_scarcity_rent_is_reclaimable, instrumental).
narrative_ontology:cs_reference_frame('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', institutionally_constructed_price_regime).
narrative_ontology:cs_drift_state('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', contemporary_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('20d36dcf-ec68-43a3-aefe-079b0b3b8c5e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_vindicates(price_formation_kernel__institutional_reading, institutional_price_formation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own homes in supply-constrained metropolitan markets. Their balance sheets appreciate as supply tightens; they receive mortgage-interest and property-tax preferences and capital-gains exclusions, and in many jurisdictions pay property taxes capped below service costs. They dominate attendance at planning hearings and vote at high rates in local elections, so their support is what planning authorities must retain. Exit: most could sell at appreciated prices and relocate to cheaper markets; a minority do, while the majority stay because the asset, the community ties, and the preference stream are all location-specific.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, incumbent_homeowners, agenda_setter).

% Originate and securitize residential mortgages. The underwriting standards they help write determine who can borrow, at what down payment, and at what rate; larger loan balances mean larger interest and fee income, so rules that support higher prices expand their revenue base. Capital is mobile: loan books are sold into securitization markets and portfolios reweight across regions and asset classes. They bear compliance costs and tail risk when standards fail, but their revenue scales with the price level the rules support.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% Brokerages and agent networks that operate the listing platforms (MLS systems) and maintain customary commission rates, historically around five to six percent split between buyer-side and seller-side agents, through membership and cooperation rules. Every sale at a higher price yields a larger commission, and every transaction run through the platform yields one at all. They lobby legislatures directly and write the platform rules that govern who may list and at what fee. Exit: the trade is portable across markets and fee models can restructure under legal pressure, though the customary rate has held for decades.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediaries, agenda_setter).

% Rent homes in the same metropolitan markets where ownership prices are set by the rules. Rents track the scarcity the rules produce; renters pay a rising share of income for housing and are excluded from the ownership preferences that subsidize their landlords' assets. High churn and hearing schedules built around owner availability thin their collective voice in land-use politics. Exit: they can move between rentals or to cheaper metros, at the cost of jobs, networks, and deposit savings.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, regional).

% Households with income and credit trying to buy a first home. They face prices set by restricted supply, underwriting standards that gate their access, transaction costs near eight to ten percent all-in, and competition against owners holding tax-advantaged equity. The arrangement holds out future membership: once bought in, today's buyer joins tomorrow's owner class with the same preferences and appreciation, which softens their opposition. Exit: keep renting and keep paying the scarcity, buy in at the set price, or leave the metro.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).

% Households who would move to high-opportunity metros for work but are priced out of both ownership and adequate rental there. They have no standing in the planning hearings of places they do not yet live and no vote in them, so the rules that exclude them are set entirely by current residents. They bear the misallocation as foregone earnings and foregone mobility. Exit: stay where they are, at a cost measured in lifetime income.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, prospective_in_migrants, excluded,
    powerless, biographical, trapped, national).

% Municipal planning departments, commissions, and councils that write, administer, and enforce land-use codes: they hold hearings, grant discretionary approvals, and police compliance. Their budgets depend on the property tax base and their electoral survival depends on incumbent homeowners, who dominate hearings and turnout. They can change the rules, and some have under state pressure, but each change runs against their own constituents' preferences and their own fiscal base.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_planning_authorities, agenda_setter,
    institutional, generational, constrained, local).

% Set underwriting standards, capital rules, and guarantee parameters (conforming loan limits, secondary-market eligibility) that determine mortgage credit availability nationwide. They tighten after crises and loosen in expansions; the parameters they set gate who can buy and at what leverage. They answer to statutory mandates and political principals, not to borrowers or renters directly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, financial_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Write and maintain the tax treatment of housing: mortgage interest deductions, property tax caps and exemptions, capital-gains exclusions, and stepped-up basis. Real estate interests rank among the largest campaign-contribution sources they face, and the preferences are popular with the homeowners who vote. They could reprice the arrangement through tax reform, but each change creates visible losers among their constituents and visible gains among people who cannot vote in their districts.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, tax_legislatures, agenda_setter,
    institutional, biographical, constrained, national).

% Researchers who measure supply elasticities, the incidence of tax preferences, commission structures, and the price effects of land-use rules across jurisdictions and reform episodes. They publish, testify, and design the natural experiments that the other seats argue over. They collect no rents from the arrangement and bear none of its costs directly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_economists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Zoning coordinates land-use externalities and infrastructure capacity across neighboring parcels; lending standards coordinate credit risk across the mortgage system and prevent default cascades; tax treatment channels household saving toward owner-occupied housing and stabilizes municipal revenue; intermediary platforms coordinate matching between scattered buyers and sellers and standardize transactions. Each pillar solves a real coordination problem, and the same rules that solve it also restrict what can be built, gate who can borrow, subsidize holding over building, and tax every move.
% TRANSFER_FUNCTION: Moves income and wealth from renters and first-time buyers - through rents set above replacement cost, purchase prices inflated by restricted supply and capitalized subsidies, customary transaction commissions, and general-revenue tax expenditures - to incumbent owners (appreciation and scarcity rents), lenders (interest spreads and origination fees on larger principal), and intermediaries (commissions on every sale). It also moves decision power over land use to current residents and away from future ones.
% ABSENT_VOICES: Prospective residents who do not yet live in a jurisdiction have no standing in its zoning hearings and no vote in its elections; future buyers and renters bear prices set in the present while holding no voice in the present; renters attend hearings at rates far below homeowners because hearing schedules and notice practices are built around owner availability. The excluded seat is carried by prospective_in_migrants in the stakeholder set.
% DISAPPEARANCE_RATIONALE: If zoning restrictiveness, lending gatekeeping, ownership tax preferences, and platform commission rules all vanished overnight, supply in high-demand metros would respond to demand, prices would converge toward replacement cost plus residual land value, ownership would broaden, intermediary commissions would compress under competition, and the local political economy built on homeowner majorities would reorganize around much broader electorates. The genuine coordination functions (building safety, credit underwriting, matching) would be rebuilt in leaner forms; the asymmetric incidence would not spontaneously regenerate.
% FOUNDING_PROBLEM: Zoning was built to manage nuisance externalities and infrastructure capacity - and, explicitly in its early decades, to exclude by class and race; lending standards were built to prevent the default cascades that followed the Depression; ownership tax treatment was built to mass-produce a property-owning middle class after World War II; the MLS was built to let cooperating brokers share listing information.
% FOUNDING_PROBLEM_CORROBORATION: Urban economics research (supply-elasticity and misallocation studies) attests from outside the beneficiary set that the externality-management rationale no longer explains observed restrictiveness in the highest-demand metros, where restrictiveness tracks price effects rather than externality intensity; fair-housing litigation history corroborates the exclusionary strand of the founding; municipal governments and the planning professional literature attest that externality and infrastructure management remain a live function. No single seat's self-attestation settles the status, which is why it is authored contested rather than live or dead.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 because the transfer is large relative to payer income: scarcity rents and capitalized subsidies in constrained metros absorb a substantial share of renter and first-time-buyer lifetime housing budgets, and the commission layer taxes mobility on top. Suppression is 0.64 because the arrangement actively forecloses alternatives - missing-middle housing types are illegal to build across most residential land, accessory units were until recently banned, entry is gated by underwriting, and the platform layer keeps transactions intermediated; exit for payers means leaving opportunity metros, which is exit from jobs and networks rather than from the rules' reach. Suppression is authored as a raw structural property, unscaled; only extractiveness gets scaled downstream by directionality and scope. Theater is 0.42: hearings, comprehensive plans, and affordable-housing set-asides perform responsiveness while unit output falls short of stated goals in the most restrictive jurisdictions, but the coordination core (building safety, underwriting, matching) is real, so the ratio sits below the piton-signaling range. Accessibility collapse is 0.50: once the construction is visible, individual alternatives are thin - no household can zone its own apartment - but jurisdictional arbitrage persists (households relocate to permissive metros; states preempt local rules), so alternatives do not collapse completely. Resistance is 0.55 and rising: pro-supply organizing, state preemption, fair-housing litigation, and commission antitrust actions each attack a specific pillar. The three measurement series run on one shared seven-point grid (1970-2025) so no metric is sampled against another's end-state; the rising suppression_requirement series is authored because the enforcement picture genuinely changed over the interval - review regimes professionalized, discretionary permitting expanded, and compliance infrastructure hardened - rather than remaining static.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute different types from the same structure: from an incumbent owner's position the arrangement is prudent stewardship - externality management, credit discipline, neighborhood stability - and its returns are earned risk-taking; from a renter's or first-time buyer's position the same rules operate as a price-setting coalition with a civic vocabulary. The agenda-setter seats compute a third version: a mandate-bound administrative process whose outputs are legitimate because procedurally produced. Coalition potential is the live threat from below: renters and first-time buyers are natural allies, and the arrangement's co-optation of the buyer seat (promised future membership in the owner class) functions partly as a coalition-blocking mechanism - the engine should see the payer seats' computed types diverge from the beneficiary seats' precisely because their exit options and structural coupling differ, not because any seat is authored a verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: incumbent homeowners collect scarcity rents and tax preferences, with mobile exit that lets a selling household capture the gain outright (d near the beneficiary end); lenders' revenue scales with loan balances and their capital is securitized and mobile (d low); intermediaries collect on every transaction regardless of direction, benefiting from churn itself (d lowest of the three). Victim declarations: renters bear scarcity with no ownership offset (d near the target end, slightly dampened by mobility between jurisdictions); first-time buyers bear the entry price at high d, dampened only by the co-optation structure - the arrangement sells them future beneficiary membership, which suppresses resistance without lowering their current cost; prospective in-migrants bear pure misallocation and receive nothing from the arrangement (d highest). The four agenda-setter seats administer without directly collecting: planning authorities are coupled to beneficiaries through the property tax base and the electorate, tax legislatures through campaign finance, regulators through mandate politics. No directionality overrides are authored: the three institutional agenda-setter seats share the institutional power atom, so per-atom overrides could not separate them, and their differentiation is carried by the structural coupling recorded here instead of by numeric correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure extraction would erase the genuine coordination - building codes do manage externalities, underwriting does prevent default cascades, the listing platforms do solve a matching problem - and would mispredict the politics, since beneficiaries defend the arrangement with sincere coordination arguments. Reading it as pure coordination would erase the incidence: the coordination is real but its costs and benefits are sharply asymmetric, and the enforcement machinery exists to hold the asymmetry, not only the coordination. Tangled rope holds both halves and locates the live question in the separability omega: if the coordination functions can be delivered without the exclusionary incidence, the arrangement's extractive layer is a removable rider on a real function. On the genealogy: the founding problem is contested rather than dead - externality and infrastructure management remain live functions even where exclusion is now dominant - so this is not a resolved mandatrophy case, and the arrangement is not a piton maintained by inertia, because identifiable seats still actively collect and actively enforce.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the institutional_reading of price_formation_kernel; which structural element do the sibling readings relocate, and how would classification move under each?',
    'Author the sibling readings as separate constraint stories over the same referent and compare per-seat classifications. The disagreement is located in the primary driver of price levels: constructed rules (this reading), natural scarcity and preference (naturalist_reading), unearned location rent (georgist_reading), or credit dynamics and asset-price feedback (financialization_reading).',
    'Under the naturalist reading the same arrangement would author epsilon near zero (nothing is extracted by a natural process); under the georgist reading the victim set narrows to holders and seekers of prime-location land; under the financialization reading the victim set extends to all credit-financed buyers and epsilon tracks credit conditions. Per-seat types and the family''s contamination edges all move with the choice; this file''s metrics are valid only for the institutional reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints, not hedges.').

omega_variable(
    pillar_contribution_decomposition,
    'How much of the observed price premium does each pillar contribute - zoning restrictiveness, lending gatekeeping, tax treatment, platform commissions?',
    'Cross-pillar natural experiments: upzoning and preemption episodes, underwriting standard changes, tax reform episodes, and commission antitrust settlements, with price effects decomposed by pillar.',
    'Reform leverage and per-seat attribution move with the decomposition: if zoning dominates, the planning seat bears the fix; if platforms dominate, the intermediary seat does; if tax treatment dominates, the legislature seat does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pillar_contribution_decomposition, empirical, 'Relative contribution of the four constructing institutions to the measured price premium.').

omega_variable(
    subsidy_capitalization_incidence,
    'Are ownership tax preferences capitalized into prices such that current homeowners partly pay for them through higher purchase prices, shifting true incidence toward earlier owners and lenders?',
    'Incidence studies of tax preference changes (deduction caps, assessment-limit episodes) tracing price and wealth effects across cohorts of owners.',
    'If heavily capitalized, the homeowner seat''s effective position rises above the derived beneficiary level and the lender and earlier-seller seats absorb more of the gain - changing per-seat classification without changing the aggregate transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_capitalization_incidence, empirical, 'Whether tax-preference incidence lands on current owners or is capitalized backward.').

omega_variable(
    coordination_extraction_separability,
    'Can the arrangement''s genuine coordination functions - externality management, credit safety, matching - be delivered without the exclusionary incidence, or are they structurally entangled with the rules that produce scarcity?',
    'Jurisdictions that liberalized supply while retaining safety codes and underwriting (Minneapolis-style reforms, permissive-market comparators): do coordination outcomes hold while prices converge toward cost?',
    'If separable, the arrangement''s extractive layer is a removable rider and reform is a transition that keeps the rope component; if entangled, part of the measured extractiveness is the price of the coordination itself and the coordination component is larger than authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and exclusionary components of the arrangement are structurally separable.').

omega_variable(
    platform_commission_necessity,
    'Is the customary commission rate a competitive price for matching and transaction services, or an MLS-rule-maintained markup above the competitive level?',
    'Post-decoupling commission data following antitrust settlements; flat-fee, FSBO, and alternative-platform market share trends over time.',
    'If markup, the platform pillar''s take is pure rent and the intermediary seat sits even nearer the beneficiary end than derived; if competitive, the platform pillar is closer to coordination cost and the aggregate extractiveness attributed to it falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_commission_necessity, empirical, 'Whether intermediary commissions price a service or defend a platform rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement_basis(pric_tr_t1970, observed).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement_basis(pric_tr_t1980, observed).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.34).
narrative_ontology:measurement_basis(pric_tr_t1990, observed).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__institutional_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(pric_tr_t2000, observed).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement_basis(pric_tr_t2010, observed).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__institutional_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(pric_tr_t2020, observed).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__institutional_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(pric_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(pric_be_t1970, observed).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement_basis(pric_be_t1980, observed).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement_basis(pric_be_t1990, observed).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__institutional_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(pric_be_t2000, observed).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(pric_be_t2010, observed).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__institutional_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(pric_be_t2020, observed).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__institutional_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(pric_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement_basis(pric_su_t1970, observed).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(pric_su_t1980, observed).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement_basis(pric_su_t1990, observed).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__institutional_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement_basis(pric_su_t2000, observed).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(pric_su_t2010, observed).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__institutional_reading, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement_basis(pric_su_t2020, observed).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__institutional_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(pric_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% 'Housing price formation' is a natural-language label covering structurally distinct claims; per the epsilon-invariance principle it is decomposed into a four-reading family over price_formation_kernel. This file is the institutional_reading; the naturalist, georgist, and financialization readings are separate stories with their own epsilon, stakeholder sets, and classifications. The institutional reading sits upstream of the georgist reading (its evidence that rules construct scarcity changes the georgist claim's operating environment and the administrability of land-value capture) and coexists with the naturalist and financialization readings as live competing frames held by different parties. Family members link through affects_constraints; no member's epsilon is hedged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
