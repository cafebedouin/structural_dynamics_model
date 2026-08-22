% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__georgist_reading, []).

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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Land Rent Capture Within Price Formation (Georgist Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   Georgist economics holds that the price of real property bundles two
 *   structurally distinct things: the value of the underlying site (which
 *   reflects population density, public infrastructure, and agglomeration
 *   effects the owner did nothing to create) and the value of improvements
 *   built on it (which reflects genuine labor and capital investment). This
 *   reading claims the land-rent component functions as extraction:
 *   landowners, speculators, and lenders secured against land value capture
 *   socially-produced value, while tenants, wage laborers, and new entrants
 *   pay for it without recourse, because current tax and zoning institutions
 *   tax and price land and improvements together rather than separating them.
 *   The land itself is fixed in supply (mountain-like: no one can manufacture
 *   more central urban land), which makes the rent-capture atop it
 *   structurally different from ordinary market profit — it is capture of a
 *   scarcity rent, not compensation for production.
 *
 * KEY AGENTS:
 *   - landowners: primary beneficiary (powerful/arbitrage) — collects unearned location value
 *   - land_speculators: organized beneficiary and agenda-setter (organized/arbitrage) — actively shapes tax/zoning rules preserving capture
 *   - tenants: primary target (powerless/trapped) — pays location rent embedded in housing cost
 *   - wage_laborers: primary target (powerless/trapped) — sees productivity gains absorbed by rising land-linked rent
 *   - productive_improvers: secondary payer (moderate/constrained) — investment penalized by co-mingled land/improvement taxation
 *   - municipal_tax_assessors: institutional agenda-setter (institutional/constrained) — has technical capacity to split assessments but faces political constraint
 *   - land_value_tax_advocates: excluded analytical voice — proposes the fix but lacks institutional standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.71).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.62).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, snare).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Land Rent Capture Within Price Formation (Georgist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '630b03a0-2f31-4638-95b6-ea1fa53a8d29').
narrative_ontology:cs_kernel_codification('630b03a0-2f31-4638-95b6-ea1fa53a8d29', distributed).
narrative_ontology:cs_authority_grounding('630b03a0-2f31-4638-95b6-ea1fa53a8d29', distributed).
narrative_ontology:cs_reading_relation('630b03a0-2f31-4638-95b6-ea1fa53a8d29', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('630b03a0-2f31-4638-95b6-ea1fa53a8d29', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('630b03a0-2f31-4638-95b6-ea1fa53a8d29', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('630b03a0-2f31-4638-95b6-ea1fa53a8d29', foundational, location_value_is_socially_created_not_earned).
narrative_ontology:cs_axiom_status(location_value_is_socially_created_not_earned, holdable).
narrative_ontology:cs_axiom_grounding('630b03a0-2f31-4638-95b6-ea1fa53a8d29', location_value_is_socially_created_not_earned, empirically_contingent).
narrative_ontology:cs_axiom('630b03a0-2f31-4638-95b6-ea1fa53a8d29', foundational, land_and_improvement_value_are_morally_distinguishable).
narrative_ontology:cs_axiom_status(land_and_improvement_value_are_morally_distinguishable, holdable).
narrative_ontology:cs_axiom_grounding('630b03a0-2f31-4638-95b6-ea1fa53a8d29', land_and_improvement_value_are_morally_distinguishable, deontological).
narrative_ontology:cs_reference_frame('630b03a0-2f31-4638-95b6-ea1fa53a8d29', classical_liberal_land_rent_critique).
narrative_ontology:cs_drift_state('630b03a0-2f31-4638-95b6-ea1fa53a8d29', contemporary_housing_affordability_crisis, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('630b03a0-2f31-4638-95b6-ea1fa53a8d29', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, land_speculators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, mortgage_lenders_secured_on_land_value).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, wage_laborers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, new_market_entrants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_improvers).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, location_value_is_socially_created).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, labor_theory_of_earned_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold title to sites whose value rises with surrounding population density, public infrastructure, and agglomeration effects they did nothing to create. Collect rent or capital gains attributable almost entirely to location rather than to any improvement they built. Can sell, lease, or hold indefinitely as land banks; bear essentially no cost for the site's rising value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, beneficiary,
    powerful, generational, arbitrage, regional).

% Acquire and hold undeveloped or underdeveloped parcels anticipating value increases from public investment (transit, schools, rezoning) rather than from any productive activity. Lobby against land value taxation and for the current property tax structure that under-taxes bare land relative to improvements, actively shaping the price-formation rules that let them capture unearned appreciation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_speculators, beneficiary,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, land_speculators, agenda_setter).

% Extend credit collateralized substantially by land value appreciation, capturing interest income tied to rising site values. Have structural interest in land price appreciation continuing and in tax/zoning regimes that preserve it, since collateral value underwrites loan books.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mortgage_lenders_secured_on_land_value, beneficiary,
    institutional, generational, arbitrage, national).

% Builders, developers, and owner-occupiers who invest labor and capital into structures and improvements. Under current tax and price-formation rules, improvements are taxed and priced alongside land, so the return to genuinely productive investment is entangled with and often crowded out by land rent capture — improving a property can trigger tax reassessment that partially penalizes the improvement itself.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_improvers, payer,
    moderate, biographical, constrained, local).

% Pay rent that embeds the site's location value — proximity to jobs, transit, schools, amenities they did not create and cannot capture. Rent rises with neighborhood value even absent any change in the dwelling itself. Exit means relocating away from the economic opportunity the location provides, which is often not a real option.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, biographical, trapped, local).

% Their wages are effectively taxed by rising housing costs driven by land rent capture near employment centers — the classic Georgist claim that rising rent absorbs the gains of productivity growth before labor sees them. Cannot bid down land prices individually; must either pay the rent premium or accept a longer commute that itself has a cost.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, wage_laborers, payer,
    powerless, biographical, trapped, regional).

% First-time buyers and new firms seeking to locate in a market must pay the full capitalized value of location rent as an upfront price, a barrier that did not exist for earlier entrants who bought before appreciation. Their exclusion from ownership is the flip side of existing owners' capital gains.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, new_market_entrants, payer,
    powerless, biographical, constrained, local).

% Argue price formation should be restructured so land rent is captured publicly (via a land value tax) rather than privately, while improvements remain untaxed to encourage productive investment. Rarely have a seat in zoning boards, assessor offices, or housing policy bodies dominated by incumbent-owner interests.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_value_tax_advocates, excluded,
    moderate, generational, analytical, national).

% Administer property tax systems that in most jurisdictions tax land and improvements at the same rate, or under-assess land relative to improvements. Have the technical capacity to split assessments and shift the tax burden toward land value, but face political pressure from landowners and limited institutional mandate to do so.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, municipal_tax_assessors, agenda_setter,
    institutional, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Price formation genuinely coordinates the allocation of scarce, non-reproducible locations among competing uses, and (via the improvement component) coordinates investment of labor and capital into structures that expand the usable housing and commercial stock.
% TRANSFER_FUNCTION: The land-rent component of the price moves value from tenants, wage laborers, and new entrants to existing landowners, speculators, and their lenders — value created by population growth, public investment, and agglomeration, not by any landowner action.
% ABSENT_VOICES: Land value tax advocates and unborn/future residents who will face capitalized location rents are structurally absent from the assessment boards, zoning commissions, and tax-policy bodies that set the rules determining how much of the price reflects rent versus improvement.
% DISAPPEARANCE_RATIONALE: If land rent capture were eliminated overnight (e.g., via full land value taxation with revenue neutrality), landowners would lose the unearned appreciation component of wealth, housing prices would fall toward improvement-cost levels, new entrants would face dramatically lower barriers, and productive investment in improvements would no longer be penalized by co-mingled taxation — a substantial reorganization of wealth and incentives.
% FOUNDING_PROBLEM: Land is fixed in supply and location value arises from social and public investment (roads, schools, density, agglomeration) rather than from the landowner's labor; the founding problem this reading identifies is that price formation as currently structured lets private titleholders capture value that is socially produced, without requiring them to produce anything in return.
% FOUNDING_PROBLEM_CORROBORATION: Georgist economists and land value tax researchers (outside the landowner beneficiary class) attest the rent-capture dynamic remains active and measurable via land price indices net of construction cost indices; some municipal assessors and public finance economists studying split-rate taxation independently corroborate that land value appreciation is decoupled from landowner productive contribution. Landowner associations and real estate lobbies dispute the framing but do not dispute the underlying price decomposition data.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__georgist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__georgist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71, reflecting the reading's claim that a substantial share of observed land price appreciation reflects capture of socially-produced value rather than any landowner contribution — this is high but not maximal because some site value does reflect genuine owner risk-taking (e.g., early speculative investment in unproven areas) that the pure Georgist model treats as a smaller residual. Suppression is moderate-high (0.62) because the capture depends on active political maintenance: property tax structures that under-tax land relative to improvements, zoning restrictions that constrain supply and inflate site scarcity rents, and organized resistance to land value tax proposals. Theater ratio (0.40) reflects that some assessment and zoning activity is performative — nominally about equitable taxation or planning, functionally about preserving existing landowner capture. Accessibility collapse (0.58) is moderate: alternatives (land value taxation, public land trusts) are technically available and have working precedents, so collapse is not total. Resistance (0.55) is substantial: land value tax movements, YIMBY coalitions, and some economists actively contest the current arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the landowner/speculator seat, price formation reads as ordinary market outcome — 'the market' assigning value to a scarce, desirable asset, indistinguishable in their account from returns to any other investment. From the tenant/wage-laborer seat, the same price structure reads as an unearned toll on access to the location where economic opportunity concentrates. The engine should compute a genuine seat divergence here: the land component alone (fixed supply, location scarcity) is mountain-like from an analytical seat, but rent CAPTURE atop that mountain is snare-like from the payer seats — the hybrid structure named in the expected delta is exactly this: mountain (land scarcity) + snare (rent capture) + rope (improvement coordination), coexisting in one price.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners, speculators, and land-value-secured lenders sit at the beneficiary end: the constraint (private capture of land rent, embedded in current price formation and tax rules) subsidizes them directly and they have arbitrage-grade exit (they can sell, hold, or relocate capital). Tenants and wage laborers sit at the target end: they are trapped by locational necessity (jobs, schools, social ties) and pay the capitalized rent whether they hold title or not. Productive improvers occupy an intermediate position — they benefit from the coordination function of price formation (it lets them price and finance genuine construction) but are also penalized because current tax and price structures do not separate their earned improvement value from the land rent riding alongside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a live mismatch: the coordination function (allocating scarce locations, financing genuine improvement) remains genuinely necessary — eliminating price formation entirely would be incoherent — but the specific mechanism by which land rent is privately captured rather than publicly recovered has no comparable ongoing justification; it persists because assessors lack political capacity to split land/improvement taxation and because organized landowner interests actively resist reform. This is not classic mandatrophy (the whole arrangement outliving its function) but a partial one: the improvement/coordination component remains live while the land-rent-capture component has arguably always been the extractive residue the arrangement never separated out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_improvement_decomposability,
    'Can observed real estate prices be cleanly decomposed into a land-rent component and an improvement-value component, or is the decomposition itself a modeling artifact that varies with assessment methodology?',
    'Compare jurisdictions using split-rate (land value tax) assessment against standard unified-rate assessment; examine whether assessed land values track independently-observable scarcity/location proxies (transit access, school quality, employment density) net of construction-cost indices for the improvement component.',
    'If the decomposition is robust and empirically recoverable, the Georgist reading''s core claim is vindicated as a measurable structural fact, not merely a normative framing. If the decomposition is highly sensitive to assessment methodology, the reading''s ε may be partly an artifact of accounting convention rather than a discoverable feature of the underlying price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_improvement_decomposability, empirical, 'Whether the land/improvement price split is a real structural feature or a methodology-dependent construct.').

omega_variable(
    committer_kernel_location,
    'This constraint is one reading (georgist_reading) of the contested price_formation_kernel, which also has naturalist_reading, institutional_reading, and financialization_reading as sibling constraints. Where precisely does the disagreement among readings live?',
    'The disagreement is located in whether observed price contains a normatively distinguishable ''unearned'' component at all. The naturalist_reading denies any earned/unearned distinction (price is simply market-clearing equilibrium). The institutional_reading agrees price is constructed but attributes construction to zoning/lending/tax rules rather than to a land-vs-improvement moral decomposition. The financialization_reading attributes price dynamics to credit and asset-feedback loops largely orthogonal to the land/improvement split. Resolving which reading best explains observed price variance would require decomposing price movements by their correlation with credit expansion (financialization), zoning changes (institutional), scarcity/preference shocks (naturalist), and land-value-index divergence from construction-cost-index (georgist) — these are not mutually exclusive causal channels, which is precisely why they are authored as separate constraints rather than competing values of one ε.',
    'If land-value-index divergence from construction-cost trends explains most price variance, the georgist_reading''s high ε is well-supported structurally. If credit-expansion metrics explain more variance, the financialization_reading is likely the dominant structural account and this reading''s ε may overstate the land-rent channel''s causal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_location, conceptual, 'Committer-frame note: names the kernel, this reading, sibling readings, and where their structural disagreement is located.').

omega_variable(
    site_value_origin_attribution,
    'Is the rise in a specific parcel''s site value ever attributable in part to the current owner''s own actions (e.g., early speculative risk-bearing, assembly of adjacent parcels, advocacy for nearby infrastructure), rather than purely to socially-produced external value?',
    'Case-level historical analysis of specific high-appreciation parcels, tracing whether owner actions (risk capital deployed before value was evident, political advocacy for adjacent public investment) contributed causally to appreciation versus pure passive holding through exogenous neighborhood change.',
    'If owner-attributable appreciation is non-trivial in a meaningful share of cases, the pure Georgist ''entirely unearned'' framing overstates victimhood and the extractiveness score should be revised downward for those cases; if passive holding dominates empirically, the current high extractiveness score is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(site_value_origin_attribution, empirical, 'Whether site value appreciation is ever partly earned by owner action rather than purely socially produced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__georgist_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__georgist_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__georgist_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__georgist_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__georgist_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__georgist_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__georgist_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__georgist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__georgist_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__georgist_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__georgist_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__georgist_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__georgist_reading, 0.1).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the price_formation_kernel, each authored as a structurally distinct constraint per the epsilon-invariance principle (a single natural-language label — 'how housing prices form' — covers claims with genuinely different extraction profiles and beneficiary structures, so each reading gets its own file). The georgist_reading claims high, structurally located extraction (land rent capture) with a hybrid mountain/snare/rope internal structure. The naturalist_reading (sibling, not this file) would author near-zero extraction, treating the same price as objective equilibrium. The institutional_reading (sibling) would author extraction as a function of specific zoning/lending/tax rules rather than a land/improvement moral split. The financialization_reading (sibling) would author extraction as a function of credit-cycle dynamics. The georgist_reading likely INFLUENCES the institutional_reading (land value taxation proposals are institutional reforms responding to the georgist diagnosis) and stands in COEXISTS_WITH relation to naturalist and financialization readings, since all three remain live positions held by different economic schools without one logically foreclosing the others within a single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
