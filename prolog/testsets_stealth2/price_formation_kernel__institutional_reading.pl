% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutional Construction of Housing Prices (Zoning-Credit-Tax-Platform Apparatus)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   Housing prices in developed economies are set inside an institutional
 *   apparatus rather than emerging from unmediated exchange: municipal zoning
 *   and discretionary permitting constrain supply; lending standards and
 *   guarantee parameters shape who can convert income into purchasing power;
 *   tax provisions tilt returns toward owner occupation; and intermediary
 *   platforms gate and tax every transaction. Each element solves some real
 *   coordination problem, and each simultaneously channels a share of housing
 *   cost from entrants and tenants to incumbents, lenders, and brokers. The
 *   claim and the metrics are independent authored facts: the constraint is
 *   CLAIMED as tangled_rope - genuine coordination fused with asymmetric,
 *   enforced extraction - while the metrics describe the apparatus's actual
 *   operation as this reading assesses it. Epsilon's referent is the standing
 *   apparatus itself, never the arrangements a reform program would install.
 *
 * KEY AGENTS:
 *   - incumbent_homeowners: Primary beneficiary and political enforcer (organized/identity_locked) - collect appreciation and dominate the forums where supply rules are made
 *   - renters: Primary target (powerless/trapped) - pay scarcity rents without accumulating equity
 *   - first_time_buyers: Primary target (powerless/constrained) - absorb the full capitalized price as mortgage debt
 *   - mortgage_lenders: Secondary beneficiary (institutional/arbitrage) - earn on inflated principals and sell risk forward
 *   - real_estate_intermediaries: Secondary beneficiary and rule-writer (organized/constrained) - collect percentage commissions protected by listing-service access rules
 *   - municipal_zoning_authorities: Administrative agenda-setter (institutional/constrained) - run permitting under homeowner political dominance
 *   - banking_regulators and national_tax_authorities: Systemic agenda-setters (institutional/constrained) - set the credit and tax parameters
 *   - residential_developers: Dual-positioned (powerful/mobile) - profit from scarcity on held land, blocked by it on new builds
 *   - housing_reform_advocates and priced_out_migrants: Excluded voices (organized/powerless) - press reform from outside the decision forums, or are locked out before arrival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.62).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Prices (Zoning-Credit-Tax-Platform Apparatus)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '1563b2f0-9a5c-4237-b821-e7312100a02a').
narrative_ontology:cs_kernel_codification('1563b2f0-9a5c-4237-b821-e7312100a02a', distributed).
narrative_ontology:cs_authority_grounding('1563b2f0-9a5c-4237-b821-e7312100a02a', distributed).
narrative_ontology:cs_reading_relation('1563b2f0-9a5c-4237-b821-e7312100a02a', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1563b2f0-9a5c-4237-b821-e7312100a02a', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('1563b2f0-9a5c-4237-b821-e7312100a02a', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('1563b2f0-9a5c-4237-b821-e7312100a02a', foundational, price_formation_is_institutionally_constructed).
narrative_ontology:cs_axiom_status(price_formation_is_institutionally_constructed, holdable).
narrative_ontology:cs_axiom_grounding('1563b2f0-9a5c-4237-b821-e7312100a02a', price_formation_is_institutionally_constructed, empirically_contingent).
narrative_ontology:cs_axiom('1563b2f0-9a5c-4237-b821-e7312100a02a', secondary, regulatory_scarcity_capitalizes_into_owner_equity).
narrative_ontology:cs_axiom_status(regulatory_scarcity_capitalizes_into_owner_equity, holdable).
narrative_ontology:cs_axiom_grounding('1563b2f0-9a5c-4237-b821-e7312100a02a', regulatory_scarcity_capitalizes_into_owner_equity, empirically_contingent).
narrative_ontology:cs_reference_frame('1563b2f0-9a5c-4237-b821-e7312100a02a', apparatus_constituted_pricing).
narrative_ontology:cs_drift_state('1563b2f0-9a5c-4237-b821-e7312100a02a', contemporary_reform_natural_experiment_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('1563b2f0-9a5c-4237-b821-e7312100a02a', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, residential_developers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, residential_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own homes whose assessed value tracks the scarcity the permitting system maintains. Vote in local elections at high rates, attend planning hearings, and organize through neighborhood associations to oppose density near their properties. Household balance sheets and retirement plans are denominated in home equity, so proposals that would lower prices register as personal losses. Selling and leaving would realize gains but dissolve the community standing and identity built around the property and its appreciation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, agenda_setter,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary).

% Administer comprehensive plans, discretionary review, and permit approval. Councils and planning commissions are elected or appointed from the local population, which skews toward long-tenured homeowners; staff careers depend on avoiding appeals and litigation from well-organized objectors. Upzoning invites recall campaigns and lawsuits; maintaining restrictions draws little organized opposition.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_zoning_authorities, agenda_setter,
    institutional, generational, constrained, local).

% Set capital requirements, underwriting standards, and guarantee parameters such as conforming loan limits that determine who can borrow how much against housing collateral. Standards tighten after crises and loosen under political pressure to broaden ownership; the rules define the credit channel through which household income becomes purchasing power.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, banking_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Write and administer the tax provisions that shape housing returns: mortgage interest deductibility, capital-gains exclusions on principal residences, property-tax assessment limits, and the tax advantages of owner occupation over tenancy. Each provision is defended by its beneficiaries and revisited only inside broad fiscal negotiations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, national_tax_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Broker transactions for commission percentages preserved by longstanding industry norms and enforced through listing-service access rules that their own associations write. Agent licensing and portal gatekeeping add layers between sellers and buyers. Proposals for decoupled fees, flat-fee listings, or open seller access threaten the commission stream and are resisted through licensing boards and association lobbying.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediaries, agenda_setter).

% Originate mortgages against appraised values that embed the scarcity premium, earning interest and origination fees on the inflated principal. Securitization moves risk forward to investors, limiting exposure to any single market; the volume-based revenue model rewards larger balances, which higher prices deliver automatically.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% Pay monthly amounts set by the same scarcity that raises purchase prices, without accumulating equity. Moving means new deposits, disrupted schooling and commutes, and re-entry into a market whose vacancy rates are kept low by the same supply constraints; staying means absorbing increases bounded only by landlord pricing power. Down-payment savings recede as prices outrun wages.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, regional).

% Enter the market carrying the full capitalized price of decades of accumulated restrictions, financed by mortgage debt sized to that price. The difference between what the previous generation paid for the same housing stock and today's entry price is the transfer they bear. Delaying purchase means paying rents that consume the savings a purchase would require.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    powerless, biographical, constrained, national).

% Hold land banks whose value appreciates under scarcity, and build infill projects whose margins depend on the same elevated prices. Permitting delays, impact fees, and discretionary review raise costs and block projects outright; the industry lobbies for individual entitlements while profiting from the scarcity its competitors' blocked projects maintain. Geographic diversification lets capital shift toward friendlier jurisdictions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, residential_developers, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, residential_developers, payer).

% Organize for upzoning, streamlined permitting, and tenant protections, winning occasional reforms through citywide plan amendments, state preemption laws, and accessory-dwelling legalization. They are outnumbered at the hearings where decisions are actually made, where attendance skews heavily toward opposed homeowners; their victories arrive through legislatures and courts rather than the local processes that administer the rules day to day.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_reform_advocates, excluded,
    organized, generational, constrained, national).

% Would move to high-wage metropolitan areas where their earnings potential is highest but cannot clear the entry price of either rent or purchase. They take jobs elsewhere at lower productivity and never appear in any local planning process, because they are not yet residents of the places deciding whether to admit them.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, priced_out_migrants, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The apparatus solves real problems: zoning and review manage land-use externalities and infrastructure load; lending standards and guarantee parameters maintain credit discipline and systemic stability; tax provisions channel investment toward owner-occupied housing and municipal finance; intermediary platforms reduce search and matching costs in the largest purchase most households ever make.
% TRANSFER_FUNCTION: Moves housing wealth and future income streams from renters and first-time buyers to incumbent owners, lenders, and intermediaries: scarcity rents flow upward monthly, interest and commissions are taken on every transaction, and regulatory restriction is capitalized into existing owner equity as windfall appreciation.
% ABSENT_VOICES: Priced-out migrants never enter any jurisdiction's process because they are not yet residents of the places making admission decisions. Renters are systematically underrepresented at planning hearings, where attendance skews toward opposed homeowners. Future entrants bear the capitalized price as debt but hold no seat anywhere in the apparatus.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse the scarcity premium: prices would fall toward replacement cost plus location rent, incumbent equity would compress sharply, lender balance sheets would absorb collateral losses, and intermediary volumes would shrink as transaction values repriced. The rearrangement would be violent precisely because so much household wealth is capitalized into the current structure.
% FOUNDING_PROBLEM: Rapid urbanization produced nuisance externalities, unsafe housing stock, speculative land bubbles, and recurrent credit collapses; the apparatus was assembled to order land use, stabilize housing finance, and widen ownership - and, in significant part, to sort neighborhoods by race and class.
% FOUNDING_PROBLEM_CORROBORATION: Urban economics and housing-finance scholarship corroborate the live portions (externality management, credit-cycle stabilization); fair-housing jurisprudence and official historical commissions attest that the sorting rationale was formally repudiated. Central-bank post-crisis reviews document the credit-stability rationale from outside the beneficiary set. No source outside the benefiting parties attests that current restrictiveness levels track the original functions.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.68: price-to-income divergence in restrictive markets, interest and commission shares of transaction value, and capitalized windfalls to incumbents are large relative to any defensible service cost, but the apparatus retains real functions (externality management, credit discipline, search-cost reduction), keeping it below pure-extraction levels. Suppression is 0.62 and is a raw structural property, unscaled by power or scope - it reflects the machinery (discretionary review, weaponized appeal rights, listing-service gatekeeping, licensing boards) that blocks alternatives; only extractiveness is scaled downstream. Theater ratio 0.42: hearings, affordability studies, and inclusionary programs increasingly perform consideration while preserving the underlying restriction. Accessibility collapse 0.45: alternatives (reform jurisdictions, missing-middle typologies, cooperative tenure) persist but are costly and localized. Resistance 0.58: sustained reform organizing, litigation, and state preemption campaigns meet the apparatus directly. All three tracked metrics share one six-point grid (t=0..40, step 8). The suppression_requirement series is authored because the story specifically traces enforcement hardening - discretionary review expanded and organized-objector leverage grew over the interval - not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-homeowner seat the apparatus is experienced as protection: a system they staff, legitimately control, and rely on to safeguard savings and neighborhood character. From the renter and first-time-buyer seats the identical structure operates as a toll: the same hearings read as veto, the same standards read as admission control. The administrative seats experience mandate fulfillment; developers experience the apparatus from both sides at once, as margin on held land and barrier on new builds. Note the coalition asymmetry: renters and first-time buyers together outnumber homeowner blocs in most restrictive metros, but lower turnout, shorter tenure, and hearing-format barriers keep them from converting numbers into agenda power - the engine reads their seats as weak because their coordinated capacity currently is weak.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (incumbent homeowners, mortgage lenders, real estate intermediaries, and secondarily developers) drive those seats toward the beneficiary end of directionality; the homeowner seat sits lowest because its benefit is direct, concentrated, and fused with home equity and neighborhood identity. Victim declarations (renters, first-time buyers) drive those seats toward the target end; trapping (deposit frictions, school and job attachment) and the size of the capitalized entry price push them nearer full-target than mobility alone would. Developers derive near-symmetric directionality from their dual declaration. The excluded seats (reform advocates, priced-out migrants) are deliberately left out of the beneficiary/victim arrays: their exclusion is the finding, recorded at commentary grade, not a directionality input.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the apparatus as tangled_rope guards against both symmetrical errors. Reading it as pure extraction would erase the live coordination functions - nuisance and infrastructure management, credit-cycle discipline, transaction-cost reduction - that independent scholarship corroborates. Reading it as pure coordination would erase the enforced, asymmetric transfer that fair-housing history and price-to-income data document. On mandatrophy: part of the founding mandate is dead (racial and class sorting is formally repudiated, though its effects persist in the geography the apparatus froze) and part remains live (externality management, credit stability), so the founding-problem status is contested rather than resolved. The arrangement persists with concentrated, capitalized benefits and diffuse costs - the classic configuration in which a mandate that has outlived part of its function nonetheless survives politically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the institutional reading of the price_formation_kernel: prices are constructed by zoning, lending standards, tax treatment, and intermediary platforms. What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Cross-reading comparison within the kernel family: the naturalist reading relocates extraction to approximately zero (prices as equilibrium signals over objective scarcity); the georgist reading splits the victim set by separating land rent from improvement value and retargets the remedy at land-value taxation; the financialization reading relocates the driving force to credit expansion and asset-feedback loops. Adjudication proceeds by which causal attribution survives jurisdiction-level natural experiments.',
    'Adopting the naturalist reading would collapse this story''s beneficiary/victim structure entirely; adopting the georgist reading would preserve the apparatus diagnosis but redirect extraction accounting to land rent alone; adopting the financialization reading would re-weight the credit channel from one lever among four to the primary driver.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the price_formation_kernel; sibling readings relocate the constraining force and reshape the beneficiary/victim sets.').

omega_variable(
    jurisdictional_restrictiveness_variance,
    'The authored epsilon is a corpus-level average over wildly heterogeneous jurisdictions - does the apparatus compute differently in permissive markets (Houston, Tokyo) versus restrictive ones (coastal California, London)?',
    'Per-metropolitan recomputation using local permitting density, price-to-income ratios, and land-use governance stringency.',
    'In highly restrictive jurisdictions the arrangement computes as substantially more extractive, with the coordination cover thinning; in permissive ones the coordination function dominates and extraction approaches the resource-allocation floor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_restrictiveness_variance, empirical, 'Extractiveness varies with regulatory restrictiveness; the scalar masks a distribution.').

omega_variable(
    coordination_extraction_separability,
    'Is the apparatus''s genuine coordination function (externality management, credit stability, transaction-cost reduction) separable from its exclusionary operation, or does restricting supply inherently generate the extraction?',
    'Natural experiments: Minneapolis 2040, Tokyo''s national zoning preemption, Houston''s absence of use-zoning - if externality management persists while prices normalize, the functions are separable.',
    'If separable, the excess extraction is removable by design rather than being the price of coordination; if inseparable, part of the measured extraction is inherent coordination cost and the classification softens toward rope-like readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the coordination and extraction components of the apparatus are structurally separable.').

omega_variable(
    capitalization_transition_justice,
    'Decades of the apparatus have capitalized its effects into existing home equity; removing it now converts a flow problem into a stock problem - does transitional liability to recent buyers change the structural classification?',
    'Preference-dependent: resolves only through political choice among compensation designs (grandfathering, buyouts, long phase-ins), weighing incumbent windfall preservation against prospective entrant access.',
    'With a grandfathered transition the apparatus can unwind without destroying household balance sheets, opening reform trajectories; without it, defensive coalition strength keeps enforcement high and the extraction ratchets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capitalization_transition_justice, preference, 'Capitalized-equity lock-in makes the constraint''s persistence partly a distributive-choice question rather than a structural fact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfk_institutional_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(pfk_institutional_tr_t0, observed).
narrative_ontology:measurement(pfk_institutional_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(pfk_institutional_tr_t8, observed).
narrative_ontology:measurement(pfk_institutional_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(pfk_institutional_tr_t16, observed).
narrative_ontology:measurement(pfk_institutional_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(pfk_institutional_tr_t24, observed).
narrative_ontology:measurement(pfk_institutional_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(pfk_institutional_tr_t32, observed).
narrative_ontology:measurement(pfk_institutional_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(pfk_institutional_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pfk_institutional_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(pfk_institutional_be_t0, observed).
narrative_ontology:measurement(pfk_institutional_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(pfk_institutional_be_t8, observed).
narrative_ontology:measurement(pfk_institutional_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement_basis(pfk_institutional_be_t16, observed).
narrative_ontology:measurement(pfk_institutional_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(pfk_institutional_be_t24, observed).
narrative_ontology:measurement(pfk_institutional_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(pfk_institutional_be_t32, observed).
narrative_ontology:measurement(pfk_institutional_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(pfk_institutional_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pfk_institutional_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(pfk_institutional_su_t0, observed).
narrative_ontology:measurement(pfk_institutional_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(pfk_institutional_su_t8, observed).
narrative_ontology:measurement(pfk_institutional_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(pfk_institutional_su_t16, observed).
narrative_ontology:measurement(pfk_institutional_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(pfk_institutional_su_t24, observed).
narrative_ontology:measurement(pfk_institutional_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement_basis(pfk_institutional_su_t32, observed).
narrative_ontology:measurement(pfk_institutional_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(pfk_institutional_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'housing price formation' decomposes into four readings of one kernel (price_formation_kernel): naturalist (equilibrium over objective scarcity), georgist (land rent separated from improvement value), financialization (credit expansion and asset-feedback loops), and this institutional reading (construction by zoning, lending standards, tax treatment, and platforms). Each reading is a separate constraint story with its own epsilon, beneficiary/victim structure, and classification; they are linked here as a constraint family. The naturalist reading carries near-zero extraction and serves as the upstream no-construction baseline the other three argue against; this institutional reading feeds the georgist reading's identification of policy-made land rent and overlaps the financialization reading on the credit-standard channel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
