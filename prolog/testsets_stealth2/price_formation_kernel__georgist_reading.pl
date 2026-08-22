% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Undifferentiated Landed-Property Price Formation (Georgist Reading)
 *   domain: political economy/housing markets/institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the georgist_reading of the
 *   price_formation_kernel: the claim that market prices for landed property
 *   decompose into two structurally different components — location rent,
 *   which arises from the site's fixed supply and the community's development
 *   around it and is therefore unearned by its titleholder, and improvement
 *   value, which reflects labor and capital embodied in buildings and is
 *   earned. The standing arrangement under contest is the undifferentiated
 *   regime in which law, taxation, and credit treat land and improvements as
 *   a single bundle: one deed, one ad valorem tax base, one mortgageable
 *   asset. Read through this reading's own lights, that arrangement lets
 *   titleholders collect location rent they did not create while the same
 *   title system genuinely coordinates improvement investment and the
 *   allocation of space. The sibling readings (naturalist, institutional,
 *   financialization) instantiate different constraints from the same kernel
 *   and are authored in their own files; nothing about them is averaged into
 *   this one. The epsilon referent is the standing bundled arrangement as
 *   this reading assesses it — never the land-value-recaptured alternative
 *   the reading endorses.
 *
 * KEY AGENTS:
 *   - - urban_landowners: primary beneficiary (powerful/arbitrage) — collects the location premium on appreciating sites
 *   - - mortgage_lending_institutions: secondary beneficiary (institutional/arbitrage) — interest and fee income scale with bundled property values
 *   - - incumbent_homeowners: beneficiary with payer exposure (organized/constrained) — paper gains plus tax and move-cost burdens
 *   - - residential_tenants: primary payer (powerless/trapped) — pays the location premium monthly with no equity accumulation
 *   - - aspiring_first_time_buyers: payer (moderate/constrained) — races appreciation to buy into job-rich locations
 *   - - productive_local_businesses: payer (organized/mobile) — bears commercial site premiums, partially passed through
 *   - - land_developers: dual-positioned payer-beneficiary (powerful/arbitrage) — pays site premiums on acquisition, earns improvement margins
 *   - - municipal_fiscal_authorities: agenda-setter (institutional/constrained) — administers the bundled tax base its revenue depends on
 *   - - land_value_tax_advocates: excluded voice (organized/constrained) — carries the separation proposal outside the legislative agenda
 *   - - political_economy_analysts: analytical observer (analytical/analytical) — measures the distribution the other seats dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.72).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.6).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Undifferentiated Landed-Property Price Formation (Georgist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political economy/housing markets/institutional analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, 'f1c714cd-da07-4db3-b796-55c3650e68b1').
narrative_ontology:cs_kernel_codification('f1c714cd-da07-4db3-b796-55c3650e68b1', distributed).
narrative_ontology:cs_authority_grounding('f1c714cd-da07-4db3-b796-55c3650e68b1', lineage).
narrative_ontology:cs_interpretation_layer_present('f1c714cd-da07-4db3-b796-55c3650e68b1').
narrative_ontology:cs_reading_relation('f1c714cd-da07-4db3-b796-55c3650e68b1', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1c714cd-da07-4db3-b796-55c3650e68b1', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('f1c714cd-da07-4db3-b796-55c3650e68b1', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('f1c714cd-da07-4db3-b796-55c3650e68b1', foundational, location_rent_is_community_created_unearned).
narrative_ontology:cs_axiom_status(location_rent_is_community_created_unearned, holdable).
narrative_ontology:cs_axiom_grounding('f1c714cd-da07-4db3-b796-55c3650e68b1', location_rent_is_community_created_unearned, empirically_contingent).
narrative_ontology:cs_axiom('f1c714cd-da07-4db3-b796-55c3650e68b1', foundational, private_capture_of_location_rent_is_unjust).
narrative_ontology:cs_axiom_status(private_capture_of_location_rent_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('f1c714cd-da07-4db3-b796-55c3650e68b1', private_capture_of_location_rent_is_unjust, deontological).
narrative_ontology:cs_axiom('f1c714cd-da07-4db3-b796-55c3650e68b1', secondary, improvement_value_belongs_to_its_producer).
narrative_ontology:cs_axiom_status(improvement_value_belongs_to_its_producer, holdable).
narrative_ontology:cs_axiom_grounding('f1c714cd-da07-4db3-b796-55c3650e68b1', improvement_value_belongs_to_its_producer, deontological).
narrative_ontology:cs_reference_frame('f1c714cd-da07-4db3-b796-55c3650e68b1', classical_factor_return_separability).
narrative_ontology:cs_drift_state('f1c714cd-da07-4db3-b796-55c3650e68b1', contemporary_financialized_housing_markets, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1c714cd-da07-4db3-b796-55c3650e68b1', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, urban_landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, mortgage_lending_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, incumbent_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, residential_tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, aspiring_first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_local_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, land_developers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, incumbent_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, land_developers).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, separable_land_value_assessment).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, ricardian_differential_rent_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold titled parcels in metropolitan locations where population growth, transit investment, and neighborhood development raise site values. Collect rents and sale proceeds that embed the location premium; can realize gains by selling, refinancing, or exchanging into other properties, and can borrow against accrued equity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, urban_landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Underwrite loans secured by property whose appraised value combines site value with improvement value. Interest income and origination volume scale with total property prices, and loans are packaged and sold onward, distributing exposure while retaining servicing fees. Balance sheets are sensitive to any policy that separates or publicly recaptures site value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mortgage_lending_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Own homes in appreciating neighborhoods; paper wealth and borrowing capacity grow with the location premium even where the gain reflects surrounding development rather than their own effort. Pay property taxes levied on the combined bundle and would face high buy-in prices if they moved within the same metro; organize politically around tax limitation and neighborhood character.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, incumbent_homeowners, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, incumbent_homeowners, payer).

% Rent units in the same appreciating locations. Monthly payments cover both the use of the dwelling and the site beneath it; they build no equity, face escalating renewals as site values rise, and moving entails deposits, school and commute disruption, and distance from work and family.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, residential_tenants, payer,
    powerless, immediate, trapped, regional).

% Need to purchase into desirable labor-market locations to access jobs and services. The price they must save against includes the capitalized location premium, which grows faster than wages in high-demand metros; relocating to cheaper regions trades earnings, networks, and family proximity for affordability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, aspiring_first_time_buyers, payer,
    moderate, biographical, constrained, national).

% Operate retail, office, and industrial activity in location-constrained markets. Commercial rents embed the site premium and are passed into wages and prices; relocation to cheaper areas is possible but sacrifices workforce pools, customer density, and supplier proximity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_local_businesses, payer,
    organized, biographical, mobile, continental).

% Acquire sites — paying prior owners the accumulated location premium — entitle and build improvements, and earn margins on construction and sales. Returns on the structures they build reflect their own activity; appreciation of the bare site during the holding period accrues without corresponding effort. They lobby for entitlements that raise neighboring site values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_developers, payer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, land_developers, beneficiary).

% Run assessment offices that already estimate site and improvement values separately for administrative purposes, levy property taxes on the combined total, and zone land in ways that move site values. Revenue stability depends on the aggregate tax base, giving them a fiscal stake in continued appreciation even as constituents press them on affordability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, municipal_fiscal_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Maintain the analytic tradition that distinguishes site value from improvement value and campaign to shift taxation onto site value. They publish, litigate, and run ballot measures that repeatedly fail against homeowner opposition; their proposals are periodically revived after housing crises.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_value_tax_advocates, excluded,
    organized, generational, constrained, global).

% Measure factor shares, construct land-price indices, and study the incidence of property taxation across the seats in this story. They produce the measurement record the other parties argue from and take no direct position in the distribution they describe.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, political_economy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, urban_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Alienable, enforceable title to sites and structures coordinates who occupies which location, lets owners finance and keep the returns on improvements they build, and produces a single price signal that rations scarce locations among competing uses.
% TRANSFER_FUNCTION: Moves location rent — the site value generated by population growth, public infrastructure, and neighborhood development around a fixed parcel — from tenants, aspiring buyers, and location-dependent businesses to land titleholders and their mortgage lenders, through rents, purchase prices, and debt service.
% ABSENT_VOICES: Tenants and the not-yet-existing residents of growing cities have no seat in assessment practice, zoning hearings, or tax design; land-value-tax advocates are organizationally present but legislatively excluded; future buyers appear only by proxy.
% DISAPPEARANCE_RATIONALE: If the bundled regime dissolved overnight — site value publicly recaptured or prices explicitly separated — site values would fall toward use value, household and bank balance sheets carrying land-leveraged collateral would reprice, municipal tax bases would shift composition, and improvement incentives would strengthen; the housing economy would reorganize around the new price structure.
% FOUNDING_PROBLEM: Securing tenure and financing improvement: granting exclusive, alienable title over land and buildings as one bundle made possession defensible, made buildings usable as loan collateral, and let builders own what they built.
% FOUNDING_PROBLEM_CORROBORATION: Land-registration historians and urban economists outside the beneficiary set attest that the original tenure-insecurity and collateral problems were real; the same literature disputes that solving them required letting location rent privatize, citing jurisdictions that secure tenure while taxing or leasing land publicly. No corroboration exists for the claim that the founding problem requires the current undifferentiated form.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.72 because, in the metros where the arrangement binds hardest, site value accounts for roughly half to two-thirds of residential property value, and this reading counts that component as a transfer from the paying seats to titleholders rather than payment for anything produced. Suppression is authored at 0.60 as a raw structural property — it is NOT scaled by power or scope; the engine applies scaling only to extractiveness. The suppression is mostly structural (enforceable possession, eviction and foreclosure machinery, municipal fiscal dependence on the bundled tax base) with an internalized layer (the naturalization of landownership as a deserved reward). Theater_ratio 0.30: assessment rituals, demand-side affordability subsidies that never touch site value, and planning consultations perform responsiveness while the bundle persists; core title enforcement remains functional. Accessibility_collapse 0.42: working alternatives stay visible once the decomposition is understood — jurisdictions that tax site value separately, public leaseholding, community land trusts — so understanding does not collapse the option space. Resistance 0.55: a recurring reform movement, tenant organizing, and repeated ballot campaigns meet organized homeowner defense. The measurement series share one time grid (points 0-40 at steps of 8) so every tracked metric is authored at every examined point; end-state values equal the base_properties scalars. Across the interval, site-value share of prices rose, possession enforcement formalized and intensified, and performative affordability activity expanded alongside.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. Payer seats — tenants, aspiring buyers, location-dependent businesses — experience the bundle as enforced transfer: they pay for a component nobody produced. Beneficiary seats — titleholders, lenders, incumbent owners — experience the same bundle as legitimate return and as the coordination they participate in; from those seats the arrangement looks like the price of tenure security and credit access. The municipal seat is internally split: administrator of the bundle and fiscal dependent on its appreciation. Same-power divergence: organized businesses and organized homeowners hold comparable civic power, yet businesses bear the premium as a cost to shed (mobile) while homeowners collect it as a gain (constrained into defending it) — exit asymmetry, not power, differentiates their positions. Coalition potential among the weaker seats exists in principle — tenants, aspiring buyers, and affordability-minded businesses overlap — but tenure divides and time-horizon mismatches fragment it; the engine may register this as unrealized coalition capacity. Incumbent homeowners additionally show a quasi-identity lock: retirement security and self-concept are fused with home equity, which would have to break before many of them could support separation.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (urban_landowners, mortgage_lending_institutions, incumbent_homeowners) derive low directionality — the arrangement subsidizes them. Declared victims (residential_tenants, aspiring_first_time_buyers, productive_local_businesses) derive high directionality. Exit modulation orders the payer seats: trapped tenants sit nearest the full-target end, mobile businesses furthest from it, constrained buyers between. Dual-positioned agents (incumbent homeowners, developers) carry offsetting roles that the derivation reads through both declarations. No directionality override is authored: the derivation from declared roles and exit options is adequate, and the override surface keys on power atoms, which would misapply across the heterogeneous institutional seats — lenders and municipalities share the institutional atom but sit on opposite sides of the flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making possession defensible and improvement investable through alienable title — is still partially live, so the arrangement is not a resolved mandate; but in high-demand metros the rent-collection function now outweighs the coordination function it grew from. Classifying the arrangement as a hybrid keeps two errors apart: reading it as pure extraction would erase the genuine tenure-and-collateral function that reform must preserve (and would push reform toward abolishing title rather than separating site value); reading it as pure coordination would erase the asymmetric flow by which community-created site value accrues privately. The R5 interview shows no zombie signature: the founding problem is contested rather than dead, and the world-rearranges verdict confirms the arrangement is load-bearing, not vestigial. Fixing is costly for whoever could fix it: legislatures face homeowner-voter resistance, mortgage-collateral repricing risk, and assessment litigation, which is why the cost-to-fix is prohibitive even where the analytic case for separation is strong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the land-versus-improvement cut the correct decomposition of price formation, or do the sibling readings (credit-driven, institution-constructed, natural-equilibrium) identify the operative structure?',
    'Comparative performance across the four linked stories: predictive tests on land-share series, cross-jurisdiction natural experiments where site value is taxed or leased separately, and adjudication of which reading''s interventions track observed outcomes.',
    'Adopting the financialization reading relocates the extraction locus from titleholders to credit intermediaries; adopting the institutional reading relocates it to the authors of regulatory and tax rules; adopting the naturalist reading dissolves the unearned/earned distinction and collapses this story''s victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which sibling reading of the price-formation kernel identifies the operative constraint.').

omega_variable(
    bundle_naturalness_vs_construction,
    'Is the undifferentiated land-plus-improvement price bundle a natural market outcome or a constructed fiscal-legal artifact maintained by assessment practice and tax design?',
    'Jurisdictions that assess and tax site value separately, or lease land publicly, show whether separation is administratively feasible and behaviorally consequential; assessor offices already compute the split for their own records.',
    'If constructed, the bundle is reformable and the arrangement''s persistence reflects choice rather than necessity; if treated as natural, the arrangement acquires immunity from reform pressure and interventions misfire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_naturalness_vs_construction, empirical, 'Whether the bundled price formation is a natural regularity or a maintained institutional artifact.').

omega_variable(
    land_share_measurement_ambiguity,
    'What fraction of observed property prices is location rent rather than improvement value, given heterogeneous assessment methods and improvement-depreciation assumptions?',
    'Hedonic regressions and assessor land-residual series harmonized across metros, with sensitivity analysis over depreciation schedules.',
    'Calibrates epsilon: a higher defensible land share raises the burden borne by the payer seats; a lower share shifts weight toward the improvement-coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_share_measurement_ambiguity, empirical, 'Defensible magnitude of the location-rent component inside observed prices.').

omega_variable(
    hybrid_classification_boundary,
    'Does the title system''s coordination function (tenure security, collateral, allocation) remain substantial enough that the arrangement is a genuine hybrid, or has rent capture so far outgrown it that the coordination story is cover?',
    'Compare jurisdictions where tenure security is provided without private location-rent capture (public leaseholds, community land trusts): if security persists while rent capture is removed, the functions are separable and the capture side stands alone.',
    'If separable, the arrangement trends toward pure extraction wearing a coordination cover; if inseparable, part of the measured burden is the irreducible price of coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_classification_boundary, conceptual, 'Whether the coordination and rent-capture components of the title system are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__georgist_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(pric_tr_t8, observed).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__georgist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement_basis(pric_tr_t16, observed).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__georgist_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(pric_tr_t24, observed).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__georgist_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement_basis(pric_tr_t32, observed).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(pric_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__georgist_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(pric_be_t8, observed).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__georgist_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(pric_be_t16, observed).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__georgist_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(pric_be_t24, observed).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__georgist_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement_basis(pric_be_t32, observed).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(pric_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__georgist_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(pric_su_t8, observed).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__georgist_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(pric_su_t16, observed).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__georgist_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement_basis(pric_su_t24, observed).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__georgist_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement_basis(pric_su_t32, observed).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(pric_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what determines house prices?' decomposes into four structurally distinct readings of one kernel, each with its own stable epsilon and its own beneficiary/victim structure. This file is the georgist_reading; the naturalist, institutional, and financialization readings are separate stories. The upstream/downstream structure runs through shared evidence: assessor land-value separation and Ricardian rent theory (vindicated here) are cited by the institutional reading's tax-treatment analyses, while the financialization reading contests this reading's causal weighting of land rent versus credit. Linkage is declarative only — no metric is averaged across family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
