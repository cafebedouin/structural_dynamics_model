% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Georgist Reading: Price Formation Split into Unearned Site Rent and Earned Improvement Value
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the georgist_reading of the
 *   price_formation_kernel: the claim that market prices for occupied
 *   property decompose into an earned component (returns to labor and capital
 *   embodied in improvements) and an unearned component (site rent arising
 *   from fixed supply and location, collected by whoever holds title). The
 *   epsilon referent is the standing arrangement under contest — actual price
 *   formation as it operates, with site values capitalized into sale prices
 *   and collected as rent, mortgage interest, and unrealized appreciation —
 *   assessed by this reading's own lights, which locate substantial unearned
 *   transfer in the site component. The reading is structurally hybrid by its
 *   own account: the scarcity of locations is a hard floor no policy removes,
 *   yet the private capture of the resulting rent is an enforced social
 *   arrangement, and the improvement side of the market performs genuine
 *   coordinating work. Sibling readings (naturalist, institutional,
 *   financialization) are separate constraint files linked through
 *   network.affects_constraints; the contest between readings lives in the
 *   omega variables and kernel_context, not inside this constraint's
 *   classification.
 *
 * KEY AGENTS:
 *   - - absentee_landowners: Primary beneficiary (powerful/arbitrage) — collects site rent without producing it; capital redeployable across regions
 *   - - incumbent_homeowners: Secondary beneficiary with payer costs (organized/identity_locked) — paper gains locked in the occupied position; supplies the political defense of supply restriction
 *   - - mortgage_lenders: Beneficiary (institutional/arbitrage) — interest income scales with capitalized site values; exposure transferable via securitization
 *   - - municipal_governments: Mixed beneficiary (institutional/constrained) — recaptures a capped slice of site value while bearing the costs that create it
 *   - - residential_tenants: Primary target (powerless/trapped) — transfers the largest income share; anchored to labor markets
 *   - - aspiring_first_time_buyers: Target (moderate/constrained) — faces entry thresholds dominated by capitalized site value
 *   - - productive_local_businesses: Target with genuine offsetting benefit (moderate/constrained) — pays site premiums for real agglomeration value
 *   - - displaced_would_be_residents: Excluded voice (powerless/trapped) — priced out before entry; absent from every local forum
 *   - - land_value_tax_advocates: Analytical observer (analytical/analytical) — measures the split, holds no stake in the flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.72).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.5).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading: Price Formation Split into Unearned Site Rent and Earned Improvement Value").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '3ac09e23-4b24-45a6-bf69-c4f8f2a8be65').
narrative_ontology:cs_kernel_codification('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', distributed).
narrative_ontology:cs_authority_grounding('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', distributed).
narrative_ontology:cs_reading_relation('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', foundational, private_ground_rent_is_unearned_extraction).
narrative_ontology:cs_axiom_status(private_ground_rent_is_unearned_extraction, holdable).
narrative_ontology:cs_axiom_grounding('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', private_ground_rent_is_unearned_extraction, deontological).
narrative_ontology:cs_axiom('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', secondary, full_site_rent_capture_is_production_neutral).
narrative_ontology:cs_axiom_status(full_site_rent_capture_is_production_neutral, holdable).
narrative_ontology:cs_axiom_grounding('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', full_site_rent_capture_is_production_neutral, empirically_contingent).
narrative_ontology:cs_reference_frame('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', rent_separated_price_formation).
narrative_ontology:cs_drift_state('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', contemporary_financialized_housing_markets, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3ac09e23-4b24-45a6-bf69-c4f8f2a8be65', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, absentee_landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, municipal_governments).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, residential_tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, aspiring_first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_local_businesses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, productive_local_businesses).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, incumbent_homeowners).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, law_of_rent).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_supply_inelasticity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold titled parcels in high-demand metropolitan areas, collecting monthly rents and watching site values appreciate as surrounding populations grow and public investment lands nearby. The value they collect arises from location and community activity rather than from anything built or maintained by the titleholder. Capital is redeployable: parcels can be sold, leveraged, or exchanged across regions, and ownership frequently sits in entities that outlast any individual. Organized associations lobby to cap property taxes and resist assessment reforms that would value the site separately from the building.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, absentee_landowners, beneficiary,
    powerful, generational, arbitrage, national).

% Own the home they live in on land whose scarcity drives most of its resale value. Their household balance sheet is concentrated in this appreciating claim, so they organize politically to defend supply restrictions that protect it, while simultaneously paying the taxes, maintenance, and mortgage costs the same asset carries. Selling would realize the gain only by surrendering the home and neighborhood their daily life is organized around, so the gain stays locked in place; their sense of standing as owners is bound up with the position itself.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, incumbent_homeowners, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, incumbent_homeowners, payer).

% Extend purchase-money mortgages secured by the combined land-and-building value of residences. Because the site component dominates collateral value in expensive metros, lending volumes and interest income scale with capitalized site values. Loans are packaged and sold into securitization markets, so exposure to any single region is transferable, and downturns are buffered by recourse to borrowers and government guarantees.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% Raise operating revenue chiefly through property taxation and bear the infrastructure, schooling, and service spending that makes locations valuable in the first place. Tax-limit measures and assessment caps constrain how much of the site value they can recapture, while planning hearings give organized owners outsized influence over supply decisions. Fiscal health rises and falls with the same land values that burden their residents.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, municipal_governments, beneficiary,
    institutional, generational, constrained, regional).

% Rent the places they live, transferring a third or more of household income to whoever holds title. Leases run month to month or year to year; job locations, school enrollment, and family ties anchor them to specific labor markets, and relocating means forfeiting deposits, absorbing moving costs, and rebuilding proximity from scratch. Individually they negotiate from weakness; whatever leverage they have exists only in aggregate.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, residential_tenants, payer,
    powerless, immediate, trapped, regional).

% Earn wages sufficient to cover the cost of buildings but face entry prices dominated by the capitalized site component. Their options are delaying household formation, moving to cheaper regions away from opportunity density, pooling family resources, or waiting on inheritance. Each year of appreciation raises the threshold they must clear.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, aspiring_first_time_buyers, payer,
    moderate, biographical, constrained, national).

% Operate storefronts, workshops, and offices at locations chosen for customer access and workforce availability, paying commercial rents set by site scarcity. The location premium buys genuine foot traffic and labor-pool access, but lease renewals reset upward whenever the surrounding district appreciates, and relocating means abandoning the customer base that justified the site.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_local_businesses, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, productive_local_businesses, beneficiary).

% Would move to high-opportunity metros for work but are priced out before ever entering the local market. They appear in no local electorate, no tenant roll, and no planning hearing; their objection registers only as aggregate migration statistics.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, displaced_would_be_residents, excluded,
    powerless, biographical, trapped, national).

% Economists, assessors, and campaign organizations that measure the land share of property prices, publish split-value assessments, and advocate shifting taxation onto site value. They hold no stake in the flows themselves; their seat is analytical.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_value_tax_advocates, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, absentee_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market price formation allocates scarce, immobile sites among competing uses and signals where additional construction earns a return; keeping site value distinct from improvement value lets capital respond to location scarcity while rewarding actual production.
% TRANSFER_FUNCTION: Moves ground rent — the surplus generated at desirable locations — from tenants, wage earners, and local businesses to titleholders, via rent payments and capitalized sale prices; mortgage finance routes a further share to lenders as interest on the capitalized principal.
% ABSENT_VOICES: Displaced would-be residents priced out before entering the market, future generations who will bid against today's capitalized rents, and renters without voting standing in owner-dominated local electorates would all object if seated; they are outside the room because entry to the conversation presupposes ownership or tenancy already secured.
% DISAPPEARANCE_RATIONALE: If private ground-rent capture vanished overnight — say, through full site-value socialization — land sale prices would collapse toward zero, ownership would become custodial rather than appreciational, housing occupancy costs would fall to improvement replacement cost plus the site charge, and trillions in household and bank balance sheets keyed to land appreciation would restructure. Nothing about the physical stock of buildings would vanish; the entire financial architecture around it would rearrange.
% FOUNDING_PROBLEM: Exclusive parcel rights were instituted so cultivators and builders could invest in improvements secure from expropriation — solving the open-access problem that otherwise deters anyone from building on land they do not reliably hold.
% FOUNDING_PROBLEM_CORROBORATION: The security-of-improvements problem is attested from outside the benefiting parties by mainstream property-law scholarship and development economics, and by the operational practice of jurisdictions that separate site value from improvement value for assessment (split-rate municipalities, land-rate territories). Tenant federations and urban-economics measurement literature independently corroborate that the site component of prices behaves as the Georgist reading describes. Landowner associations dispute the unearned characterization of the rent component, which is itself signal that the corroboration does not come from the beneficiary set.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.72 because in major metropolitan markets the site component accounts for half to two-thirds of dwelling prices, and that component is collected by titleholders without corresponding production — the flow scales with community growth, not owner effort. Suppression is authored at 0.50 as a raw structural property, deliberately unscaled: it reflects shelter necessity, title and eviction machinery, and the political entrenchment of tax-limit and supply-restriction rules, not any context multiplier. Theater is 0.25: the 'we provide housing' framing under which titleholders market themselves overstates a contribution that is largely the site alone, but the underlying price signal and allocation function are real, so performance is a minority share of activity. Accessibility_collapse is 0.45 — alternatives exist (cheaper regions, renting, density, delayed entry) but opportunity concentrates precisely where site rents are highest, so understanding the structure narrows rather than eliminates workable exits. Resistance is 0.60: tenant unions, rent-stabilization campaigns, site-value ballot measures, and supply-deregulation organizing constitute sustained, visible opposition unusual for an arrangement defended as ordinary market operation. The three temporal series share one six-point grid (1950–2025) so every metric is authored at every examined time point; the trajectories track the secular rise in the land share of prices, with cyclical boom-bust oscillation riding on top of the trend rather than constituting it — the cycle is treated as exogenous credit weather, not as an intermittent-reinforcement mechanism, though omega rent_vs_anticipation_blur flags the speculative layer this leaves unresolved. Receipt surface: the gains demonstrably accrue to the titleholding seat (absentee_landowners), with a routed secondary share to lenders as interest on capitalized principal; fixing_cost is prohibitive because although shifting taxation onto site value is economically non-distortionary, the fixers (legislatures) face organized owner blocs, constitutional tax-limit entrenchment, and balance-sheet shock to the majority of households, making the political cost of the fix exceed any incumbent's appetite for it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently from identical structural data. From the tenant and aspiring-buyer positions the arrangement operates as enforced transfer with trapped exit — the highest-chi profile in the story. From the absentee-owner position the same structure is a legitimate return to a titled asset acquired in good faith, with full exit mobility damping experienced extraction toward zero or negative. The homeowner seat sits between: beneficiary by role, yet bearing carrying costs and unable to realize gains without exiting the position, so its computed chi should land mid-range rather than at the beneficiary pole. The municipal seat adds an institutional wrinkle — a collector whose receipts are capped by the same political economy it administers. The engine computes these divergences from the declared roles, exits, and scopes; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. absentee_landowners (arbitrage exit, generational horizon) sit nearest the beneficiary pole — the arrangement subsidizes them and they can leave with gains realized. mortgage_lenders derive similarly low d: they collect a routed share with transferable exposure. incumbent_homeowners carry a secondary payer role and identity-locked exit, pulling their derived d off the beneficiary pole toward the middle — the identity lock cuts both ways, since the position they cannot leave is the one generating their gain. municipal_governments derive as beneficiaries but with damped intensity given constrained exit and capped receipts. residential_tenants (trapped, powerless) and aspiring_first_time_buyers (constrained) sit near the full-target pole; productive_local_businesses are pulled back from it by their secondary beneficiary role — the site premium buys real agglomeration value, so their net position is target-side but not maximally so. displaced_would_be_residents are excluded rather than coordinated: their exclusion is a consequence of the arrangement, not an enforcement object. No directionality overrides were needed: the role/exit declarations reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification is what prevents mislabeling in both directions. Collapsing the arrangement to pure extraction would erase the genuine coordination function this reading explicitly affirms — price signals do allocate scarce sites and do direct construction capital, and the security-of-improvements function the founding problem names is still live, which is why founding_problem_status is live and no mandatrophy resolution is declared. Collapsing it to pure coordination would erase the unearned-rent critique that defines this reading. The tangled-rope classification holds both truths in one structure: coordination of production on the improvement side, asymmetric enforced capture on the site side. It equally resists a piton misread — the arrangement is not maintained by inertia or performance but by active, interested enforcement with identifiable concentrated beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading (georgist_reading) of the price_formation_kernel; is the land-versus-improvement decomposition the operative structure of price formation, or do the sibling mechanisms (natural equilibrium, institutional construction, credit-driven financialization) dominate?',
    'Comparative structural fit: test which reading''s decomposition best predicts observed price behavior across regimes (supply-elastic vs supply-inelastic metros, credit-cycle phases, differing tax treatments), treating the four readings as rival structural hypotheses over the same price data.',
    'Adopting the financialization reading moves the extraction locus from titleholders to credit intermediaries and changes the victim set; adopting the naturalist reading dissolves the extraction claim entirely and reclassifies the arrangement toward coordination-only; adopting the institutional reading relocates causation to zoning and tax rules while preserving much of this reading''s victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Which reading of the price-formation kernel correctly identifies the constraint''s operative structure.').

omega_variable(
    land_component_naturality,
    'Is the fixed-supply, location-scarcity core of this reading a genuine natural limit beneath the arrangement, or is the observed scarcity substantially manufactured by supply restrictions that a different policy regime would relax?',
    'Cross-regime comparison of land-share behavior where physical geography and regulatory elasticity differ: if land shares converge where regulation loosens, much of the scarcity is constructed; if they persist, the natural floor is confirmed.',
    'If scarcity is largely manufactured, the mountain-floor attribution weakens and the whole arrangement reads as enforced extraction with no irreducible component; if the floor is genuine, part of the measured cost is irreducible and the extractive share is correspondingly smaller than the headline metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_component_naturality, empirical, 'How much of the site-scarcity component is natural fact versus constructed restriction.').

omega_variable(
    rent_vs_anticipation_blur,
    'How much of measured land value is pure location rent versus capitalized anticipation of future restriction and appreciation — a speculative layer that behaves differently from steady-state rent?',
    'Event studies around rezoning announcements, infrastructure commitments, and tax-limit referenda: discontinuous repricing at those moments identifies the anticipation component separable from underlying rent.',
    'A large anticipation component means the arrangement carries a self-reinforcing speculative dynamic layered on top of rent capture, changing persistence dynamics and making bust-phase measurements systematically understate the steady-state extraction rate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_vs_anticipation_blur, empirical, 'Separating steady-state site rent from capitalized speculative anticipation within measured land value.').

omega_variable(
    enforcement_dependence,
    'Does the rent-capture arrangement persist because participants prefer it, or because active enforcement machinery (title registries, eviction law, tax-limit constitutional provisions) holds it in place against latent majority preference?',
    'Natural experiments from jurisdictions that shifted to site-value rating or strengthened tenant protections: track whether private rent capture erodes when enforcement posture changes without participant composition changing.',
    'If preference-sustained, the authored suppression overstates coercive content and the arrangement is closer to a stable equilibrium; if enforcement-sustained, removal of the machinery predicts rapid restructuring and the snare-side component of the hybrid is larger than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependence, empirical, 'Whether persistence runs through preference or through active enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfk_georgist_tr_t1950, price_formation_kernel__georgist_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(pfk_georgist_tr_t1950, observed).
narrative_ontology:measurement(pfk_georgist_tr_t1965, price_formation_kernel__georgist_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement_basis(pfk_georgist_tr_t1965, observed).
narrative_ontology:measurement(pfk_georgist_tr_t1980, price_formation_kernel__georgist_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement_basis(pfk_georgist_tr_t1980, observed).
narrative_ontology:measurement(pfk_georgist_tr_t1995, price_formation_kernel__georgist_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(pfk_georgist_tr_t1995, observed).
narrative_ontology:measurement(pfk_georgist_tr_t2010, price_formation_kernel__georgist_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(pfk_georgist_tr_t2010, observed).
narrative_ontology:measurement(pfk_georgist_tr_t2025, price_formation_kernel__georgist_reading, theater_ratio, 2025, 0.25).
narrative_ontology:measurement_basis(pfk_georgist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(pfk_georgist_be_t1950, price_formation_kernel__georgist_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement_basis(pfk_georgist_be_t1950, observed).
narrative_ontology:measurement(pfk_georgist_be_t1965, price_formation_kernel__georgist_reading, base_extractiveness, 1965, 0.47).
narrative_ontology:measurement_basis(pfk_georgist_be_t1965, observed).
narrative_ontology:measurement(pfk_georgist_be_t1980, price_formation_kernel__georgist_reading, base_extractiveness, 1980, 0.53).
narrative_ontology:measurement_basis(pfk_georgist_be_t1980, observed).
narrative_ontology:measurement(pfk_georgist_be_t1995, price_formation_kernel__georgist_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement_basis(pfk_georgist_be_t1995, observed).
narrative_ontology:measurement(pfk_georgist_be_t2010, price_formation_kernel__georgist_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement_basis(pfk_georgist_be_t2010, observed).
narrative_ontology:measurement(pfk_georgist_be_t2025, price_formation_kernel__georgist_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(pfk_georgist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(pfk_georgist_su_t1950, price_formation_kernel__georgist_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement_basis(pfk_georgist_su_t1950, observed).
narrative_ontology:measurement(pfk_georgist_su_t1965, price_formation_kernel__georgist_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement_basis(pfk_georgist_su_t1965, observed).
narrative_ontology:measurement(pfk_georgist_su_t1980, price_formation_kernel__georgist_reading, suppression_requirement, 1980, 0.43).
narrative_ontology:measurement_basis(pfk_georgist_su_t1980, observed).
narrative_ontology:measurement(pfk_georgist_su_t1995, price_formation_kernel__georgist_reading, suppression_requirement, 1995, 0.46).
narrative_ontology:measurement_basis(pfk_georgist_su_t1995, observed).
narrative_ontology:measurement(pfk_georgist_su_t2010, price_formation_kernel__georgist_reading, suppression_requirement, 2010, 0.49).
narrative_ontology:measurement_basis(pfk_georgist_su_t2010, observed).
narrative_ontology:measurement(pfk_georgist_su_t2025, price_formation_kernel__georgist_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(pfk_georgist_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how housing prices form' covers four structurally distinct claims held as rival readings of one kernel. This file instantiates the georgist_reading (factor decomposition: unearned site rent vs earned improvement value, epsilon 0.72 over the standing arrangement). The naturalist_reading treats formation as natural equilibrium (epsilon near zero by its lights); the institutional_reading attributes formation to zoning, lending standards, and tax treatment; the financialization_reading attributes it to credit expansion and asset-price feedback. The Georgist reading upstream-influences the institutional reading because its rent-separation apparatus is the measurement tool institutional reform proposals deploy, while coexisting with the other two as live rival positions. Each file carries its own epsilon, beneficiaries, and victims; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
