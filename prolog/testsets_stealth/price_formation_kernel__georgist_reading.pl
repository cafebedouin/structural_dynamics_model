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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Private Capture of Location Rent in Property Price Formation (Georgist Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This story instantiates the georgist_reading of the
 *   price_formation_kernel. The standing arrangement under contest is market
 *   price formation in which site value and improvement value are transacted
 *   as a single bundle, with the site component — location rent — privately
 *   captured by titleholders. By this reading's own lights, the bundle
 *   conceals a component created by community density, public infrastructure,
 *   and neighborhood investment rather than by the holder, collected as
 *   periodic rent and realized appreciation, alongside a genuine earned
 *   component reflecting labor and capital. Per the ε-referent rule,
 *   extractiveness is authored FOR the standing bundled-capture arrangement
 *   as this reading assesses it — never for the land-value-taxed alternative
 *   the reading endorses, which would trivially measure zero. The
 *   claim/metric independence rule is respected: the claimed type is stated
 *   from the reading's structural analysis, the metrics from the reading's
 *   descriptive assessment, and the engine computes per-seat classifications
 *   independently. This file is one member of a four-story constraint family;
 *   the sibling readings (naturalist, institutional, financialization) are
 *   separate constraints with their own ε values, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - urban_landowners: Primary beneficiary (powerful/arbitrage) — collects site rent and appreciation created by surrounding development; politically organizes to defend the arrangement
 *   - - mortgage_lenders: Secondary beneficiary (institutional/arbitrage) — earns interest on credit collateralized by capitalized site value
 *   - - urban_tenants: Primary target (powerless/constrained) — pays location premiums month to month with weak individual bargaining power
 *   - - aspiring_homebuyers: Target (moderate/constrained) — priced out; buying capitalizes the rent, renting pays it serially
 *   - - wage_earners: Target (organized/mobile) — location costs absorb part of productivity gains
 *   - - location_dependent_businesses: Target (moderate/constrained) — site costs are fixed overhead decoupled from performance
 *   - - property_law_enforcement_apparatus: Agenda setter (institutional/constrained) — administers titles, evictions, foreclosures; could restructure taxation but faces constitutional and reliance constraints
 *   - - land_value_tax_advocates: Excluded voice (organized/constrained) — holds a complete alternative program outside the legislative conversation
 *   - - independent_assessors: Analytical observer (institutional/analytical) — performs the site/improvement separation routinely, demonstrating its feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.74).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.6).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Private Capture of Location Rent in Property Price Formation (Georgist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "economic/political").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, 'b605ad5f-8032-4798-8288-4e88d4c50c8a').
narrative_ontology:cs_kernel_codification('b605ad5f-8032-4798-8288-4e88d4c50c8a', distributed).
narrative_ontology:cs_authority_grounding('b605ad5f-8032-4798-8288-4e88d4c50c8a', lineage).
narrative_ontology:cs_interpretation_layer_present('b605ad5f-8032-4798-8288-4e88d4c50c8a').
narrative_ontology:cs_reading_relation('b605ad5f-8032-4798-8288-4e88d4c50c8a', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b605ad5f-8032-4798-8288-4e88d4c50c8a', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('b605ad5f-8032-4798-8288-4e88d4c50c8a', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('b605ad5f-8032-4798-8288-4e88d4c50c8a', foundational, site_rent_is_socially_created_unearned_value).
narrative_ontology:cs_axiom_status(site_rent_is_socially_created_unearned_value, holdable).
narrative_ontology:cs_axiom_grounding('b605ad5f-8032-4798-8288-4e88d4c50c8a', site_rent_is_socially_created_unearned_value, empirically_contingent).
narrative_ontology:cs_axiom('b605ad5f-8032-4798-8288-4e88d4c50c8a', foundational, equal_claim_to_natural_opportunities).
narrative_ontology:cs_axiom_status(equal_claim_to_natural_opportunities, holdable).
narrative_ontology:cs_axiom_grounding('b605ad5f-8032-4798-8288-4e88d4c50c8a', equal_claim_to_natural_opportunities, deontological).
narrative_ontology:cs_axiom('b605ad5f-8032-4798-8288-4e88d4c50c8a', secondary, tax_rent_not_production).
narrative_ontology:cs_axiom_status(tax_rent_not_production, holdable).
narrative_ontology:cs_axiom_grounding('b605ad5f-8032-4798-8288-4e88d4c50c8a', tax_rent_not_production, instrumental).
narrative_ontology:cs_reference_frame('b605ad5f-8032-4798-8288-4e88d4c50c8a', classical_three_factor_decomposition).
narrative_ontology:cs_drift_state('b605ad5f-8032-4798-8288-4e88d4c50c8a', contemporary_neoclassical_synthesis, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b605ad5f-8032-4798-8288-4e88d4c50c8a', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, urban_landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, mortgage_lenders).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, urban_tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, aspiring_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, wage_earners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, location_dependent_businesses).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, ricardian_law_of_rent).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, unearned_increment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold titled sites in locations whose value was created by surrounding population density, public infrastructure, and neighborhood investment rather than by the titleholder's own effort. Collect periodic rent from occupants and realize appreciation on sale. Organize through property-owner associations to defend favorable tax treatment of site value. Selling converts the position to portable capital at will, so the personal cost of holding or releasing any particular parcel is low.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, urban_landowners, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, urban_landowners, agenda_setter).

% Extend credit secured by property, with the site component serving as the durable collateral. Rising site values expand the lendable collateral base and support larger loan books earning interest. Capital moves freely between lending, other asset classes, and jurisdictions, so exposure to any single property market is a portfolio choice.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mortgage_lenders, beneficiary,
    institutional, biographical, arbitrage, global).

% Pay monthly amounts set by competition for access to locations near work, schools, and services. Individual households have little bargaining power against owners and face moving costs, deposit requirements, and application screening if they leave. Alternative dwellings in the same metro charge comparable location premiums, so relocation within reach of employment rarely escapes the payment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, urban_tenants, payer,
    powerless, immediate, constrained, regional).

% Want to purchase but find that the site component of the asking price has risen faster than savings. Buying means servicing a mortgage whose principal capitalizes decades of anticipated location payments; continuing to rent means paying the same location premium month to month while prices climb further away. Delaying the decision has historically made entry harder, not easier.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, aspiring_homebuyers, payer,
    moderate, biographical, constrained, regional).

% Earn incomes that are bid against by the cost of occupying space in productive regions. Productivity gains from dense labor markets show up partly as higher site payments rather than sustained living-standard gains. Some bargaining power exists through unions and professional mobility, and relocation between labor markets is possible, though destination cities carry their own location premiums.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, wage_earners, payer,
    organized, biographical, mobile, national).

% Need sites near customers and workforce, so occupancy costs are fixed overhead that compresses margins regardless of business performance. Lease renewals reset with area-wide site values. Moving forfeits established customer proximity and staff catchments, and comparable sites in the same region carry similar premiums.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, location_dependent_businesses, payer,
    moderate, biographical, constrained, regional).

% Courts, title registries, municipal codes, and enforcement officers administer the arrangement: recording titles, executing evictions and foreclosures, and adjudicating possession disputes. Legislatures within the same apparatus set property-tax bases and could restructure how site value is taxed, but face constitutional limits, reliance interests of existing holders, and intense lobbying.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, property_law_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Maintain a developed policy program: shift public revenue onto site value and off production, using existing mass-appraisal techniques, with staged transition designs. They argue in hearings, publish in land economics journals, and contest elections, but remain marginal in legislative agendas dominated by incumbent property interests and short electoral horizons.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_value_tax_advocates, excluded,
    organized, generational, constrained, national).

% Mass-appraisal professionals already separate site value from improvement value for ad valorem purposes in most jurisdictions, using comparable-sales and residual methods. Their practice demonstrates that the analytical split this reading depends on is routinely performed, with quantified error bands, and their data is cited by every faction in the policy dispute.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, independent_assessors, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, urban_landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market price formation allocates scarce locations among competing uses, signals where development capacity is valuable, and, through titled ownership, makes parcels transactable and usable as loan collateral. Title systems solve boundary and possession disputes once, centrally, instead of per-occupant.
% TRANSFER_FUNCTION: Moves a recurring share of household and business income — and, upon sale, a capitalized lump sum — from occupants, buyers, and employers to titleholders, in exchange for permission to occupy locations whose value the surrounding community and public works largely created.
% ABSENT_VOICES: Future residents of growing cities bear the capitalized location payments embedded in today's prices but hold no seat. Tenants are numerous but unorganized per dwelling. Land-value-tax advocates hold a complete alternative program yet sit outside the legislative conversation. Small owner-occupiers, who both pay mortgages and receive appreciation, lack a forum where their divided position is represented.
% DISAPPEARANCE_RATIONALE: If private capture of site value vanished overnight, purchase prices would fall toward improvement replacement cost, occupant payments would drop to operating costs plus whatever successor arrangement replaced the rent stream, credit collateral would shrink abruptly, and the distribution of accumulated household wealth would shift decisively — the housing economy, public finance, and banking would all reorganize.
% FOUNDING_PROBLEM: Open-access land produced boundary conflict, insecure tenure, and underinvestment: nobody improves a plot they might lose, and possession disputes were settled by force. Exclusive titled ownership solved this by making parcels transactable, mortgageable, and worth improving.
% FOUNDING_PROBLEM_CORROBORATION: Tenure-security research in development economics and land-administration programs run by multilateral agencies corroborate that the founding problem remains live wherever titling is weak. Assessor professional bodies corroborate that site and improvement values are separately measurable. Historical and contemporary land-economics studies corroborate the unearned character of site appreciation. Landowner associations, notably, do not corroborate the unearned-character claim — they contest it — so the corroboration base sits outside the beneficiary set.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.74 at interval end) because the site component of price is set by scarcity of locations rather than by the holder's contribution, and its share of property value has risen secularly across the interval (the grid maps approximately onto 1985–2025). Suppression is 0.60: the arrangement rests on state-backed title enforcement, eviction and foreclosure machinery, and the political marginalization of alternatives — real coercive content, but short of the total closure seen in hard snares, since tenancy, cooperatives, and community land trusts remain lawful. Theater is moderate-low (0.30): the coordination functions (allocation, credit collateral, transaction clearing) are genuinely performed; the rising share is justificatory — homeownership-wealth narratives and provider framings that grow as the rent share grows. Accessibility_collapse is 0.38: alternatives persist and are known, so understanding the arrangement does not close the option set. Resistance is 0.55: tenant organizing, affordability movements, upzoning campaigns, and the LVT movement constitute recurring, partially effective opposition, with episodic coalitions across tenant and small-owner classes. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled downstream by directionality and scope in the engine's computation. Both tracked series run on one shared six-point grid; the engine samples the union. Cyclical booms and busts overlay the traced dynamic, but the grid samples the secular trend, not the cycle — the cycle is documented here as context, not as the measured object.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the landowner seat the arrangement presents as ordinary property income and voluntary exchange — a functioning market doing allocation and credit work, with the rent component experienced as deserved return on a purchased asset. From the tenant and buyer seats the same structure presents as a rising claim on income decoupled from anyone's production. The enforcement seat experiences neutral administration of settled law. The assessor seat treats the site/improvement separation as routine technique, neither contested nor mysterious. The engine derives these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end: urban_landowners collect the rent stream directly with liquid exit (selling converts the position to capital), and mortgage_lenders earn on the capitalized collateral with portfolio-level exit. Declared victims sit toward the target end: tenants, buyers, wage earners, and location-dependent businesses all pay the location premium with constrained or only partially effective exit. Two overrides correct derivations that would miss the story's specifics. First, moderate-power seats (aspiring_homebuyers, location_dependent_businesses): a derivation keyed to victim declarations alone would push d toward the full-target end (~0.9), but their exposure is substantial yet partial — the location premium is a budget share, not the whole of their economic life, and some hold offsetting assets — so d is pinned at 0.68. Second, the powerful seat (urban_landowners): a derivation crediting their management expenditure and tax payments as cost-bearing would blur a near-pure collection position; d is pinned at 0.06 to reflect that the class's defining flow is receipt, not payment. The powerless seat (tenants) needs no override — full-target derivation is descriptively right.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — open-access conflict, insecure tenure, unbankable parcels — remains live, so mandatrophy_resolved is not declared and the arrangement is not a piton candidate: its functions are performed, not performed-at. The classification danger runs in both directions. Reading the whole arrangement as pure extraction erases the real coordination (title systems, credit collateral, allocation signaling) that would survive any reform; reading it as pure coordination erases the asymmetric rent capture that motivates the reading. The tangled_rope claim holds both halves: genuine coordination function, active enforcement, identifiable beneficiaries and victims in the same structure. The temporal series guards the opposite failure — extraction accumulation — by showing the rent share rising steadily rather than holding at coordination-cost levels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the georgist_reading of price_formation_kernel; what would each sibling reading change structurally if adopted as the operative account?',
    'Cross-reading comparison of epsilon and computed type across the four family files: the naturalist reading would drive measured extraction toward coordination-cost levels (rent as natural return), the financialization reading would relocate the extraction locus to credit issuance, the institutional reading would relocate it to regulatory and tax constructs.',
    'Classification of the standing arrangement swings between rope-flavored (naturalist), snare-with-different-agents (financialization/institutional), and this file''s tangled_rope; the family comparison, not any single file, carries the verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one kernel, four readings, divergent extraction loci.').

omega_variable(
    land_improvement_separability,
    'Can site value be separated from improvement value accurately enough, at scale, to ground policy that treats the components differently?',
    'Assessment-error studies of mass appraisal in dense, mixed-use, and rapidly changing markets; comparison of assessed site residuals against transaction evidence where land sales occur separately.',
    'If separability holds within tolerable error, the rent component is administrably addressable and the extraction half of the structure is remediable; if error bands blow out in exactly the high-rent districts, the remedy fails in practice and the arrangement persists with its extraction intact behind technical impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_improvement_separability, empirical, 'Whether the reading''s central analytical operation survives contact with assessment practice.').

omega_variable(
    owner_occupier_net_position,
    'Are landowners a net-collecting class, or do owner-occupiers — who pay mortgages and taxes while receiving appreciation — wash the class position out?',
    'Distributional analysis splitting the landowning class by tenure and by holdings size: net site-income flows for rentier holders versus owner-occupiers, including imputed rent and unrealized appreciation.',
    'If the class is internally split, cross-class coalitions (small owners plus tenants) become the plausible reform path and the beneficiary bloc is weaker than its headline political weight; if rentier holdings dominate the flows, the arrangement is a straight capture structure with a concentrated beneficiary seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(owner_occupier_net_position, empirical, 'Internal heterogeneity of the beneficiary class and its consequence for coalition politics.').

omega_variable(
    capitalization_transition_incidence,
    'Site value is capitalized into outstanding mortgages and retirement portfolios; who should bear the windfall losses if the rent stream is redirected, and does that incidence question determine whether fixing is affordable?',
    'Transition-design analysis: phase-in schedules, grandfathering, and compensation schemes evaluated against realized-loss distributions across holder classes.',
    'If transition losses can be contained without compensating the rent stream itself, fixing is cheap relative to the recurring gain; if credible transition requires compensating capitalized expectations, fixing approaches prohibitive and the arrangement''s persistence is locked in by reliance interests rather than by ongoing defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capitalization_transition_incidence, preference, 'Whether the cost of fixing is a design choice or a structural lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__georgist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__georgist_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__georgist_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__georgist_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__georgist_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__georgist_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__georgist_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__georgist_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.74).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__georgist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'how housing prices form' covers four structurally distinct claims, each with its own epsilon, beneficiary/victim structure, and classification. This file authors the georgist composition claim (separable earned/unearned components, rent privately captured). The naturalist reading authors the equilibrium-process claim (low extraction by construction); the institutional reading authors the constructed-by-rules claim (extraction locus in regulatory and tax design); the financialization reading authors the credit-driven claim (extraction locus in lending). Upstream/downstream: the georgist decomposition supplies the site/improvement separation that institutional analyses presuppose, hence the influences edge; the naturalist and financialization readings are held as live rivals by different parties, hence coexists_with edges. No member of the family is orphaned; each links the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, moderate, 0.68).
constraint_indexing:directionality_override(price_formation_kernel__georgist_reading, powerful, 0.06).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
