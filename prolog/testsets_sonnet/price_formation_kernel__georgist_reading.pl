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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Land Rent Extraction Within Price Formation (Georgist Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the Georgist reading of the
 *   price_formation_kernel: within any observed property price, the land-rent
 *   component (site value, arising from location, public investment, and
 *   agglomeration effects) is structurally distinct from the improvement
 *   component (earned through labor and capital investment in construction
 *   and maintenance). The land component behaves like a Mountain at the
 *   physical level — locations are fixed, non-reproducible, and scarcity is
 *   real and not created by any single actor. But the CAPTURE of that
 *   scarcity value by private titleholders, enforced through property law and
 *   tax systems that under-tax land relative to improvements, is a
 *   Snare/Tangled-Rope structure: labor and tenants who generate the
 *   underlying demand and locational value receive no share of the rent,
 *   while landowners collect it passively. The improvement component
 *   genuinely coordinates production (labor, capital, materials) and
 *   functions as a Rope. This story evaluates the COMPOSITE — the bundled
 *   market institution as it actually prices property — as a Tangled Rope: it
 *   does coordinate a real transfer of housing services (the rope function)
 *   but is sustained by asymmetric extraction of unearned land value from
 *   those who cannot avoid paying for locational access (the snare function),
 *   and this requires active enforcement (property law, title registries,
 *   mortgage systems, tax codes that decline to distinguish land from
 *   improvement).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Land Rent Extraction Within Price Formation (Georgist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b').
narrative_ontology:cs_kernel_codification('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', distributed).
narrative_ontology:cs_authority_grounding('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', distributed).
narrative_ontology:cs_reading_relation('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', foundational, land_rent_is_unearned_community_created_value).
narrative_ontology:cs_axiom_status(land_rent_is_unearned_community_created_value, holdable).
narrative_ontology:cs_axiom_grounding('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', land_rent_is_unearned_community_created_value, empirically_contingent).
narrative_ontology:cs_axiom('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', secondary, improvement_value_is_labor_capital_earned).
narrative_ontology:cs_axiom_status(improvement_value_is_labor_capital_earned, holdable).
narrative_ontology:cs_axiom_grounding('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', improvement_value_is_labor_capital_earned, empirically_contingent).
narrative_ontology:cs_reference_frame('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', classical_political_economy_rent_theory).
narrative_ontology:cs_drift_state('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', contemporary_bundled_property_taxation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bfa8ec9-ff67-4612-bc6e-040bfc1ff54b', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, land_speculators).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, mortgage_lenders_on_land_value).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, wage_laborers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, productive_improvers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, productive_improvers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold title to land whose price reflects locational scarcity, proximity to infrastructure, and community-generated demand they did not create. Collect rising site value as unearned income through sale, lease, or mortgage equity, with legal title enforced by the state regardless of whether they improve the parcel. Can hold vacant or underused land indefinitely (land banking) with no penalty beyond opportunity cost, since the tax system falls mainly on improvements rather than land value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, beneficiary,
    organized, generational, arbitrage, regional).

% Purchase land in anticipation of location-value appreciation driven by public investment (transit, schools, zoning changes) they did not fund. Lobby against land value taxation and for preferential capital-gains and property-tax treatment that keeps rent capture cheap. Exit freely by selling into the next buyer; the constraint's persistence is partly their doing through political influence over assessment and tax rules.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_speculators, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, land_speculators, agenda_setter).

% Extend credit collateralized substantially by land value rather than structure value, capturing interest income on financed rent. Benefit from rising land prices as they expand lending capacity and collateral coverage; face no structural cost from land banking or vacant-lot speculation since the loan is secured either way.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, mortgage_lenders_on_land_value, beneficiary,
    institutional, generational, arbitrage, national).

% Pay rent that embeds the site-value premium of a location they did not create and cannot avoid without leaving the region's job and social network entirely. Rent increases track land value appreciation more than the landlord's cost of maintaining the structure. Exit means relocation, often at high personal and economic cost, not escape from the mechanism.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, immediate, trapped, local).

% Produce the economic activity, infrastructure demand, and population growth that raises surrounding land values, then must pay a rising share of wages back to landowners as rent or purchase price to remain near employment centers. Their labor is what generates the location value landowners capture; they see none of that value as its origin.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, wage_laborers, payer,
    powerless, biographical, constrained, national).

% Must purchase land component and improvement component bundled together at prices where land has appreciated independent of any labor or investment by the seller. Priced out by land-value inflation even where construction costs are stable; financing terms further capitalize expected future land appreciation into the purchase price they must pay today.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, first_time_homebuyers, payer,
    moderate, biographical, constrained, regional).

% Builders, developers, and homeowners who add genuine structural value through construction, renovation, and maintenance. Their earned improvement value is taxed and priced in the same market transaction as the unearned land component, and property tax systems that assess land and structure together can penalize improvement (discouraging development) while barely touching idle land.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, productive_improvers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, productive_improvers, beneficiary).

% Administer property tax systems that could, in principle, separate land value from improvement value for assessment purposes (split-rate or land value taxation) but in most jurisdictions tax bundled property value, effectively subsidizing land holding over improvement. Have the technical capacity to change this but face political resistance from organized landowner and speculator interests.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, municipal_tax_assessors, agenda_setter,
    institutional, generational, constrained, local).

% Economists and reformers who argue the land rent component should be publicly captured (via land value tax) since it derives from community and locational factors, not landowner labor. Present in policy debate but rarely control the assessment or tax-rate levers directly; their proposals are enacted only in isolated jurisdictions.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_land_value_tax_advocates, excluded,
    moderate, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bundled land-and-improvement pricing allows a single market transaction to transfer a developed parcel, coordinating the transfer of both physical structure and locational access in one exchange rather than requiring separate markets for site rights and buildings.
% TRANSFER_FUNCTION: Moves unearned locational value — created by public investment, agglomeration, and community activity — from tenants, wage laborers, and buyers to titleholders of land, via rent, sale price, and mortgage interest, disguised within a single 'property price' that also includes genuinely earned improvement value.
% ABSENT_VOICES: Land value tax advocates and the broader public whose collective activity generates site value are structurally absent from price-setting; they benefit from no share of the rent they help create and have no seat in how land is assessed or taxed relative to improvements.
% DISAPPEARANCE_RATIONALE: If land rent capture were separated out and publicly recovered (the Georgist reform), land prices would fall toward pure-improvement value, land banking would become costly rather than profitable, development incentives would shift toward use rather than speculation, and a substantial wealth transfer currently running from labor and tenants to titleholders would cease — housing markets, municipal finance, and speculative land investment would all reorganize.
% FOUNDING_PROBLEM: Land title and land markets emerged to allocate scarce, non-reproducible locations to productive use and to secure investment in improvements built upon them.
% FOUNDING_PROBLEM_CORROBORATION: Landowners and industry groups attest the current bundled system correctly rewards ownership risk and investment. Independent public-finance economists (e.g., land value tax literature going back to Henry George and revived in contemporary urban economics) and municipal assessors who have implemented split-rate taxation in isolated cases attest that the land component specifically reflects community-generated value with no landowner contribution, corroborating the extraction reading from outside the beneficiary group.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantial and rising share of property price attributable to pure locational scarcity rather than improvement cost, especially in supply-constrained urban markets. Suppression (0.55) is moderate: there is no legal barrier to alternative land-tenure arrangements (community land trusts, land value taxation exist and are legally permitted), but political economy — landowner lobbying, assessment practices, and the difficulty of unbundling land from improvement in a single transaction — makes the extractive default hard to dislodge. Theater ratio (0.40) captures the substantial gap between the stated justification for property taxation (funding public services fairly) and its actual incidence (falling more heavily on improvements than on the land value it nominally taxes alongside). Accessibility collapse (0.62) is significant because once a region urbanizes, alternatives to paying the land-rent premium for access (moving elsewhere, building your own city) become vanishingly practical for an individual. Resistance (0.58) reflects organized Georgist, urbanist, and tenant advocacy that persistently proposes land value tax reforms, even though it rarely succeeds against organized landowner interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners, speculators, and land-value-collateralized lenders sit at the beneficiary end: they collect value they did not produce, with high exit options (arbitrage — sell, refinance, hold) because their asset is liquid and their capital is mobile. Tenants and wage laborers sit at the target end: trapped or constrained exit, because leaving a high-rent region means leaving jobs, schools, and social networks, and their labor is precisely what generates the land value being extracted from them. Productive improvers occupy an intermediate position — they both pay into the bundled price (as buyers of land-inclusive real estate) and benefit somewhat as producers of genuine value, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating scarce locations to productive use and rewarding investment in improvements — remains partly live (scarce locations still need allocating, improvements still need incentivizing) but the CURRENT mechanism for doing so has drifted from that founding function: it now also serves as a vehicle for capturing publicly-generated value privately, a function the founding problem never called for. This is the specific mandatrophy this reading identifies: distinguishing the still-live coordination function (rope) from the drifted extraction function (snare) prevents the whole institution from being mislabeled either as pure natural necessity (naturalist reading's error) or as pure conspiracy (which would miss the real coordination the land/improvement bundle also performs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_improvement_decomposability,
    'Can land value and improvement value be cleanly separated in practice, or is the decomposition itself theory-laden and contestable at the margins (e.g., does a subway extension count as a ''location'' improvement or a public ''improvement'' investment)?',
    'Comparison of jurisdictions that implement split-rate or full land value taxation (e.g., parts of Pennsylvania, Estonia, Australian states) against assessment accuracy studies and hedonic pricing models that attempt to isolate site value econometrically.',
    'If land value cannot be reliably separated from improvement value in practice, the Georgist reading''s policy prescription (land value tax) becomes harder to implement even if the underlying moral/structural claim is correct — this would not refute the reading but would raise its administrative cost, potentially explaining part of the institutional_reading''s persistence as a rival account of why bundled taxation continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_improvement_decomposability, empirical, 'Whether land rent is practically separable from improvement value for assessment and policy purposes.').

omega_variable(
    unearned_increment_causal_attribution,
    'Is rising land value genuinely attributable to community/public factors (infrastructure, agglomeration, population growth) rather than to the landowner''s own risk-bearing and holding-period investment?',
    'Natural experiments isolating land value changes from public investment events (transit line openings, zoning upzones) versus land value changes attributable to owner-initiated actions (site preparation, environmental remediation, assembly of parcels).',
    'If a meaningful share of land value appreciation is attributable to owner actions (site assembly, remediation, risk-bearing during the holding period), the beneficiary/victim framing in this story overstates the ''unearned'' character of land rent and some landowner activity should be reclassified as improvement-adjacent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unearned_increment_causal_attribution, conceptual, 'Whether land value appreciation is attributable to community factors or to landowner activity.').

omega_variable(
    reading_selection_ambiguity,
    'Given that price formation is simultaneously subject to Georgist (land/improvement), naturalist (equilibrium), institutional (zoning/lending), and financialization (credit) dynamics, is the Georgist decomposition the PRIMARY structural driver of extraction, or one contributing factor among several that this story''s ε value overstates by isolating?',
    'Comparative variance decomposition: what share of observed price variation and distributional transfer is explained by land-rent capture versus credit expansion versus zoning constraint, across multiple housing markets and time periods.',
    'If land-rent capture explains a smaller share of total price formation than credit/zoning factors in most markets, this reading''s extractiveness score may be locally accurate for land-rich, credit-stable markets but not generalizable — supporting the ε-invariance principle''s requirement that each reading be evaluated as its own constraint rather than as competing full explanations of the same price.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_ambiguity, conceptual, 'Whether the Georgist decomposition is the dominant or merely a contributing structural account of price formation, relative to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__georgist_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__georgist_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__georgist_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__georgist_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__georgist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__georgist_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__georgist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__georgist_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__georgist_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__georgist_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__georgist_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__georgist_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the price_formation_kernel, each instantiating a structurally distinct claim about how housing/land prices form and who bears the resulting costs. The naturalist_reading treats the price as a natural equilibrium output with no distributional decomposition (closest to a Mountain claim at the aggregate level, contestable as an FSM candidate given landowner beneficiaries). The institutional_reading attributes price formation to constructed rules (zoning, lending standards, tax treatment) rather than to any natural or distributional-composition claim. The financialization_reading attributes price DYNAMICS to credit expansion and asset-feedback loops. This georgist_reading is orthogonal to all three: it makes a claim about the DISTRIBUTIONAL COMPOSITION of whatever price forms — land-rent vs. improvement-value — regardless of what sets the price level. Each reading has its own ε and its own beneficiary/victim structure; they are linked here because policy or empirical work on one (e.g., a land value tax reform under the georgist_reading) would shift resource availability and legitimacy conditions for the institutional_reading's zoning/tax apparatus and for the financialization_reading's credit-collateral base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
