% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Institutionally Constructed Housing Price Formation (Zoning, Lending, Tax, Platform Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   In most developed housing markets, the price a buyer or renter faces is
 *   not the output of unconstrained supply meeting unconstrained demand: it
 *   is the output of local zoning ordinances that fix how much housing can
 *   legally exist in a given area, underwriting standards that determine who
 *   can access mortgage credit and on what terms, tax provisions (mortgage
 *   interest deductions, capital gains treatment, property tax assessment
 *   rules) that subsidize certain forms of tenure and asset-holding over
 *   others, and intermediary platforms whose commission structures are baked
 *   into every transaction. These four institutional layers jointly construct
 *   the price level and its distribution of winners and losers, and each
 *   layer is actively administered and defended by identifiable actors with a
 *   stake in its current configuration.
 *
 * KEY AGENTS:
 *   - incumbent_homeowners: primary beneficiary (organized/constrained) — collects scarcity-driven appreciation protected by zoning
 *   - mortgage_lenders: primary beneficiary (institutional/arbitrage) — sets underwriting terms, profits from origination and interest spread
 *   - real_estate_intermediary_platforms: beneficiary (powerful/arbitrage) — extracts transaction-fee rent regardless of price direction
 *   - municipal_tax_authorities: beneficiary/agenda_setter (institutional/constrained) — administers zoning, revenue-dependent on constrained supply
 *   - renters: primary target (powerless/trapped) — bears price effects of constrained supply with no hearing-room voice
 *   - first_time_buyers: target (moderate/constrained) — competes against wealth-advantaged incumbents for artificially scarce supply
 *   - informal_sector_workers_excluded_by_underwriting: target (powerless/trapped) — excluded from credit access by proxy criteria
 *   - housing_policy_researchers: analytical observer — documents the institutional construction empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.62).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.58).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutionally Constructed Housing Price Formation (Zoning, Lending, Tax, Platform Reading)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, 'dc168a87-6f58-474d-b20e-37f4c7606f0a').
narrative_ontology:cs_kernel_codification('dc168a87-6f58-474d-b20e-37f4c7606f0a', distributed).
narrative_ontology:cs_authority_grounding('dc168a87-6f58-474d-b20e-37f4c7606f0a', distributed).
narrative_ontology:cs_reading_relation('dc168a87-6f58-474d-b20e-37f4c7606f0a', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('dc168a87-6f58-474d-b20e-37f4c7606f0a', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('dc168a87-6f58-474d-b20e-37f4c7606f0a', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('dc168a87-6f58-474d-b20e-37f4c7606f0a', foundational, price_is_rule_constructed_not_equilibrium_discovered).
narrative_ontology:cs_axiom_status(price_is_rule_constructed_not_equilibrium_discovered, holdable).
narrative_ontology:cs_axiom_grounding('dc168a87-6f58-474d-b20e-37f4c7606f0a', price_is_rule_constructed_not_equilibrium_discovered, empirically_contingent).
narrative_ontology:cs_axiom('dc168a87-6f58-474d-b20e-37f4c7606f0a', secondary, administered_rules_have_identifiable_accountable_authors).
narrative_ontology:cs_axiom_status(administered_rules_have_identifiable_accountable_authors, holdable).
narrative_ontology:cs_axiom_grounding('dc168a87-6f58-474d-b20e-37f4c7606f0a', administered_rules_have_identifiable_accountable_authors, conventional).
narrative_ontology:cs_reference_frame('dc168a87-6f58-474d-b20e-37f4c7606f0a', institutional_rule_construction_baseline).
narrative_ontology:cs_drift_state('dc168a87-6f58-474d-b20e-37f4c7606f0a', contemporary_housing_affordability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc168a87-6f58-474d-b20e-37f4c7606f0a', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, municipal_tax_authorities).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, informal_sector_workers_excluded_by_underwriting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold appreciating assets protected by zoning that restricts new supply near them (single-family-only districts, height limits, minimum lot sizes) and by mortgage-interest and capital-gains tax treatment that subsidizes ownership over renting. They vote in local land-use hearings at disproportionate rates and organize to oppose upzoning that would erode their asset's scarcity premium. Their exit from the arrangement would mean voluntarily giving up appreciation they did not have to earn through improvement.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, generational, constrained, national).

% Set underwriting standards (credit score floors, debt-to-income ratios, down payment requirements) that determine who can access mortgage credit at all, and lobby for the tax and regulatory treatment (mortgage interest deduction, government-sponsored-enterprise guarantees) that makes debt-financed ownership the default path to housing. They profit from origination fees, interest spreads, and securitization regardless of whether the underlying supply constraint is loosened or tightened; they can move capital to other credit markets if housing lending sours.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, mortgage_lenders, agenda_setter).

% Operate the listing, brokerage-commission, and search infrastructure that both buyers and sellers must pass through to transact; commission structures and platform fees extract a percentage of every transaction regardless of the fairness of the underlying price. They have no stake in supply expansion or contraction — transaction volume and price level both generate revenue for them, and they can pivot to rental, commercial, or adjacent data markets if housing sales slow.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediary_platforms, beneficiary,
    powerful, biographical, arbitrage, national).

% Depend on property tax revenue tied to assessed home values and often on development impact fees; some jurisdictions set zoning explicitly to protect the tax base by restricting the multi-family or affordable development that would lower per-unit assessed values. They administer the zoning code and can rewrite it, but rewriting it threatens the revenue base their budgets already depend on.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_tax_authorities, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, municipal_tax_authorities, agenda_setter).

% Pay rents set within a housing stock artificially constrained by zoning and are excluded from ownership by lending standards calibrated to income and credit histories they often cannot meet. They have no seat in local land-use hearings that determine supply, no access to the mortgage-interest tax subsidy, and no ability to relocate cheaply given job and school ties — exit means leaving the metro area entirely, not switching housing arrangements.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, trapped, regional).

% Compete for a supply artificially limited by zoning against households with existing home equity or family wealth transfers, while underwriting standards impose down-payment and income thresholds that fall harder on those without inherited assets. Every year of delay means buying into a higher price set partly by the same incumbents' zoning preferences and partly by intermediary commission structures baked into the transaction price.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).

% Earn income that does not fit standard underwriting documentation (gig work, cash wages, irregular hours) and are excluded from mortgage credit almost entirely regardless of actual ability to pay, forcing permanent reliance on a rental market whose price level is itself set by the same constrained supply and lending regime. Exit would require formalizing income in ways their labor market does not offer.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, informal_sector_workers_excluded_by_underwriting, payer,
    powerless, biographical, trapped, regional).

% Study the price effects of zoning liberalization, lending standard changes, and platform commission structures across jurisdictions, producing the empirical record that lets the institutional construction of price be identified and compared against natural-equilibrium claims.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_policy_researchers, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized zoning categories, underwriting criteria, and property transaction infrastructure genuinely reduce information costs and default risk relative to an unregulated, uncoordinated land and credit market — buyers and lenders can rely on common categories and documented standards rather than negotiating trust from scratch each time.
% TRANSFER_FUNCTION: Moves scarcity rents from those excluded by zoning-constrained supply and underwriting thresholds (renters, first-time buyers, informally employed workers) to those positioned to hold appreciating assets, originate and service debt, or extract transaction fees (incumbent owners, lenders, intermediary platforms) — with municipal tax authorities capturing a share via assessed-value-linked revenue.
% ABSENT_VOICES: Renters are structurally absent from the zoning hearings that set supply constraints in the neighborhoods they live in — voting and hearing-attendance rates skew heavily toward owners. Informally employed workers have no representation in underwriting standard-setting bodies, which are shaped by lenders and regulators, not by those excluded from credit access.
% DISAPPEARANCE_RATIONALE: If zoning restrictions, current lending standards, tax treatment favoring ownership, and intermediary commission structures were simultaneously removed, housing supply would respond to demand at a different rate, credit access would broaden or tighten by different criteria, and the transaction-fee layer would likely compress or restructure — asset values held by incumbents would almost certainly fall, and the current distribution of who can access ownership would shift substantially. This is not a natural equilibrium quietly re-forming; specific institutional actors would need to rebuild replacement rules, and their interests are why the current configuration persists.
% FOUNDING_PROBLEM: Early 20th century zoning was built to address genuine externality problems (incompatible land uses, public health, infrastructure planning) and mortgage underwriting standards were built to reduce default risk in an opaque credit market lacking standardized information.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning departments and lender trade associations attest the founding problems (externality management, default risk) remain live and justify current rules. Independent housing economists and legal historians, publishing outside these institutions, document that zoning categories have been progressively rewritten toward exclusionary minimum-lot and single-family provisions well beyond what the original externality rationale requires, and that underwriting standards exclude creditworthy borrowers by proxy criteria correlated with race and informal employment rather than by demonstrated default risk — supporting a shifted-function reading in which the original coordination problem is substantially solved but the restrictive apparatus persists.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at moderate-to-high (0.62 by interval end) because the institutional layers are not neutral infrastructure but actively calibrated to favor incumbents and credit-privileged buyers over renters and credit-excluded workers — the coordination function (standardized zoning categories, underwriting documentation, transaction infrastructure) is genuine but has been progressively tightened past what the original externality/risk rationale requires. Suppression sits at 0.58: renters and excluded workers face real structural barriers (hearing access, documentation requirements) rather than mere inconvenience, but organized tenant movements and reform coalitions do mount active resistance, so suppression is not near-total. Theater ratio is moderate-low (0.31): most of the zoning and underwriting apparatus performs a genuine function, but a growing share of restrictive minimum-lot and exclusionary provisions serve no function beyond asset protection, which is the theatrical residue the rising trajectory tracks.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-owner and lender seats, the arrangement looks like Rope: zoning protects neighborhood character and investment value, underwriting protects credit-system stability, and everyone who follows the rules benefits. From the renter and excluded-worker seats, the identical rule-set operates as enforced extraction: supply is deliberately constrained, credit access is deliberately gated, and their price exposure is the mechanism's output, not a side effect. The engine computes this divergence from the structural power/exit asymmetry; the claimed_type of tangled_rope reflects that both the coordination function and the extraction are real and co-located in the same rule-set, which is exactly the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, lenders, intermediary platforms, and tax authorities sit near the beneficiary end of directionality: each collects value from the arrangement's current configuration (appreciation, interest spread, transaction fees, assessed-value tax base) and each has some capacity to shape the rules (voting in hearings, setting underwriting criteria, lobbying tax treatment, setting commission structures). Renters and credit-excluded informal workers sit near the full-target end: trapped exit options, no rule-setting access, and the price effects of the other four actors' rule-setting land on them directly. First-time buyers are intermediate — moderate power, constrained but not trapped exit — because they can sometimes access credit or relocate, but compete on unequal terms set by the same institutional layers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — externality management in zoning, default-risk reduction in underwriting — are largely solved by modern data and risk-modeling capacity, yet the restrictive apparatus (exclusionary zoning minimums, proxy-criteria underwriting) persists and has arguably intensified. Classifying this as tangled_rope rather than snare preserves the genuine remaining coordination value (some zoning categorization, some underwriting standardization, and platform transaction infrastructure are still functionally necessary) while flagging that the mandate has partially outlived the function it was built to serve — full snare classification would erase the real coordination residue; full rope classification would erase the asymmetric extraction that the mismatch between founding_problem_status (contested/largely dead) and disappearance_verdict (world_rearranges) reveals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_naturalist_price_attribution,
    'Is the observed price level and its distribution primarily attributable to the institutional rule-set (zoning, lending, tax, platforms) claimed here, or to an underlying scarcity/preference equilibrium that would produce similar prices under alternative institutional configurations (the naturalist_reading''s claim)?',
    'Natural experiments from jurisdictions that substantially deregulate zoning or loosen underwriting standards: if price and access outcomes converge toward the constrained baseline regardless of institutional configuration, the naturalist reading gains support; if outcomes diverge substantially and track the specific rule changes, the institutional reading is corroborated.',
    'If naturalist forces dominate, this constraint''s extraction estimate is overstated and closer to the ehrenfest-style near-mountain profile of the sibling naturalist_reading; if institutional forces dominate, the tangled_rope classification and current extraction level are well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_naturalist_price_attribution, empirical, 'Whether price formation is institutionally constructed or naturalistically emergent — the core kernel contest this reading takes a side on.').

omega_variable(
    institutional_georgist_overlap,
    'How much of the extraction attributed here to zoning/lending/tax/platform construction is actually a restatement of the georgist_reading''s land-rent-versus-improvement-value distinction, mediated through institutional mechanisms rather than caused independently by them?',
    'Decompose price appreciation in constrained-zoning jurisdictions into land-value versus structure-value components; if appreciation tracks land value specifically (as georgist theory predicts) rather than institutional rule tightness generally, the two readings substantially overlap rather than being independent claims.',
    'High overlap would suggest the institutional_reading is partly a proximate-cause restatement of the georgist_reading''s more fundamental land-rent claim, which would not change this story''s classification but would refine the network relationship between the two sibling constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_georgist_overlap, conceptual, 'Whether institutional and georgist readings identify the same underlying mechanism at different levels of abstraction.').

omega_variable(
    zoning_reform_beneficiary_capture,
    'Would municipal tax authorities and incumbent homeowners actively resist zoning liberalization even if it were revenue-neutral or revenue-positive at the jurisdiction level, indicating capture beyond the stated fiscal rationale?',
    'Compare voting and lobbying behavior in jurisdictions where zoning reform is modeled to be revenue-neutral versus revenue-negative; sustained resistance in the revenue-neutral case would indicate the fiscal rationale is a cover story for asset-protection motive.',
    'If resistance persists regardless of fiscal impact, the beneficiary structure is better characterized as asset-protection extraction than genuine fiscal coordination, strengthening the tangled_rope-toward-snare direction of drift documented in the rising extractiveness measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zoning_reform_beneficiary_capture, empirical, 'Whether the tax-base rationale for restrictive zoning is genuine coordination or cover for asset-value protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pric_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(pric_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(pric_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(pric_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement(pric_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(pric_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(pric_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(pric_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(pric_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(pric_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(pric_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(pric_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints instantiating the price_formation_kernel. naturalist_reading claims price emerges from objective scarcity/preference equilibrium (Mountain-leaning, minimal beneficiary structure). georgist_reading separates land rent (unearned) from improvement value (earned), implying a different victim/beneficiary partition centered on landholders versus laborers/capital-improvers. financialization_reading attributes price formation to credit-expansion feedback loops and housing-as-financial-asset demand, implicating a different beneficiary set (asset managers, leveraged investors) than the direct rule-administering institutions named here. This institutional_reading is narrower than all three: it claims specifically that zoning, underwriting, tax treatment, and platform commission structures are the causally operative construction, without asserting these subsume or are subsumed by credit dynamics or land-rent dynamics. Each story carries its own ε and its own stakeholder set; they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
