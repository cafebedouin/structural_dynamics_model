% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Institutional Construction of Housing Price Formation
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   This constraint instantiates the institutional reading of the
 *   price_formation_kernel: housing prices are not natural scarcity
 *   equilibria but are actively constructed by zoning codes, mortgage lending
 *   standards, tax expenditures favoring ownership, and digital intermediary
 *   platforms. The constraint is a tangled rope because it retains a genuine
 *   coordination function (managing land-use externalities, credit risk,
 *   municipal finance) while simultaneously operating as an extraction
 *   mechanism that transfers wealth from entrants to incumbents and financial
 *   intermediaries. It is one reading of a contested kernel; sibling readings
 *   include naturalist (equilibrium/scarcity), georgist (land rent), and
 *   financialization (credit-asset feedback loops).
 *
 * KEY AGENTS:
 *   - Incumbent homeowners (organized/regional): Primary beneficiaries who capture appreciation and tax subsidies while enforcing scarcity through local political opposition.
 *   - Mortgage lenders (institutional/global): Beneficiaries who earn on inflated collateral values and restricted credit alternatives.
 *   - Real estate intermediaries (institutional/national): Agenda-setters who control listing infrastructure and extract fees proportionally to constructed prices.
 *   - Municipal governments (institutional/local): Agenda-setters who write zoning and tax rules but are politically constrained by incumbent constituencies.
 *   - Renters (powerless/local): Primary payers facing extraction through cost burdens with constrained exit.
 *   - First-time buyers (moderate/regional): Secondary payers locked out by entry barriers and skewed tax treatment.
 *   - Affordable housing advocates (organized/national): Excluded voices barred from standard-setting forums.
 *   - Housing market analysts (analytical/national): Observers documenting the institutional wedge between cost and price.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.82).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Institutional Construction of Housing Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '503b6943-6c79-40fe-b41f-8ec5dd98d148').
narrative_ontology:cs_kernel_codification('503b6943-6c79-40fe-b41f-8ec5dd98d148', formalized).
narrative_ontology:cs_authority_grounding('503b6943-6c79-40fe-b41f-8ec5dd98d148', extraction).
narrative_ontology:cs_interpretation_layer_present('503b6943-6c79-40fe-b41f-8ec5dd98d148').
narrative_ontology:cs_reading_relation('503b6943-6c79-40fe-b41f-8ec5dd98d148', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('503b6943-6c79-40fe-b41f-8ec5dd98d148', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('503b6943-6c79-40fe-b41f-8ec5dd98d148', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('503b6943-6c79-40fe-b41f-8ec5dd98d148', foundational, housing_market_outcomes_are_policy_outputs).
narrative_ontology:cs_axiom_status(housing_market_outcomes_are_policy_outputs, holdable).
narrative_ontology:cs_axiom_grounding('503b6943-6c79-40fe-b41f-8ec5dd98d148', housing_market_outcomes_are_policy_outputs, conventional).
narrative_ontology:cs_axiom('503b6943-6c79-40fe-b41f-8ec5dd98d148', foundational, land_use_regulation_constitutes_price).
narrative_ontology:cs_axiom_status(land_use_regulation_constitutes_price, holdable).
narrative_ontology:cs_axiom_grounding('503b6943-6c79-40fe-b41f-8ec5dd98d148', land_use_regulation_constitutes_price, conventional).
narrative_ontology:cs_reference_frame('503b6943-6c79-40fe-b41f-8ec5dd98d148', regulated_property_market).
narrative_ontology:cs_drift_state('503b6943-6c79-40fe-b41f-8ec5dd98d148', contemporary_affordability_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('503b6943-6c79-40fe-b41f-8ec5dd98d148', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, mortgage_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_buyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold properties in supply-constrained jurisdictions where zoning and tax treatment inflate values. They receive unrealized capital gains and homeowner tax subsidies. Their political opposition to upzoning and dense development preserves the scarcity that sustains price appreciation. They can exit by selling into the same inflated market, capturing the gain.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    organized, generational, mobile, regional).

% Originate and securitize mortgages against collateral whose nominal value is inflated by zoning scarcity and favorable tax treatment. They earn interest and fee income on larger loan balances. They benefit from lending standards that restrict alternative credit channels and from the systemic treatment of housing debt as low-risk.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, mortgage_lenders, beneficiary,
    institutional, generational, arbitrage, global).

% Operate listing platforms and broker networks that set commission structures, listing standards, and data-access rules. They extract fees proportional to transaction prices and control the information infrastructure of the market. Their rules shape how prices are discovered and reported, reinforcing the institutional construction of value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary).

% Write and enforce zoning codes, property-tax assessments, and development ordinances that directly construct the scarcity and fiscal incentives driving price formation. They are politically constrained by incumbent homeowners and rely on property-tax revenues from high values, creating a structural alignment with the beneficiary class even when they nominally regulate in the public interest.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_governments, agenda_setter,
    institutional, generational, constrained, local).

% Pay rents set in markets where institutional scarcity prevents adequate supply expansion. They bear the extraction directly through housing-cost burdens but lack the political capital to alter zoning or tax structures. Their primary exit options are displacement to lower-cost jurisdictions or overcrowding.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Face entry prices inflated by zoning scarcity, lending standards favoring existing collateral, and tax treatment skewed toward incumbents. They must accumulate larger down payments and accept higher leverage to access the same housing stock. Their exit from the constraint is to remain in the rental market indefinitely or exit homeownership aspiration entirely.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_buyers, payer,
    moderate, biographical, constrained, regional).

% Advocate for social housing, inclusionary zoning, and tenant protections. They are structurally excluded from zoning board deliberations and standard-setting bodies where price-constructing rules are written. Their proposals are regularly diluted or blocked by incumbent-dominated political processes.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, affordable_housing_advocates, excluded,
    organized, biographical, constrained, national).

% Study and model housing markets, often documenting the divergence between construction costs and sale prices attributable to regulatory and tax structures. They observe the institutional machinery but do not collect from or pay into the constraint directly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_market_analysts, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocating scarce urban land and housing credit while managing density externalities, credit default risk, and municipal fiscal capacity through formal rules and intermediary infrastructure.
% TRANSFER_FUNCTION: Moves wealth from housing entrants (renters and first-time buyers) to incumbent owners, mortgage lenders, and intermediary platforms through institutionally maintained scarcity and transaction-cost extraction.
% ABSENT_VOICES: Renters and first-time buyers have limited political voice in zoning and tax politics; Georgist land-value tax advocates and decommodification advocates are structurally excluded from mainstream policy forums where price-constructing rules are ratified.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax treatment, and platform rules vanished, land values would immediately reprice, credit channels would collapse and reorganize around unregulated signals, and urban development patterns would shift away from the current scarcity-driven model; the housing market as currently constituted would cease to exist.
% FOUNDING_PROBLEM: Managing incompatible land uses in dense urban settlements, ensuring a stable housing credit system, and enabling municipal governments to fund services through property taxation.
% FOUNDING_PROBLEM_CORROBORATION: Urban historians and public-finance scholars corroborate the original land-use and fiscal coordination problems. Incumbent homeowners and mortgage lenders attest that the framework remains necessary for market stability. Affordable housing researchers, tenant unions, and Georgist political economists from outside the benefiting parties contest that the current configuration still addresses the founding problem, arguing it has inverted into a mechanism of wealth extraction and exclusion.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is moderate-high because the constraint structurally moves substantial wealth via artificial scarcity maintained by zoning and lending gatekeeping. Suppression (0.82) is higher because the arrangement requires continuous active enforcement: zoning hearings, lending underwriting, platform exclusion of off-market transactions, and tax-code maintenance. Theater ratio (0.55) reflects growing performative maintenanceâzoning for 'neighborhood character,' complex lending 'innovations,' and platform rituals that defend scarcity rather than solve founding coordination problems. Accessibility collapse (0.65) captures the foreclosure of alternatives such as social housing, self-build, and unregulated land use, though informal markets persist at the margins. Resistance (0.60) reflects active YIMBY movements, tenant organizing, and sporadic regulatory reform attempts.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent homeowner and lender seats compute the constraint as coordination-plus-entitlement: they experience the arrangement as protecting legitimate investment and community stability. The renter and first-time buyer seats compute it as extraction: they experience the same rules as barriers to entry and wealth siphoning. The engine produces this divergence from the same structural data because the beneficiary declarations push directionality toward subsidy for incumbents while the victim declarations push directionality toward extraction for entrants.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, mortgage lenders, and real estate intermediaries are declared beneficiaries; their structural relationship to the constraint is subsidizing (low d, damped effective extraction). Renters and first-time buyers are declared victims; their structural relationship is targeting (high d, amplified effective extraction). Municipal governments sit in a mixed agenda-setter position without explicit beneficiary or victim declarations, so their directionality reverts to the institutional power-atom fallbackâmoderately low d because they administer the constraint rather than pay into it. Analysts are analytical and generate no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by its R5 genealogy: it was built to solve land-use conflicts, credit instability, and municipal fiscal capacity. That founding problem is contestedâsome argue it is still live (genuine externalities persist), while others argue it has been captured. The temporal measurements show extraction and theater rising over the interval while the founding coordination function has not scaled proportionally, suggesting mandatrophy drift. The tangled_rope classification captures this precisely: it is not a pure snare because some coordination residue remains, but it is not a rope because asymmetric extraction is structurally necessary to its current operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the institutional reading of price formation exhaust the phenomenon, or do the financialization reading (credit-asset feedback loops) and georgist reading (land rent) represent structurally distinct constraints with independent epsilon values?',
    'Decompose cross-jurisdictional price variance into regulatory restriction indices, credit expansion metrics, and land-value components; if any sibling reading''s core variable explains price dynamics independently of institutional rules, split the kernel per epsilon-invariance.',
    'If siblings are independent, the institutional reading''s extractiveness is overstated and the kernel decomposes into a constraint family; if institutional variables subsume the siblings, the reading stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading is epsilon-invariant or must split into a constraint family').

omega_variable(
    founding_problem_obsolescence,
    'Have zoning, lending standards, and tax treatment transitioned from solving genuine coordination problems (land-use externalities, credit risk, municipal finance) to primarily protecting incumbent asset values?',
    'Compare jurisdictions with relaxed zoning and neutral tax treatment against restrictive jurisdictions on metrics of construction volume, price stability, and wealth concentration.',
    'If the founding problem is dead, the constraint drifts toward snare or piton classification; if live, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the institutional framework has outlived its founding coordination function').

omega_variable(
    beneficiary_concentration,
    'Is the extraction from renters and first-time buyers captured by concentrated beneficiaries (lenders, platforms, incumbents) or diffused across a broad homeowner class?',
    'Measure wealth accumulation rates by income and tenure status; trace mortgage origination profits and platform fee revenue to specific seats.',
    'Concentrated capture supports snare classification; broad diffusion with genuine coordination residue supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Whether extraction is concentrated or diffuse across beneficiary seats').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_inst_tr_t0, price_formation_kernel__institutional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(price_inst_tr_t8, price_formation_kernel__institutional_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(price_inst_tr_t16, price_formation_kernel__institutional_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(price_inst_tr_t24, price_formation_kernel__institutional_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(price_inst_tr_t32, price_formation_kernel__institutional_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(price_inst_tr_t40, price_formation_kernel__institutional_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(price_inst_be_t0, price_formation_kernel__institutional_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(price_inst_be_t8, price_formation_kernel__institutional_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(price_inst_be_t16, price_formation_kernel__institutional_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(price_inst_be_t24, price_formation_kernel__institutional_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(price_inst_be_t32, price_formation_kernel__institutional_reading, base_extractiveness, 32, 0.72).
narrative_ontology:measurement(price_inst_be_t40, price_formation_kernel__institutional_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(price_inst_su_t0, price_formation_kernel__institutional_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(price_inst_su_t8, price_formation_kernel__institutional_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(price_inst_su_t16, price_formation_kernel__institutional_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(price_inst_su_t24, price_formation_kernel__institutional_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(price_inst_su_t32, price_formation_kernel__institutional_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(price_inst_su_t40, price_formation_kernel__institutional_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the institutional reading of the price_formation_kernel, decomposed from naturalist, georgist, and financialization readings per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
