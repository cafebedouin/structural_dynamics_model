% ============================================================================
% CONSTRAINT STORY: property_sector_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_property_sector_overhang, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: property_sector_overhang
 *   human_readable: Property Sector Overhang and Local Government Fiscal Dependence
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   China's property sector, which grew to represent roughly 25% of GDP,
 *   entered sustained decline in 2021 following regulatory tightening on
 *   developer leverage. By 2025, property investment had contracted 17.2%
 *   year-over-year, creating cascading fiscal stress for local governments
 *   dependent on land sales and deflationary pressure despite monetary
 *   stimulus. The constraint coordinates against disorderly collapse while
 *   extracting fiscal autonomy from local governments and maintaining high
 *   property prices that burden prospective buyers. The central government
 *   benefits from enhanced control over credit allocation and the capacity to
 *   redirect investment toward strategic industries, while local governments
 *   and property developers bear the costs of deleveraging. The claimed type
 *   is tangled_rope reflecting the genuine coordination function (preventing
 *   systemic crisis) combined with asymmetric extraction (fiscal
 *   centralization, wealth transfer from young to existing homeowners,
 *   capital misallocation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(property_sector_overhang, 0.68).
domain_priors:suppression_score(property_sector_overhang, 0.72).
domain_priors:theater_ratio(property_sector_overhang, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(property_sector_overhang, extractiveness, 0.68).
narrative_ontology:constraint_metric(property_sector_overhang, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(property_sector_overhang, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(property_sector_overhang, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(property_sector_overhang, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(property_sector_overhang, tangled_rope).
narrative_ontology:human_readable(property_sector_overhang, "Property Sector Overhang and Local Government Fiscal Dependence").
narrative_ontology:topic_domain(property_sector_overhang, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(property_sector_overhang).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(property_sector_overhang, '325a4182-3b1d-47a0-a1ed-94de4f5d9d6f').
narrative_ontology:cs_kernel_codification('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', formalized).
narrative_ontology:cs_authority_grounding('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', lineage).
narrative_ontology:cs_interpretation_layer_present('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f').
narrative_ontology:cs_reading_relation('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', property_sector_overhang__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', property_sector_overhang__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', property_sector_overhang__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', foundational, growth_rate_legitimacy_floor).
narrative_ontology:cs_axiom_status(growth_rate_legitimacy_floor, holdable).
narrative_ontology:cs_axiom_grounding('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', growth_rate_legitimacy_floor, empirically_contingent).
narrative_ontology:cs_axiom('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', secondary, property_sector_growth_necessity).
narrative_ontology:cs_axiom_status(property_sector_growth_necessity, holdable).
narrative_ontology:cs_axiom_grounding('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', property_sector_growth_necessity, instrumental).
narrative_ontology:cs_reference_frame('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', quantitative_growth_primacy).
narrative_ontology:cs_drift_state('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', post_property_peak_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('325a4182-3b1d-47a0-a1ed-94de4f5d9d6f', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(property_sector_overhang, central_government_fiscal_control).
narrative_ontology:constraint_beneficiary(property_sector_overhang, state_owned_banks).
narrative_ontology:constraint_beneficiary(property_sector_overhang, urban_homeowners).
narrative_ontology:constraint_victim(property_sector_overhang, local_government_revenue_base).
narrative_ontology:constraint_victim(property_sector_overhang, property_developers).
narrative_ontology:constraint_victim(property_sector_overhang, prospective_homebuyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(property_sector_overhang, state_owned_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets monetary policy, credit allocation rules, and property market regulations. Maintains control over systemic financial risk by preventing disorderly property sector collapse while extracting fiscal discipline from local governments. Benefits from centralized authority over credit flows and the capacity to direct investment away from property toward strategic industries. Can adjust policy levers but faces legitimacy costs if either growth collapses or debt crisis materializes.
narrative_ontology:constraint_stakeholder(property_sector_overhang, central_government_fiscal_control, agenda_setter,
    institutional, generational, arbitrage, national).

% Dependent on land sales for 30-40% of fiscal revenue; property investment decline directly erodes their primary funding source. Must continue infrastructure spending and public service delivery while revenue contracts. Cannot exit the land-finance model without central authorization to raise alternative taxes. Bears the coordination cost of maintaining urban development while absorbing the extraction of reduced fiscal autonomy as central government tightens credit and restricts land-sale practices.
narrative_ontology:constraint_stakeholder(property_sector_overhang, local_government_revenue_base, payer,
    institutional, biographical, trapped, regional).

% Hold substantial property-related debt exposure but benefit from implicit state guarantees and directed lending authority. Coordinate credit allocation under central directives, gaining stability from state backing. Pay through non-performing loan accumulation and reduced profitability, but their systemic position is protected by the constraint's enforcement preventing disorderly collapse.
narrative_ontology:constraint_stakeholder(property_sector_overhang, state_owned_banks, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(property_sector_overhang, state_owned_banks, payer).

% Face declining sales, restricted credit access, and regulatory pressure to complete pre-sold units without new financing. Large developers have restructured or defaulted; smaller ones face insolvency. Exit options are constrained by sunk costs in land banks and incomplete projects. The constraint's enforcement maintains orderly deleveraging but extracts their capital base and market position.
narrative_ontology:constraint_stakeholder(property_sector_overhang, property_developers, payer,
    powerful, biographical, constrained, national).

% Benefit from policy measures preventing sharp property price collapse, which would destroy household wealth concentrated in real estate. The constraint coordinates against disorderly deflation that would wipe out their primary asset. Identity-locked because homeownership is fused with social status and family security; exit would mean accepting wealth destruction.
narrative_ontology:constraint_stakeholder(property_sector_overhang, urban_homeowners, beneficiary,
    organized, biographical, identity_locked, regional).

% Face continued high property prices relative to income despite sector distress, as policy prevents price discovery that would make housing affordable. Constrained exit: can delay purchase but face social pressure to own; renting lacks security and status. Pay through foregone consumption and high debt burdens when they do purchase.
narrative_ontology:constraint_stakeholder(property_sector_overhang, prospective_homebuyers, payer,
    moderate, biographical, constrained, regional).

% Largely excluded from direct property investment and increasingly wary of exposure to Chinese property debt. Would provide alternative capital if admitted but are kept out by capital controls and ownership restrictions. Their exclusion is structural to maintaining domestic policy control.
narrative_ontology:constraint_stakeholder(property_sector_overhang, foreign_investors, excluded,
    powerful, biographical, mobile, global).

% Monitor systemic risk from property sector overhang and local government debt. Publish assessments of financial stability and recommend policy adjustments. Have no enforcement power but their analysis shapes global investor sentiment and can amplify or dampen crisis expectations.
narrative_ontology:constraint_stakeholder(property_sector_overhang, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(property_sector_overhang, central_government_fiscal_control).
narrative_ontology:fixing_cost_class(property_sector_overhang, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents disorderly collapse of the property sector and local government finances that would trigger systemic financial crisis, mass unemployment, and social instability. Coordinates gradual deleveraging across developers, banks, and local governments while maintaining urban homeowner wealth.
% TRANSFER_FUNCTION: Extracts fiscal autonomy and revenue from local governments to central control; transfers risk from property developers and banks onto local government balance sheets and prospective homebuyers through sustained high prices and restricted credit; moves resources from consumption and alternative investment toward propping up existing property debt structures.
% ABSENT_VOICES: Prospective homebuyers seeking affordable housing and workers in non-property sectors who would benefit from reallocation of capital toward consumption or strategic industries are structurally underrepresented. Foreign capital that could provide alternative financing is excluded. Rural migrants who cannot access urban property markets are outside the coordination entirely.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, property prices would discover market-clearing levels (likely sharply lower), local governments would face immediate fiscal crisis and default on obligations, banks would recognize massive loan losses, urban homeowner wealth would collapse, but capital would reallocate toward consumption and non-property investment. The entire local government financing model and household wealth structure would reorganize.
% FOUNDING_PROBLEM: Post-1994 tax reform left local governments with expenditure responsibilities exceeding tax revenue, while rapid urbanization required massive infrastructure investment. Land sales became the primary financing mechanism, creating mutual dependence between local government fiscal capacity and property sector growth.
% FOUNDING_PROBLEM_CORROBORATION: Central government and local officials attest the fiscal-capacity gap remains live and land sales are still necessary for urban development. Independent economists and international observers attest the founding problem has transformed: the original infrastructure-financing need has been substantially met, and the arrangement now persists primarily to prevent recognition of accumulated debt and maintain existing wealth structures. Academic research and IMF assessments from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(property_sector_overhang, world_rearranges).
narrative_ontology:founding_problem_status(property_sector_overhang, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(property_sector_overhang, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2025-06-12',
    'uke_scope_china_property_overhang', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(property_sector_overhang, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(property_sector_overhang_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(property_sector_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(property_sector_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because the constraint maintains property prices above market-clearing levels, extracts fiscal capacity from local governments, and prevents capital reallocation that would benefit consumption and alternative sectors. Suppression is high (0.72) because the arrangement requires active enforcement through credit controls, capital account restrictions, regulatory pressure on developers, and prevention of local government fiscal alternatives. Theater ratio is moderate (0.41) and rising: an increasing share of policy activity is performative stimulus that cannot address the structural overhang, while the real function is managing orderly deleveraging and preventing recognition of losses. Accessibility collapse is moderate (0.58) because alternative development models exist but are politically foreclosed; resistance is substantial (0.64) from local governments seeking fiscal autonomy and prospective buyers demanding affordability.
 *
 * PERSPECTIVAL GAP:
 *   From the central government seat, the constraint is necessary macroprudential management preventing systemic crisis while enabling strategic reorientation toward high-quality development. From the local government seat, the same structure operates as fiscal extraction and loss of autonomy, forcing them to maintain services while their revenue base collapses. From the prospective homebuyer seat, it is a mechanism that prevents affordable housing from emerging despite sector distress. The engine computes these divergent classifications from the structural positions; the claimed tangled_rope type reflects the author's assessment that both coordination and extraction are genuinely present.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government fiscal control is the primary beneficiary (d near 0.2): gains enhanced authority over credit allocation, forces local government discipline, and can redirect resources toward strategic priorities. State-owned banks are mixed beneficiaries (d around 0.35): protected from disorderly collapse but accumulating non-performing loans. Urban homeowners are beneficiaries (d around 0.25): wealth preservation through price support, identity-locked into the asset. Local governments are primary targets (d near 0.85): trapped in revenue collapse with no exit, bearing the coordination cost while losing autonomy. Property developers are targets (d around 0.75): constrained exit, capital destruction, but some coordination benefit from orderly process. Prospective homebuyers are targets (d around 0.7): constrained by high prices and social pressure, paying through foregone consumption.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: the founding problem (local government infrastructure financing) has been substantially addressed through decades of urban development, but the arrangement persists because dismantling it would require recognizing accumulated losses and restructuring the entire local government financing model. The theater ratio is rising as policy stimulus becomes increasingly performative rather than addressing the structural overhang. However, the coordination function remains live (preventing disorderly collapse and systemic crisis), distinguishing this from pure piton. The tangled_rope classification captures this: genuine coordination necessity combined with substantial extraction that has accumulated as the original function was fulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_crisis_threshold,
    'At what level of property investment decline and local government fiscal stress does the coordination function (preventing systemic crisis) fail and the constraint collapse into disorderly adjustment?',
    'Observable through stress indicators: local government bond defaults, bank non-performing loan recognition, property price discovery in major cities, or central government emergency intervention scale. Historical precedent from other property-dependent economies provides comparative thresholds.',
    'If the threshold is near current stress levels, the constraint is approaching failure and the coordination function is exhausted. If substantial margin remains, the extraction can continue while crisis is deferred. Determines whether the arrangement is sustainable tangled_rope or pre-collapse snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_crisis_threshold, empirical, 'Threshold at which coordination function fails and crisis becomes disorderly').

omega_variable(
    capital_reallocation_counterfactual,
    'If the constraint were removed and property sector capital reallocated, would the resulting consumption growth and strategic industry investment offset the transition costs of local government restructuring and homeowner wealth loss?',
    'Requires structural economic modeling of reallocation effects, comparison with economies that underwent similar transitions, and assessment of social stability costs. Natural experiments from regional variations in property dependence provide partial evidence.',
    'If reallocation benefits substantially exceed transition costs, the constraint is primarily extractive (preventing beneficial adjustment). If transition costs dominate, the coordination function is genuine and necessary. Determines the extraction-to-coordination ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_reallocation_counterfactual, empirical, 'Net welfare effect of capital reallocation versus managed overhang').

omega_variable(
    performance_legitimacy_reading_dominance,
    'Which reading of the performance legitimacy kernel dominates central government decision-making on property sector policy: quantitative growth (requiring property sector stabilization), qualitative development (tolerating property decline for reallocation), techno-nationalist (subordinating property to strategic industries), or livelihood security (prioritizing affordable housing)?',
    'Observable through policy priority revealed in resource allocation, regulatory emphasis, and leadership rhetoric. The 15th Five-Year Plan targets and implementation provide evidence. Competing readings produce different constraint enforcement patterns.',
    'If quantitative growth reading dominates, the constraint persists with high suppression to prevent growth collapse. If qualitative development or techno-nationalist readings dominate, enforcement may relax to enable reallocation. If livelihood security dominates, the constraint would be restructured to prioritize affordability over price stability. The reading determines whether current extraction is transitional or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_legitimacy_reading_dominance, conceptual, 'Which performance legitimacy reading governs property sector policy').

omega_variable(
    local_government_fiscal_alternatives,
    'Are alternative local government revenue sources (property tax, VAT sharing, central transfers) structurally feasible and politically available, or is land-sale dependence irreducible given current fiscal architecture?',
    'Pilot programs in property tax implementation, analysis of fiscal transfer adequacy, and political economy assessment of central-local revenue sharing negotiations. International comparisons of local government financing models provide feasibility evidence.',
    'If alternatives are feasible, local government trapped status is policy choice rather than structural necessity, increasing the extractive component. If alternatives are structurally or politically foreclosed, the coordination function is more binding and local government victimization is inherent to the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_government_fiscal_alternatives, empirical, 'Feasibility of local government revenue diversification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(property_sector_overhang, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_tr_t0, property_sector_overhang, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(prop_tr_t0, observed).
narrative_ontology:measurement(prop_tr_t6, property_sector_overhang, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(prop_tr_t6, observed).
narrative_ontology:measurement(prop_tr_t12, property_sector_overhang, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(prop_tr_t12, observed).
narrative_ontology:measurement(prop_tr_t18, property_sector_overhang, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(prop_tr_t18, observed).
narrative_ontology:measurement(prop_tr_t24, property_sector_overhang, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(prop_tr_t24, observed).
narrative_ontology:measurement(prop_tr_t30, property_sector_overhang, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(prop_tr_t30, observed).
narrative_ontology:measurement(prop_tr_t36, property_sector_overhang, theater_ratio, 36, 0.41).
narrative_ontology:measurement_basis(prop_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(prop_be_t0, property_sector_overhang, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(prop_be_t0, observed).
narrative_ontology:measurement(prop_be_t6, property_sector_overhang, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(prop_be_t6, observed).
narrative_ontology:measurement(prop_be_t12, property_sector_overhang, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(prop_be_t12, observed).
narrative_ontology:measurement(prop_be_t18, property_sector_overhang, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(prop_be_t18, observed).
narrative_ontology:measurement(prop_be_t24, property_sector_overhang, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(prop_be_t24, observed).
narrative_ontology:measurement(prop_be_t30, property_sector_overhang, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(prop_be_t30, observed).
narrative_ontology:measurement(prop_be_t36, property_sector_overhang, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(prop_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(prop_su_t0, property_sector_overhang, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(prop_su_t0, observed).
narrative_ontology:measurement(prop_su_t6, property_sector_overhang, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(prop_su_t6, observed).
narrative_ontology:measurement(prop_su_t12, property_sector_overhang, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(prop_su_t12, observed).
narrative_ontology:measurement(prop_su_t18, property_sector_overhang, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(prop_su_t18, observed).
narrative_ontology:measurement(prop_su_t24, property_sector_overhang, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(prop_su_t24, observed).
narrative_ontology:measurement(prop_su_t30, property_sector_overhang, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(prop_su_t30, observed).
narrative_ontology:measurement(prop_su_t36, property_sector_overhang, suppression_requirement, 36, 0.72).
narrative_ontology:measurement_basis(prop_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(property_sector_overhang, resource_allocation).
narrative_ontology:boltzmann_floor_override(property_sector_overhang, 0.18).
narrative_ontology:affects_constraint(property_sector_overhang, local_government_debt_accumulation).
narrative_ontology:affects_constraint(property_sector_overhang, household_consumption_suppression).
narrative_ontology:affects_constraint(property_sector_overhang, strategic_industry_capital_availability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(property_sector_overhang, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
