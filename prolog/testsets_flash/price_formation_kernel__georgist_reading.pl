% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__georgist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Georgist Reading of Land Price Formation (Rent vs. Improvement)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the Georgist reading of price formation in land
 *   and housing markets, where the value of land is conceptually and
 *   practically separated into unearned 'land rent' (derived from location,
 *   public investment, and scarcity) and 'improvement value' (derived from
 *   labor and capital investment). The Georgist perspective argues that while
 *   the fixed supply of land is a natural constraint (mountain-like), the
 *   private appropriation of land rent is a human-made, extractive mechanism
 *   (snare-like), while the value of improvements is a legitimate return to
 *   coordination (rope-like). This reading frames the overall price formation
 *   as a tangled rope due to the coordination function of property rights
 *   combined with the asymmetric extraction of land rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.7).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading of Land Price Formation (Rent vs. Improvement)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '440076e1-908a-4cc1-ac30-074149860424').
narrative_ontology:cs_kernel_codification('440076e1-908a-4cc1-ac30-074149860424', implicit).
narrative_ontology:cs_authority_grounding('440076e1-908a-4cc1-ac30-074149860424', distributed).
narrative_ontology:cs_reading_relation('440076e1-908a-4cc1-ac30-074149860424', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('440076e1-908a-4cc1-ac30-074149860424', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('440076e1-908a-4cc1-ac30-074149860424', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('440076e1-908a-4cc1-ac30-074149860424', foundational, land_is_not_product_of_labor).
narrative_ontology:cs_axiom_status(land_is_not_product_of_labor, holdable).
narrative_ontology:cs_axiom_grounding('440076e1-908a-4cc1-ac30-074149860424', land_is_not_product_of_labor, deontological).
narrative_ontology:cs_axiom('440076e1-908a-4cc1-ac30-074149860424', foundational, economic_rent_belongs_to_community).
narrative_ontology:cs_axiom_status(economic_rent_belongs_to_community, holdable).
narrative_ontology:cs_axiom_grounding('440076e1-908a-4cc1-ac30-074149860424', economic_rent_belongs_to_community, deontological).
narrative_ontology:cs_reference_frame('440076e1-908a-4cc1-ac30-074149860424', classical_political_economy_distinction).
narrative_ontology:cs_drift_state('440076e1-908a-4cc1-ac30-074149860424', contemporary_financialized_markets, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('440076e1-908a-4cc1-ac30-074149860424', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, land_speculators).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, capital_investors).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, homebuyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects economic rent from land ownership, which is unearned value derived from location and public investment rather than productive effort. Benefits from rising land values without contributing to production. Actively lobbies for policies that protect land value and minimize land value taxation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, agenda_setter,
    powerful, generational, arbitrage, local).

% Pays a portion of its earned income as land rent, either directly through housing costs or indirectly through higher prices for goods and services. Has limited options to avoid paying for access to desirable locations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor, payer,
    organized, biographical, constrained, local).

% Invests in productive improvements on land, but a portion of the returns to capital is captured by rising land rents, reducing the incentive for productive investment. Seeks to minimize land costs but is bound by location scarcity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, capital_investors, payer,
    powerful, biographical, constrained, local).

% Directly pays land rent as part of their housing costs, often with limited ability to move to lower-cost areas due to job markets, social ties, and public services. Bears the brunt of rising land values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, immediate, trapped, local).

% Pays for both the value of improvements and the unearned land value when purchasing property. Often takes on significant debt to acquire land, which represents a claim on future economic rent.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, homebuyers, payer,
    moderate, biographical, constrained, local).

% Profits from holding land and waiting for its value to increase due to public investment or population growth, without making productive use of it. Benefits from the legal framework that allows private appropriation of land rent.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, land_speculators, beneficiary,
    powerful, biographical, arbitrage, local).

% Provides infrastructure and services that increase land values, but often fails to capture this publicly created value through taxation, leading to underfunded public goods and reliance on other, less efficient taxes. Could implement land value taxation but faces political resistance.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, public_sector, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of scarce land resources by assigning private ownership rights, which in turn facilitates investment in improvements and provides a basis for market transactions.
% TRANSFER_FUNCTION: Transfers unearned economic rent from labor and capital (through housing costs, business costs, and property prices) to private landowners, based on the location and scarcity value of land.
% ABSENT_VOICES: Future generations, who will inherit a system where land rent is privately appropriated, would object to the current price formation mechanism. They are excluded by their non-existence and the short-term focus of current political systems.
% DISAPPEARANCE_RATIONALE: If the separation of land rent from improvement value, and the private capture of land rent, vanished overnight (e.g., through full land value taxation), the entire structure of property ownership, taxation, and investment would fundamentally reorganize. Land prices would fall, housing would become more affordable, and public revenue would increase, leading to a massive reallocation of wealth and economic activity.
% FOUNDING_PROBLEM: The need to establish clear property rights for land to enable investment and prevent conflict over scarce resources, and to distinguish between value created by nature/society and value created by individual effort.
% FOUNDING_PROBLEM_CORROBORATION: Landowners and many economists argue that private land ownership and rent collection are essential for efficient land use and investment (live problem). Georgist economists and social reformers argue that the problem of unearned rent capture persists and exacerbates inequality, indicating the founding problem is either misidentified or its solution has become extractive (contested status). Historical economic texts and contemporary analyses from outside the landowning class corroborate the critique.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because a significant portion of wealth is transferred from productive labor and capital to landowners without a corresponding productive contribution. Suppression (0.70) is also high, as the legal and institutional framework actively enforces private land ownership and the right to collect rent, suppressing alternatives like public capture of land value. Theater ratio (0.20) is low, as the system is highly functional in its extractive purpose, with little performative maintenance. Accessibility collapse (0.40) is moderate, as alternatives to private land ownership exist conceptually (e.g., land value tax, common ownership) but are politically and institutionally difficult to implement. Resistance (0.55) is moderate, reflecting ongoing advocacy for land value taxation and housing affordability movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of landowners, the system is a legitimate coordination mechanism for property rights and investment. From the perspective of labor and tenants, it is an extractive system that forces them to pay for access to essential resources. The engine's classification as a tangled rope reflects this hybrid nature, where a genuine coordination function (property rights) is intertwined with asymmetric extraction (private land rent).
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners and speculators are clear beneficiaries (d=0.0-0.2) as they collect unearned rent. Labor, capital investors, tenants, and homebuyers are victims/payers (d=0.8-1.0) as they bear the cost of land rent. The public sector is an observer, potentially a victim if it fails to capture publicly created land value. The system is actively enforced to maintain the private capture of land rent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_value_attribution,
    'What proportion of land value is genuinely ''unearned'' (from public investment/scarcity) versus ''earned'' (from private effort to improve the land)?',
    'Detailed econometric analysis separating land value from improvement value across diverse markets, accounting for public infrastructure, zoning changes, and private development costs.',
    'A higher proportion of unearned value strengthens the Georgist claim of extraction and the case for land value taxation; a lower proportion would weaken it, suggesting more value is privately created.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_value_attribution, empirical, 'Quantifying the ''unearned'' component of land value.').

omega_variable(
    political_will_for_lvt,
    'Is the political resistance to land value taxation (LVT) primarily due to conceptual disagreement about land rent, or due to concentrated economic interests of landowners?',
    'Analysis of lobbying efforts, campaign finance, and public discourse surrounding LVT proposals, alongside surveys of public understanding of land economics.',
    'If resistance is primarily due to economic interests, the ''snare'' aspect of rent capture is more pronounced, requiring stronger political intervention. If conceptual disagreement is dominant, educational efforts might be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_for_lvt, preference, 'Understanding the drivers of political resistance to land value taxation.').

omega_variable(
    georgist_vs_institutional_framing,
    'To what extent does the Georgist reading (focus on land rent) adequately account for the institutional factors (zoning, lending, tax) highlighted by the institutional_reading?',
    'Comparative analysis of policy interventions: do Georgist-inspired policies (LVT) effectively address problems attributed to institutional factors, or are separate institutional reforms needed?',
    'If LVT effectively mitigates institutionally-driven problems, the Georgist reading''s explanatory power is enhanced. If not, the institutional_reading offers a more complete account, potentially shifting the classification towards a more purely constructed ''snare'' or ''tangled rope'' based on regulatory capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(georgist_vs_institutional_framing, conceptual, 'Framing under-determination: Georgist vs. Institutional explanations of price formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1800, price_formation_kernel__georgist_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(pric_tr_t1850, price_formation_kernel__georgist_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(pric_tr_t1900, price_formation_kernel__georgist_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__georgist_reading, theater_ratio, 1950, 0.17).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__georgist_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__georgist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(pric_be_t1800, price_formation_kernel__georgist_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(pric_be_t1850, price_formation_kernel__georgist_reading, base_extractiveness, 1850, 0.48).
narrative_ontology:measurement(pric_be_t1900, price_formation_kernel__georgist_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__georgist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__georgist_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__georgist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1800, price_formation_kernel__georgist_reading, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(pric_su_t1850, price_formation_kernel__georgist_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(pric_su_t1900, price_formation_kernel__georgist_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__georgist_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__georgist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__georgist_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, housing_affordability_crisis).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, wealth_inequality_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is the Georgist reading of the price_formation_kernel, which also includes naturalist_reading, institutional_reading, and financialization_reading. Each reading offers a distinct structural account of how land and housing prices are formed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
