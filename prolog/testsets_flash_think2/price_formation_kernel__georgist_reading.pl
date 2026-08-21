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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: price_formation_kernel__georgist_reading
 *   human_readable: Price Formation: Georgist Reading (Land Rent vs. Improvement Value)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story presents a Georgist reading of price formation in
 *   land and housing markets. It posits that the market mechanism, while
 *   coordinating land allocation and improvement, also enables the private
 *   capture of 'land rent' – value derived from location and public
 *   investment, not individual labor or capital. This unearned increment is
 *   seen as extractive, making the overall price formation a 'tangled_rope'
 *   that combines a genuine coordination function with asymmetric extraction.
 *   The story emphasizes the distinction between land value (fixed supply,
 *   location-based) and improvement value (labor/capital-based).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.8).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.75).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Price Formation: Georgist Reading (Land Rent vs. Improvement Value)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '476a10d3-c613-4758-b4c3-f003417d2825').
narrative_ontology:cs_kernel_codification('476a10d3-c613-4758-b4c3-f003417d2825', formalized).
narrative_ontology:cs_authority_grounding('476a10d3-c613-4758-b4c3-f003417d2825', practice).
narrative_ontology:cs_interpretation_layer_present('476a10d3-c613-4758-b4c3-f003417d2825').
narrative_ontology:cs_reading_relation('476a10d3-c613-4758-b4c3-f003417d2825', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('476a10d3-c613-4758-b4c3-f003417d2825', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('476a10d3-c613-4758-b4c3-f003417d2825', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('476a10d3-c613-4758-b4c3-f003417d2825', foundational, land_value_is_socially_created).
narrative_ontology:cs_axiom_status(land_value_is_socially_created, holdable).
narrative_ontology:cs_axiom_grounding('476a10d3-c613-4758-b4c3-f003417d2825', land_value_is_socially_created, conventional).
narrative_ontology:cs_axiom('476a10d3-c613-4758-b4c3-f003417d2825', foundational, private_appropriation_of_land_rent_is_unjust).
narrative_ontology:cs_axiom_status(private_appropriation_of_land_rent_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('476a10d3-c613-4758-b4c3-f003417d2825', private_appropriation_of_land_rent_is_unjust, deontological).
narrative_ontology:cs_reference_frame('476a10d3-c613-4758-b4c3-f003417d2825', land_as_common_heritage).
narrative_ontology:cs_drift_state('476a10d3-c613-4758-b4c3-f003417d2825', contemporary_private_land_ownership, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('476a10d3-c613-4758-b4c3-f003417d2825', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, financial_institutions).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants_and_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, developers_and_builders).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_and_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, developers_and_builders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, local_governments).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, georgist_economic_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, land_value_tax_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold title to land and collect rent, which is seen as unearned income derived from location scarcity and public investment rather than productive effort. They benefit directly from rising land values and the legal framework that enforces private land ownership.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, agenda_setter,
    powerful, generational, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, landowners, beneficiary).

% Pay land rent directly (as tenants) or indirectly (as part of the purchase price of property). They bear the cost of location scarcity and public investment without receiving a corresponding benefit from the land itself, leading to reduced disposable income and housing affordability issues. Exit options are limited to moving to less desirable or more distant locations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants_and_homebuyers, payer,
    moderate, biographical, constrained, local).

% Pay high prices for land, which increases the cost of housing development and reduces the return on their labor and capital invested in improvements. They are beneficiaries of the market for improvements but payers of land rent. Their ability to build affordable housing is constrained by land costs.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, developers_and_builders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, developers_and_builders, beneficiary).

% Collect property taxes, a portion of which falls on land value. While they benefit from this revenue, the current tax system often disincentivizes productive improvements and fails to fully capture socially created land value for public benefit. They enforce property rights and zoning laws that shape land prices.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, local_governments, beneficiary,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__georgist_reading, local_governments, agenda_setter).

% The returns to labor and capital are diminished by the need to pay land rent, which acts as a drag on overall economic productivity and wages. They are forced to allocate a significant portion of their output to landowners, reducing investment and consumption in other sectors.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_and_capital, payer,
    moderate, biographical, constrained, national).

% Lend against the value of land, profiting from the appreciation of land assets and the demand for mortgages. They benefit from the existing price formation mechanism that capitalizes land rent into asset values, even if it contributes to housing bubbles and financial instability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, financial_institutions, beneficiary,
    institutional, generational, mobile, global).

% Analyze the economic effects of land rent and advocate for policies like a Land Value Tax (LVT) to capture socially created land value for public use. They seek to reform the price formation mechanism to eliminate unearned income from land ownership and reduce economic inequality.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:fixing_cost_class(price_formation_kernel__georgist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce land resources and signals demand for both land and improvements, guiding investment in housing and infrastructure within a market framework.
% TRANSFER_FUNCTION: Moves unearned land rent from tenants, homebuyers, developers, labor, and capital to private landowners and, to a lesser extent, to local governments through property taxes.
% ABSENT_VOICES: Those dispossessed by high land prices, future generations whose access to land is diminished, and advocates for a more equitable distribution of socially created wealth. They are often excluded from policy-making processes dominated by entrenched property interests.
% DISAPPEARANCE_RATIONALE: If the current price formation mechanism, particularly the private capture of land rent, vanished overnight, the allocation of land, investment in improvements, and the distribution of wealth would fundamentally reorganize. Land would be valued primarily for its use and improvements, rather than speculative rent, leading to significant shifts in housing affordability, economic activity, and public finance.
% FOUNDING_PROBLEM: To efficiently allocate scarce land resources and incentivize the creation of productive improvements within a growing economy.
% FOUNDING_PROBLEM_CORROBORATION: While the problem of land allocation remains live, Georgist economists and social justice advocates attest that the current mechanism has evolved to primarily facilitate rent extraction rather than optimal allocation. Mainstream economists and landowners often attest that the current system is efficient and fair, reflecting earned returns. Legislative hearings and independent economic analyses from outside benefiting parties support the shifted-function reading, highlighting the growing disparity between land value and improvement value.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) reflects the Georgist view that land rent constitutes a significant, unearned transfer from productive activity to landowners. Suppression (0.75) is high due to the legal and institutional enforcement of private land ownership and the lack of viable alternatives to paying land rent in desirable locations. The theater ratio is low (0.15) because the market genuinely performs a resource allocation function; the extraction is embedded within this functional mechanism, not a theatrical overlay. Accessibility collapse is high (0.7) as escaping land rent is difficult, though not impossible (e.g., moving to less developed areas). Resistance is moderate (0.4) reflecting ongoing, but not dominant, advocacy for land value taxation and related reforms.
 *
 * PERSPECTIVAL GAP:
 *   Landowners and financial institutions perceive the current price formation as a natural and fair return on investment or property rights. Tenants, homebuyers, and Georgist advocates, however, experience it as an extractive mechanism that concentrates wealth and hinders productive economic activity. The engine's classification will highlight this divergence between the claimed 'natural' or 'efficient' function and the measured extractive reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners and financial institutions are clear beneficiaries, collecting rents and profiting from land value appreciation. Tenants, homebuyers, developers, and labor/capital are victims, bearing the costs of land rent. Local governments are mixed, benefiting from property taxes but also constrained by the system's inefficiencies. The Georgist advocates act as analytical observers, highlighting the structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to efficiently allocate land and incentivize improvements. From a Georgist perspective, while it still performs allocation, its primary function has drifted towards facilitating the private capture of socially created land value. The persistence of high land rents, despite the founding problem of efficient allocation being partially addressed, indicates a form of mandatrophy where the mechanism serves rent-seeking more than its initial coordination purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_value_origin_ambiguity,
    'To what extent is land value a ''natural'' consequence of scarcity and location, versus a ''socially created'' value arising from public investment and community presence?',
    'Detailed econometric studies isolating the contribution of public infrastructure and agglomeration effects to land value, distinct from private improvements.',
    'If land value is predominantly socially created, it strengthens the case for public capture (e.g., LVT) and reclassifies the ''natural'' component as a constructed constraint. If it''s mostly natural, the extractive component is harder to challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_value_origin_ambiguity, empirical, 'Ambiguity regarding the source of land value.').

omega_variable(
    separability_of_land_and_improvement_value,
    'Is it practically and conceptually possible to fully separate the value of bare land from the value of improvements (buildings, infrastructure) for taxation and economic analysis?',
    'Pilot programs for Land Value Tax implementation and their administrative feasibility, alongside refined appraisal methodologies.',
    'If separable, the Georgist analysis of land rent as distinct extraction is robust. If inseparable, the ''tangled_rope'' classification becomes more complex, as the coordination and extraction functions are harder to disentangle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_land_and_improvement_value, conceptual, 'Feasibility of separating land and improvement values.').

omega_variable(
    impact_of_financialization_on_land_rent,
    'What proportion of observed land value appreciation is driven by financialization (credit expansion, speculative bubbles) versus fundamental economic factors (population growth, public investment)?',
    'Comparative analysis of land value trends across different financial regulatory regimes and credit cycles, alongside counterfactual modeling.',
    'If financialization is a dominant driver, the ''tangled_rope'' aspect is amplified by external forces, potentially shifting the classification towards ''snare'' due to increased speculative extraction. If fundamental factors dominate, the inherent extractive nature of land ownership is more central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_financialization_on_land_rent, empirical, 'Role of financialization in land rent dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__georgist_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(pric_tr_t1965, price_formation_kernel__georgist_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__georgist_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement(pric_tr_t1995, price_formation_kernel__georgist_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__georgist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__georgist_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__georgist_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(pric_be_t1965, price_formation_kernel__georgist_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__georgist_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(pric_be_t1995, price_formation_kernel__georgist_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__georgist_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__georgist_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__georgist_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(pric_su_t1965, price_formation_kernel__georgist_reading, suppression_requirement, 1965, 0.65).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__georgist_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(pric_su_t1995, price_formation_kernel__georgist_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__georgist_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__georgist_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'price_formation_kernel', each offering a distinct structural analysis of how prices for land and improvements are formed and who benefits. This Georgist reading focuses on the distinction between earned improvement value and unearned land rent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
