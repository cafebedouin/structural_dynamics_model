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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   domain: economic/political_economy/housing_markets
 *
 * SUMMARY:
 *   This constraint represents the Georgist reading of price formation, which
 *   analytically separates land rent (unearned value derived from location
 *   and public investment) from improvement value (earned value from labor
 *   and capital). From this perspective, the constraint is a hybrid: the
 *   fixed supply and location scarcity of land are 'mountain-like' features,
 *   but the private capture of land rent is a 'snare' that extracts from
 *   productive activity. The coordination of investment in improvements,
 *   however, functions as a 'rope'. The overall claimed type is
 *   'tangled_rope' due to the combination of coordination for improvements
 *   and significant extraction via land rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.8).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.75).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Price Formation: Georgist Reading (Land Rent vs. Improvement Value)").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "economic/political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '0ffff909-e7ab-4da4-bacb-667cab6d2e84').
narrative_ontology:cs_kernel_codification('0ffff909-e7ab-4da4-bacb-667cab6d2e84', formalized).
narrative_ontology:cs_authority_grounding('0ffff909-e7ab-4da4-bacb-667cab6d2e84', expertise).
narrative_ontology:cs_interpretation_layer_present('0ffff909-e7ab-4da4-bacb-667cab6d2e84').
narrative_ontology:cs_reading_relation('0ffff909-e7ab-4da4-bacb-667cab6d2e84', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ffff909-e7ab-4da4-bacb-667cab6d2e84', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ffff909-e7ab-4da4-bacb-667cab6d2e84', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_axiom('0ffff909-e7ab-4da4-bacb-667cab6d2e84', foundational, land_value_is_socially_created).
narrative_ontology:cs_axiom_status(land_value_is_socially_created, holdable).
narrative_ontology:cs_axiom_grounding('0ffff909-e7ab-4da4-bacb-667cab6d2e84', land_value_is_socially_created, conventional).
narrative_ontology:cs_axiom('0ffff909-e7ab-4da4-bacb-667cab6d2e84', foundational, private_appropriation_of_land_rent_is_unjust).
narrative_ontology:cs_axiom_status(private_appropriation_of_land_rent_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('0ffff909-e7ab-4da4-bacb-667cab6d2e84', private_appropriation_of_land_rent_is_unjust, deontological).
narrative_ontology:cs_reference_frame('0ffff909-e7ab-4da4-bacb-667cab6d2e84', classical_economic_distinction).
narrative_ontology:cs_drift_state('0ffff909-e7ab-4da4-bacb-667cab6d2e84', contemporary_housing_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0ffff909-e7ab-4da4-bacb-667cab6d2e84', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, developers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_and_capital_investors).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, tenants).
narrative_ontology:constraint_vindicates(price_formation_kernel__georgist_reading, georgist_land_value_tax_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land and collect rent, which the Georgist reading identifies as unearned value derived from location and public investment, not from their labor or capital improvements. They benefit from the existing legal framework that allows private appropriation of this rent.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, agenda_setter,
    powerful, generational, mobile, national).

% Invest their labor and capital into improving land or producing goods and services, but a portion of their earned returns is transferred as land rent to landowners. Their options are to accept lower returns or relocate to areas with lower land values, which is often costly.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_and_capital_investors, payer,
    organized, biographical, constrained, national).

% Pay rent for access to land and improvements, with a significant portion of this payment attributed to unearned land rent. They have limited exit options due to housing scarcity and high moving costs, making them highly vulnerable to rent extraction.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, tenants, payer,
    powerless, immediate, constrained, local).

% Create value through improvements on land. While they pay land rent, their ability to profit from construction and development is coordinated by the clear distinction between land and improvement value, allowing them to finance and sell improvements.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, developers, beneficiary,
    organized, biographical, mobile, regional).

% Analyze the economic system through the lens of land rent and advocate for policies like a land value tax to capture socially created value for public benefit. They are outside the direct flow of extraction but seek to reform it.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_advocates, observer,
    analytical, generational, analytical, global).

% Administer property taxes, which typically conflate land and improvement values. From a Georgist perspective, they are agenda-setters who could reform the system to tax land value more effectively but are constrained by political and institutional inertia.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, government_tax_authorities, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment in productive improvements by analytically separating their value from the unearned value of land, theoretically allowing for efficient allocation of capital and labor.
% TRANSFER_FUNCTION: Transfers unearned economic rent, derived from location and public investment, from productive labor and capital to private landowners through the mechanism of land prices and rents.
% ABSENT_VOICES: Future generations and those dispossessed by land speculation are structurally absent from the current price formation mechanism; they would advocate for policies that ensure equitable access to land and capture of socially created land value.
% DISAPPEARANCE_RATIONALE: If the analytical distinction between land rent and improvement value, and the legal framework allowing private capture of land rent, vanished overnight, the entire system of property valuation, taxation, and investment in real estate would fundamentally reorganize. Land would likely be treated as a common resource, drastically altering wealth distribution and economic incentives.
% FOUNDING_PROBLEM: To understand the source of wealth and poverty, distinguishing between wealth created by labor and capital (earned) and wealth derived from natural resources or location (unearned), and to identify a just basis for taxation.
% FOUNDING_PROBLEM_CORROBORATION: Economists and political theorists outside the direct beneficiary class (e.g., public finance scholars, urban planners, housing advocates) continue to attest to the problem of land speculation, housing affordability, and the economic inefficiencies arising from the private capture of land rent, corroborating the founding problem's ongoing relevance.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.8) because the Georgist framework highlights the substantial transfer of socially created land value to private hands without corresponding productive effort. Suppression is also high (0.75) as the legal and institutional apparatus of private property rights, zoning, and taxation actively enforces this transfer and limits alternatives. Theater ratio is moderate (0.4) because while some justification for land ownership exists (e.g., stewardship), a significant portion of the system's operation is performative maintenance of the rent-seeking mechanism. Accessibility collapse is moderate (0.6) as access to prime locations without paying rent is effectively suppressed. Resistance is moderate (0.5) due to ongoing advocacy for land value taxation and housing justice, which challenges the status quo.
 *
 * PERSPECTIVAL GAP:
 *   Landowners perceive the system as a fair return on their investment and property rights, viewing all value as earned. Laborers, capital investors, and tenants, from the Georgist perspective, experience it as an extractive mechanism that siphons off their productive output or income. The government, as an agenda-setter, navigates these competing claims, often conflating land and improvement values in policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners are primary beneficiaries, collecting unearned rent. Developers benefit from the coordination of improvement value, though they also pay land rent. Labor and capital investors, along with tenants, are the primary victims, bearing the costs of land rent. Government tax authorities are agenda-setters who could alter the system but are constrained by existing legal and political structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The Georgist analysis suggests that while the original problem of coordinating land use and development might have been addressed, the mechanism for capturing land rent has become a source of extraction. The classification as a Tangled Rope prevents mislabeling the entire system as pure extraction (ignoring the coordination of improvements) or pure coordination (ignoring the unearned rent capture). It highlights the dual nature where a genuine coordination function coexists with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_empirical_separation,
    'Is the separation of land rent from improvement value a purely conceptual tool for economic analysis, or does it reflect an empirically distinct and measurable phenomenon in real estate markets?',
    'Development of robust econometric models capable of consistently disaggregating land and improvement values across diverse markets and over time, with high predictive power.',
    'If purely conceptual, the constraint''s extractiveness is a theoretical construct; if empirically distinct, the extraction is a measurable economic reality, strengthening the case for policy interventions like land value taxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_empirical_separation, conceptual, 'Ambiguity regarding the analytical vs. empirical status of land rent separation.').

omega_variable(
    private_rent_incentive_ambiguity,
    'Is the private appropriation of land rent a necessary incentive for efficient land stewardship and development, or is it primarily a mechanism for unearned wealth accumulation that distorts economic activity?',
    'Comparative studies of land use efficiency and development rates in jurisdictions with varying land value taxation regimes or alternative land tenure systems.',
    'If necessary for incentives, a portion of the measured extraction could be reclassified as a coordination cost; if primarily distortive, the extraction is confirmed as inefficient and unjust, strengthening the Snare aspect of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_rent_incentive_ambiguity, preference, 'Whether private land rent serves a functional incentive role or is purely extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1900, price_formation_kernel__georgist_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(pric_tr_t1930, price_formation_kernel__georgist_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement(pric_tr_t1960, price_formation_kernel__georgist_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__georgist_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__georgist_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__georgist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(pric_be_t1900, price_formation_kernel__georgist_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(pric_be_t1930, price_formation_kernel__georgist_reading, base_extractiveness, 1930, 0.65).
narrative_ontology:measurement(pric_be_t1960, price_formation_kernel__georgist_reading, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__georgist_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__georgist_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__georgist_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1900, price_formation_kernel__georgist_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(pric_su_t1930, price_formation_kernel__georgist_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(pric_su_t1960, price_formation_kernel__georgist_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__georgist_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__georgist_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__georgist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__georgist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is the Georgist reading of the 'price_formation_kernel', distinguishing land rent from improvement value. It is one of several structurally distinct interpretations of how prices are formed in real estate markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
