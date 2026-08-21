% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Housing Price Formation (Institutional Reading)
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes housing price formation as a product of
 *   specific institutional designs: zoning laws, lending standards, tax
 *   treatment, and intermediary platforms. It is presented as the
 *   'institutional reading' of the broader 'price_formation_kernel',
 *   emphasizing that prices are not merely natural market outcomes but are
 *   actively constructed and shaped by human-made rules and systems. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual function of
 *   coordinating urban development and financial stability while
 *   simultaneously enabling asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.55).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation (Institutional Reading)").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '1f22e41e-6736-4594-835e-d588d411e7f6').
narrative_ontology:cs_kernel_codification('1f22e41e-6736-4594-835e-d588d411e7f6', formalized).
narrative_ontology:cs_authority_grounding('1f22e41e-6736-4594-835e-d588d411e7f6', practice).
narrative_ontology:cs_interpretation_layer_present('1f22e41e-6736-4594-835e-d588d411e7f6').
narrative_ontology:cs_reading_relation('1f22e41e-6736-4594-835e-d588d411e7f6', price_formation_kernel__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1f22e41e-6736-4594-835e-d588d411e7f6', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f22e41e-6736-4594-835e-d588d411e7f6', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('1f22e41e-6736-4594-835e-d588d411e7f6', foundational, housing_market_is_social_construct).
narrative_ontology:cs_axiom_status(housing_market_is_social_construct, holdable).
narrative_ontology:cs_axiom_grounding('1f22e41e-6736-4594-835e-d588d411e7f6', housing_market_is_social_construct, conventional).
narrative_ontology:cs_axiom('1f22e41e-6736-4594-835e-d588d411e7f6', foundational, institutional_rules_shape_value).
narrative_ontology:cs_axiom_status(institutional_rules_shape_value, holdable).
narrative_ontology:cs_axiom_grounding('1f22e41e-6736-4594-835e-d588d411e7f6', institutional_rules_shape_value, empirically_contingent).
narrative_ontology:cs_reference_frame('1f22e41e-6736-4594-835e-d588d411e7f6', regulatory_framework_as_market_shaper).
narrative_ontology:cs_drift_state('1f22e41e-6736-4594-835e-d588d411e7f6', contemporary_housing_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f22e41e-6736-4594-835e-d588d411e7f6', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_homeowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, lending_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_platforms_and_agents).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from rising property values due to restrictive zoning, favorable tax treatment, and limited supply. Their personal wealth and retirement plans are often tied to these appreciating assets, creating a strong incentive to maintain the status quo and making advocacy for looser regulations difficult due to self-interest.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_homeowners, beneficiary,
    powerful, biographical, identity_locked, local).

% Profit from mortgage origination, servicing, and securitization. They benefit from stable, appreciating asset values and standardized lending products, which reduce risk and increase transaction volume. They actively influence lending standards and financial regulations.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, lending_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from transaction volume and high property values, as their fees are typically percentage-based. They have a vested interest in maintaining market friction, information asymmetry, and high prices, often lobbying against policies that would streamline transactions or increase supply.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_platforms_and_agents, beneficiary,
    organized, biographical, arbitrage, national).

% Bear the direct cost of high housing prices through rising rents, with limited options for affordable housing or pathways to homeownership. Their mobility is often constrained by lease terms and the scarcity of alternative housing.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, immediate, constrained, local).

% Face significant barriers to entry due to high prices, strict lending standards, and intense competition. They often incur substantial long-term debt, delaying other life goals. Their options are limited to accepting high costs or remaining renters.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    moderate, biographical, constrained, local).

% Set zoning laws, approve development projects, and collect property taxes. They benefit from a stable, appreciating tax base but face conflicting pressures from incumbent owners (to maintain property values and neighborhood character) and affordability advocates (to increase housing supply).
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, local_governments, agenda_setter,
    institutional, generational, constrained, local).

% Advocate for policy changes to improve housing affordability and access, often challenging existing institutional arrangements and highlighting the extractive nature of current policies. Their influence is often limited by the political power of beneficiaries.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, housing_advocacy_groups, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes property rights, facilitates transactions, provides a framework for urban development, and ensures a stable tax base for local governments. It coordinates the allocation of land and housing resources within a regulated framework.
% TRANSFER_FUNCTION: Transfers wealth from new entrants and renters to incumbent owners, lenders, and intermediaries through asset appreciation, interest payments, and transaction fees, all mediated by the institutional rules of zoning, lending, and taxation.
% ABSENT_VOICES: Future generations, who will inherit the consequences of current housing policy, and those entirely priced out of formal housing markets (e.g., homeless populations), whose needs are not directly represented in policy debates or whose political power is negligible.
% DISAPPEARANCE_RATIONALE: If zoning, lending standards, tax treatment, and intermediary platforms vanished overnight, the housing market would collapse into chaos. Property rights would be unclear, financing impossible, and urban development unplanned. A new, likely different, system would rapidly emerge to fill the void.
% FOUNDING_PROBLEM: To establish clear property rights, facilitate orderly urban development, ensure financial stability in lending, and provide a stable tax base for local governments, while also addressing public health and safety concerns through building codes and land use planning.
% FOUNDING_PROBLEM_CORROBORATION: Local governments and financial institutions attest that the problems of stability and order are still live. Housing advocates and economists, from outside the benefiting parties, attest that while some original problems are solved, the current institutional framework has created new, severe affordability crises, suggesting the original mandate has been distorted into a mechanism for rent extraction.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.65) because the institutional framework, while providing some coordination, systematically inflates prices and transfers wealth to specific beneficiaries. Suppression (0.55) is present through the difficulty of challenging entrenched zoning, financial regulations, and industry practices. Theater ratio (0.25) is low, as the functions of these institutions (e.g., public safety, financial stability) are real, but their stated justifications often mask their price-inflating and wealth-transferring effects. The temporal measurements show a gradual increase in extractiveness and suppression over time, reflecting the hardening of these institutional structures and their increasing impact on affordability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries like incumbent homeowners and lenders, the institutional framework provides essential stability and order, justifying the costs as necessary for a functioning market. From the perspective of renters and first-time buyers, the same framework appears as a system designed to extract wealth and limit access, with its 'coordination' functions serving as cover for its extractive effects. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent homeowners, lending institutions, and real estate intermediaries are structural beneficiaries (low directionality), as they directly gain from the price inflation and transaction volume facilitated by these institutions. Renters and first-time homebuyers are the primary targets (high directionality), bearing the costs of high prices and limited access. Local governments act as agenda-setters, balancing competing interests while benefiting from a stable tax base.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_natural_ambiguity,
    'To what extent is housing price formation a natural market outcome reflecting objective scarcity and preference, versus a constructed outcome primarily shaped by institutional design?',
    'Comparative analysis of housing markets with significantly different institutional frameworks (e.g., highly regulated vs. less regulated, different tax regimes) to isolate the impact of institutional variables on price elasticity and affordability.',
    'If primarily constructed, the constraint''s extractiveness is more clearly attributable to policy choices and thus more amenable to reform; if more natural, policy interventions might be less effective or carry higher unintended consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_natural_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature of housing price formation.').

omega_variable(
    extraction_boundary_ambiguity,
    'Where does legitimate coordination cost (e.g., for clear property rights, financial stability, public infrastructure) end and rent extraction begin within the current institutional framework?',
    'Detailed economic modeling that disaggregates the costs of providing essential market functions from the surplus captured by beneficiaries due to market power or regulatory capture, potentially informed by ''cost-plus'' regulatory approaches.',
    'A clearer boundary would enable more precise policy interventions to reduce extraction without undermining essential coordination functions, potentially reclassifying parts of the constraint as pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_boundary_ambiguity, empirical, 'Distinguishing necessary coordination costs from pure rent extraction.').

omega_variable(
    policy_effectiveness_ambiguity,
    'Do current zoning, lending standards, and tax treatments achieve their stated goals (e.g., public safety, financial stability, community character) or primarily serve to restrict supply, inflate prices, and transfer wealth?',
    'Longitudinal studies evaluating the actual outcomes of specific policies against their stated objectives, including unintended consequences on affordability and equity, corroborated by independent expert panels.',
    'If policies are found to be ineffective or counterproductive to their stated goals, it would strengthen the argument for their reform or abolition, potentially shifting the constraint''s classification towards a Piton or Snare if the original function has atrophied or become cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_effectiveness_ambiguity, empirical, 'Effectiveness of institutional policies in achieving stated goals versus actual outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__institutional_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__institutional_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__institutional_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__institutional_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(pric_tr_t2024, price_formation_kernel__institutional_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__institutional_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__institutional_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__institutional_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__institutional_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(pric_be_t2024, price_formation_kernel__institutional_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__institutional_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__institutional_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__institutional_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__institutional_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(pric_su_t2024, price_formation_kernel__institutional_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'price_formation_kernel', each representing a different structural interpretation of how housing prices are formed. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
