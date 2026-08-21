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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Georgist Reading of Price Formation: Land Rent vs. Improvement Value
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint represents the Georgist reading of price formation in
 *   land and housing markets, which distinguishes between land rent (unearned
 *   value derived from location and public investment) and improvement value
 *   (earned value from labor and capital). From this perspective, the land
 *   component of price formation is akin to a Mountain (fixed supply,
 *   location scarcity), but the private capture of land rent operates as a
 *   Snare, extracting wealth from productive activity. The improvement
 *   component, however, functions more like a Rope, coordinating production.
 *   The overall constraint is classified as a Tangled Rope due to the hybrid
 *   nature: a genuine coordination function for improvements, but asymmetric
 *   extraction from land rent, requiring active enforcement (property rights,
 *   zoning) to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.75).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading of Price Formation: Land Rent vs. Improvement Value").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, 'b2219d57-abb4-4600-a214-eec0336dd075').
narrative_ontology:cs_kernel_codification('b2219d57-abb4-4600-a214-eec0336dd075', distributed).
narrative_ontology:cs_authority_grounding('b2219d57-abb4-4600-a214-eec0336dd075', practice).
narrative_ontology:cs_reading_relation('b2219d57-abb4-4600-a214-eec0336dd075', price_formation_kernel__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('b2219d57-abb4-4600-a214-eec0336dd075', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2219d57-abb4-4600-a214-eec0336dd075', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('b2219d57-abb4-4600-a214-eec0336dd075', foundational, land_is_not_product_of_labor).
narrative_ontology:cs_axiom_status(land_is_not_product_of_labor, holdable).
narrative_ontology:cs_axiom_grounding('b2219d57-abb4-4600-a214-eec0336dd075', land_is_not_product_of_labor, deontological).
narrative_ontology:cs_axiom('b2219d57-abb4-4600-a214-eec0336dd075', foundational, rent_belongs_to_community).
narrative_ontology:cs_axiom_status(rent_belongs_to_community, holdable).
narrative_ontology:cs_axiom_grounding('b2219d57-abb4-4600-a214-eec0336dd075', rent_belongs_to_community, deontological).
narrative_ontology:cs_reference_frame('b2219d57-abb4-4600-a214-eec0336dd075', classical_georgist_principles).
narrative_ontology:cs_drift_state('b2219d57-abb4-4600-a214-eec0336dd075', contemporary_neoclassical_economics, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b2219d57-abb4-4600-a214-eec0336dd075', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_and_capital_producers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, homebuyers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, public_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own land and collect rent based on location scarcity and public investment, without contributing labor or capital to its value. They benefit directly from rising land values and resist policies that would capture land rent for public use.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, agenda_setter,
    powerful, generational, arbitrage, local).

% Generate wealth through their work and investments, but a significant portion of this wealth is captured as land rent, reducing their returns. They face a choice between paying high rents or relocating to less desirable areas.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_and_capital_producers, payer,
    moderate, biographical, constrained, local).

% Pay land rent indirectly through their housing costs, which are inflated by speculative land values. They have limited options to escape rising rents due to housing scarcity and the immobility of their jobs and communities.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, renters, payer,
    powerless, immediate, trapped, local).

% Must pay a significant premium for land value when purchasing property, which represents unearned wealth for the seller. This increases the barrier to homeownership and diverts capital from productive investment.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, homebuyers, payer,
    moderate, biographical, constrained, local).

% Provides infrastructure and services that increase land values, but often fails to capture this publicly-created value through taxation, leading to underfunded public goods and reliance on other, less efficient taxes. Could be a major beneficiary if land rent were captured.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, public_sector, beneficiary,
    institutional, generational, constrained, local).

% Analyze the economic system through the lens of land rent and advocate for policies like a Land Value Tax to capture unearned increment for public benefit. They seek to reframe the understanding of wealth distribution.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The price formation mechanism coordinates the allocation of land and improvements in a market economy, signaling demand and supply for both. The improvement component coordinates labor and capital for construction and maintenance.
% TRANSFER_FUNCTION: Transfers unearned economic rent from labor and capital (producers, renters, homebuyers) to landowners, based on location and publicly-created value, rather than productive effort.
% ABSENT_VOICES: Future generations, who will inherit a system where land is increasingly monopolized and expensive, are absent from the current policy debate. Also, those who are priced out of desirable locations entirely, forced into less productive or less connected areas, have limited voice.
% DISAPPEARANCE_RATIONALE: If the separation of land rent from improvement value, and the private capture of land rent, vanished overnight (e.g., replaced by a full Land Value Tax), the entire structure of property ownership, taxation, and wealth distribution would fundamentally rearrange. Land prices would fall, housing affordability would improve, and public revenue streams would shift dramatically.
% FOUNDING_PROBLEM: The problem of allocating scarce land resources and incentivizing productive use of land, while also ensuring fair distribution of wealth generated by society and nature.
% FOUNDING_PROBLEM_CORROBORATION: Georgist economists and urban planners attest that the problem of land speculation and unearned wealth persists, leading to economic inequality and housing crises. Mainstream economists acknowledge the concept of economic rent but often dispute the extent of its impact or the feasibility of its capture; however, the existence of the problem itself is widely recognized, even if solutions are contested.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(price_formation_kernel__georgist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__georgist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because a significant portion of economic activity is diverted to landowners as unearned rent. Suppression is also high (0.75) because the system of private land ownership and taxation actively suppresses alternative land tenure models and the public capture of land value. Theater ratio is low (0.1) as the system is highly functional in its extractive purpose, with little performative maintenance. The increasing extractiveness and suppression over time reflect the growing impact of land speculation and the hardening of property rights regimes.
 *
 * PERSPECTIVAL GAP:
 *   Landowners perceive the system as a fair return on their investment and property rights, while labor/capital producers, renters, and homebuyers experience it as an extractive burden. The engine's classification as Tangled Rope reflects this hybrid reality, where a coordination function (allocating land) is intertwined with asymmetric extraction (private capture of land rent).
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners are clear beneficiaries (d near 0.0) as they collect unearned rent. Labor and capital producers, renters, and homebuyers are targets (d near 1.0) as they bear the cost of land rent. The public sector is a constrained beneficiary, as it provides value-enhancing infrastructure but often fails to capture the resulting land value for public benefit. Georgist advocates are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_value_tax_feasibility,
    'To what extent is a comprehensive Land Value Tax (LVT) politically and practically feasible as a mechanism to capture land rent for public benefit?',
    'Empirical studies of LVT implementation in various jurisdictions, analysis of political economy barriers, and public opinion surveys on property taxation reform.',
    'If LVT is highly feasible, the ''snare'' component of land rent capture is more easily resolvable, potentially shifting the constraint towards a more ''rope-like'' coordination of land use. If infeasible, the extractive nature of land rent is more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_value_tax_feasibility, empirical, 'Feasibility of policy interventions to alter land rent capture.').

omega_variable(
    natural_vs_constructed_scarcity,
    'What proportion of land value is attributable to natural scarcity (fixed supply, inherent location advantage) versus constructed scarcity (zoning, planning restrictions, speculative hoarding)?',
    'Detailed econometric analysis of land price determinants, disentangling natural attributes from regulatory and market-driven factors.',
    'A higher proportion of constructed scarcity would weaken the ''mountain'' aspect of land value, suggesting more degrees of freedom for policy intervention. A higher natural scarcity component would reinforce the inherent, unearned nature of land rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_scarcity, empirical, 'Distinguishing natural from constructed components of land value.').

omega_variable(
    georgist_framing_validity,
    'Is the Georgist distinction between land rent and improvement value a robust analytical framework, or does it oversimplify complex interactions between capital, labor, and location in modern economies?',
    'Comparative analysis with other economic theories of value and distribution, assessing its explanatory power for contemporary housing and wealth inequality.',
    'If the framework is robust, it provides a clear basis for identifying and addressing unearned extraction. If it oversimplifies, alternative readings (e.g., institutional, financialization) may offer more accurate diagnoses, potentially shifting the classification of the ''snare'' component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(georgist_framing_validity, conceptual, 'Validity of the Georgist analytical distinction in contemporary economics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__georgist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__georgist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__georgist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__georgist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__georgist_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__georgist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__georgist_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
