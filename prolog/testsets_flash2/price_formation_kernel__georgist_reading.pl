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
 *   human_readable: Georgist Reading of Price Formation: Land Rent vs. Improvement Value
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the Georgist reading of price formation, which
 *   posits a fundamental distinction between land rent (unearned income from
 *   location scarcity) and improvement value (earned income from labor and
 *   capital). From this perspective, the land component of price formation is
 *   akin to a Mountain (fixed supply), but its private capture is a Snare,
 *   extracting from productive activity. The improvement component, however,
 *   functions more like a Rope, coordinating productive effort. The overall
 *   constraint is classified as a Tangled Rope due to the hybrid nature and
 *   active enforcement required to maintain private land rent capture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__georgist_reading, 0.65).
domain_priors:suppression_score(price_formation_kernel__georgist_reading, 0.7).
domain_priors:theater_ratio(price_formation_kernel__georgist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__georgist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__georgist_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__georgist_reading, "Georgist Reading of Price Formation: Land Rent vs. Improvement Value").
narrative_ontology:topic_domain(price_formation_kernel__georgist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__georgist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__georgist_reading, '1f3f265f-9bed-462f-a5c3-101d162b7121').
narrative_ontology:cs_kernel_codification('1f3f265f-9bed-462f-a5c3-101d162b7121', distributed).
narrative_ontology:cs_authority_grounding('1f3f265f-9bed-462f-a5c3-101d162b7121', practice).
narrative_ontology:cs_interpretation_layer_present('1f3f265f-9bed-462f-a5c3-101d162b7121').
narrative_ontology:cs_reading_relation('1f3f265f-9bed-462f-a5c3-101d162b7121', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f3f265f-9bed-462f-a5c3-101d162b7121', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f3f265f-9bed-462f-a5c3-101d162b7121', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('1f3f265f-9bed-462f-a5c3-101d162b7121', foundational, land_value_is_unearned_rent).
narrative_ontology:cs_axiom_status(land_value_is_unearned_rent, holdable).
narrative_ontology:cs_axiom_grounding('1f3f265f-9bed-462f-a5c3-101d162b7121', land_value_is_unearned_rent, deontological).
narrative_ontology:cs_axiom('1f3f265f-9bed-462f-a5c3-101d162b7121', foundational, improvements_are_earned_capital_labor).
narrative_ontology:cs_axiom_status(improvements_are_earned_capital_labor, holdable).
narrative_ontology:cs_axiom_grounding('1f3f265f-9bed-462f-a5c3-101d162b7121', improvements_are_earned_capital_labor, deontological).
narrative_ontology:cs_reference_frame('1f3f265f-9bed-462f-a5c3-101d162b7121', classical_economic_distinction).
narrative_ontology:cs_drift_state('1f3f265f-9bed-462f-a5c3-101d162b7121', contemporary_financialized_economy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f3f265f-9bed-462f-a5c3-101d162b7121', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__georgist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, landowners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__georgist_reading, real_estate_investors).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, labor_capital_producers).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__georgist_reading, homebuyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects land rent, which is seen as unearned income derived from location scarcity and public investment, not from their own labor or capital improvements. Benefits from rising land values without productive effort.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, landowners, beneficiary,
    powerful, generational, arbitrage, local).

% Profit from the appreciation of land values and the ability to leverage land as an asset. Their investment strategies often prioritize land speculation over productive development, benefiting from the existing price formation mechanism.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, real_estate_investors, beneficiary,
    organized, biographical, mobile, regional).

% Bear the cost of land rent, which reduces the return on their labor and capital invested in improvements. They are forced to pay for access to location, diminishing their productive gains.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, labor_capital_producers, payer,
    moderate, biographical, constrained, local).

% Pay a significant portion of their income as rent, which includes both the cost of improvements and the unearned land rent. Their ability to access housing is directly impacted by the separation and capture of land value.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, renters, payer,
    powerless, immediate, constrained, local).

% Must purchase land at prices inflated by speculative land rent, increasing the barrier to homeownership. They pay for location value that is not a product of labor or capital.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, homebuyers, payer,
    moderate, biographical, constrained, local).

% Analyze and advocate for policies (like Land Value Tax) that would capture land rent for public benefit, thereby distinguishing it from earned improvement value. They seek to reform the current price formation mechanism.
narrative_ontology:constraint_stakeholder(price_formation_kernel__georgist_reading, georgist_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The price formation mechanism coordinates the allocation of land and improvements in a market economy, signaling demand and supply for both location and constructed value.
% TRANSFER_FUNCTION: Transfers unearned economic rent, derived from location and public investment, from labor and capital producers (including renters and homebuyers) to private landowners and real estate investors.
% ABSENT_VOICES: Future generations, who will inherit a system where land value is privately captured, are absent. Also, those who would benefit from public capture of land rent (e.g., for public services or reduced taxes on labor/capital) are not directly represented in the current mechanism's design.
% DISAPPEARANCE_RATIONALE: If the current price formation mechanism, particularly the private capture of land rent, vanished overnight, the entire structure of property ownership, taxation, and investment in real estate would fundamentally reorganize. Land values would plummet, investment would shift towards productive improvements, and public finance would need a new basis.
% FOUNDING_PROBLEM: The problem of allocating scarce land resources and incentivizing productive improvements in a growing economy, while ensuring fair distribution of wealth.
% FOUNDING_PROBLEM_CORROBORATION: Landowners and real estate investors argue the current system efficiently allocates resources and rewards risk. Georgist advocates and some economists (outside the benefiting parties) argue the system fails to distinguish earned from unearned income, leading to inequality and speculative bubbles, indicating the founding problem of fair distribution remains unsolved or exacerbated.
narrative_ontology:disappearance_verdict(price_formation_kernel__georgist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__georgist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__georgist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high because a significant portion of economic value (land rent) is captured without productive effort, diverting wealth from labor and capital. Suppression is also high, as the legal and institutional framework actively enforces private land ownership and the right to collect rent, suppressing alternatives like public capture of land value. Theater ratio is low because the system is genuinely functional in allocating land, even if its distributive outcomes are contested. The increasing extractiveness and suppression over time reflect the growing importance of land as an asset and the hardening of legal protections for its private capture.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of landowners, the system is a natural and fair mechanism for property rights and wealth accumulation. From the perspective of labor/capital producers, it is an extractive system that siphons off their earned income. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Landowners and real estate investors are clear beneficiaries, as they collect unearned rent (low directionality). Labor/capital producers, renters, and homebuyers are targets, as they bear the cost of this extraction (high directionality). Georgist advocates act as observers, analyzing the system's mechanics and advocating for reform.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_value_attribution,
    'To what extent is land value truly ''unearned'' versus a reflection of entrepreneurial risk or prior investment in development?',
    'Detailed economic analysis separating the contribution of location, public infrastructure, and private development to land value appreciation over time, across diverse geographies.',
    'If a significant portion of land value is found to be attributable to private entrepreneurial effort, the ''snare'' component of the Georgist reading would be weakened, potentially shifting the classification towards a more ''rope''-like coordination function. If ''unearned'' value predominates, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_value_attribution, empirical, 'Ambiguity in attributing land value to unearned rent versus earned returns.').

omega_variable(
    georgist_policy_feasibility,
    'Is a comprehensive Land Value Tax (LVT) politically and practically feasible to implement at scale, given existing property rights and institutional inertia?',
    'Pilot programs and case studies of LVT implementation in various jurisdictions, assessing political resistance, administrative costs, and economic impacts.',
    'If LVT proves infeasible, the ''snare'' aspect of private land rent capture might be re-evaluated as a more ''mountain''-like constraint, reflecting an irreducible institutional barrier rather than a remediable extraction. If feasible, it reinforces the ''tangled_rope'' classification by demonstrating a viable alternative to the extractive component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(georgist_policy_feasibility, preference, 'Political and practical feasibility of Georgist policy solutions.').

omega_variable(
    kernel_reading_divergence,
    'How do the structural properties of price formation differ across the Georgist, Naturalist, Institutional, and Financialization readings?',
    'Comparative analysis of constraint stories for each reading, focusing on differences in extractiveness, suppression, beneficiaries, and victims, as well as the underlying causal mechanisms each reading emphasizes.',
    'Significant divergence would confirm the utility of the kernel/reading framework for disambiguating complex economic phenomena. Minimal divergence would suggest the readings are largely perspectival rather than structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between this Georgist reading and sibling readings of price formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__georgist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__georgist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__georgist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__georgist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__georgist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__georgist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(pric_tr_t50, price_formation_kernel__georgist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__georgist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__georgist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__georgist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__georgist_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__georgist_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(pric_be_t50, price_formation_kernel__georgist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__georgist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__georgist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__georgist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__georgist_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__georgist_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(pric_su_t50, price_formation_kernel__georgist_reading, suppression_requirement, 50, 0.7).


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
