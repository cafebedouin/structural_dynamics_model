% ============================================================================
% CONSTRAINT STORY: brazil_hiv_vtn_elimination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_hiv_vtn_elimination, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazil_hiv_vtn_elimination
 *   human_readable: Brazil's Program for Eliminating Vertical HIV Transmission
 *   domain: social / public_health
 *
 * SUMMARY:
 *   This constraint story models Brazil's national public health program to
 *   eliminate mother-to-child (vertical) transmission of HIV. Anchored in the
 *   universal Unified Health System (SUS), the program provides free testing
 *   and antiretroviral treatment to pregnant women, reducing transmission
 *   rates to below 2% and earning WHO validation. It represents a highly
 *   successful, large-scale coordination effort that serves as a global
 *   model. The structure is overwhelmingly that of a Rope, a public good that
 *   aligns the actions of individuals and institutions for a collective
 *   benefit.
 *
 * KEY AGENTS:
 *   - Mothers with HIV: Primary beneficiaries who receive care (moderate/constrained).
 *   - Infants: The ultimate beneficiaries, protected from infection (powerless/trapped).
 *   - Brazilian Public Health System (SUS): The institutional actor implementing the coordination (institutional/arbitrage).
 *   - Women in Underserved Regions: Victims of implementation gaps and inequality, experiencing the system's failures (powerless/trapped).
 *   - Brazilian Taxpayers: Fund the program, bearing the distributed cost of the public good.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_hiv_vtn_elimination, 0.12).
domain_priors:suppression_score(brazil_hiv_vtn_elimination, 0.35).
domain_priors:theater_ratio(brazil_hiv_vtn_elimination, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, extractiveness, 0.12).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_hiv_vtn_elimination, rope).
narrative_ontology:human_readable(brazil_hiv_vtn_elimination, "Brazil's Program for Eliminating Vertical HIV Transmission").
narrative_ontology:topic_domain(brazil_hiv_vtn_elimination, "social / public_health").

domain_priors:requires_active_enforcement(brazil_hiv_vtn_elimination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, infants_born_hiv_free).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, mothers_with_hiv).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, brazilian_public_health_system).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, brazilian_society).
narrative_ontology:constraint_victim(brazil_hiv_vtn_elimination, women_in_underserved_regions).
narrative_ontology:constraint_victim(brazil_hiv_vtn_elimination, brazilian_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NEWBORN INFANT (ROPE) — The ultimate beneficiary. The program is a pure subsidy, coordinating medical interventions to prevent infection. There is no extraction from this perspective; it is a life-saving public good. As a primary beneficiary, d is minimal, leading to negative effective extraction (χ < 0).
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MOTHER WITH HIV (ROPE) — A primary beneficiary who receives free, universal access to testing, counseling, and life-saving antiretroviral therapy. The constraint is the set of protocols she must follow, but this is a pure coordination function for her and her child's health. Exit is constrained by her medical needs, but her agency is moderate. d is low, χ is low.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PUBLIC HEALTH SYSTEM (ROPE) — The institutional actor implementing the program. It experiences the constraint as a highly effective coordination tool to achieve a major public health goal, reduce future healthcare costs, and gain international recognition. d is minimal as a beneficiary, χ is negative.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MOTHER IN UNDERSERVED REGION (TANGLED ROPE) — While a nominal beneficiary, she is a victim of implementation gaps due to regional inequality. She bears costs (travel, lost time, stigma, inconsistent care) that are not fully compensated by the program's benefits in her location. For her, the system has an extractive component layered on its coordination function. d is higher due to victim status, pushing χ into the tangled_rope range.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (ROPE) — From a global, long-term perspective, the program is a textbook example of a successful public health Rope. It solves a complex coordination problem with minimal extraction and massive social benefit. The analytical view acknowledges the implementation gaps (Tangled Rope perspectives) but classifies the overall structure by its dominant function and intent.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_hiv_vtn_elimination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(brazil_hiv_vtn_elimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.12) is very low. The program is a net subsidy to its target population. The small ε value accounts for indirect costs like time, travel, and the non-monetary cost of adherence to medical protocols. Suppression (0.35) is moderate, reflecting strong public health guidance that discourages alternatives (like not testing or treating) which have severe negative consequences. It's a suppression of harmful choices, not viable alternatives. Theater Ratio (0.10) is very low; the program is highly functional and outcome-driven, as confirmed by WHO validation.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between those who experience the program's ideal implementation (Rope) and those who are victims of its structural inequalities (Tangled Rope). For a mother in a major city with full access, the program is a pure public good. For a mother in a remote Amazonian state with inconsistent access to clinics and medication, the program's demands can feel extractive, as she bears significant personal costs to comply with a system that doesn't fully deliver on its promise. This demonstrates how even a low-ε Rope can generate pockets of extraction due to systemic friction and inequality.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (mothers, infants, SUS) have a very low directionality (d), resulting in low or negative effective extraction (χ), confirming the Rope classification. The victims of inequality (women in underserved regions) are declared as such, leading to a higher derived d. This higher d, when multiplied by the base ε, is sufficient to push the effective extraction χ across the threshold into the Tangled Rope category from their specific perspective, accurately modeling their experience of a partially failed coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a clear example of a low-extraction public good, correctly classified as a Rope. It avoids the mandatrophy of mislabeling a beneficial coordination system as something sinister. The framework's ability to capture the 'Tangled Rope' perspective for marginalized groups shows its nuance: it can identify and measure the harms caused by implementation failures without mischaracterizing the fundamental nature of the overall constraint, which is benevolent coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_hiv_vtn_elimination, 1996, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_hiv_vtn_elimination, resource_allocation).
narrative_ontology:affects_constraint(brazil_hiv_vtn_elimination, brazil_universal_healthcare_sus).
narrative_ontology:affects_constraint(brazil_hiv_vtn_elimination, global_hiv_funding_mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
