% ============================================================================
% CONSTRAINT STORY: treaty_land_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_land_entrenchment, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_land_entrenchment
 *   human_readable: Treaty Sovereignty (The Crown-Indigenous Mountain)
 *   domain: legal/political
 *
 * SUMMARY:
 *   Alberta exists on Treaty 6, 7, and 8 lands. Treaty sovereignty is a
 *   Mountain because the parties' commitments are very resistant to change,
 *   and perceived as such.
 *
 * KEY AGENTS:
 *   - Indigenous Peoples: Primary holders of Treaty Rights (powerless/trapped)
 *   - The Crown (Government of Canada): Treaty signatory and responsible for upholding Treaty Rights (institutional/constrained)
 *   - Analytical Observer: Legal scholars, historians, and political scientists (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_land_entrenchment, 0.05).
domain_priors:suppression_score(treaty_land_entrenchment, 0.01).
domain_priors:theater_ratio(treaty_land_entrenchment, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_land_entrenchment, extractiveness, 0.05).
narrative_ontology:constraint_metric(treaty_land_entrenchment, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(treaty_land_entrenchment, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_land_entrenchment, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(treaty_land_entrenchment, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_land_entrenchment, mountain).
narrative_ontology:human_readable(treaty_land_entrenchment, "Treaty Sovereignty (The Crown-Indigenous Mountain)").
narrative_ontology:topic_domain(treaty_land_entrenchment, "legal/political").

domain_priors:emerges_naturally(treaty_land_entrenchment).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of Indigenous peoples, treaty rights, particularly those pertaining to land, are seen as inherent and inalienable. These rights are deeply connected to their culture, spirituality, and way of life. They are a mountain, resistant to change, and fundamentally define their relationship with the land.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The Crown views treaties as foundational agreements establishing a legal and political framework for coexistence. While governments may interpret or adjust policies within these frameworks, the core principle of treaty rights as they pertain to land remains a fundamental legal principle. The government is constrained, but it recognizes the mountain.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the enduring nature of treaty rights stems from a combination of legal precedent, historical context, and ongoing societal values. The underlying principle that treaties constitute binding agreements regarding land and sovereignty is treated as a constant in legal and political analysis.
constraint_indexing:constraint_classification(treaty_land_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_land_entrenchment_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(treaty_land_entrenchment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(treaty_land_entrenchment, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, ExtMetricName, E),
    domain_priors:suppression_score(treaty_land_entrenchment, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(treaty_land_entrenchment),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(treaty_land_entrenchment, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(treaty_land_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Low. The treaties are meant to protect Indigenous rights, not extract from them. Suppression (0.01): Extremely Low. The treaties are intended to provide a framework for co-existence, not to suppress Indigenous sovereignty. Theater Ratio (0.02): Very Low. There is some performative action around treaty rights, but it is substantially overshadowed by the core legal and ethical commitments.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the Mountain classification, reflecting the enduring nature of treaty rights related to land. Although interpretations and implementations may vary, the fundamental principle of treaty sovereignty remains a constant across different viewpoints.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown benefits from the treaties as they establish a legal framework for coexistence and governance. Indigenous peoples are intended to be the beneficiaries, though the history of treaty implementation is complex. The Analytical Observer perceives the enduring legal and political significance of the treaties.
 *
 * MANDATROPHY ANALYSIS:
 *   N/A - Mountain
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_land_entrenchment, 1876, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_land_entrenchment, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
