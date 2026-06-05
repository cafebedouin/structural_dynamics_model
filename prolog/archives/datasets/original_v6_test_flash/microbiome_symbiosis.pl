% ============================================================================
% CONSTRAINT STORY: microbiome_symbiosis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microbiome_symbiosis, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: microbiome_symbiosis
 *   human_readable: The Microbial-Immune Symbiosis
 *   domain: biological/healthcare
 *
 * SUMMARY:
 *   The relationship between the human host and their gut microbiome is a
 *   fundamental biological constraint characterized by mutualistic
 *   interactions. The gut microbiota benefits from a stable environment and
 *   constant nutrient supply within the host's digestive tract. The human
 *   host, in turn, receives essential nutrients, immune system modulation,
 *   and protection against pathogenic organisms from the diverse microbial
 *   community inhabiting their gut.
 *
 * KEY AGENTS:
 *   - Human Host: benefits from nutrient provision, immune system modulation, and pathogen defense (moderate/constrained)
 *   - Gut Microbiota: benefits from stable environment and constant nutrient supply (powerless/trapped)
 *   - Analytical Observer: identifies long-term coordination and co-evolutionary dynamics (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microbiome_symbiosis, 0.35).
domain_priors:suppression_score(microbiome_symbiosis, 0.15).
domain_priors:theater_ratio(microbiome_symbiosis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microbiome_symbiosis, extractiveness, 0.35).
narrative_ontology:constraint_metric(microbiome_symbiosis, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(microbiome_symbiosis, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microbiome_symbiosis, rope).
narrative_ontology:human_readable(microbiome_symbiosis, "The Microbial-Immune Symbiosis").
narrative_ontology:topic_domain(microbiome_symbiosis, "biological/healthcare").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, human_host).
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, gut_microbiota).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a single bacterium, the symbiosis provides a stable environment and nutrient source, representing a pure coordination (Rope) that enhances survival and reproduction.
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% From the perspective of the human host, the symbiosis provides essential nutrients, immune system modulation, and protection against pathogens. While constrained by the need to maintain a healthy gut environment, the overall relationship is one of coordination (Rope).
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% Viewed analytically over a civilizational timescale, the microbial-immune symbiosis represents a fundamental coordination (Rope) that has shaped the evolution of both the human host and their gut microbiota. Occasional dysbiosis exists, but overall represents a coordinated relationship.
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microbiome_symbiosis_tests).
:- end_tests(microbiome_symbiosis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low, reflecting the mutually beneficial nature of the symbiosis. Both host and microbiota benefit, but extraction occurs due to resource competition between bacterial species. Suppression (0.15): Low, reflecting the ability of the host to alter the microbiome composition through diet and lifestyle choices, and the relative freedom of bacterial species to compete. Theater ratio (0.20): Low, indicating a functional relationship with minimal performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The apparent consensus across perspectives (all rope) reflects the deep evolutionary integration of this symbiosis. Any 'extraction' involved is relatively symmetric — all participants gain net benefit from coordination. More complex scenarios involving opportunistic pathogens can be modeled as separate constraints that contaminate the symbiosis.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the human host and the gut microbiota. Each perspective aligns on this benefit, reflecting the long history of co-evolution and mutualism. Directionality is computed symmetrically, because both parties are beneficiaries. An analytical observer would still classify as rope because both parties are receiving a net positive benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The absence of a strong extraction element (relative symmetry of benefits) allows clear differentiation from Tangled Rope and Snare. The classification prevents mislabeling this coordination as pure extraction, as the interdependence and mutual benefits of the symbiosis are well-established.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microbiome_symbiosis, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microbiome_symbiosis, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
