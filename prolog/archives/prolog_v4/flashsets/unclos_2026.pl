% ============================================================================
% CONSTRAINT STORY: unclos_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_2026, []).

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
 *   constraint_id: unclos_2026
 *   human_readable: UN Convention on the Law of the Sea (2026 Context)
 *   domain: legal/geopolitical/environmental
 *
 * SUMMARY:
 *   The UN Convention on the Law of the Sea (UNCLOS) is a comprehensive legal
 *   framework governing all maritime activities. It establishes rules for
 *   navigation, resource management, environmental protection, and dispute
 *   resolution. While UNCLOS provides significant benefits, challenges remain
 *   in addressing emerging issues such as seabed mining, climate change, and
 *   the exploitation of marine genetic resources. The integration of the
 *   Biodiversity Beyond National Jurisdiction (BBNJ) agreement by 2026 seeks
 *   to address some of these gaps.
 *
 * KEY AGENTS:
 *   - Coastal States: Primary beneficiaries (institutional/constrained) - Defined maritime zones and resource rights.
 *   - Maritime Shipping Industry: Beneficiary (institutional/constrained) - Freedom of navigation and clear shipping lanes.
 *   - Scientific Research Community: Beneficiary (institutional/constrained) - Provisions for marine scientific research.
 *   - International Seabed Authority (ISA): Regulating body (institutional/constrained) - bureaucratic and subject to influence, hindering efficient resource management.
 *   - Analytical Observer: Perspective (analytical/analytical) - assesses long-term benefits and limitations of UNCLOS.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_2026, 0.35).
domain_priors:suppression_score(unclos_2026, 0.25).
domain_priors:theater_ratio(unclos_2026, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_2026, extractiveness, 0.35).
narrative_ontology:constraint_metric(unclos_2026, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(unclos_2026, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_2026, rope).
narrative_ontology:human_readable(unclos_2026, "UN Convention on the Law of the Sea (2026 Context)").
narrative_ontology:topic_domain(unclos_2026, "legal/geopolitical/environmental").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_2026, coastal_states).
narrative_ontology:constraint_beneficiary(unclos_2026, maritime_shipping_industry).
narrative_ontology:constraint_beneficiary(unclos_2026, scientific_research_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Coastal states benefit from UNCLOS by having clearly defined maritime zones, resource rights, and dispute resolution mechanisms. While they are constrained by the treaty's obligations, the overall framework is beneficial for managing their ocean resources.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The maritime shipping industry benefits from the freedom of navigation provisions and the establishment of clear shipping lanes, promoting trade and reducing uncertainty. They are constrained by regulations related to safety and environmental protection, but the overall framework facilitates global commerce.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The scientific research community benefits from the provisions allowing for marine scientific research, facilitating data collection and knowledge sharing. They are constrained by the need to obtain consent from coastal states in certain areas, but UNCLOS supports international collaboration in ocean exploration.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% From a civilizational perspective, UNCLOS is a crucial framework for managing ocean resources and resolving maritime disputes, promoting stability and cooperation among nations. The treaty's long-term benefits outweigh its limitations, making it a vital component of international law.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The ISA is responsible for regulating seabed mining in international waters. While initially intended to ensure equitable distribution of resources, it has become increasingly bureaucratic and subject to political influence, hindering efficient resource management. The theater_ratio reflects the performative aspects of compliance over functional benefit.
constraint_indexing:constraint_classification(unclos_2026, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_2026_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_2026, TR),
    TR >= 0.70.

:- end_tests(unclos_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   UNCLOS is a framework for managing ocean resources and activities. Extractiveness (0.35): UNCLOS does extract resources to promote global ocean stability, but not significantly. Suppression (0.25): The convention has a moderate degree of regulation. Theater Ratio (0.15): UNCLOS is not performative. The framework facilitates trade and navigation. The implementation is effective for addressing emerging issues like climate change and marine resources. BBNJ integration is crucial to ensure UNCLOS keeps pace with global ocean governance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives highlight the diverse interests and roles within the UNCLOS framework. Coastal states, the shipping industry, and the scientific community generally view UNCLOS positively, but the ISA and analytical observers recognize the challenges and limitations of the treaty. This difference in perspectives stems from the varying benefits and constraints experienced by each group.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent experiences UNCLOS differently. Coastal states and maritime industries benefit significantly through protection and regulation of marine activities (low d values). Observers from a scientific viewpoint and the global community acknowledge the constraints and limitations of UNCLOS. However, their insights and actions contribute to its effectiveness (intermediate to high d values).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_2026, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_2026, global_infrastructure).
narrative_ontology:affects_constraint(unclos_2026, bbnj_agreement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
