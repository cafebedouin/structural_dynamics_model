% ============================================================================
% CONSTRAINT STORY: jupiter_composition_knowledge_gap
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_jupiter_composition_knowledge_gap, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jupiter_composition_knowledge_gap
 *   human_readable: Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models
 *   domain: technological
 *
 * SUMMARY:
 *   The imprecise knowledge of Jupiter's core composition, particularly the abundance of heavy elements, constrains the development of accurate planetary formation models. This knowledge gap limits our understanding of the early solar system and the processes that govern gas giant formation. It creates a dynamic where solving the problem (coordination) is beneficial for institutions, while the existence of the problem (extraction) hinders individual researchers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Early-Career Researchers: Primary target (powerless/trapped) — careers are constrained by the data gap, unable to direct new missions.
 *   - Established Planetary Scientists: Secondary target (moderate/constrained) — research is limited by the lack of precise data.
 *   - Space Agencies (NASA, ESA): Primary beneficiary (institutional/arbitrage) — resource allocation and mission planning benefit from the knowledge pursuit.
 *   - Analytical Observer: Sees the full structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jupiter_composition_knowledge_gap, 0.35).
domain_priors:suppression_score(jupiter_composition_knowledge_gap, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(jupiter_composition_knowledge_gap, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, extractiveness, 0.35).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(jupiter_composition_knowledge_gap, tangled_rope).
narrative_ontology:human_readable(jupiter_composition_knowledge_gap, "Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models").

% --- Binary flags ---
domain_priors:requires_active_enforcement(jupiter_composition_knowledge_gap). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, space_agencies).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, planetary_scientists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (POWERLESS)
% Early-career researchers whose work is blocked by the data gap.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ESTABLISHED SCIENTIST
% Planetary scientists lacking precise data but with some agency.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% Space Agencies benefiting from resource allocation and mission planning.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jupiter_composition_knowledge_gap_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, ExtMetricName, E),
    E < 0.46, % Ensure it's not classified as a snare.
    narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, suppression_requirement, S),
    S >= 0.40. % Ensure it meets the tangled_rope suppression threshold.

:- end_tests(jupiter_composition_knowledge_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.35) reflects the limitations imposed on scientific progress by the current lack of knowledge. The suppression score (0.45) is set to reflect the high technological and financial barriers to obtaining new data, which effectively suppresses the alternative of direct measurement. This meets the threshold for a Tangled Rope. The theater ratio (0.10) is low, indicating that current efforts are primarily focused on genuine research.
 *
 *   The constraint requires active enforcement to be a Tangled Rope. This 'enforcement' is structural, enacted by the scientific community itself: peer review processes and funding bodies require that new models are consistent with existing, limited data, thereby reinforcing the constraints imposed by the knowledge gap.
 *
 * PERSPECTIVAL GAP:
 *   Researchers, especially early-career ones (powerless/trapped), experience the knowledge gap as a significant barrier (Tangled Rope) that extracts from their ability to produce novel work. In contrast, space agencies (institutional/arbitrage) perceive it as a pure coordination problem (Rope), as it provides a clear justification for funding new missions and technologies, thus benefiting the institution.
 *
 * DIRECTIONALITY LOGIC:
 *   Space agencies are beneficiaries because the constraint drives mission planning and resource allocation towards Jupiter-related research. Planetary scientists are the victims as they lack the precise data necessary for their work, with early-career researchers being the most trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification correctly identifies that this is not a simple coordination problem. While there is a genuine coordination function (organizing missions to gather data), there is also asymmetric extraction where the careers and research of scientists are hindered by the very problem that provides institutional justification for the beneficiaries. This prevents mislabeling the situation as a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jupiter_comp,
    'Can future missions accurately determine the core composition of Jupiter?',
    'New observational data from advanced space probes and improved computational models.',
    'If true, the knowledge gap will be significantly reduced, leading to more accurate planetary formation models. If false, the knowledge gap persists, requiring alternative theoretical frameworks.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(jupiter_composition_knowledge_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required as base_extractiveness < 0.46, but
% is included to model the constraint's stability over the interval.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(jupiter_comp_tr_t0, jupiter_composition_knowledge_gap, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jupiter_comp_tr_t5, jupiter_composition_knowledge_gap, theater_ratio, 5, 0.10).
narrative_ontology:measurement(jupiter_comp_tr_t10, jupiter_composition_knowledge_gap, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(jupiter_comp_ex_t0, jupiter_composition_knowledge_gap, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(jupiter_comp_ex_t5, jupiter_composition_knowledge_gap, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(jupiter_comp_ex_t10, jupiter_composition_knowledge_gap, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(jupiter_composition_knowledge_gap, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% groups and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */