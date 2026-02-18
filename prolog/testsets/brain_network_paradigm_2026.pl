% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_brain_network_paradigm_2026, []).

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
 *   constraint_id: brain_network_paradigm_2026
 *   human_readable: Distributed Brain Network Scientific Paradigm
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the dominant scientific paradigm in neuroscience (c. 2026) that
 *   views the brain as a series of interconnected, overlapping networks rather than a collection
 *   of discrete, specialized modules. This paradigm coordinates research and funding, but also
 *   extracts career opportunities and prestige from scientists invested in the older, modular
 *   view. Behaviors are seen as emergent from synchronized activity across systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Legacy Modularist Neuroscientists: Primary target (moderate/constrained) — Their research paradigm is suppressed, and professional resources are redirected away from them.
 *   - Network Neuroscientists: Primary beneficiary (institutional/arbitrage) — The paradigm coordinates their research and funnels funding and prestige to their work.
 *   - Patients with Brain Injuries: Secondary beneficiary (powerless/trapped) — The paradigm provides a better framework for understanding their condition, coordinating treatment.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.50).
domain_priors:suppression_score(brain_network_paradigm_2026, 0.70).
domain_priors:theater_ratio(brain_network_paradigm_2026, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.50).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(brain_network_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(brain_network_paradigm_2026, "Distributed Brain Network Scientific Paradigm").
narrative_ontology:topic_domain(brain_network_paradigm_2026, "technological/scientific").

% --- Binary flags ---
domain_priors:requires_active_enforcement(brain_network_paradigm_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, network_neuroscientists).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, patients_with_brain_injuries).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(brain_network_paradigm_2026, legacy_modularist_neuroscientists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent whose career and research paradigm is suppressed by the new model.
% Engine derives d from: victim membership + constrained exit -> high d -> high χ
constraint_indexing:constraint_classification(brain_network_paradigm_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent whose research is coordinated and amplified by the paradigm.
% Engine derives d from: beneficiary membership + arbitrage exit -> low d -> low/negative χ
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SECONDARY BENEFICIARY (ROPE)
% The patient, for whom the paradigm is a coordination tool for understanding their
% condition, even if they are trapped by their biological reality.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function for neuroscience and the asymmetric
% extraction of professional resources from the old guard.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brain_network_paradigm_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, snare, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope, context(agent_power(institutional), _, _, _)),
    true. % Test passes if both clauses unify.

test(threshold_validation) :-
    % Verify that the base extractiveness is in the high-extraction range required for Snare/Tangled Rope.
    narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, E),
    E >= 0.46.

:- end_tests(brain_network_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file's base_extractiveness of 0.0 was logically inconsistent with its
 *   claim of a 'Snare' perspective, as χ = ε * f(d) * σ(S) would always be zero.
 *   I have set base_extractiveness (ε) to 0.50 to model the tangible redirection of
 *   professional resources (funding, prestige, career paths) from the old paradigm to the new.
 *   Suppression is high (0.70) because a dominant scientific paradigm actively suppresses
 *   alternatives through peer review, funding allocation, and educational focus.
 *   The constraint requires active enforcement (via these institutional mechanisms) to maintain its dominance.
 *
 * PERSPECTIVAL GAP:
 *   - Network Neuroscientists (Beneficiaries) see a Rope: The paradigm is a powerful coordination
 *     tool that organizes research, standardizes terminology, and focuses inquiry, yielding
 *     significant scientific progress. The extractive component is invisible or seen as a necessary
 *     byproduct of progress.
 *   - Legacy Modularists (Victims) see a Snare: The paradigm invalidates their life's work,
 *     restricts their access to funding, and makes it difficult to publish dissenting views.
 *     For them, it is a coercive system that extracts their professional standing.
 *   - The Analytical Observer sees a Tangled Rope: It recognizes both the valid, powerful
 *     coordination function and the simultaneous, asymmetric extraction from a specific group.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `network_neuroscientists` and `patients_with_brain_injuries` are explicitly
 *     declared. The former have `arbitrage` exit (they can propose new models), leading to very low
 *     directionality (d). The latter have `trapped` exit, but their beneficiary status still
 *     results in a low d, classifying the paradigm as a helpful Rope.
 *   - Victims: `legacy_modularist_neuroscientists` are declared. Their `constrained` exit options
 *     and victim status result in a high d, amplifying the base extraction into a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   By assigning a non-zero ε, this model avoids mislabeling the paradigm as a pure coordination
 *   Rope. It correctly identifies that even beneficial scientific progress involves extraction,
 *   whereby resources and influence are taken from one group and given to another. The Tangled
 *   Rope classification prevents the system from seeing this as either pure, benign coordination
 *   or pure, malicious extraction, capturing the dual nature of paradigm shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_network_individuality,
    "Is every individual's brain network map unique (a 'Snare' for generalized treatments) or does a standard template exist (a 'Rope' for universal protocols)?",
    "Longitudinal high-resolution connectivity mapping of diverse populations; personalized medicine trials based on individual connectomes.",
    "If unique: Standardized medicine may fail. If template: Universal protocols remain viable.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(brain_network_paradigm_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the rise of the network paradigm from 2001-2026.
% As the paradigm gained acceptance, its ability to extract resources from the
% old paradigm increased.

% Theater ratio over time (remains low as it's a functional scientific model):
narrative_ontology:measurement(brain_network_paradigm_2026_tr_t0, brain_network_paradigm_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(brain_network_paradigm_2026_tr_t5, brain_network_paradigm_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(brain_network_paradigm_2026_tr_t10, brain_network_paradigm_2026, theater_ratio, 10, 0.10).

% Extraction over time (grows as the paradigm becomes dominant):
narrative_ontology:measurement(brain_network_paradigm_2026_ex_t0, brain_network_paradigm_2026, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(brain_network_paradigm_2026_ex_t5, brain_network_paradigm_2026, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(brain_network_paradigm_2026_ex_t10, brain_network_paradigm_2026, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(brain_network_paradigm_2026, information_standard).

% Network relationships: The scientific paradigm is enabled by and attempts to
% describe the underlying physical reality of the brain's network structure.
narrative_ontology:affects_constraint(brain_network_physical_structure, brain_network_paradigm_2026).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics of this
% scientific paradigm shift.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */