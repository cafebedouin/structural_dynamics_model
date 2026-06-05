% ============================================================================
% CONSTRAINT STORY: jupiter_composition_knowledge_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 *   The imprecise knowledge of Jupiter's core composition, particularly the
 *   abundance of heavy elements, constrains the development of accurate
 *   planetary formation models. This lack of precision acts as a tangled
 *   rope, both enabling certain models to persist unchallenged and hindering
 *   the progress of alternative theories that require more specific data for
 *   validation. The constraint manifests differently for different actors,
 *   depending on their vested interests and analytical positions.
 *
 * KEY AGENTS:
 *   - Alternative Planetary Formation Models: Primary victim (powerless/trapped) - Suppressed due to lack of precise data.
 *   - Researchers Relying on Precise Models: Secondary victim (moderate/constrained) - Limited by current model's limitations, but benefit from innovation.
 *   - Proponents of Core Accretion Model: Primary beneficiary (institutional/arbitrage) - Model remains viable due to existing knowledge gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jupiter_composition_knowledge_gap, 0.55).
domain_priors:suppression_score(jupiter_composition_knowledge_gap, 0.65).
domain_priors:theater_ratio(jupiter_composition_knowledge_gap, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, extractiveness, 0.55).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jupiter_composition_knowledge_gap, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jupiter_composition_knowledge_gap, tangled_rope).
narrative_ontology:human_readable(jupiter_composition_knowledge_gap, "Knowledge Gap in Jupiter's Composition Affecting Planetary Formation Models").
narrative_ontology:topic_domain(jupiter_composition_knowledge_gap, "technological").

domain_priors:requires_active_enforcement(jupiter_composition_knowledge_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jupiter_composition_knowledge_gap, proponents_core_accretion_model).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, alternative_planetary_formation_models).
narrative_ontology:constraint_victim(jupiter_composition_knowledge_gap, researchers_relying_precise_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE PLANETARY FORMATION MODELS (SNARE) - Models that deviate from core accretion are suppressed due to lack of precise compositional data from Jupiter. These models are trapped as they cannot effectively compete without better data.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCHERS RELYING ON PRECISE MODELS (TANGLED ROPE) - Researchers are constrained by the limitations in current models but also benefit from the challenge, driving innovation. They are constrained by needing Jupiter models as inputs, yet benefit by advancing novel methods to work around data gaps.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROPONENTS OF CORE ACCRETION MODEL (ROPE) - Benefit from the existing knowledge gap, as it allows their models to remain viable despite inconsistencies. They can 'arbitrage' the uncertainty by tweaking parameters to fit observations.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: LEGACY PLANETARY FORMATION MODELS (PITON) - Existing models are maintained despite their known limitations, showing inertial behavior. The 'theater' consists of continued use and refinement despite limited improvement in predictive power. The models are used due to a lack of viable alternatives and significant investment in their development.
constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jupiter_composition_knowledge_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jupiter_composition_knowledge_gap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jupiter_composition_knowledge_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jupiter_composition_knowledge_gap, TR),
    TR >= 0.70.

:- end_tests(jupiter_composition_knowledge_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Moderate extractiveness reflects the limited progress in refining planetary formation models due to data gaps, hindering the advancement of alternative models. Suppression: 0.65 - High suppression stems from the challenges in acquiring precise data and the persistence of legacy models. Theater Ratio: 0.75 - The continued use and refinement of legacy models, despite limited improvement in predictive power, constitutes a significant performative aspect. This is driven by a lack of viable alternatives and significant investment in their development.
 *
 * PERSPECTIVAL GAP:
 *   The proponents of core accretion see a rope, as it allows their model to persist. Researchers face a tangled rope, where they are both constrained and enabled by this lack of knowledge. Alternative models face a snare as it suppresses their development. The analytical perspective identifies a piton, where legacy models remain despite limitations.
 *
 * DIRECTIONALITY LOGIC:
 *   Proponents of Core Accretion benefit (low d), Alternative Models are harmed (high d), and Researchers have a mixed experience (moderate d). The extractiveness from knowledge limitations flows toward the Legacy Models, allowing them to stay viable due to the persistent knowledge gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The knowledge gap is correctly classified as a tangled rope. Mislabeling as a pure extraction (snare) would ignore the role it plays in driving research and innovation. Mislabeling as a pure coordination (rope) would ignore how this gap sustains a dominant, potentially flawed model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heavy_element_abundance_uncertainty,
    'What is the true abundance of heavy elements in Jupiter''s core?',
    'Improved gravity field measurements by Juno, future missions to directly sample Jupiter''s atmosphere.',
    'Resolving this uncertainty would either validate or invalidate the core accretion model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heavy_element_abundance_uncertainty, empirical, 'Uncertainty in the abundance of heavy elements in Jupiter''s core.').

omega_variable(
    equation_of_state_reliability,
    'How reliable are the equations of state used to model Jupiter''s interior?',
    'High-pressure experiments on hydrogen and helium mixtures.',
    'More reliable equations of state would constrain the possible core compositions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equation_of_state_reliability, empirical, 'Equation of state reliability').

omega_variable(
    magnetic_field_influence_assessment,
    'What role does Jupiter''s magnetic field play in planetary formation?',
    'Improved magnetohydrodynamic simulations.',
    'Understanding this role could reconcile differences between models.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magnetic_field_influence_assessment, conceptual, 'Magnetic Field Influence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jupiter_composition_knowledge_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jupi_tr_t0, jupiter_composition_knowledge_gap, theater_ratio, 0, 0.6).
narrative_ontology:measurement(jupi_tr_t5, jupiter_composition_knowledge_gap, theater_ratio, 5, 0.7).
narrative_ontology:measurement(jupi_tr_t10, jupiter_composition_knowledge_gap, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(jupi_be_t0, jupiter_composition_knowledge_gap, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jupi_be_t5, jupiter_composition_knowledge_gap, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(jupi_be_t10, jupiter_composition_knowledge_gap, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jupiter_composition_knowledge_gap, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
