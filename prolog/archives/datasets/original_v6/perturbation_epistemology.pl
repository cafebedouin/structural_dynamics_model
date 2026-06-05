% ============================================================================
% CONSTRAINT STORY: perturbation_epistemology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perturbation_epistemology, []).

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
 *   constraint_id: perturbation_epistemology
 *   human_readable: Perturbation Epistemology Gap in Systems Biology
 *   domain: philosophy_of_science/systems_biology/epistemology
 *
 * SUMMARY:
 *   The perturbation epistemology gap is a fundamental constraint in systems
 *   biology and experimental genetics: perturbation experiments (knockouts,
 *   knockdowns, overexpression, pharmacological inhibition) can identify
 *   components that are causally necessary for a phenotype, but they
 *   systematically underdetermine the mechanistic role those components play
 *   in producing the phenotype. A gene knockout that abolishes a behavior
 *   tells you the gene is necessary, but not whether it encodes a
 *   transcription factor, a metabolic enzyme, a structural protein, or a
 *   signaling molecule. The ratio of identified components to mechanistic
 *   models in genomics literature is high and rising: GWAS studies identify
 *   thousands of disease-associated loci; CRISPR screens identify hundreds of
 *   essential genes for specific phenotypes; yet mechanistic models lag far
 *   behind. This is not a temporary technological gap but a logical property
 *   of perturbation-based inference. The constraint is a mountain from all
 *   perspectives because it emerges from the structure of causal inference
 *   itself, not from institutional arrangements, resource limitations, or
 *   coordination failures. No agent benefits from the constraint; no agent is
 *   extracted from. It is a natural law of experimental epistemology.
 *
 * KEY AGENTS:
 *   - Experimental Biologist: Powerless/trapped — directly confronts the gap in daily research; cannot exit the constraint
 *   - Genomics Consortium: Institutional/arbitrage — has resources to scale perturbation experiments but encounters the same epistemic limit
 *   - Systems Biology Community: Organized/constrained — collective effort to develop orthogonal methods (imaging, biochemistry, structural biology) that complement perturbation data
 *   - Pharmaceutical Researcher: Moderate/mobile — can switch between targets but each target presents the same mechanistic underdetermination
 *   - Epistemological Observer: Analytical/analytical — recognizes the constraint as a logical property of inference from perturbation data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perturbation_epistemology, 0.08).
domain_priors:suppression_score(perturbation_epistemology, 0.03).
domain_priors:theater_ratio(perturbation_epistemology, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perturbation_epistemology, extractiveness, 0.08).
narrative_ontology:constraint_metric(perturbation_epistemology, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(perturbation_epistemology, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(perturbation_epistemology, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(perturbation_epistemology, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perturbation_epistemology, mountain).
narrative_ontology:human_readable(perturbation_epistemology, "Perturbation Epistemology Gap in Systems Biology").
narrative_ontology:topic_domain(perturbation_epistemology, "philosophy_of_science/systems_biology/epistemology").

domain_priors:emerges_naturally(perturbation_epistemology).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL BIOLOGIST (MOUNTAIN) — Faces the irreducible gap between component identification and mechanistic understanding. Knockout experiments reveal necessity but not function. The constraint is experienced as a fundamental limit of perturbation-based inference.
constraint_indexing:constraint_classification(perturbation_epistemology, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: GENOMICS CONSORTIUM (MOUNTAIN) — Large-scale gene identification projects encounter the same epistemic limit. GWAS identifies associations; CRISPR screens identify necessary genes; neither yields mechanistic models. The gap persists regardless of scale or resources.
constraint_indexing:constraint_classification(perturbation_epistemology, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEMS BIOLOGY COMMUNITY (MOUNTAIN) — Organized effort to bridge the gap through computational modeling, network inference, and dynamical systems theory. Yet the fundamental constraint remains: perturbation reveals causal necessity but underdetermines mechanism. Multiple mechanistic models can produce identical perturbation phenotypes.
constraint_indexing:constraint_classification(perturbation_epistemology, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMOLOGICAL OBSERVER (MOUNTAIN) — The constraint is a logical property of inference from perturbation data. Necessity does not entail sufficiency; causal involvement does not specify mechanism. This is not a technological limitation but a structural feature of how perturbation experiments constrain the space of possible explanations. The gap is irreducible to the inference method itself.
constraint_indexing:constraint_classification(perturbation_epistemology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: PHARMACEUTICAL RESEARCHER (MOUNTAIN) — Drug target identification faces the same constraint. A gene knockout that rescues a phenotype identifies a necessary component but does not reveal whether the gene product is an enzyme, scaffold, regulator, or structural element. Mechanistic understanding requires orthogonal methods beyond perturbation.
constraint_indexing:constraint_classification(perturbation_epistemology, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perturbation_epistemology_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(perturbation_epistemology, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perturbation_epistemology, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(perturbation_epistemology, ExtMetricName, E),
    domain_priors:suppression_score(perturbation_epistemology, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(perturbation_epistemology),
    narrative_ontology:constraint_metric(perturbation_epistemology, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(perturbation_epistemology, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(perturbation_epistemology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes cognitive and experimental costs (researchers must use multiple orthogonal methods to build mechanistic models), but these are inherent costs of knowledge acquisition, not asymmetric extraction. No agent captures rents from the constraint's existence. The small non-zero value reflects the real resource cost of the additional experiments required. Suppression (0.03): Negligible. Alternative inference methods exist (structural biology, biochemistry, live-cell imaging, computational modeling) and are widely accessible. The constraint does not suppress alternatives; it defines the limits of what perturbation experiments alone can tell you. Researchers are free to use complementary methods. Theater ratio (0.15): Very low. Perturbation experiments are genuinely informative — they establish causal necessity, which is a real and valuable form of knowledge. The gap between necessity and mechanism is not theatrical; it is a structural feature of the inference. Accessibility collapse (0.92): Very high. The constraint is universally accessible to anyone performing perturbation experiments. The epistemic gap appears immediately upon attempting to infer mechanism from knockout phenotypes. Resistance (0.08): Very low. The constraint cannot be overcome by effort, resources, or ingenuity within the perturbation framework. Orthogonal methods are required, which is acceptance of the constraint rather than resistance to it.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in this constraint. All five perspectives classify as mountain. The experimental biologist with no resources, the genomics consortium with vast resources, the organized systems biology community, the pharmaceutical researcher, and the analytical epistemological observer all encounter the same irreducible limit. This uniformity is the signature of a genuine natural law constraint: it is invariant across power, time, exit options, and scope. The constraint does not depend on institutional arrangements (no one enforces it), resource distribution (money does not solve it), or coordination failures (collaboration does not eliminate it). It is a logical property of perturbation-based causal inference. The lack of perspectival gap is itself diagnostic — it confirms that the constraint is not a contingent social arrangement being naturalized but an actual structural limit of the inference method.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims, so directionality derivation does not apply. All agents experience the constraint as an immutable epistemic limit. The constraint is not a flow of extraction from one agent to another but a universal barrier to a specific form of inference. The small extractiveness value (0.08) reflects the inherent cost of the constraint (additional experiments required to build mechanistic models), distributed equally across all agents who perform perturbation experiments. No agent has lower or higher experienced extraction based on their structural position — the epistemic gap is the same for a graduate student with a single knockout mouse and a genomics consortium with a genome-wide CRISPR library.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a paradigmatic mountain and poses no mandatrophy risk. It is not coordination (no collective action problem is being solved), not extraction (no asymmetric benefit), not temporary (no sunset mechanism exists or is possible), not degraded (perturbation experiments remain highly informative for their intended purpose), and not hybrid (no enforcement mechanism, no beneficiaries, no victims). The constraint is a pure epistemic limit. The mandatrophy framework would flag as suspicious any attempt to classify this as rope (there is no coordination function), snare (there are no victims), or tangled_rope (there is no extraction). The mountain classification is overdetermined by the structural data: very low extractiveness, negligible suppression, emerges naturally, very high accessibility collapse, very low resistance, no beneficiaries, no victims, no enforcement. The constraint is what mountains look like when they are not being naturalized — an actual irreducible limit that no social rearrangement can overcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perturbation_epistemology, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perturbation_epistemology, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is not part of a decomposed family. It is a single, unified epistemic limit. The gap between component identification and mechanistic understanding is not observable-dependent — it appears identically whether the perturbation is genetic (knockout), pharmacological (inhibitor), or physical (lesion). All perturbation methods encounter the same underdetermination. No decomposition is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
