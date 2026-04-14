% ============================================================================
% CONSTRAINT STORY: cpt_discriminator_vs_conventional_models
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cpt_discriminator_vs_conventional_models, []).

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
 *   constraint_id: cpt_discriminator_vs_conventional_models
 *   human_readable: CPT Discriminator: NMCC Mass Hierarchy vs Conventional Equal-Mass Models
 *   domain: theoretical_physics/high_energy_physics/cosmology
 *
 * SUMMARY:
 *   The CPT discriminator constraint arises from a structural difference
 *   between NMCC (Non-Minimal Chiral Coupling) and all conventional monopole
 *   models. Conventional models, grounded in CPT symmetry, predict that
 *   magnetic monopoles have north and south poles as exact CPT conjugates
 *   with identical masses. NMCC, deriving monopole structure from dimensional
 *   analogy to electric charge species (electron/proton mass hierarchy),
 *   predicts that magnetic charge manifests as distinct species — magnetic
 *   leptons and magnetic hadrons — with hierarchical masses analogous to the
 *   electron-proton mass ratio. This prediction provides a clean empirical
 *   discriminator: if monopoles are detected, measuring whether north/south
 *   poles have identical masses (conventional) or hierarchical masses (NMCC)
 *   immediately falsifies one class of models. The constraint coordinates
 *   experimental search strategies, theoretical model development, and
 *   cosmological monopole scenarios around this single observable. It imposes
 *   minimal cost on researchers (theorists can work in either framework;
 *   experimentalists design searches that would detect either signature)
 *   while providing genuine epistemic value (a sharp empirical fork). The
 *   constraint is downstream of the dimensional analogy vs Lagrangian
 *   derivation constraint, which establishes the structural basis for NMCC's
 *   mass hierarchy prediction.
 *
 * KEY AGENTS:
 *   - Experimental Monopole Search Programs: Primary beneficiary (institutional/mobile) — the mass hierarchy prediction focuses detector design and analysis strategies around a testable structural difference
 *   - Theoretical Model Builders: Primary beneficiary (institutional/mobile) — the discriminator clarifies what experimental data would falsify which class of models, coordinating model development effort
 *   - Cosmological Observers: Beneficiary (organized/mobile) — mass hierarchy prediction structures cosmological monopole scenarios (production mechanisms, freeze-out, relic abundance)
 *   - Individual Theorist: Beneficiary (moderate/mobile) — can work within either framework with minimal switching cost; constraint coordinates field attention without locking researchers in
 *   - Analytical Observer: Sees pure coordination (analytical/analytical) — minimal suppression, minimal extraction, genuine epistemic value from a single discriminating observable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cpt_discriminator_vs_conventional_models, 0.08).
domain_priors:suppression_score(cpt_discriminator_vs_conventional_models, 0.12).
domain_priors:theater_ratio(cpt_discriminator_vs_conventional_models, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cpt_discriminator_vs_conventional_models, extractiveness, 0.08).
narrative_ontology:constraint_metric(cpt_discriminator_vs_conventional_models, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(cpt_discriminator_vs_conventional_models, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cpt_discriminator_vs_conventional_models, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(cpt_discriminator_vs_conventional_models, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cpt_discriminator_vs_conventional_models, rope).
narrative_ontology:human_readable(cpt_discriminator_vs_conventional_models, "CPT Discriminator: NMCC Mass Hierarchy vs Conventional Equal-Mass Models").
narrative_ontology:topic_domain(cpt_discriminator_vs_conventional_models, "theoretical_physics/high_energy_physics/cosmology").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cpt_discriminator_vs_conventional_models, experimental_monopole_search_programs).
narrative_ontology:constraint_beneficiary(cpt_discriminator_vs_conventional_models, theoretical_model_builders).
narrative_ontology:constraint_beneficiary(cpt_discriminator_vs_conventional_models, cosmological_observers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPERIMENTAL MONOPOLE SEARCH PROGRAMS (ROPE) — The mass hierarchy prediction provides a coordination mechanism: if monopoles exist, knowing to search for distinct magnetic lepton and magnetic hadron species with different mass scales focuses detector design and analysis strategies. The constraint coordinates experimental effort around a testable structural difference rather than leaving search parameters unconstrained.
constraint_indexing:constraint_classification(cpt_discriminator_vs_conventional_models, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THEORETICAL MODEL BUILDERS (ROPE) — The CPT discriminator coordinates model development by providing a sharp empirical fork: conventional models predict CPT-conjugate equal-mass poles; NMCC predicts mass hierarchy from magnetic species structure. This coordination is low-cost: theorists can work within either framework, and the discriminator clarifies what experimental data would falsify which class of models.
constraint_indexing:constraint_classification(cpt_discriminator_vs_conventional_models, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: COSMOLOGICAL OBSERVERS (ROPE) — Cosmological monopole abundance constraints and relic density calculations depend on monopole mass. The mass hierarchy prediction coordinates cosmological modeling: if magnetic leptons and hadrons have different masses, their production mechanisms, freeze-out temperatures, and relic abundances differ. The constraint provides structure to cosmological monopole scenarios without imposing significant cost.
constraint_indexing:constraint_classification(cpt_discriminator_vs_conventional_models, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — The CPT discriminator is a pure coordination mechanism arising from competing theoretical frameworks. It imposes minimal suppression (theorists can work in either framework), minimal extraction (no career penalty for choosing either side pre-detection), and provides genuine epistemic value: a single observable (mass ratio of detected monopoles) cleanly separates model classes. The constraint coordinates research effort around a well-defined empirical target.
constraint_indexing:constraint_classification(cpt_discriminator_vs_conventional_models, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: INDIVIDUAL THEORIST (ROPE) — An individual researcher can work within conventional CPT-symmetric models or explore NMCC mass hierarchy predictions with minimal switching cost. The constraint coordinates the field's attention on a discriminating observable without locking researchers into either framework. Low extraction, low suppression, genuine coordination function.
constraint_indexing:constraint_classification(cpt_discriminator_vs_conventional_models, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cpt_discriminator_vs_conventional_models_tests).
:- end_tests(cpt_discriminator_vs_conventional_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal career cost for choosing either theoretical framework pre-detection. Theorists working on conventional CPT-symmetric models face no penalty; theorists exploring NMCC mass hierarchy predictions face no penalty. Post-detection (if monopoles are found), one framework will be falsified, but this is epistemic progress, not extraction. The small non-zero value reflects minor coordination overhead (researchers must track which framework they're working in) and potential for premature commitment to one framework in grant proposals or detector design. Suppression (0.12): Very low. Researchers have high mobility between frameworks. The constraint does not suppress alternative monopole models — it provides a discriminating observable that any model must address. The small non-zero value reflects that experimental searches must make design choices (mass range, detector sensitivity) that could miss monopoles if the wrong mass scale is assumed, but this is inherent to experimental design, not coercive suppression. Theater ratio (0.15): Very low. The constraint has minimal performative content. The mass hierarchy prediction is a genuine structural difference between model classes, not a rhetorical distinction. Experimental searches designed around this discriminator are testing a real physical hypothesis, not performing a ritual. The small non-zero value reflects that some theoretical work may emphasize the discriminator's novelty for grant-writing purposes without contributing to its empirical resolution.
 *
 * PERSPECTIVAL GAP:
 *   No significant perspectival gap exists for this constraint. All agents — experimental programs, theorists, cosmologists, individual researchers, and the analytical observer — classify the CPT discriminator as rope. This uniformity reflects the constraint's structural properties: very low extraction (ε = 0.08), very low suppression (0.12), genuine coordination function (focuses research on a discriminating observable), and minimal theater (0.15). The constraint is a textbook example of pure coordination: it provides epistemic value (a sharp empirical test) at low cost (researchers can work in either framework) without coercion (no suppression of alternatives). The absence of a perspectival gap is itself diagnostic — it confirms that the constraint is not hiding extraction behind coordination claims.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives are beneficiaries with mobile or analytical exit options, producing low directionality values (d ≈ 0.10-0.20 for institutional/mobile beneficiaries, d ≈ 0.72 for analytical). The constraint coordinates research effort around a well-defined empirical target without imposing significant costs on any agent. Experimental programs benefit from focused search strategies. Theorists benefit from a clear empirical fork. Cosmologists benefit from structured monopole scenarios. No agent is a victim — the constraint does not extract from any position. The uniform rope classification across all perspectives reflects that this is a pure coordination mechanism: it solves the collective action problem of 'what observable should we prioritize to discriminate monopole models?' with minimal coercive overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   The CPT discriminator resolves mandatrophy by demonstrating that low-extraction coordination constraints exist and are empirically distinguishable from mountains. The constraint is NOT a mountain: it is not a law of nature, and it does not emerge from physical necessity. CPT symmetry itself is a (contested) mountain; the dimensional analogy that generates NMCC's mass hierarchy prediction is a (contested) mountain. But the discriminator between these frameworks — the prediction that monopole mass ratios will be either equal (CPT) or hierarchical (NMCC) — is a coordination mechanism, not a physical law. It coordinates research effort around an empirical target. The low extractiveness (0.08) and low suppression (0.12) confirm that this coordination is not a cover story for hidden extraction. The constraint provides genuine epistemic value: if monopoles are detected, measuring their mass ratio immediately falsifies one class of models. This is coordination in service of knowledge production, not extraction disguised as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cpt_discriminator_vs_conventional_models, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cpt_discriminator_vs_conventional_models, information_standard).

% DUAL FORMULATION NOTE:
% The CPT discriminator is downstream of the dimensional_analogy_vs_lagrangian_derivation constraint, which establishes the structural basis for NMCC's mass hierarchy prediction. The upstream constraint (mountain-class) determines whether the dimensional analogy is a valid derivation principle; the CPT discriminator (rope-class) coordinates research around the empirical consequence of that principle. The two constraints have different ε values because they address different structural questions: the upstream constraint asks 'is dimensional analogy a valid foundation for monopole theory?' (contested, higher ε); the CPT discriminator asks 'what observable would test the consequence of that foundation?' (coordination, low ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
