% ============================================================================
% CONSTRAINT STORY: bifurcation_analysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bifurcation_analysis, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bifurcation_analysis
 *   human_readable: Bifurcation Analysis as Coordination Mechanism
 *   domain: dynamical_systems/mathematical_physics
 *
 * SUMMARY:
 *   Bifurcation analysis is a mathematical framework for understanding
 *   qualitative changes in dynamical systems' behavior as parameters vary. At
 *   critical 'bifurcation points,' the phase portrait transitions from one
 *   topology to another—saddle-node bifurcations collapse fixed points, Hopf
 *   bifurcations generate oscillations, pitchfork bifurcations break
 *   symmetries. This constraint story models bifurcation analysis not as a
 *   mathematical fact but as a coordination mechanism that enables
 *   researchers across disciplines to communicate about critical transitions,
 *   tipping points, and catastrophic shifts. The framework emerged
 *   organically in dynamical systems theory but has become institutionalized
 *   through pedagogy, software packages, and disciplinary gatekeeping. The
 *   constraint exhibits different character from different perspectives: pure
 *   coordination mechanism for theorists and practitioners with arbitrage
 *   options (rope), mandatory pedagogical overhead for students without
 *   alternatives (tangled rope), temporary coordination tool being replaced
 *   by numerics (scaffold), and immutable mathematical feature from the
 *   perspective of pure mathematics (mountain—likely false).
 *
 * KEY AGENTS:
 *   - Dynamical systems theorists: Primary beneficiary (institutional/arbitrage) — can freely choose to use or not use bifurcation language; arbitrage to alternative formalisms is available
 *   - Applied researchers: Primary beneficiary (powerful/mobile) — use bifurcation analysis as lingua franca; can exit to domain-specific descriptions if needed but benefit from standardized language
 *   - Undergraduate students: Primary victim (moderate/constrained) — required to learn bifurcation apparatus for credential; exit via alternative preparation paths not recognized by institutions
 *   - Numerical methods community: Secondary actor (organized/constrained) — develops algorithms and software; building increasingly automated packages that reduce human engagement with bifurcation theory
 *   - Mathematical analyst (civilizational perspective): Observer at risk of false summit — perspective that bifurcation is immutable law of mathematics naturalizes what is contingent analytical choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bifurcation_analysis, 0.32).
domain_priors:suppression_score(bifurcation_analysis, 0.18).
domain_priors:theater_ratio(bifurcation_analysis, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bifurcation_analysis, extractiveness, 0.32).
narrative_ontology:constraint_metric(bifurcation_analysis, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(bifurcation_analysis, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bifurcation_analysis, rope).
narrative_ontology:human_readable(bifurcation_analysis, "Bifurcation Analysis as Coordination Mechanism").
narrative_ontology:topic_domain(bifurcation_analysis, "dynamical_systems/mathematical_physics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bifurcation_analysis, dynamical_systems_theorists).
narrative_ontology:constraint_beneficiary(bifurcation_analysis, applied_researchers).
narrative_ontology:constraint_beneficiary(bifurcation_analysis, predictive_modelers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Bifurcation analysis as a coordination mechanism: enables researchers to translate between competing mathematical formalisms (local stability analysis, normal forms, numerical continuation) by establishing shared reference points where phase space structure changes qualitatively. This perspective sees bifurcation classification as lowering communication costs and enabling cross-disciplinary collaboration.
constraint_indexing:constraint_classification(bifurcation_analysis, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Engineers and scientists applying bifurcation analysis to physical systems (climate, neural dynamics, materials behavior) experience it as a coordination standard. Bifurcation language provides a lingua franca for communicating critical transitions, tipping points, and catastrophic failures across domains without requiring domain-specific terminology. Effective extraction is minimal — the tool is available to all practitioners at roughly equal cost.
constraint_indexing:constraint_classification(bifurcation_analysis, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Bifurcation analysis functions as coordination for researchers but imposes significant pedagogical extraction on students. Learning bifurcation diagrams, normal forms, and codimension calculations is mandatory for entry into dynamical systems research but represents substantial cognitive overhead. Students experience constrained exit: the analysis is required knowledge for career progression in certain fields, and alternative methods are not recognized as equivalent by the institutional credentialing system.
constraint_indexing:constraint_classification(bifurcation_analysis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Bifurcation continuation algorithms (AUTO software suite, numerical packages) coordinate the translation from abstract theory to computational verification. Early versions required substantial manual expertise; modern packages increasingly automate the analysis. The scaffold perspective sees bifurcation analysis as a temporary coordination problem being solved by better numerics, with a sunset clause: as machine learning and neural operators improve, explicit bifurcation analysis may become unnecessary for many applications.
constraint_indexing:constraint_classification(bifurcation_analysis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From a pure mathematics perspective, bifurcation is an immutable feature of nonlinear dynamical systems. At critical parameter values where the flow's qualitative behavior changes, bifurcations are unavoidable mathematical facts — they are not contingent institutional features but intrinsic to the structure of differential equations. This perspective risks naturalizing the analysis method itself (bifurcation diagrams, normal forms) as if they were natural law, when the method is actually a contingent human invention for organizing our understanding.
constraint_indexing:constraint_classification(bifurcation_analysis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bifurcation_analysis_tests).
:- end_tests(bifurcation_analysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. Bifurcation analysis creates genuine coordination value—it provides a shared language for discussing critical transitions across disciplines and reduces translation costs between different mathematical formalisms. The extraction component is moderate because some researchers and practitioners are locked into learning the apparatus as a credential requirement. The trajectory shows slow growth (0.22→0.32 over 30 years) reflecting increased institutionalization and pedagogical gatekeeping. Suppression (0.18): Low. Barriers to exit are moderate—researchers can choose alternative mathematical languages (Lyapunov analysis, information-theoretic measures, topological methods), and these alternatives are legitimate but not equivalent to bifurcation thinking. Suppression is not severe because the alternative formalisms are accessible and do not require institutional permission. Theater ratio (0.35): Low. The functional content of bifurcation analysis is high relative to performative content. The method delivers actual mathematical insight into system behavior; diagrams accurately represent phase space structure; predictions are testable. Theater is not minimal because some pedagogical presentation emphasizes memorization of bifurcation types over understanding of underlying mechanisms, and software packages sometimes hide the mathematical structure beneath interfaces.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiaries with exit options (theorists, practitioners) who see rope and victims without exit options (students) who see tangled rope. Both experience the same constraint, but with inverted directionality. A secondary gap exists between the pragmatic rope/tangled_rope perspectives (which recognize bifurcation analysis as a useful but contingent choice) and the false mountain perspective (which naturalizes it as immutable law). This gap is diagnostic: if bifurcation analysis were truly a natural law, all perspectives should converge on mountain. The fact that pragmatic perspectives see rope indicates the mountain view is false summit—it results from taking the mathematical limit of the framework (differential equations have bifurcations) and mistakenly attributing the law-like character to the analytical method (bifurcation diagrams, normal forms) rather than to the phenomenon (phase portrait topology changes). The scaffold perspective introduces a temporal gap: the same constraint appears as coordination now but expects to appear as legacy/piton in 20-30 years as machine learning and neural operator methods automate the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional theorists with arbitrage options experience d ≈ 0.15 (low directionality toward extraction): they benefit from the coordination function and can exit to alternatives freely. Applied practitioners with mobile exit experience d ≈ 0.45 (moderate directionality): they benefit from shared language but face some coordination lock-in through their field's institutional expectations. Students with constrained exit experience d ≈ 0.65 (high directionality toward extraction): they are targets of the pedagogical apparatus with limited alternatives. The analytical observer at civilizational scope experiences d ≈ 0.70 if they adopt the false mountain framing (naturalizing analytical choice as mathematical inevitability). The network effects and scope amplification suggest that bifurcation analysis's extractiveness is underestimated when measured purely at individual level—at continental/global scope, it coordinates billions of person-hours of academic training and research, and the extraction is scaled by this scope multiplier σ(S) ≈ 1.1.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that bifurcation analysis coordinates two distinct goods simultaneously: (1) genuine mathematical insight into system behavior (coordination, baseline), and (2) institutional credentialing and gatekeeping (extraction overlay). The constraint does not collapse into pure extraction because removing the analysis method would degrade the coordination—researchers do lose insight if forced to use only Lyapunov exponents or entropy measures. But the constraint is not pure rope because the pedagogical presentation emphasizes gatekeeping and memorization over insight, and the extraction is enforceable through institutional credentials. The trajectory shows theater_ratio increasing slightly (0.28→0.38) as the method becomes more institutionalized and more often taught via black-box software, reducing direct engagement with underlying mathematics. The extraction is not increasing dramatically because the numerical/software revolution (AUTO, MatCont, modern neural packages) is automating bifurcation analysis itself, effectively disintermediating some of the pedagogical gatekeeping—a student can now use bifurcation software without deep theory training. This suggests the constraint may be transiting toward piton (theater-dominated, function-degraded) rather than remaining in rope. The omega variables address whether this transition is structural (automation genuinely replacing understanding) or superficial (software hides but still requires underlying knowledge).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    method_versus_phenomenon,
    'Is bifurcation analysis revealing inherent structure (mountain) or imposing analytical structure (contingent coordination mechanism)?',
    'Comparison of bifurcation predictions with other mathematical formulations (Lyapunov exponents, entropy measures, topological methods). If predictions are invariant across formalisms, bifurcation analysis is instrumental (coordination). If predictions differ, the analysis method is extracting structure from the system that other methods miss (contingent mathematical choice).',
    'If mountain: bifurcation analysis is inevitable and universal; all researchers converge on the same diagrams. If rope: alternative formalisms exist; bifurcation analysis is one choice among many.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(method_versus_phenomenon, conceptual, 'Whether bifurcation analysis reveals structure or imposes analytical framework').

omega_variable(
    pedagogical_necessity_threshold,
    'How much bifurcation theory knowledge is genuinely required for practitioners, and how much is institutional gatekeeping?',
    'Longitudinal tracking of practitioners who learned bifurcation analysis vs those who avoided it; correlation between depth of bifurcation knowledge and success in applied problem-solving. Survey of applied researchers on whether bifurcation theory was essential to their work or marginally useful.',
    'If threshold is high: students should learn full bifurcation apparatus (current practice). If threshold is low: undergraduate curricula extract unnecessarily; extraction can be reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_necessity_threshold, empirical, 'Required depth of bifurcation knowledge for practitioner success').

omega_variable(
    automation_of_bifurcation_analysis,
    'Can machine learning systems (neural operators, symbolic regression) replace explicit bifurcation analysis for identifying critical transitions in experimental data?',
    'Benchmark ML-based transition detection against classical bifurcation analysis on standard test systems; evaluate interpretability gap (can ML systems explain why a bifurcation occurred in domain-interpretable language?).',
    'If ML systems are successful: bifurcation analysis becomes a legacy method (piton); the scaffold sunset clause activates. If ML systems fail on high-dimensional systems: bifurcation analysis remains mandatory; no sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_of_bifurcation_analysis, empirical, 'Whether ML methods can replace bifurcation analysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bifurcation_analysis, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bifurcation_tr_t0, bifurcation_analysis, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bifurcation_tr_t15, bifurcation_analysis, theater_ratio, 15, 0.32).
narrative_ontology:measurement(bifurcation_tr_t30, bifurcation_analysis, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(bifurcation_be_t0, bifurcation_analysis, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bifurcation_be_t15, bifurcation_analysis, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(bifurcation_be_t30, bifurcation_analysis, base_extractiveness, 30, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bifurcation_analysis, information_standard).
narrative_ontology:affects_constraint(bifurcation_analysis, dynamical_systems_pedagogy).
narrative_ontology:affects_constraint(bifurcation_analysis, tipping_point_communication).

% DUAL FORMULATION NOTE:
% Bifurcation analysis decomposes into two structurally distinct constraints: (1) bifurcation_analysis_coordination (ε≈0.08, Rope)—the mathematical framework for communicating critical transitions across domains; (2) bifurcation_pedagogy_gatekeeping (ε≈0.42, Tangled Rope)—the institutionalized requirement to learn the apparatus as credential. These are networked but distinct, with different ε values reflecting different measurements: coordinate shared-language utility vs. pedagogical overhead. This story addresses the unified constraint seen from multiple perspectives; decomposition would separate coordination from extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
