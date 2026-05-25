% ============================================================================
% CONSTRAINT STORY: biological_specification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biological_specification, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biological_specification
 *   human_readable: Biological Specification (Real vs. Apparent Design Distinction)
 *   domain: biology/philosophy_of_science/evolutionary_theory
 *
 * SUMMARY:
 *   Biological specification — the question of how to distinguish real design
 *   from apparent design produced by evolution — creates a structural
 *   constraint in biological research that exhibits tangled-rope
 *   characteristics: genuine coordination function (unified theoretical
 *   framework enables efficient resource allocation and knowledge
 *   integration) coupled with asymmetric extraction (researchers questioning
 *   the specification boundary face institutional barriers disproportionate
 *   to methodological concern). The constraint operates across institutional
 *   levels: at the research community level, it suppresses alternative
 *   specification formalisms through publication bias and funding
 *   gatekeeping; at the practicing biologist level, it creates mixed effects
 *   (benefits from research infrastructure, costs from methodological
 *   constraints); at the establishment level, it provides low-cost
 *   coordination. The theater_ratio (0.65) reflects that significant
 *   scientific activity is devoted to defending the coherence of design
 *   detection or evolutionary adequacy rather than generating novel empirical
 *   discoveries. Over a 10-year interval, both theater and extractiveness
 *   have increased, indicating institutional consolidation rather than
 *   dissolution of the constraint.
 *
 * KEY AGENTS:
 *   - Design Inference Research Communities: Primary victims (powerless/trapped) — face publication barriers, funding exclusion, and career risk when proposing specification frameworks outside evolutionary consensus
 *   - Practicing Field Biologists: Secondary victims (moderate/constrained) — experience mixed effects: benefit from evolutionary framework's research infrastructure but constrained from exploring alternative specification questions
 *   - Evolutionary Biology Establishment: Primary beneficiary (institutional/arbitrage) — institutional actors maintaining coordinated research direction, controlling resource allocation, and defining specification boundary
 *   - Computational Biology / Systems Perspective: Organized agents (organized/mobile) — developing alternative formalisms (algorithmic information, mutual information, information-theoretic approaches) that sidestep the traditional specification dichotomy
 *   - Design Detection Methodology Apparatus: Institutional actor (institutional/constrained) — persists in academic margins through historical continuity and specialized venues; maintains piton status through performative activity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the institutional constraint as a natural limit on biological inquiry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_specification, 0.38).
domain_priors:suppression_score(biological_specification, 0.48).
domain_priors:theater_ratio(biological_specification, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_specification, extractiveness, 0.38).
narrative_ontology:constraint_metric(biological_specification, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biological_specification, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_specification, tangled_rope).
narrative_ontology:human_readable(biological_specification, "Biological Specification (Real vs. Apparent Design Distinction)").
narrative_ontology:topic_domain(biological_specification, "biology/philosophy_of_science/evolutionary_theory").

domain_priors:requires_active_enforcement(biological_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biological_specification, evolutionary_biology_framework).
narrative_ontology:constraint_beneficiary(biological_specification, mechanistic_research_programs).
narrative_ontology:constraint_victim(biological_specification, design_inference_methodology).
narrative_ontology:constraint_victim(biological_specification, alternative_explanatory_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESIGN INFERENCE COMMUNITIES (SNARE) — Trapped within institutional constraints that render alternative hypotheses about specification unpublishable in mainstream venues. Career risk and resource access are explicitly contingent on methodological compliance. No exit option without complete reorientation. Experiences pure extraction: suppressed without coordination benefit.
constraint_indexing:constraint_classification(biological_specification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRACTICING BIOLOGISTS (TANGLED ROPE) — Constrained by institutional structure and grant funding mechanisms that require evolutionary framework adoption, but also benefit from the research ecosystem that evolutionary theory enables (sequencing resources, population-level data access, collaborative networks). Significant asymmetry: benefits accrue to committed adopters, costs to dissenters. Some agency (can work around constraints) but not full freedom.
constraint_indexing:constraint_classification(biological_specification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EVOLUTIONARY BIOLOGY ESTABLISHMENT (ROPE) — Institutional beneficiary with arbitrage capacity: can navigate between theoretical frameworks at the institutional level, fund preferred research directions, and maintain institutional coherence. Experiences the specification constraint as a coordination mechanism: unified framework enables resource allocation, training pipeline, and publication infrastructure. Low experienced extraction — they are the architects.
constraint_indexing:constraint_classification(biological_specification, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTATIONAL BIOLOGY / SYSTEMS PERSPECTIVE (SCAFFOLD) — Organized research communities (systems biology, astrobiology, synthetic biology) are developing alternative formalizations of specification that bypass the design inference vs evolutionary dichotomy. These frameworks (information theory, algorithmic information content, mutual information analysis) represent sunset pathways — as computational tools mature and cross-disciplinary adoption grows, the need to choose between traditional dichotomies declines. Mobile exit through tool development and methodological pluralism.
constraint_indexing:constraint_classification(biological_specification, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DESIGN DETECTION APPARATUS (PITON) — Historical design detection methodologies (natural theology, argument from design, specified complexity analysis) persist in academic margins and specialized contexts despite episodic critique and limited empirical productivity. Theater ratio high (0.65): much activity devoted to defending the conceptual coherence of detection methods rather than generating new empirical discoveries. Maintained through institutional inertia (biology history courses) and specialized journal networks, not through functional verification.
constraint_indexing:constraint_classification(biological_specification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the distinction between real and apparent design may appear as a fundamental limit on biological inquiry: organisms exhibit apparent design (functional integration, hierarchical organization); distinguishing between design and evolution-mimicked design is inherently difficult given observer limitations and historical contingency. This perspective risks naturalizing an institutional/epistemological constraint as a logical or physical limit. False summit detection required.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_specification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biological_specification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_specification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(biological_specification, TR),
    TR >= 0.70.

:- end_tests(biological_specification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts opportunity cost and intellectual labor from design inference researchers, but the extraction is not total — alternative venues exist (specialized journals, cross-disciplinary research, computational frameworks), and the evolutionary framework itself provides genuine coordination benefits that offset some of the asymmetry. The extractiveness is not as severe as a full snare (which would involve total exclusion) but significantly higher than pure coordination (rope). Suppression (0.48): Moderate-high. Institutional mechanisms suppress design inference research: review bias against specification-focused frameworks, grant funding channeled toward evolutionary mechanism research, hiring preferences for evolutionary competence. However, suppression is not absolute — escape routes exist through computational reframing, cross-disciplinary positioning, and specialized publication. Theater ratio (0.65): High and increasing. Over the 10-year interval, theater has risen as institutional investment in defending the specification boundary has increased relative to empirical discovery. Much recent literature focuses on demonstrating the adequacy of evolutionary mechanisms to produce apparent design rather than developing novel detection capabilities. Theater increase from 0.42 to 0.65 indicates Goodhart-type drift: defending methodological consensus becomes an end in itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full indexical range from institutional establishment (rope, sees coordination) to powerless researchers (snare, sees extraction). The establishment's view of the constraint as coordination (rope) is structurally accurate for them: they experience low extraction, high benefits, and efficient resource allocation. The design inference community's view of the same constraint as pure extraction (snare) is also structurally accurate for them: they experience career risk, publication barriers, and suppressed research opportunity with minimal institutional support. The practicing biologist's moderate-power perspective captures the tangled reality: real research benefits from evolutionary framework (infrastructure, data access, training pipeline) coupled with real penalties for methodological dissent. The computational biology coalition's scaffold perspective is forward-looking: as information-theoretic formalisms mature, they provide an exit pathway that avoids the traditional dichotomy entirely. The piton perspective (design detection apparatus) captures institutional inertia: historical detection methods persist through academic tradition and specialized venues despite episodic critique and limited new empirical yield.
 *
 * DIRECTIONALITY LOGIC:
 *   The specification constraint's directionality values are determined by each agent's structural position relative to specification research. Design inference communities occupy high-d positions (victims with few exit options) — they experience strong extraction. The evolutionary establishment occupies low-d positions (beneficiaries with institutional arbitrage) — they experience coordination as a low-extraction mechanism. Practicing biologists occupy mid-d positions (moderate power, constrained exit) — they experience mixed effects. The computational/systems biology coalition occupies low-d positions but with mobile exit through methodological alternatives — they can sidestep the constraint without institutional penalty. The design detection apparatus occupies a degraded position (high theater, low new functionality) — piton classification. The analytical observer at civilizational scope risks d = 0.72 (observer position) and false mountain classification from naturalizing institutional constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the temptation to classify specification as either pure coordination (rope — a unified framework is genuinely useful) or pure extraction (snare — researchers are clearly suppressed). The correct classification (tangled rope) acknowledges both: the evolutionary framework IS a coordination mechanism with real benefits, AND the specification boundary IS enforced asymmetrically to suppress alternatives. The constraint resolves the mandatrophy by showing that legitimate coordination can coexist with asymmetric extraction. The establishment genuinely benefits from unified frameworks (they are not lying when they defend evolutionary theory's explanatory power). The design inference communities are genuinely suppressed (they are not paranoid when they identify publication bias). Both are true. The piton perspective warns against another mandatrophy: mistaking institutional persistence (theater) for genuine function. Design detection methods survive not because they are continuously validated but because they are historically embedded and defended in specialized contexts. The mountain perspective represents the deepest mandatrophy: the risk of treating the institutional constraint as a law of nature. If specification is fundamentally indistinguishable from evolution-mimicked design (as some analytical perspectives suggest), then the constraint is indeed a mountain — a limit on biological knowledge. But this risks collapsing the institutional constraint into an epistemic constraint. The framework's job is to keep them separate and measure which dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_measurability,
    'Is biological specification objectively quantifiable independent of theoretical framework, or is the quantity always theory-laden?',
    'Cross-framework analysis of specification measures: algorithmic information content, mutual information, functional requirement metrics. Test whether different frameworks yield commensurate quantifications for the same biological systems.',
    'If objective: specification could be framework-neutral, reducing extraction. If theory-laden: specification is inherently contested, and the current framework dominance represents suppression of alternative measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_measurability, empirical, 'Whether specification is objectively quantifiable').

omega_variable(
    design_inference_distinguishability,
    'Can evolutionary processes produce the same distribution of apparent-design features as intentional design, making the classes empirically indistinguishable in practice?',
    'Comparative analysis of known designed systems (human engineering, synthetic biology) vs evolved systems using consistent detection metrics. Bayesian analysis of false-positive and false-negative rates across framework applications.',
    'If indistinguishable: the classification itself is not empirically grounded (high theater), supporting piton/snare classifications. If distinguishable: design inference is a legitimate alternative methodology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(design_inference_distinguishability, empirical, 'Whether design and evolution produce empirically distinguishable signatures').

omega_variable(
    institutional_capture_mechanism,
    'Is the specification constraint enforced primarily through active institutional gatekeeping (grant allocation, journal review, hiring) or through genuine empirical superiority of the evolutionary framework?',
    'Historical analysis of publication acceptance rates, funding distribution, and career trajectories for researchers proposing alternative specification frameworks. Comparative study of institutional vs non-institutional research environments.',
    'If gatekeeping: tangled rope / snare classifications confirmed. If empirical superiority: the constraint is actually a rope (legitimate coordination on superior method).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_mechanism, empirical, 'Institutional gatekeeping vs genuine methodological superiority').

omega_variable(
    specification_plasticity,
    'As computational tools and information theory mature, can multiple specification formalisms coexist productively, or is institutional unification required?',
    'Case studies in computational biology, systems biology, and astrobiology where specification questions arise. Track whether diverse methodological approaches generate complementary insights or whether one framework consistently dominates.',
    'If coexistence: scaffold perspective is correct, and sunset is real. If unification required: snare/tangled rope classifications are more accurate; pluralism is not viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_plasticity, empirical, 'Whether diverse specification formalisms can coexist productively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biological_specification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biospec_tr_t0, biological_specification, theater_ratio, 0, 0.42).
narrative_ontology:measurement(biospec_tr_t5, biological_specification, theater_ratio, 5, 0.55).
narrative_ontology:measurement(biospec_tr_t10, biological_specification, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(biospec_be_t0, biological_specification, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(biospec_be_t5, biological_specification, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(biospec_be_t10, biological_specification, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biological_specification, information_standard).
narrative_ontology:affects_constraint(biological_specification, evolutionary_mechanism_sufficiency).
narrative_ontology:affects_constraint(biological_specification, intelligent_design_methodology).
narrative_ontology:affects_constraint(biological_specification, specified_complexity_detection).

% DUAL FORMULATION NOTE:
% Biological specification decomposes into three related constraints: (1) Evolutionary Mechanism Sufficiency (ε ≈ 0.12, Mountain) — whether evolutionary processes can produce observed biological complexity; (2) Specified Complexity Detection (ε ≈ 0.42, Tangled Rope) — whether detection methods can reliably distinguish design from evolution-mimicked design; (3) Biological Specification as institutional constraint (this story, ε = 0.38, Tangled Rope) — how the specification distinction is institutionally enforced within biology. The first is a factual/scientific question; the second is a methodological question; the third is a structural/institutional question. All three affect each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biological_specification, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
