% ============================================================================
% CONSTRAINT STORY: technical_documentation_quality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technical_documentation_quality, []).

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
 *   constraint_id: technical_documentation_quality
 *   human_readable: Technical Documentation Quality Constraint
 *   domain: software_engineering/knowledge_management
 *
 * SUMMARY:
 *   Technical documentation quality creates a structural asymmetry between
 *   those who benefit from shipping without documentation (product teams,
 *   organizations optimizing for velocity) and those who bear the cost of
 *   poor documentation (end users, maintenance developers, knowledge
 *   workers). The constraint exhibits high extractiveness because
 *   organizations can systematically choose to underinvest in documentation
 *   when users face high switching costs. Suppression is substantial because
 *   end users have limited alternatives (locked into platforms by network
 *   effects, integration costs, and ecosystem effects), maintenance
 *   developers face career path dependence (specialized knowledge becomes a
 *   lock), and the organizational incentive structure systematically rewards
 *   feature velocity over documentation. The theater ratio (0.65) reflects
 *   that formal documentation often becomes performative:
 *   style-guide-compliant documents that users ignore in favor of Stack
 *   Overflow answers, vendor examples, and community wikis. The constraint's
 *   extractiveness has increased over the interval (0.35 → 0.52) as software
 *   complexity outpaced documentation practices and network effects
 *   strengthened platform lock-in. Open documentation movements and
 *   AI-assisted generation tools represent a genuine sunset pathway — if
 *   these mature, the extraction mechanism loses force because documentation
 *   cost becomes negligible and users gain viable exit options.
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — depend on software despite poor documentation; face high switching costs from network effects and integration dependencies; cannot organize to demand better documentation
 *   - Maintenance Developers: Secondary victim (moderate/constrained) — trapped in knowledge silos; specialized expertise in undocumented systems creates job security but limits career mobility; bear cognitive load of reverse-engineering peers' code
 *   - Product Team: Primary beneficiary (institutional/arbitrage) — captures time value by shipping without documentation overhead; achieves faster iteration velocity; redirects resources to feature development; arbitrage capacity enables trade-off between documentation and velocity
 *   - Development Organization: Secondary beneficiary (institutional/arbitrage) — reduced labor costs, faster feature delivery, competitive advantage in market velocity; can extract rents through platform lock-in and switching cost asymmetry
 *   - Open Documentation Movement: Organized coalition (organized/mobile) — building alternative knowledge pathways through doc-as-code tooling, community-maintained wikis, AI-assisted generation; have exit capacity and see sunset logic in documentation constraints
 *   - Enterprise Documentation Department: Institutional actor (institutional/arbitrage) — maintains documentation ritual through inertia; primary function has atrophied but organizational structure persists; sees constraint as natural law of software development
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent organizational incentives as inherent software properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technical_documentation_quality, 0.52).
domain_priors:suppression_score(technical_documentation_quality, 0.58).
domain_priors:theater_ratio(technical_documentation_quality, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technical_documentation_quality, extractiveness, 0.52).
narrative_ontology:constraint_metric(technical_documentation_quality, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technical_documentation_quality, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technical_documentation_quality, tangled_rope).
narrative_ontology:human_readable(technical_documentation_quality, "Technical Documentation Quality Constraint").
narrative_ontology:topic_domain(technical_documentation_quality, "software_engineering/knowledge_management").

domain_priors:requires_active_enforcement(technical_documentation_quality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technical_documentation_quality, product_team).
narrative_ontology:constraint_beneficiary(technical_documentation_quality, development_organization).
narrative_ontology:constraint_victim(technical_documentation_quality, end_users).
narrative_ontology:constraint_victim(technical_documentation_quality, maintenance_developers).
narrative_ontology:constraint_victim(technical_documentation_quality, knowledge_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Cannot exit system dependency; bears full cost of poor documentation. Trapped by software lock-in, network effects, and lack of alternatives. Forced to reverse-engineer undocumented features, waste time troubleshooting, pay for workarounds. Maximum extraction with no exit capacity.
constraint_indexing:constraint_classification(technical_documentation_quality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAINTENANCE DEVELOPER (TANGLED ROPE) — Constrained by career path dependence and skill specificity to the codebase; also benefits from job security through system complexity and knowledge asymmetry. Experiences both coordination (they solve real problems for the organization) and extraction (their labor is undervalued and their specialized knowledge locks them in). High extraction cost but some institutional recognition and stability.
constraint_indexing:constraint_classification(technical_documentation_quality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRODUCT TEAM (ROPE) — Benefits from documentation gap; can capture time value by shipping without documentation overhead, achieve faster iteration cycles, redirect resources to feature development. Experiences constraint as pure coordination: documentation enables handoff between team members. Net beneficiary through arbitrage — can substitute feature velocity for documentation completeness.
constraint_indexing:constraint_classification(technical_documentation_quality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN DOCUMENTATION MOVEMENT (SCAFFOLD) — Organized agents (doc-as-code, community wikis, AI-assisted generation tools) see poor proprietary documentation as a temporary failure. Building alternative knowledge pathways: open-source projects with community-maintained documentation, markdown-first tooling, automated doc generation from code. Sunset logic: as cultural norms shift toward documentation-first development and tools lower documentation cost, the extraction mechanism loses force. Estimated sunset: 5-10 years as tooling matures.
constraint_indexing:constraint_classification(technical_documentation_quality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ENTERPRISE DOCUMENTATION DEPARTMENT (PITON) — Traditional technical writing and documentation maintenance persists largely through institutional inertia. The actual function has atrophied: users ignore formal documentation and rely on Stack Overflow, vendor-supplied examples, and community forums. The documentation department continues performing the ritual (maintaining style guides, reviewing documents, producing PDFs) despite low functional verification. Maintained because the alternative (dissolving the function) requires organizational change, not because documentation is working. Theater dominates function.
constraint_indexing:constraint_classification(technical_documentation_quality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some documentation lag is inherent to software systems: complexity always outpaces documentation completeness, tacit knowledge in code always exceeds explicit documentation, and the gap between implementation and documentation is a structural feature of how systems evolve. This perspective sees the constraint as an immutable property of software development. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'inherent to software' naturalizes what is actually a contingent organizational choice.
constraint_indexing:constraint_classification(technical_documentation_quality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technical_documentation_quality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technical_documentation_quality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technical_documentation_quality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technical_documentation_quality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technical_documentation_quality, TR),
    TR >= 0.70.

:- end_tests(technical_documentation_quality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Organizations can systematically choose to underinvest in documentation when users face switching costs, but the extraction is not maximal because some organizations do invest in documentation as a competitive differentiator, and open-source communities often maintain documentation despite no direct extraction incentive. The value reflects that the choice to underdocument is contingent on market structure (platform power, switching costs), not inevitable. Suppression (0.58): Moderate-high. Significant barriers include network effects (users locked into platforms despite poor documentation), integration dependencies, specialized skill requirements for documentation, and organizational career incentives that reward shipping features over documentation work. But suppression is not total — users can migrate between tools, documentation can be crowdsourced, and tooling innovations (doc-as-code, AI generation) are lowering documentation barriers. Theater ratio (0.65): Moderate-high. Formal technical documentation often becomes performative: style-guide-compliant documents maintained for regulatory compliance or quality theater while users rely on Stack Overflow, vendor examples, blog posts, and video tutorials. The gap between official documentation and actual knowledge sources has widened as complexity increased and community alternatives matured. Theater has increased over the interval as organizations maintained documentation department rituals while the actual epistemic function shifted to distributed community knowledge.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perpectival collapse across institutional boundaries. The product team sees coordination (Rope): documentation enables handoff between developers and enables feature velocity. The organization sees coordination (Rope): documentation is a communication cost traded against velocity. The end user sees extraction (Snare): they are forced to expend personal labor to understand undocumented features with no exit option. The maintenance developer sees mixed extraction and coordination (Tangled Rope): they solve real problems for the organization but are trapped in knowledge silos. The open documentation coalition sees a solvable problem with sunset (Scaffold): tooling and cultural shifts are building alternatives. The documentation department sees degraded ritual (Piton): formal documentation persists through inertia while actual knowledge flows through external channels. The civilizational observer risks naturalizing the constraint (Mountain): 'documentation always lags implementation' — but the structural data reveals this as a false summit, a contingent organizational choice enabled by platform power and switching cost asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim asymmetry drives directionality. Product teams and organizations benefit from faster shipping without documentation overhead — they have arbitrage capacity (can choose velocity over completeness). End users face trapped exit options: locked into platforms by network effects, switching costs, and integration dependencies, with no recourse except reverse-engineering or external knowledge sources. Maintenance developers face constrained exit: they benefit from documentation for coordination but are trapped in knowledge silos when documentation is poor; their specialized expertise creates lock-in. The organization perceives the constraint as coordination (solving the communication problem between engineers) while users perceive it as extraction (forced to substitute personal labor for missing documentation). The product team's low directionality (d ≈ 0.15) comes from arbitrage exit capacity; the end user's high directionality (d ≈ 0.90) comes from trapped dependency; the maintenance developer's moderate directionality (d ≈ 0.55) reflects constrained exit with mixed benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that documentation quality is NOT an immutable property of software development. The civilizational/natural law perspective (Mountain) is a false summit: organizations with strong documentation commitments (Apple, PostgreSQL, Rust) prove that full documentation IS achievable. The constraint's extractiveness is a contingent organizational choice, not a law of nature. The classification resolves as Tangled Rope because: (1) Genuine coordination function exists — documentation enables team communication and knowledge transfer; (2) Asymmetric extraction occurs — the coordination costs are borne by users and maintenance developers while benefits flow to product teams and organizations through velocity gains; (3) Active enforcement required — organizations must choose to prioritize velocity over documentation, a choice that is actively enforced through resource allocation, performance metrics, and incentive structures. The constraint persists not because documentation is impossible but because the incentive structure rewards shipping over completeness. The scaffold perspective's sunset logic is real: open documentation tooling and AI-assisted generation are reducing the labor cost of documentation, which will eventually eliminate the extraction incentive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_vs_code_divergence_threshold,
    'At what divergence rate between documented and actual behavior does documentation cease being useful and become actively harmful?',
    'Empirical tracking of documentation accuracy over software versions; correlation between accuracy decay rate and user reliance metrics; analysis of support ticket distributions before/after documentation accuracy drops',
    'If threshold is low (<5% divergence): current documentation practices are extractive — users are trapped by inaccurate information. If threshold is high (>20% divergence): substantial documentation debt is tolerable, loosening the extraction characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_vs_code_divergence_threshold, empirical, 'Threshold divergence rate between documentation and implementation').

omega_variable(
    self_documenting_code_sufficiency,
    'Do well-structured code (clear naming, type hints, inline comments) and automated documentation generation (docstrings, API specs) constitute adequate documentation, or do users require separate prose documentation?',
    'User satisfaction surveys controlling for code quality levels; support ticket analysis comparing low-comment vs high-comment codebases; time-to-productivity studies for new developers with only generated vs prose documentation',
    'If code-level documentation sufficient: constraint shifts to Rope — problem is coordination of documentation discipline, not extraction. If prose documentation necessary: constraint remains Snare for users — they are trapped by the gap between generated specs and practical understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_documenting_code_sufficiency, empirical, 'Whether self-documenting code eliminates documentation extraction').

omega_variable(
    documentation_production_cost_evolution,
    'Will AI-assisted documentation generation (large language models generating docs from code, automated test case extraction, docstring inference) reduce the labor cost of documentation sufficiently to eliminate the extraction incentive?',
    'Comparison of human documentation labor hours vs AI-assisted generation output quality; cost-benefit analysis of AI tooling adoption rates across organizations; longitudinal tracking of documentation investment as percentage of engineering budget',
    'If labor cost drops below 5% of engineering budget: extraction mechanism breaks — no longer economical to skip documentation. If labor cost remains >15%: extraction incentive persists regardless of tool improvements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documentation_production_cost_evolution, empirical, 'Will AI tooling eliminate the documentation labor cost extraction').

omega_variable(
    knowledge_worker_exit_capacity_evolution,
    'As community-maintained documentation, video tutorials, and AI chatbots improve, do users actually exit low-documentation systems for better-documented alternatives, or do network effects lock them in regardless of documentation quality?',
    'Market share analysis of software with strong vs weak documentation; user survey on documentation quality as decision factor in tool adoption; longitudinal tracking of churn rates correlated with documentation investment',
    'If users readily switch: exit capacity is higher than current assessment — reclassify users from trapped to constrained. If users stay despite poor documentation: network effects dominate — trapped classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_worker_exit_capacity_evolution, empirical, 'Whether users actually exit systems with poor documentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technical_documentation_quality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techdoc_tr_t0, technical_documentation_quality, theater_ratio, 0, 0.4).
narrative_ontology:measurement(techdoc_tr_t5, technical_documentation_quality, theater_ratio, 5, 0.55).
narrative_ontology:measurement(techdoc_tr_t10, technical_documentation_quality, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(techdoc_be_t0, technical_documentation_quality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(techdoc_be_t5, technical_documentation_quality, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(techdoc_be_t10, technical_documentation_quality, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technical_documentation_quality, information_standard).
narrative_ontology:affects_constraint(technical_documentation_quality, software_maintenance_burden).
narrative_ontology:affects_constraint(technical_documentation_quality, knowledge_worker_lock_in).
narrative_ontology:affects_constraint(technical_documentation_quality, platform_switching_cost).

% DUAL FORMULATION NOTE:
% Technical documentation quality is downstream of organizational incentive structures (feature velocity prioritization) and platform market structures (switching costs, network effects). The documentation constraint is distinct from these upstream constraints but structurally enabled by them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technical_documentation_quality, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
