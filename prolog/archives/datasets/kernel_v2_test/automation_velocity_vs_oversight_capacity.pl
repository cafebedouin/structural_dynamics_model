% ============================================================================
% CONSTRAINT STORY: automation_velocity_vs_oversight_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_automation_velocity_vs_oversight_capacity, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: automation_velocity_vs_oversight_capacity
 *   human_readable: Automation Velocity vs Oversight Capacity in AI-Assisted Development
 *   domain: technology_governance/organizational_dynamics/software_engineering
 *
 * SUMMARY:
 *   The automation velocity vs oversight capacity constraint emerges from the
 *   structural mismatch between AI code generation rates and human review
 *   throughput. As AI coding assistants (Claude, Copilot, internal tools)
 *   accelerate development velocity, the bottleneck migrates from code
 *   production to code review. Organizations face a trilemma: slow down
 *   AI-assisted development (sacrificing competitive advantage), reduce
 *   review depth (accepting higher bug escape rates and technical debt), or
 *   massively scale review capacity (expensive, slow to hire, limited by
 *   human cognitive bandwidth). Most organizations choose option 2, creating
 *   a systematic transfer of risk from the organization to individual
 *   reviewers and downstream maintainers. The constraint exhibits tangled
 *   rope structure because it contains both genuine coordination (AI
 *   assistance does solve real development bottlenecks, enables faster
 *   iteration, democratizes coding capability) and genuine extraction (risk
 *   externalization, reviewer expertise devaluation, technical debt
 *   accumulation, craft knowledge erosion). The theater ratio (0.58) reflects
 *   that traditional line-by-line code review has degraded into performance:
 *   reviewers increasingly rubber-stamp AI-generated PRs because deep review
 *   is impossible at current volumes, but the review ritual persists for
 *   audit trails and liability protection. Suppression (0.62) captures the
 *   limited alternatives: reviewers cannot exit without leaving the
 *   profession, organizations cannot slow down without losing competitive
 *   position, and the industry-wide adoption of AI tooling creates lock-in.
 *
 * KEY AGENTS:
 *   - Human Reviewers: Primary victim (powerless/trapped) — trapped in accelerating treadmill, expertise devalued, liability increased, cannot exit without leaving profession
 *   - Anthropic as Organization: Primary beneficiary (institutional/arbitrage) — captures competitive advantage through velocity, scales output without proportional headcount, can adjust tooling at will
 *   - Downstream Maintainers: Secondary victim (moderate/constrained) — benefit from faster features but bear maintenance burden and technical debt
 *   - AI Safety Research Community: Organized coalition (organized/mobile) — sees temporary coordination failure with sunset via automated oversight tools
 *   - Traditional Code Review Process: Institutional ritual (institutional/constrained) — atrophied into performance theater, maintained for compliance
 *   - AI Tooling Vendors: Mixed beneficiary (powerful/mobile) — profit from selling solutions to problem their products help create
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination and genuine extraction structurally entangled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(automation_velocity_vs_oversight_capacity, 0.48).
domain_priors:suppression_score(automation_velocity_vs_oversight_capacity, 0.62).
domain_priors:theater_ratio(automation_velocity_vs_oversight_capacity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(automation_velocity_vs_oversight_capacity, extractiveness, 0.48).
narrative_ontology:constraint_metric(automation_velocity_vs_oversight_capacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(automation_velocity_vs_oversight_capacity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(automation_velocity_vs_oversight_capacity, tangled_rope).
narrative_ontology:human_readable(automation_velocity_vs_oversight_capacity, "Automation Velocity vs Oversight Capacity in AI-Assisted Development").
narrative_ontology:topic_domain(automation_velocity_vs_oversight_capacity, "technology_governance/organizational_dynamics/software_engineering").

domain_priors:requires_active_enforcement(automation_velocity_vs_oversight_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(automation_velocity_vs_oversight_capacity, anthropic_as_organization).
narrative_ontology:constraint_beneficiary(automation_velocity_vs_oversight_capacity, velocity_optimized_teams).
narrative_ontology:constraint_beneficiary(automation_velocity_vs_oversight_capacity, ai_tooling_vendors).
narrative_ontology:constraint_victim(automation_velocity_vs_oversight_capacity, code_quality_assurance_process).
narrative_ontology:constraint_victim(automation_velocity_vs_oversight_capacity, human_reviewers).
narrative_ontology:constraint_victim(automation_velocity_vs_oversight_capacity, downstream_maintainers).
narrative_ontology:constraint_vindicates(automation_velocity_vs_oversight_capacity, move_fast_break_things_doctrine).
narrative_ontology:constraint_vindicates(automation_velocity_vs_oversight_capacity, automation_inevitability_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMAN REVIEWER (SNARE) — Trapped in an accelerating treadmill where AI generates code faster than humanly possible to review with adequate depth. Cannot exit without leaving the profession. Career advancement now depends on review throughput metrics that incentivize shallow approval over deep scrutiny. The coordination story (faster development cycles) is cover for extraction: reviewer expertise is devalued while their liability for missed bugs increases.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOWNSTREAM MAINTAINER (TANGLED ROPE) — Benefits from faster feature delivery and larger codebases to work with, but bears the cost of technical debt accumulation and undocumented AI-generated patterns. Constrained by organizational momentum toward AI tooling adoption. Experiences genuine coordination (more code, faster iteration) alongside genuine extraction (maintenance burden, debugging opacity, implicit knowledge loss).
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTHROPIC AS ORGANIZATION (ROPE) — Primary beneficiary. Captures competitive advantage through velocity: ships features faster, iterates more rapidly, scales engineering output without proportional headcount growth. The bottleneck migration is a coordination problem being solved through tooling investment. Arbitrage exit: can adjust AI assistance levels, hire more reviewers, or change review standards at will. Experiences the constraint as pure coordination with minimal extraction.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI SAFETY RESEARCH COMMUNITY (SCAFFOLD) — Organized coalition (academic researchers, policy advocates, internal safety teams) sees the velocity-oversight gap as a temporary coordination failure with a sunset: automated code review, formal verification tools, and AI-assisted auditing will eventually close the gap. The current bottleneck justifies investment in oversight automation. Mobile exit: can shift focus to other AI governance challenges if this one resolves or proves intractable.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL CODE REVIEW PROCESS (PITON) — The ritual of line-by-line human review has atrophied into performance. Reviewers increasingly rubber-stamp AI-generated PRs because deep review is impossible at current volumes. The process persists through institutional inertia and compliance theater: organizations maintain review requirements to satisfy audit trails and liability protection, not because the reviews catch meaningful bugs. High theater ratio reflects this degradation.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AI TOOLING VENDOR (TANGLED ROPE) — Benefits from selling solutions to the bottleneck (AI-assisted review tools, automated testing, code quality dashboards) while also contributing to the problem by accelerating generation velocity. Genuine coordination function: their tools do enable faster development. Genuine extraction: they profit from a problem their own products help create. Mobile exit: can pivot to other enterprise AI markets.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this constraint exhibits both genuine coordination (AI assistance does solve real development bottlenecks, enables new capabilities, democratizes coding) and genuine extraction (systematic transfer of risk from organizations to individual reviewers, technical debt externalization to future maintainers, erosion of craft knowledge). The bottleneck migration is not a bug but a feature: organizations rationally optimize for velocity over quality when competitive pressure dominates. The constraint is structurally tangled because both functions are real and neither can be removed without changing the constraint's identity.
constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(automation_velocity_vs_oversight_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(automation_velocity_vs_oversight_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(automation_velocity_vs_oversight_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(automation_velocity_vs_oversight_capacity, TR),
    TR >= 0.70.

:- end_tests(automation_velocity_vs_oversight_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Organizations capture velocity benefits while externalizing costs to reviewers (increased cognitive load, liability risk, expertise devaluation) and downstream maintainers (technical debt, debugging opacity). The extraction is substantial but not maximal because some coordination benefit is real — AI assistance does solve genuine bottlenecks. The value reflects that roughly half the constraint's operation is extractive overhead beyond coordination cost. Suppression (0.62): Moderate-high. Reviewers face limited alternatives: cannot slow the treadmill without organizational override, cannot exit without leaving the profession, cannot organize effectively because review work is distributed and individual. Organizations face competitive pressure that suppresses the option to slow down. Industry-wide AI tooling adoption creates lock-in. But suppression is not total — some organizations do maintain higher review standards, some reviewers do exit to less velocity-optimized roles, and the AI safety community is building alternative oversight pathways. Theater ratio (0.58): Moderate-high. Traditional code review has substantially degraded into performance. Reviewers cannot perform deep review at current volumes, so they increasingly rubber-stamp AI-generated PRs while maintaining the ritual for audit trails. The review process persists through institutional inertia and compliance requirements, not because it catches meaningful bugs at the rate it once did. The theater has increased over the 6-year interval as AI generation velocity has outpaced human review capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — AI code generation outpacing human review capacity — appears as different constraint types depending on the observer's position. Human reviewers see a snare: they are trapped in an accelerating treadmill with no exit, and the coordination story (faster development) is cover for extraction (expertise devaluation, liability transfer). Anthropic as organization sees rope: they are solving a genuine coordination problem (scaling engineering output) and capturing legitimate competitive advantage. Downstream maintainers see tangled rope: they benefit from faster features but bear the cost of technical debt and maintenance burden. The AI safety community sees scaffold: a temporary coordination failure being solved through automated oversight tools with a real sunset. The traditional code review process sees piton: its function has atrophied into performance theater maintained through institutional inertia. AI tooling vendors see tangled rope: they provide genuine coordination value while also profiting from a problem their products help create. The analytical observer sees tangled rope at the civilizational level: both coordination and extraction are structurally real and neither can be removed without changing the constraint's identity. The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Human reviewers are victims with trapped exit options — they experience high directionality toward full extraction (d approaching 1.0), amplified by their powerless position and inability to exit. Anthropic as organization is a beneficiary with arbitrage exit options — they experience low or negative directionality (d approaching 0.0), experiencing the constraint as subsidy rather than extraction. Downstream maintainers are victims with constrained exit — they experience moderate-high directionality, less than reviewers because they have some agency and some benefit from faster feature delivery. AI tooling vendors are beneficiaries with mobile exit — they experience low directionality because they profit from the constraint. The AI safety community is neither beneficiary nor victim in the extraction sense — they are organized agents working to resolve the constraint, experiencing moderate directionality because they face resource constraints but have exit options. The traditional code review process is an institutional victim with constrained exit — it experiences moderate directionality as its function atrophies but institutional inertia prevents rapid change. The analytical observer has analytical exit and sees the full structure — they experience the constraint as tangled rope regardless of directionality because both coordination and extraction are structurally real.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is a stable structural category, not a transitional state. The constraint contains both genuine coordination (AI assistance does solve real development bottlenecks, enables capabilities that would not exist otherwise, democratizes coding) and genuine extraction (systematic risk transfer, expertise devaluation, technical debt externalization, craft knowledge erosion). Neither function can be removed without destroying the constraint's identity. If you remove the coordination function, you no longer have AI-assisted development — you have pure extraction (a snare). If you remove the extraction function, you no longer have the velocity-oversight gap — you have pure coordination (a rope). The tangled rope classification is not a failure to choose between rope and snare; it is the correct classification of a constraint where both functions are structurally real and operationally inseparable. The mandate (faster development through AI assistance) has not outlived its function — the function is still active and valuable. But the mandate also enables extraction that is not incidental or correctable through better implementation. The extraction is structural: it emerges from the velocity-oversight mismatch that is inherent to the automation pattern. Organizations that adopt AI coding assistance face a trilemma with no pure-coordination solution. The tangled rope classification captures this structural reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    review_depth_threshold,
    'What level of review depth is sufficient for AI-generated code vs human-generated code? Is shallow review adequate when the generator is an AI system with known failure modes?',
    'Empirical comparison of bug escape rates for AI-generated code under different review intensities; controlled experiments varying review time per line of AI vs human code',
    'If shallow review is sufficient: the bottleneck is pure coordination (Rope from more perspectives). If deep review is necessary: the bottleneck is extraction mechanism (Snare from more perspectives) because organizations are systematically under-investing in required oversight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_depth_threshold, empirical, 'Required review depth for AI-generated code').

omega_variable(
    automation_closure_timeline,
    'Will automated code review tools (AI-assisted auditing, formal verification, property-based testing) actually close the oversight gap, or will they simply accelerate the generation side further and migrate the bottleneck again?',
    'Longitudinal tracking of generation-to-review ratio as automated review tools are adopted; historical analysis of previous automation waves (compilers, static analyzers, CI/CD) and whether they closed or migrated bottlenecks',
    'If tools close the gap: Scaffold perspective confirmed, sunset is real. If tools migrate the bottleneck: the constraint is a permanent structural feature of automation-driven development, not a temporary coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(automation_closure_timeline, empirical, 'Whether automated review closes the gap or migrates the bottleneck').

omega_variable(
    liability_distribution_ambiguity,
    'When AI-generated code causes a production incident, who bears the liability: the AI vendor, the organization deploying the tool, the human reviewer who approved the PR, or the downstream maintainer who didn''t catch the bug?',
    'Legal precedent analysis as AI-generated code incidents accumulate; insurance industry risk pricing for AI-assisted development; organizational policy evolution around AI code approval authority',
    'If liability falls on reviewers: extraction is higher than measured (reviewers bear risk without compensation). If liability falls on organizations: extraction is lower (organizations internalize the cost of velocity). If liability is unresolved: the ambiguity itself is an extraction mechanism (risk is externalized to whoever lacks bargaining power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_distribution_ambiguity, preference, 'Liability distribution for AI-generated code failures').

omega_variable(
    craft_knowledge_preservation,
    'Does the velocity-oversight gap erode the transmission of software engineering craft knowledge (design patterns, debugging intuition, code smell recognition) from senior to junior developers?',
    'Longitudinal skill assessment of developers trained primarily on AI-assisted codebases vs traditional mentorship; retention rates and debugging proficiency of junior developers in high-AI-velocity environments',
    'If craft knowledge erodes: the constraint has a hidden long-term extraction component (organizational capability degradation) not captured in immediate metrics. If knowledge transmission adapts: the coordination story is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(craft_knowledge_preservation, empirical, 'Impact on craft knowledge transmission').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(automation_velocity_vs_oversight_capacity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_vel_theater_t0, automation_velocity_vs_oversight_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(auto_vel_theater_t2, automation_velocity_vs_oversight_capacity, theater_ratio, 2, 0.42).
narrative_ontology:measurement(auto_vel_theater_t4, automation_velocity_vs_oversight_capacity, theater_ratio, 4, 0.51).
narrative_ontology:measurement(auto_vel_theater_t6, automation_velocity_vs_oversight_capacity, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(auto_vel_extract_t0, automation_velocity_vs_oversight_capacity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(auto_vel_extract_t2, automation_velocity_vs_oversight_capacity, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(auto_vel_extract_t4, automation_velocity_vs_oversight_capacity, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(auto_vel_extract_t6, automation_velocity_vs_oversight_capacity, base_extractiveness, 6, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(auto_vel_suppress_t0, automation_velocity_vs_oversight_capacity, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(auto_vel_suppress_t2, automation_velocity_vs_oversight_capacity, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(auto_vel_suppress_t4, automation_velocity_vs_oversight_capacity, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(auto_vel_suppress_t6, automation_velocity_vs_oversight_capacity, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(automation_velocity_vs_oversight_capacity, resource_allocation).
narrative_ontology:affects_constraint(automation_velocity_vs_oversight_capacity, technical_debt_accumulation).
narrative_ontology:affects_constraint(automation_velocity_vs_oversight_capacity, junior_developer_skill_formation).
narrative_ontology:affects_constraint(automation_velocity_vs_oversight_capacity, production_incident_liability).

% DUAL FORMULATION NOTE:
% The automation velocity vs oversight capacity constraint is upstream of technical debt accumulation (faster velocity with shallower review increases debt), junior developer skill formation (reduced mentorship time and craft knowledge transmission), and production incident liability (ambiguous responsibility for AI-generated code failures). Each downstream constraint has its own extractiveness reflecting its specific structural dynamics, but all are influenced by the velocity-oversight gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
