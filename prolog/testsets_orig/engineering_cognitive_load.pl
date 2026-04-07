% ============================================================================
% CONSTRAINT STORY: engineering_cognitive_load
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_engineering_cognitive_load, []).

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
 *   constraint_id: engineering_cognitive_load
 *   human_readable: Engineering Cognitive Load Extraction
 *   domain: labor/organizational/cognitive_science
 *
 * SUMMARY:
 *   Engineering cognitive load extraction occurs when organizational
 *   structures (deadline compression, task distribution, responsiveness
 *   expectations) concentrate cognitive burden on individual engineers beyond
 *   their working memory and attention capacity. This constraint exhibits
 *   multiple classification types from different structural positions. From
 *   the individual engineer's perspective, it is a Snare: trapped by economic
 *   dependency with no realistic exit, suppressed by both formal expectations
 *   and informal culture norms, experiencing severe extraction of cognitive
 *   resources that would otherwise support design quality and safety. From
 *   organizational leadership's perspective, it is a Rope: they experience
 *   the constraint as pure coordination (allocating tasks) with asymmetric
 *   benefit. From the discipline's perspective, it is Tangled Rope: genuine
 *   coordination function (complex projects require distributed work) exists
 *   alongside extraction (degraded designs, safety vulnerabilities). The
 *   constraint's extractiveness has increased from 0.32 to 0.58 over the
 *   measurement interval as project complexity has outpaced team scaling and
 *   deadline pressure has intensified. Theater ratio (0.48) reflects that
 *   formal processes like Agile and project management ceremonies provide an
 *   appearance of controlled work distribution while actual cognitive load
 *   persists underneath. The analytical observer risks seeing this as a
 *   natural law of cognition (humans have fixed working memory limits) when
 *   it is actually a contingent choice to concentrate deadline pressure on
 *   individuals rather than distribute it across time, team size, or tooling.
 *
 * KEY AGENTS:
 *   - Individual Engineers: Primary victim (powerless/trapped) — bears cognitive load extraction through employment structure and economic dependency
 *   - Project Managers and Organizational Leadership: Primary beneficiary (institutional/arbitrage) — captures productivity gain and schedule compression benefit
 *   - Engineering Discipline: Secondary victim (moderate/constrained) — collective good (design quality, safety margins) degraded by individual load extraction
 *   - AI-Assisted Development Movement: Organized actor (organized/constrained) — building alternative verification pathways with sunset logic
 *   - Formal Agile Process: Institutional actor (institutional/arbitrage) — maintains performative ceremonies that mask underlying cognitive load
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent organizational choices as inherent human limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(engineering_cognitive_load, 0.58).
domain_priors:suppression_score(engineering_cognitive_load, 0.65).
domain_priors:theater_ratio(engineering_cognitive_load, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(engineering_cognitive_load, extractiveness, 0.58).
narrative_ontology:constraint_metric(engineering_cognitive_load, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(engineering_cognitive_load, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(engineering_cognitive_load, snare).
narrative_ontology:human_readable(engineering_cognitive_load, "Engineering Cognitive Load Extraction").
narrative_ontology:topic_domain(engineering_cognitive_load, "labor/organizational/cognitive_science").

domain_priors:requires_active_enforcement(engineering_cognitive_load).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(engineering_cognitive_load, project_managers).
narrative_ontology:constraint_beneficiary(engineering_cognitive_load, organizational_leadership).
narrative_ontology:constraint_victim(engineering_cognitive_load, individual_engineers).
narrative_ontology:constraint_victim(engineering_cognitive_load, design_quality).
narrative_ontology:constraint_victim(engineering_cognitive_load, safety_margins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ENGINEER (SNARE) — Trapped by employment structure, specialized skill set, and economic dependency. No realistic exit from cognitive overload: declining tasks reduces compensation and career trajectory; leaving the firm requires relocation and credential-specific job search. Maximum suppression: formal expectations (scheduling) plus informal culture (response-time norms, 'passion for the work'). Extraction is severe: cognitive resources diverted from design quality toward deadline compliance and interruption management.
constraint_indexing:constraint_classification(engineering_cognitive_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENGINEERING DISCIPLINE (TANGLED ROPE) — At the discipline level, excessive cognitive load extracts from design quality, safety margins, and long-term reliability. But the discipline also benefits from the coordination function: complex projects require distributed cognition across teams. The extraction is genuine (degraded designs, accumulated technical debt, safety vulnerabilities), but coordination is also necessary. Constrained exit: moving away from deadline-driven development requires industry-wide norm shift, not individual action.
constraint_indexing:constraint_classification(engineering_cognitive_load, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROJECT MANAGEMENT / LEADERSHIP (ROPE) — Experiences the constraint as coordination: allocating tasks and deadlines solves the legitimate problem of distributing work across a team. Net beneficiary position: extraction flows toward leadership through productivity gain, schedule compression, and risk transfer. Arbitrage exit available: can relocate cognitive load to contractors, offshore teams, or automated tooling without personal cost. For this agent, the constraint appears as pure coordination with asymmetric benefit.
constraint_indexing:constraint_classification(engineering_cognitive_load, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI-ASSISTED DEVELOPMENT MOVEMENT (SCAFFOLD) — Organized agents (tool developers, forward-thinking engineering firms, standards bodies) view cognitive load as a temporary coordination failure with a sunset: AI code generation, automated testing, and intelligent documentation are building alternative pathways that distribute cognitive burden away from individual engineers. Low effective extraction because organized actors see exit paths and have agency to build them. Sunset timeline: 10-15 years for AI tools to mature sufficiently to shift baseline expectations.
constraint_indexing:constraint_classification(engineering_cognitive_load, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL AGILE PROCESS (PITON) — Agile ceremonies (standups, sprint planning, retrospectives) are substantially performative in high-load environments. The rituals maintain appearance of controlled delivery while engineers work nights and weekends to absorb actual load. Theater ratio reflects: formal process allocates ~8 hours/week to Agile theater that does not reduce cognitive load; actual work persists underneath. The process persists through institutional inertia — firms maintain Agile rituals because alternatives haven't replaced them, not because the rituals effectively distribute load.
constraint_indexing:constraint_classification(engineering_cognitive_load, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal view, cognitive load in engineering is inherent to complex system design: managing multiple interacting components requires distributed knowledge that cannot be fully centralized, and humans have fixed working memory limits. This perspective sees the bottleneck as an immutable property of cognition itself. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that 'human cognitive limits are natural law' naturalizes what is actually a contingent organizational choice to concentrate deadline pressure on individual engineers rather than distribute it across time or team size.
constraint_indexing:constraint_classification(engineering_cognitive_load, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(engineering_cognitive_load_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(engineering_cognitive_load, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(engineering_cognitive_load, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(engineering_cognitive_load, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(engineering_cognitive_load, TR),
    TR >= 0.70.

:- end_tests(engineering_cognitive_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts cognitive resources from individual engineers through deadline compression, task multitasking, and expectation for rapid context-switching and availability. This is not fair compensation for legitimate skill deployment — the extraction is asymmetric (engineers bear full cognitive cost while organizational benefits concentrate in leadership). The measurement trajectory (0.32 → 0.58) reflects increasing project complexity and deadline intensity over the interval. Suppression (0.65): High. Significant barriers to exit include employment structure (quitting requires finding another job in a specialized field), cultural norms ('passionate engineers work hard'), formal scheduling (response-time expectations, on-call duties), and economic dependency (mortgage, benefits, stock vesting). Some engineers can negotiate reduced load (senior, specialized skills, job-market advantage), but most face real suppression. Theater ratio (0.48): Moderate. Formal Agile ceremonies and project management processes allocate visible structure and checkpoints but do not proportionally reduce actual cognitive load when the underlying deadline intensity is high. The theater has increased from 0.28 to 0.48 as more firms adopt process formalism while maintaining deadline compression.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between engineers (Snare: trapped, high extraction) and leadership (Rope: coordination with benefit). Engineers perceive the constraint as purely extractive because they bear the cognitive cost and cannot exit. Leadership perceives it as coordination because allocating tasks solves the legitimate problem of distributing work across a team and generates productivity benefit. Secondary gaps emerge between the Agile process (Piton: degraded ritual) and the AI movement (Scaffold: temporary problem with sunset). The Agile perspective sees existing formal structures as mostly performative — they maintain appearance of control without reducing load. The AI perspective sees cognitive load as solvable through tooling and distributed cognition — a temporary problem that will sunset as AI maturity increases. The discipline perspective (Tangled Rope) bridges these: coordination is necessary, but extraction is real and measurable as degraded safety margins and accumulated technical debt.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline computes directionality (d) from the agent's structural relationship to the constraint. Engineers classified as powerless/trapped have high d (near 1.0) — they experience maximum extraction because they cannot exit. Organizational leadership classified as institutional/arbitrage have low d (near 0.0–0.15) — they experience the constraint as pure coordination with benefit. Project managers classified as moderate may have d around 0.50–0.65 depending on whether they can negotiate extended deadlines with leadership or whether they are themselves trapped in deadline escalation from above. The engineering discipline as a collective has no single d value — it aggregates the d values of all agents plus the externality of degraded designs and safety margins, which affects all future projects. The analytical observer's d is around 0.72 (observer position), which drives their classification through the false-summit detection gate.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that cognitive load extraction is genuine (not pure coordination) but is not a pure Snare — it does perform a coordination function (task allocation) that is necessary for complex projects. The Tangled Rope classification (from the discipline perspective) is the most structural: coordination function (distributing work across a large team) is real; extraction (degraded design quality, safety margins, engineer wellbeing) is also real and measurable. The individual engineer's Snare perspective is their subjective reality: they experience only extraction because they cannot see the coordination benefit and cannot exit. Leadership's Rope perspective is their subjective reality: they experience only coordination because the extraction cost flows away from them. The false-summit detector on the analytical observer (Mountain) prevents naturalizing organizational design choices as cognitive law. The constraint's reality is that it is a Tangled Rope with a scaffold path opening through AI tooling and distributed deadline structures — it is not immutable but is not trivially solvable within existing power structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_load_measurement_ambiguity,
    'Is measured cognitive load a property of the task complexity or of the organizational structure?',
    'Comparative study of same projects across different organizational structures (distributed deadlines vs compressed schedules, larger teams vs tight teams, async vs synchronous workflows). If load varies significantly across structures for identical technical work, the load is extractive structure, not inherent complexity.',
    'If structural: the constraint is a Snare of organizational design, not a Mountain of inherent cognition. Classification upgrades from mountain-defense to snare-mitigation strategies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_load_measurement_ambiguity, empirical, 'Whether cognitive load derives from task complexity or organizational structure').

omega_variable(
    cognitive_load_safety_correlation,
    'Does high cognitive load causally reduce safety margins and design quality, or do low-quality projects generate higher load retrospectively?',
    'Longitudinal study tracking cognitive load metrics against safety incident rates, design review findings, and technical debt accumulation. Cross-project analysis controlling for project complexity and team experience.',
    'If causal: cognitive load extraction directly degrades safety — classification remains Snare. If correlational: poor projects attract more work; extraction mechanism is different; management visibility into causal direction affects remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_safety_correlation, empirical, 'Causal direction between cognitive load and safety degradation').

omega_variable(
    ai_tool_cognitive_displacement,
    'Do AI code generation and automated testing reduce engineer cognitive load or merely relocate it to AI system verification and prompt engineering?',
    'Time-use studies pre- and post-AI tool adoption. Measurement of mental effort distribution across code writing, testing, verification, and tool management. Surveys of engineer subjective cognitive burden.',
    'If reduces: AI is genuine sunset mechanism (Scaffold perspective validated). If relocates: AI may reduce certain load types while creating new ones; sunset timeline extends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_tool_cognitive_displacement, empirical, 'Whether AI tools reduce or relocate cognitive load').

omega_variable(
    organizational_incentive_alignment,
    'Are tight deadlines and high cognitive load driven by genuine competitive necessity or by organizational preference for deadline-based discipline?',
    'Historical analysis of firms that successfully adopted distributed-deadline and reduced-load organizational structures. Measurement of market share, profitability, and product quality outcomes. Case studies of deadline relaxation experiments.',
    'If preference: the constraint is purely extractive (Snare confirmed) and readily reformable. If necessity: the constraint has coordination function (Tangled Rope more accurate); remedies must preserve deadline coordination while reducing cognitive load.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(organizational_incentive_alignment, preference, 'Whether tight deadlines derive from competitive necessity or organizational design choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(engineering_cognitive_load, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(engcl_tr_t0, engineering_cognitive_load, theater_ratio, 0, 0.28).
narrative_ontology:measurement(engcl_tr_t5, engineering_cognitive_load, theater_ratio, 5, 0.38).
narrative_ontology:measurement(engcl_tr_t10, engineering_cognitive_load, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(engcl_be_t0, engineering_cognitive_load, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(engcl_be_t5, engineering_cognitive_load, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(engcl_be_t10, engineering_cognitive_load, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(engineering_cognitive_load, resource_allocation).
narrative_ontology:affects_constraint(engineering_cognitive_load, technical_debt_accumulation).
narrative_ontology:affects_constraint(engineering_cognitive_load, engineering_burnout_cycle).
narrative_ontology:affects_constraint(engineering_cognitive_load, safety_margin_degradation).

% DUAL FORMULATION NOTE:
% Engineering cognitive load is upstream of three distinct constraints: technical debt accumulation (the long-term legacy of rushed designs), engineering burnout (the individual psychological cost), and safety margin degradation (the collective harm). Each has its own extractiveness value reflecting its specific measurement domain. The cognitive load constraint is the structural mechanism linking all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(engineering_cognitive_load, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
