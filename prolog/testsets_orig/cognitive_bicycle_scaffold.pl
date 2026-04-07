% ============================================================================
% CONSTRAINT STORY: cognitive_bicycle_scaffold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_bicycle_scaffold, []).

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
 *   constraint_id: cognitive_bicycle_scaffold
 *   human_readable: The Bicycle of the Mind
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The cognitive bicycle represents a technological constraint where
 *   reasoning capacity is amplified through AI tool mediation. Like a
 *   physical bicycle that enables greater distance travel than unaided
 *   walking, cognitive tools extend reasoning range, speed, and complexity
 *   handling. The constraint operates as a temporary support structure:
 *   agents adopt tools to amplify capacity during performance-critical
 *   periods, with the expectation that unaugmented skills remain available or
 *   recoverable. However, the constraint exhibits degradation into dependency
 *   when adoption occurs during skill development windows, when exit costs
 *   accumulate through infrastructure lock-in, or when institutional reward
 *   systems anchor to tool-mediated output. The theater_ratio rises over time
 *   (0.25→0.52) as intellectual identity becomes increasingly mediated by
 *   tool interaction rather than grounded in independent reasoning practice.
 *   The extractiveness grows moderately (0.15→0.28) as providers capture
 *   value from adoption, as users internalize dependency, and as alternatives
 *   become less accessible. The constraint is fundamentally a scaffold: it
 *   has a genuine coordination function (enabling access to reasoning
 *   capacity across skill levels), active enforcement mechanisms (adoption
 *   policies, performance benchmarking), and a visible sunset clause
 *   (open-source alternatives, skill recovery programs, cultural shifts
 *   toward unaugmented reasoning). The perspectival gap is large: the
 *   augmented reasoner sees temporary support; the deskilled worker sees
 *   degraded piton; the service provider sees pure coordination; the
 *   institution sees hybrid extraction; the precarious worker sees a snare;
 *   the philosopher sees an immutable law of cognition.
 *
 * KEY AGENTS:
 *   - Augmented Reasoner: Primary beneficiary (moderate/constrained) — adopts tool for reasoning amplification; experiences constraint as enablement structure with visible exit path
 *   - Deskilled Cognitive Worker: Secondary victim (powerless/trapped) — over-dependent on tool during critical development window; now exhibits atrophied unaugmented reasoning and internalized belief in tool necessity
 *   - AI Service Provider: Primary beneficiary (institutional/arbitrage) — captures adoption volume, licensing revenue, and data-driven improvement from tool use at scale
 *   - Educational Institution: Hybrid actor (organized/constrained) — benefits from enhanced learning access and reduced pedagogical burden; victimized by vendor lock-in and erosion of faculty expertise
 *   - Precarious Knowledge Worker: Primary victim (powerless/trapped) — required to adopt tool for employment; bears full cost of tool unreliability and lack of control
 *   - Philosophical Observer: Analytical context (analytical/analytical) — risks naturalizing contingent tool-mediated reasoning as immutable human cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_bicycle_scaffold, 0.28).
domain_priors:suppression_score(cognitive_bicycle_scaffold, 0.35).
domain_priors:theater_ratio(cognitive_bicycle_scaffold, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, extractiveness, 0.28).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_bicycle_scaffold, scaffold).
narrative_ontology:human_readable(cognitive_bicycle_scaffold, "The Bicycle of the Mind").
narrative_ontology:topic_domain(cognitive_bicycle_scaffold, "technological/cognitive").

domain_priors:requires_active_enforcement(cognitive_bicycle_scaffold).
narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, augmented_human_cognition).
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, ai_service_provider).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, unaided_cognitive_labor).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, skill_development_pathway).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AUGMENTED REASONER (SCAFFOLD) — Individual adopting AI cognitive amplification tools. Experiences constraint as temporary support structure enabling better reasoning. Constrained exit due to knowledge lock-in and career dependence on tool output, but sunset is visible: tool training and cognitive skill recovery pathways exist. Suppression moderate — can practice unaided reasoning, but opportunity cost is high.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE DESKILLED COGNITIVE WORKER (PITON) — Agent who depended on AI augmentation during critical skill development window and now faces atrophied unaugmented reasoning capacity. Trapped in tool dependency; exit options degraded over time. Theatrical maintenance: performs intellectual work through tool mediation while internalized belief that unaugmented reasoning is ineffective persists. High theater ratio reflects performative intellectual identity sustained by system inertia rather than functional necessity.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE AI SERVICE PROVIDER (ROPE) — Coordination mechanism provider with arbitrage exit. Service provider experiences constraint as pure coordination problem: enabling human reasoning capacity through tool access is a genuine collective good. High exit optionality through market competition, API modularization, and capability licensing. Extractiveness flows toward provider but is justified by coordination function value — reasoners benefit from access, provider benefits from adoption volume.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL INSTITUTION (TANGLED ROPE) — Schools and universities face hybrid constraint. Coordination function: AI tools amplify learning capacity for disadvantaged students, reduce grading burden, enable personalized pedagogy. Extraction function: institutional dependency on proprietary tools, vendor lock-in, displacement of faculty expertise, reduced incentive for curriculum development independent of tool capability. Active enforcement required: policies regulating tool use in learning. Sunset clause: institutional independence and local tool ecosystems are building alternatives (open-source tutors, in-house LLMs). Moderate beneficiaries (enhanced learning access) and victims (skill-development pathways, faculty autonomy).
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE PRECARIOUS KNOWLEDGE WORKER (SNARE) — Gig worker, contractor, or low-autonomy employee required to adopt tool for productivity benchmarking but bearing full cost of tool failures, calibration time, and quality liability. Trapped: employment depends on tool adoption but worker has no control over tool updates, pricing, or reliability. Maximum suppression: cannot work without tool, cannot influence tool direction, cannot afford to exit. Pure extraction: employer captures productivity gains while worker absorbs risk and cognitive burden of tool mediation.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: THE PHILOSOPHICAL OBSERVER (MOUNTAIN) — From civilizational perspective, cognitive tool use is viewed as inherent to human reasoning: humans have always externalized cognition via writing, mathematics, notation systems, and tools. The cognitive bicycle is continuous with the history of writing, the printing press, and calculus. From this view, the constraint appears immutable — tool-mediated reasoning is not contingent but fundamental to human cognition. However, base metrics (extractiveness 0.28, theater 0.52) contradict the mountain gate, revealing this perspective as a false summit that naturalizes contingent institutional arrangements around tool ownership and access.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_bicycle_scaffold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_bicycle_scaffold, TR),
    TR >= 0.70.

:- end_tests(cognitive_bicycle_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The constraint extracts value from users through adoption, data generation, and vendor lock-in, but the extraction is justified by genuine reasoning amplification. The value is not as high as pure extraction mechanisms (snares at 0.46+) because users do experience real cognitive benefits and alternative tools exist. The trajectory shows growth from 0.15 to 0.28 as tool dependency consolidates and switching costs increase. Suppression (0.35): Moderate. Users can in principle choose not to adopt tools or can adopt alternatives, but opportunity costs (performance disadvantage, social comparison, institutional requirements) create soft pressure. Skill development dependency creates stronger suppression for younger cohorts. Theater ratio (0.52): Moderate-high and rising. The constraint exhibits increasing performative content as intellectual identity becomes mediated by tool interaction: the 'thinking' that occurs is visibly filtered through the tool's interface, generating documentation of the thinking process that becomes more valued than the reasoning itself. The rise from 0.25 to 0.52 reflects this drift—tools transition from invisible enabling infrastructure to prominent centers of cognitive performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same technological structure produces radically different classifications depending on structural position. The augmented reasoner (moderate power, constrained exit) sees a temporary coordination tool (Scaffold) — they retain control and can envision recovery. The deskilled worker (powerless, trapped) sees an inescapable dependency (Piton) — the tool was meant to be temporary but became internalized. The service provider (institutional power, arbitrage exit) sees pure coordination (Rope) — they are solving a genuine problem of enabling reasoning access. The educational institution sees hybrid extraction (Tangled Rope) — genuine benefits to learning access but entangled with vendor capture and skill displacement. The precarious worker sees pure extraction (Snare) — the tool is imposed, failures are their liability, benefits accrue elsewhere. The philosophical observer sees an immutable cognitive law (Mountain) — but this is a false summit: the mounting theater_ratio and rising extractiveness reveal that tool-mediated reasoning has become increasingly contingent on proprietary infrastructure rather than fundamental to cognition. The perspectival gap is not an observational puzzle but a structural phenomenon revealing how power and exit options determine experienced constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from power level, exit options, and structural position in the extraction/coordination balance. The service provider (institutional power, arbitrage exit, beneficiary status) derives low d ≈ 0.15 through the beneficiary + arbitrage pathway, producing negative effective extractiveness — they are a net beneficiary of the coordination. The augmented reasoner (moderate power, constrained exit, mixed beneficiary/victim) derives mid-range d ≈ 0.50, producing moderate chi through the mixed pathway — they experience both benefit and cost. The deskilled worker (powerless, trapped, victim status) derives high d ≈ 0.95 through the victim + trapped pathway, producing maximum experienced extractiveness via sigmoid f(d) ≈ 1.42 — they bear full cost with no exit. The precarious worker also derives high d through victim + trapped, but may be overridden with explicit directionality_overrides if analysis reveals additional institutional capture (e.g., employer control over tool configuration). The educator (organized power, constrained exit, hybrid beneficiary/victim) derives moderate-high d ≈ 0.60 reflecting the tangled rope position — both benefits (learning access) and costs (vendor lock-in) are real, and the institution has some but not complete agency. No directionality overrides are needed here because the structural derivation chain captures the essential relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The cognitive bicycle constraint resolves the mandatrophy through temporal framing and sunset clarity. The scaffold classification is confirmed because: (1) genuine coordination function exists (amplifying reasoning access is a real collective good), (2) active enforcement is required (adoption policies, integration into workflows), (3) a visible sunset clause exists (open-source alternatives, skill recovery programs, cultural shifts toward reasoning autonomy), and (4) theater_ratio remains below 0.70 at current interval endpoint (0.52), meaning performative content has not yet swallowed functional content. The constraint is not a false coordination-as-extraction masquerade because users can articulate and pursue exit: learning to reason unaided is possible, open-source tools reduce vendor lock-in, institutional policies can mandate tool-independent skill assessment. However, the rising theater_ratio (0.25→0.52) and rising extractiveness (0.15→0.28) are warning signals: if current trajectories continue beyond the interval endpoint, the constraint will degrade from Scaffold toward Tangled Rope (if institutional enforcement tightens) or Piton (if theater_ratio exceeds 0.70). The mandatrophy is resolved by recognizing that scaffold constraints inherently face drift risk — the sunset clause must be actively maintained through open-source development, skill preservation curricula, and periodic disengagement practice. If these sunset mechanisms atrophy, the constraint morphs into a degraded form. The analytical observer's mountain classification is false: there is nothing immutable about tool-mediated reasoning. The theatrical increase shows that the constraint's 'necessity' is socially constructed through interface prominence, reward architecture, and internalized expectations of tool dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_atrophy_threshold,
    'At what cumulative duration of tool-mediated reasoning does unaugmented cognitive skill atrophy become irreversible within a human lifetime?',
    'Longitudinal cognitive testing of tool-dependent and tool-naive cohorts; measurement of reasoning recovery rates after tool removal; identification of neuroplasticity windows',
    'If threshold < 2 years: skill recovery is feasible and sunset clause is real. If threshold > 10 years: dependency is nearly permanent and scaffold becomes piton or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_atrophy_threshold, empirical, 'Reversibility timeline for cognitive skill atrophy from tool dependency').

omega_variable(
    coordination_function_necessity,
    'What proportion of tool-amplified reasoning output represents genuine cognitive extension versus displacement of work that should be performed unaided?',
    'Task-by-task analysis: classification of tool use as amplification (enabling new thinking) vs substitution (replacing fundamental skill practice); measurement of downstream reasoning quality on unaided tasks',
    'If amplification dominates: constraint is legitimate scaffold. If substitution dominates: constraint is snare or piton masquerading as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether tool use amplifies or substitutes cognitive labor').

omega_variable(
    open_cognitive_infrastructure_viability,
    'Can decentralized, open-source, locally-controlled cognitive tools achieve parity with proprietary systems in reasoning amplification while maintaining user autonomy?',
    'Comparative analysis of open-source vs proprietary tool performance on standardized reasoning tasks; measurement of user control and exit cost; projection of open-source capability growth rates',
    'If viability confirmed: sunset clause is real and scaffold is structural. If open-source tools remain inferior: dependency on proprietary systems persists and scaffold transitions to tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_cognitive_infrastructure_viability, empirical, 'Whether open-source tools can provide credible alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_bicycle_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cogbike_tr_t0, cognitive_bicycle_scaffold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cogbike_tr_t5, cognitive_bicycle_scaffold, theater_ratio, 5, 0.38).
narrative_ontology:measurement(cogbike_tr_t10, cognitive_bicycle_scaffold, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(cogbike_be_t0, cognitive_bicycle_scaffold, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cogbike_be_t5, cognitive_bicycle_scaffold, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(cogbike_be_t10, cognitive_bicycle_scaffold, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_bicycle_scaffold, resource_allocation).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, skill_development_displacement).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, cognitive_labor_commodification).
narrative_ontology:affects_constraint(cognitive_bicycle_scaffold, human_ai_epistemic_dependency).

% DUAL FORMULATION NOTE:
% The cognitive bicycle decomposes into three related constraints: skill-development displacement (how tool mediation during learning windows creates permanent dependency), cognitive labor commodification (how AI service providers extract value from reasoning tasks), and human-AI epistemic dependency (how reasoning validation becomes mediated through tool output). This story addresses the scaffold structure shared across all three; downstream stories analyze the snare and tangled rope variants that emerge when specific contexts (skill development, precarious labor, institutional capture) activate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
