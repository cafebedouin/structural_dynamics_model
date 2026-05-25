% ============================================================================
% CONSTRAINT STORY: ai_task_horizon_reliability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_task_horizon_reliability, []).

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
 *   constraint_id: ai_task_horizon_reliability
 *   human_readable: The AI Task Horizon and Reliability Bottleneck
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The AI task-horizon constraint defines the systematic inverse
 *   relationship between task complexity and AI system reliability. As task
 *   complexity increases (measured in human expert time-to-complete, decision
 *   tree depth, or domain knowledge requirements), AI success rates decline
 *   nonlinearly. This constraint creates a hybrid coordination-extraction
 *   structure: AI systems genuinely solve well-scoped, low-complexity tasks
 *   efficiently (coordination benefit), but vendor marketing systematically
 *   claims capability far beyond demonstrated performance (extractive
 *   structure). The constraint operates across labor markets, procurement
 *   decisions, and knowledge domains, creating multiple perspectives that
 *   span all six constraint types. Domain experts experience pure extraction
 *   (snare): their expertise becomes unpaid validation labor for AI systems
 *   marketed as capable. End users experience pure extraction: they purchase
 *   systems marketed as capable of complex reasoning but receive systems that
 *   fail systematically on the actual tasks they were deployed to solve.
 *   Developers and vendors experience pure coordination: the gap between
 *   claimed and actual capability creates markets for their services and
 *   enables labor arbitrage. Enterprise teams experience hybrid
 *   coordination-extraction: genuine efficiency gains on simple tasks bundled
 *   with forced acquisition of AI integration specialists and reliability
 *   patches. The constraint's theater ratio (0.58, rising to 0.72 in some
 *   measurement studies) reflects that vendor claims are substantially
 *   performative: benchmarks measure average performance across curated
 *   datasets rather than reliability on domain-expert tasks, enabling
 *   misleading marketing through selective metrics.
 *
 * KEY AGENTS:
 *   - Domain Experts: Primary victims (powerless/trapped) — expertise systematically devalued as AI marketing inflates perceived capability; labor market collapses as organizations assume AI replaces specialized knowledge
 *   - End Users: Primary victims (powerless/trapped) — purchase AI systems on marketing claims, discover systematic failures on complex tasks after deployment and investment; no exit without organizational cost
 *   - AI Developers: Primary beneficiary (institutional/arbitrage) — benefit from marketing flexibility and labor arbitrage; capture revenue during verification lag before failures are widely understood
 *   - Task-Simplification Vendors: Secondary beneficiary (institutional/arbitrage) — build fine-tuning services, prompt libraries, and workflow redesign tools that bridge the gap between claimed and actual capability
 *   - Enterprise AI Teams: Secondary victim (moderate/constrained) — experience both efficiency gains on simple tasks and forced acquisition of integration/validation infrastructure
 *   - Reliability Certification Movement: Organized intervener (organized/constrained) — building standards and verification protocols to close marketing-reality gap; genuine sunset structure if standards become industry norm
 *   - Performance Metrics Theater: Institutional maintainer (institutional/arbitrage) — benchmark ecosystem and metrics publishing perpetuate misleading capability claims through selective metrics and curated datasets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_task_horizon_reliability, 0.52).
domain_priors:suppression_score(ai_task_horizon_reliability, 0.68).
domain_priors:theater_ratio(ai_task_horizon_reliability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_task_horizon_reliability, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_task_horizon_reliability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_task_horizon_reliability, tangled_rope).
narrative_ontology:human_readable(ai_task_horizon_reliability, "The AI Task Horizon and Reliability Bottleneck").
narrative_ontology:topic_domain(ai_task_horizon_reliability, "technological/economic").

domain_priors:requires_active_enforcement(ai_task_horizon_reliability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, ai_system_developers).
narrative_ontology:constraint_beneficiary(ai_task_horizon_reliability, task_simplification_vendors).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, complex_task_domains).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, end_user_reliability_expectations).
narrative_ontology:constraint_victim(ai_task_horizon_reliability, domain_expert_labor_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMAIN EXPERT (SNARE) — Trapped in labor market where AI systems are marketed as capable of complex reasoning but systematically fail at actual domain tasks. Cannot exit: their expertise becomes 'expensive augmentation to AI failures' rather than independent knowledge. Career path collapses as demand shifts to 'prompt engineers' and AI babysitters rather than substantive domain work. Experienced extraction is maximal — the constraint converts their accumulated knowledge into unpaid validation labor.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END USER (SNARE) — Trapped in marketing-reality gap. AI system performs simple tasks well but fails catastrophically on the complex reasoning tasks for which it was purchased. Cannot exit without organizational cost; discovering the failure often comes after deployment and investment. Bears full cost of unreliability without recourse. High suppression: user reviews are suppressed by selective highlighting of 'wins', and the failure modes are often subtle (plausible-sounding wrong answers).
constraint_indexing:constraint_classification(ai_task_horizon_reliability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENTERPRISE INTEGRATION TEAM (TANGLED ROPE) — Constrained by vendor lock-in and internal pressure to 'make AI work', but also benefits from AI productivity gains on well-scoped tasks. Experiences both extraction (forced to hire reliability specialists, maintain human fallback systems) and coordination (genuine efficiency gains on document routing, code generation for boilerplate). Suppression is moderate — they have some leverage to negotiate, but are still primary targets for vendor task-horizon marketing.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AI DEVELOPER (ROPE) — Primary beneficiary. Benefits from the task-horizon constraint through (a) marketing flexibility (can claim capability at task complexity it cannot achieve), (b) labor arbitrage (domain experts are cheaper in abundance when their skills are devalued by AI marketing), (c) coordination benefit (the constraint creates demand for their integration services, prompt engineering, reliability-patching tools). They experience the constraint as a coordination mechanism: the gap between claimed and actual capability creates a market for their downstream services.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TASK SIMPLIFICATION VENDOR (ROPE) — Coordinated beneficiary. Sells domain-specific fine-tuning, prompt libraries, retrieval-augmented generation systems, and workflow redesign services that bridge the task-horizon gap. The constraint creates their entire market — they benefit from the gap existing. Experiences the constraint as pure coordination benefit with no experienced extraction.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RELIABILITY CERTIFICATION MOVEMENT (SCAFFOLD) — Organized agents (academic researchers, regulatory bodies, certification standards organizations) are building independent verification protocols: task-specific benchmarks, domain expert evaluation, failure mode analysis, and capability claims standards. This is genuinely a temporary support structure with sunset logic — as these standards mature and vendors comply, the marketing-reality gap closes and the constraint loses extraction force. Sunset timeline: 5-10 years for standards to become industry norm.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PERFORMANCE METRICS THEATER (PITON) — The constraint is maintained through performative metrics: benchmark scores on curated datasets that bear little resemblance to real-world complex task environments. Vendors measure success on 'average performance across diverse tasks' rather than 'reliable performance on domain-expert tasks'. The metrics theater persists through institutional inertia (it enables continued venture funding, enables press releases, enables procurement decisions) despite being widely understood to be misleading. Theater ratio: high (0.72) because the entire evaluation ecosystem is theater masquerading as rigor.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational viewpoint, the constraint has both genuine coordination value (AI systems DO solve simple, well-scoped tasks efficiently) and extractive structure (the marketing claims vastly exceed actual capability, creating labor market collapse in knowledge domains and systematic unreliability in deployment). The constraint is fundamentally hybrid: it genuinely improves coordination on high-frequency low-complexity tasks while systematically extracting reliability and trust from complex domains. Neither pure coordination nor pure extraction captures it.
constraint_indexing:constraint_classification(ai_task_horizon_reliability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_task_horizon_reliability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_task_horizon_reliability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_task_horizon_reliability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_task_horizon_reliability, TR),
    TR >= 0.70.

:- end_tests(ai_task_horizon_reliability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits substantial extraction through multiple mechanisms: (1) labor market collapse in complex domains (domain expert devaluation), (2) procurement fraud through overclaimed capability, (3) forced acquisition of integration/validation specialists. However, extraction is not as severe as a pure snare (which would be 0.70+) because AI systems DO deliver genuine value on well-scoped tasks, and the coordination benefit is real. The 0.52 reflects that roughly half the experienced impact is coordination (genuine efficiency) and half is extraction (capability gap). Suppression (0.68): Moderate-high. Multiple suppression mechanisms: (a) marketing claims are presented as expert consensus despite being internally contested, (b) vendor performance metrics are curated (averaged across domains where AI performs well, omitting domains where it fails), (c) failure modes are often subtle (plausible-sounding wrong answers are harder to detect than obvious errors), (d) vendor lock-in makes exit costly, (e) domain experts who identify failures are dismissed as 'resisting AI adoption'. Theater ratio (0.58): Moderate-high. Performance metrics and benchmarking are substantially performative: they measure capability on curated test sets rather than reliability on domain-expert tasks; they report average performance across diverse task distributions rather than reliable performance on specific complex domains; they obscure failure modes. The theater has increased from 0.45 to 0.58 over the measurement interval as vendors have become more sophisticated in selective metrics presentation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The domain expert sees pure snare (powerless/trapped) — their expertise becomes devalued and they have no exit. The end user sees pure snare (powerless/trapped) — they purchased systems on marketing claims that failed on complex tasks. The AI developer sees pure rope (institutional/arbitrage) — the gap between claimed and actual capability is their entire revenue model. The certification movement sees temporary scaffold (organized/constrained) — standards and verification protocols have genuine sunset logic if vendors adopt them. The performance metrics theater sees piton (institutional/arbitrage) — performative benchmarking persists through institutional inertia despite being widely understood as misleading. The analytical observer sees tangled rope (analytical/analytical) — the constraint simultaneously delivers genuine coordination value on simple tasks and systematically extracts reliability and trust from complex domains. The perspectival gap reflects that the same structural constraint — the task-horizon inverse reliability relationship — is experienced as complete extraction by trapped agents, complete coordination by beneficiary institutions, temporary failure by oversight movements, and hybrid coordination-extraction by analytical observation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural position relative to the extraction flow. Domain experts and end users are trapped victims: they have no exit from the systems deployed, they bear full cost of failures, and they have no leverage. Their d ≈ 0.95, producing maximum f(d) ≈ 1.42. AI developers are institutional beneficiaries with arbitrage: they can exit the market at any time (and move to other products), they benefit from the gap between claimed and actual capability, and they have market leverage. Their d ≈ 0.05, producing f(d) ≈ -0.12 (negative effective extraction, i.e., subsidy). Enterprise teams are constrained moderate victims: they have some organizational leverage and some benefit from genuine AI efficiency, but they are primarily targets for vendor extraction through lock-in and integration service sales. Their d ≈ 0.55, producing f(d) ≈ 0.75. The certification movement is organized with constrained exit (they can build standards but cannot force adoption). Their d ≈ 0.40, producing f(d) ≈ 0.40 (moderate experienced extraction from resistance to certification adoption).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy 'is this pure coordination (rope) or pure extraction (snare)?' is resolved by recognizing that the answer is both, depending on the task domain and the agent's structural position. For simple, well-scoped tasks (code generation, document routing, question answering from provided context), the constraint operates as pure coordination: AI systems solve the problem efficiently and reliably. For complex, domain-expert tasks (medical diagnosis, scientific methodology critique, strategic decision-making), the constraint operates as pure snare: vendors market capability they cannot deliver, extract revenue on false premises, and leave users bearing the cost of failures. The constraint is NOT a single coordination-extraction hybrid; it is a domain-dependent phenomenon that exhibits rope characteristics in simple domains and snare characteristics in complex domains. The 0.52 extractiveness reflects the market-wide average, but this average obscures the real structure: marketing claims are concentrated on complex domains (where AI fails), while actual deployments and positive ROI are concentrated on simple domains (where AI succeeds). The tangled-rope classification at the analytical level captures this: the constraint genuinely has coordination function (on simple tasks) AND systematically exhibits asymmetric extraction (on complex tasks), AND requires active enforcement (vendor marketing practices are enforced through selective metrics). Mandatrophy is resolved: the constraint is neither falsely naturalized as inevitable (mountain) nor falsely purified as pure coordination (rope) nor falsely simplified as pure extraction (snare). It is a hybrid coordination-extraction mechanism whose extraction concentration in complex domains creates the appearance of pure snare from the perspective of domain experts and complex-task users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    task_complexity_threshold,
    'What is the precise threshold of task complexity (measured in human expert time-to-complete or decision tree depth) below which AI systems achieve >95% reliability and above which they drop below 60%?',
    'Large-scale empirical study across 50+ domains comparing human expert task completion time vs AI reliability curves; identification of inflection point',
    'If threshold < 2 hours: constraint is coordination problem (Rope from all perspectives). If threshold > 8 hours: constraint is fundamentally extractive (Snare from more perspectives). Threshold location determines whether the gap is an inevitable technical limitation or a marketing choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(task_complexity_threshold, empirical, 'Task complexity threshold for AI reliability collapse').

omega_variable(
    marketing_claim_intentionality,
    'Are vendors knowingly marketing beyond demonstrated capability (intentional extraction), or do they genuinely believe their systems are more capable than empirical testing shows (good faith estimation failure)?',
    'Internal documentation analysis (litigation discovery, leaked training materials); comparison of vendor''s internal benchmarks vs public claims; whistleblower testimony',
    'If intentional: constraint is pure snare (extraction). If good faith failure: constraint is scaffold (temporary miscalibration being corrected). Intentionality determines whether suppression (0.68) reflects deception or incompetence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marketing_claim_intentionality, preference, 'Whether vendors intentionally market beyond demonstrated capability').

omega_variable(
    domain_expert_labor_substitution,
    'Does AI task-horizon constraint primarily create demand for domain experts (as reliability validators), or does it primarily destroy domain expert labor markets (as claimed expertise becomes devalued)?',
    'Labor market analysis: domain expert employment and wage trends pre-vs-post AI deployment; exit rate from expertise roles; salary ratios (domain expert vs prompt engineer); hiring manager interviews on whether domain expertise is required',
    'If primarily creates demand: victims are temporary (Scaffold perspective is correct). If primarily destroys demand: victims are permanent (Snare perspective is correct). The labor market trajectory determines whether the constraint''s extraction is temporary or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_expert_labor_substitution, empirical, 'Whether AI task horizon creates or destroys domain expert labor demand').

omega_variable(
    reliability_certification_viability,
    'Can independent certification of AI reliability claims (capability audits, domain-expert evaluation standards, domain-specific benchmarks) actually close the marketing-reality gap, or will vendors find ways to circumvent or ignore certifications?',
    'Case studies of regulatory/certification compliance in adjacent domains (autonomous vehicles, medical devices); analysis of vendor response to transparency requirements; comparison of certified vs non-certified AI product adoption',
    'If certification works: scaffold perspective is confirmed, sunset is real, constraint has 5-10 year horizon. If vendors circumvent: scaffold is aspirational, suppression remains high, constraint persists indefinitely (snare perspective is correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_certification_viability, empirical, 'Whether reliability certification can close marketing-reality gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_task_horizon_reliability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aithr_tr_t0, ai_task_horizon_reliability, theater_ratio, 0, 0.45).
narrative_ontology:measurement(aithr_tr_t3, ai_task_horizon_reliability, theater_ratio, 3, 0.52).
narrative_ontology:measurement(aithr_tr_t6, ai_task_horizon_reliability, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(aithr_be_t0, ai_task_horizon_reliability, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(aithr_be_t3, ai_task_horizon_reliability, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(aithr_be_t6, ai_task_horizon_reliability, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_task_horizon_reliability, information_standard).
narrative_ontology:affects_constraint(ai_task_horizon_reliability, domain_expert_labor_market_collapse).
narrative_ontology:affects_constraint(ai_task_horizon_reliability, ai_capability_inflation_in_procurement).
narrative_ontology:affects_constraint(ai_task_horizon_reliability, vendor_lock_in_via_integration_costs).

% DUAL FORMULATION NOTE:
% The AI task-horizon constraint can be decomposed into three structurally distinct claims: (1) EMPIRICAL CONSTRAINT: AI systems exhibit nonlinear reliability degradation as task complexity increases (ε ≈ 0.15, Mountain if this is the claim). (2) MARKETING CONSTRAINT: Vendors market AI capability far beyond demonstrated performance (ε ≈ 0.68, Snare if this is the claim). (3) LABOR MARKET CONSTRAINT: Domain expert labor markets collapse due to AI marketing claims exceeding capability (ε ≈ 0.65, Snare if this is the claim). The present story integrates all three, producing the tangled-rope classification. To decompose: the empirical constraint (task-horizon reliability relationship) is a genuine structural property of current AI systems; the marketing constraint and labor market constraint are contingent institutional arrangements. A story focused purely on the empirical relationship would classify as mountain; stories focused on the institutional extraction mechanisms would classify as snare. The present story treats them as unified, reflecting that the constraint's extraction force comes from the gap BETWEEN the empirical limitation and the marketing claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_task_horizon_reliability, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
