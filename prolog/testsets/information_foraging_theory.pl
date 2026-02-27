% ============================================================================
% CONSTRAINT STORY: information_foraging_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_foraging_theory, []).

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
 *   constraint_id: information_foraging_theory
 *   human_readable: Information Foraging Theory (IFT)
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   Information Foraging Theory (IFT), formalized by Pirolli & Card (1999),
 *   posits that humans searching for information behave analogously to
 *   animals foraging for food: they assess information scent, evaluate patch
 *   value, and decide when to leave a depleted patch for a new one. The
 *   theory emerged from cognitive science as a legitimate descriptive model
 *   of information-seeking behavior. However, over the past 15 years, IFT has
 *   undergone a structural transformation in applied contexts. Platform
 *   operators, interface designers, and attention economists have
 *   operationalized IFT not as a constraint to accommodate but as a design
 *   target to optimize. Infinite scroll, recommendation loops, and
 *   algorithmic curation are justified through IFT logic. The constraint now
 *   functions as a hybrid: it provides genuine coordination benefits (helping
 *   users discover information) while simultaneously enabling extraction of
 *   attention beyond user intent. The constraint exhibits a rising theater
 *   ratio (0.25 → 0.58) reflecting the increasing gap between rigorous
 *   cognitive science and rationalization-for-extraction. The foraging
 *   metaphor itself becomes performative — invoked to justify superstimuli
 *   design without implementing the core IFT mechanism (patch depletion
 *   costs). The constraint is not a natural law but a design choice with
 *   institutional and economic drivers.
 *
 * KEY AGENTS:
 *   - Information Seekers: Primary victims (powerless/trapped) — experience hijacked attention and superstimuli without exit options; cognitive autonomy reduced
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — benefit from engagement optimization; experience IFT as legitimate coordination technology
 *   - Interface Designers: Secondary actors (powerful/constrained) — constrained by engagement metrics; use IFT as actionable framework; experience both enabling and extractive dimensions
 *   - Attention Economists: Secondary beneficiaries (powerful/mobile) — research and optimize attention capture; publish on IFT mechanisms; have mobility through academic positions
 *   - Cognitive Autonomy Coalition: Organized resistance (organized/constrained) — digital rights advocates, humane tech researchers, user experience ethicists; constrained by platform entrenchment
 *   - Academic Establishment: Institutional (institutional/arbitrage) — legitimacy source for IFT; maintains rigor in theory but has limited control over application; experiences piton degradation as industry uses theory for post-hoc justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_foraging_theory, 0.38).
domain_priors:suppression_score(information_foraging_theory, 0.42).
domain_priors:theater_ratio(information_foraging_theory, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_foraging_theory, extractiveness, 0.38).
narrative_ontology:constraint_metric(information_foraging_theory, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(information_foraging_theory, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_foraging_theory, tangled_rope).
narrative_ontology:human_readable(information_foraging_theory, "Information Foraging Theory (IFT)").
narrative_ontology:topic_domain(information_foraging_theory, "technological/cognitive").

domain_priors:requires_active_enforcement(information_foraging_theory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_foraging_theory, interface_designers).
narrative_ontology:constraint_beneficiary(information_foraging_theory, attention_economists).
narrative_ontology:constraint_beneficiary(information_foraging_theory, algorithmic_platforms).
narrative_ontology:constraint_victim(information_foraging_theory, cognitive_autonomy).
narrative_ontology:constraint_victim(information_foraging_theory, epistemic_commons).
narrative_ontology:constraint_victim(information_foraging_theory, user_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INFORMATION SEEKER (SNARE) — Individual users framed as foragers cannot exit the constraint. IFT naturalizes attention harvesting as inevitable behavioral biology. Users experience the full extraction: algorithmic curation designed around foraging analogies creates superstimuli (infinite scroll, recommendation loops) that hijack search behavior. No alternative information ecology available at scale.
constraint_indexing:constraint_classification(information_foraging_theory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PLATFORM OPERATOR (ROPE) — Experiences IFT as a legitimate coordination mechanism. The foraging metaphor enables efficient matching between users and content patches. Platforms benefit from engagement optimization but also genuinely solve the information discovery coordination problem. From this view, IFT is cooperative technology design that scales information access.
constraint_indexing:constraint_classification(information_foraging_theory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE INTERFACE DESIGNER (TANGLED ROPE) — Constrained by metrics (engagement, time-on-site, retention) that optimize for foraging behavior. Also benefits from IFT as a legitimate design framework that makes cognitive science actionable. Experiences the constraint as both enabling (I have a theory to justify designs) and extractive (I am measured on attention capture, not user flourishing). Active enforcement through metrics dashboards and engagement targets.
constraint_indexing:constraint_classification(information_foraging_theory, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ACADEMIC ESTABLISHMENT (PITON) — IFT began as genuine cognitive science describing how humans search for information (Pirolli & Card, 1999). The theory has become largely performative in industry application: companies invoke 'foraging behavior' to justify attention harvesting, but the academic content (patch depletion costs, information scent) is rarely operationalized precisely. IFT persists as institutional legitimacy theater even as the underlying science is challenged by neuroscience findings on dopamine and behavioral addiction. Theater ratio reflects the gap between rigorous theory and rationalization-for-extraction.
constraint_indexing:constraint_classification(information_foraging_theory, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE COGNITIVE AUTONOMY COALITION (TANGLED ROPE) — Organized agents (digital rights groups, attention researchers, humane tech advocates) see IFT as both coordination and extraction. The theory genuinely solves information discovery problems (coordination benefit), but industry operationalization systematically extracts attention beyond user interest (extraction). Active enforcement of alternative frameworks (attention budgets, friction-by-design, serendipity-loss awareness) constrained by platform entrenchment.
constraint_indexing:constraint_classification(information_foraging_theory, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational view, IFT risks being framed as a natural law: 'humans are information foragers; this is inevitable biology.' The engine will detect this as a false summit. The constraint is not a law of nature but an institutional choice — the mapping of foraging metaphor onto interface design is contingent, not inherent to human cognition. The extractiveness arises from specific design choices and business models, not from the biological fact of foraging.
constraint_indexing:constraint_classification(information_foraging_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_foraging_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_foraging_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_foraging_theory, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_foraging_theory, TR),
    TR >= 0.70.

:- end_tests(information_foraging_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. IFT operationalization does extract attention beyond user intent, but the extraction is not pure (unlike a predatory snare) because the theory also solves a genuine coordination problem — information discovery at scale. The extraction arises from systematic misalignment: platforms optimize for engagement while users optimize for goal achievement. These diverge over time, but the starting state (user wants to find information; platform helps them find it) is genuinely coordinated. The extractiveness value reflects this hybrid nature. Suppression (0.42): Moderate. Users cannot easily exit attention-optimized platforms, and alternative information ecologies operate at smaller scale. However, suppression is not total — users can reduce engagement, switch platforms, or use friction tools. The rise in theater ratio (0.25 → 0.58) reflects increasing performative content: as neuroscience reveals dopamine exploitation and behavioral addiction mechanisms, industry invokes IFT more explicitly to justify designs that IFT itself wouldn't necessarily endorse. Theater ratio growth indicates Goodhart drift — engagement metrics have replaced the original coordination goal (helping users find relevant information).
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap separates beneficiaries and victims. The platform operator sees coordination (Rope): IFT enables efficient information routing at scale. The information seeker sees extraction (Snare): behavioral superstimuli hijack attention independent of actual information need. The interface designer occupies the hybrid middle (Tangled Rope): constrained by engagement metrics but also using IFT legitimately to solve discovery problems. The academic establishment sees its own degradation (Piton): the theory was rigorous; its application became performative. The cognitive autonomy coalition sees mixed coordination-extraction (Tangled Rope): the mechanisms work but are operationalized asymmetrically. The analytical observer risks seeing natural law (false summit): 'humans are foragers; this is inevitable' — but the constraint is contingent on specific business models, interface choices, and metric optimization.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value derives from their structural position relative to the attention extraction flow. Beneficiaries (platforms, designers, attention economists) experience low or negative d — they capture value from the extraction. Interface designers are constrained (d ≈ 0.50) because they both benefit from a working design theory and are measured on extraction targets. The information seeker is trapped (d ≈ 0.95): no exit options, no benefit, full cost of attention hijacking. The cognitive autonomy coalition is organized but constrained (d ≈ 0.55): has power to articulate alternatives but lacks institutional leverage to implement them at scale. The directionality chain reveals that the constraint's extractiveness is not inevitable but structural — it arises from specific alignments of incentives and metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   IFT resolves the mandatrophy by disambiguating the theory from its application. The theory itself (Pirolli & Card, 1999) is legitimate cognitive science describing how humans search information. The constraint is not 'information foraging is real' but 'information foraging has been operationalized as a target for attention extraction.' These are distinct structural claims. From a powerless agent's perspective, IFT-optimized interfaces produce Snare classification: full extraction with no coordination benefit (they experience hijacked attention). From an institutional perspective (platform operator), the same interfaces produce Rope classification: genuine coordination with minimal extraction overhead. The mandatrophy is resolved not by choosing one type but by recognizing that the six types represent different structural positions in the same system. The false summit classification at the analytical level identifies the naturalization risk: framing contingent design choices (optimize for engagement) as inevitable biology ('humans are foragers'). The constraint's true nature emerges from the perspectival ensemble: a hybrid coordination-extraction system whose balance is determined by operational choices, not by cognitive science necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foraging_metaphor_structural_validity,
    'Is the foraging metaphor a structurally accurate model of human information search, or does it systematically mischaracterize goal-directed cognition as scavenging?',
    'Empirical comparison of user behavior under foraging-optimized interfaces vs goal-tracking interfaces; eye-tracking and cognitive load studies; longitudinal user satisfaction and knowledge retention metrics',
    'If valid: IFT classification shifts toward Rope (coordination mechanism). If invalid: IFT is a rationalization for attention extraction, classification shifts toward Snare, and ''foraging behavior'' reflects design-induced pathology rather than natural cognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foraging_metaphor_structural_validity, empirical, 'Whether foraging metaphor accurately characterizes information search').

omega_variable(
    patch_depletion_operationalization,
    'Do modern algorithms actually implement patch depletion costs (the core mechanism in IFT), or do they exploit the absence of such costs?',
    'Analysis of recommendation algorithm behavior: measurement of whether systems increase friction as patches deplete, or whether they lower friction to capture users between patches. Comparison of engagement metrics under patch-cost vs patch-free designs.',
    'If operationalized: IFT functions as theory. If absent: algorithms exploit information scent without behavioral cost feedback, making IFT a post-hoc rationalization for a different extraction mechanism (superstimuli without foraging logic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patch_depletion_operationalization, empirical, 'Whether algorithms implement patch depletion costs').

omega_variable(
    cognitive_autonomy_collapse,
    'To what degree does IFT-optimized interface design measurably reduce user cognitive autonomy (volitional information seeking) vs enhance information access?',
    'Randomized controlled trials comparing user goal-achievement rates and self-reported autonomy on foraging-optimized vs neutral interfaces. Longitudinal studies on information diet diversity and serendipity.',
    'If autonomy preserved: extraction classification is overstated, constraint shifts toward Rope. If autonomy degraded: victims classification (cognitive_autonomy) is vindicated, suppression metrics increase, classification remains Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_autonomy_collapse, empirical, 'Degree of cognitive autonomy loss under IFT-optimized design').

omega_variable(
    alternative_framework_viability,
    'Can information systems be designed to preserve foraging benefits (efficient patch discovery) while implementing friction mechanisms (patch depletion costs, attention budgets) that prevent superstimulus exploitation?',
    'Prototype implementations of attention-budgeted search, friction-by-design information discovery, and user-directed information scent. Measurement of engagement reduction vs user satisfaction and goal achievement.',
    'If viable: scaffold perspective confirmed — IFT''s extraction is contingent and can be sunset by alternative architectures. If not viable: the foraging mechanism itself may enable exploitation, making the constraint more structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Whether attention-aware alternatives to IFT-optimized design are viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_foraging_theory, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ift_tr_t0, information_foraging_theory, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ift_tr_t5, information_foraging_theory, theater_ratio, 5, 0.42).
narrative_ontology:measurement(ift_tr_t10, information_foraging_theory, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ift_be_t0, information_foraging_theory, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ift_be_t5, information_foraging_theory, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ift_be_t10, information_foraging_theory, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_foraging_theory, information_standard).
narrative_ontology:affects_constraint(information_foraging_theory, attention_economy).
narrative_ontology:affects_constraint(information_foraging_theory, algorithmic_curation).
narrative_ontology:affects_constraint(information_foraging_theory, behavioral_addiction_mechanisms).

% DUAL FORMULATION NOTE:
% IFT is part of a constraint family connecting cognitive science to platform economics. Upstream: the genuine cognitive science of information search (foraging as descriptive model). Downstream: specific operationalizations in recommendation algorithms and engagement optimization (foraging as design target). The ε difference reflects the gap between descriptive accuracy and prescriptive misuse. The upstream constraint is lower extractiveness (ε ≈ 0.08, mountain/rope boundary); the downstream constraint is higher extractiveness (ε ≈ 0.38, tangled rope) because application context enables extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_foraging_theory, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
