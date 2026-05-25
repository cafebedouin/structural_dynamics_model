% ============================================================================
% CONSTRAINT STORY: decision_fatigue_opportunity_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decision_fatigue_opportunity_cost, []).

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
 *   constraint_id: decision_fatigue_opportunity_cost
 *   human_readable: Decision Fatigue Opportunity Cost
 *   domain: cognitive_economics/behavioral_systems
 *
 * SUMMARY:
 *   Decision fatigue represents a structural constraint operating at the
 *   intersection of cognitive psychology, institutional design, and economic
 *   extraction. The constraint emerges from the mismatch between
 *   exponentially growing choice complexity in modern environments (digital
 *   platforms, healthcare systems, financial products, organizational
 *   hierarchies) and the bounded cognitive capacity of individual
 *   decision-makers. This mismatch creates both a genuine coordination
 *   problem (how do we organize complex choice environments so
 *   decision-making remains feasible?) and an extraction opportunity (those
 *   who architect choice sets can influence behavior toward their interests
 *   by exploiting fatigue-induced heuristics and defaults). The constraint
 *   exhibits all six DR types from different perspectives, making it a
 *   diagnostic case for cognitive capture and institutional theater. The
 *   theater ratio (0.45 currently, rising to 0.72 by measurement endpoint)
 *   reflects increasing invocation of 'cognitive limits' as organizational
 *   justification for overhead and complexity that may not serve
 *   decision-maker interests. The extractiveness trajectory (0.35 → 0.58)
 *   shows accumulation of extraction through layered complexity in digital
 *   platforms and financial products designed to exploit decision fatigue.
 *
 * KEY AGENTS:
 *   - Decision Maker (Individual): Primary victim (powerless/trapped) — faces exponential growth in choice complexity without proportional increase in cognitive capacity; bears full cost as autonomy degrades
 *   - Choice Architects (Platform/UX/Policy Designers): Primary beneficiary (institutional/arbitrage) — solve genuine coordination problem but also profit from predictable behavioral biases induced by fatigue; can exit markets if less complex competitors emerge
 *   - Conscious Consumer/Worker: Secondary victim (moderate/constrained) — aware of choice architecture effects but constrained by resource costs of optimizing decisions or switching platforms
 *   - Attention Extractors (Tech Platforms, Financial Services): Secondary beneficiary (institutional/arbitrage) — exploit fatigue-induced defaults and simplified heuristics to capture attention and revenue
 *   - Regulatory Coalition (Consumer Protection, Data Privacy, Accessibility Standards): Organized reformers (organized/constrained) — building regulatory alternatives to choice simplification and transparency mandates; sunset logic applies as regulation matures
 *   - Cognitive Science Establishment: Institutional consensus-holder (institutional/arbitrage) — maintains theoretical framework explaining cognitive fatigue; benefits from invocations of cognitive limits to justify institutional complexity and overhead
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable cognitive laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decision_fatigue_opportunity_cost, 0.58).
domain_priors:suppression_score(decision_fatigue_opportunity_cost, 0.62).
domain_priors:theater_ratio(decision_fatigue_opportunity_cost, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decision_fatigue_opportunity_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(decision_fatigue_opportunity_cost, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(decision_fatigue_opportunity_cost, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decision_fatigue_opportunity_cost, tangled_rope).
narrative_ontology:human_readable(decision_fatigue_opportunity_cost, "Decision Fatigue Opportunity Cost").
narrative_ontology:topic_domain(decision_fatigue_opportunity_cost, "cognitive_economics/behavioral_systems").

domain_priors:requires_active_enforcement(decision_fatigue_opportunity_cost).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decision_fatigue_opportunity_cost, choice_architects).
narrative_ontology:constraint_beneficiary(decision_fatigue_opportunity_cost, attention_extractors).
narrative_ontology:constraint_victim(decision_fatigue_opportunity_cost, decision_makers).
narrative_ontology:constraint_victim(decision_fatigue_opportunity_cost, rational_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OVERWHELMED DECISION MAKER (SNARE) — Individual agents face exponential growth in choice complexity without proportional increase in cognitive capacity. Trapped by biological limits and environmental demands; bears full extraction cost as decision quality degrades and autonomy collapses. No viable exit option within the constraint.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSCIOUS CONSUMER/WORKER (TANGLED ROPE) — Experiences both coordination benefit (genuine need for choice frameworks to navigate complex markets) and extraction (architecture of choice sets influences behavior toward extractors' interests). Can exit at cost (reduced consumption, suboptimal career choices); experiences meaningful constraint but not total capture.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHOICE ARCHITECT (ROPE) — Platform designers, user interface engineers, policy designers experience the constraint as pure coordination: solving the genuine problem of organizing complex choice environments. Net beneficiary; can exit if capacity shifts (strategic retreat from markets). Interprets constraint as benign design problem, not extraction mechanism.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Regulatory agents (consumer protection bodies, data privacy regulators, accessibility standards) see decision fatigue as a temporary coordination failure being solved through mandatory simplification frameworks: choice limits, complexity reduction mandates, attention-cost transparency. Sunset logic applies: as regulation matures, default architecture improves, and decision fatigue naturally declines.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COGNITIVE SCIENCE CONSENSUS (PITON) — The psychological research documenting decision fatigue (ego depletion, choice overload, attention scarcity) persists as institutional consensus and organizational justification (meeting culture, process overhead, complexity budgets) despite contested empirical status. Theater ratio reflects performative invocations of 'cognitive limits' to justify inefficiency; the underlying constraint may be weaker than institutional narratives suggest. Degraded from functional description to theater.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, human cognitive capacity is bounded: any sufficiently complex environment will produce fatigue, and the gap between environmental complexity and individual processing capacity is structurally inevitable. This perspective risks naturalizing what may be a contingent institutional arrangement — exponential choice proliferation and attention capture are not laws of nature but design decisions. The engine will likely flag this as a false summit revealing naturalization of contingent extraction.
constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decision_fatigue_opportunity_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decision_fatigue_opportunity_cost, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decision_fatigue_opportunity_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decision_fatigue_opportunity_cost, TR),
    TR >= 0.70.

:- end_tests(decision_fatigue_opportunity_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine extraction: choice architects benefit from fatigue-induced defaults and simplified heuristics that steer behavior toward their interests. However, extraction is not maximal because legitimate coordination problems (organizing complex choice environments) create genuine benefits that partially offset extraction cost. The trajectory from 0.35 to 0.58 reflects accumulation of extractive design layers in digital platforms and financial products. Suppression (0.62): Moderate-high. Significant barriers include cognitive limits (real, though contested), information asymmetries (intentional choice architecture), and internalized acceptance of 'cognitive overload' as inevitable. Suppression is not total because regulatory frameworks and simplified interface design are showing that complexity can be managed, suggesting barriers are partially contingent rather than immutable. Theater ratio (0.45 rising to 0.72): Rising. Early invocations of 'cognitive limits' reflected genuine psychological research; current invocations increasingly serve as organizational justification for overhead and complexity. The theater ratio rise indicates performative use of fatigue narratives to justify institutional designs that may not serve decision-maker interests. The constraint is tangled_rope not snare because legitimate coordination problems (how to organize complex choice sets) exist alongside extraction, and the institutional beneficiaries (choice architects) do solve real problems while capturing surplus.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the choice architect's rope (genuine coordination) and the powerless agent's snare (exploitation via fatigue) reveals that the constraint has dual properties. From the beneficiary's view, they are solving a real problem (how to present complex information). From the victim's view, they are being steered by exploitation of predictable cognitive limitations. Both perspectives are structurally accurate — the constraint coordinates information presentation while extracting through fatigue-induced defaults. This is the defining feature of tangled_rope: simultaneous presence of genuine coordination function and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by structural position. Choice architects with arbitrage exit options (can move to less complex markets) experience low d → negative effective extraction. Decision-makers with trapped status experience high d → high effective extraction. Conscious consumers with constrained exit (can exit at cost but not freely) experience moderate d. Regulatory coalition with organized power and constrained exit experiences moderate d with declining pressure as scaffold sunset approaches. The key directionality signal: choice architects benefit from fatigue (d ≈ 0.10 for institutional beneficiary), but decision-makers bear costs (d ≈ 0.95 for powerless victim). This asymmetry (factor of 10) is the signature of tangled_rope, not pure rope or pure snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination from extraction: The coordination function (organizing complex choice environments) is real and benefits all agents. The extraction function (exploiting fatigue-induced defaults) is real and benefits only architects. The constraint is not 'is this coordination or extraction?' but 'how much of the measured suppression is coordination overhead versus extractive exploitation?' The theater ratio rising from 0.25 to 0.45 indicates increasing performative invocation of 'cognitive limits' to justify complexity that may serve architects more than decision-makers. The regulatory coalition's scaffold perspective identifies a genuine sunset path: simplification mandates, complexity limits, and transparency requirements reduce extraction while preserving coordination function. The mountain perspective (naturalizing cognitive limits) is a false summit revealed by the structural data: extractiveness is driven by choice architecture design decisions, not immutable cognitive laws. If cognitive limits were truly immutable, simplification mandates and regulatory interventions would be ineffective — but evidence suggests they work, indicating that much extraction is contingent on architectural choices rather than inherent to cognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ego_depletion_replication_crisis,
    'Is decision fatigue (ego depletion) a robust psychological phenomenon or a statistical artifact from underpowered studies and publication bias?',
    'Pre-registered replication studies with large sample sizes and registered protocols; meta-analysis of effect sizes across high-quality studies; separation of foundational ego depletion claims from downstream applications',
    'If robust: decision fatigue is a real structural constraint (extractiveness remains 0.55+). If artifact: much of the perceived extraction is theater, and extractiveness should be downgraded to 0.25-0.35 (regulatory scaffold + coordinated simplification is sufficient).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ego_depletion_replication_crisis, empirical, 'Whether ego depletion is a robust phenomenon or statistical artifact').

omega_variable(
    choice_overload_threshold_identification,
    'What is the actual cognitive load threshold at which choice overload becomes extractive rather than simply complex? Does it vary by domain, agent sophistication, or choice architecture?',
    'Controlled studies varying choice complexity while holding agent expertise constant; longitudinal tracking of decision quality as complexity increases across domains (medical, financial, consumer, organizational)',
    'If threshold is low (>7-10 choices): most modern environments exceed it, and extractiveness is high (0.60+). If threshold is high (>50-100 choices): extraction is concentrated in specific domains, and extractiveness is lower (0.35-0.45).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(choice_overload_threshold_identification, empirical, 'Identification of actual cognitive load threshold for decision overload').

omega_variable(
    intentional_choice_architecture_extraction,
    'To what degree is decision fatigue deliberately engineered by choice architects (dark patterns, hidden complexity, attention extraction) versus incidental to legitimate system complexity?',
    'Forensic analysis of user interface design decisions; A/B testing of simplified vs. complex choice environments; documentation of design intent in product teams (if accessible); comparison of internal complexity in user-facing vs. internal tools',
    'If primarily intentional: extractiveness should increase to 0.65-0.75 (snare dynamics). If primarily incidental: extractiveness should decrease to 0.40-0.50 (rope with asymmetric side effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentional_choice_architecture_extraction, empirical, 'Degree of intentional design extraction in choice architecture').

omega_variable(
    cognitive_capacity_expansion_feasibility,
    'Can decision fatigue be genuinely mitigated through education, cognitive training, tool-mediated decision support, or regulatory simplification, or does it reflect a hard ceiling on human processing?',
    'Longitudinal intervention studies: cognitive training programs, decision support tool adoption, regulatory simplification in test markets; measurement of decision quality and fatigue resistance post-intervention',
    'If capacity is expandable/improvable: scaffold sunset is real, and the constraint is temporary. If capacity is fixed: constraint is mountain-like and extractiveness is intrinsic. If capacity expands but architects add complexity faster: arms race dynamics, and extractiveness increases over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_expansion_feasibility, empirical, 'Whether cognitive capacity for decision-making can be expanded or is fixed').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (actual cognitive limits, information access barriers) or internalized (agents have accepted cognitive limits as inevitable and stopped seeking alternatives)?',
    'Post-intervention suppression trajectory: if regulatory simplification or cognitive training reduces complexity, do agents re-engage with decision-making or remain disengaged? If disengagement persists after barriers removed, suppression is partially internalized.',
    'If structural: reducing complexity directly improves decision autonomy. If internalized: agents continue delegating decisions even when capacity becomes available; constraint persists through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized cognitive acceptance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decision_fatigue_opportunity_cost, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dfoc_tr_t0, decision_fatigue_opportunity_cost, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dfoc_tr_t5, decision_fatigue_opportunity_cost, theater_ratio, 5, 0.35).
narrative_ontology:measurement(dfoc_tr_t10, decision_fatigue_opportunity_cost, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(dfoc_be_t0, decision_fatigue_opportunity_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dfoc_be_t5, decision_fatigue_opportunity_cost, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dfoc_be_t10, decision_fatigue_opportunity_cost, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decision_fatigue_opportunity_cost, resource_allocation).
narrative_ontology:affects_constraint(decision_fatigue_opportunity_cost, attention_extraction_asymmetry).
narrative_ontology:affects_constraint(decision_fatigue_opportunity_cost, default_option_behavioral_capture).
narrative_ontology:affects_constraint(decision_fatigue_opportunity_cost, information_complexity_barrier).

% DUAL FORMULATION NOTE:
% Decision fatigue operates as a constraint family with three structurally distinct mechanisms: (1) resource_allocation coordination (genuine problem of organizing choice complexity), (2) behavioral_capture via fatigue-induced defaults (extraction mechanism), (3) internalized cognitive acceptance of limits (suppression mechanism). Each mechanism has its own ε value and should be decomposed into separate constraint stories if empirical measurement requires independent observation. This story models the integrated phenomenon; downstream constraints model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decision_fatigue_opportunity_cost, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
