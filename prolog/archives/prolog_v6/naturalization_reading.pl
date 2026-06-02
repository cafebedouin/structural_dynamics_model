% ============================================================================
% CONSTRAINT STORY: naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naturalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naturalization_reading
 *   human_readable: QWERTY Persistence via Genuine Adequacy (Naturalization Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout has persisted as the global standard for over
 *   150 years despite periodic challenges from alternatives, most notably the
 *   Dvorak Simplified Keyboard. This persistence has been interpreted in two
 *   radically different ways: (1) as a natural equilibrium outcome in which
 *   QWERTY genuinely is adequate or superior, such that alternatives fairly
 *   lapsed through competition; or (2) as an artificial lock-in mechanism
 *   created by early adoption accidents and network effects, in which users
 *   are trapped by switching costs and manufacturers maintain the standard
 *   through coordination failure. This constraint story instantiates the
 *   NATURALIZATION READING: QWERTY persists because it was and became
 *   genuinely adequate for the keyboard task. No systematic beneficiary
 *   exploits the standard; switching costs reflect fair learning investments;
 *   alternatives were not suppressed but rather offered insufficient
 *   advantage to overcome coordination demand. The constraint is a mountain
 *   from the civilizational analytical perspective (natural law of keyboard
 *   equilibria) and pure rope from the coordinating parties' perspectives
 *   (universal standard solves genuine problem). The kernel is contested —
 *   the lock-in reading claims QWERTY is an arbitrary lock-in, while the
 *   beneficiary extraction reading claims QWERTY benefits manufacturers by
 *   preventing profitable alternatives. This reading occupies a middle
 *   ground: the constraint is real and persistent, but for reasons of genuine
 *   coordination and market equilibrium, not artificial extraction.
 *
 * KEY AGENTS:
 *   - Typists: Massive distributed population with accumulated human capital in QWERTY skill (moderate/constrained) — faces real switching costs reflecting their own past investment
 *   - Keyboard Manufacturers: Institutional actors coordinating on a universal standard (institutional/arbitrage) — benefit from coordination but do not extract from typists
 *   - Software Ecosystem (OS developers, application designers): Institutional actors building around QWERTY assumptions (institutional/arbitrage) — genuine coordination benefit from standardization
 *   - Alternative Layout Communities: Organized niche adopters (organized/mobile) — successfully use Dvorak and other layouts in local coordination contexts, demonstrating that lock-in is not absolute
 *   - Typesetting and Early Typing Industry: Historical actors who originally selected among competing layouts (institutional/arbitrage) — face historical contingency, but their choice reflected genuine coordination problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naturalization_reading, 0.18).
domain_priors:suppression_score(naturalization_reading, 0.22).
domain_priors:theater_ratio(naturalization_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naturalization_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(naturalization_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(naturalization_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naturalization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(naturalization_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naturalization_reading, rope).
narrative_ontology:human_readable(naturalization_reading, "QWERTY Persistence via Genuine Adequacy (Naturalization Reading)").
narrative_ontology:topic_domain(naturalization_reading, "economic_history/technology_studies/path_dependence").

domain_priors:emerges_naturally(naturalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naturalization_reading, 'd4a03781-bb03-4f44-8e8b-95844eba96d1').
narrative_ontology:cs_created_at('d4a03781-bb03-4f44-8e8b-95844eba96d1', '').
narrative_ontology:cs_kernel_codification('d4a03781-bb03-4f44-8e8b-95844eba96d1', distributed).
narrative_ontology:cs_authority_grounding('d4a03781-bb03-4f44-8e8b-95844eba96d1', expertise).
narrative_ontology:cs_kernel_id(naturalization_reading, qwerty_persistence_mechanism).
narrative_ontology:cs_reading_relation('d4a03781-bb03-4f44-8e8b-95844eba96d1', lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4a03781-bb03-4f44-8e8b-95844eba96d1', beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('d4a03781-bb03-4f44-8e8b-95844eba96d1', foundational, dvorak_performance_negligible).
narrative_ontology:cs_axiom_status(dvorak_performance_negligible, holdable).
narrative_ontology:cs_axiom_grounding('d4a03781-bb03-4f44-8e8b-95844eba96d1', dvorak_performance_negligible, empirically_contingent).
narrative_ontology:cs_axiom('d4a03781-bb03-4f44-8e8b-95844eba96d1', foundational, switching_costs_reflect_rational_investment).
narrative_ontology:cs_axiom_status(switching_costs_reflect_rational_investment, holdable).
narrative_ontology:cs_axiom_grounding('d4a03781-bb03-4f44-8e8b-95844eba96d1', switching_costs_reflect_rational_investment, conventional).
narrative_ontology:cs_reference_frame('d4a03781-bb03-4f44-8e8b-95844eba96d1', market_equilibrium_through_fair_competition).
narrative_ontology:cs_drift_state('d4a03781-bb03-4f44-8e8b-95844eba96d1', contemporary, gap(stable, minor, true)).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NATURALIZATION / MARKET SELECTION (MOUNTAIN) — QWERTY persists because it is genuinely adequate for the keyboard task. The constraint emerges naturally from the interaction of typing speed, learning costs, and coordination among users. No systematic beneficiary exploits the standard; alternatives lapsed because they offered insufficient advantage to justify retraining. The persistence is an equilibrium outcome of fair competition, not artificial lock-in. From this civilizational/analytical perspective, QWERTY is a natural law of keyboard equilibria.
constraint_indexing:constraint_classification(naturalization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: MANUFACTURERS & TYPISTS (ROPE) — Both parties benefit from a universal standard. Manufacturers produce compatible keyboards; typists' skills transfer across devices. The constraint solves a genuine coordination problem: if everyone learned a different layout, keyboards would be less useful. QWERTY is the solution that emerged. Switching costs are real but reflect legitimate skill investment, not artificial lock-in. Experienced extraction is low — the standard is the coordination benefit, not a tax on it.
constraint_indexing:constraint_classification(naturalization_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL TYPIST (TANGLED ROPE) — A typist who has invested years in QWERTY skill faces real switching costs to Dvorak (retraining time, professional risk if work requires QWERTY, muscle memory investment). But these costs reflect the typist's own past coordination choice, not extraction by manufacturers. The constraint coordinates the typist's learning with the installed base. There is a modest asymmetry: the typist cannot easily exit without cost, but the cost is symmetrically borne by anyone who switches, not concentrated on a victim class. This reading classifies as Tangled Rope because the coordination function (skill standardization) coexists with switching costs, but the switching costs are not extractive in nature — they are the natural friction of retraining.
constraint_indexing:constraint_classification(naturalization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRAMMING COMMUNITIES (SCAFFOLD) — Programmers and specialized communities have successfully adopted alternative layouts (Dvorak, Colemak) in niche contexts where the coordination problem is solved locally (within a team or domain). This demonstrates that QWERTY's persistence is conditional on coordination demand at scale, not absolute lock-in. The scaffold reading shows that alternatives can exist and function when the coordination requirement is reduced — QWERTY dominates at the mass-market scale, not universally. The sunset is implicit: if a significant coordination shift occurred (e.g., voice input replacing keyboards), QWERTY's necessity vanishes.
constraint_indexing:constraint_classification(naturalization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: OEM HARDWARE STANDARD (PITON) — For manufacturers, QWERTY is a vestigial institutional standard maintained through inertia in the supply chain. From the OEM perspective, manufacturing QWERTY keyboards is the inherited default, not a functional necessity — software remapping could produce any layout. But OEMs don't actively promote alternatives because the installed base coordination is real. The piton classification reflects that QWERTY persists partly through institutional inertia (high theater ratio in OEM production decisions) even though the underlying coordination demand is genuine.
constraint_indexing:constraint_classification(naturalization_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SOFTWARE ECOSYSTEM (ROPE) — Operating systems and applications coordinate around QWERTY assumptions (keyboard mapping, shortcut keys). This coordination function is genuine — applications designed for QWERTY shortcuts are more usable for the majority of typists. The constraint is a pure coordination mechanism from this institutional perspective: software developers benefit from knowing the user base is trained on QWERTY; users benefit from applications designed with that assumption.
constraint_indexing:constraint_classification(naturalization_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naturalization_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(naturalization_reading, TR),
    TR >= 0.70.

:- end_tests(naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Under the naturalization reading, QWERTY does not systematically extract from any party. Typists face switching costs, but these reflect their own investment in skill, not a tax imposed by beneficiaries. Manufacturers benefit from standardization, but standardization is a public good they all share, not an extractive mechanism concentrated on victims. Suppression (0.22): Low-moderate. Alternatives to QWERTY are not actively suppressed by coercive mechanisms — software allows remapping, Dvorak keyboards exist, open-source operating systems support alternative layouts. The 'suppression' is coordination inertia: everyone has learned QWERTY, so switching is individually costly even if collectively beneficial. This is friction, not coercion. Theater ratio (0.35): Low-moderate. The institutional maintenance of QWERTY involves some performative elements (OEM standard defaults, keyboard labeling), but the core coordination function is genuine. The theater reflects path dependence, not manufactured justification. This reading claims QWERTY is a mountain at the civilizational level but a rope from coordinating parties' perspectives — both perspectives reflect the structural reality that the constraint solves a genuine problem without systematic extraction.
 *
 * PERSPECTIVAL GAP:
 *   The naturalization reading produces a perspectival gap primarily along time and scope axes rather than power/exit axes. The individual typist (biographical/constrained) sees moderate switching costs. The software developer (institutional/arbitrage) sees genuine coordination benefit. The civilization-scale analyst (civilizational/analytical) sees an emergent equilibrium without central extraction. All three perspectives are compatible with the claim that QWERTY is adequately competitive — they differ in how they weigh the switching cost friction against the coordination benefit. In contrast, the lock-in reading produces a fundamental disagreement about whether switching costs represent fair investment or artificial trapping. The naturalization reading classifies the typist's switching cost as fair tangled rope (modest asymmetry reflecting past choice); the lock-in reading classifies it as snare (entrapment by network effects). The empirical resolution of the Dvorak performance question may resolve which perspectival gap is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares no beneficiaries or victims because it holds that QWERTY is a symmetric coordination mechanism. All parties (manufacturers, typists, software developers) benefit from the universal standard; all bear fair coordination costs. The absence of beneficiary/victim asymmetry distinguishes this reading from the lock-in and beneficiary extraction readings. The directionality framework applies, but under this reading it produces d ≈ 0.5 for all parties (symmetric relationship to the constraint) when beneficiary/victim structure is derived. The analytical observer's d is elevated (0.72) because they observe the full structure, but they observe genuine coordination, not extraction. The naturalization reading's claim is that the perspectival gap between parties reflects differences in time horizon and spatial scope, not differences in exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves potential mandatrophy by claiming that QWERTY is a legitimate mountain (natural equilibrium) from the civilizational/analytical perspective AND a pure rope (symmetric coordination) from the institutional perspective. There is no contradiction — both are true from different observational contexts. The individual typist perspective is tangled rope (modest asymmetry from switching costs), but the asymmetry is fair equilibrium, not extraction. The constraint avoids mandatrophy because the typist's switching costs are presented as rational investments in human capital, not as extractive coercion. The reading's coherence depends on the empirical claim that Dvorak performance advantage is negligible or contested — if Dvorak were definitively superior, the switching cost asymmetry would become extractive, and the constraint would reclassify as snare in the lock-in reading. The naturalization reading holds that empirical evidence does not support this reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_performance_empirical_claim,
    'Does Dvorak layout actually provide measurable typing speed or ergonomic advantage over QWERTY in controlled studies?',
    'Meta-analysis of typing speed studies; ergonomic measurements (wrist strain, finger movement distance); blinded typing tests with experienced typists',
    'If Dvorak advantage is significant: supports lock-in reading (network effects prevent efficient alternative from spreading). If negligible or contested: supports naturalization reading (QWERTY''s persistence reflects genuine adequacy). If advantage exists but insufficient to overcome switching costs: supports tangled rope reading (asymmetric but rational outcome).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dvorak_performance_empirical_claim, empirical, 'Empirical performance comparison between Dvorak and QWERTY layouts').

omega_variable(
    learning_cost_internalization,
    'Do typists internalize switching costs as personal investment or as an extracted lock-in cost?',
    'Survey and interview data distinguishing between ''I invested years learning this'' vs ''I am forced to stay because alternatives are suppressed''; analysis of typist willingness-to-pay for alternatives; actual adoption rates when alternatives are actively promoted',
    'If internalized as investment: naturalization reading (typists rationally chose to learn QWERTY). If experienced as extraction: lock-in reading (typists are trapped by suppressed alternatives). The framing is observer-dependent but has structural consequences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(learning_cost_internalization, conceptual, 'Whether switching costs are experienced as rational investment or extractive lock-in').

omega_variable(
    counter_factual_competitive_scenario,
    'If QWERTY had never been adopted, would an alternative layout now dominate? Would QWERTY re-emerge if all layouts started equal?',
    'Historical analysis of competing layouts in early typing era (1870s-1920s); simulation models of adoption dynamics; experimental comparison of learning curves for naive users',
    'If QWERTY would re-emerge from equal competition: naturalization reading confirmed (QWERTY is genuinely superior). If alternatives would dominate: lock-in reading confirmed (early adoption accident is irreversible). If outcomes are path-dependent but QWERTY is no worse: tangled rope reading (outcome is fair equilibrium, not extractive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_factual_competitive_scenario, empirical, 'Counterfactual competitive scenario for keyboard layout adoption').

omega_variable(
    kernel_committer_ambiguity,
    'Is QWERTY''s persistence a natural law of keyboard equilibria, or a contingent institutional arrangement that benefits from network effects?',
    'This is the core contestation between readings. Resolution depends on empirical resolution of the Dvorak performance claim and learning cost mechanisms. The readings coexist as live positions because the empirical evidence is genuinely contested.',
    'If QWERTY is naturally adequate: mountain classification (this reading). If network effects are artificial lock-in: snare classification (lock-in reading). If mixed: tangled rope with fair equilibrium (this reading) vs tangled rope with extraction (lock-in reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity, conceptual, 'Fundamental disagreement between naturalization and lock-in readings on the mechanism of QWERTY persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naturalization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(natu_tr_t0, naturalization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(natu_tr_t5, naturalization_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(natu_tr_t10, naturalization_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(natu_be_t0, naturalization_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(natu_be_t5, naturalization_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(natu_be_t10, naturalization_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(naturalization_reading, 0.15).
narrative_ontology:affects_constraint(naturalization_reading, lock_in_reading).
narrative_ontology:affects_constraint(naturalization_reading, beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% QWERTY_PERSISTENCE_MECHANISM kernel decomposes into three structurally distinct readings (constraints): naturalization_reading (ε=0.18, market equilibrium outcome); lock_in_reading (ε=0.55, network effects entrap alternatives); beneficiary_extraction_reading (ε=0.72, manufacturers suppress alternatives). Each reading instantiates a different ε value because each interprets the causal mechanism differently. Under naturalization, switching costs reflect fair investment and modest coordination friction (low ε). Under lock-in, switching costs are artificial barriers created by network effects (high ε). Under beneficiary extraction, switching costs are deliberately maintained by manufacturer coordination (highest ε). The three readings coexist as live positions because the empirical evidence on Dvorak performance and manufacturer coordination is genuinely contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
