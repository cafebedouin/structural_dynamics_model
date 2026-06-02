% ============================================================================
% CONSTRAINT STORY: deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deterrence_unthinkable, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deterrence_unthinkable
 *   human_readable: Nuclear Deterrence as Unwinnable War (Deterrence-Unthinkable Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the 'deterrence_unthinkable' reading of the
 *   contested kernel 'war_winnability_post_1945'—the claim that nuclear
 *   weapons have made great-power total war categorically unwinnable,
 *   rendering war-planning incoherent as a strategic objective. The nuclear
 *   age introduced a structural inversion: military establishments were
 *   created to plan for victory in war, but nuclear deterrence made victory
 *   impossible to achieve or even conceive. The constraint functions as a
 *   tangled hybrid of coordination (deterrence prevents war) and extraction
 *   (military establishments maintain themselves through institutional
 *   incoherence; civilian populations are suppressed by the threat of
 *   annihilation; war-planning doctrine becomes theater). The
 *   deterrence_unthinkable reading claims that the operative constraint is
 *   the logical foreclosure of victory, which shifts strategic planning from
 *   'how to win' to 'how to prevent war from starting.' This is distinct from
 *   the sibling reading countervailing_thinkable, which asserts that limited
 *   nuclear war is winnable through escalation dominance and counterforce
 *   strategy. The readings do not coexist peacefully in institutional
 *   practice—they represent competing operational doctrines (Flexible
 *   Response vs Mutually Assured Destruction, for instance) held by different
 *   military factions across the decades. The deterrence_unthinkable reading
 *   has dominated declaratory policy since the 1960s, though countervailing
 *   logic persists in operational planning. The constraint exhibits high
 *   theater ratio (0.51) because much of nuclear strategy—war games, doctrine
 *   revision, force posturing, strategic command exercises—is ritualistic
 *   maintenance of a system whose core function is self-negation. The
 *   extractiveness value (0.62) reflects the sustained military institutional
 *   presence despite strategic incoherence, the suppression of civilian
 *   populations through perpetual threat, and the coordination benefits for
 *   preventing great-power war.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victim (powerless/trapped) — depend for survival on rationality of nuclear-armed adversaries; bear full cost of deterrence failure; cannot exit
 *   - Military Planning Establishments: Secondary victim (moderate/trapped) — trapped in institutional incoherence; mission to prepare for victory becomes logically impossible; maintain themselves through theater
 *   - Nuclear-Armed State Governments: Beneficiary and victim (institutional/constrained) — benefit from deterrence preventing war but constrained by logic that prevents using weapons; cannot exit without jeopardizing deterrence
 *   - Allied States in Extended Deterrence: Mixed (organized/constrained) — benefit from security coordination but constrained by dependence on nuclear umbrella; cannot achieve strategic autonomy without rejecting deterrence
 *   - Strategic Doctrine System: Institutional maintainer (institutional/arbitrage) — extracts authority through perpetual maintenance of system despite acknowledged incoherence; sees itself as degraded (piton observation)
 *   - Analytical Observer: Sees coordination function (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable consequence of technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deterrence_unthinkable, 0.62).
domain_priors:suppression_score(deterrence_unthinkable, 0.68).
domain_priors:theater_ratio(deterrence_unthinkable, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deterrence_unthinkable, extractiveness, 0.62).
narrative_ontology:constraint_metric(deterrence_unthinkable, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(deterrence_unthinkable, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(deterrence_unthinkable, "Nuclear Deterrence as Unwinnable War (Deterrence-Unthinkable Reading)").
narrative_ontology:topic_domain(deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deterrence_unthinkable, '2bff7d2c-8800-4518-9e18-5603f8c83b1a').
narrative_ontology:cs_created_at('2bff7d2c-8800-4518-9e18-5603f8c83b1a', '').
narrative_ontology:cs_kernel_codification('2bff7d2c-8800-4518-9e18-5603f8c83b1a', distributed).
narrative_ontology:cs_authority_grounding('2bff7d2c-8800-4518-9e18-5603f8c83b1a', extraction).
narrative_ontology:cs_kernel_id(deterrence_unthinkable, war_winnability_post_1945).
narrative_ontology:cs_reading_relation('2bff7d2c-8800-4518-9e18-5603f8c83b1a', countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('2bff7d2c-8800-4518-9e18-5603f8c83b1a', rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('2bff7d2c-8800-4518-9e18-5603f8c83b1a', foundational, nuclear_exchange_unwinnable).
narrative_ontology:cs_axiom_status(nuclear_exchange_unwinnable, holdable).
narrative_ontology:cs_axiom('2bff7d2c-8800-4518-9e18-5603f8c83b1a', foundational, mutual_vulnerability_prevents_total_war).
narrative_ontology:cs_axiom_status(mutual_vulnerability_prevents_total_war, holdable).
narrative_ontology:cs_reference_frame('2bff7d2c-8800-4518-9e18-5603f8c83b1a', deterrence_logic_as_primary).
narrative_ontology:cs_drift_state('2bff7d2c-8800-4518-9e18-5603f8c83b1a', contemporary_doctrine_contestation, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(deterrence_unthinkable, non_aligned_states).
narrative_ontology:constraint_victim(deterrence_unthinkable, military_planning_establishments).
narrative_ontology:constraint_victim(deterrence_unthinkable, nuclear_doctrine_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped in a system where their survival depends on the rationality of adversaries and the stability of a balance that cannot be tested without annihilation. No exit option. High suppression: the threat of nuclear retaliation is absolute and inescapable. Maximum experienced extraction: their fate is sealed by strategic calculations made by institutional actors whose incentives they cannot influence.
constraint_indexing:constraint_classification(deterrence_unthinkable, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED STATES (TANGLED ROPE) — Constrained by dependence on nuclear umbrella but also benefit from the coordination function: extended deterrence prevents major-power war and provides security framework. The constraint enforces alliance cohesion and prevents arms races among allies. Mixed: benefits from coordination (don't have to build their own nuclear arsenals) but constrained by inability to exit without jeopardizing security.
constraint_indexing:constraint_classification(deterrence_unthinkable, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR-ARMED STATE GOVERNMENTS (TANGLED ROPE) — Constrained by the logic of deterrence itself: they cannot use nuclear weapons without mutual annihilation (incoherent), but they cannot credibly disarm without jeopardizing deterrence. The constraint enforces strategic stability but also enforces massive military expenditures and perpetual readiness. Mixed: benefits from deterrence coordination (prevents war) but constrained by escalatory logic and cost.
constraint_indexing:constraint_classification(deterrence_unthinkable, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY PLANNING ESTABLISHMENTS (SNARE) — Trapped in incoherence: their institutional mission is to prepare for victory in war, but nuclear deterrence makes victory in total war impossible to achieve or even conceive. War-planning becomes pure theater—contingency exercises that cannot and must not be executed. High suppression: the institutional pressure to maintain readiness despite the incoherence of the mission. High extraction: the institutions extract continued funding and authority based on a threat they simultaneously render unthinkable.
constraint_indexing:constraint_classification(deterrence_unthinkable, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: NUCLEAR STRATEGIC DOCTRINE SYSTEM (PITON) — The elaborate apparatus of deterrence theory, strategic doctrine (MAD, flexible response, credible first strike), and operational planning has become substantially performative. Theater ratio high: much of the activity (war games, doctrine revision cycles, force posturing) is ritualistic maintenance of a system whose primary function is to prevent its own use. The doctrine system persists through institutional inertia and career structure, not because it solves the core incoherence. The system sees itself as degraded—actors within it acknowledge the unthinkability while preserving the machinery.
constraint_indexing:constraint_classification(deterrence_unthinkable, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the civilizational analytical view, nuclear deterrence is fundamentally a coordination mechanism solving the collective action problem of preventing total war. The constraint coordinates rational actors around the shared understanding that mutual vulnerability makes war unwinnable. This reading sees the constraint as pure coordination with minimal extraction overhead. However, this reading naturalizes and abstracts away the real suppression (threat of annihilation) and real extraction (military establishments maintain themselves through incoherence). The perspective risks false summit classification by treating deterrence as a natural law rather than a contested institutional arrangement.
constraint_indexing:constraint_classification(deterrence_unthinkable, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deterrence_unthinkable_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deterrence_unthinkable, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deterrence_unthinkable, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deterrence_unthinkable, TR),
    TR >= 0.70.

:- end_tests(deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high. The constraint exhibits real extraction mechanisms: (1) military establishments maintain institutional authority and resource flows despite strategic incoherence—they extract legitimacy from the threat of war they simultaneously make impossible; (2) civilian populations are suppressed by perpetual threat of annihilation—extraction through fear; (3) allied states are constrained by dependence on nuclear umbrella—extraction through strategic subordination. However, extractiveness is not extreme (would be 0.80+) because the coordination function is genuine—deterrence actually does prevent great-power war, which is a real collective benefit. The value reflects that coordination and extraction are genuinely intertwined, not separable. Suppression (0.68): High. The threat of nuclear retaliation is absolute, inescapable, and cannot be negotiated away within the deterrence framework. Civilian populations cannot exit. Military establishments cannot acknowledge the incoherence without jeopardizing their institutional role. States cannot disarm without jeopardizing deterrence. The suppression is structural—built into the logic of mutual vulnerability itself. Theater Ratio (0.51): Moderate-high. Nuclear strategy involves substantial theatrical elements: war games that cannot be executed, doctrine revisions that do not change operational logic, force posturing as communication, strategic command exercises as ritual maintenance. However, theater is not dominant (would be 0.70+ for Piton) because the deterrence function does genuinely require operational readiness and credible threat—some of the activity is functionally necessary, not purely performative. The value reflects the ambiguity between functional deterrent signaling and ritualistic compliance with institutional procedures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Civilian populations experience it as a Snare (trapped, maximum extraction through threat). Military establishments experience it as a Snare (trapped in institutional incoherence). Nuclear-armed states experience it as Tangled Rope (benefits from deterrence coordination, constrained by logic that prevents war use). Allied states experience it as Tangled Rope (security benefit, strategic dependence). The doctrine system itself experiences it as Piton (degraded ritual). The analytical observer risks seeing it as pure Rope (coordination mechanism preventing war) by abstracting away the suppression and incoherence. The perspectival gap reveals that the same structural arrangement is experienced as inescapable constraint by some actors and as successful coordination by others. The gap is not resolvable by choosing one perspective 'correct'—it reflects that the constraint genuinely has multiple structural effects, some beneficial (preventing war) and some extractive (institutional incoherence, civilian suppression), simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from agent power, exit options, and beneficiary/victim status. Civilian populations: powerless + trapped + victim → d=0.95 (maximum target), experience maximum extracted chi. Military establishments: moderate + trapped + victim (institutional coherence loss) → d=0.90 (target), experience high chi from institutional incoherence and budget extraction. Nuclear-armed states: institutional + constrained + mixed (beneficiary from deterrence, victim from constraint) → d=0.45 (moderate), experience moderate chi from balanced coordination and constraint. Allied states: organized + constrained + mixed → d=0.50 (balanced), experience moderate chi. Doctrine system: institutional + arbitrage + beneficiary (extracts through maintenance) → d=0.10 (low target), experience low chi but from theater rather than true extraction. Analytical observer: analytical + analytical → d=0.73 (standard), risks missing the extraction because the coordination function is real and visible. The directionality overrides are not needed—the derivation from structural position correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that Tangled Rope classification is correct and stable. The reading deterrence_unthinkable does NOT collapse into pure Rope (pure coordination) because military establishments are structurally extracted through the institutional incoherence that the constraint creates. It does NOT collapse into Snare (pure extraction) because the coordination function genuinely prevents war and genuinely benefits the protected populations (civilians and allied states), even though they experience it as suppressive. The hybrid classification reflects that the constraint simultaneously (1) coordinates rational actors around mutual vulnerability, preventing war, and (2) maintains military institutional authority through strategic doctrine that acknowledges its own incoherence. Both functions are real. Mandatrophy resolution: the constraint is correctly classified as Tangled Rope because it requires active enforcement (nuclear deterrence must be continuously signaled, doctrine must be maintained), it has genuine coordination benefits (prevents great-power war), and it has asymmetric extraction (military establishments benefit from the incoherence, civilian populations suffer from the suppression). The three kernel readings represent different ways of weighting the coordination vs extraction components: deterrence_unthinkable emphasizes the coordination (mutual vulnerability prevents war), countervailing_thinkable tries to restore extraction for one side (winning through escalation dominance), rhetorical_contraction reframes coordination as rhetorical rather than logical. But all three operate within the constraint's basic structure: nuclear weapons have made total war incoherent as a strategic objective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_assumption_stability,
    'Does the deterrence constraint depend on rational-actor assumptions that have alternative readings or failure modes?',
    'Historical analysis of near-miss incidents (Cuban Missile Crisis, Able Archer 83, K-19 incident, etc.) where rationality assumptions were tested; comparison of ex ante rationality models with actual decision-making process documentation released post-hoc.',
    'If rationality assumption is empirically brittle: the constraint is fundamentally unstable and high-suppression extraction mechanisms (fear-based) dominate. If rationality holds across incidents: the coordination function is genuine and the constraint approaches Rope. If rationality is context-dependent (works under some conditions, fails under others): the constraint is Tangled Rope with conditional stability—which this reading claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_assumption_stability, empirical, 'Stability of rationality assumptions underlying deterrence logic').

omega_variable(
    military_institutional_coherence_loss,
    'To what extent do military planning establishments genuinely internalize the incoherence of nuclear war planning versus treating it as routine institutional theater?',
    'Qualitative interviews with military strategists, analysis of strategic doctrine evolution, examination of service culture attitudes toward nuclear employment, comparison of resource allocation to genuinely executable vs purely deterrent capabilities.',
    'If institutions genuinely internalize incoherence: this reading''s Snare classification of military establishments holds; extraction through maintained institutional incoherence is real. If institutions treat it as theater: this reading''s Piton observation is correct—the system is degraded and theatrical. If mixed (some actors internalize, others theater): the constraint has fractured institutional legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_institutional_coherence_loss, empirical, 'Degree of genuine internalization of war-planning incoherence within military institutions').

omega_variable(
    kernel_reading_contest_mechanics,
    'How do the three kernel readings (deterrence_unthinkable, countervailing_thinkable, rhetorical_contraction) relate structurally? Does one foreclose the others within a single institutional framework?',
    'Documentary analysis of strategic doctrine evolution; examination of whether different readings are held by different institutional factions simultaneously or sequentially; analysis of whether adoption of one reading logically entails rejection of the others.',
    'If readings are genuinely simultaneous factions (coexist): nuclear strategy maintains internal contradiction as a feature. If readings are sequential (one replaced another): history shows which reading was overridden and why. If readings foreclose each other: institutions face an irreducible choice. This omega documents the kernel contest itself as an omega—the reading frame is itself a source of irreducible uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_mechanics, conceptual, 'Structural relationships between kernel readings on war winnability post-1945').

omega_variable(
    extended_deterrence_victim_reframing,
    'Are allied states under extended deterrence appropriately classified as victims or beneficiaries? Does dependence on nuclear umbrella constitute extraction or coordination benefit?',
    'Comparative analysis of allied state strategic autonomy before and after extended deterrence; examination of counterfactual: what would allied states choose if nuclear umbrella were costless vs if it required subordination; assessment of whether NATO/allied alliance structure would dissolve without nuclear deterrence.',
    'If allied states are genuine beneficiaries: the constraint is closer to Rope for them. If extended deterrence extracts subordination and strategic dependence: allied states are victims. If mixed (some benefits, some extraction): Tangled Rope holds and the perspective classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extended_deterrence_victim_reframing, empirical, 'Whether extended deterrence creates genuine security benefit or enforces strategic dependence').

omega_variable(
    civilization_constraint_vs_institutional_choice,
    'Is nuclear deterrence an immutable constraint of the post-1945 era (a Mountain in disguise) or a contested institutional reading of war winnability that could be dismantle or radically reframed?',
    'Analysis of whether deterrence emerged as inevitable response to nuclear weapons technology or as one choice among alternatives; examination of periods where deterrence logic was rejected or reframed (disarmament movements, strategic ambiguity periods); counterfactual: what institutional arrangements would prevent nuclear war without deterrence logic?',
    'If deterrence is immutable: the constraint approaches Mountain status despite current Tangled Rope classification—institutional arrangements are inescapable consequences of technology. If deterrence is chosen institutional reading: this reading''s claim that alternative framings (countervailing_thinkable, rhetorical_contraction) are live competitors is correct. If institutional choice but difficult to reverse: the constraint is path-dependent—operationally contingent but strategically trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilization_constraint_vs_institutional_choice, conceptual, 'Whether deterrence is immutable constraint or contingent institutional reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dete_tr_t0, deterrence_unthinkable, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dete_tr_t15, deterrence_unthinkable, theater_ratio, 15, 0.45).
narrative_ontology:measurement(dete_tr_t30, deterrence_unthinkable, theater_ratio, 30, 0.51).
narrative_ontology:measurement(dete_tr_t45, deterrence_unthinkable, theater_ratio, 45, 0.54).

% Extraction over time
narrative_ontology:measurement(dete_be_t0, deterrence_unthinkable, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(dete_be_t15, deterrence_unthinkable, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(dete_be_t30, deterrence_unthinkable, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(dete_be_t45, deterrence_unthinkable, base_extractiveness, 45, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(deterrence_unthinkable, countervailing_thinkable).
narrative_ontology:affects_constraint(deterrence_unthinkable, rhetorical_contraction).
narrative_ontology:affects_constraint(deterrence_unthinkable, extended_deterrence_stability).
narrative_ontology:affects_constraint(deterrence_unthinkable, arms_race_prevention_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_winnability_post_1945 kernel. The sibling readings (countervailing_thinkable, rhetorical_contraction) are separate constraint stories with different ε values and perspectives. They should not be folded into this story. Instead, they are linked via network.affects_constraints as competing institutional readings of the same underlying disagreement about whether nuclear war has a winnable state. Each reading is a complete, ε-invariant constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
