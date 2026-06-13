% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Nuclear Countervailing Winnability Doctrine
 *   domain: strategic/military/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the countervailing reading of the contested
 *   kernel 'war_winnability_post_1945': the strategic doctrine that nuclear
 *   weapons constrain but do not eliminate winnability, and that limited
 *   victory remains achievable through precise counterforce targeting of
 *   military assets while avoiding city destruction. The constraint is both a
 *   coordination mechanism (solves the credibility problem of deterrence) and
 *   extractive (sustains military-industrial interests, blocks arms control,
 *   places civilian populations at risk). Strategic command administers the
 *   doctrine; military-industrial institutions benefit from it; arms control
 *   regimes and civilian populations bear its costs. The theater_ratio
 *   trajectory (0.35 to 0.58) reflects the rising proportion of doctrine
 *   maintenance that is purely performative — winnability remains officially
 *   thinkable but increasingly challenged by game-theoretic analysis and
 *   historical evidence, making the constraint rely more on institutional
 *   gatekeeping and career incentives than on strategic coherence.
 *
 * KEY AGENTS:
 *   - Nuclear strategic command: Agenda-setter administering countervailing doctrine. Identity-locked to winnability assumptions; doctrine shapes career paths, institutional mission, and force-structure justification.
 *   - Military-industrial complex: Beneficiary capturing sustained procurement and research contracts. Benefits from the constraint's persistence through warhead modernization, precision guidance development, and advanced delivery systems.
 *   - Arms control regimes: Victim whose negotiating leverage is undermined by countervailing planning that assumes continued arsenals and rejects disarmament.
 *   - Civilian populations in targeting zones: Victims bearing the risk of collateral damage from counterforce strategies; powerless and trapped geographically.
 *   - Disarmament advocates: Excluded from doctrine-setting but present in public discourse; argue winnability is incoherent and serves institutional rather than security interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Nuclear Countervailing Winnability Doctrine").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic/military/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '5062d0a0-b7e9-498e-a6e1-e12554300720').
narrative_ontology:cs_kernel_codification('5062d0a0-b7e9-498e-a6e1-e12554300720', distributed).
narrative_ontology:cs_authority_grounding('5062d0a0-b7e9-498e-a6e1-e12554300720', extraction).
narrative_ontology:cs_interpretation_layer_present('5062d0a0-b7e9-498e-a6e1-e12554300720').
narrative_ontology:cs_reading_relation('5062d0a0-b7e9-498e-a6e1-e12554300720', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('5062d0a0-b7e9-498e-a6e1-e12554300720', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('5062d0a0-b7e9-498e-a6e1-e12554300720', foundational, limited_nuclear_war_strategically_coherent).
narrative_ontology:cs_axiom_status(limited_nuclear_war_strategically_coherent, holdable).
narrative_ontology:cs_axiom_grounding('5062d0a0-b7e9-498e-a6e1-e12554300720', limited_nuclear_war_strategically_coherent, instrumental).
narrative_ontology:cs_axiom('5062d0a0-b7e9-498e-a6e1-e12554300720', foundational, counterforce_targeting_escalation_containable).
narrative_ontology:cs_axiom_status(counterforce_targeting_escalation_containable, holdable).
narrative_ontology:cs_axiom_grounding('5062d0a0-b7e9-498e-a6e1-e12554300720', counterforce_targeting_escalation_containable, empirically_contingent).
narrative_ontology:cs_reference_frame('5062d0a0-b7e9-498e-a6e1-e12554300720', rational_deterrence_through_winnability_credibility).
narrative_ontology:cs_drift_state('5062d0a0-b7e9-498e-a6e1-e12554300720', contemporary_game_theoretic_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5062d0a0-b7e9-498e-a6e1-e12554300720', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_command_institutions).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, nuclear_weapons_laboratories).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeting_zones).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, non_nuclear_adversaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_studies_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, allied_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, non_nuclear_states).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, allied_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plans, maintains, and theorizes counterforce targeting strategies that assume limited nuclear war remains winnable through precision strikes on military assets. Administers the operational doctrine that underpins force posture, training, and procurement. Justifies the doctrine as maintaining credible deterrence and protecting strategic stability through damage-limitation theory.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_strategic_command, agenda_setter,
    institutional, generational, identity_locked, global).

% Captures sustained procurement contracts, research funding, and technological development mandates grounded in the requirement to maintain counterforce capabilities. The winnability narrative justifies advanced warhead designs, guidance systems, and delivery platforms. Their material interests are direct and substantial.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Intellectual architects and maintainers of countervailing doctrine. Control peer-review processes, defense-funded research agendas, and think-tank positions that authorize the winnability framework. Benefit from career advancement, publication opportunities, and policy influence rooted in the doctrine's legitimacy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_studies_establishment, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, strategic_studies_establishment, agenda_setter).

% Undermined by strategic planning that assumes limited nuclear war is winnable. Counterforce doctrine justifies continued warhead modernization, rejected arms reduction treaties, and maintained arsenals that arms control seeks to constrain. Their negotiating position and legitimacy are eroded by the continuous deployment of winnability arguments.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Face countervailing targeting strategies aimed at their own military assets. Must respond with their own counterforce doctrines and force modernization to maintain deterrent credibility. The constraint compels expensive technological competition in precision and survivability even when both sides would prefer de-escalation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, rival_nuclear_powers, payer,
    powerful, generational, trapped, global).

% Located near military installations and strategic infrastructure targeted by counterforce strategies. Winnability doctrine assumes acceptable collateral damage and permits military-adjacent targeting that carries civilian risk. They have no voice in targeting doctrine and cannot exit the geographic constraint.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations_in_targeting_zones, payer,
    powerless, biographical, trapped, global).

% Face the persistence of nuclear arsenals justified by winnability narratives, which block disarmament momentum. Their deterrent vulnerability is sustained because nuclear powers maintain counterforce postures that assume great-power nuclear conflict remains militarily rational.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% Protected (in principle) by extended nuclear deterrence built on countervailing doctrine. They depend on the doctrine's credibility for security guarantees but also face the risk that winnability assumptions could lead to conflict escalation. Their benefit is conditional on the doctrine preventing rather than enabling nuclear use.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, allied_populations, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, allied_populations, beneficiary).

% Argue that winnability doctrine is incoherent and dangerous, that nuclear war cannot be won, and that the doctrine serves military-industrial interests rather than security. They are structurally excluded from strategic command decision-making and doctrine development, though their position influences public discourse and some policy circles.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, disarmament_advocates, excluded,
    moderate, generational, mobile, global).

% The analytical framework that underwrites countervailing doctrine: assumes rational actors choose strategies to maximize payoff under constraint, that nuclear weapons create a constraint on destructive capacity rather than making war irrational, and that limited war remains analytically coherent. This is not an agent but an intellectual commitment the doctrine depends on.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, game_theoretic_rationality_framework, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__countervailing_thinkable, game_theoretic_rationality_framework).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes mutual deterrence by maintaining a credible threat of limited war that can be won through precise targeting of military assets, avoiding city-destruction escalation spirals. Theoretically solves the credibility problem: if deterrence is believed to be based on suicidal mutual annihilation, it becomes less credible; if limited victory is thinkable, the threat becomes rational and therefore more believable.
% TRANSFER_FUNCTION: Moves research funding, procurement contracts, and intellectual authority from general defense spending into advanced nuclear weapons development, precision targeting systems, and strategic doctrine maintenance. Transfers the risk of nuclear conflict escalation from military planners (who assume winnability) to civilian populations near military targets and allied states dependent on the doctrine's stability.
% ABSENT_VOICES: Disarmament advocates and non-nuclear states are excluded from strategic command doctrine development. They argue winnability doctrine is a self-fulfilling prophecy that makes nuclear war thinkable where it should be unthinkable. Global South populations targeted by counterforce strategies have no voice in the doctrine that places them at risk.
% DISAPPEARANCE_RATIONALE: If the countervailing winnability doctrine disappeared, military strategy would reorganize around either pure deterrence (nuclear war is unwinnable and therefore irrational) or disarmament frameworks. Force postures would shift away from counterforce targeting, arms control regimes would regain negotiating leverage, and military procurement would contract toward maintenance and eventual elimination rather than modernization. Strategic studies would reorganize around different foundational assumptions.
% FOUNDING_PROBLEM: After 1945, military planners faced the problem of extending credible deterrence to allies without relying on suicidal threats of total retaliation. Countervailing strategy solved this by arguing that nuclear war could be limited, controlled, and won through precise targeting that avoids city destruction. This made the threat of nuclear use rational rather than apocalyptic.
% FOUNDING_PROBLEM_CORROBORATION: Strategic command and defense intellectuals attest the founding problem remains live: credibility requires that the threat be rational, which requires winnability. Disarmament advocates and many physicists attest the problem is false: nuclear war cannot be limited once nuclear weapons are used, the doctrine creates dangerous escalation dynamics, and its persistence is explained by institutional and industrial interests rather than strategic necessity. Arms control regimes document that winnability doctrine has blocked disarmament agreements and justified warhead modernization that extends arsenal life indefinitely.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval as strategic command doctrine becomes increasingly relied upon to justify force modernization that serves industrial interests rather than deterrent credibility. The foundational coordination function (solving the credibility problem) remains real but diminishes as alternatives (assured retaliation, disarmament frameworks) become analytically available. Suppression rises from 0.48 to 0.72 as countervailing doctrine faces mounting theoretical challenges — the constraint's persistence increasingly depends on institutional gatekeeping (career risk for dissident thinkers, publication bias in defense journals, funding concentration in countervailing research) rather than on strategic consensus. Theater_ratio climbs from 0.35 to 0.58 as the proportion of doctrine maintenance devoted to performative credibility assertion grows relative to actual strategic reasoning. Accessibility_collapse is moderate (0.45) because alternative strategic frameworks (existential deterrence, disarmament) remain intellectually available and are actively articulated by excluded voices; the constraint persists not because alternatives are literally unthinkable but because institutional power makes them professionally costly to advocate. Resistance is substantial (0.62) because disarmament advocates, arms control regimes, and historical analysis continue to challenge winnability assumptions even within defense institutions.
 *
 * PERSPECTIVAL GAP:
 *   Strategic command and military-industrial stakeholders experience the constraint as real coordination (solving deterrence credibility) with extractive side-effects (procurement benefits). Arms control regimes and disarmament advocates experience the constraint as pure extraction wearing a coordination mask (winnability doctrine serves institutional interests, not security). The divided perception tracks the structural asymmetry: beneficiaries frame the constraint as necessary security; victims frame it as institutional self-dealing. The engine computes per-seat type from the structural data — strategic command should compute as tangled_rope (coordinates deterrence, extracts procurement justification); arms control should compute as snare (no coordination benefit, pure extraction of negotiating leverage). This seat divergence IS the measurement the story takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic command is near the full-target end of directionality (d~0.75): it bears the existential responsibility for nuclear strategy and faces intense pressure to maintain doctrine credibility, yet its institutional survival depends on the constraint's persistence. Military-industrial complex is near full-beneficiary (d~0.05): captures procurement contracts without bearing strategic risk. Arms control regimes face high directionality (d~0.85): their entire institutional purpose is constrained by countervailing planning. Civilian populations are trapped targets (d~0.95): no exit options, no voice, bearing risk. Strategic studies establishment sits near symmetric (d~0.45): benefits from doctrine legitimacy and research funding, but also depends on intellectual coherence — increasing cognitive dissonance as winnability assumptions face challenge. The engine derives these from the beneficiary/victim declarations and exit options; directionality overrides are not needed because the structural data produces the correct positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible deterrence under nuclear constraint) is declared contested. Strategic command attests it remains live: winnability doctrine is necessary to maintain rational threat credibility. Disarmament advocates and game theorists attest the problem is partly dead: nuclear war is demonstrably uncontrollable once initiated, so winnability assumptions are strategically incoherent — the doctrine persists because military institutions have path-dependent lock-in, not because the founding problem is unsolved. The constraint's theater_ratio trajectory (rising performative maintenance) supports the mandatrophy reading: the doctrine is maintained increasingly through institutional gatekeeping rather than strategic consensus, indicating the founding problem has shifted from genuine deterrence requirement to institutional legitimation requirement. The presence of the suppression_requirement measurements documents the rising cost of maintaining the doctrine against mounting challenge — mandatrophy is not yet declared (the engine decides) but the metrics clearly show the constraint's functional purpose has decayed while institutional maintenance has hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_vs_deterrence_incoherence,
    'Is limited nuclear war genuinely thinkable as a coherent strategic scenario, or does the doctrine''s winnability claim rest on logical impossibilities that are maintained only through selective focus on counterforce targeting while ignoring escalation dynamics?',
    'War-game analysis comparing models that assume rational actor containment of escalation (winnability framework) against models that incorporate uncertainty, communication breakdown, and system-level feedback (deterrence framework). Historical case study of near-misses and escalation incidents.',
    'If winnability is genuinely thinkable, the doctrine is a rational constraint on nuclear strategy; if it is logically incoherent, the doctrine is a cover story for institutional maintenance, and should be reclassified as snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(winnability_vs_deterrence_incoherence, conceptual, 'The logical coherence of limited nuclear war assumption.').

omega_variable(
    institutional_lock_in_vs_strategic_necessity,
    'Does countervailing doctrine persist because it solves a genuine strategic problem (credible deterrence), or because military and industrial institutions have locked in to winnability assumptions and resist disconfirmation?',
    'Comparison of doctrine persistence across different institutional structures. Analysis of how doctrine shifts when leadership or doctrine-setting institutions change. Study of institutional resistance to evidence contrary to winnability assumptions.',
    'If institutional lock-in explains persistence, the constraint is extractive (serving institution interests rather than security); if strategic necessity is genuine, the constraint solves real coordination problems under nuclear constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_lock_in_vs_strategic_necessity, empirical, 'Whether doctrine persists due to strategic necessity or institutional path-dependence.').

omega_variable(
    kernel_reading_boundary,
    'This constraint instantiates ONE reading of the contested kernel ''war_winnability_post_1945'': the claim that limited victory remains achievable through counterforce targeting. Sibling readings hold that winnability was made categorically unthinkable by nuclear weapons, or that winnability became rhetorically unsayable while remaining operationally planned. Is the boundary between these readings empirical (what is actually strategically possible) or conceptual (how winnability is framed and justified)?',
    'Textual and institutional analysis of how winnability language is deployed, what alternatives are considered feasible, and how doctrine shifts if winnability framing is removed. Game-theoretic and historical analysis of whether limited nuclear war has ever been demonstrated as strategically stable or operationally viable.',
    'If the boundary is empirical, one reading is correct and the others are false. If the boundary is conceptual, all three readings coexist as different framings of the same strategic situation, and the kernel is fundamentally under-determined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether readings differ empirically or only in framing.').

omega_variable(
    suppression_internalization_interpersonal,
    'For defense intellectuals and military planners, does the suppression of disconfirmatory evidence (contrary arguments, game-theoretic challenges to winnability assumptions, historical evidence of escalation) operate as structural suppression (career risk, institutional pressure, publication gatekeeping) or as internalized suppression (belief in winnability has become part of professional identity, making disconfirmation feel personally threatening)?',
    'Post-exit analysis: when strategic thinkers leave military institutions and publish disconfirmatory work, does the suppression persist or dissolve? Interview and memorialization evidence from retired planners about when and why they adopted or questioned winnability doctrine.',
    'If suppression is structural, exiting the institution removes it; if internalized, the doctrine persists in the thinker''s reasoning even after institutional exit, indicating deeper identity fusion with the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'Whether suppression of disconfirmatory evidence is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(war__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(war__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(war__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_regime_viability).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_proliferation_incentives).

% DUAL FORMULATION NOTE:
% This constraint is one of three constraint stories decomposing the contested kernel 'war_winnability_post_1945'. All three are linked via network.affects_constraints and share the same kernel_id but different reading_ids. The countervailing reading assumes winnability remains thinkable; the deterrence reading assumes winnability became unthinkable; the rhetorical reading assumes winnability became unsayable-but-planned. Each reading has its own ε, beneficiary/victim structure, and classification. The readings do not measure different aspects of one constraint — they represent genuinely different commitments about nuclear strategy's rational structure. Decomposition follows the ε-invariance principle: if measuring winnability one way (operationally viable counterforce) gives high ε and measuring it another way (logically incoherent) gives low ε, the observer is looking at two constraints, not one measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
