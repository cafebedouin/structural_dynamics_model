% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational-Dropout Constraint (great-power war removed from the active choice set on cost-benefit grounds)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   Since 1945, and especially since the achievement of secure second-strike
 *   capability by multiple states in the 1960s, a widely held strategic
 *   doctrine holds that nuclear weapons have not eliminated the physical
 *   possibility of great-power war but have removed it from the set of
 *   actions any rational leadership would choose, because the destructive
 *   costs of nuclear exchange overwhelm any strategic gain obtainable through
 *   victory. This constraint coordinates the avoidance of direct great-power
 *   war (a genuine stabilizing function) while displacing the costs of
 *   unresolved great-power rivalry onto proxy states, conventional theaters,
 *   and the populations sheltering under extended deterrence commitments —
 *   none of whom set the terms of the calculus that governs their exposure.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — maintain arsenals and set the calculus's terms
 *   - extended_deterrence_allies: beneficiary (powerful/constrained) — sheltered but doctrinally dependent
 *   - strategic_stability_analysts: beneficiary/observer (organized/mobile) — professionally invested in the calculus's coherence
 *   - non_nuclear_frontline_states: payer (moderate/trapped) — absorb displaced conventional and proxy violence
 *   - civilian_populations_under_extended_deterrence: payer/beneficiary (powerless/trapped) — bear uncompensated tail risk
 *   - conventional_war_constrained_revisionist_states: payer (powerful/constrained) — denied a historically available strategic option
 *   - arms_control_treaty_bodies: observer (institutional/analytical) — monitor and adjust the calculus's parameters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.42).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational-Dropout Constraint (great-power war removed from the active choice set on cost-benefit grounds)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'aa656196-5175-4215-a115-43bd4511092c').
narrative_ontology:cs_kernel_codification('aa656196-5175-4215-a115-43bd4511092c', distributed).
narrative_ontology:cs_authority_grounding('aa656196-5175-4215-a115-43bd4511092c', distributed).
narrative_ontology:cs_reading_relation('aa656196-5175-4215-a115-43bd4511092c', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa656196-5175-4215-a115-43bd4511092c', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('aa656196-5175-4215-a115-43bd4511092c', foundational, war_remains_choosable_but_irrational).
narrative_ontology:cs_axiom_status(war_remains_choosable_but_irrational, holdable).
narrative_ontology:cs_axiom_grounding('aa656196-5175-4215-a115-43bd4511092c', war_remains_choosable_but_irrational, empirically_contingent).
narrative_ontology:cs_axiom('aa656196-5175-4215-a115-43bd4511092c', foundational, cost_benefit_calculus_governs_state_war_decisions).
narrative_ontology:cs_axiom_status(cost_benefit_calculus_governs_state_war_decisions, holdable).
narrative_ontology:cs_axiom_grounding('aa656196-5175-4215-a115-43bd4511092c', cost_benefit_calculus_governs_state_war_decisions, instrumental).
narrative_ontology:cs_reference_frame('aa656196-5175-4215-a115-43bd4511092c', mutual_assured_destruction_calculus_baseline).
narrative_ontology:cs_drift_state('aa656196-5175-4215-a115-43bd4511092c', post_cold_war_multipolar_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa656196-5175-4215-a115-43bd4511092c', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_analysts).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_frontline_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, conventional_war_constrained_revisionist_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_extended_deterrence).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_cost_benefit_calculus).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, great_power_war_obsolescence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain arsenals and doctrine that keep general war structurally reachable but calculably irrational to initiate. They set the terms of the calculus itself — what counts as a 'cost' and a 'benefit' in the arithmetic — and use the resulting stability to pursue coercive bargaining, conventional adventurism, and proxy conflict below the threshold where the dropout logic bites.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states, beneficiary).

% Shelter under a nuclear patron's umbrella, gaining security against conventional invasion because the patron's nuclear guarantee makes major war against them irrational for an adversary. Their exit from the arrangement would mean either developing independent deterrents or facing conventional exposure; neither is cheap, so they remain inside the calculus they did not design.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% Build careers, institutes, and policy influence on formalizing and defending the rational-dropout logic — modeling escalation ladders, cost-benefit thresholds, and stability metrics. They profit intellectually and professionally from the constraint's continued plausibility and have limited incentive to highlight scenarios where the calculus breaks down.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_analysts, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, strategic_stability_analysts, observer).

% Sit on borders between nuclear-armed rivals or their proxies. The rational-dropout constraint keeps war 'off the table' for the great powers themselves, but does not remove conventional or proxy war conducted through or against these states, who absorb the violence that the great-power calculus displaces downward. They cannot opt out of geography.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_frontline_states, payer,
    moderate, immediate, trapped, regional).

% Live under the umbrella that the dropout logic protects, but also bear the residual tail risk that the calculus is wrong, miscalculated, or overridden by accident or irrational leadership. They receive the stated security benefit while carrying an irreducible low-probability, catastrophic-magnitude cost they never individually priced or consented to.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_extended_deterrence, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, civilian_populations_under_extended_deterrence, beneficiary).

% Have territorial or status ambitions that would historically have been pursued through direct great-power war, but the rational-dropout calculus removes that avenue as cost-ineffective against a nuclear-armed rival. They are pushed into slower, costlier, more constrained instruments — economic coercion, gray-zone operations, proxy war — and experience the constraint as a binding limit on an otherwise available strategy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, conventional_war_constrained_revisionist_states, payer,
    powerful, biographical, constrained, continental).

% Monitor and negotiate the arsenal levels and doctrines that keep the cost-benefit ratio unfavorable to war. They can shift the calculus's parameters through verification regimes and reduction agreements but do not control whether states privately recompute the ratio differently under crisis conditions.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great-power behavior around a shared, if informally held, cost-benefit arithmetic in which the destructive yield of nuclear arsenals makes the expected costs of major war exceed any calculable strategic gain — allowing rival nuclear states to avoid direct war without requiring trust, treaty, or physical impossibility, merely mutual recognition that the sums do not work.
% TRANSFER_FUNCTION: Moves the risk and cost of great-power conflict downward and outward: from direct war between nuclear principals (avoided) to proxy wars, conventional conflicts on the periphery, and the diffuse, uncompensated tail risk borne by populations sheltering under extended deterrence and by frontline states caught in the resulting substitution effects.
% ABSENT_VOICES: Frontline populations who bear the substituted proxy and conventional violence, and future generations who inherit the tail risk of a calculus that depends on continued rational leadership, are not party to how the cost-benefit terms are set; strategic stability analysts and weapon states largely author the calculus among themselves.
% DISAPPEARANCE_RATIONALE: If the rational-dropout constraint were shown false tomorrow — if war were recomputed as net-beneficial for some nuclear-armed actor — nuclear weapon states and their doctrine communities would need to rebuild deterrence theory from scratch, and extended-deterrence allies would face an acute credibility crisis. Whether the 'world rearranges' or 'stays the same' is itself contested among the readings of this kernel: the rational-dropout reading holds that behavior would change sharply if the calculus flipped, while a structural-impossibility reading would deny the calculus was ever the operative constraint in the first place.
% FOUNDING_PROBLEM: Following Hiroshima and Nagasaki, and accelerating through thermonuclear development and mutual arsenal growth, strategists needed an account of why great-power war between nuclear-armed rivals had not recurred despite persistent geopolitical rivalry (Cold War, subsequent multipolar nuclear competition) — the rational-dropout account supplies that: war stayed structurally possible but became irrational to choose.
% FOUNDING_PROBLEM_CORROBORATION: Strategic stability analysts and nuclear weapon states attest the calculus remains live and operative (citing continued arsenal maintenance, doctrine review, and crisis stability planning). Independent voices outside the benefiting community — arms control scholars studying near-miss incidents (1983 Able Archer, 1995 Norwegian rocket incident), and historians of nuclear command-and-control failures — corroborate that the calculus has held so far but attribute this partly to luck and human intervention against automated escalation logic, not solely to stable rational calculation; this is exactly the kind of outside corroboration the founding-problem narrative needs and only partially receives.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).
:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than extreme: the constraint genuinely coordinates avoidance of catastrophic war, which is a real and substantial benefit shared broadly, but the arithmetic that sustains this avoidance is set and interpreted by the nuclear weapon states and the analyst community that services them, and the residual risk and the displaced conventional/proxy violence fall on parties who did not write the calculus. Suppression (0.58) reflects the active doctrinal, institutional, and sometimes coercive work (extended deterrence commitments, non-proliferation enforcement, arms control verification, occasional coercive nonproliferation actions) required to keep the calculus's terms stable and to prevent revisionist actors from testing whether the arithmetic actually holds. Theater ratio (0.3) is moderate: doctrine and posture reviews contain real analytical content but also substantial performative signaling (declaratory policy, force posture theater) whose function is persuasion of adversaries and domestic audiences as much as genuine capability assessment. The suppression_requirement series shows a Cold War peak (1962, Cuban Missile Crisis era) as the calculus was actively tested and defended, a post-Cold-War trough as tensions eased, and a recent uptick reflecting renewed great-power competition and multipolar nuclear dynamics (2008-2025) that require the calculus to be actively re-asserted rather than passively assumed.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and their extended-deterrence beneficiaries sit near the beneficiary end of directionality: the arrangement subsidizes their security at low marginal cost relative to the alternative of active great-power war. Strategic stability analysts also benefit, though indirectly, through professional and institutional capture of the calculus's authority. Frontline states and civilian populations under the umbrella sit nearer the target end: they bear displaced or tail-risk costs they did not calculate and cannot exit, given trapped or constrained exit options tied to geography and citizenship. Conventional-war-constrained revisionist states experience the constraint as a binding strategic limit — their exit option (constrained) reflects that they retain agency but only within a narrowed strategic menu.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining the absence of recurrent great-power war among nuclear rivals — could in principle become obsolete if the underlying mechanism were shown false (e.g., if a rational path to net-beneficial nuclear war were demonstrated to exist under some technological or doctrinal configuration, such as effective missile defense or decapitation-strike capability). The founding_problem_status is authored as contested rather than resolved because near-miss incidents and evolving strike-and-defense technology continually reopen the empirical question of whether the calculus still holds, while the institutions built around the rational-dropout reading (arms control regimes, deterrence doctrine communities) have strong incentives to declare it settled. This prevents the classification from either dismissing the constraint as pure theater (it has coordinated real avoidance of catastrophic war) or crediting it as a costless natural law (it requires continuous active maintenance and imposes real, unevenly distributed costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Among the three readings of the nuclear_impossibility_kernel (rational_dropout, credibility_paradox, structural_contraction), which mechanism actually explains the absence of great-power nuclear war — is it that war remains reachable but irrational (this reading), that deterrence threats are inherently incredible yet somehow function anyway (credibility_paradox_reading), or that mutual annihilation makes rational victory paths structurally nonexistent (structural_contraction_reading)?',
    'Historical case analysis of near-miss crises (Cuban Missile Crisis, Able Archer 1983, India-Pakistan crises) examining whether decision-makers actually performed cost-benefit calculations that concluded war was survivable-but-not-worth-it (supporting this reading) versus treating the option as literally unconsidered or physically foreclosed (supporting a sibling reading).',
    'If the historical record shows leaders genuinely computing and rejecting a costly-but-possible victory path, this reading is vindicated as the operative mechanism; if leaders never treated war as a live option at all, the structural_contraction_reading better describes the actual cognitive/strategic process, and this reading''s extraction profile (which depends on an active, contestable calculus) would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three kernel readings correctly characterizes the operative decision mechanism.').

omega_variable(
    rational_actor_assumption_fragility,
    'Does the rational-dropout constraint depend on an assumption of continuously rational, well-informed leadership that may not hold under crisis conditions, technological surprise, or leadership transition?',
    'Review of declassified crisis-decision records and command-and-control failure incidents (false alarms, miscommunication events) for evidence of near-departures from the rational calculus under stress.',
    'If rationality is shown to be fragile under crisis pressure, the constraint''s effective reliability is lower than its stated calculus suggests, and the tail risk borne by civilian populations under extended deterrence is understated in the current extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_fragility, empirical, 'Whether the constraint''s stabilizing function depends on an assumption that may be empirically fragile.').

omega_variable(
    beneficiary_authorship_of_calculus,
    'Is the cost-benefit arithmetic itself an objective feature of the strategic environment, or is it substantially authored and maintained by the same nuclear weapon states and analyst communities who benefit from it being believed?',
    'Comparative analysis of how the calculus''s terms (what counts as an acceptable cost, what counts as sufficient benefit) have shifted with each state''s declared doctrine changes, and whether shifts track genuine capability changes or institutional/political interests.',
    'If the calculus''s terms are substantially self-authored by beneficiaries rather than objectively derived, the tangled_rope classification is reinforced (coordination function real, but its specific terms serve extraction); if the terms track objective capability facts closely, the constraint sits closer to a genuine rope with incidental distributional effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_authorship_of_calculus, conceptual, 'Whether the rational-dropout calculus is objectively derived or authored by its beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.22).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.2).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.3).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.62).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(nucl_su_t2008, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nuclear_impossibility_kernel. structural_contraction_reading claims physical/structural impossibility of a rational victory path (a stronger, near-mountain claim); credibility_paradox_reading claims the deterrent threat is self-undermining yet functionally operative (a paradox-centered claim emphasizing signaling dynamics); this story (rational_dropout_reading) claims war remains reachable in the strategy space but is excluded from rational selection by cost-benefit dominance — a tangled_rope claim emphasizing an actively maintained and beneficiary-authored calculus. All three share the same underlying empirical referent (absence of recurrent great-power nuclear war since 1945) but diverge in mechanism, in beneficiary structure, and in extraction profile, per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
