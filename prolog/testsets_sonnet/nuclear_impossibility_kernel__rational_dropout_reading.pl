% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Nuclear Rational-Dropout Constraint on Great-Power War
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This story instantiates the rational-dropout reading of the nuclear
 *   impossibility kernel: nuclear weapons did not make great-power war
 *   physically impossible, nor produce a logically incoherent deterrence
 *   paradox — they altered the cost-benefit calculus such that victory
 *   remains structurally reachable but rationally unappealing to any
 *   calculating actor. This is a live-choice reading, not a foreclosure
 *   reading: the M-set of strategic options still contains 'war' as a site,
 *   but that site has been demoted from active consideration by cost-benefit
 *   accounting that arsenal states, defense industries, and strategic-studies
 *   professionals actively maintain and periodically recalibrate (through new
 *   weapons systems, limited-use doctrine, missile defense). Because the war
 *   option remains reachable rather than foreclosed, the framework requires
 *   ongoing active maintenance — modernization programs, doctrine revision,
 *   credible signaling — which is exactly the enforcement structure a tangled
 *   rope requires. The sibling structural_contraction_reading treats war as
 *   physically foreclosed (no maintenance needed, closer to mountain); the
 *   sibling credibility_paradox_reading treats the deterrent threat itself as
 *   internally incoherent. This story does not adjudicate between them — it
 *   is the ε for THIS reading alone.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: Primary agenda-setter and beneficiary (institutional/arbitrage) — sets and recalibrates the cost-benefit calculus
 *   - defense_industrial_establishments: Beneficiary (organized/arbitrage) — profits from continuous recalibration of the ratio
 *   - strategic_studies_professionals: Beneficiary/observer (moderate/mobile) — professional capital invested in the rational-actor framework
 *   - non_nuclear_frontline_states: Payer (powerless/trapped) — bears extended-deterrence risk without controlling the calculus
 *   - conventional_force_populations: Payer (powerless/trapped) — absorbs displaced great-power competition
 *   - historians_of_nuclear_crises: Analytical observer (analytical) — assesses whether rational calculation or luck actually governed past crises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.71).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational-Dropout Constraint on Great-Power War").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '3a6ae720-834f-4e86-ab37-65ca9ba921a1').
narrative_ontology:cs_kernel_codification('3a6ae720-834f-4e86-ab37-65ca9ba921a1', distributed).
narrative_ontology:cs_authority_grounding('3a6ae720-834f-4e86-ab37-65ca9ba921a1', distributed).
narrative_ontology:cs_reading_relation('3a6ae720-834f-4e86-ab37-65ca9ba921a1', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a6ae720-834f-4e86-ab37-65ca9ba921a1', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('3a6ae720-834f-4e86-ab37-65ca9ba921a1', foundational, war_remains_in_reachable_option_set).
narrative_ontology:cs_axiom_status(war_remains_in_reachable_option_set, holdable).
narrative_ontology:cs_axiom_grounding('3a6ae720-834f-4e86-ab37-65ca9ba921a1', war_remains_in_reachable_option_set, empirically_contingent).
narrative_ontology:cs_axiom('3a6ae720-834f-4e86-ab37-65ca9ba921a1', foundational, rational_cost_benefit_calculation_governs_escalation_choice).
narrative_ontology:cs_axiom_status(rational_cost_benefit_calculation_governs_escalation_choice, holdable).
narrative_ontology:cs_axiom_grounding('3a6ae720-834f-4e86-ab37-65ca9ba921a1', rational_cost_benefit_calculation_governs_escalation_choice, instrumental).
narrative_ontology:cs_reference_frame('3a6ae720-834f-4e86-ab37-65ca9ba921a1', cold_war_rational_deterrence_consensus).
narrative_ontology:cs_drift_state('3a6ae720-834f-4e86-ab37-65ca9ba921a1', post_cold_war_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a6ae720-834f-4e86-ab37-65ca9ba921a1', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_professionals).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_frontline_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, conventional_force_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_arsenal_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_actor_deterrence_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, cost_benefit_war_calculus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize arsenals whose primary justification is that direct great-power war has become irrational rather than impossible. They set doctrine, arms-control terms, and escalation thresholds. Because the constraint operates on cost-benefit calculation rather than physical foreclosure, they retain latitude to calibrate 'usable' scenarios (limited strikes, tactical use, proxy escalation) that the sibling structural-contraction reading would treat as foreclosed. Their exit from the constraint's logic is effectively arbitrage: they can adjust the perceived cost-benefit ratio through new weapons systems, missile defense, or doctrine changes without ever fully closing the reachable set.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_armed_states, beneficiary).

% Profit from continuous modernization programs justified by the claim that the cost-benefit calculus must be continuously re-secured against erosion (new delivery systems, missile defense, low-yield weapons that reopen limited-use scenarios). Their revenue depends on the reading remaining a live, actively-managed calculation rather than a settled physical fact — a fully foreclosed war option would reduce the perceived need for continuous arsenal renewal.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_establishments, beneficiary,
    organized, generational, arbitrage, national).

% Build careers modeling the rational-choice cost-benefit calculus of nuclear war — escalation ladders, limited-nuclear-option theory, counterforce/countervalue tradeoffs. Their professional identity depends on war remaining a modelable, rationally-evaluable option rather than a foreclosed impossibility (which would end the analytical genre) or an incoherent paradox (which would undercut the rational-actor framework they use). They can exit into adjacent fields but their disciplinary capital is invested in this reading.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_professionals, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_professionals, observer).

% Live inside the security architecture the rational-dropout logic produces (extended deterrence, alliance guarantees, nuclear umbrellas) without controlling the calculus. They bear the risk that a nuclear patron's cost-benefit recalibration (new limited-use doctrine, willingness to escalate for credibility) changes the war-avoidance math on their territory. They cannot exit the alliance structure without losing the umbrella, and cannot influence how the arsenal states weigh costs and benefits.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_frontline_states, payer,
    powerless, biographical, trapped, regional).

% Personnel and civilians in conventional theaters where great-power competition is displaced downward because direct nuclear-state confrontation is rationally deprecated. Proxy wars, gray-zone conflict, and conventional escalation absorb the competitive energy that the rational-dropout logic redirects away from direct confrontation, concentrating actual violence on populations who have no say in the calculus that produces this displacement.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, conventional_force_populations, payer,
    powerless, immediate, trapped, regional).

% Fund continuous arsenal modernization justified by the need to keep the cost-benefit ratio unfavorable to adversaries. They have some democratic leverage in principle but face an entrenched bipartisan consensus and classified decision-making that make the actual cost-benefit accounting largely inaccessible to public scrutiny.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_arsenal_states, payer,
    moderate, generational, constrained, national).

% Argue that treating war as merely irrational (rather than physically foreclosed or paradoxically incoherent) leaves dangerous latitude for miscalculation, limited-use doctrine creep, and arms races justified by continuous cost-benefit recalibration. They are largely excluded from classified doctrine-setting processes and treaty negotiations dominated by the arsenal states themselves.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% Study near-miss crises (Cuban Missile Crisis, 1983 war scare, various false alarms) to assess whether the rational-choice framing accurately describes decision-making under pressure, or whether it retrospectively rationalizes outcomes that were closer to luck than calculation.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, historians_of_nuclear_crises, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, diffuse).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides decision-makers in nuclear-armed states with a stable, shared framework for evaluating whether direct war serves any conceivable strategic objective, allowing crisis behavior to be modeled, communicated, and mutually anticipated across adversary states.
% TRANSFER_FUNCTION: Redirects competitive and coercive energy away from direct nuclear-state confrontation and downward onto conventional theaters, proxy states, and frontline non-nuclear populations, while channeling continuous fiscal resources from taxpayers to defense-industrial modernization justified by maintaining the unfavorable cost-benefit ratio.
% ABSENT_VOICES: Frontline non-nuclear states and populations in proxy/gray-zone theaters bear the practical consequences of the calculus (displaced conflict, extended-deterrence risk) but have no seat in the classified doctrine-setting and modernization decisions that determine how the cost-benefit ratio is calibrated.
% DISAPPEARANCE_RATIONALE: If the rational-dropout framing were abandoned overnight, arsenal states would either fall back to the structural-contraction reading (treating war as physically impossible, potentially reducing modernization urgency) or to the credibility-paradox reading (destabilizing deterrence signaling). Strategic-studies professionals dispute which alternative framing would dominate; defense establishments would likely argue any framing shift threatens deterrence credibility, while arms-control advocates would argue a shift toward physical-impossibility framing would reduce dangerous doctrine creep.
% FOUNDING_PROBLEM: Cold War strategists needed a decision-theoretic account of why great-power nuclear war did not occur despite deep hostility and repeated crises, and a framework for advising leaders on escalation risk that did not depend on treating war as literally impossible (which early game theorists worried was empirically false and dangerously complacent).
% FOUNDING_PROBLEM_CORROBORATION: Cold War historians and declassified crisis archives (Cuban Missile Crisis ExComm transcripts, Able Archer 83 retrospectives) corroborate that the rational-choice framing was actively used in real decision-making, outside the interest of arms-control advocates or defense contractors. However, independent historians of nuclear near-misses note that several crises were resolved by luck, miscommunication, or subordinate-level restraint rather than by the rational cost-benefit calculus the framework claims governed events — suggesting the founding problem may be only partially live and partially a retrospective rationalization maintained by the professional and industrial beneficiaries of the framing.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, contested).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that this reading requires continuous fiscal and institutional investment to keep the war option's cost-benefit ratio unfavorable — a genuinely foreclosed impossibility would need no such maintenance, but a rationally-reachable-but-deprecated option does. Suppression (0.71) is high because classified doctrine, alliance structures, and bipartisan consensus actively suppress alternative framings (both the more sanguine structural-contraction reading and the more anxious credibility-paradox reading) from displacing the rational-actor consensus among decision-makers. Theater ratio (0.40) is moderate: substantial genuine strategic analysis occurs, but a meaningful share of modernization and doctrine activity is signaling/credibility performance rather than functionally necessary recalculation. Accessibility collapse (0.62) is moderate-high: alternative framings are not fully inaccessible (arms-control and historical-luck counter-narratives exist and circulate) but the rational-actor framework dominates institutional decision-making almost completely. Resistance (0.55) is real and rising — arms-control advocates and revisionist historians actively contest the framing, which is exactly what distinguishes an actively-maintained tangled rope from an inert mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a nuclear-armed state's strategic planning apparatus, the rational-dropout reading looks like sober, functional risk management — a genuine coordination achievement that prevented catastrophe for seven decades. From the seat of a conventional-force population in a proxy theater, the same reading looks like a mechanism that displaced great-power violence onto them without their consent or representation. The engine computes these as structurally different positions from the same authored data; the divergence is not resolved by this story, only recorded.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed states, their defense industries, and strategic-studies professionals sit near the beneficiary end: they set the terms of the calculus, profit from its continuous maintenance, or build careers modeling it, and they retain arbitrage-grade exit (they can shift doctrine, weapons systems, or professional focus without losing standing). Frontline non-nuclear states and conventional-theater populations sit near the target end: they are trapped inside a security architecture whose cost-benefit terms they cannot set, and they absorb the downstream costs (extended-deterrence risk, displaced proxy conflict) that the rational-dropout logic exports away from direct nuclear-state confrontation. Taxpayers in arsenal states occupy an intermediate position — constrained rather than trapped, with formal democratic leverage undermined by classification and bipartisan consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accounting for great-power restraint under nuclear conditions without assuming either physical impossibility or logical incoherence) retains partial vitality — the rational-choice framework is still actively invoked in real crisis decision-making, per declassified archives. But its persistence is also partly self-serving: defense industries and strategic-studies careers depend on war remaining a modelable, continuously-recalibrated option rather than settling into either sibling reading, which would reduce the perceived need for their ongoing analytical and industrial labor. The tangled_rope classification captures this duality precisely: genuine coordination function (shared crisis-behavior expectations across adversaries) coexists with asymmetric extraction (fiscal costs to taxpayers, risk exported to frontline states) sustained by active enforcement (classified doctrine control, alliance structure maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_foreclosure_ambiguity,
    'Is the war option genuinely still reachable in the rational-choice sense this reading claims (available but irrational), or has escalation dynamics research (inadvertent escalation, command-and-control failure modes, use-them-or-lose-them pressures) shown that the ''rational'' framing itself breaks down under crisis conditions, making the structural_contraction reading''s harder foreclosure claim more accurate?',
    'Close historical analysis of near-miss crises (1962, 1983, 1995 Norwegian rocket incident) for evidence of whether decision-makers were actually performing cost-benefit calculation under pressure or were closer to structurally constrained by systems and time pressure regardless of rational assessment.',
    'If crisis behavior shows decision-makers were not meaningfully exercising rational choice under time pressure, this reading''s core premise (a live, calculated dropout) weakens relative to the structural_contraction reading, which does not depend on rational deliberation actually occurring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_foreclosure_ambiguity, empirical, 'Whether the rational-choice framing accurately describes actual crisis decision-making or retrospectively rationalizes outcomes closer to luck or structural constraint.').

omega_variable(
    beneficiary_capture_of_framing_choice,
    'Is the rational-dropout reading''s dominance in institutional doctrine a genuine analytical conclusion, or is it partly sustained because it is the framing most favorable to continuous modernization funding and analytical career structures (as opposed to the structural_contraction reading, which would reduce perceived urgency for continuous recalibration)?',
    'Compare doctrine and funding patterns in states/eras where alternative framings (arms-control-driven foreclosure narratives) gained institutional traction against periods of rational-dropout dominance, controlling for external threat perception.',
    'If framing choice correlates with institutional funding incentives independent of external threat, this constraint''s classification shifts further toward tangled_rope (framing serving extraction) rather than rope (framing serving pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_framing_choice, conceptual, 'Whether the choice among sibling readings is itself influenced by which reading best serves the interests of the professional and industrial beneficiaries of continuous nuclear modernization.').

omega_variable(
    kernel_site_expansion_stability,
    'Does the war-option site in the M-set remain stably reachable across the full interval, or has it contracted (moving this reading closer to the structural_contraction reading) or expanded (moving it toward greater active engagement) as weapons technology, missile defense, and doctrine have evolved?',
    'Track doctrinal statements and weapons-system development (low-yield weapons, missile defense claims of first-strike viability) for evidence of whether decision-makers increasingly treat limited nuclear use as viable (site expansion) or increasingly treat any use as guaranteed catastrophe (site contraction toward the sibling reading).',
    'Site contraction would suggest this reading is drifting toward the structural_contraction reading over time; site expansion (e.g. low-yield ''usable'' weapons) would suggest the rational-dropout reading is becoming more actively contested by war-fighting doctrine that treats limited victory as achievable at acceptable cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_site_expansion_stability, empirical, 'Whether the reachable-but-deprecated status of the war option is stable, contracting, or expanding over the measured interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1949, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1949, 0.25).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1983, 0.35).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.38).
narrative_ontology:measurement(nucl_tr_t2008, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1949, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.42).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1983, 0.55).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.48).
narrative_ontology:measurement(nucl_be_t2008, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2008, 0.52).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1949, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1983, 0.72).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.58).
narrative_ontology:measurement(nucl_su_t2008, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2024, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1949, tn=2024
narrative_ontology:measurement(nucl_grid_01, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(class), 1949, 0.3).
narrative_ontology:measurement(nucl_grid_02, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(class), 2024, 0.5).
narrative_ontology:measurement(nucl_grid_03, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(individual), 1949, 0.2).
narrative_ontology:measurement(nucl_grid_04, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(individual), 2024, 0.35).
narrative_ontology:measurement(nucl_grid_05, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(organizational), 1949, 0.35).
narrative_ontology:measurement(nucl_grid_06, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(organizational), 2024, 0.58).
narrative_ontology:measurement(nucl_grid_07, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(structural), 1949, 0.4).
narrative_ontology:measurement(nucl_grid_08, nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse(structural), 2024, 0.62).
narrative_ontology:measurement(nucl_grid_09, nuclear_impossibility_kernel__rational_dropout_reading, resistance(class), 1949, 0.15).
narrative_ontology:measurement(nucl_grid_10, nuclear_impossibility_kernel__rational_dropout_reading, resistance(class), 2024, 0.45).
narrative_ontology:measurement(nucl_grid_11, nuclear_impossibility_kernel__rational_dropout_reading, resistance(individual), 1949, 0.1).
narrative_ontology:measurement(nucl_grid_12, nuclear_impossibility_kernel__rational_dropout_reading, resistance(individual), 2024, 0.3).
narrative_ontology:measurement(nucl_grid_13, nuclear_impossibility_kernel__rational_dropout_reading, resistance(organizational), 1949, 0.25).
narrative_ontology:measurement(nucl_grid_14, nuclear_impossibility_kernel__rational_dropout_reading, resistance(organizational), 2024, 0.5).
narrative_ontology:measurement(nucl_grid_15, nuclear_impossibility_kernel__rational_dropout_reading, resistance(structural), 1949, 0.2).
narrative_ontology:measurement(nucl_grid_16, nuclear_impossibility_kernel__rational_dropout_reading, resistance(structural), 2024, 0.4).
narrative_ontology:measurement(nucl_grid_17, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(class), 1949, 0.3).
narrative_ontology:measurement(nucl_grid_18, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(class), 2024, 0.55).
narrative_ontology:measurement(nucl_grid_19, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(individual), 1949, 0.15).
narrative_ontology:measurement(nucl_grid_20, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(individual), 2024, 0.3).
narrative_ontology:measurement(nucl_grid_21, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(organizational), 1949, 0.4).
narrative_ontology:measurement(nucl_grid_22, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(organizational), 2024, 0.65).
narrative_ontology:measurement(nucl_grid_23, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(structural), 1949, 0.5).
narrative_ontology:measurement(nucl_grid_24, nuclear_impossibility_kernel__rational_dropout_reading, stakes_inflation(structural), 2024, 0.75).
narrative_ontology:measurement(nucl_grid_25, nuclear_impossibility_kernel__rational_dropout_reading, suppression(class), 1949, 0.25).
narrative_ontology:measurement(nucl_grid_26, nuclear_impossibility_kernel__rational_dropout_reading, suppression(class), 2024, 0.45).
narrative_ontology:measurement(nucl_grid_27, nuclear_impossibility_kernel__rational_dropout_reading, suppression(individual), 1949, 0.15).
narrative_ontology:measurement(nucl_grid_28, nuclear_impossibility_kernel__rational_dropout_reading, suppression(individual), 2024, 0.25).
narrative_ontology:measurement(nucl_grid_29, nuclear_impossibility_kernel__rational_dropout_reading, suppression(organizational), 1949, 0.4).
narrative_ontology:measurement(nucl_grid_30, nuclear_impossibility_kernel__rational_dropout_reading, suppression(organizational), 2024, 0.6).
narrative_ontology:measurement(nucl_grid_31, nuclear_impossibility_kernel__rational_dropout_reading, suppression(structural), 1949, 0.45).
narrative_ontology:measurement(nucl_grid_32, nuclear_impossibility_kernel__rational_dropout_reading, suppression(structural), 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__rational_dropout_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the nuclear_impossibility_kernel. structural_contraction_reading claims physical foreclosure (mountain-leaning, low required maintenance); credibility_paradox_reading claims the deterrent threat is internally incoherent (a different structural claim about signal coherence rather than option reachability); rational_dropout_reading (this story) claims the option remains reachable but is rationally deprecated, requiring active, ongoing maintenance of an unfavorable cost-benefit ratio (tangled_rope-leaning). Each carries its own ε and stakeholder structure; they are linked here rather than merged because the ε-invariance principle forbids averaging structurally distinct claims into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
