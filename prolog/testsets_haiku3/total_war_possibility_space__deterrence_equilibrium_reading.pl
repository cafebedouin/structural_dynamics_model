% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Total War Deterrence Equilibrium (Mutual Vulnerability Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint embodies the deterrence equilibrium reading of the
 *   total-war possibility space: nuclear weapons remain strategically
 *   reachable but are deterred by mutual vulnerability. The reading holds
 *   that total war is not forbidden by physical law or logical impossibility,
 *   but by a cost-benefit calculation so extreme that no rational actor would
 *   cross the threshold. This reading predicts: states will continuously
 *   invest in war-fighting capability to maintain deterrent credibility;
 *   doctrine will develop more sophisticated targeting schemes and escalation
 *   ladders; the constraint is enforced through active deployment, signaling,
 *   and the perpetual threat of retaliation. The measurement series shows
 *   rising extractiveness and theater activity over time, consistent with a
 *   constraint that requires increasing institutional investment to remain
 *   credible. The reading competes with two sibling constraints: the
 *   nuclear_taboo_reading (which frames the constraint as a normative
 *   prohibition independent of material capability) and the
 *   space_contraction_reading (which argues total war has been removed from
 *   what is strategically thinkable, not merely what is strategically
 *   preferable).
 *
 * KEY AGENTS:
 *   - Nuclear weapons states — set and enforce deterrence doctrine; face a commitment trap where abandoning arsenals breaks deterrence logic but maintaining them requires perpetual arms racing and doctrine refinement.
 *   - Non-nuclear states — benefit from extended deterrence (security guarantees) but pay through reduced autonomy, alliance dependence, and exposure to proxy conflicts.
 *   - Proxy-war populations — bear the costs of deterrence by absorbing the violence of conflicts nuclear powers fight through surrogates, precisely because direct confrontation is deterred.
 *   - Strategic theorists and analysts — occupy the observer seat; their analysis shapes how deterrence is understood and justified, but they control no nuclear forces.
 *   - Disarmament advocates — excluded from nuclear doctrine-setting despite having stakes in whether deterrence continues to hold; argue the constraint should be reframed as treaty-based prohibition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.71).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Total War Deterrence Equilibrium (Mutual Vulnerability Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '65e3301c-ede1-4ece-b370-84677fa9435e').
narrative_ontology:cs_kernel_codification('65e3301c-ede1-4ece-b370-84677fa9435e', distributed).
narrative_ontology:cs_authority_grounding('65e3301c-ede1-4ece-b370-84677fa9435e', extraction).
narrative_ontology:cs_interpretation_layer_present('65e3301c-ede1-4ece-b370-84677fa9435e').
narrative_ontology:cs_reading_relation('65e3301c-ede1-4ece-b370-84677fa9435e', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('65e3301c-ede1-4ece-b370-84677fa9435e', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('65e3301c-ede1-4ece-b370-84677fa9435e', foundational, mutual_vulnerability_determines_restraint).
narrative_ontology:cs_axiom_status(mutual_vulnerability_determines_restraint, holdable).
narrative_ontology:cs_axiom_grounding('65e3301c-ede1-4ece-b370-84677fa9435e', mutual_vulnerability_determines_restraint, empirically_contingent).
narrative_ontology:cs_axiom('65e3301c-ede1-4ece-b370-84677fa9435e', foundational, war_remains_thinkable_and_plannable).
narrative_ontology:cs_axiom_status(war_remains_thinkable_and_plannable, holdable).
narrative_ontology:cs_axiom_grounding('65e3301c-ede1-4ece-b370-84677fa9435e', war_remains_thinkable_and_plannable, empirically_contingent).
narrative_ontology:cs_reference_frame('65e3301c-ede1-4ece-b370-84677fa9435e', rational_cost_benefit_deterrence).
narrative_ontology:cs_drift_state('65e3301c-ede1-4ece-b370-84677fa9435e', contemporary_multi_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65e3301c-ede1-4ece-b370-84677fa9435e', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, great_power_stability_regime).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, proxy_war_populations).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, escalation_risk_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and continuously develop nuclear arsenals under the logic of deterrence. They set doctrine (mutually assured destruction, counterforce, escalation dominance), control nuclear force employment policy, and maintain the threat credibility that the deterrent requires. They face an irreversible commitment: dismantling arsenals abandons the deterrent logic; maintaining them locks them into perpetual investment in war-fighting capability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, trapped, global).

% The system-level arrangement that benefits from mutual vulnerability as a stabilizer. No direct agent collects from it, but the institutional order that emerged post-1945 depends on the deterrent logic remaining credible and operative. It is a beneficiary in the sense that its reproduction requires the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, great_power_stability_regime, beneficiary,
    institutional, civilizational, analytical, global).

% Remain under the extended deterrent umbrella or security guarantee of nuclear states, but pay through reduced autonomy, alliance dependence, and exposure to proxy conflicts that nuclear powers fight without direct risk of escalation to their own territory. Their security is conditional on nuclear-armed patron benevolence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, national).

% Absorb the violence of conflicts fought by nuclear powers through proxy actors, precisely because the nuclear threat makes direct confrontation prohibitively costly. They have no exit from the geography of proxy conflict and no voice in the strategic calculations that drive it. The deterrent logic that prevents nuclear war is predicated on accepting continuous conventional violence as its cost.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, proxy_war_populations, payer,
    powerless, biographical, trapped, local).

% All populations within reach of nuclear weapons, to whom the entire deterrent system is transparent: it works through the credible threat of civilization-ending retaliation. The deterrent's effectiveness depends on the belief that escalation could reach them. They bear the continuous psychological cost of living under the sword of Damocles, and any miscalculation or accident could materialize that cost instantaneously.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, escalation_risk_bearers, payer,
    powerless, biographical, identity_locked, global).

% Analyze and interpret the deterrence equilibrium: whether it is stable, whether rational calculation truly governs nuclear policy, whether accident or miscalculation risks are tolerable, whether the constraint is enforced by mutual interest or by path dependence and institutional inertia. They hold no enforcement power but their analysis shapes how the constraint is understood and justified.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% Argue that total war can be prevented through abolition rather than deterrence; that the constraint should be reframed as a treaty-based prohibition rather than a cost-benefit calculation. They are excluded from the strategic decision-making process (nuclear doctrine is set by armed forces and executive branches) despite having substantive stakes in whether deterrence continues to work.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_weapons_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes great-power competition by making direct nuclear war catastrophically expensive for all parties. Creates a shared interest in preventing escalation, accident prevention mechanisms (hotlines, confidence-building measures), and mutual vulnerability as a substitute for conquest or annihilation. The coordination problem it solves is: how do nuclear-armed rivals coexist without destroying civilization?
% TRANSFER_FUNCTION: Moves sovereignty and strategic autonomy from non-nuclear states to nuclear patrons (security guarantees in exchange for alliance discipline); moves the costs of great-power competition onto proxy-war populations and civilian populations at risk of escalation. Nuclear powers transfer the threat of mutual annihilation onto the entire species, collected as a continuous psychological cost borne by all.
% ABSENT_VOICES: Disarmament advocates and abolitionist movements argue the constraint should not exist, or should be superseded by treaty-based prohibition. They are excluded from nuclear doctrine-setting and force-employment policy. Future generations, who will inherit the arsenals and the risks, have no seat at the table where deterrence doctrine is refined.
% DISAPPEARANCE_RATIONALE: If deterrence suddenly stopped holding (because arsenals were dismantled, or because a nuclear power abandoned the logic), the great-power competition it currently channels would reorganize into either: (a) direct warfare with uncontrolled escalation risk, or (b) a new coordination mechanism (treaty-based prohibition, complete disarmament with verification, or power redistribution). The international order rebuilt post-1945 is predicated on this constraint.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, states possessed weapons of such destructive power that their use could end civilization. The founding problem was: how do nuclear-armed rivals coexist without one attempting to eliminate the other (or both attempting mutual elimination)? The deterrence reading answered: mutual vulnerability makes the cost of winning a nuclear war higher than the cost of not fighting it.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear-armed states' strategic doctrine and force-deployment decisions corroborate that they treat deterrence as live: they maintain second-strike capability, invest continuously in modernization and counterforce systems, and conduct exercises simulating escalation control. Disarmament advocates and many security scholars outside the state apparatus attest the founding problem persists but argue deterrence is a dangerous solution. Strategic theorists outside government (Sagan, Waltz, Jervis) attest both the problem and the deterrence solution remain contested — that rationality-based deterrence stability is not assured by its own logic.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 and rising because deterrence requires continuous institutional investment (doctrine development, counterforce capability, signaling) to maintain credibility as a deterrent. The cost of this maintenance is borne by non-nuclear states (through alliance discipline and reduced autonomy) and proxy-war populations (through continued conventional violence as the safe outlet for great-power competition). Theater_ratio is high and rising (0.52) because an increasing share of enforcement activity is devoted to signaling and demonstrating resolve rather than to direct war-fighting (war-fighting is deterred, so the institutional apparatus dedicated to it performs signaling instead). Suppression is high (0.71) because the constraint is enforced through command authority, classification regimes, and institutional control of nuclear forces — non-state actors and disarmament movements are excluded from decision-making. Accessibility_collapse is moderate (0.45) because alternatives to deterrence are theoretically available (disarmament, treaty-based prohibition, power redistribution) but are institutionally suppressed and practically inaccessible. Resistance is moderate-high (0.58) because disarmament movements, some strategic theorists, and some non-nuclear states actively contest the deterrence framing, but the nuclear weapons states command the enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   The deterrence_equilibrium reading is authored from the seat of strategic rationality — the analyst who accepts that mutual vulnerability is the mechanism that prevents total war. From the nuclear weapons states' seat, this reading justifies continuous arms racing as deterrence maintenance. From the non-nuclear-state seat, the same logic appears as imposed exposure to proxy violence and alliance dependence. From the proxy-war-population seat, the reading appears as rationalization for why their violence matters less (the deterrent only works because nuclear war is avoided; conventional war can be tolerated). The engine computes these divergences from the structural data: the nuclear weapons states as agenda-setters (d near 0 — they benefit), the proxy-war populations as trapped payers (d near 1 — they bear costs), the strategic theorists as observers (d near 0.5 — they have neither power nor direct extraction). This structural divergence is the point: the same constraint produces different types from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states are the structural beneficiaries and agenda-setters: they set doctrine, control forces, and define what 'deterrence' means. Their directionality is near 0 (beneficiary end) because they gain strategic autonomy (immunity from conquest or elimination by rivals) and because they shape the rules. Their exit options are trapped — they cannot unilaterally abandon arsenals without breaking deterrence logic. Non-nuclear states pay through reduced autonomy and alliance dependence; their directionality is high (0.65–0.75, target end) because they bear costs they do not set. Proxy-war populations are maximally exploited: their directionality is near 1.0 (target end) because they are trapped (no geographic exit), powerless, and subject to violence driven by calculations they do not make. Strategic theorists are analytical observers (directionality 0.5, symmetric) — they benefit from the stability the constraint provides but bear no direct cost; they could exit by leaving the field. Disarmament advocates are excluded: they would have directionality pointing toward the target end (they bear the psychological cost of deterrence without influencing doctrine) but are kept out of the formal constraint-setting process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of incipient mandatrophy (rising theater_ratio, continuous doctrine revision without functional change) while the founding problem remains contested rather than resolved. Founding_problem_status = contested: nuclear-armed states argue deterrence remains necessary (new threats, proliferation risks); disarmament movements argue the founding problem (uncontrolled escalation risk) can be solved through abolition. The rising theater_ratio (0.52 at interval end, up from 0.25 at 1945) suggests an increasing share of enforcement activity devoted to signaling resolve rather than to maintaining war-fighting capacity. This is consistent with piton dynamics: the deterrent may be working through psychological credibility (theater) rather than through rational calculation of costs. However, the constraint remains classified as tangled_rope rather than piton because: (a) the coordination function is still live (great-power stability does depend on mutual vulnerability), (b) the extraction is asymmetric but acknowledged (non-nuclear states accept the arrangement as the price of security), and (c) the enforcement is active rather than purely inertial. The mandatrophy question is whether rising theater signals incipient degradation of the constraint toward piton status as the founding problem (escalation risk) persists while doctrine becomes performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_assumption,
    'Is deterrence stable by its own rationality logic, or does it persist only through luck, institutional inertia, and accident avoidance mechanisms that conceal instability?',
    'Systematic analysis of near-misses, accident scenarios, and rational-choice models under stress (finance-sector shock, climate emergency, regional power shifts); observation of whether nuclear-armed states de-escalate in crises or escalate until external factors intervene.',
    'If deterrence is fragile/inertial, the constraint is piton-like (performative maintenance masking risk). If deterrence is robust, the constraint is tangled_rope as claimed — genuine coordination riding on enforced extraction. This omega directly addresses whether the theater_ratio is rising because enforcement is working (maintaining deterrence through psychological credibility) or because enforcement machinery is theatrical while the real stability is luck.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_assumption, empirical, 'Whether deterrence is stable by rational logic or by contingent accident-avoidance.').

omega_variable(
    reading_kernel_contest,
    'Is total war strategically forbidden by the logical consequences of mutual vulnerability (this reading''s core), or is it normatively prohibited by constructed taboo independent of material capability (nuclear_taboo_reading), or has it been removed from what is strategically thinkable by the sheer scope of nuclear capability (space_contraction_reading)?',
    'Examine strategic doctrine, war games, and scenario analysis: does planning continue to treat total war as a reachable option at prohibitive cost (deterrence reading), or has total war vanished from the military imagination as unthinkable (space_contraction reading)? Examine norm evolution: do disarmament movements frame abolition as overcoming a taboo (nuclear_taboo_reading) or as recovering a strategic option that was wrongly removed (deterrence_reading pushing back)?',
    'Different reading implies different constraint type and different policy implications: deterrence reading predicts continuous arms-racing and doctrine refinement; nuclear_taboo reading predicts that norm erosion destabilizes the constraint; space_contraction reading predicts that the constraint is fragile if it ever becomes thinkable again. The three readings assign different ε values to the same kernel (what war is forbidden and why) because they disagree on the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which reading of the contested total_war_possibility_space kernel is structurally true: cost-benefit deterrence, constructed taboo, or logical space contraction?').

omega_variable(
    deterrence_vs_taboo_boundary,
    'Does the observed taboo against nuclear weapon use (the historical fact: no nukes used since Nagasaki) rest on deterrence logic or on normative prohibition, or on some mixture? And if the mixture is real, is deterrence the binding constraint or is the taboo?',
    'Test through scenarios where deterrence incentives are perverse (e.g., a nuclear power facing imminent defeat, or a new nuclear state facing security threat): would decision-makers violate the taboo if deterrence logic favored use? Historical analysis of near-uses (1950s Korea, 1973 Yom Kippur, 1983 Soviet false alarm) shows decision-makers treating escalation as catastrophic despite deterrence incentives — evidence that taboo is real. Compare to proliferation: do new nuclear states adopt deterrence logic or resist acquiring weapons as norm-violating?',
    'If taboo is the binding constraint, the deterrence_equilibrium_reading over-claims what''s enforcing the prohibition. The constraint type might shift toward scaffold (the taboo is a transitional norm that could be replaced by treaty-based abolition) or piton (the taboo is performing but fragile). If deterrence is binding, the reading stands as tangled_rope with continuous enforcement investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_taboo_boundary, empirical, 'Whether the no-use taboo is an artifact of deterrence logic or an independent normative constraint.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (external barriers enforced by state capacity: nuclear command authority, security clearances, institutional control) or internalized (decision-makers have absorbed the logic of deterrence and treat escalation as self-evidently catastrophic)?',
    'Post-exit test: if a decision-maker is removed from nuclear authority, do they retain deterrence-logic reasoning? Do whistleblowers and defectors describe external constraint (they wanted to act differently but couldn''t) or internalized logic (they thought escalation was irrational)? Analysis of institutional transmission: are new officers trained to think deterrence-logically or drilled into restraint through hierarchy?',
    'If structural, suppression is enforced by command systems and could be bypassed if authority broke down. If internalized, decision-makers carry deterrence logic with them; suppression persists even if institutional structures weaken. Mixed suppression would indicate the constraint relies on both external enforcement and cultivated rationality — the theater_ratio rising might indicate the balance shifting toward performative maintenance of external control while rationality becomes assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether nuclear restraint is enforced by external command structures or internalized as rational belief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.38).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1979, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1979, 0.48).
narrative_ontology:measurement_basis(tota_tr_t1979, observed).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.42).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2008, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2008, 0.51).
narrative_ontology:measurement_basis(tota_tr_t2008, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.58).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1979, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1979, 0.65).
narrative_ontology:measurement_basis(tota_be_t1979, observed).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.55).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2008, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement_basis(tota_be_t2008, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.62).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1979, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1979, 0.68).
narrative_ontology:measurement_basis(tota_su_t1979, observed).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.59).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2008, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2008, 0.69).
narrative_ontology:measurement_basis(tota_su_t2008, observed).
narrative_ontology:measurement(tota_su_t2026, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(tota_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__deterrence_equilibrium_reading, 0.18).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, great_power_strategic_stability).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, proxy_war_deferral_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the total_war_possibility_space kernel. The deterrence_equilibrium_reading models total war as a reachable option deterred by cost-benefit calculation. The sibling constraints (nuclear_taboo_reading and space_contraction_reading) model the same kernel through different mechanisms: normative prohibition and cognitive impossibility, respectively. These three constraints share a referent (what prevents total war) but diverge in their ε values (mechanism and persistence), beneficiary/victim structure (who benefits from the constraint's enforcement), and predicted trajectories (whether the constraint is stable, fragile, or degrading). The ε-invariance principle requires separate constraint stories because the three readings would assign substantially different extractiveness values to the same observable (the post-1945 no-use norm).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__deterrence_equilibrium_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
