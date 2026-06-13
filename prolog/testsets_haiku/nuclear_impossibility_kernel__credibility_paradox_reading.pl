% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Credibility Paradox Reading
 *   domain: strategic/political
 *
 * SUMMARY:
 *   The credibility paradox reading frames nuclear deterrence as a
 *   fundamentally unstable constraint: deterrence requires that great powers
 *   credibly threaten mutual destruction, but mutual destruction is not in
 *   any actor's rational interest, so the threat is logically incredible.
 *   This reading predicts that great powers will attempt to solve the paradox
 *   by developing 'usable' nuclear options (counterforce doctrines, limited
 *   war scenarios, escalation control), making war MORE reachable, not less.
 *   The constraint is claimed as tangled_rope (genuine coordination problem +
 *   asymmetric extraction) because deterrence does prevent some wars but does
 *   so by forcing all actors into a perpetual state of coerced strategic
 *   uncertainty and high-stakes military competition. The theater ratio rises
 *   dramatically over the 81-year interval (0.15 to 0.71) because modern
 *   nuclear doctrine is increasingly performative: maintaining the appearance
 *   of credibility through doctrinal elaboration and capability modernization
 *   becomes the dominant activity, while the actual coordination function
 *   (preventing war) remains contested and possibly illusory.
 *
 * KEY AGENTS:
 *   - Nuclear_weapons_state_a: Maintains the deterrent threat and articulates doctrine; identity-locked into the great-power role; faces pressure to make the threat more 'usable' or admit it is bluff
 *   - Rival_nuclear_power: Forced to assume the opponent might be irrational; must invest in counterforce and escalation control; identity-locked by great-power status
 *   - Extended_deterrence_allies: Benefit from the umbrella; want credibility but also want to believe the weapon will never be used; constrained exit
 *   - Non_nuclear_regional_power: Trapped between nuclear rivals or under extended deterrence; pays through military buildups and vulnerability
 *   - Military_establishment: Derives institutional legitimacy and budget from the deterrent mission; profits from the paradox's existence via continuous modernization
 *   - Disarmament_movement: Excluded from decision-making; would argue the paradox is insoluble; voice heard but not heeded
 *   - Strategic_analyst: Observes that great powers are solving the paradox by developing usable options, making war more likely, not less
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Credibility Paradox Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic/political").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '98f5515a-2c14-460a-816d-44d69665d82b').
narrative_ontology:cs_kernel_codification('98f5515a-2c14-460a-816d-44d69665d82b', distributed).
narrative_ontology:cs_authority_grounding('98f5515a-2c14-460a-816d-44d69665d82b', extraction).
narrative_ontology:cs_interpretation_layer_present('98f5515a-2c14-460a-816d-44d69665d82b').
narrative_ontology:cs_reading_relation('98f5515a-2c14-460a-816d-44d69665d82b', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('98f5515a-2c14-460a-816d-44d69665d82b', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('98f5515a-2c14-460a-816d-44d69665d82b', foundational, credible_threat_requires_irrational_willingness).
narrative_ontology:cs_axiom_status(credible_threat_requires_irrational_willingness, holdable).
narrative_ontology:cs_axiom_grounding('98f5515a-2c14-460a-816d-44d69665d82b', credible_threat_requires_irrational_willingness, empirically_contingent).
narrative_ontology:cs_axiom('98f5515a-2c14-460a-816d-44d69665d82b', foundational, escalation_ladders_make_war_reachable).
narrative_ontology:cs_axiom_status(escalation_ladders_make_war_reachable, holdable).
narrative_ontology:cs_axiom_grounding('98f5515a-2c14-460a-816d-44d69665d82b', escalation_ladders_make_war_reachable, empirically_contingent).
narrative_ontology:cs_reference_frame('98f5515a-2c14-460a-816d-44d69665d82b', mutual_vulnerability_with_unstable_deterrence).
narrative_ontology:cs_drift_state('98f5515a-2c14-460a-816d-44d69665d82b', contemporary_great_power_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98f5515a-2c14-460a-816d-44d69665d82b', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_trapped_rivals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_military_establishment).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, rival_nuclear_power).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_regional_power).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, neorealist_power_politics).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, rational_actor_assumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains a nuclear arsenal and articulates deterrent threats against existential threats or major power conflict. Seeks credibility by demonstrating willingness to use nuclear weapons while knowing that actual use guarantees mutual annihilation. Administers the doctrine, deploys the capability, and maintains the political narrative that the threat is credible despite the logical paradox. Faces constant pressure to either make the threat more 'usable' (counterforce doctrine, tactical nukes, limited war scenarios) or acknowledge it is bluff.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_state_a, agenda_setter,
    institutional, generational, trapped, global).

% Receive security guarantees (extended nuclear umbrella) from a nuclear power. Benefit from the deterrent effect against their regional rivals without bearing the cost of nuclear development. Accept the credibility paradox as the price of great-power protection: they want the threat to be credible but also want to believe it will never be used.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, global).

% Faces a deterrent threat it must take seriously enough to avoid major-power war, yet knows the threat's credibility is structurally undermined by mutual destruction. Forced to assume the opponent might be irrational, miscalculating, or willing to accept unthinkable costs. Must invest in counterforce capabilities, escalation control doctrines, and strategic uncertainty to hedge against the paradox. Identity locked into the great-power role which requires accepting the paradox as permanent.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, rival_nuclear_power, payer,
    institutional, generational, identity_locked, global).

% Faces a nuclear-armed rival or lives under extended deterrence of a nuclear power. Cannot escape the constraint's logic: if a nuclear power uses the weapon, regional security is devastated regardless of the war's outcome; if the nuclear power does not use it, the deterrent is revealed as bluff. Pays through military buildups, alliance dependence, and vulnerability to coercion by any actor claiming nuclear capability.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_regional_power, payer,
    moderate, biographical, constrained, regional).

% Administers nuclear doctrine and weapons systems. Derives institutional legitimacy and budget from the deterrent mission. Faces career and institutional pressure to make the deterrent appear more credible and usable via new doctrines (counterforce, limited war, escalation control). Profits from the paradox's existence: ambiguity about usability justifies continuous modernization and doctrine revision.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_military_establishment, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_military_establishment, beneficiary).

% Would argue the paradox proves deterrence is irrational and that the credibility problem is insoluble, making nuclear weapons elimination the only rational exit. Structurally excluded from the decision-making apparatus because the constraint's persistence depends on treating the paradox as solvable through doctrine and capability management. Their voice is heard in civil society but does not inform strategic doctrine.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_disarmament_movement, excluded,
    organized, generational, constrained, global).

% Examines the logical structure of deterrence credibility. Observes that great powers are solving the paradox not by making threats more rational but by investing in usable nuclear options, escalation ladders, and strategic ambiguity. Reads the constraint as a structure that forces actors toward destabilizing choices despite the mutual doom logic.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_analyst_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_state_a).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents major-power war by creating mutual vulnerability: both sides understand that large-scale conflict risks nuclear escalation and mutual annihilation, so both refrain from actions that might cross the nuclear threshold. The coordination problem is: how to make the threat to escalate credible enough to deter without making it so credible that it invites preemptive war or arms-race instability.
% TRANSFER_FUNCTION: Transfers security from nuclear-armed great powers to their allies in exchange for strategic alignment and deference; transfers vulnerability from non-nuclear states to all parties in the system. The constraint moves strategic decision-making authority upward (to nuclear powers) and constrains the military options available to non-nuclear actors. It also transfers resources: continuous military modernization and nuclear infrastructure investment is extracted from national budgets in the name of credibility maintenance.
% ABSENT_VOICES: Nuclear disarmament advocates and non-nuclear states (especially those without great-power patrons) would object that the paradox cannot be solved within the logic of deterrence, and that the attempt to solve it (by developing usable nuclear options, maintaining strategic ambiguity, and refining escalation doctrines) makes war MORE likely. They are structurally excluded because great powers have no incentive to admit the paradox is insoluble.
% DISAPPEARANCE_RATIONALE: If nuclear deterrence credibility collapsed overnight — if great powers publicly accepted the paradox as unsolvable and abandoned the threat of use — the entire post-WWII international order would reorganize: regional powers would pursue conventional military dominance or their own nuclear weapons, alliance structures would destabilize, and competition for hegemony would resume at conventional and sub-nuclear levels. The constraint's disappearance would be catastrophic precisely because it has structured decades of conflict avoidance.
% FOUNDING_PROBLEM: How to prevent great-power war when all parties possess weapons that guarantee mutual destruction if used? The founding problem is structural: create deterrence despite the logical contradiction between credible threat (I will use the weapon) and rational behavior (I will not use it because it destroys me too).
% FOUNDING_PROBLEM_CORROBORATION: Nuclear powers assert the founding problem remains live: regional conflicts, miscalculation risk, and great-power competition still exist, and deterrence prevents them from escalating to nuclear war. Strategic analysts and military establishments attest the problem persists and claim they have solved it through doctrine refinement and usable-nuclear options. Disarmament advocates and non-nuclear states attest the problem is NOT solved — the paradox is structural and attempts to solve it (counterforce doctrines, tactical nukes, escalation ladders) make war MORE likely, not less. Independent scholarly analysis (Sagan, Waltz, Schelling, Jervis) splits: some argue deterrence works despite the paradox; others argue the paradox makes war more likely, not less.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is moderate-high because deterrence stability requires all actors to accept great-power primacy, continuous military competition, and the risk of nuclear war—costs borne unevenly. The constraint extracts military resources, constrains non-nuclear states' options, and transfers strategic decision-making authority to nuclear powers. Suppression (0.58) is moderate because the constraint is maintained partly by force (military enforcement, deterrent threat) but also by the logic itself—no actor can easily exit a mutual vulnerability structure. Theater ratio (0.71) is high and rising over the interval, indicating the constraint's coordination function is increasingly obscured by elaborate doctrinal performance: counterforce doctrines, escalation ladders, strategic ambiguity, and continuous weapons modernization are designed to maintain credibility, not to solve the underlying coordination problem. Accessibility collapse (0.45) is moderate because alternatives do exist (disarmament, unilateral de-escalation, conventional deterrence) but are structurally foreclosed by the great-power commitment to nuclear capability and credibility. Resistance (0.72) is high because the disarmament movement, non-nuclear states, and even some strategic analysts actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear powers' strategic establishment (agenda_setter seat) experiences this as a solved problem: deterrence works, has prevented great-power war, and requires only continuous modernization and doctrinal refinement. Rival nuclear powers (payer seat) experience it as an insoluble paradox: they must assume credibility to be possible while knowing it is not, forcing them into continuous military competition and escalation risk. Non-nuclear states (payer seat) experience it as coerced vulnerability: they are trapped in a system whose benefits accrue to great powers and whose costs are borne globally. The strategic analyst (observer seat) measures the divergence and sees a constraint whose claimed coordination function is increasingly decoupled from its actual operation (military competition, institutional profit, and escalation risk).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are beneficiaries (they maintain strategic primacy, deter wars at the nuclear level, and profit institutionally from the constraint). Extended-deterrence allies are beneficiaries (they receive security without bearing nuclear costs) and partial payers (they accept dependence on great powers and vulnerability to nuclear escalation). Rival nuclear powers are forced payers (they must invest in counterforce and escalation control to hedge against the paradox). Non-nuclear states are victims (they are structurally excluded from strategic choice and vulnerable to escalation they did not create). The military establishment is both agenda-setter and beneficiary (it administers the constraint and profits from the paradox's existence). Directionality for nuclear powers and rivals is near-symmetric (both face mutual vulnerability, high stakes, continuous competition) but asymmetrically perceived: powers with offensive doctrines see themselves as beneficiaries; powers with defensive postures see themselves as payers. No directionality overrides are needed; the structural data suffice to differentiate seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy resolution: the founding problem (how to deter great-power war given mutual destruction) was real and arguably solved (no great-power nuclear war has occurred). But the constraint has persisted and expanded by transforming the mandate: modern nuclear doctrine does not claim to solve the original problem; it claims to make the threat more credible and more usable. This redefinition keeps the constraint alive while its original function recedes. The high theater ratio (0.71) and rising measurement series signal the atrophy: the constraint now exists largely to sustain institutional careers, military budgets, and strategic doctrine, not to prevent war. The mandatrophy is not fully resolved because the founding problem (deterrence credibility) remains contested, but the classification correctly identifies the functional drift: from coordination (prevent war) to extraction (maintain great-power primacy) to performance (elaborate doctrine and capability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_solvability_ambiguity,
    'Can the credibility paradox be solved through doctrinal innovation (usable nuclear options, escalation control, counterforce), or is the paradox structural and insoluble within the logic of deterrence?',
    'Empirical observation over decades: if the development of usable nuclear options (counterforce, tactical nukes, limited war doctrines) REDUCES the likelihood of great-power war, then the paradox is solvable. If such doctrines INCREASE destabilization, miscalculation risk, or arms-race intensity, then the paradox is structural. The resolution also turns on strategic outcomes: major-power conflicts (proxy wars, direct confrontations that approach escalation) and their relationship to doctrine change.',
    'If the paradox is solvable, this constraint is rightly classified as tangled_rope coordinating around a solved problem. If insoluble, the constraint is misclassified as coordination and should be reclassified as a snare (coerced into accepting an incredible threat as the price of avoiding catastrophe). The classification outcome depends directly on the resolution of this omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_solvability_ambiguity, empirical, 'Whether doctrinal innovation solves or exacerbates the credibility problem').

omega_variable(
    kernel_reading_determination,
    'Which reading of the nuclear_impossibility_kernel is the correct description of how nuclear weapons actually constrain state behavior: credibility paradox (war reachable via escalation instability), structural contraction (war impossible due to mutual annihilation), or rational dropout (war unlikely due to cost-benefit calculation)?',
    'Analysis of historical great-power near-conflicts (Cuban Missile Crisis, Cold War brinks, post-Cold War crises, contemporary great-power competition) and what prevented escalation: Was it belief in credible deterrence (credibility paradox reading—actors took the risk seriously), belief in structural impossibility of victory (structural contraction reading—actors saw war as pointless), or belief that costs would exceed benefits (rational dropout reading—actors calculated rationally)? Contemporary evidence from strategic competition between nuclear powers (US-China, US-Russia, India-Pakistan) should reveal which reading explains actual decision-making.',
    'This is the core ambiguity of the kernel itself. Resolving it would determine the correct reading of nuclear deterrence and might require classifying this constraint differently (possibly as a mountain if the structural contraction reading is correct and mutual annihilation is truly inevitable; or as a snare if the rational dropout reading is correct and the constraint depends on coerced belief in an irrational deterrent). The other two sibling constraints will resolve differently depending on the answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_determination, conceptual, 'Which reading of the nuclear impossibility kernel is empirically correct').

omega_variable(
    institutional_profit_dynamics,
    'To what extent does the military establishment''s institutional interest in maintaining the credibility paradox distort strategic doctrine toward instability and performative elaboration?',
    'Historical analysis of doctrine change, budget allocation, and weapons development over the interval, coupled with organizational analysis of military institutions'' incentives. Compare periods of high institutional profit (Cold War, arms-race phases) to periods of lower profit (post-Cold War arms-control moments) and assess whether doctrine became less or more elaborate, whether weapons development accelerated or decelerated, and whether war risk increased or decreased.',
    'If institutional profit significantly drives doctrine toward instability, the theater ratio measurement is accurate and the constraint is rightly classified as extractive (tangled_rope, with the extraction component dominating). If institutional profit is secondary to structural deterrence logic, the classification is less extractive and more coordinative. This omega is important for distinguishing between ''the paradox forces actors toward instability'' (structural account) and ''the military establishment profits from the paradox and elaborates doctrine to maintain it'' (institutional-interest account). Both may be true, but their relative weight determines whether theater_ratio is high or moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_profit_dynamics, empirical, 'Role of military institutional profit in doctrine elaboration and strategic instability').

omega_variable(
    sibling_reading_foreclosure,
    'Do the axioms and empirical commitments of the credibility_paradox reading logically foreclose the other two sibling readings, or do they coexist in the same strategic landscape?',
    'Logical analysis of the three readings'' core axioms and empirical premises. The credibility paradox reading asserts ''deterrence requires credibility and credibility is rationally impossible.'' The structural contraction reading asserts ''mutual annihilation is guaranteed, making rational escalation impossible.'' The rational dropout reading asserts ''victory is logically possible but costs exceed benefits.'' These three premises are logically distinct: they can coexist if different actors hold them simultaneously, or foreclose each other if they make mutually incompatible claims about the same strategic reality.',
    'If they logically foreclose each other, one reading is correct and the others are false—the engine''s computing of the kernel resolution should reclassify sibling constraints. If they coexist, all three remain live positions in strategic discourse, held by different analytical communities and different great powers. This determines whether reading_relations should be ''forecloses'' (rare, only when premises directly contradict) or ''coexists_with'' (readings remain live despite disagreement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the credibility paradox reading logically forecloses or coexists with sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement_basis(nucl_tr_t1945, observed).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.35).
narrative_ontology:measurement_basis(nucl_tr_t1962, observed).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1980, 0.62).
narrative_ontology:measurement_basis(nucl_tr_t1980, observed).
narrative_ontology:measurement(nucl_tr_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2001, 0.68).
narrative_ontology:measurement_basis(nucl_tr_t2001, observed).
narrative_ontology:measurement(nucl_tr_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2015, 0.7).
narrative_ontology:measurement_basis(nucl_tr_t2015, observed).
narrative_ontology:measurement(nucl_tr_t2026, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2026, 0.71).
narrative_ontology:measurement_basis(nucl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement_basis(nucl_be_t1945, observed).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement_basis(nucl_be_t1962, observed).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1980, 0.59).
narrative_ontology:measurement_basis(nucl_be_t1980, observed).
narrative_ontology:measurement(nucl_be_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2001, 0.61).
narrative_ontology:measurement_basis(nucl_be_t2001, observed).
narrative_ontology:measurement(nucl_be_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(nucl_be_t2015, observed).
narrative_ontology:measurement(nucl_be_t2026, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(nucl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement_basis(nucl_su_t1945, observed).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.42).
narrative_ontology:measurement_basis(nucl_su_t1962, observed).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(nucl_su_t1980, observed).
narrative_ontology:measurement(nucl_su_t2001, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement_basis(nucl_su_t2001, observed).
narrative_ontology:measurement(nucl_su_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement_basis(nucl_su_t2015, observed).
narrative_ontology:measurement(nucl_su_t2026, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(nucl_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2026
narrative_ontology:measurement(nucl_grid_01, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(class), 1945, 0.08).
narrative_ontology:measurement(nucl_grid_02, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(class), 2026, 0.35).
narrative_ontology:measurement(nucl_grid_03, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(individual), 1945, 0.05).
narrative_ontology:measurement(nucl_grid_04, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(individual), 2026, 0.22).
narrative_ontology:measurement(nucl_grid_05, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(organizational), 1945, 0.25).
narrative_ontology:measurement(nucl_grid_06, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(organizational), 2026, 0.48).
narrative_ontology:measurement(nucl_grid_07, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(structural), 1945, 0.15).
narrative_ontology:measurement(nucl_grid_08, nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse(structural), 2026, 0.42).
narrative_ontology:measurement(nucl_grid_09, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(class), 1945, 0.72).
narrative_ontology:measurement(nucl_grid_10, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(class), 2026, 0.78).
narrative_ontology:measurement(nucl_grid_11, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(individual), 1945, 0.68).
narrative_ontology:measurement(nucl_grid_12, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(individual), 2026, 0.75).
narrative_ontology:measurement(nucl_grid_13, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(organizational), 1945, 0.58).
narrative_ontology:measurement(nucl_grid_14, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(organizational), 2026, 0.68).
narrative_ontology:measurement(nucl_grid_15, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(structural), 1945, 0.65).
narrative_ontology:measurement(nucl_grid_16, nuclear_impossibility_kernel__credibility_paradox_reading, resistance(structural), 2026, 0.72).
narrative_ontology:measurement(nucl_grid_17, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(class), 1945, 0.12).
narrative_ontology:measurement(nucl_grid_18, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(class), 2026, 0.55).
narrative_ontology:measurement(nucl_grid_19, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(individual), 1945, 0.08).
narrative_ontology:measurement(nucl_grid_20, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(individual), 2026, 0.42).
narrative_ontology:measurement(nucl_grid_21, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(organizational), 1945, 0.35).
narrative_ontology:measurement(nucl_grid_22, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(organizational), 2026, 0.72).
narrative_ontology:measurement(nucl_grid_23, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(structural), 1945, 0.28).
narrative_ontology:measurement(nucl_grid_24, nuclear_impossibility_kernel__credibility_paradox_reading, stakes_inflation(structural), 2026, 0.68).
narrative_ontology:measurement(nucl_grid_25, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(class), 1945, 0.08).
narrative_ontology:measurement(nucl_grid_26, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(class), 2026, 0.48).
narrative_ontology:measurement(nucl_grid_27, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(individual), 1945, 0.05).
narrative_ontology:measurement(nucl_grid_28, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(individual), 2026, 0.35).
narrative_ontology:measurement(nucl_grid_29, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(organizational), 1945, 0.22).
narrative_ontology:measurement(nucl_grid_30, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(organizational), 2026, 0.65).
narrative_ontology:measurement(nucl_grid_31, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(structural), 1945, 0.18).
narrative_ontology:measurement(nucl_grid_32, nuclear_impossibility_kernel__credibility_paradox_reading, suppression(structural), 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.18).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel has three distinct readings, each instantiating a different constraint with different ε values and structural relationships. The credibility_paradox_reading asserts deterrence is unstable and war remains reachable through escalation ladders; it predicts great powers will develop usable nuclear options, making destabilization likely. This reading decomposes from the structural_contraction_reading (which asserts war is structurally impossible) and the rational_dropout_reading (which asserts war is unlikely due to cost-benefit rationality). All three readings are live in contemporary strategic discourse; they are not alternative formulations of a single constraint but three structurally distinct claims about how nuclear weapons constrain state behavior. Sibling constraints carry the structural_contraction_reading and rational_dropout_reading; this file carries the credibility_paradox_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
