% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Rational Dropout Reading of the Nuclear Impossibility Kernel
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   Since 1945 the destructiveness of nuclear arsenals has made the expected
 *   costs of war between great powers exceed any gain victory could deliver:
 *   conquest, coercion, and territorial revision remain physically reachable
 *   objectives, but no rational leadership can purchase them at the price the
 *   exchange would charge. This story instantiates the rational-dropout
 *   reading of that arrangement: peer war stays in the reachable set and is
 *   removed from active consideration by payoff arithmetic alone. The
 *   standing arrangement under assessment is the maintained mutual-restraint
 *   regime — arsenals, alert postures, doctrines, and arms-control
 *   instruments kept continuously in repair since 1945. Its coordination
 *   achievement is real and civilizational; its costs are also real and
 *   unevenly placed: the rivalry the center forbids itself was fought out in
 *   Korea, Indochina, Afghanistan, and Angola; the territorial map the
 *   arrangement froze stayed frozen for those who wanted it moved; and the
 *   arsenals the arithmetic renders redundant beyond a minimum were funded
 *   anyway. Sibling readings of the same kernel — guaranteed-annihilation
 *   impossibility and the credibility paradox — are separate constraint
 *   stories linked through the network section; they carry different victim
 *   sets and different extraction profiles and are not averaged into this
 *   one. KEY AGENTS (by structural relationship): -
 *   incumbent_nuclear_weapon_states: Primary beneficiary and administrator
 *   (institutional/arbitrage) — maintains the posture, collects the security
 *   surplus - defense_industrial_complexes: Secondary beneficiary
 *   (institutional/mobile) — collects procurement streams -
 *   great_power_home_populations: Diffuse beneficiary (organized/constrained)
 *   — receives the peace dividend - peripheral_proxy_war_populations: Primary
 *   target (powerless/trapped) — absorbs the channeled violence -
 *   revisionist_powers: Secondary target (powerful/constrained) — locked
 *   behind the frozen map - taxpayers_in_nuclear_states: Secondary target
 *   (moderate/mobile) — funds inventories past sufficiency -
 *   nonaligned_and_small_states: Excluded voice (organized/constrained) -
 *   abolitionist_movements: Excluded voice (organized/constrained) -
 *   strategic_studies_community: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.6).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.58).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Rational Dropout Reading of the Nuclear Impossibility Kernel").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'f756993e-cf90-4bf3-953a-e3f617c03d02').
narrative_ontology:cs_kernel_codification('f756993e-cf90-4bf3-953a-e3f617c03d02', distributed).
narrative_ontology:cs_authority_grounding('f756993e-cf90-4bf3-953a-e3f617c03d02', expertise).
narrative_ontology:cs_interpretation_layer_present('f756993e-cf90-4bf3-953a-e3f617c03d02').
narrative_ontology:cs_reading_relation('f756993e-cf90-4bf3-953a-e3f617c03d02', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('f756993e-cf90-4bf3-953a-e3f617c03d02', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('f756993e-cf90-4bf3-953a-e3f617c03d02', foundational, victory_remains_structurally_possible).
narrative_ontology:cs_axiom_status(victory_remains_structurally_possible, holdable).
narrative_ontology:cs_axiom_grounding('f756993e-cf90-4bf3-953a-e3f617c03d02', victory_remains_structurally_possible, empirically_contingent).
narrative_ontology:cs_axiom('f756993e-cf90-4bf3-953a-e3f617c03d02', secondary, expected_cost_dominance_excludes_peer_war).
narrative_ontology:cs_axiom_status(expected_cost_dominance_excludes_peer_war, holdable).
narrative_ontology:cs_axiom_grounding('f756993e-cf90-4bf3-953a-e3f617c03d02', expected_cost_dominance_excludes_peer_war, instrumental).
narrative_ontology:cs_reference_frame('f756993e-cf90-4bf3-953a-e3f617c03d02', rational_cost_dominance_equilibrium).
narrative_ontology:cs_drift_state('f756993e-cf90-4bf3-953a-e3f617c03d02', contemporary_strategic_environment, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f756993e-cf90-4bf3-953a-e3f617c03d02', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, incumbent_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_complexes).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, great_power_home_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, peripheral_proxy_war_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, revisionist_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_nuclear_states).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_revolution_thesis).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_deterrence_stability_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the arsenals, alert postures, and doctrines that sustain mutual cost-dominance, and set the terms of arms-control bargaining. Since 1945 no war has been fought on their home territories; their cores are invulnerable in a way no great-power core was before. They collect the security surplus and the status hierarchy of recognized weapons states, and they bear the maintenance bills they themselves authorize. Leaving the arrangement would mean dismantling the posture that secures them, so they adjust force structure at the margins instead.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, incumbent_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, incumbent_nuclear_weapon_states, beneficiary).

% Receive multi-decade procurement streams justified by the need to keep the deterrent posture credible and current. They design, build, and lobby for the force structures the posture calls for. Conversion to purely conventional markets is possible but has repeatedly proven loss-making, so the firms and laboratories orient their pipelines around the strategic mission.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_complexes, beneficiary,
    institutional, generational, mobile, national).

% Have lived under the longest great-power peace on record: no bombs, invasions, or mass mobilizations on home soil since 1945. They also carry the tax share of arsenal upkeep and a diffuse background risk of accident or miscalculation they did not choose and cannot individually decline. Their main lever is the ballot, which moves posture slowly.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, great_power_home_populations, beneficiary,
    organized, biographical, constrained, national).

% Live where the armed rivalry of the great powers was actually fought: Korea, Indochina, Afghanistan, Angola, Central America. Because direct war between the patrons was priced out by its costs, the competition migrated to their villages, and they absorbed the casualties, displacement, and destruction without ever being party to the decisions that routed the fighting through their territory. Exit means flight across borders that are typically closed to them.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, peripheral_proxy_war_populations, payer,
    powerless, immediate, trapped, local).

% Hold territorial or positional ambitions that the cost-dominance of great-power war places out of reach by force. They adapt by competing economically and technologically instead, and they probe at the margins of the frozen map where the nuclear ceiling permits conventional action. Their grievance is structural: the arrangement protects the holdings of the incumbents who wrote it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, revisionist_powers, payer,
    powerful, generational, constrained, continental).

% Fund arsenals and delivery systems whose scale exceeds anything a minimum-deterrence logic would call for, on the strength of the argument that the posture must be kept unquestionably sufficient. They can vote, protest, or relocate, but no jurisdiction offers relief from the bill while remaining inside the security community.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_nuclear_states, payer,
    moderate, biographical, mobile, national).

% Were never seated at the bargains that divided the world into spheres and later into recognized and unrecognized weapons states. They joined the nonproliferation bargain under a two-tier design they did not draft, watched their regions serve as competition arenas, and organize collectively — the Non-Aligned Movement, the humanitarian initiative — for a voice they still lack inside the strategic councils.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nonaligned_and_small_states, excluded,
    organized, generational, constrained, global).

% Argue that the arrangement's accumulated accident and miscalculation risk outweighs the stability it purchases, and campaign for the elimination of the weapons altogether. They secured a ban treaty in 2017 without a single nuclear-armed state participating, which measures both their organizing reach and their exclusion from the rooms where the posture is decided.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, abolitionist_movements, excluded,
    organized, generational, constrained, global).

% Produces the theories through which the arrangement is understood — the cost-dominance calculus, the stability-instability paradox, the revolution literature. It attests the founding problem from outside any government and supplies the analytical vocabulary in which every seat argues its position. Its exit is disciplinary: it can revise its own frames but not the arsenals.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, incumbent_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes general war between great powers from the active option set of rational leadership: by pricing any achievable victory above any conceivably acceptable cost, it converts an historically recurring catastrophe (general European war in 1914, renewed in 1939) into a permanently declined option, letting armed rivals coexist and plan on stable expectations.
% TRANSFER_FUNCTION: Moves security from a contested good to a shared condition among the incumbent nuclear states; moves the fighting the center forbids itself outward onto peripheral territories and their populations; moves wealth from taxpayers to arsenal-sustaining industry; and moves status into a two-tier order of recognized and unrecognized weapons states.
% ABSENT_VOICES: The populations of the proxy theaters had no seat when the rivalry was routed through them; non-aligned and small states were absent from the sphere-of-interest and nonproliferation bargains that tiered the world; abolitionist movements are heard in General Assembly halls but not in the councils that set posture; future generations, who inherit the accident risk and the frozen map, are represented by no one.
% DISAPPEARANCE_RATIONALE: If the cost-dominance judgment vanished overnight — if great-power war again looked winnable or affordable — alliance structures would recalibrate within months, war planning would return to active staff work, the frozen map would reopen as a live question, and the peripheral channeling would end only because the center itself had reopened; the post-1945 international order is arranged around this restraint and would rearrange around its absence.
% FOUNDING_PROBLEM: After two general wars in thirty years, and then the arrival of weapons that could destroy both camps outright, the founding problem was how armed great-power rivals coexist at all — how to prevent the historically normal recourse to general war without requiring either disarmament or capitulation.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by the declassified crisis record (ExComm tapes of October 1962, the 1983 Able Archer exercise files, the 1995 Norwegian-rocket episode) showing how near the unrestrained alternative repeatedly stood; by the Long Peace historiography and nuclear-revolution scholarship produced independent of any nuclear-armed government; and by the revealed conduct of non-nuclear states, which buy security under the arrangement rather than against it. No attestation rests solely on the incumbent governments that operate the arsenals.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.60 because the arrangement's burdens concentrate on seats that never consented to them — the millions killed in proxy theaters, the revisionists locked behind a frozen map, the taxpayers funding inventories far past minimum-deterrence sufficiency — while its benefits concentrate on the incumbents who administer it. Suppression is authored at 0.58 as a raw structural property (only extractiveness is scaled by the engine): the arrangement forecloses forceful revision and channels conflict, and keeping it in repair has twice demanded crisis-level alert postures, but it compels no internal orthodoxy and leaves arms-control and abolition alternatives legally visible. Theater is 0.28: the restraint function is genuine and has held for eight decades, while a persistent minority of activity — inventory scale beyond sufficiency, doctrinal rhetoric, exercise spectacle — serves posture maintenance rather than restraint. Accessibility collapse is 0.55: once the cost arithmetic is understood, rational peer war exits the planner's option set, but policy alternatives (deep cuts, de-alerting, abolition) remain on the table. Resistance is 0.38: revisionist probing at the nuclear ceiling, proliferation attempts against the two-tier order, and the abolition campaign press against the arrangement without threatening its core. The three measurement series share one nine-point grid. The suppression series traces roughly two full crisis-reform-relaxation cycles (1962 peak, détente trough, 1983 secondary peak, post-1991 trough, current re-tension); the oscillation is driven by external geopolitical cycling rather than by intermittent reinforcement, though each crisis wave has left a slightly higher enforcement floor. base_properties describe the end-state at the re-tension phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from the structural data. From the incumbent seat the arrangement is the stability it built and pays for — coordination it administers. From the peripheral-population seat the same structure is the mechanism that routed the great-power rivalry through their territory: bearing others' costs under duress. From the revisionist seat it is a cage drawn by the winners; from the taxpayer seat a bill justified by necessity. Inter-institutionally, the incumbent states and the defense complexes sit at adjacent institutional levels but diverge sharply on exit: states cannot leave the security predicament they anchor, while firms can convert to conventional markets at a loss. Same-level lateral divergence appears between incumbent and revisionist great powers — comparable global standing, opposite directionalities — differentiated by who wrote the frozen map and whose holdings it protects.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent nuclear states declare as beneficiaries and administer the arrangement, so their derived directionality sits near the beneficiary pole; the security surplus and status hierarchy flow to them. Defense complexes declare as beneficiaries collecting procurement streams. Home populations declare as beneficiaries receiving the peace dividend against diffuse accident and tax costs — near-symmetric but benefit-leaning. Peripheral proxy-war populations declare as victims with trapped exit and no power, placing them nearest the full-target pole; the channeled violence lands on them with no offsetting benefit. Revisionist powers declare as victims despite great power: the victim declaration dominates, and their constrained exit — the ceiling is the point of the arrangement — keeps them high on the target side. Taxpayers declare as victims with mobile-but-blunt exit, moderately high. No directionality overrides were needed: the beneficiary/victim declarations plus the exit atoms reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how armed great-power camps coexist without general war — remains live: the arrangement solves it continuously, so no mandatrophy resolution is declared and the R5 status-by-verdict pair (live, world_rearranges) is consistent, raising no zombie flag. The tangled-rope claim guards against both mislabels: calling the arrangement a mountain would launder administered, beneficiary-bearing maintenance into natural law and erase the payer seats — the false-summit failure mode this domain invites, since the reading's own tradition speaks of the arrangement in law-like registers; calling it a snare would erase the civilizational coordination function that separates it from arrangements whose coordination story is mere cover. The hybrid category is what the structure actually is: a genuine collective-action solution, real asymmetric extraction, and active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical_status,
    'Is the operative great-power restraint best captured by this reading''s payoff-dominance formulation, or by a sibling formulation of nuclear_impossibility_kernel — guaranteed-annihilation impossibility, or the credibility paradox?',
    'Comparative crisis-behavior analysis: do leaders act as if peer war were merely payoff-dominated (staff war games, victory-scenario hedging, SIOP evolution) or as if impossible (no serious war planning at all)? Declassified war-plan archives and crisis decision records discriminate between the readings.',
    'Sibling adoption changes the victim set and enforcement profile: the contraction reading licenses larger arsenals as insurance against an impossible outcome; the paradox reading shifts the extraction surface onto credibility spending. This story''s ε is valid only for the rational-dropout instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexical_status, conceptual, 'Which reading of the nuclear impossibility kernel the world actually runs on.').

omega_variable(
    natural_law_or_maintained_arrangement,
    'Is the exclusion of peer war a self-sustaining feature of nuclear destructiveness plus rational choice — law-like, persisting untended — or a constructed arrangement requiring continuous maintenance of alert postures, doctrine, and modernization?',
    'Counterfactual posture-decay analysis: does restraint survive arsenal neglect and doctrinal abandonment, as in periods of reduced readiness and lapsed arms control? Compare restraint durability across high- and low-maintenance phases.',
    'A law-like finding trends the classification toward mountain and dissolves the payer seats; a maintained-arrangement finding confirms active enforcement and the tangled-rope structure authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_maintained_arrangement, empirical, 'Whether the restraint is natural law or administered construction.').

omega_variable(
    proxy_channeling_causation,
    'Were the peripheral proxy wars caused by the central exclusion of direct great-power war — violence displaced by the same structure that coordinates the center — or would ideological competition have produced them regardless?',
    'Compare proxy-war incidence and scale across nuclear and pre-nuclear great-power rivalry eras; examine archival decision records citing the nuclear ceiling as the reason for engaging peripheries rather than centers.',
    'If the causal attribution fails, the victim set shrinks toward taxpayers and revisionists, and the arrangement reads closer to pure coordination than to the hybrid authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_channeling_causation, empirical, 'Whether proxy-war costs belong to this arrangement''s ledger.').

omega_variable(
    rationality_assumption_durability,
    'Does payoff-dominance bind under non-ideal rationality — bounded cognition, organizational pathology, compressed decision timelines, or automated response systems?',
    'Near-miss forensics (October 1962 ExCom tapes, the 1983 Petrov incident, the 1995 Norwegian-rocket episode) and organizational studies of command-and-control under stress.',
    'Weakened rationality raises the enforcement and suppression burden the arrangement requires and destabilizes the reading''s foundational axiom; the constraint''s reliability becomes conditional rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_durability, empirical, 'Whether the rational-choice foundation holds under real decision conditions.').

omega_variable(
    minimum_deterrence_sufficiency,
    'Is the maintained arsenal scale justified by the restraint function itself, or does the excess over minimum deterrence constitute rent collected by the posture''s administrators and suppliers?',
    'Independent force-structure analysis comparing deterrence-stability requirements (second-strike survivability thresholds) against deployed inventories over time.',
    'A wide excess supports reading the budget flows as extraction through the arrangement; a narrow excess supports treating them as coordination cost within the Boltzmann floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimum_deterrence_sufficiency, empirical, 'Whether arsenal scale tracks restraint needs or administrative rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rat_dropout_tr_t0, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(rat_dropout_tr_t0, observed).
narrative_ontology:measurement(rat_dropout_tr_t10, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(rat_dropout_tr_t10, observed).
narrative_ontology:measurement(rat_dropout_tr_t20, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(rat_dropout_tr_t20, observed).
narrative_ontology:measurement(rat_dropout_tr_t30, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(rat_dropout_tr_t30, observed).
narrative_ontology:measurement(rat_dropout_tr_t40, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(rat_dropout_tr_t40, observed).
narrative_ontology:measurement(rat_dropout_tr_t50, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(rat_dropout_tr_t50, observed).
narrative_ontology:measurement(rat_dropout_tr_t60, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(rat_dropout_tr_t60, observed).
narrative_ontology:measurement(rat_dropout_tr_t70, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 70, 0.31).
narrative_ontology:measurement_basis(rat_dropout_tr_t70, observed).
narrative_ontology:measurement(rat_dropout_tr_t79, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 79, 0.28).
narrative_ontology:measurement_basis(rat_dropout_tr_t79, observed).

% Extraction over time
narrative_ontology:measurement(rat_dropout_be_t0, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(rat_dropout_be_t0, observed).
narrative_ontology:measurement(rat_dropout_be_t10, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(rat_dropout_be_t10, observed).
narrative_ontology:measurement(rat_dropout_be_t20, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(rat_dropout_be_t20, observed).
narrative_ontology:measurement(rat_dropout_be_t30, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(rat_dropout_be_t30, observed).
narrative_ontology:measurement(rat_dropout_be_t40, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(rat_dropout_be_t40, observed).
narrative_ontology:measurement(rat_dropout_be_t50, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(rat_dropout_be_t50, observed).
narrative_ontology:measurement(rat_dropout_be_t60, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(rat_dropout_be_t60, observed).
narrative_ontology:measurement(rat_dropout_be_t70, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 70, 0.5).
narrative_ontology:measurement_basis(rat_dropout_be_t70, observed).
narrative_ontology:measurement(rat_dropout_be_t79, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 79, 0.6).
narrative_ontology:measurement_basis(rat_dropout_be_t79, observed).

% Suppression requirement over time
narrative_ontology:measurement(rat_dropout_su_t0, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(rat_dropout_su_t0, observed).
narrative_ontology:measurement(rat_dropout_su_t10, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(rat_dropout_su_t10, observed).
narrative_ontology:measurement(rat_dropout_su_t20, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(rat_dropout_su_t20, observed).
narrative_ontology:measurement(rat_dropout_su_t30, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(rat_dropout_su_t30, observed).
narrative_ontology:measurement(rat_dropout_su_t40, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(rat_dropout_su_t40, observed).
narrative_ontology:measurement(rat_dropout_su_t50, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(rat_dropout_su_t50, observed).
narrative_ontology:measurement(rat_dropout_su_t60, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 60, 0.36).
narrative_ontology:measurement_basis(rat_dropout_su_t60, observed).
narrative_ontology:measurement(rat_dropout_su_t70, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 70, 0.45).
narrative_ontology:measurement_basis(rat_dropout_su_t70, observed).
narrative_ontology:measurement(rat_dropout_su_t79, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 79, 0.58).
narrative_ontology:measurement_basis(rat_dropout_su_t79, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'nuclear impossibility' decomposes, per the ε-invariance principle, into three structurally distinct claims that the single label conflates. structural_contraction_reading asserts physical impossibility (guaranteed mutual annihilation); this story, rational_dropout_reading, asserts rational exclusion (reachable but payoff-dominated); credibility_paradox_reading asserts threat incoherence (credible-use paradox). Each carries its own ε, victim set, and failure mode: the contraction claim, if adopted, shrinks the victim set and licenses insurance-scale arsenals; the paradox claim relocates extraction onto credibility spending. This reading is upstream of both siblings in scholarly practice — the rational-choice consensus is the platform on which stronger impossibility claims are argued and the apparatus inside which the paradox is formulated — which is why the edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
