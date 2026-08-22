% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Nuclear Deterrence as Reachable-but-Improbable Total War Equilibrium
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story reads the total-war reachability boundary as a coordination
 *   equilibrium under continuous strain rather than a fixed feature of the
 *   strategic landscape. Since 1945, the probability of great-power total war
 *   has fallen substantially — no direct nuclear-armed-state war has occurred
 *   — but the underlying capability, the alert postures, and the doctrinal
 *   architecture that make total war reachable in a matter of minutes have
 *   never been dismantled. This reading treats deterrence as a tangled rope:
 *   it does coordinate (mutual restraint genuinely lowers realized risk
 *   relative to a world of unconstrained great-power war) but it also
 *   extracts (arsenal-holders and their allies capture ongoing security,
 *   leverage, and industrial rents, while populations under threat and future
 *   generations bear a nonzero residual catastrophic risk they never
 *   consented to and cannot exit). The Cuban Missile Crisis peak (1962), the
 *   post-Cold-War drawdown (1991), and the recent doctrine-modernization and
 *   arms-control-erosion period (2022 onward) mark visible swings in how
 *   tightly the coordination equilibrium has held, which the measurement
 *   series traces on one shared grid.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Nuclear Deterrence as Reachable-but-Improbable Total War Equilibrium").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '5ce0f411-bd84-4eb1-9f82-1a422cb355b6').
narrative_ontology:cs_kernel_codification('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', distributed).
narrative_ontology:cs_authority_grounding('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', distributed).
narrative_ontology:cs_reading_relation('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', foundational, deterrence_is_managed_equilibrium_not_removed_boundary).
narrative_ontology:cs_axiom_status(deterrence_is_managed_equilibrium_not_removed_boundary, holdable).
narrative_ontology:cs_axiom_grounding('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', deterrence_is_managed_equilibrium_not_removed_boundary, empirically_contingent).
narrative_ontology:cs_axiom('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', foundational, reachability_persists_independent_of_probability).
narrative_ontology:cs_axiom_status(reachability_persists_independent_of_probability, holdable).
narrative_ontology:cs_axiom_grounding('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', reachability_persists_independent_of_probability, empirically_contingent).
narrative_ontology:cs_reference_frame('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', mutual_assured_destruction_stability_equilibrium).
narrative_ontology:cs_drift_state('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', post_2022_doctrine_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ce0f411-bd84-4eb1-9f82-1a422cb355b6', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_studies_establishment).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states_outside_umbrellas).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, future_generations_under_residual_risk).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutual_assured_destruction_stability_thesis).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, nuclear_peace_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain arsenals, doctrine, and command-and-control infrastructure that both make total war reachable (the capability exists and is postured for use) and suppress its probability (deterrence signaling, second-strike survivability). They set the rules of the coordination game — alert postures, declaratory doctrine, arms control terms — and derive geopolitical leverage, security-council standing, and industrial rents from possessing the capability, independent of whether it is ever used.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, beneficiary).

% Shelter under a nuclear patron's umbrella, gaining security subsidy without independently bearing proliferation costs or the full weight of arsenal maintenance. Their exit from the arrangement would require either developing independent deterrents or accepting reduced security guarantees, both costly; they benefit from the coordination equilibrium's stability while contributing little to its maintenance cost.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, regional).

% Manufactures, modernizes, and services delivery systems, warheads, and command infrastructure. Revenue flows directly from the reachability of total war being maintained as credible — a fully retired capability would eliminate a market. Lobbies for modernization programs framed as maintaining deterrence credibility.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_industrial_base, beneficiary,
    organized, generational, arbitrage, national).

% Academic and think-tank apparatus that theorizes, models, and advises on deterrence stability. Careers, funding streams, and institutional prestige are built on treating deterrence as a solvable coordination problem requiring continuous expert management. Benefits from the constraint's persistence as an object of study and policy relevance.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_studies_establishment, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, strategic_studies_establishment, observer).

% Bear the tail-risk of total war's reachability without having consented to the coordination game or having any lever to exit the arrangement. Their fate is a stake in a game played by others; no individual or local action changes the probability they face. This is the seat the reachability boundary is measured against.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, generational, trapped, global).

% States without arsenals and without extended-deterrence guarantees experience the same global reachability of total war without any of the coordination benefits nuclear states and their allies extract from it. They can lobby for disarmament regimes or nonproliferation treaties, but cannot unilaterally alter the reachability boundary.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states_outside_umbrellas, payer,
    moderate, generational, constrained, national).

% Inherit whatever residual probability of total war the current coordination equilibrium leaves in place, plus any accumulated risk from near-misses, doctrine drift, or arsenal expansion, without having participated in setting the terms. Cannot exit a risk profile set before their existence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, future_generations_under_residual_risk, payer,
    powerless, civilizational, trapped, universal).

% Broker treaties (New START-type instruments, nonproliferation regimes) that adjust the parameters of the coordination equilibrium — verification regimes, force caps, no-first-use pledges. Can shift the boundary's probability but not eliminate its reachability; their leverage depends on continued cooperation from the nuclear weapon states they are negotiating with.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_negotiators, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, arms_control_negotiators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nuclear-armed states coordinate on mutual restraint: each refrains from first use and from provocations that would trigger the other's use, because the expected cost of total war exceeds any conceivable gain for all parties possessing second-strike capability. This is a genuine coordination equilibrium — a real Schelling point that lowers the probability of total war relative to a world without credible retaliatory capability.
% TRANSFER_FUNCTION: The arrangement moves security assurance from arsenal-holders and their formal allies to those populations, concentrating residual catastrophic risk on populations who have no seat in the coordination game — including populations of the arsenal-holding states themselves, non-aligned states, and all future generations. It also moves ongoing resource transfers (modernization budgets, industrial contracts, alliance dues) from taxpayers to the defense-industrial and strategic-studies apparatus that maintains the equilibrium's credibility.
% ABSENT_VOICES: Populations under nuclear threat and future generations have no representation in doctrine-setting, arsenal-sizing, or alert-posture decisions. Non-nuclear states outside umbrellas participate only through nonproliferation diplomacy, a much weaker lever than possessing or being shielded by an arsenal. They would argue the coordination equilibrium externalizes catastrophic tail risk onto those with no say in managing it.
% DISAPPEARANCE_RATIONALE: If deterrence credibility vanished overnight (arsenals remaining but doctrine, signaling, and command discipline collapsing), nuclear-weapon-state relations would likely become far more volatile and the probability of total war would rise sharply — the world would rearrange toward higher realized risk. But if the underlying capability (the weapons themselves) also vanished, the reachability boundary itself would be eliminated, not merely the coordination managing it — a different, more radical rearrangement. Whether the constraint's disappearance means 'coordination fails' or 'capability disappears' changes the verdict, which is why this reading treats it as contested rather than settled.
% FOUNDING_PROBLEM: The founding problem was preventing catastrophic great-power war in a world where nuclear weapons made victory-through-conquest strategically incoherent (no post-war spoils survive a nuclear exchange) while the weapons themselves could not be un-invented. Deterrence doctrine was built to convert an unmanageable existential risk into a managed, if imperfect, equilibrium.
% FOUNDING_PROBLEM_CORROBORATION: Independent nuclear risk researchers (e.g., Federation of American Scientists near-miss archives, Bulletin of the Atomic Scientists Doomsday Clock assessments) and disarmament-focused NGOs outside any nuclear-weapon-state government or defense-industrial interest corroborate that the underlying risk of inadvertent or deliberate nuclear war remains live — citing documented near-misses, doctrine modernization (hypersonic delivery, lowered-yield options), and eroding arms control architecture as evidence the founding problem has not been retired, only managed at a lower but nonzero probability.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at a moderate-high 0.58 by 2025: the coordination function is genuine (mutual restraint is real and load-bearing) but so is the extraction (concentrated benefit to arsenal-holders, defense industry, and strategic-studies careers, diffuse catastrophic cost to non-participants). Suppression at 0.71 reflects that the arrangement's persistence depends on continuously reinforced alert postures, doctrine, and signaling — remove the active maintenance and the equilibrium does not hold itself. Accessibility collapse is moderate (0.4): unlike a mountain, meaningful alternatives (disarmament regimes, no-first-use pledges, arms control) exist and have been partially implemented, they are simply politically difficult, not physically or logically foreclosed. Resistance (0.55) reflects sustained disarmament and arms-control advocacy contesting the arrangement's terms without succeeding in retiring it.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapon-state seat, deterrence is stable, rational, and mutually beneficial coordination — a solved game everyone plays correctly. From the populations-under-threat seat, the same architecture is an imposed background risk maintained by parties who do not bear its tail. The engine should compute these as structurally different experiences of the same constraint, driven by the power/exit asymmetry, not by differing beliefs about the same facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and their defense-industrial and strategic-studies ecosystems sit near the beneficiary end: they set doctrine, capture security leverage and revenue, and have arbitrage-grade exit (they can adjust posture, negotiate, or modernize at will). Extended deterrence allies benefit similarly but with constrained exit (leaving the umbrella is costly). Populations under threat, non-nuclear states outside umbrellas, and future generations sit at the target end: trapped exit, no seat in doctrine-setting, and the reachability boundary is measured directly against their exposure. This is why the story authors ε against the standing coordination-equilibrium arrangement as this reading sees it, not against a disarmed counterfactual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing catastrophic great-power war under conditions where the weapons cannot be un-invented) remains live by independent corroboration, which is why this reading resists classifying the arrangement as pure legacy theater (a piton) or as a fully solved natural fact (a mountain). Tangled rope avoids both mislabelings: it registers the genuine coordination achievement (probability has dropped) without erasing the ongoing extraction and residual risk (reachability persists) that a mountain classification would silently launder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_probability_conflation,
    'Is the observed decline in total war frequency evidence that the reachability boundary itself has contracted (siding with contraction_reading), or only that a coordination equilibrium is currently suppressing realized probability while reachability stays constant (this reading)?',
    'Examine whether any structural capability to wage total war has been dismantled (delivery systems retired, command infrastructure decommissioned, treaties with verified irreversible reductions) versus whether only alert postures and doctrine have shifted. Irreversible capability reduction would support contraction; reversible doctrine-only change supports the dropping reading.',
    'If reachability has genuinely contracted, this constraint collapses toward the sibling contraction_reading and the tangled_rope classification weakens toward something closer to a partially-realized mountain (a boundary that no longer exists rather than one being actively managed). If reachability is intact and only probability has dropped, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_probability_conflation, conceptual, 'Whether declining war frequency reflects boundary contraction or equilibrium management — the central fork between this reading and its contraction sibling.').

omega_variable(
    technology_dependent_reversibility,
    'How much of the current low-reachability state depends on specific technologies (early-warning systems, second-strike survivability, verification regimes) that could degrade or be leapfrogged, versus how much is a durable feature of the coordination game itself?',
    'Track investment and doctrine changes in hypersonic weapons, AI-assisted command and control, and anti-satellite capabilities that could compress decision timelines or undermine second-strike assurance; a sustained trend toward compression would support the contingent_reachability_reading''s framing of the current state as a piton vulnerable to reversal.',
    'If reachability is highly technology-contingent, the coordination equilibrium modeled here is less stable than a tangled_rope classification implies — it may be better described (per the sibling contingent_reachability_reading) as an atrophied-risk state that could snap back rather than a durable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_dependent_reversibility, empirical, 'Whether the current equilibrium is a stable coordination structure or a technologically contingent, reversible lull.').

omega_variable(
    beneficiary_capture_of_risk_narrative,
    'Does the strategic-studies establishment''s institutional interest in framing deterrence as a manageable, expert-navigable coordination problem bias the field''s own risk assessments downward, understating true reachability?',
    'Compare institutionally-funded strategic stability assessments against fully independent risk modeling (e.g., actuarial or systems-safety approaches to near-miss data) for systematic divergence in estimated annual probability of inadvertent nuclear war.',
    'If a systematic downward bias exists, the extractiveness score understates the true beneficiary capture, and part of what looks like ''coordination'' is better described as risk-narrative management serving the establishment''s own institutional interest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_risk_narrative, conceptual, 'Whether the coordination framing is itself partly a beneficiary-authored narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__dropping_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(tota_tr_t2010, total_war_reachability_boundary__dropping_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(tota_tr_t2022, total_war_reachability_boundary__dropping_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__dropping_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.62).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.42).
narrative_ontology:measurement(tota_be_t2010, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(tota_be_t2022, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2022, 0.6).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(tota_su_t2010, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(tota_su_t2022, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_nonproliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, extended_deterrence_alliance_structures).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the BGS-style total war reachability boundary' into structurally distinct constraints per the epsilon-invariance principle: dropping_reading (this file, tangled_rope, epsilon=0.58 — probability dropped, reachability intact, actively coordinated), contraction_reading (mountain-leaning, epsilon much lower — the boundary itself contracted, feasible set shrank), and contingent_reachability_reading (piton-leaning — current low reachability is atrophied capability contingent on technology, could reverse). All three describe the same colloquial 'has total war become impossible' question but instantiate different structural claims with different epsilon values and different victim/beneficiary sets; they must not be merged into one story with an observable-dependent epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
