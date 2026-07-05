% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Nuclear-Contracted Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Before 1945, and arguably through the early Cold War until
 *   second-strike-capable arsenals matured on both sides, total war between
 *   great powers was a reachable strategic outcome — costly, but winnable in
 *   the sense that a state could rationally calculate net advantage from
 *   prosecuting it to conclusion. This reading holds that the maturation of
 *   secure, survivable nuclear retaliatory capability (submarine-launched and
 *   dispersed land-based systems, redundant command-and-control) changed the
 *   physics of the strategic space itself: no combination of first-strike
 *   capability, defense, or resolve could deliver a state a total war it
 *   could survive as a functioning society. The feasible set for great-power
 *   strategy no longer contains 'win a total war against a nuclear peer.'
 *   This is not a claim about probability (that is the dropping reading) or
 *   about a temporary capability profile (that is the contingent reachability
 *   reading) — it is a claim about the permanent geometry of what is
 *   achievable.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: administer arsenals and doctrine but do not set or control the physical boundary itself (institutional/trapped, civilizational)
 *   - global_civilian_population: bears diffuse residual existential risk with no voice or exit (powerless/trapped, civilizational)
 *   - future_generations: inherit the risk profile without consent (powerless/trapped, civilizational)
 *   - non_human_biosphere: bears catastrophic tail-risk harm with no representation (powerless/trapped, global)
 *   - strategic_theorists: analytical observers debating which of the three readings is structurally correct (analytical/analytical, generational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.1).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Nuclear-Contracted Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '393bf87f-298b-4ed3-a19c-147ce8170fc2').
narrative_ontology:cs_kernel_codification('393bf87f-298b-4ed3-a19c-147ce8170fc2', distributed).
narrative_ontology:cs_authority_grounding('393bf87f-298b-4ed3-a19c-147ce8170fc2', distributed).
narrative_ontology:cs_reading_relation('393bf87f-298b-4ed3-a19c-147ce8170fc2', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_reading_relation('393bf87f-298b-4ed3-a19c-147ce8170fc2', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('393bf87f-298b-4ed3-a19c-147ce8170fc2', foundational, retaliatory_certainty_forecloses_victory_permanently).
narrative_ontology:cs_axiom_status(retaliatory_certainty_forecloses_victory_permanently, holdable).
narrative_ontology:cs_axiom_grounding('393bf87f-298b-4ed3-a19c-147ce8170fc2', retaliatory_certainty_forecloses_victory_permanently, empirically_contingent).
narrative_ontology:cs_axiom('393bf87f-298b-4ed3-a19c-147ce8170fc2', secondary, physical_boundary_is_not_policy_maintained).
narrative_ontology:cs_axiom_status(physical_boundary_is_not_policy_maintained, holdable).
narrative_ontology:cs_axiom_grounding('393bf87f-298b-4ed3-a19c-147ce8170fc2', physical_boundary_is_not_policy_maintained, empirically_contingent).
narrative_ontology:cs_created_at('393bf87f-298b-4ed3-a19c-147ce8170fc2', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_civilian_population).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, future_generations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_human_biosphere).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutual_assured_destruction_thesis).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, nuclear_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals capable of societal destruction but cannot use them to win a total war without triggering retaliation that destroys the initiator as well. They administer deterrence postures, but the boundary itself is not something they set or could relax by choice — it is fixed by the physics of fission/fusion yield relative to any conceivable defense or dispersal, not by policy.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_armed_states, observer,
    institutional, civilizational, trapped, global).

% Bears the residual existential risk that the boundary itself does not eliminate — the arsenals that make total war unwinnable still exist and could be used in error, escalation, or system failure. Has no vote over the constraint's existence and no exit from a planet where it holds; the risk is diffuse and species-wide, not something any single population can bargain away.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_civilian_population, payer,
    powerless, civilizational, trapped, global).

% Inherit whatever residual probability of catastrophic use the current arsenals carry, without having consented to the risk or having any means to alter the physical boundary that both contracts winnable total war and holds the destructive potential in reserve.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Would bear catastrophic and irreversible harm (nuclear winter, radiological contamination, ecosystem collapse) in the event the residual probability the boundary does not erase were ever realized. Has no representation in any decision process.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_human_biosphere, payer,
    powerless, civilizational, trapped, global).

% Study whether the contraction of the feasible set to exclude winnable total war is a permanent structural fact (this reading) or a contingent, reversible, or merely probabilistic phenomenon (the sibling readings). Their analysis does not change the physical boundary but shapes how policymakers describe it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, on this reading. The boundary is not a coordination solution any party constructed or maintains — it is a physical fact about weapons yield, delivery reliability, and retaliatory certainty that removes an entire strategy (winnable total war between nuclear peers) from the feasible set regardless of what any actor wants or does.
% TRANSFER_FUNCTION: Nothing is transferred by the boundary itself; it forecloses a category of action rather than moving resources between parties. What IS at stake — retained existential risk — is a universally distributed liability rather than a rent captured by anyone.
% ABSENT_VOICES: Future generations and the non-human biosphere have no seat in any deliberation about arsenal levels, doctrine, or risk tolerance; they bear the tail risk the boundary leaves unresolved without any voice in setting it. This is commentary-grade: no correction to the mountain classification follows from their absence, since the boundary is not something their presence could have altered.
% DISAPPEARANCE_RATIONALE: On the contraction reading, the boundary is a physical fact about arsenals and retaliatory dynamics, not a socially constructed rule that could be repealed. If states stopped believing in or discussing the boundary, the underlying physical reachability of winnable total war would not change — it remains outside the feasible set as long as second-strike-capable nuclear arsenals exist. Disbelief does not restore reachability; only physical disarmament or technological change (the province of the sibling readings) could.
% FOUNDING_PROBLEM: The problem this reading identifies is not one anyone 'built a solution' for — it is the discovery, forced by the physical properties of thermonuclear weapons after roughly 1945-1960, that no state could execute a total war against a comparably armed peer and expect to survive as a functioning society, regardless of strategy, technology, or resolve.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by independent physical and technical analysis (yield-to-population-density modeling, nuclear winter studies from atmospheric scientists outside any state's military establishment, and continuity-of-arsenals verification by arms-control monitoring bodies) rather than by the nuclear-armed states themselves, who have institutional incentives to describe the boundary in policy terms (deterrence success) rather than physical ones (structural foreclosure).
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.05) because no actor collects rent from the boundary's existence — there is no beneficiary group at all under this reading, which is the structural delta the kernel context specifies. Suppression is low (0.10) because the boundary does not require active coercive maintenance; it is not enforced against a resistant population the way a snare would be — it simply is the case that no strategy in the feasible set delivers a winnable total war. Accessibility collapse is very high (0.92): once the retaliatory-certainty logic is understood, no alternative strategic path to a winnable total war between nuclear peers presents itself — the alternative (fighting and winning total war) has been physically foreclosed, not merely discouraged. Resistance is low (0.15): unlike a constructed constraint that provokes organized opposition, a physical impossibility does not meet resistance in the ordinary sense — occasional doctrinal challenges (first-strike theorizing, missile defense advocacy) exist but do not constitute resistance to a maintained arrangement, since there is no arrangement to resist, only a physical fact to contest empirically.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear-armed states, in their doctrinal posture, sometimes describe the boundary as something THEY maintain through deterrence policy (closer to the dropping reading's rope framing) rather than as an external physical fact they merely operate within. This reading insists the states' agency is real at the level of arsenal management and doctrine but does not extend to the underlying boundary itself — no state chose the yield-to-population-density physics or the impossibility of leak-proof missile defense against saturation attack. The gap between the states' self-description (agency, coordination, choice) and this reading's claim (physical foreclosure, no agency over the boundary itself) is exactly the seam the kernel's three readings exist to disambiguate.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary group exists under this reading by construction — the schema's FSM check does not apply because no beneficiaries are declared. Victims are declared as the universal set of parties bearing residual existential risk (global civilian population, future generations, non-human biosphere) since the boundary's foreclosure of winnable total war does not eliminate the underlying destructive capacity, only the rational path to victory through its use. All three victim groups are powerless with trapped exit at civilizational time horizon and global spatial scope — there is no differentiated directionality to compute because there is no differentiated relationship to a coordinating or extracting party; the risk is symmetric across the powerless population in the sense that no subgroup can arbitrage out of species-level tail risk.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply cleanly to this reading, and that is itself informative: mandatrophy names a constraint whose founding problem has died while the constraint persists on inertia or capture. Here the founding problem (physical impossibility of winnable total war) is asserted as LIVE and permanent, not as a problem that could go dead and leave a hollow institution behind. If the founding-problem status were ever to be reassessed as 'dead' — for instance, if a technological breakthrough restored reachability — that would not indicate mandatrophy of THIS constraint; it would indicate that this reading was empirically wrong and the contingent_reachability_reading was correct all along. The disappearance_verdict of world_unchanged reflects that disbelief in the boundary does not restore reachability on this reading; the boundary is not sustained by belief.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_dropping_vs_contingent,
    'Is the post-1945 exclusion of winnable total war from the strategic feasible set a permanent physical foreclosure (this reading), a probability-lowering coordination equilibrium that remains reachable in principle (dropping_reading), or a currently-atrophied capability profile that could reverse with technological change such as effective missile defense or counterforce precision (contingent_reachability_reading)?',
    'No empirical test can fully resolve this at present, since it is partly a claim about counterfactual robustness (would total war remain unwinnable under ANY future technology) rather than about current fact. Partial evidence: track whether emerging technologies (hypersonic counterforce weapons, layered missile defense, AI-driven early warning/decapitation strategies) meaningfully restore a rational path to victory in modeling and wargaming conducted by independent strategic-studies institutions, not by the militaries advocating for the technologies.',
    'If contingent_reachability_reading is empirically vindicated (technology reopens the feasible set), this contraction reading is falsified as a permanent claim and should be understood retrospectively as describing a multi-decade piton-like plateau rather than a mountain. If dropping_reading is correct, the boundary was never a mountain but a maintained rope whose maintenance could fail — a materially different risk profile with an identifiable coordinating structure (arms control regimes, deterrence doctrine) that this reading denies exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_dropping_vs_contingent, conceptual, 'Which of the three kernel readings correctly characterizes the post-1945 strategic boundary: permanent foreclosure, probabilistic coordination, or contingent/reversible capability.').

omega_variable(
    mountain_with_universal_victims_coherence,
    'Can a genuine mountain (physical impossibility, zero degrees of freedom) coherently carry a victim set at all, given that mountains are defined by having no party who collects from their operation and typically no declared beneficiaries or victims (e.g. gravity)?',
    'Distinguish the boundary itself (which forecloses a strategy — no victims, since foreclosing an option is not extraction) from the retained underlying destructive capacity (which creates residual risk borne by populations who did not choose it). The victims here are victims of the retained risk that the boundary does not erase, not victims of the boundary''s foreclosure function.',
    'If this distinction does not hold structurally, the victim declarations should be removed and this story should read as a pure mountain with no parties (like gravity), exempting it from the stakeholder-coverage requirement entirely. As authored, the victim declarations reflect that species-level tail risk is a real cost even though the boundary that forecloses winnable total war is not itself an extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_with_universal_victims_coherence, conceptual, 'Whether declaring universal victims alongside a mountain claim is structurally coherent or an artifact of the kernel''s expected delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contraction_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contraction_reading, theater_ratio, 1991, 0.07).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2008, total_war_reachability_boundary__contraction_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement_basis(tota_tr_t2008, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.08).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.03).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1991, 0.04).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2008, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2008, 0.05).
narrative_ontology:measurement_basis(tota_be_t2008, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.05).
narrative_ontology:measurement_basis(tota_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the total_war_reachability_boundary kernel. contraction_reading (this file) claims permanent physical foreclosure — mountain, near-zero ε, no beneficiaries, universal victim set. dropping_reading claims a maintained probabilistic coordination equilibrium — rope, moderate ε tied to maintenance cost, identifiable coordinating parties (deterrence-postured states). contingent_reachability_reading claims a technology-contingent, currently-atrophied capability — piton, with the possibility of reversal. All three share the same underlying historical phenomenon (post-1945 great-power strategic behavior) but assign it structurally distinct ε values and types; per the ε-invariance principle they are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
