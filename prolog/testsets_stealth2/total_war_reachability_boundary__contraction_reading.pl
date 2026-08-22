% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Thermonuclear Closure of Winnable Total War (Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Between 1815 and 1945, great-power total war was a recurrent, operable
 *   strategic option, culminating in the World Wars. Thermonuclear arsenals
 *   ended its operability: at current scales, a full-scale great-power war
 *   cannot be won, survived, or terminated on favorable terms by any
 *   participant, and no strategy, technology, or expenditure of will has
 *   restored it. This story authors that closure as a single
 *   epsilon-invariant constraint: the standing fact that winnable total war
 *   is outside the feasible set of great-power strategy. The closure was not
 *   designed by anyone; it emerged as a side effect of weapons physics
 *   interacting with arsenal scale, and it collects nothing for anyone — its
 *   costs (universal extinction-risk exposure, funded substrates, secrecy
 *   governance) are borne diffusely by everyone inside the risk envelope,
 *   while no seat converts the closure itself into rents. KEY AGENTS (by
 *   structural relationship): - nuclear_great_powers: Substrate
 *   administrators (institutional/trapped) — operate the forces that define
 *   the condition; cannot exit - global_civilian_population: Universal
 *   risk-bearers (powerless/trapped) — fund and host the forces, absorb any
 *   use - umbrella_allies: Protected dependents (moderate/constrained) —
 *   receive the security guarantee, pay in basing and autonomy -
 *   nonaligned_smaller_states: Unrepresented risk-bearers (organized/trapped)
 *   — bear consequences without voice - future_generations: Absent
 *   cost-bearers (powerless/trapped) — inherit the maintained risk level -
 *   arms_control_epistemic_community: Analytical observer
 *   (analytical/analytical) — measures effects, models escalation, audits
 *   forces
 *
 * KEY AGENTS:
 *   - nuclear_great_powers: Substrate administrators (institutional/trapped) — operate the forces defining the condition; exit would mean unilateral disarmament into inferiority or a war they cannot survive
 *   - global_civilian_population: Universal risk-bearers (powerless/trapped) — taxed to fund the forces, hosted under them, first to absorb any use; no geography outside the envelope
 *   - umbrella_allies: Protected dependents (moderate/constrained) — organize defense around the guarantee that major-power war will not reach them; pay in basing access, targeting exposure, and crisis autonomy
 *   - nonaligned_smaller_states: Unrepresented risk-bearers (organized/trapped) — exposed to fallout, famine, and collapse from a war they have no part in; coalitions have demanded voice without altering forces or doctrines
 *   - future_generations: Absent cost-bearers (powerless/trapped) — inherit the risk level, the waste streams, and the precedent; represented only by advocacy
 *   - arms_control_epistemic_community: Analytical observer (analytical/analytical) — physicists, strategists, and verifiers publishing the damage estimates and stability analyses every other seat argues with
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.18).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Thermonuclear Closure of Winnable Total War (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '3f059ed7-008e-43ff-82c0-029f3c749876').
narrative_ontology:cs_kernel_codification('3f059ed7-008e-43ff-82c0-029f3c749876', distributed).
narrative_ontology:cs_authority_grounding('3f059ed7-008e-43ff-82c0-029f3c749876', expertise).
narrative_ontology:cs_interpretation_layer_present('3f059ed7-008e-43ff-82c0-029f3c749876').
narrative_ontology:cs_reading_relation('3f059ed7-008e-43ff-82c0-029f3c749876', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('3f059ed7-008e-43ff-82c0-029f3c749876', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('3f059ed7-008e-43ff-82c0-029f3c749876', foundational, total_war_categorical_unreachability).
narrative_ontology:cs_axiom_status(total_war_categorical_unreachability, holdable).
narrative_ontology:cs_axiom_grounding('3f059ed7-008e-43ff-82c0-029f3c749876', total_war_categorical_unreachability, empirically_contingent).
narrative_ontology:cs_axiom('3f059ed7-008e-43ff-82c0-029f3c749876', foundational, mutual_vulnerability_structurally_inescapable).
narrative_ontology:cs_axiom_status(mutual_vulnerability_structurally_inescapable, holdable).
narrative_ontology:cs_axiom_grounding('3f059ed7-008e-43ff-82c0-029f3c749876', mutual_vulnerability_structurally_inescapable, empirically_contingent).
narrative_ontology:cs_reference_frame('3f059ed7-008e-43ff-82c0-029f3c749876', thermonuclear_feasible_set_closure).
narrative_ontology:cs_drift_state('3f059ed7-008e-43ff-82c0-029f3c749876', contemporary_multipolar_nuclear_order, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3f059ed7-008e-43ff-82c0-029f3c749876', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_civilian_population).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, nonaligned_smaller_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contraction_reading, umbrella_allies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, umbrella_allies).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, nuclear_revolution_thesis).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, assured_destruction_logic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the arsenals whose destructiveness defines the strategic condition. They did not choose the condition and cannot opt out: dismantling their forces would not restore the war-fighting options their predecessors exercised, while retaining them absorbs a permanent share of national wealth and holds their own cities at risk. Their doctrines, planning cycles, and diplomacy all proceed inside a world where the largest wars their grandparents fought are no longer operations any of them could expect to survive. Leaving would mean either unilateral disarmament into conventional inferiority or a war no one wins; neither is available.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_great_powers, agenda_setter,
    institutional, generational, trapped, global).

% Live entirely inside the risk envelope: every city, food system, and supply chain sits within range of forces they do not control and never consented to. They fund those forces through taxation, host them in their territories, and would absorb the consequences of any large-scale use wherever it began. There is no geography outside the envelope and no purchase by which an individual declines coverage.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_civilian_population, payer,
    powerless, generational, trapped, global).

% Host allied nuclear forces and organize their entire defense posture around the guarantee that major-power war will not come to them. They receive a protection they could not generate alone and pay for it with basing access, forward-targeting exposure, and reduced autonomy in crises involving the protector. Abandoning the arrangement would leave them facing the same dangers without the guarantee.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, umbrella_allies, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contraction_reading, umbrella_allies, payer).

% Bear the fallout, famine, and economic-collapse risks of a war they have no part in and no vote on. Coalitions of them have organized repeatedly — treaty proposals, General Assembly resolutions, humanitarian-initiative conferences — to demand a voice and a disarmament timetable, and have been unable to alter the forces or doctrines of the states that hold them at risk.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nonaligned_smaller_states, payer,
    organized, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contraction_reading, nonaligned_smaller_states, excluded).

% Inherit whatever risk level the current generation chooses to maintain, together with contaminated sites, waste streams, and the precedent of permanent armed standoff. They are affected by every decision and present for none of them; their only representation is advocacy by the living.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Physicists, strategists, and treaty verifiers who measure weapons effects, model escalation pathways, and audit force structures. They produce the damage estimates and stability analyses the other seats argue with, staff the inspection regimes, and keep the public record of how closely the system has approached testing its own premises.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, arms_control_epistemic_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The closure removes a mutually catastrophic course of action from every great power's option set simultaneously, without requiring agreement: each state's unilateral recognition that the war cannot be won suffices, and no compliance mechanism is needed because violation is self-punishing. It coordinates by subtraction rather than by procedure.
% TRANSFER_FUNCTION: The closure itself transfers nothing to anyone: no rents, goods, work, or status flow through it, and no seat collects from its operation. The adjacent substrate arrangement moves wealth from taxpayers to military-industrial complexes and imposes universal risk exposure, but those flows belong to the separable arsenal-maintenance constraint, not to the boundary.
% ABSENT_VOICES: The global public — above all of non-nuclear and nonaligned states, and future generations — has never been seated in the arrangements that hold it at risk. Anti-nuclear movements and the humanitarian-initiative coalition articulate the objection and hold no decision rights; the dead of any future exchange are the permanently absent seat.
% DISAPPEARANCE_RATIONALE: If the closure vanished overnight — if winnable total war returned to the feasible set — great-power planning would revert within a single planning cycle: mobilization and industrial-conversion plans would be dusted off, alliances would be re-priced around war-fighting contribution, civil defense would revive as serious policy, and every arrangement built on the assumption that the largest war is unwinnable would be rebuilt around its return.
% FOUNDING_PROBLEM: The recurrence of civilization-scale great-power wars — the 1815–1945 pattern culminating in the World Wars. The closure was not built to solve this problem; it emerged unintentionally as a side effect of thermonuclear weapons development, and the problem it interrupted has not recurred since.
% FOUNDING_PROBLEM_CORROBORATION: Independent physics assessments — the nuclear-winter literature beginning with the 1983 TTAPS study and subsequent peer-reviewed modeling — attest the destructive scale from outside any state's apparatus, and diplomatic historians document the pre-1945 recurrence pattern the closure interrupted. No corroborator attests from a neutral seat that the problem is permanently dead: abolitionist coalitions attest the risk remains live, and warfighting-doctrine advocates attest reachability persists, so the status is disputed along the same lines that divide the readings of the parent kernel.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.05) and flat across the interval because the closure transfers nothing to any collector: it is an externality of arsenal physics, not an operated arrangement. The universal costs people bear under it (risk exposure, defense burdens) flow from the arsenals' existence, not from any seat's collection, and belong analytically to a separable substrate arrangement (see network note). Suppression is low-to-moderate (0.18) because the closure is self-enforcing — violation is self-punishing, so no enforcement machinery is required — but the series shows the closure's enforcement is not perfectly free: the 1962 spike (0.42) marks the one sustained moment when human crisis management, not physics, stood between the system and a test of its own premise, and the contemporary elevation (0.18) reflects renewed crisis signaling and information control. Theater ratio (0.26) tracks performative maintenance around a structure that needs none: civil-defense drills against thermonuclear attack, winnable-war rhetoric, modernization pageantry, and doomsday signaling are performances layered on a closure that operates whether or not they occur; the mid-century peak and post-Cold-War decline, with recent re-rise, are visible in the series. Accessibility collapse is high (0.92): once thermonuclear effects are understood, no doctrine, technology, or national will restores a winnable total war — alternatives do not merely fail, they stop being thinkable as strategy. Resistance is near-zero (0.06): occasional warfighting-doctrine advocacy exists, but no actor sustains an attempt to violate the closure, because attempting it is the violation. Emerges naturally is true: the closure is a structural consequence of weapons physics at scale, not a chosen rule. No boltzmann coordination type is declared: the closure operates no mechanism and solves no collective-action problem through procedure — it simply removes an option, which is why the coordination-function answer below describes a removal rather than an operated scheme. All three tracked metrics share one eight-point grid (1945–2025) so no metric's row is ever backfilled from another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute materially different types from identical structural data. From the nuclear_great_powers seat, the closure is the foundation of strategic stability they professionally administer — a condition they manage, plan within, and derive status from, experienced as closer to a maintained order than an imposed law. From the global_civilian_population and nonaligned_smaller_states seats, the same structure is an unconsented risk envelope: total protection from great-power war purchased with total personal exposure, with no exit and (for the nonaligned) no voice. From the umbrella_allies seat it is a guarantee they consume and partially pay for. From the analytical seat it is a physical claim under permanent test. The engine computes these divergences from power, exit, and directionality data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations: victims are the universal risk-bearing classes (global_civilian_population, nonaligned_smaller_states, future_generations); no beneficiaries are declared because no actor can convert the closure into winnings — the reading's core premise is that the prize (winnable total war) no longer exists for anyone. The derivation chain has a known blind spot here: deriving directionality from victim declarations plus trapped exit would push the powerless and organized seats toward the full-target end (~0.9), but the victim class is coextensive with the protected class — the same population that bears the risk receives the cessation of total war — so their net positions sit nearer symmetric. Overrides encode this: powerless 0.62 (risk-bearers who are also the protected), organized 0.72 (nonaligned coalitions bear risk with neither the protection premium of alliance nor any voice), moderate 0.35 (umbrella allies net-benefit from the guarantee while paying basing and autonomy costs), institutional 0.55 (the great powers administer the substrate and absorb its direct costs and first-strike vulnerabilities — slight target lean, neither subsidized nor purely targeted). The analytical seat takes the engine fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy analysis runs in the inverse direction for this constraint: there was never a mandate, so there is no mandate to outlive its function. The closure was nobody's project; it arrived as a byproduct of weapons development, and its persistence is its function — the two cannot come apart the way they do in administered arrangements. The classification discipline this story serves is therefore mislabeling-prevention in both directions. Reading the closure as a negotiated equilibrium (a coordination achievement among rational actors) licenses policies premised on its negotiability — deep arsenal cuts, defense-dominance optimism — that would be sound only if restraint, not physics, were doing the work. Reading it as atrophied capability licenses revival programs — damage-limitation investment, warfighting doctrine — that treat a structural closure as a muscle that has merely weakened. Classifying it as the standing structural fact it claims to be keeps both policy families honest: nothing administers the closure, so nothing can sunset it, and nothing can renegotiate it. On the genealogy interview: the founding problem (recurrent great-power total war) is recorded as contested rather than dead, because the parties genuinely dispute whether the problem is extinct or merely suppressed — but the combination with a world_rearranges verdict is not a zombie signature here: the arrangement's persistence IS its function, theater and capture are absent, and the mismatch cross-check against the computed piton/theater path should clear it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the contraction_reading of the total_war_reachability_boundary kernel; what structural differences would the sibling readings (dropping_reading, contingent_reachability_reading) introduce if instantiated instead?',
    'Cross-file comparison of the three reading stories: the dropping_reading would carry a beneficiary structure (rational-actor equilibrium collecting stability) and classify as a coordination arrangement; the contingent_reachability_reading would carry reversal-potential structure and classify as an atrophied-capability remnant.',
    'The disagreement is located in the modality of unreachability: categorical physical closure (this reading) versus probabilistic suppression by deterrence (dropping) versus technological contingency of current capability levels (contingent). Resolving the modality changes the type, the beneficiary structure, and the policy implications of the entire family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega recording that this constraint is one reading of a contested kernel and naming where the readings diverge.').

omega_variable(
    natural_vs_contingent_closure,
    'Is the closure of the total-war option a fixed consequence of thermonuclear weapons physics, or is it contingent on current arsenal scales, postures, and delivery technologies that political or technical change could alter?',
    'Technical assessment of damage-limitation pathways: ballistic-missile-defense saturation limits, counterforce feasibility against hardened and mobile forces, hypersonic penetration, and AI-enabled strike complexity. If no plausible pathway restores acceptable damage ratios, the closure is structural.',
    'If the closure is contingent, the mountain claim fails and the constraint shifts toward the contingent sibling''s characterization as a reversible capability condition; if structural, the mountain claim stands and revival programs are category errors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_contingent_closure, empirical, 'Whether the unreachability of total war is a law-like feature or an artifact of current force configurations.').

omega_variable(
    substrate_vs_boundary_decomposition,
    'Do the resource flows sustaining the arsenals (budgets, establishments, command infrastructure) constitute extraction operated by the reachability boundary itself, or do they belong to a distinct arsenal-substrate arrangement that merely shares a physical substrate with the boundary?',
    'Counterfactual analysis: if arsenals were drawn down to a minimum credible second-strike posture, the boundary would persist under this reading while establishment budgets and prestige flows would contract sharply — separable trajectories indicate distinct constraints.',
    'If fused, identifiable beneficiaries exist for the combined arrangement and the false-summit machinery becomes relevant, pushing classification toward a hybrid coordination/extraction type; if separable, this story remains a clean no-collector closure and the substrate arrangement warrants its own story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_boundary_decomposition, conceptual, 'Whether the boundary and the arsenal-maintenance economy are one constraint or two.').

omega_variable(
    edge_of_boundary_limited_war,
    'Does the closure cover all nuclear use or only unlimited strategic exchange? The reachability of limited nuclear war sits at the boundary''s edge and is not settled by the contraction claim.',
    'Escalation-pathway modeling and historical incident analysis: whether any documented crisis pathway terminates in contained exchange rather than unrestricted escalation, and whether warfighting doctrines possess credible termination mechanics.',
    'If limited exchange is feasible, the boundary is narrower than the categorical claim asserts and the residual-reachability position gains evidentiary ground at the edge; if every pathway escalates, the closure is confirmed as total.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(edge_of_boundary_limited_war, empirical, 'Location of the boundary''s edge with respect to limited nuclear use.').

omega_variable(
    suppression_spike_interpretation,
    'Do the crisis-moment spikes in required active management (most sharply at the 1962 missile crisis, and again in the contemporary signaling environment) reveal a latent dependence on human restraint that the closure claim ignores, or do they merely record stress tests that the structure survived?',
    'Comparative crisis analysis: whether near-miss episodes cluster around failures of the physical closure or around failures of judgment and communication that the closure independently survived.',
    'If restraint is load-bearing, the closure is partly a practiced equilibrium rather than pure physics, strengthening the residual-reachability position; if the structure held through every stress test autonomously, the self-enforcement claim is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_spike_interpretation, empirical, 'Whether crisis spikes expose hidden dependence on active restraint beneath the claimed physical closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twarb_contraction_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(twarb_contraction_tr_t8, total_war_reachability_boundary__contraction_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(twarb_contraction_tr_t17, total_war_reachability_boundary__contraction_reading, theater_ratio, 17, 0.38).
narrative_ontology:measurement(twarb_contraction_tr_t20, total_war_reachability_boundary__contraction_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(twarb_contraction_tr_t30, total_war_reachability_boundary__contraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(twarb_contraction_tr_t46, total_war_reachability_boundary__contraction_reading, theater_ratio, 46, 0.16).
narrative_ontology:measurement(twarb_contraction_tr_t60, total_war_reachability_boundary__contraction_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(twarb_contraction_tr_t80, total_war_reachability_boundary__contraction_reading, theater_ratio, 80, 0.26).

% Extraction over time
narrative_ontology:measurement(twarb_contraction_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(twarb_contraction_be_t8, total_war_reachability_boundary__contraction_reading, base_extractiveness, 8, 0.03).
narrative_ontology:measurement(twarb_contraction_be_t17, total_war_reachability_boundary__contraction_reading, base_extractiveness, 17, 0.04).
narrative_ontology:measurement(twarb_contraction_be_t20, total_war_reachability_boundary__contraction_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(twarb_contraction_be_t30, total_war_reachability_boundary__contraction_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(twarb_contraction_be_t46, total_war_reachability_boundary__contraction_reading, base_extractiveness, 46, 0.04).
narrative_ontology:measurement(twarb_contraction_be_t60, total_war_reachability_boundary__contraction_reading, base_extractiveness, 60, 0.03).
narrative_ontology:measurement(twarb_contraction_be_t80, total_war_reachability_boundary__contraction_reading, base_extractiveness, 80, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(twarb_contraction_su_t0, total_war_reachability_boundary__contraction_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(twarb_contraction_su_t8, total_war_reachability_boundary__contraction_reading, suppression_requirement, 8, 0.06).
narrative_ontology:measurement(twarb_contraction_su_t17, total_war_reachability_boundary__contraction_reading, suppression_requirement, 17, 0.42).
narrative_ontology:measurement(twarb_contraction_su_t20, total_war_reachability_boundary__contraction_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(twarb_contraction_su_t30, total_war_reachability_boundary__contraction_reading, suppression_requirement, 30, 0.06).
narrative_ontology:measurement(twarb_contraction_su_t46, total_war_reachability_boundary__contraction_reading, suppression_requirement, 46, 0.04).
narrative_ontology:measurement(twarb_contraction_su_t60, total_war_reachability_boundary__contraction_reading, suppression_requirement, 60, 0.07).
narrative_ontology:measurement(twarb_contraction_su_t80, total_war_reachability_boundary__contraction_reading, suppression_requirement, 80, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_arsenal_substrate_arrangement).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_nonproliferation_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'the nuclear peace' conflates at least three structurally distinct constraints, decomposed per the epsilon-invariance principle: (1) the reachability boundary itself — this story, a no-collector structural closure with universal diffuse cost-bearing; (2) the nuclear_arsenal_substrate_arrangement — the funding, establishments, and command infrastructure where identifiable beneficiaries and budgetary flows actually live, warranting its own story with its own epsilon; (3) the deterrence-equilibrium construal captured by the dropping_reading sibling, in which stability is produced by rational-actor coordination rather than physics. Epsilon differs sharply across members: near-zero here, substantial in the substrate arrangement. Family members link through affects_constraints; the upstream physical-closure claim is typically cited as evidence by the downstream equilibrium and atrophy construals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, powerless, 0.62).
constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, moderate, 0.35).
constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, organized, 0.72).
constraint_indexing:directionality_override(total_war_reachability_boundary__contraction_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
